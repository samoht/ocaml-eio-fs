open! Hermetic
module M = Chaos_model
module Plan = Chaos_plan

type env =
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t >

module Op = Plan.Op
module Fault = Plan.Fault

(* {1 Wrapper} *)

let numberer () =
  let index = ref 0 and nths = Hashtbl.create 16 in
  fun (op : M.Op.t) : Op.t ->
    incr index;
    let kind = (op.kind :> Op.kind) in
    let nth =
      1 + Option.value ~default:0 (Hashtbl.find_opt nths (kind, op.path))
    in
    Hashtbl.replace nths (kind, op.path) nth;
    {
      index = !index;
      kind;
      path = op.path;
      nth;
      file_offset = op.file_offset;
      length = op.length;
      dest = op.dest;
    }

(* An [ENOSPC] on an operation that needs no room is none: Linux never
   answers it there. *)
let model_fault ~space (op : Op.t) : Fault.t -> M.Fault.t option = function
  | Fail `Enospc when (not space) && (op.kind = `Change || op.kind = `Open_write)
    ->
      None
  | Fail c ->
      Some
        (match op.kind with
        | `Read | `Read_dir -> Read_error c
        | `Write | `Truncate -> Write_error c
        | `Sync -> Sync_error c
        | `Open_read | `Open_write -> Open_error c
        | `Stat -> Stat_error c
        | `Change -> Change_error c)
  | Short n -> (
      match op.kind with
      | `Read -> Some (Short_read n)
      | `Write -> Some (Short_write n)
      | _ -> None)
  | Torn n -> ( match op.kind with `Write -> Some (Torn_write n) | _ -> None)
  | Swap_symlink s -> (
      match op.kind with
      | `Open_read | `Open_write | `Stat -> Some (Swap_symlink s)
      | _ -> None)

let wrap f base =
  let num = numberer () in
  let hook (mop : M.Op.t) =
    let op = num mop in
    Option.bind (f op) (model_fault ~space:mop.space op)
  in
  (M.wrap (M.Schedule.hook hook) base :> Eio.Fs.dir_ty Eio.Path.t)

(* {1 Runs} *)

(* A program that retries a failed operation at once, with no time passing,
   spins for ever under a lasting condition. Once one operation on one path
   has failed 10000 times more than three times the first run made it, or
   one kind of operation in one directory, as a retry under a fresh name
   makes, 10000 times more than 1000 times, while the clock stands still, it
   raises [Failure] instead, and after twice as many it waits for ever. *)
let streak_limit tag first =
  10_000 + (first * match tag with `Path -> 3 | `Dir -> 1000)

(* A run that makes more operations spins, or is more than a check plans
   from: it stops there, so its operations stay bounded. *)
let max_ops = 100_000

type outcome =
  | Passed
  | Raised of exn
  | Deadlock
  | Time_limit
  | Busy
  | Too_long
  | Kept_going
  | Too_many
  | Livelock of [ `Path | `Dir ] * string * int
  | Over_budget of int (* the operation the first run stopped at *)

let outcome_text = function
  | Passed -> "none"
  | Raised x -> Printexc.to_string x
  | Deadlock -> "the run hangs: every fiber waits and no timer is due"
  | Time_limit ->
      "the run hangs: it is still running after an hour of its clock"
  | Busy ->
      "the run woke 10,000,000 times without waiting 1 ms or more: a busy loop"
  | Too_long ->
      "the run took 10,000,000 turns, more than a check can repeat: a busy \
       loop, or a workload too large for a check"
  | Kept_going ->
      "the run hangs: it kept going for 1000 operations after the power cut, \
       so it catches Eio.Cancel.Cancelled"
  | Too_many ->
      "the run made 100,000 filesystem operations, more than a check plans \
       from: a busy loop, or a workload too large for a check"
  | Livelock (`Path, path, n) ->
      Fmt.str
        "the run hangs: the same operation on %s failed %d times while its \
         clock stood still"
        path n
  | Livelock (`Dir, dir, n) ->
      Fmt.str
        "the run hangs: one kind of operation in %s failed %d times while its \
         clock stood still"
        dir n
  | Over_budget at ->
      Fmt.str "the linear tier needs more runs than the budget by operation #%d"
        at

(* A cut fails the run's switch with it, which cancels every fiber of the
   run. *)
exception Cut

let env_of clock mono (d : _ Eio.Path.t) : env =
  object
    method fs = (d :> Eio.Fs.dir_ty Eio.Path.t)
    method clock = clock
    method mono_clock = mono
  end

(* A run ends once [halted] holds why. *)
let in_loop ?idle ?(halted = ref None) main =
  match
    Quiet_mock.run ?idle
      ~stop:(fun () -> Option.is_some !halted)
      ~limit:3600. main
  with
  | v -> Ok v
  | exception Quiet_mock.Stopped -> Error (Option.get !halted)
  | exception Quiet_mock.Deadlock_detected -> Error Deadlock
  | exception Quiet_mock.Time_limit -> Error Time_limit
  | exception Quiet_mock.Busy -> Error Busy
  | exception Quiet_mock.Too_long -> Error Too_long

(* Park the fiber where no cancel reaches, so even a loop that catches
   everything stops; the loop then reports a deadlock. *)
let park () = Eio.Cancel.protect Eio.Fiber.await_cancel

let halt halted o =
  halted := Some o;
  park ()

(* The first run stops once the linear tier needs more runs than the
   budget: past that, the check cannot pass, and the run's operations need
   not grow. *)
let count_op halted ops =
  incr ops;
  if !ops > max_ops then halt halted Too_many

let add_floor halted ~budget ~floor ~at n =
  match budget with
  | Some b ->
      floor := !floor + n;
      if !floor > b then halt halted (Over_budget at)
  | None -> ()

let fresh () = (Memfs.v () :> Eio.Fs.dir_ty Eio.Path.t)

let rec walk (d : _ Eio.Path.t) prefix f =
  List.iter
    (fun name ->
      let q = Eio.Path.(d / name) and path = prefix ^ "/" ^ name in
      let kind = Eio.Path.kind ~follow:false q in
      f path q kind;
      if kind = `Directory then walk q path f)
    (List.sort compare (Eio.Path.read_dir d))

let files d =
  let l = ref [] in
  walk d "" (fun path q -> function
    | `Regular_file -> l := (path, String.length (Eio.Path.load q)) :: !l
    | _ -> ());
  List.rev !l

(* {1 Reports, budget, replay} *)

let fail name fmt = Fmt.kstr (fun s -> failwith (name ^ ": " ^ s)) fmt

let refuse_budget name ~budget at =
  fail name
    "the linear tier needs more than the budget of %d runs by operation #%d of \
     the first run, which stopped there, and a check never passes on a partial \
     one. Shrink the workload, or raise ~budget."
    budget at

let budget_of name = function
  | None -> 20_000
  | Some b when b < 1 -> Fmt.invalid_arg "%s: the budget is %d" name b
  | Some b -> b

let parse_replay name r =
  let bad () = fail name "~replay:%S is not a replay of eio-fs" r in
  if not (String.starts_with ~prefix:"fs/" r) then bad ()
  else
    let r' = String.sub r 3 (String.length r - 3) in
    match String.index_opt r' '/' with
    | Some i when i > 0 && i < String.length r' - 1 ->
        (String.sub r' 0 i, String.sub r' (i + 1) (String.length r' - i - 1))
    | _ -> bad ()

let digest (trace : Plan.trace) =
  let s = Fmt.str "%a" Fmt.(list ~sep:(any ";") Op.pp) trace in
  String.sub (Digest.to_hex (Digest.string s)) 0 8

let rec diverge (a : Plan.trace) (b : Plan.trace) =
  match (a, b) with
  | x :: a, y :: b ->
      if x = y then diverge a b else Fmt.str "%a, then %a" Op.pp x Op.pp y
  | x :: _, [] | [], x :: _ -> Fmt.str "%a, then nothing" Op.pp x
  | [], [] -> "nothing"

type tier = Linear | Sampled

let tier_name = function Linear -> "linear" | Sampled -> "sampled"

let parent path =
  match String.rindex_opt path '/' with
  | Some 0 -> "/"
  | Some i -> String.sub path 0 i
  | None -> "."

(* What a repeated operation keeps: its kind, the directory it is in and
   its fault, so a retry under one name or under fresh ones repeats. *)
let shape ((op : Op.t), fault) = (op.kind, parent op.path, fault)

(* The lines of a trace: an operation, or a block of [period] operations
   repeated, of which the first and the last are shown. *)
type line =
  | Line of (Op.t * Fault.t option)
  | Repeated of { period : int; times : int; first : int; last : int }

let max_period = 8
let min_times = 3

(* A trace shows at most [keep] lines at each end, and those around the
   first fault. *)
let keep = 100
let context = 10

(* A run's trace as its report shows it, kept as the run goes. At each
   place, the block of up to [max_period] operations repeated [min_times]
   times or more that covers the most operations, the shortest on a tie,
   collapses to its first and last instances. Of the lines that leaves, only
   the first and last [window] and those around the first fault are kept, so
   a run that spins costs no more than one that does not. *)
module Recorder = struct
  type entry = (Op.t * Fault.t option) * (Op.kind * string * Fault.t option)

  (* A block repeating at the end of the trace: its first instance, the
     instances so far, the last whole one and the start of the next. *)
  type rep = {
    period : int;
    first_block : entry array;
    second : int; (* the first operation of the second instance *)
    mutable count : int;
    mutable last_block : entry array;
    mutable before_last : int; (* the operation before the last instance *)
    mutable partial : entry list; (* the next instance, latest first *)
    mutable npartial : int;
  }

  let window = (2 * keep) + (2 * context) + 1
  let blank = Repeated { period = 0; times = 0; first = 0; last = 0 }

  type t = {
    mutable buf : entry array; (* the operations no line holds yet *)
    mutable len : int;
    mutable rep : rep option;
    head : line array; (* the first [window] lines *)
    ring : line array; (* the last [window] lines *)
    mutable n : int;
    mutable fault : int option; (* the line of the first fault *)
    mutable around : (int * line) list;
  }

  let v () =
    {
      buf = [||];
      len = 0;
      rep = None;
      head = Array.make window blank;
      ring = Array.make window blank;
      n = 0;
      fault = None;
      around = [];
    }

  let index ((((op : Op.t), _), _) : entry) = op.index

  let emit t l =
    let k = t.n in
    if k < window then t.head.(k) <- l;
    t.ring.(k mod window) <- l;
    (match (t.fault, l) with
    | None, Line (_, Some _) ->
        t.fault <- Some k;
        for j = max 0 (k - context) to k do
          t.around <- (j, t.ring.(j mod window)) :: t.around
        done
    | Some f, _ when k <= f + context -> t.around <- (k, l) :: t.around
    | _ -> ());
    t.n <- k + 1

  let emit_block t b = Array.iter (fun (e, _) -> emit t (Line e)) b

  let append t e =
    if t.len = Array.length t.buf then begin
      let b = Array.make (max 16 (2 * t.len)) e in
      Array.blit t.buf 0 b 0 t.len;
      t.buf <- b
    end;
    t.buf.(t.len) <- e;
    t.len <- t.len + 1

  let drop t k =
    Array.blit t.buf k t.buf 0 (t.len - k);
    t.len <- t.len - k

  (* The whole instances of the block of [p] operations from the start of
     the buffer, and whether more may come. *)
  let run t p =
    let rec go r =
      let j = r * p in
      let rec same k =
        k >= p
        || j + k >= t.len
        || (snd t.buf.(j + k) = snd t.buf.(j + k - p) && same (k + 1))
      in
      if not (same 0) then (r, false)
      else if j + p > t.len then (r, true)
      else go (r + 1)
    in
    go 1

  let close t rep rest =
    t.rep <- None;
    emit_block t rep.first_block;
    emit t
      (Repeated
         {
           period = rep.period;
           times = rep.count - 2;
           first = rep.second;
           last = rep.before_last;
         });
    emit_block t rep.last_block;
    List.iter (append t) rest

  (* Decide the lines the buffer starts with, as far as the operations so
     far allow, or all of them at the end. *)
  let rec decide ~at_end t =
    if t.len > 0 then begin
      let runs = Array.init max_period (fun k -> run t (k + 1)) in
      let ongoing k = (not at_end) && snd runs.(k) in
      let best ~only =
        let bp = ref 1 and br = ref 1 in
        Array.iteri
          (fun k (r, _) ->
            let p = k + 1 in
            if only k && r >= min_times && r * p > !bp * !br then begin
              bp := p;
              br := r
            end)
          runs;
        (!bp, !br)
      in
      match List.find_opt ongoing (List.init max_period Fun.id) with
      | None ->
          let p, r = best ~only:(fun _ -> true) in
          if r >= min_times then begin
            let b = Array.sub t.buf 0 (r * p) in
            emit_block t (Array.sub b 0 p);
            emit t
              (Repeated
                 {
                   period = p;
                   times = r - 2;
                   first = index b.(p);
                   last = index b.(((r - 1) * p) - 1);
                 });
            emit_block t (Array.sub b ((r - 1) * p) p);
            drop t (r * p)
          end
          else begin
            emit t (Line (fst t.buf.(0)));
            drop t 1
          end;
          decide ~at_end t
      | Some k ->
          (* Past [2 * max_period] operations, every block still repeating
             is a multiple of the shortest one, which covers as much as
             they do: once it covers more than any that ended, it wins. *)
          let q = k + 1 and r = fst runs.(k) in
          let bp, br = best ~only:(fun k -> not (ongoing k)) in
          if t.len >= 2 * max_period && r >= min_times && r * q > bp * br then begin
            let rest =
              Array.to_list (Array.sub t.buf (r * q) (t.len - (r * q)))
            in
            t.rep <-
              Some
                {
                  period = q;
                  first_block = Array.sub t.buf 0 q;
                  second = index t.buf.(q);
                  count = r;
                  last_block = Array.sub t.buf ((r - 1) * q) q;
                  before_last = index t.buf.(((r - 1) * q) - 1);
                  partial = List.rev rest;
                  npartial = List.length rest;
                };
            t.len <- 0;
            t.buf <- [||]
          end
    end

  let add t ((op, fault) as e) =
    let e = (e, shape (op, fault)) in
    match t.rep with
    | None ->
        append t e;
        decide ~at_end:false t
    | Some rep ->
        let k = rep.npartial in
        if snd e = snd rep.last_block.(k) then begin
          rep.partial <- e :: rep.partial;
          rep.npartial <- k + 1;
          if k + 1 = rep.period then begin
            rep.before_last <- index rep.last_block.(rep.period - 1);
            rep.last_block <- Array.of_list (List.rev rep.partial);
            rep.partial <- [];
            rep.npartial <- 0;
            rep.count <- rep.count + 1
          end
        end
        else begin
          close t rep (List.rev (e :: rep.partial));
          decide ~at_end:false t
        end

  let finish t =
    (match t.rep with
    | Some rep -> close t rep (List.rev rep.partial)
    | None -> ());
    decide ~at_end:true t

  let line t i =
    if i < window then t.head.(i)
    else if i >= t.n - window then t.ring.(i mod window)
    else List.assoc i t.around

  (* The report's lines: the first and last [keep] and those around the
     first fault, or all of them when they are few. *)
  let pp b t =
    finish t;
    let n = t.n in
    let shown i =
      n <= (2 * keep) + (2 * context) + 1
      || i < keep
      || i >= n - keep
      || match t.fault with Some f -> abs (i - f) <= context | None -> false
    in
    let plural k = if k = 1 then "" else "s" in
    let rec pp i =
      if i < n then (
        if shown i then begin
          (match line t i with
          | Line (op, None) ->
              Printf.bprintf b "    %s\n" (Fmt.str "%a" Op.pp op)
          | Line (op, Some f) ->
              Printf.bprintf b "    %s  <- %s\n" (Fmt.str "%a" Op.pp op)
                (Fmt.str "%a" Fault.pp f)
          | Repeated { period; times; first; last } ->
              Printf.bprintf b
                "    ... the %d operation%s above, %d more time%s, #%d to #%d\n"
                period (plural period) times (plural times) first last);
          pp (i + 1)
        end
        else
          let rec skip j = if j < n && not (shown j) then skip (j + 1) else j in
          let j = skip i in
          Printf.bprintf b "    ... %d more line%s\n" (j - i) (plural (j - i));
          pp j)
    in
    pp 0
end

(* What a run keeps of its operations: the report's lines, its first [cap]
   operations, which the plan of the next scenarios reads, and those at
   [marked] places, which the report names. *)
type record = {
  lines : Recorder.t;
  mutable kept : (Op.t * Fault.t option) list; (* latest first *)
  mutable nkept : int;
  cap : int;
  marked : Op.t -> bool;
  mutable marks : Op.t list;
  mutable last : int; (* the last operation's index *)
}

let record ?(cap = max_int) ?(marked = fun _ -> false) () =
  {
    lines = Recorder.v ();
    kept = [];
    nkept = 0;
    cap;
    marked;
    marks = [];
    last = 0;
  }

let note r ((op : Op.t), fault) =
  Recorder.add r.lines (op, fault);
  r.last <- op.index;
  if r.nkept < r.cap then begin
    r.kept <- (op, fault) :: r.kept;
    r.nkept <- r.nkept + 1
  end;
  if r.marked op then r.marks <- op :: r.marks

(* The operations kept, in order. *)
let kept r = List.rev_map fst r.kept

(* The operation at [p], where the report names it. *)
let found r p =
  match List.find_opt (Plan.at p) r.marks with
  | Some op -> Some op
  | None -> List.find_opt (Plan.at p) (kept r)

(* The report of a failing run: the scenario's lines, the trace, and the
   replay that runs it alone. *)
let report name ~title ~tier ~hash ~key ~matched ~(trace : record) ~error lines
    =
  let b = Buffer.create 256 in
  let line k v = if v <> "" then Printf.bprintf b "  %-9s %s\n" (k ^ ":") v in
  Printf.bprintf b "%s: %s\n" name title;
  List.iter (fun (k, v) -> line k v) lines;
  line "tier" (tier_name tier);
  Option.iter
    (fun old ->
      line "matched"
        (Fmt.str "the replay named first run %s, and the workload's is now %s"
           old hash))
    matched;
  line "error" error;
  Printf.bprintf b "  trace:\n";
  Recorder.pp b trace.lines;
  Printf.bprintf b "  replay:   ~replay:%S" (Fmt.str "fs/%s/%s" hash key);
  failwith (Buffer.contents b)

let no_longer name ~hash (h, k) =
  if h = hash then
    fail name "the replay names %s, a scenario this check no longer has" k
  else
    fail name
      "the replay names %s, a scenario this check no longer has: its first run \
       was %s and is now %s. Run the check without ~replay."
      k h hash

(* {1 check} *)

let check_name = "Eio_fs.Chaos.check"

(* An operation: its kind and the path it names, or the directory that
   path is in. *)
type target = Op.kind * [ `Path | `Dir ] * string

let targets (op : Op.t) =
  [ (op.kind, `Path, op.path); (op.kind, `Dir, parent op.path) ]

(* How often the first run made each operation. *)
let made (trace : Plan.trace) =
  let t = Hashtbl.create 16 in
  List.iter
    (fun op ->
      List.iter
        (fun k ->
          Hashtbl.replace t k
            (1 + Option.value ~default:0 (Hashtbl.find_opt t k)))
        (targets op))
    trace;
  t

(* Injected failures of each operation while the run's clock stands still,
   since its last attempt that met no fault. *)
type streak = {
  counts : (target, int) Hashtbl.t;
  first : (target, int) Hashtbl.t;
  mutable at : float;
  mutable hit : ([ `Path | `Dir ] * string * int) option;
  mutable clock : float Eio.Time.clock_ty Eio.Resource.t option;
}

let streak first =
  {
    counts = Hashtbl.create 16;
    first;
    at = neg_infinity;
    hit = None;
    clock = None;
  }

let failed st (op : Op.t) =
  let now = match st.clock with Some c -> Eio.Time.now c | None -> 0. in
  if now <> st.at then begin
    st.at <- now;
    Hashtbl.reset st.counts
  end;
  List.iter
    (fun ((_, tag, at) as key) ->
      let n = 1 + Option.value ~default:0 (Hashtbl.find_opt st.counts key) in
      Hashtbl.replace st.counts key n;
      let limit =
        streak_limit tag
          (Option.value ~default:0 (Hashtbl.find_opt st.first key))
      in
      if n >= limit then begin
        st.hit <- Some (tag, at, limit);
        if n >= 2 * limit then park ();
        failwith ("Eio_fs.Chaos: " ^ outcome_text (Livelock (tag, at, limit)))
      end)
    (targets op)

let met_no_fault st (op : Op.t) =
  if Hashtbl.length st.counts > 0 then
    List.iter (Hashtbl.remove st.counts) (targets op)

let exec_check ?spaces ?cap ?budget ~first ~setup run (s : Plan.check) =
  let marked op =
    List.exists (fun (p, _) -> Plan.at p op) s.faults
    || match s.lasting with Some (_, p) -> Plan.at p op | None -> false
  in
  let trace = record ?cap ~marked () and num = numberer () in
  let pending = ref s.faults and on = ref false and st = streak first in
  let halted = ref None and floor = ref 0 and ops = ref 0 in
  let hook (mop : M.Op.t) =
    let op = num mop in
    count_op halted ops;
    add_floor halted ~budget ~floor ~at:op.index
      (Plan.check_floor ~space:mop.space op);
    Option.iter
      (fun t ->
        Hashtbl.replace t
          { Plan.kind = op.kind; path = op.path; nth = op.nth }
          mop.space)
      spaces;
    let fault =
      match s.lasting with
      | Some (l, p) ->
          if Plan.at p op then on := true;
          if !on then
            Option.map (fun c -> Fault.Fail c) (Plan.lasting_code l op)
          else None
      | None -> (
          match List.partition (fun (p, _) -> Plan.at p op) !pending with
          | (_, f) :: _, rest ->
              pending := rest;
              Some f
          | [], _ -> None)
    in
    let fault =
      match fault with
      | Some f ->
          Option.map (fun m -> (f, m)) (model_fault ~space:mop.space op f)
      | None -> None
    in
    note trace (op, Option.map fst fault);
    match fault with
    | Some (_, m) ->
        failed st op;
        Some m
    | None ->
        met_no_fault st op;
        None
  in
  let outcome =
    match
      in_loop ~halted @@ fun clock mono ->
      st.clock <- Some clock;
      let base = fresh () in
      setup (env_of clock mono base);
      let fs = M.wrap (M.Schedule.hook hook) base in
      match run (env_of clock mono fs) with
      | () -> Passed
      | exception x -> Raised x
    with
    | Ok o -> o
    | Error o -> o
  in
  let outcome =
    match (!halted, st.hit) with
    | Some o, _ -> o
    | None, Some (t, p, n) -> Livelock (t, p, n)
    | None, None -> outcome
  in
  (trace, outcome)

let check ?budget ?replay ?(setup = ignore) run =
  let name = check_name in
  let budget = budget_of name budget in
  let replay = Option.map (parse_replay name) replay in
  let spaces = Hashtbl.create 16 in
  (* A replay runs one scenario, whatever the budget. *)
  let first, out =
    exec_check ~spaces
      ?budget:(if replay = None then Some budget else None)
      ~first:(Hashtbl.create 1) ~setup run Plan.none
  in
  (match out with
  | Passed -> ()
  | Over_budget at -> refuse_budget name ~budget at
  | _ -> fail name "the run with no fault failed: %s" (outcome_text out));
  let first = kept first in
  if first = [] then
    fail name "the workload makes no filesystem operation, so it tests nothing";
  (* A run keeps no more operations than four times the first run made:
     past that, the plan of the next scenarios reads its first ones. *)
  let n = List.length first in
  let exec ?(cap = (4 * n) + 10_000) s =
    exec_check ~cap ~first:(made first) ~setup run s
  in
  let again = kept (fst (exec ~cap:(n + 1) Plan.none)) in

  if again <> first then
    fail name
      "two runs with no fault made different operations (%s); the workload \
       must be deterministic"
      (diverge first again);
  let hash = digest first in
  (* A scenario whose fault the operation never meets is none. *)
  let meets (s : Plan.check) =
    List.for_all
      (fun (p, f) ->
        match f with
        | Fault.Fail `Enospc -> Hashtbl.find_opt spaces p <> Some false
        | _ -> true)
      s.faults
  in
  let linear = List.filter meets (Plan.check_linear first) in
  let attempt ?matched tier s =
    let trace, out = exec s in
    if out <> Passed then
      report name ~title:"the workload failed under a fault." ~tier ~hash
        ~key:(Plan.check_key s) ~matched ~trace ~error:(outcome_text out)
        [ ("faults", Plan.check_text ~at:(found trace) s) ];
    kept trace
  in
  match replay with
  | Some ((h, k) as r) -> (
      (* After a fix the first run may differ: the scenario is found by the
         operations it names, which its key carries. *)
      let find l = List.find_opt (fun s -> Plan.check_key s = k) l in
      let found =
        match find linear with
        | Some s -> Some (s, Linear)
        | None -> (
            match find (List.filter meets (Plan.check_others first)) with
            | Some s -> Some (s, Sampled)
            | None ->
                List.find_map
                  (fun (s : Plan.check) ->
                    match s.faults with
                    | [ one ]
                      when String.starts_with ~prefix:(Plan.check_key s ^ "+") k
                      ->
                        let after = kept (fst (exec s)) in
                        Option.map
                          (fun s -> (s, Sampled))
                          (find (Plan.check_seconds one ~after))
                    | _ -> None)
                  linear)
      in
      match found with
      | Some (s, tier) ->
          let matched = if h = hash then None else Some h in
          ignore (attempt ?matched tier s : Plan.trace)
      | None -> no_longer name ~hash r)
  | None ->
      let n = List.length linear in
      if n > budget then
        fail name
          "the linear tier needs %d runs, more than the budget of %d, and a \
           check never passes on a partial one. Shrink the workload, or raise \
           ~budget."
          n budget;
      let others = List.filter meets (Plan.check_others first) in
      let share classes = Plan.sampled_share ~budget ~linear:n ~classes in
      (* The second faults each run allows, spread as it ends, so no run's
         operations outlive it: a share of the tier's, evenly. *)
      let singles =
        List.length
          (List.filter
             (fun (s : Plan.check) -> List.length s.faults = 1)
             linear)
      in
      let quota =
        let sh = share (if others = [] then 1 else 2) in
        (sh + singles - 1) / max 1 singles
      in
      let pairs =
        List.concat_map
          (fun (s : Plan.check) ->
            let after = attempt Linear s in
            match s.faults with
            | [ one ] -> Plan.spread quota (Plan.check_seconds one ~after)
            | _ -> [])
          linear
      in
      let classes = List.filter (( <> ) []) [ others; pairs ] in
      let share = share (List.length classes) in
      List.iter
        (fun c ->
          List.iter
            (fun s -> ignore (attempt Sampled s : Plan.trace))
            (Plan.spread share c))
        classes

(* {1 Crash} *)

module Crash = struct
  type op_kind = M.Crash.op_kind =
    | Data
    | Truncate
    | Create
    | Mkdir
    | Unlink
    | Rmdir
    | Symlink of { target : string }
    | Rename of { dest : string }
    | Chmod of { perm : int }
    | Chown of { uid : int64; gid : int64 }

  type unsynced = M.Crash.unsynced = {
    path : string;
    kind : op_kind;
    offset : int;
    length : int;
    seq : int;
  }

  let pp_unsynced = M.Crash.pp_unsynced

  type policy = Drop_unsynced | Survive of (unsynced -> bool)
  type t = M.Crash.t

  let v = function
    | Drop_unsynced -> M.Crash.v ~policy:Drop_unsynced ()
    | Survive p -> M.Crash.v ~policy:(Survive p) ()

  let wrap t base = (M.Crash.wrap t base :> Eio.Fs.dir_ty Eio.Path.t)
  let unsynced = M.Crash.unsynced
  let cut = M.Crash.post_crash

  (* {2 The check} *)

  let name = "Eio_fs.Chaos.Crash.check"

  (* The draws a state makes go to [trail], each with the choice it names,
     out of range or not, and how many it had. *)
  let policy_of trail : Plan.state -> M.Crash.policy = function
    | Drop -> Drop_unsynced
    | Keep -> Survive (fun _ -> true)
    | Enum v | Shape v ->
        let v = Array.of_list v and pos = ref 0 in
        Choose
          (fun n ->
            let c = if !pos < Array.length v then v.(!pos) else 0 in
            incr pos;
            trail := (c, n) :: !trail;
            if c < n then c else 0)
    | Seed s ->
        let rng = Random.State.make [| s |] in
        Choose
          (fun n ->
            let c = Random.State.int rng n in
            trail := (c, n) :: !trail;
            c)

  (* Whether the cut allows [st], given the draws it made: no choice out of
     range, and no choice for a draw it did not make. *)
  let allows (st : Plan.state) trail =
    match st with
    | Enum v | Shape v ->
        List.for_all (fun (c, n) -> c < n) trail
        && List.length v <= List.length trail
    | Drop | Keep | Seed _ -> true

  type result = {
    trace : record;
    cut_op : Op.t option; (* the operation the cut fell before *)
    seqs : (int * int) list;
        (* for each operation, latest first, the last change before it *)
    rtrace : Plan.trace;
    acked_at : int list; (* the operation each ack came after *)
    lost : unsynced list;
    left : string list;
    trail : (int * int) list;
    allowed : bool;
    error : error option;
  }

  and error = Workload of outcome | Recover of outcome | Again of exn

  (* A phase of a run: [f] on a crash-tracked [base] whose power is cut
     before the operation at [cut], if it comes. It returns the trace, the
     outcome, and the unsynced changes the cut lost. *)
  let phase ?(guard = ignore) ~kept_going ~trace ~ctl ~faults ~cut ~cut_op ~seqs
      ~stop fs f =
    let num = numberer () and pending = ref faults in
    let after = ref 0 and lost = ref [] and run_sw = ref None in
    let hook (mop : M.Op.t) =
      let op = num mop in
      guard op;
      if not (M.Crash.is_crashed ctl) then begin
        (seqs :=
           match !seqs with
           | (s, _) :: rest when s = M.Crash.seq ctl -> (s, op.index) :: rest
           | l -> (M.Crash.seq ctl, op.index) :: l);
        match cut with
        | Some p when Plan.at p op ->
            cut_op := Some op;
            lost := M.Crash.unsynced ctl;
            M.Crash.crash ctl;
            stop := !run_sw
        | _ -> ()
      end;
      if M.Crash.is_crashed ctl then begin
        (* The operation the cut falls before, and every one after, is
           cancelled; once every fiber waits, so is the run. One that catches
           the cancellation still reaches this hook, and is stopped. *)
        incr after;
        if !after > 1000 then begin
          kept_going := true;
          park ()
        end;
        raise (Eio.Cancel.Cancelled Cut)
      end;
      match List.partition (fun (p, _) -> Plan.at p op) !pending with
      | (_, f) :: _, rest ->
          pending := rest;
          let m = model_fault ~space:mop.space op f in
          note trace (op, Option.map (fun _ -> f) m);
          m
      | [], _ ->
          note trace (op, None);
          None
    in
    let wrapped = M.wrap (M.Schedule.hook hook) fs in
    let outcome =
      match
        Eio.Switch.run (fun sw ->
            run_sw := Some sw;
            f wrapped)
      with
      | () ->
          stop := None;
          Passed
      | exception _ when M.Crash.is_crashed ctl ->
          stop := None;
          Passed
      | exception x -> Raised x
    in
    let outcome = if !kept_going then Kept_going else outcome in
    (outcome, !lost)

  let exec ?cap ?budget ~setup run ~recover (s : Plan.crash) =
    let trail = ref [] and rtrail = ref [] in
    let acked = ref [] and acked_at = ref [] in
    let marked op = List.exists (fun (p, _) -> Plan.at p op) s.cfaults in
    let trace = record ?cap ~marked () and rtrace = record ?cap () in
    let lost = ref [] and left = ref [] in
    let cut_op = ref None and seqs = ref [] in
    let kept_run = ref false and kept_recover = ref false in
    let recovering = ref false in
    let halted = ref None and floor = ref 0 and ops = ref 0 in
    (* A cut stops the run, as nothing runs after a power cut: once every
       fiber waits, the phase's switch fails, so a fiber that waits on a
       timer or another fiber stops too. *)
    let stop = ref None in
    let idle () =
      match !stop with
      | Some sw ->
          stop := None;
          Eio.Switch.fail sw Cut;
          true
      | None -> false
    in
    let error =
      match
        in_loop ~idle ~halted @@ fun clock mono ->
        let base = fresh () in
        setup (env_of clock mono base);
        let ctl =
          M.Crash.v ~policy:(policy_of trail s.state) ~block_size:512 ()
        in
        let fs = M.Crash.wrap ctl base in
        let out, l =
          phase
            ~guard:(fun op ->
              count_op halted ops;
              add_floor halted ~budget ~floor ~at:op.index (Plan.crash_floor op))
            ~kept_going:kept_run ~trace ~ctl ~faults:s.cfaults ~cut:s.cut
            ~cut_op ~seqs ~stop fs
            (fun fs ->
              run
                ~ack:(fun v ->
                  if not (M.Crash.is_crashed ctl) then begin
                    acked := v :: !acked;
                    acked_at := trace.last :: !acked_at
                  end)
                (env_of clock mono fs))
        in
        if not (M.Crash.is_crashed ctl) then begin
          lost := M.Crash.unsynced ctl;
          M.Crash.crash ctl
        end
        else lost := l;
        left := M.Crash.left ctl;
        let acked = List.rev !acked in
        match out with
        | Passed -> (
            let rctl, rcut =
              match s.rcut with
              | Some (j, st) ->
                  (M.Crash.v ~policy:(policy_of rtrail st) ~block_size:512 (), j)
              | None -> (M.Crash.v ~policy:Drop_unsynced (), None)
            in
            let rfs = M.Crash.wrap rctl base in
            recovering := true;
            let rout, _ =
              phase
                ~guard:(fun _ -> count_op halted ops)
                ~kept_going:kept_recover ~trace:rtrace ~ctl:rctl ~faults:[]
                ~cut:rcut ~cut_op:(ref None) ~seqs:(ref []) ~stop rfs
                (fun fs -> recover ~acked (env_of clock mono fs))
            in
            match (rout, s.rcut) with
            | Passed, None -> None
            | Passed, Some _ -> (
                if not (M.Crash.is_crashed rctl) then M.Crash.crash rctl;
                ignore (M.Crash.post_crash rctl : Eio.Fs.dir_ty Eio.Path.t);
                match recover ~acked (env_of clock mono base) with
                | () -> None
                | exception x -> Some (Again x))
            | o, _ -> Some (Recover o))
        | o -> Some (Workload o)
      with
      | Ok e -> e
      | Error o ->
          if !kept_run then Some (Workload Kept_going)
          else if !kept_recover then Some (Recover Kept_going)
          else if !recovering then Some (Recover o)
          else Some (Workload o)
    in
    {
      trace;
      cut_op = !cut_op;
      seqs = !seqs;
      rtrace = kept rtrace;
      acked_at = List.rev !acked_at;
      lost = !lost;
      left = !left;
      trail = !trail;
      allowed =
        allows s.state !trail
        && Option.fold ~none:true
             ~some:(fun (_, st) -> allows st !rtrail)
             s.rcut;
      error;
    }

  (* The values acknowledged, by the operation each came after: their type
     has no printer, and recover names the one it misses. *)
  let acked_text = function
    | [] -> "none"
    | ats ->
        let n = List.length ats in
        let shown = List.filteri (fun i _ -> i < 8) ats in
        Fmt.str "%d value%s, after %s%s" n
          (if n = 1 then "" else "s")
          (String.concat ", "
             (List.map
                (function 0 -> "the start" | i -> Fmt.str "#%d" i)
                shown))
          (if n > 8 then Fmt.str " and %d more" (n - 8) else "")

  let state_text = function
    | Plan.Drop -> "every unsynced change lost"
    | Keep -> "every change kept, as after a process kill"
    | Enum _ as st ->
        Fmt.str "state %s of those the cut allows, tried one by one"
          (Plan.state_key st)
    | (Seed _ | Shape _) as st ->
        Fmt.str
          "state %s, drawn at random: the cut allows more states than its \
           share of the tier"
          (Plan.state_key st)

  let cut_text ?at = function
    | None -> "at the end"
    | Some p -> "before " ^ Plan.pos_text ?at p

  (* A change, numbered by the operation that made it, as the trace shows
     it. *)
  let change_text seqs (u : unsynced) =
    let n =
      match List.find_opt (fun (s, _) -> s < u.seq) seqs with
      | Some (_, i) -> Fmt.str "#%d " i
      | None -> ""
    in
    match u.kind with
    | Data -> Fmt.str "%swrite %s@%d+%d" n u.path u.offset u.length
    | Truncate -> Fmt.str "%struncate %s to %d" n u.path u.offset
    | Create -> Fmt.str "%screate %s" n u.path
    | Mkdir -> Fmt.str "%smkdir %s" n u.path
    | Unlink -> Fmt.str "%sunlink %s" n u.path
    | Rmdir -> Fmt.str "%srmdir %s" n u.path
    | Symlink { target } -> Fmt.str "%ssymlink %s -> %s" n u.path target
    | Rename { dest } -> Fmt.str "%srename %s to %s" n u.path dest
    | Chmod { perm } -> Fmt.str "%schmod %s %o" n u.path perm
    | Chown { uid; gid } -> Fmt.str "%schown %s %Ld:%Ld" n u.path uid gid

  let lines (s : Plan.crash) (r : result) =
    let at p =
      match r.cut_op with
      | Some op when Plan.at p op -> Some op
      | _ -> found r.trace p
    in
    [
      ("cut", Fmt.str "%s, %s" (cut_text ~at s.cut) (state_text s.state));
      ("faults", Plan.faults_text ~at s.cfaults);
      ( "recovery",
        match s.rcut with
        | None -> ""
        | Some (j, st) ->
            Fmt.str "cut in recover %s, %s, then recover again" (cut_text j)
              (state_text st) );
      ("acked", acked_text r.acked_at);
      (* What the cut may have lost: all of it, none of it, or some. *)
      (let changes =
         String.concat "; " (List.map (change_text r.seqs) r.lost)
       in
       match s.state with
       | Drop -> ("lost", changes)
       | Keep -> ("lost", "")
       | Enum _ | Seed _ | Shape _ -> ("unsynced", changes));
      ( "left",
        match s.state with
        | Drop | Keep -> ""
        | Enum _ | Seed _ | Shape _ ->
            let n = List.length r.left in
            (List.filteri (fun i _ -> i < 8) r.left
            @ if n > 8 then [ Fmt.str "and %d more files" (n - 8) ] else [])
            |> String.concat "\n            " );
    ]

  let check ?budget ?replay ?(setup = ignore) run ~recover =
    let budget = budget_of name budget in
    let replay = Option.map (parse_replay name) replay in
    let exec ?cap ?budget s = exec ?cap ?budget ~setup run ~recover s in
    (* A replay runs one scenario, whatever the budget. *)
    let first =
      exec ?budget:(if replay = None then Some budget else None) Plan.base
    in
    (match first.error with
    | Some (Workload (Over_budget at)) -> refuse_budget name ~budget at
    | Some (Workload o) ->
        fail name "the run with no cut failed: %s" (outcome_text o)
    | _ -> ());
    let first_ops = kept first.trace in
    if first_ops = [] then
      fail name
        "the workload makes no filesystem operation, so it tests nothing";
    let n = List.length first_ops in
    let again = kept (exec ~cap:(n + 1) Plan.base).trace in
    (* A run keeps no more operations than four times the first run made:
       past that, the plan of the next scenarios reads its first ones. *)
    let exec = exec ~cap:((4 * (n + List.length first.rtrace)) + 10_000) in
    if again <> first_ops then
      fail name
        "two runs with no cut made different operations (%s); the workload \
         must be deterministic"
        (diverge first_ops again);
    let hash = digest first_ops in
    let sizes =
      match
        in_loop (fun clock mono ->
            let d = fresh () in
            setup (env_of clock mono d);
            files d)
      with
      | Ok l -> l
      | Error o -> fail name "setup failed: %s" (outcome_text o)
    in
    (* Whether recover looked at the directory in some run. *)
    let reads = ref (first.rtrace <> []) in
    let tears = Plan.tears ~sizes first_ops in
    (* A run's result is dropped once the scenarios it leads to are drawn
       from it, so memory stays that of one run. *)
    let runs = ref 0 in
    let attempt ?matched tier s =
      incr runs;
      let r = exec s in
      (* A shape is named by the choices it made, which a replay makes
         again. *)
      let s =
        match s.state with
        | Shape _ ->
            {
              s with
              state =
                Shape
                  (List.rev_map (fun (c, n) -> if c < n then c else 0) r.trail);
            }
        | _ -> s
      in
      if r.rtrace <> [] then reads := true;
      (match r.error with
      | None -> ()
      | Some e ->
          let title, err =
            match e with
            | Workload o -> ("the workload failed.", outcome_text o)
            | Recover o -> ("recover rejected what a cut left.", outcome_text o)
            | Again x ->
                ( "recover, run again after a cut during recovery, rejected \
                   what was left.",
                  Printexc.to_string x )
          in
          report name ~title ~tier ~hash ~key:(Plan.crash_key s) ~matched
            ~trace:r.trace ~error:err (lines s r));
      r
    in
    let trace = first_ops in
    let cuts = Plan.crash_cuts trace in
    let faults = Plan.crash_faults ~tears trace in
    match replay with
    | Some ((h, k) as r) -> (
        let matched = if h = hash then None else Some h in
        let find l = List.find_opt (fun s -> Plan.crash_key s = k) l in
        (* The scenarios each run of [l] leads to, one run at a time. *)
        let find_after l f = List.find_map (fun s -> find (f s (exec s))) l in
        let rcuts s r = Plan.rcuts s ~recover:r.rtrace in
        let candidates =
          [
            (Linear, fun () -> find (cuts @ faults));
            (Linear, fun () -> find_after cuts rcuts);
            (Sampled, fun () -> find (Plan.crash_others ~tears trace));
            ( Sampled,
              fun () ->
                find_after faults (fun (s : Plan.crash) r ->
                    match s.cfaults with
                    | [ one ] when s.state = Drop ->
                        Plan.crash_seconds ~tears one ~after:(kept r.trace)
                    | _ -> []) );
            (Sampled, fun () -> find_after faults rcuts);
          ]
        in
        let found =
          List.find_map
            (fun (tier, f) -> Option.map (fun s -> (s, tier)) (f ()))
            candidates
        in
        let found =
          match found with
          | Some _ -> found
          | None -> (
              (* A state a cut allows, named on top of a cut of the plan. *)
              match Plan.split_state_key k with
              | Some (k', st) -> (
                  match List.find_opt (fun s -> Plan.crash_key s = k') cuts with
                  | Some s -> Some ({ s with state = st }, Sampled)
                  | None -> None)
              | None -> None)
        in
        (* A state drawn at random is one of a cut that allows more than two
           states, which the check may not try one by one, by a seed below a
           fourth of the budget. *)
        let drawn (s : Plan.crash) =
          let more (st : Plan.state) =
            Plan.next_choices (exec { s with state = st }).trail
          in
          match s.state with
          | Seed n when n < 0 || n >= budget / 4 -> false
          | Seed _ | Shape _ -> (
              match more (Enum []) with
              | None -> false
              | Some v -> Option.is_some (more (Enum v)))
          | Drop | Keep | Enum _ -> true
        in
        match found with
        | Some (s, _) when not (drawn s && (exec s).allowed) ->
            no_longer name ~hash r
        | Some (s, tier) -> ignore (attempt ?matched tier s)
        | None -> no_longer name ~hash r)
    | None ->
        let over n =
          fail name
            "the linear tier needs %d runs, more than the budget of %d, and a \
             check never passes on a partial one. Shrink the workload, or \
             raise ~budget."
            n budget
        in
        let lin = cuts @ faults in
        if List.length lin > budget then over (List.length lin);
        (* The cuts during recover come from the traces of the cuts, so the
           tier's size is known once those ran; past the budget, only their
           number is kept. *)
        let nrcuts = ref 0 in
        let rcuts =
          List.concat_map
            (fun s ->
              let l = Plan.rcuts s ~recover:(attempt Linear s).rtrace in
              nrcuts := !nrcuts + List.length l;
              if List.length lin + !nrcuts > budget then [] else l)
            cuts
        in
        let linear = List.length lin + !nrcuts in
        (* The second faults and the cuts during recover each fault allows,
           spread as each run ends, so no run's operations outlive it: an
           even share of what the sampled tier can spend. *)
        let quota =
          let total = Plan.sampled_share ~budget ~linear ~classes:1 in
          let k = List.length faults in
          (total + k - 1) / max 1 k
        in
        let after_faults =
          List.map
            (fun (s : Plan.crash) ->
              let r = attempt Linear s in
              let pairs =
                match s.cfaults with
                | [ one ] when s.state = Drop ->
                    Plan.crash_seconds ~tears one ~after:(kept r.trace)
                | _ -> []
              in
              ( Plan.spread quota pairs,
                Plan.spread quota (Plan.rcuts s ~recover:r.rtrace) ))
            faults
        in
        if linear > budget then over linear;
        List.iter (fun r -> ignore (attempt Linear r)) rcuts;
        (* Sampled tier. *)
        let thunks = List.map (fun s () -> ignore (attempt Sampled s)) in
        let others = thunks (Plan.crash_others ~tears trace) in
        let pairs = List.concat_map (fun (p, _) -> thunks p) after_faults in
        let recovery = List.concat_map (fun (_, r) -> thunks r) after_faults in
        (* Every state each cut allows, enumerated while the draws met so
           far allow no more than the cut's share, and drawn at random past
           that. Each cut gets an even share of what is left: first every cut
           whose states fit in it, then, with what those left, the others,
           smallest first. Of a cut past its share only its size is kept, so
           memory stays that of one cut: one that fits in its new share is
           enumerated again from its first state. *)
        let drops = List.filter (fun (s : Plan.crash) -> s.state = Drop) cuts in
        let enumerate (s : Plan.crash) tree spent allowance next =
          let rec go = function
            | None -> None
            | Some v
              when Plan.Draws.estimate tree > allowance || !spent >= allowance
              ->
                Some v
            | Some v ->
                incr spent;
                let r = attempt Sampled { s with state = Enum v } in
                Plan.Draws.add tree r.trail;
                go (Plan.next_choices r.trail)
          in
          go next
        in
        (* The states of cut [s] one by one, up to [allowance] runs: [None]
           once they are all tried, else the draws' estimate. *)
        let enumerate_from_first s spent allowance =
          let tree = Plan.Draws.empty () in
          incr spent;
          let r = attempt Sampled { s with state = Enum [] } in
          Plan.Draws.add tree r.trail;
          match
            enumerate s tree spent allowance (Plan.next_choices r.trail)
          with
          | None -> None
          | Some _ -> Some (Plan.Draws.estimate tree)
        in
        let states left =
          let rec first left acc = function
            | [] -> (left, List.rev acc)
            | (s : Plan.crash) :: rest as cuts ->
                let allowance = max 1 (left / List.length cuts) in
                let spent = ref 0 in
                let acc =
                  match enumerate_from_first s spent allowance with
                  | None -> acc
                  | Some estimate -> (s, estimate, !spent) :: acc
                in
                first (left - !spent) acc rest
          in
          let left, large = first left [] drops in
          let large =
            List.stable_sort (fun (_, a, _) (_, b, _) -> Int.compare a b) large
          in
          let rec share_out left = function
            | [] -> ()
            | (s, estimate, before) :: rest as cuts ->
                let share = max 0 (left / List.length cuts) in
                let spent = ref 0 in
                if share = 0 then ()
                else if estimate <= before + share then
                  ignore
                    (enumerate_from_first s spent (before + share) : int option)
                else begin
                  (* The first draw is a seed, which needs no widths: its
                      draws give them to the others. *)
                  incr spent;
                  let r =
                    attempt Sampled { s with state = Plan.shape ~widths:[] 0 }
                  in
                  let widths = List.rev_map snd r.trail in
                  for i = 1 to share - 1 do
                    incr spent;
                    ignore
                      (attempt Sampled { s with state = Plan.shape ~widths i })
                  done
                end;
                share_out (left - !spent) rest
          in
          share_out left large
        in
        let classes = List.filter (( <> ) []) [ others; pairs; recovery ] in
        let total = Plan.sampled_share ~budget ~linear ~classes:1 in
        let before = !runs in
        let share = total / 2 / max 1 (List.length classes) in
        List.iter
          (fun c -> List.iter (fun run -> run ()) (Plan.spread share c))
          classes;
        states (total - (!runs - before));
        if not !reads then
          fail name
            "recover makes no filesystem operation in any run, so it checks \
             nothing a cut leaves: read in recover what setup and the workload \
             made."
end

module Private = Chaos_model
