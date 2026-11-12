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
        | `Write -> Write_error c
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
   spins for ever under a lasting condition. Once one operation on one path,
   or one kind of operation in one directory, as a retry under a fresh name
   makes, has failed this many times more than three tries of each the first
   run made, while the clock stands still, it raises [Failure] instead, and
   after twice as many it waits for ever. *)
let streak_limit = 10_000

type outcome =
  | Passed
  | Raised of exn
  | Deadlock
  | Time_limit
  | Kept_going
  | Livelock of [ `Path | `Dir ] * string * int

let outcome_text = function
  | Passed -> "none"
  | Raised x -> Printexc.to_string x
  | Deadlock -> "the run hangs: every fiber waits and no timer is due"
  | Time_limit ->
      "the run hangs: it is still running after an hour of its clock"
  | Kept_going ->
      "the run hangs: it kept going for 1000 operations after the power cut, \
       so it catches Eio.Cancel.Cancelled"
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

(* A cut fails the run's switch with it, which cancels every fiber of the
   run. *)
exception Cut

let env_of clock mono (d : _ Eio.Path.t) : env =
  object
    method fs = (d :> Eio.Fs.dir_ty Eio.Path.t)
    method clock = clock
    method mono_clock = mono
  end

let in_loop ?idle main =
  match Quiet_mock.run ?idle ~limit:3600. main with
  | v -> Ok v
  | exception Quiet_mock.Deadlock_detected -> Error Deadlock
  | exception Quiet_mock.Time_limit -> Error Time_limit

(* Park the fiber where no cancel reaches, so even a loop that catches
   everything stops; the loop then reports a deadlock. *)
let park () = Eio.Cancel.protect Eio.Fiber.await_cancel
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

type run_trace = (Op.t * Fault.t option) list

let ops (t : run_trace) = List.map fst t

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

(* The report of a failing run: the scenario's lines, the trace, and the
   replay that runs it alone. *)
let report name ~title ~tier ~hash ~key ~matched ~(trace : run_trace) ~error
    lines =
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
  List.iter
    (fun (op, fault) ->
      match fault with
      | None -> Printf.bprintf b "    %s\n" (Fmt.str "%a" Op.pp op)
      | Some f ->
          Printf.bprintf b "    %s  <- %s\n" (Fmt.str "%a" Op.pp op)
            (Fmt.str "%a" Fault.pp f))
    trace;
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

let parent path =
  match String.rindex_opt path '/' with
  | Some 0 -> "/"
  | Some i -> String.sub path 0 i
  | None -> "."

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
        streak_limit
        + (3 * Option.value ~default:0 (Hashtbl.find_opt st.first key))
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

let exec_check ?(spaces = Hashtbl.create 1) ~first ~setup run (s : Plan.check) =
  let trace = ref [] and num = numberer () in
  let pending = ref s.faults and on = ref false and st = streak first in
  let hook (mop : M.Op.t) =
    let op = num mop in
    Hashtbl.replace spaces
      { Plan.kind = op.kind; path = op.path; nth = op.nth }
      mop.space;
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
    trace := (op, Option.map fst fault) :: !trace;
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
      in_loop @@ fun clock mono ->
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
    match st.hit with Some (t, p, n) -> Livelock (t, p, n) | None -> outcome
  in
  (List.rev !trace, outcome)

let check ?budget ?replay ?(setup = ignore) run =
  let name = check_name in
  let budget = budget_of name budget in
  let replay = Option.map (parse_replay name) replay in
  let spaces = Hashtbl.create 16 in
  let first, out =
    exec_check ~spaces ~first:(Hashtbl.create 1) ~setup run Plan.none
  in
  if out <> Passed then
    fail name "the run with no fault failed: %s" (outcome_text out);
  let first = ops first in
  if first = [] then
    fail name "the workload makes no filesystem operation, so it tests nothing";
  let exec s = exec_check ~first:(made first) ~setup run s in
  let again = ops (fst (exec Plan.none)) in

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
        [ ("faults", Plan.check_text s) ];
    ops trace
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
                        let after = ops (fst (exec s)) in
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
      let afters = List.map (fun s -> (s, attempt Linear s)) linear in
      let pairs =
        List.concat_map
          (fun ((s : Plan.check), after) ->
            match s.faults with
            | [ one ] -> Plan.check_seconds one ~after
            | _ -> [])
          afters
      in
      let classes =
        List.filter (( <> ) [])
          [ List.filter meets (Plan.check_others first); pairs ]
      in
      let share =
        Plan.sampled_share ~budget ~linear:n ~classes:(List.length classes)
      in
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

  (* At most this many states a cut allows are enumerated; past it, they are
     drawn at random. *)
  let enum_limit = 64

  let policy_of trail : Plan.state -> M.Crash.policy = function
    | Drop -> Drop_unsynced
    | Keep -> Survive (fun _ -> true)
    | Enum v ->
        let v = Array.of_list v and pos = ref 0 in
        Choose
          (fun n ->
            let c = if !pos < Array.length v then v.(!pos) else 0 in
            incr pos;
            let c = if c < n then c else 0 in
            trail := (c, n) :: !trail;
            c)
    | Seed s ->
        let rng = Random.State.make [| s |] in
        Choose (fun n -> Random.State.int rng n)

  type 'a result = {
    trace : run_trace;
    rtrace : Plan.trace;
    acked : 'a list;
    lost : unsynced list;
    trail : (int * int) list;
    error : error option;
  }

  and error = Workload of outcome | Recover of outcome | Again of exn

  (* A phase of a run: [f] on a crash-tracked [base] whose power is cut
     before the operation at [cut], if it comes. It returns the trace, the
     outcome, and the unsynced changes the cut lost. *)
  let phase ~kept_going ~trace ~ctl ~faults ~cut ~stop fs f =
    let num = numberer () and pending = ref faults in
    let after = ref 0 and lost = ref [] and run_sw = ref None in
    let hook (mop : M.Op.t) =
      let op = num mop in
      if not (M.Crash.is_crashed ctl) then
        begin match cut with
        | Some p when Plan.at p op ->
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
          trace := (op, Option.map (fun _ -> f) m) :: !trace;
          m
      | [], _ ->
          trace := (op, None) :: !trace;
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

  let exec ~setup run ~recover (s : Plan.crash) =
    let trail = ref [] in
    let acked = ref [] and trace = ref [] and rtrace = ref [] in
    let lost = ref [] in
    let kept_run = ref false and kept_recover = ref false in
    let recovering = ref false in
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
        in_loop ~idle @@ fun clock mono ->
        let base = fresh () in
        setup (env_of clock mono base);
        let ctl =
          M.Crash.v ~policy:(policy_of trail s.state) ~block_size:512 ()
        in
        let fs = M.Crash.wrap ctl base in
        let out, l =
          phase ~kept_going:kept_run ~trace ~ctl ~faults:s.cfaults ~cut:s.cut
            ~stop fs (fun fs ->
              run
                ~ack:(fun v ->
                  if not (M.Crash.is_crashed ctl) then acked := v :: !acked)
                (env_of clock mono fs))
        in
        if not (M.Crash.is_crashed ctl) then begin
          lost := M.Crash.unsynced ctl;
          M.Crash.crash ctl
        end
        else lost := l;
        let acked = List.rev !acked in
        match out with
        | Passed -> (
            let rctl, rcut =
              match s.rcut with
              | Some (j, st) ->
                  ( M.Crash.v ~policy:(policy_of (ref []) st) ~block_size:512 (),
                    j )
              | None -> (M.Crash.v ~policy:Drop_unsynced (), None)
            in
            let rfs = M.Crash.wrap rctl base in
            recovering := true;
            let rout, _ =
              phase ~kept_going:kept_recover ~trace:rtrace ~ctl:rctl ~faults:[]
                ~cut:rcut ~stop rfs (fun fs ->
                  recover ~acked (env_of clock mono fs))
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
      trace = List.rev !trace;
      rtrace = ops (List.rev !rtrace);
      acked = List.rev !acked;
      lost = !lost;
      trail = !trail;
      error;
    }

  let state_text = function
    | Plan.Drop -> "every unsynced change lost"
    | Keep -> "every change kept, as after a process kill"
    | Enum _ as st ->
        Fmt.str "state %s of those the cut allows, tried one by one"
          (Plan.state_key st)
    | Seed _ as st ->
        Fmt.str
          "state %s, drawn at random: the cut allows more than the %d states \
           the check would try one by one"
          (Plan.state_key st) enum_limit

  let cut_text = function
    | None -> "at the end"
    | Some p -> "before " ^ Plan.pos_text p

  let lines (s : Plan.crash) (r : _ result) =
    [
      ("cut", Fmt.str "%s, %s" (cut_text s.cut) (state_text s.state));
      ("faults", Plan.faults_text s.cfaults);
      ( "recovery",
        match s.rcut with
        | None -> ""
        | Some (j, st) ->
            Fmt.str "cut %s of recover, %s, then recover again" (cut_text j)
              (state_text st) );
      ("acked", Fmt.str "%d value(s)" (List.length r.acked));
      (* What the cut may have lost: all of it, none of it, or some. *)
      (let changes =
         Fmt.str "%a" Fmt.(list ~sep:(any "; ") pp_unsynced) r.lost
       in
       match s.state with
       | Drop -> ("lost", changes)
       | Keep -> ("lost", "")
       | Enum _ | Seed _ -> ("unsynced", changes));
    ]

  let check ?budget ?replay ?(setup = ignore) run ~recover =
    let budget = budget_of name budget in
    let replay = Option.map (parse_replay name) replay in
    let exec s = exec ~setup run ~recover s in
    let first = exec Plan.base in
    (match first.error with
    | Some (Workload o) ->
        fail name "the run with no cut failed: %s" (outcome_text o)
    | _ -> ());
    let first_ops = ops first.trace in
    if first_ops = [] then
      fail name
        "the workload makes no filesystem operation, so it tests nothing";
    let again = ops (exec Plan.base).trace in
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
    let runs = ref 0 and results = Hashtbl.create 64 in
    let attempt ?matched tier s =
      incr runs;
      let r = exec s in
      if r.rtrace <> [] then reads := true;
      Hashtbl.replace results (Plan.crash_key s) r;
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
    let result s =
      match Hashtbl.find_opt results (Plan.crash_key s) with
      | Some r -> r
      | None -> exec s
    in
    let trace = first_ops in
    let cuts = Plan.crash_cuts trace in
    let faults = Plan.crash_faults ~tears trace in
    match replay with
    | Some ((h, k) as r) -> (
        let matched = if h = hash then None else Some h in
        let find l = List.find_opt (fun s -> Plan.crash_key s = k) l in
        let rcuts_of l =
          List.concat_map (fun s -> Plan.rcuts s ~recover:(result s).rtrace) l
        in
        let candidates () =
          [
            (Linear, fun () -> cuts @ faults);
            (Linear, fun () -> rcuts_of cuts);
            (Sampled, fun () -> Plan.crash_others ~tears trace);
            ( Sampled,
              fun () ->
                List.concat_map
                  (fun (s : Plan.crash) ->
                    match s.cfaults with
                    | [ one ] when s.state = Drop ->
                        Plan.crash_seconds ~tears one
                          ~after:(ops (result s).trace)
                    | _ -> [])
                  faults );
            (Sampled, fun () -> rcuts_of faults);
          ]
        in
        let found =
          List.find_map
            (fun (tier, l) -> Option.map (fun s -> (s, tier)) (find (l ())))
            (candidates ())
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
        match found with
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
           tier's size is known once those ran. *)
        List.iter (fun s -> ignore (attempt Linear s)) lin;
        let rcuts =
          List.concat_map
            (fun s -> Plan.rcuts s ~recover:(result s).rtrace)
            cuts
        in
        let n = List.length lin + List.length rcuts in
        if n > budget then over n;
        List.iter (fun r -> ignore (attempt Linear r)) rcuts;
        let linear = !runs in
        (* Sampled tier. *)
        let thunks = List.map (fun s () -> ignore (attempt Sampled s)) in
        let others = thunks (Plan.crash_others ~tears trace) in
        let pairs =
          List.concat_map
            (fun (s : Plan.crash) ->
              match s.cfaults with
              | [ one ] when s.state = Drop ->
                  thunks
                    (Plan.crash_seconds ~tears one ~after:(ops (result s).trace))
              | _ -> [])
            faults
        in
        let recovery =
          List.concat_map
            (fun s -> thunks (Plan.rcuts s ~recover:(result s).rtrace))
            faults
        in
        (* Every state each cut allows, enumerated while the draws met so
           far allow no more than the limit, and drawn at random past that.
           Each cut gets an even share of what is left. *)
        let drops = List.filter (fun (s : Plan.crash) -> s.state = Drop) cuts in
        let states left =
          let rec loop left = function
            | [] -> ()
            | (s : Plan.crash) :: rest as cuts ->
                let allowance = max 1 (left / List.length cuts) in
                let limit = min allowance enum_limit in
                let tree = Plan.Draws.empty () in
                let r = attempt Sampled { s with state = Enum [] } in
                Plan.Draws.add tree r.trail;
                let spent = ref 1 in
                let rec go = function
                  | None -> true
                  | Some _ when Plan.Draws.lower_bound tree > limit -> false
                  | Some v ->
                      incr spent;
                      let r = attempt Sampled { s with state = Enum v } in
                      Plan.Draws.add tree r.trail;
                      go (Plan.next_choices r.trail)
                in
                if not (go (Plan.next_choices r.trail)) then
                  for seed = 0 to allowance - !spent - 1 do
                    incr spent;
                    ignore (attempt Sampled { s with state = Seed seed })
                  done;
                loop (left - !spent) rest
          in
          loop left drops
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
