open! Hermetic

module Op = struct
  type kind =
    [ `Open_read
    | `Open_write
    | `Stat
    | `Read
    | `Read_dir
    | `Write
    | `Truncate
    | `Sync
    | `Change ]

  type t = {
    index : int;
    kind : kind;
    path : string;
    nth : int;
    file_offset : int;
    length : int;
    dest : string option;
  }

  let kind_name : kind -> string = function
    | `Open_read -> "open"
    | `Open_write -> "open-write"
    | `Stat -> "stat"
    | `Read -> "read"
    | `Read_dir -> "read-dir"
    | `Write -> "write"
    | `Truncate -> "truncate"
    | `Sync -> "sync"
    | `Change -> "change"

  let pp ppf t =
    match t.kind with
    | `Read | `Write ->
        Fmt.pf ppf "#%d %s %s@%d+%d" t.index (kind_name t.kind) t.path
          t.file_offset t.length
    | `Truncate ->
        Fmt.pf ppf "#%d truncate %s to %d" t.index t.path t.file_offset
    | `Change when Option.is_some t.dest ->
        Fmt.pf ppf "#%d rename %s to %s" t.index t.path (Option.get t.dest)
    | k -> Fmt.pf ppf "#%d %s %s" t.index (kind_name k) t.path
end

module Fault = struct
  type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]
  type t = Fail of code | Short of int | Torn of int | Swap_symlink of string

  let code_name : code -> string = function
    | `Enoent -> "ENOENT"
    | `Eacces -> "EACCES"
    | `Eperm -> "EPERM"
    | `Erofs -> "EROFS"
    | `Enospc -> "ENOSPC"
    | `Eio -> "EIO"

  let pp ppf = function
    | Fail c -> Fmt.pf ppf "fails with %s" (code_name c)
    | Short n -> Fmt.pf ppf "moves %d byte(s)" (max 1 n)
    | Torn n -> Fmt.pf ppf "is torn after %d byte(s)" n
    | Swap_symlink t -> Fmt.pf ppf "is swapped for a link to %s" t

  let key = function
    | Fail c -> String.lowercase_ascii (code_name c)
    | Short n -> Fmt.str "short%d" n
    | Torn n -> Fmt.str "torn%d" n
    | Swap_symlink t -> "swap:" ^ t
end

type pos = { kind : Op.kind; path : string; nth : int }

let pos_of (op : Op.t) = { kind = op.kind; path = op.path; nth = op.nth }
let at p (op : Op.t) = op.kind = p.kind && op.path = p.path && op.nth = p.nth
let pos_key p = Fmt.str "%s:%s#%d" (Op.kind_name p.kind) p.path p.nth

let ordinal n =
  let suffix =
    match (n mod 100, n mod 10) with
    | (11 | 12 | 13), _ -> "th"
    | _, 1 -> "st"
    | _, 2 -> "nd"
    | _, 3 -> "rd"
    | _ -> "th"
  in
  string_of_int n ^ suffix

let pos_text ?(at = fun _ -> None) p =
  match at p with
  | Some op -> Fmt.str "%a" Op.pp op
  | None ->
      Fmt.str "the %s %s of %s" (ordinal p.nth) (Op.kind_name p.kind) p.path

type trace = Op.t list

let index_of trace p = List.find_opt (at p) trace

(* {1 Structure} *)

(* The length class of a read or a write: its length rounded down to a
   power of two. *)
let length_class n =
  let rec go c n = if n <= 1 then c else go (c + 1) (n lsr 1) in
  go 0 n

(* The reads or writes of a file that make its structure: the first three,
   the last, and the first of each length class, so a log of many records of
   varying length costs runs by its classes, not its records. *)

let structural (trace : trace) =
  let per = Hashtbl.create 7 in
  List.iter
    (fun (op : Op.t) ->
      match op.kind with
      | `Read | `Write ->
          let k = (op.kind, op.path) in
          Hashtbl.replace per k
            (op :: Option.value ~default:[] (Hashtbl.find_opt per k))
      | _ -> ())
    trace;
  let keep = Hashtbl.create 7 in
  Hashtbl.iter
    (fun _ ops ->
      let ops = List.rev ops in
      let n = List.length ops and seen = Hashtbl.create 8 in
      List.iteri
        (fun i (op : Op.t) ->
          let c = length_class op.length in
          let first = not (Hashtbl.mem seen c) in
          Hashtbl.replace seen c ();
          if i < 3 || i = n - 1 || first then Hashtbl.replace keep op.index ())
        ops)
    per;
  let last_write = Hashtbl.create 7 in
  List.filter
    (fun (op : Op.t) ->
      match op.kind with
      | `Read -> Hashtbl.mem keep op.index
      | `Write ->
          Hashtbl.replace last_write op.path op.index;
          Hashtbl.mem keep op.index
      | `Sync -> (
          match Hashtbl.find_opt last_write op.path with
          | None -> true
          | Some i -> Hashtbl.mem keep i)
      | _ -> true)
    trace

(* The operations sure to be structural as a run goes, whatever follows:
   any but a read, a write or a sync; one of the first three reads or writes
   of a file, or the first of its length class; a sync after none of those,
   or after one sure to be. *)
module Sure = struct
  type t = {
    seen : (Op.kind * string, int * (int, unit) Hashtbl.t) Hashtbl.t;
    last : (string, bool) Hashtbl.t; (* whether a file's last write is *)
  }

  let v () = { seen = Hashtbl.create 7; last = Hashtbl.create 7 }

  let add t (op : Op.t) =
    match op.kind with
    | `Read | `Write ->
        let k = (op.kind, op.path) in
        let n, classes =
          match Hashtbl.find_opt t.seen k with
          | Some v -> v
          | None -> (0, Hashtbl.create 4)
        in
        let c = length_class op.length in
        let sure = n < 3 || not (Hashtbl.mem classes c) in
        Hashtbl.replace classes c ();
        Hashtbl.replace t.seen k (n + 1, classes);
        if op.kind = `Write then Hashtbl.replace t.last op.path sure;
        sure
    | `Sync -> Option.value ~default:true (Hashtbl.find_opt t.last op.path)
    | _ -> true
end

let rest trace sel =
  let s = Hashtbl.create 64 in
  List.iter (fun (op : Op.t) -> Hashtbl.replace s op.index ()) sel;
  List.filter (fun (op : Op.t) -> not (Hashtbl.mem s op.index)) trace

(* {1 Sampling} *)

let spread n l =
  let len = List.length l in
  if len <= n then l
  else
    let a = Array.of_list l in
    List.init n (fun i -> a.(i * len / n))

let sampled_share ~budget ~linear ~classes =
  let spend = min (budget - linear) (max (budget / 40) (linear / 2)) in
  max 0 spend / max 1 classes

(* {1 check} *)

type lasting = Read_only | Full | Denied
type check = { faults : (pos * Fault.t) list; lasting : (lasting * pos) option }

let none = { faults = []; lasting = None }

let lasting_name = function
  | Read_only -> "read-only"
  | Full -> "full"
  | Denied -> "denying"

let lasting_text = function
  | Read_only -> "read-only"
  | Full -> "full"
  | Denied -> "denying every access"

let lasting_code l (op : Op.t) : Fault.code option =
  match (l, op.kind) with
  | Read_only, (`Write | `Truncate | `Change | `Open_write) -> Some `Erofs
  | Full, (`Write | `Change | `Open_write) -> Some `Enospc
  | Denied, (`Open_read | `Open_write | `Change | `Read_dir | `Stat) ->
      Some `Eacces
  | _ -> None

let check_faults (op : Op.t) : Fault.t list =
  match op.kind with
  | `Read -> [ Short 1; Fail `Eio ]
  | `Read_dir -> [ Fail `Eio ]
  | `Write -> [ Short 1; Torn (op.length / 2); Fail `Enospc; Fail `Eio ]
  | `Truncate | `Sync -> [ Fail `Eio ]
  | `Open_read -> [ Fail `Eacces; Fail `Eio ]
  | `Open_write | `Change -> [ Fail `Eacces; Fail `Erofs; Fail `Enospc ]
  | `Stat -> [ Fail `Eacces; Fail `Eio ]

let singles ops =
  List.concat_map
    (fun op -> List.map (fun f -> (pos_of op, f)) (check_faults op))
    ops

let lastings ops =
  List.concat_map
    (fun l -> List.map (fun op -> (l, pos_of op)) ops)
    [ Read_only; Full; Denied ]

let of_singles ops =
  List.map (fun one -> { faults = [ one ]; lasting = None }) (singles ops)
  @ List.map (fun l -> { faults = []; lasting = Some l }) (lastings ops)

let check_linear trace = of_singles (structural trace)
let check_others trace = of_singles (rest trace (structural trace))

(* Spread as [spread] spreads [check_others], but the scenarios are counted
   first and built only once picked. *)
let check_others_spread ~keep trace =
  let ops = rest trace (structural trace) in
  let at op = List.filter keep (of_singles [ op ]) in
  let total = List.fold_left (fun n op -> n + List.length (at op)) 0 ops in
  let pick n =
    if total <= n then List.concat_map at ops
    else
      let next i = i * total / n in
      let rec go i k acc = function
        | [] -> List.rev acc
        | op :: rest ->
            let c = Array.of_list (at op) in
            let m = Array.length c in
            let rec take i acc =
              if i < n && next i < k + m then
                take (i + 1) (c.(next i - k) :: acc)
              else (i, acc)
            in
            let i, acc = take i acc in
            go i (k + m) acc rest
      in
      go 0 0 [] ops
  in
  (total, pick)

let check_floor ~space ~sure op =
  if not sure then 0
  else
    List.length
      (List.filter
         (fun s ->
           List.for_all
             (fun (_, f) -> space || f <> Fault.Fail `Enospc)
             s.faults)
         (of_singles [ op ]))

let disk_kind (op : Op.t) =
  match op.kind with
  | `Write | `Truncate | `Change | `Open_write | `Sync -> true
  | _ -> false

(* The changes to the disk a run made, in order: what tells whether a
   fault changed its course. *)
module Disk = struct
  type t = Op.t array

  let v trace = Array.of_list (List.filter disk_kind trace)

  let same (a : Op.t) (b : Op.t) =
    a.kind = b.kind && a.path = b.path
    && a.file_offset = b.file_offset
    && a.length = b.length && a.dest = b.dest

  (* Whether the changes of [after] past [op1] differ from those of the
     first run past the same place. *)
  let diverge (t : t) (later : Op.t list) (op1 : Op.t) =
    let rec start i =
      if i < Array.length t && t.(i).index <= op1.index then start (i + 1)
      else i
    in
    let rec go i = function
      | [] -> i < Array.length t
      | (op : Op.t) :: rest ->
          i >= Array.length t || (not (same op t.(i))) || go (i + 1) rest
    in
    go (start 0) (List.filter disk_kind later)
end

(* The operations of [after] past [op1], where the first fault landed: if
   the fault changed what the run writes, those that change the disk, then
   the others, each nearest the fault first; else none, and all of them. *)
let aimed ~first ~(after : trace) (op1 : Op.t) =
  let later = List.filter (fun (op : Op.t) -> op.index > op1.index) after in
  if Disk.diverge first later op1 then
    let disk, other = List.partition disk_kind later in
    (disk @ other, [])
  else ([], later)

(* [l] in an order each prefix of which is spread evenly over it: by the
   bits of each place reversed. *)
let interleave l =
  let a = Array.of_list l in
  let n = Array.length a in
  let rec bits b = if 1 lsl b >= n then b else bits (b + 1) in
  let b = bits 0 in
  let rev i =
    let r = ref 0 in
    for k = 0 to b - 1 do
      if i land (1 lsl k) <> 0 then r := !r lor (1 lsl (b - 1 - k))
    done;
    !r
  in
  List.filter_map
    (fun i ->
      let j = rev i in
      if j < n then Some a.(j) else None)
    (List.init (1 lsl b) Fun.id)

(* At most [limit] of the pairs [pairs] makes at [changed], in order, and
   of those at [rest], evenly: each operation makes one pair or more, so
   [limit] operations of each are enough. *)
let aim limit pairs (changed, rest) =
  let first l = List.filteri (fun i _ -> i < limit) l in
  ( first (pairs (first changed)),
    interleave (spread limit (pairs (spread limit rest))) )

module Pairs = struct
  type 'a t = {
    cap : int;
    ranks : 'a list array;
        (* by rank, latest first: those on a changed operation, then the
           others *)
    mutable size : int;
    mutable top : int; (* no rank above holds one *)
  }

  let v cap =
    let cap = max 0 cap in
    { cap; ranks = Array.make (2 * cap) []; size = 0; top = 0 }

  let add_at t r x =
    if r < Array.length t.ranks then begin
      t.ranks.(r) <- x :: t.ranks.(r);
      t.size <- t.size + 1;
      t.top <- max t.top r;
      while t.size > t.cap do
        while t.ranks.(t.top) = [] do
          t.top <- t.top - 1
        done;
        t.ranks.(t.top) <- List.tl t.ranks.(t.top);
        t.size <- t.size - 1
      done
    end

  let add t (changed, rest) =
    List.iteri (fun r x -> add_at t r x) changed;
    List.iteri (fun r x -> add_at t (t.cap + r) x) rest

  let to_list t = List.concat_map List.rev (Array.to_list t.ranks)
end

let check_seconds ?(limit = max_int) one ~first ~(after : trace) =
  match index_of after (fst one) with
  | None -> ([], [])
  | Some (op1 : Op.t) ->
      let changed, rest = aimed ~first ~after op1 in
      let pairs ops =
        List.map
          (fun two -> { faults = [ one; two ]; lasting = None })
          (singles ops)
      in
      aim limit pairs (changed, rest)

let faults_key = function
  | [] -> "none"
  | fs ->
      String.concat "+"
        (List.map (fun (p, f) -> pos_key p ^ "." ^ Fault.key f) fs)

let faults_text ?at = function
  | [] -> "none"
  | fs ->
      String.concat "; then "
        (List.map
           (fun (p, f) -> Fmt.str "%s %a" (pos_text ?at p) Fault.pp f)
           fs)

let check_key s =
  match s.lasting with
  | Some (l, p) -> Fmt.str "from:%s.%s" (pos_key p) (lasting_name l)
  | None -> faults_key s.faults

let check_text ?at s =
  match s.lasting with
  | Some (l, p) ->
      Fmt.str "the filesystem is %s from %s on" (lasting_text l)
        (pos_text ?at p)
  | None -> faults_text ?at s.faults

(* {1 Crash.check} *)

type state = Drop | Keep | Enum of int list | Seed of int | Shape of int list

let shape ~widths i =
  let rng = Random.State.make [| i |] in
  let d = List.length widths in
  let last n = max 0 (n - 1) in
  match i mod 4 with
  | 0 -> Seed (i / 4)
  | 1 ->
      let t = Random.State.int rng (d + 1) in
      Shape (List.mapi (fun k n -> if k < t then last n else 0) widths)
  | 2 ->
      let j = Random.State.int rng (max 1 d) in
      Shape
        (List.mapi
           (fun k n ->
             if k = j && n > 1 then 1 + Random.State.int rng (n - 1) else 0)
           widths)
  | _ ->
      let j = Random.State.int rng (max 1 d) in
      Shape (List.mapi (fun k n -> if k = j then 0 else last n) widths)

type crash = {
  cfaults : (pos * Fault.t) list;
  cut : pos option;
  state : state;
  rcut : (pos option * state) option;
}

let base = { cfaults = []; cut = None; state = Drop; rcut = None }

(* A write, a truncation, a directory change or an open that may write
   changes what a cut leaves; a sync does only where it commits something,
   which [changed] says, as for any operation an ack follows. *)
let changes ~changed (op : Op.t) =
  match op.kind with
  | `Write | `Truncate | `Change | `Open_write -> true
  | _ -> changed op

(* After each operation of [ops] that changes the disk, before the one that
   follows it in [trace]; [None] is the end. *)
let cut_points ~changed (trace : trace) ops =
  let next = Hashtbl.create 16 in
  let rec link = function
    | (a : Op.t) :: (b :: _ as rest) ->
        Hashtbl.replace next a.index b;
        link rest
    | _ -> ()
  in
  link trace;
  let after =
    List.filter_map
      (fun (op : Op.t) ->
        if changes ~changed op then
          Option.map (fun b -> Some (pos_of b)) (Hashtbl.find_opt next op.index)
        else None)
      ops
  in
  None :: List.sort_uniq compare after

(* The bytes a write overwrites, given what each file held: those tear only
   at the 512-byte sectors a disk writes whole. Bytes that extend a file tear
   anywhere. *)
let tears ~sizes (trace : trace) =
  let size = Hashtbl.create 7 in
  List.iter (fun (p, n) -> Hashtbl.replace size p n) sizes;
  let t = Hashtbl.create 16 in
  List.iter
    (fun (op : Op.t) ->
      if op.kind = `Write then begin
        let s = Option.value ~default:0 (Hashtbl.find_opt size op.path) in
        let len = op.length in
        let over = max 0 (min len (s - op.file_offset)) in
        let sector =
          List.filter
            (fun k -> k > 0 && k < over)
            (List.init
               ((over / 512) + 2)
               (fun j -> (512 * j) - (op.file_offset mod 512)))
        in
        let extend =
          if over < len then
            List.filter
              (fun k -> k > over && k < len)
              [ over + 1; (over + len) / 2; len - 1 ]
          else []
        in
        let ks = List.sort_uniq compare (sector @ extend) in
        Hashtbl.replace t op.index (spread 4 ks);
        Hashtbl.replace size op.path (max s (op.file_offset + len))
      end)
    trace;
  fun (op : Op.t) -> Option.value ~default:[] (Hashtbl.find_opt t op.index)

let durable_faults ~tears ops =
  List.concat_map
    (fun (op : Op.t) ->
      match op.kind with
      | `Sync -> [ (pos_of op, Fault.Fail `Eio) ]
      | `Write -> List.map (fun n -> (pos_of op, Fault.Torn n)) (tears op)
      | _ -> [])
    ops

let both s = [ { s with state = Drop }; { s with state = Keep } ]

let crash_cuts ~changed trace =
  List.concat_map
    (fun cut -> both { base with cut })
    (cut_points ~changed trace (structural trace))

(* A cut is tried in both states, and each one's recover at its end in
   both. *)
let crash_floor ~sure ~changed (op : Op.t) =
  if not sure then 0
  else
    (if changes ~changed:(fun _ -> changed) op then 6 else 0)
    + if op.kind = `Sync then List.length (both base) else 0

let crash_faults ~tears trace =
  List.concat_map
    (fun one -> both { base with cfaults = [ one ] })
    (durable_faults ~tears (structural trace))

let crash_others ~changed ~tears trace =
  let sel = structural trace in
  let lin = cut_points ~changed trace sel in
  List.concat_map
    (fun cut -> both { base with cut })
    (List.filter
       (fun c -> not (List.mem c lin))
       (cut_points ~changed trace (rest trace sel)))
  @ List.concat_map
      (fun one -> both { base with cfaults = [ one ] })
      (durable_faults ~tears (rest trace sel))

let rcuts ~changed s ~(recover : trace) =
  List.concat_map
    (fun j ->
      List.map (fun st -> { s with rcut = Some (j, st) }) [ Drop; Keep ])
    (cut_points ~changed recover (structural recover))

let crash_seconds ?(limit = max_int) ~tears one ~first ~(after : trace) =
  match index_of after (fst one) with
  | None -> ([], [])
  | Some (op1 : Op.t) ->
      let changed, rest = aimed ~first ~after:(structural after) op1 in
      let pairs ops =
        List.concat_map
          (fun two -> both { base with cfaults = [ one; two ] })
          (durable_faults ~tears ops)
      in
      aim limit pairs (changed, rest)

let next_choices trail =
  let rec go = function
    | [] -> None
    | (c, n) :: rest when c + 1 < n -> Some (List.rev_map fst rest @ [ c + 1 ])
    | _ :: rest -> go rest
  in
  go trail

(* The tree of the draws the states met so far make: a node per draw, with
   the choices it had, and below it the choices taken. *)
module Draws = struct
  type t = { mutable width : int; below : (int, t) Hashtbl.t }

  let empty () = { width = 0; below = Hashtbl.create 2 }

  let add t trail =
    ignore
      (List.fold_left
         (fun node (c, n) ->
           node.width <- n;
           match Hashtbl.find_opt node.below c with
           | Some next -> next
           | None ->
               let next = empty () in
               Hashtbl.replace node.below c next;
               next)
         t (List.rev trail)
        : t)

  (* The states the tree allows, a choice not taken counted as the largest
     taken one at its draw. *)
  let rec estimate t =
    if t.width = 0 then 1
    else
      let sum, widest, taken =
        Hashtbl.fold
          (fun _ next (sum, widest, taken) ->
            let e = estimate next in
            (min max_int (sum + e), max widest e, taken + 1))
          t.below (0, 0, 0)
      in
      let rest = t.width - taken in
      if rest > 0 && widest > (max_int - sum) / rest then max_int
      else sum + (rest * widest)
end

let rec trim = function
  | [] -> []
  | l -> ( match List.rev l with 0 :: r -> trim (List.rev r) | _ -> l)

let state_key = function
  | Drop -> "drop"
  | Keep -> "keep"
  | Enum v -> "e" ^ String.concat "." (List.map string_of_int (trim v))
  | Seed s -> Fmt.str "s%d" s
  | Shape v -> "p" ^ String.concat "." (List.map string_of_int (trim v))

(* Only the key [state_key] makes names a state: no negative choice, no
   leading zero or sign, no trailing zero. *)
let state_of_key k =
  let body = if k = "" then "" else String.sub k 1 (String.length k - 1) in
  let choice s =
    match int_of_string_opt s with Some c when c >= 0 -> Some c | _ -> None
  in
  let choices mk =
    if body = "" then Some (mk [])
    else
      List.fold_right
        (fun s acc ->
          match (acc, choice s) with
          | Some l, Some c -> Some (c :: l)
          | _ -> None)
        (String.split_on_char '.' body)
        (Some [])
      |> Option.map mk
  in
  let st =
    match k with
    | "drop" -> Some Drop
    | "keep" -> Some Keep
    | _ when k = "" -> None
    | _ when k.[0] = 'e' -> choices (fun v -> Enum v)
    | _ when k.[0] = 'p' -> choices (fun v -> Shape v)
    | _ when k.[0] = 's' -> Option.map (fun s -> Seed s) (choice body)
    | _ -> None
  in
  match st with Some st when state_key st = k -> Some st | _ -> None

let cut_key = function None -> "end" | Some p -> "before:" ^ pos_key p

let crash_key s =
  let r =
    match s.rcut with
    | None -> ""
    | Some (j, st) -> Fmt.str "|r|%s|%s" (cut_key j) (state_key st)
  in
  Fmt.str "%s|%s|%s%s" (faults_key s.cfaults) (cut_key s.cut)
    (state_key s.state) r

let cut_last s =
  match String.rindex_opt s '|' with
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> None

(* The key before the last [|r|] in [s], if any. *)
let before_rcut s =
  let rec go i =
    if i < 0 then None
    else if String.sub s i 3 = "|r|" then Some (String.sub s 0 i)
    else go (i - 1)
  in
  go (String.length s - 3)

let well_formed k =
  let state s = Option.is_some (state_of_key s) in
  match cut_last k with
  | None -> false
  | Some (rest, st) -> (
      state st
      &&
      match before_rcut rest with
      | None -> String.contains rest '|'
      | Some main -> (
          match cut_last main with
          | Some (rest, st) -> state st && String.contains rest '|'
          | None -> false))

(* The key of [s] with its state left out, and the state. *)
let split_state_key k =
  match String.split_on_char '|' k with
  | f :: c :: st :: rest ->
      Option.map
        (fun st -> (String.concat "|" (f :: c :: "drop" :: rest), st))
        (state_of_key st)
  | _ -> None
