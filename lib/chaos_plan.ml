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

let rest trace sel = List.filter (fun op -> not (List.memq op sel)) trace

(* {1 Sampling} *)

let spread n l =
  let len = List.length l in
  if len <= n then l
  else
    let a = Array.of_list l in
    List.init n (fun i -> a.(i * len / n))

let sampled_share ~budget ~linear ~classes =
  let spend = min (budget - linear) (max 500 (linear / 2)) in
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

(* Second faults, at the operations of [after], the run [first] made, past
   the one it landed on. *)
let check_seconds first ~(after : trace) =
  match index_of after (fst first) with
  | None -> []
  | Some (op1 : Op.t) ->
      List.filter (fun (op : Op.t) -> op.index > op1.index) after
      |> singles
      |> List.map (fun two -> { faults = [ first; two ]; lasting = None })

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

let changes (op : Op.t) =
  match op.kind with
  | `Write | `Truncate | `Sync | `Change | `Open_write -> true
  | _ -> false

(* After each operation of [ops] that changes the disk, before the one that
   follows it in [trace]; [None] is the end. *)
let cut_points (trace : trace) ops =
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
        if changes op then
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

let crash_cuts trace =
  List.concat_map
    (fun cut -> both { base with cut })
    (cut_points trace (structural trace))

let crash_faults ~tears trace =
  List.concat_map
    (fun one -> both { base with cfaults = [ one ] })
    (durable_faults ~tears (structural trace))

let crash_others ~tears trace =
  let sel = structural trace in
  let lin = cut_points trace sel in
  List.concat_map
    (fun cut -> both { base with cut })
    (List.filter
       (fun c -> not (List.mem c lin))
       (cut_points trace (rest trace sel)))
  @ List.concat_map
      (fun one -> both { base with cfaults = [ one ] })
      (durable_faults ~tears (rest trace sel))

let rcuts s ~(recover : trace) =
  List.concat_map
    (fun j ->
      List.map (fun st -> { s with rcut = Some (j, st) }) [ Drop; Keep ])
    (cut_points recover (structural recover))

let crash_seconds ~tears first ~(after : trace) =
  match index_of after (fst first) with
  | None -> []
  | Some (op1 : Op.t) ->
      List.filter (fun (op : Op.t) -> op.index > op1.index) (structural after)
      |> durable_faults ~tears
      |> List.concat_map (fun two ->
          both { base with cfaults = [ first; two ] })

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

let state_of_key k =
  let choices k mk =
    let body = String.sub k 1 (String.length k - 1) in
    if body = "" then Some (mk [])
    else
      match List.map int_of_string (String.split_on_char '.' body) with
      | v -> Some (mk v)
      | exception Failure _ -> None
  in
  match k with
  | "drop" -> Some Drop
  | "keep" -> Some Keep
  | _ when String.length k > 0 && k.[0] = 'e' -> choices k (fun v -> Enum v)
  | _ when String.length k > 0 && k.[0] = 'p' -> choices k (fun v -> Shape v)
  | _ when String.length k > 0 && k.[0] = 's' -> (
      let body = String.sub k 1 (String.length k - 1) in
      match int_of_string_opt body with
      | Some s when string_of_int s = body -> Some (Seed s)
      | Some _ | None -> None)
  | _ -> None

let cut_key = function None -> "end" | Some p -> "before:" ^ pos_key p

let crash_key s =
  let r =
    match s.rcut with
    | None -> ""
    | Some (j, st) -> Fmt.str "|r|%s|%s" (cut_key j) (state_key st)
  in
  Fmt.str "%s|%s|%s%s" (faults_key s.cfaults) (cut_key s.cut)
    (state_key s.state) r

(* The key of [s] with its state left out, and the state. *)
let split_state_key k =
  match String.split_on_char '|' k with
  | f :: c :: st :: rest ->
      Option.map
        (fun st -> (String.concat "|" (f :: c :: "drop" :: rest), st))
        (state_of_key st)
  | _ -> None
