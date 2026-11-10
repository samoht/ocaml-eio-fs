(* An independent reference crash model, an inode graph. It shares no code
   with Chaos.Crash: it sees only the live trees before and after each
   operation and derives events from them. *)

module IM = Map.Make (Int)
module SM = Map.Make (String)

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* {1 Operations} *)

type op =
  | Mkdir of string
  | Creat of string * char
  | Append of string * char
  | Rename of string * string
  | Unlink of string
  | Rmdir of string
  | Fsync of string
  | Fsync_dir of string
  | Chmod of string * int
  | Trunc of string * int

let pp_op ppf = function
  | Mkdir p -> Fmt.pf ppf "mkdir %s" p
  | Creat (p, c) -> Fmt.pf ppf "creat %s %c" p c
  | Append (p, c) -> Fmt.pf ppf "append %s %c" p c
  | Rename (a, b) -> Fmt.pf ppf "rename %s %s" a b
  | Unlink p -> Fmt.pf ppf "unlink %s" p
  | Rmdir p -> Fmt.pf ppf "rmdir %s" p
  | Fsync p -> Fmt.pf ppf "fsync %s" p
  | Fsync_dir p -> Fmt.pf ppf "fsync_dir %S" p
  | Chmod (p, m) -> Fmt.pf ppf "chmod %s %o" p m
  | Trunc (p, n) -> Fmt.pf ppf "trunc %s %d" p n

let pp_ops = Fmt.(list ~sep:(any "; ") pp_op)

let exec fs = function
  | Mkdir p -> Eio.Path.mkdir ~perm:0o777 (fs / p)
  | Creat (p, c) ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o666) (fs / p) (fun f ->
          Eio.Flow.copy_string (String.make 1 c) f)
  | Append (p, c) ->
      Eio.Path.with_open_out ~append:true ~create:`Never (fs / p) (fun f ->
          Eio.Flow.copy_string (String.make 1 c) f)
  | Trunc (p, n) ->
      Eio.Path.with_open_out ~create:`Never (fs / p) (fun f ->
          Eio.File.truncate f (i63 n))
  | Rename (a, b) -> Eio.Path.rename (fs / a) (fs / b)
  | Unlink p -> Eio.Path.unlink (fs / p)
  | Rmdir p -> Eio.Path.rmdir (fs / p)
  | Fsync p -> Eio.Path.with_open_out ~create:`Never (fs / p) Eio.File.sync
  | Fsync_dir "" -> Eio_fs_sync.sync_dir fs
  | Fsync_dir p -> Eio_fs_sync.sync_dir (fs / p)
  | Chmod (p, m) -> Eio.Path.chmod ~follow:true ~perm:m (fs / p)

(* {1 Live trees, by inode} *)

type node = { kind : [ `F | `D ]; perm : int; data : string; ents : int SM.t }
type tree = int * node IM.t

(* Small ids for inodes, in the order they are first met. *)
type ids = { tbl : (int64, int) Hashtbl.t; mutable next : int }

let ids () = { tbl = Hashtbl.create 16; next = 0 }

let id ids ino =
  match Hashtbl.find_opt ids.tbl ino with
  | Some i -> i
  | None ->
      let i = ids.next in
      ids.next <- i + 1;
      Hashtbl.replace ids.tbl ino i;
      i

let id_opt ids ino = Hashtbl.find_opt ids.tbl ino

let snapshot ids root =
  let nodes = ref IM.empty in
  let rec walk p =
    let st = Eio.Path.stat ~follow:false p in
    let i = id ids st.ino in
    let node =
      match st.kind with
      | `Directory ->
          let names = List.sort String.compare (Eio.Path.read_dir p) in
          let ents =
            List.fold_left (fun m n -> SM.add n (walk (p / n)) m) SM.empty names
          in
          { kind = `D; perm = st.perm; data = ""; ents }
      | `Regular_file ->
          { kind = `F; perm = st.perm; data = Eio.Path.load p; ents = SM.empty }
      | _ -> Fmt.failwith "%a: unexpected kind" Eio.Path.pp p
    in
    nodes := IM.add i node !nodes;
    i
  in
  let r = walk root in
  (r, !nodes)

(* {1 Events} *)

type event =
  | Meta of {
      changes : (int * string * int option) list;
          (* directory, name, what the name now holds *)
      touches : int list; (* the directories it changes or removes *)
      links : int list; (* the new directories it links *)
      moved : int list; (* the nodes it moves *)
      under : int list;
          (* for a move, the directories from the root down to where it
             lands *)
    }
  | Init of int (* a new directory exists *)
  | Attrs of int * int
  | Data of int * string
  | Bdir of int
  | Bfile of int

let pp_event ppf = function
  | Meta { changes; touches; _ } ->
      Fmt.pf ppf "meta[%a|%a]"
        Fmt.(
          list ~sep:(any ",") (fun ppf (d, n, t) ->
              pf ppf "%d/%s->%a" d n (option ~none:(any "-") int) t))
        changes
        Fmt.(list ~sep:(any ",") int)
        touches
  | Init d -> Fmt.pf ppf "init %d" d
  | Attrs (i, p) -> Fmt.pf ppf "attrs %d %o" i p
  | Data (i, s) -> Fmt.pf ppf "data %d %S" i s
  | Bdir d -> Fmt.pf ppf "bdir %d" d
  | Bfile f -> Fmt.pf ppf "bfile %d" f

let pp_events = Fmt.(list ~sep:(any "; ") pp_event)

(* The directories from [root] down to [d] in [nodes]. *)
let path_to (root, nodes) d =
  let rec go i acc =
    if i = d then Some (List.rev (i :: acc))
    else
      match IM.find_opt i nodes with
      | Some { kind = `D; ents; _ } ->
          SM.fold
            (fun _ c found ->
              match found with Some _ -> found | None -> go c (i :: acc))
            ents None
      | Some _ | None -> None
  in
  Option.value ~default:[] (go root [])

(* The events of one operation, from the trees before and after it, then
   its barrier. *)
let events ~before:(_, n0) ~after:((_, n1) as t1) ~barrier =
  let dirs m =
    IM.fold (fun i n acc -> if n.kind = `D then i :: acc else acc) m []
  in
  let changes = ref [] and touches = ref [] and inits = ref [] in
  let touch d = if not (List.mem d !touches) then touches := d :: !touches in
  List.iter
    (fun d ->
      match (IM.find_opt d n0, IM.find_opt d n1) with
      | Some a, Some b ->
          let names =
            List.sort_uniq String.compare
              (List.map fst (SM.bindings a.ents)
              @ List.map fst (SM.bindings b.ents))
          in
          List.iter
            (fun nm ->
              let x = SM.find_opt nm a.ents and y = SM.find_opt nm b.ents in
              if x <> y then begin
                changes := (d, nm, y) :: !changes;
                touch d
              end)
            names
      | None, Some _ -> inits := d :: !inits
      | Some _, None -> touch d
      | None, None -> ())
    (List.sort_uniq Int.compare (dirs n0 @ dirs n1));
  let changes = List.rev !changes in
  let links =
    List.filter_map
      (function _, _, Some t when List.mem t !inits -> Some t | _ -> None)
      changes
  in
  let olds =
    List.filter_map
      (fun (d, nm, _) ->
        match IM.find_opt d n0 with
        | Some a -> SM.find_opt nm a.ents
        | None -> None)
      changes
  in
  let moved =
    List.filter_map
      (function
        | _, _, Some t when List.mem t olds && IM.mem t n0 -> Some t | _ -> None)
      changes
  in
  let under =
    List.concat_map
      (function d, _, Some t when List.mem t moved -> path_to t1 d | _ -> [])
      changes
  in
  let attrs = ref [] and data = ref [] in
  IM.iter
    (fun i b ->
      match IM.find_opt i n0 with
      | Some a ->
          if a.perm <> b.perm then attrs := Attrs (i, b.perm) :: !attrs;
          if b.kind = `F && a.data <> b.data then
            data := Data (i, b.data) :: !data
      | None ->
          if b.kind = `F && b.data <> "" then data := Data (i, b.data) :: !data)
    n1;
  let meta =
    if changes = [] && !touches = [] then []
    else [ Meta { changes; touches = !touches; links; moved; under } ]
  in
  List.map (fun d -> Init d) !inits @ meta @ !attrs @ !data @ barrier

(* Each inode's attributes as first met, with no data and no entries. *)
let bases trees =
  List.fold_left
    (fun acc (_, nodes) ->
      IM.fold
        (fun i n acc ->
          if IM.mem i acc then acc
          else IM.add i { n with data = ""; ents = SM.empty } acc)
        nodes acc)
    IM.empty trees

let touches_of = function
  | Meta { touches; _ } -> touches
  | Init d -> [ d ]
  | Attrs _ | Data _ | Bdir _ | Bfile _ -> []

(* Whether event [a], earlier in program order, must land before [b]:
   events of one directory stay ordered, as do the attributes and the data
   of one inode; a directory barrier waits for the earlier changes to that
   directory and a file barrier for its file's data and attributes; nothing
   passes a barrier. *)
let must_precede a b =
  match (a, b) with
  | (Bdir _ | Bfile _), _ -> true
  | _, Bdir d -> (
      List.mem d (touches_of a)
      || match a with Attrs (i, _) -> i = d | _ -> false)
  | _, Bfile f -> (
      match a with Attrs (i, _) | Data (i, _) -> i = f | _ -> false)
  | Init d, Meta { links; touches; _ } -> List.mem d links || List.mem d touches
  | Meta x, Meta y -> List.exists (fun d -> List.mem d y.touches) x.touches
  | Attrs (i, _), Attrs (j, _) -> i = j
  | Data (i, _), Data (j, _) -> i = j
  | _ -> false

(* A rename waits for the earlier renames that moved a directory above
   where it lands: eio-fs's own rule, not the reference's. *)
let ancestor_dep a b =
  match (a, b) with
  | Meta x, Meta y -> List.exists (fun m -> List.mem m y.under) x.moved
  | _ -> false

(* {1 Crash states} *)

type entry = {
  path : string;
  k : [ `F | `D ];
  id : int;
  perm : int;
  content : string;
}

let pp_entry ppf e =
  Fmt.pf ppf "%s:%s#%d:%o%s" e.path
    (match e.k with `F -> "f" | `D -> "d")
    e.id e.perm
    (match e.k with `F -> Fmt.str "=%S" e.content | `D -> "")

type state = entry list

let pp_state = Fmt.(brackets (list ~sep:(any " ") pp_entry))

module States = Set.Make (struct
  type t = state

  let compare = compare
end)

let without_data = List.map (fun e -> { e with content = "" })
let without_modes = List.map (fun e -> { e with perm = 0 })

type states = { all : States.t; prefix : States.t; ancestors : States.t }

let states ~root ~init ~base evs =
  let evs = Array.of_list evs in
  let n = Array.length evs in
  if n > Sys.int_size - 1 then Fmt.failwith "%d events" n;
  let bit i = 1 lsl i in
  let preds_by rel =
    Array.init n (fun j ->
        let m = ref 0 in
        for i = 0 to j - 1 do
          if rel evs.(i) evs.(j) then m := !m lor bit i
        done;
        !m)
  in
  let preds = preds_by must_precede in
  let preds_anc = preds_by (fun a b -> must_precede a b || ancestor_dep a b) in
  let barriers = ref 0 and journal = ref 0 in
  Array.iteri
    (fun i e ->
      match e with
      | Bdir _ | Bfile _ -> barriers := !barriers lor bit i
      | Meta _ | Init _ | Attrs _ -> journal := !journal lor bit i
      | Data _ -> ())
    evs;
  let close preds m =
    let rec go m =
      let m' = ref m in
      for j = 0 to n - 1 do
        if m land bit j <> 0 then m' := !m' lor preds.(j)
      done;
      if !m' = m then m else go !m'
    in
    go m
  in
  let apply mask =
    let nodes = ref init in
    let get i =
      match IM.find_opt i !nodes with Some n -> n | None -> IM.find i base
    in
    for i = 0 to n - 1 do
      if mask land bit i <> 0 then
        match evs.(i) with
        | Meta { changes; _ } ->
            List.iter
              (fun (d, nm, t) ->
                let dn = get d in
                let ents =
                  match t with
                  | Some t -> SM.add nm t dn.ents
                  | None -> SM.remove nm dn.ents
                in
                nodes := IM.add d { dn with ents } !nodes)
              changes
        | Attrs (i, p) -> nodes := IM.add i { (get i) with perm = p } !nodes
        | Data (i, s) -> nodes := IM.add i { (get i) with data = s } !nodes
        | Init _ | Bdir _ | Bfile _ -> ()
    done;
    let out = ref [] in
    let rec walk seen prefix i =
      SM.iter
        (fun nm c ->
          let path = if prefix = "" then nm else prefix ^ "/" ^ nm in
          let cn = get c in
          out :=
            { path; k = cn.kind; id = c; perm = cn.perm; content = cn.data }
            :: !out;
          if cn.kind = `D && not (List.mem c seen) then walk (c :: seen) path c)
        (get i).ents
    in
    walk [ root ] "" root;
    List.sort compare !out
  in
  (* Whether [mask]'s journal events are [forced]'s and a program-order
     prefix of the rest: eio-fs keeps what was never committed as a prefix. *)
  let prefix_shaped forced mask =
    let jm = mask land !journal and jf = forced land !journal in
    let rec go cut p =
      jm = jf lor p || (cut < n && go (cut + 1) (p lor (bit cut land !journal)))
    in
    go 0 0
  in
  let forced = close preds !barriers
  and forced_anc = close preds_anc !barriers in
  let seen = Hashtbl.create 64 in
  let all = ref States.empty
  and prefix = ref States.empty
  and ancestors = ref States.empty in
  let rec visit mask =
    if not (Hashtbl.mem seen mask) then begin
      Hashtbl.add seen mask ();
      if mask land !barriers = !barriers then begin
        let st = apply mask in
        all := States.add st !all;
        if prefix_shaped forced mask then prefix := States.add st !prefix;
        if prefix_shaped forced_anc mask then
          ancestors := States.add st !ancestors
      end;
      for j = 0 to n - 1 do
        if mask land bit j = 0 && preds.(j) land mask = preds.(j) then
          visit (mask lor bit j)
      done
    end
  in
  visit 0;
  { all = !all; prefix = !prefix; ancestors = !ancestors }
