(* A model-based test of the in-memory backend and Chaos.Crash: each seed
   draws a workload of file and directory operations, run as root or as a
   user, through a reference model and through the filesystem, and checks
   that each operation succeeds or fails in both and that the live trees
   agree. With a crash model, it also checks the post-crash image: exactly
   the committed state under Drop_unsynced, and a state some journal prefix
   allows under Reorder_unsynced, block by block. *)
module Crash = Eio_fs.Chaos.Crash
module SM = Map.Make (String)

let ( // ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* ---------- users ---------- *)
type who = Root | Alice

let alice_user = { Eio_fs.uid = 1000; gid = 1000; groups = []; umask = 0o022 }
let uid_of = function Root -> 0 | Alice -> 1000
let gid_of = function Root -> 0 | Alice -> 1000

(* ---------- model ---------- *)
type node = F of int | D of dnode | L of string
and dnode = { ents : node SM.t; dperm : int; duid : int; dgid : int }

type finfo = {
  mutable data : string;
  mutable fperm : int;
  fuid : int;
  fgid : int;
}

type change = W of int * string | R of int

type m = {
  mutable root : dnode;
  files : (int, finfo) Hashtbl.t;
  mutable next_id : int;
  (* durability *)
  durable : (int, string) Hashtbl.t;
  pending : (int, change list) Hashtbl.t; (* newest first *)
  (* metadata states: (tree, perms of files) at last commit, then after each op *)
  mutable committed : dnode * (int * int) list;
  mutable meta_states : (dnode * (int * int) list) list;
  fsynced : (int, int * int) Hashtbl.t;
      (* the mode a sync since the last commit made durable, by file, and the
         number of metadata states before that sync *)
      (* newest first, since commit *)
}

exception Err of string
exception Skip

let err s = raise (Err s)

let can who ~perm ~uid ~gid want =
  match who with
  | Root -> true
  | Alice ->
      let bits =
        if uid = 1000 then perm lsr 6
        else if gid = 1000 then perm lsr 3
        else perm
      in
      bits land want = want

let dcan who d want = can who ~perm:d.dperm ~uid:d.duid ~gid:d.dgid want
let segs p = List.filter (( <> ) "") (String.split_on_char '/' p)

let split p =
  match List.rev (segs p) with [] -> assert false | n :: r -> (List.rev r, n)

(* walk to the directory [comps], checking search on each dir entered *)
let rec get_dir who d = function
  | [] ->
      if not (dcan who d 1) then err "eacces-search";
      d
  | c :: rest -> (
      if not (dcan who d 1) then err "eacces-search";
      match SM.find_opt c d.ents with
      | Some (D d') -> get_dir who d' rest
      | Some (F _) -> err "enotdir"
      | Some (L _) -> raise Skip
      | None -> err "enoent")

let rec set_dir d comps nd =
  match comps with
  | [] -> nd
  | c :: rest -> (
      match SM.find_opt c d.ents with
      | Some (D d') ->
          { d with ents = SM.add c (D (set_dir d' rest nd)) d.ents }
      | _ -> assert false)

let finfo m id = Hashtbl.find m.files id

let add_pending m id c =
  let l = Option.value ~default:[] (Hashtbl.find_opt m.pending id) in
  Hashtbl.replace m.pending id (c :: l)

let perms_snapshot m =
  Hashtbl.fold (fun id f acc -> (id, f.fperm) :: acc) m.files []

let record_meta m = m.meta_states <- (m.root, perms_snapshot m) :: m.meta_states
let masked perm = perm land lnot 0o022

(* follow a final link in the same directory; [name] resolves to a file id,
   creating it if [create]. Returns (dir comps, name) of the final entry. *)
let resolve_final pdir name =
  let rec go name hops =
    if hops > 40 then err "eloop";
    match SM.find_opt name pdir.ents with
    | Some (L t) -> if String.contains t '/' then raise Skip else go t (hops + 1)
    | _ -> name
  in
  go name 0

let write_data s off data =
  let len = String.length s in
  let need = off + String.length data in
  let b = Bytes.make (max len need) '\000' in
  Bytes.blit_string s 0 b 0 len;
  Bytes.blit_string data 0 b off (String.length data);
  Bytes.to_string b

let trunc_data s n =
  if n <= String.length s then String.sub s 0 n
  else s ^ String.make (n - String.length s) '\000'

(* open a file for read-write; returns its id *)
let m_open m who p ~create =
  let comps, name = split p in
  let pdir = get_dir who m.root comps in
  (match (create, SM.find_opt name pdir.ents) with
  | `Excl _, Some _ -> err "eexist"
  | _ -> ());
  let fname = resolve_final pdir name in
  match SM.find_opt fname pdir.ents with
  | Some (D _) -> err "eisdir"
  | Some (L _) -> assert false
  | Some (F id) ->
      let f = finfo m id in
      if not (can who ~perm:f.fperm ~uid:f.fuid ~gid:f.fgid 6) then
        err "eacces-open";
      (match create with
      | `Trunc _ ->
          if f.data <> "" then begin
            f.data <- "";
            add_pending m id (R 0)
          end
      | _ -> ());
      id
  | None -> (
      match create with
      | `Never -> err "enoent"
      | `Excl perm | `Ifm perm | `Trunc perm ->
          if not (dcan who pdir 3) then err "eacces-create";
          let id = m.next_id in
          m.next_id <- id + 1;
          Hashtbl.replace m.files id
            {
              data = "";
              fperm = masked perm;
              fuid = uid_of who;
              fgid = gid_of who;
            };
          Hashtbl.replace m.durable id "";
          let pdir = { pdir with ents = SM.add fname (F id) pdir.ents } in
          m.root <- set_dir m.root comps pdir;
          record_meta m;
          id)

let m_write m id off data =
  let f = finfo m id in
  f.data <- write_data f.data off data;
  add_pending m id (W (off, data))

let m_trunc m id n =
  let f = finfo m id in
  f.data <- trunc_data f.data n;
  add_pending m id (R n)

let m_sync m id =
  Hashtbl.replace m.durable id (finfo m id).data;
  Hashtbl.replace m.fsynced id ((finfo m id).fperm, List.length m.meta_states);
  Hashtbl.remove m.pending id

let lookup m who p =
  let comps, name = split p in
  let pdir = get_dir who m.root comps in
  (comps, name, pdir, SM.find_opt name pdir.ents)

let m_unlink m who p =
  let comps, name, pdir, e = lookup m who p in
  match e with
  | None -> err "enoent"
  | Some (D _) -> err "eisdir"
  | Some _ ->
      if not (dcan who pdir 3) then err "eacces";
      m.root <-
        set_dir m.root comps { pdir with ents = SM.remove name pdir.ents };
      record_meta m

let m_mkdir m who p perm =
  let comps, name, pdir, e = lookup m who p in
  match e with
  | Some _ -> err "eexist"
  | None ->
      if not (dcan who pdir 3) then err "eacces";
      let nd =
        {
          ents = SM.empty;
          dperm = masked perm;
          duid = uid_of who;
          dgid = gid_of who;
        }
      in
      m.root <-
        set_dir m.root comps { pdir with ents = SM.add name (D nd) pdir.ents };
      record_meta m

let m_rmdir m who p =
  let comps, name, pdir, e = lookup m who p in
  match e with
  | None -> err "enoent"
  | Some (D d) ->
      if not (dcan who pdir 3) then err "eacces";
      if not (SM.is_empty d.ents) then err "enotempty";
      m.root <-
        set_dir m.root comps { pdir with ents = SM.remove name pdir.ents };
      record_meta m
  | Some _ -> err "enotdir"

let m_symlink m who p target =
  let comps, name, pdir, e = lookup m who p in
  match e with
  | Some _ -> err "eexist"
  | None ->
      if not (dcan who pdir 3) then err "eacces";
      m.root <-
        set_dir m.root comps
          { pdir with ents = SM.add name (L target) pdir.ents };
      record_meta m

let is_prefix a b =
  let rec go = function
    | [], _ -> true
    | x :: a, y :: b when x = y -> go (a, b)
    | _ -> false
  in
  go (a, b)

let m_rename m who src dst =
  let scomps, sname = split src and dcomps, dname = split dst in
  let spdir = get_dir who m.root scomps in
  let dpdir = get_dir who m.root dcomps in
  let snode =
    match SM.find_opt sname spdir.ents with None -> err "enoent" | Some n -> n
  in
  let dnode = SM.find_opt dname dpdir.ents in
  if scomps = dcomps && sname = dname then record_meta m
  else begin
    (match snode with
    | D _ when is_prefix (scomps @ [ sname ]) (dcomps @ [ dname ]) ->
        err "einval"
    | _ -> ());
    (match (snode, dnode) with
    | D _, Some (D d) when not (SM.is_empty d.ents) -> err "enotempty"
    | D _, Some (F _ | L _) -> err "enotdir"
    | (F _ | L _), Some (D _) -> err "eisdir"
    | _ -> ());
    if not (dcan who spdir 3) then err "eacces";
    if not (dcan who dpdir 3) then err "eacces";
    (* a directory moved to another parent has its .. rewritten *)
    (match snode with
    | D d when scomps <> dcomps && not (dcan who d 2) -> err "eacces"
    | _ -> ());
    (* remove src then add dst on the updated tree *)
    m.root <-
      set_dir m.root scomps { spdir with ents = SM.remove sname spdir.ents };
    let dpdir = get_dir Root m.root dcomps in
    m.root <-
      set_dir m.root dcomps { dpdir with ents = SM.add dname snode dpdir.ents };
    record_meta m
  end

let m_chmod m who p perm =
  let comps, name, pdir, _ = lookup m who p in
  let fname = resolve_final pdir name in
  match SM.find_opt fname pdir.ents with
  | None -> err "enoent"
  | Some (F id) ->
      let f = finfo m id in
      if who = Alice && f.fuid <> 1000 then err "eperm";
      f.fperm <- perm;
      record_meta m
  | Some (D d) ->
      if who = Alice && d.duid <> 1000 then err "eperm";
      m.root <- set_dir m.root (comps @ [ fname ]) { d with dperm = perm };
      record_meta m
  | Some (L _) -> assert false

let m_commit m =
  m.committed <- (m.root, perms_snapshot m);
  m.meta_states <- [];
  Hashtbl.reset m.fsynced

(* ---------- rendering ---------- *)
let render_model root (fdata : int -> string) (fmeta : int -> int * int) =
  let b = Buffer.create 256 in
  let rec go path d =
    Printf.bprintf b "%s/ d %o %d\n" path d.dperm d.duid;
    SM.iter
      (fun n e ->
        let p = path ^ "/" ^ n in
        match e with
        | D d -> go p d
        | L t -> Printf.bprintf b "%s l %s\n" p t
        | F id ->
            let perm, uid = fmeta id in
            Printf.bprintf b "%s f %o %d %S\n" p perm uid (fdata id))
      d.ents
  in
  go "" root;
  Buffer.contents b

let render_real dir =
  let b = Buffer.create 256 in
  let rec go path p =
    let s = Eio.Path.stat ~follow:false p in
    match s.kind with
    | `Directory ->
        Printf.bprintf b "%s/ d %o %Ld\n" path s.perm s.uid;
        List.iter
          (fun n -> go (path ^ "/" ^ n) (p // n))
          (List.sort compare (Eio.Path.read_dir p))
    | `Symbolic_link -> Printf.bprintf b "%s l %s\n" path (Eio.Path.read_link p)
    | `Regular_file ->
        Printf.bprintf b "%s f %o %Ld %S\n" path s.perm s.uid (Eio.Path.load p)
    | _ -> Printf.bprintf b "%s ?\n" path
  in
  go "" dir;
  Buffer.contents b

(* ---------- operations ---------- *)
type op =
  | Create of {
      p : string;
      mode : [ `Excl | `Ifm | `Trunc ];
      data : string;
      sync : bool;
    }
  | Pwrite of { p : string; off : int; data : string }
  | Append of { p : string; data : string }
  | Trunc of { p : string; len : int }
  | Rename of { src : string; dst : string }
  | Unlink of string
  | Mkdir of { p : string; perm : int }
  | Rmdir of string
  | Symlink of { p : string; target : string }
  | Fsync of string
  | Chmod of { p : string; perm : int }
  | Hopen of { h : int; p : string }
  | Hwrite of { h : int; off : int; data : string }
  | Hsync of int
  | Hclose of int
  | Commit
  | Sync_dir of string

let pp_op ppf = function
  | Create { p; mode; data; sync } ->
      Fmt.pf ppf "create %s %s %S%s" p
        (match mode with `Excl -> "excl" | `Ifm -> "ifm" | `Trunc -> "trunc")
        data
        (if sync then " +sync" else "")
  | Pwrite { p; off; data } -> Fmt.pf ppf "pwrite %s @%d %S" p off data
  | Append { p; data } -> Fmt.pf ppf "append %s %S" p data
  | Trunc { p; len } -> Fmt.pf ppf "truncate %s %d" p len
  | Rename { src; dst } -> Fmt.pf ppf "rename %s %s" src dst
  | Unlink p -> Fmt.pf ppf "unlink %s" p
  | Mkdir { p; perm } -> Fmt.pf ppf "mkdir %s %o" p perm
  | Rmdir p -> Fmt.pf ppf "rmdir %s" p
  | Symlink { p; target } -> Fmt.pf ppf "symlink %s -> %s" p target
  | Fsync p -> Fmt.pf ppf "fsync %s" p
  | Chmod { p; perm } -> Fmt.pf ppf "chmod %s %o" p perm
  | Hopen { h; p } -> Fmt.pf ppf "h%d = open %s" h p
  | Hwrite { h; off; data } -> Fmt.pf ppf "h%d pwrite @%d %S" h off data
  | Hsync h -> Fmt.pf ppf "h%d fsync" h
  | Hclose h -> Fmt.pf ppf "h%d close" h
  | Commit -> Fmt.pf ppf "sync_metadata"
  | Sync_dir p -> Fmt.pf ppf "sync_dir %S" p

let paths =
  [|
    "a";
    "b";
    "d0";
    "d1";
    "d0/a";
    "d0/b";
    "d0/d2";
    "d0/d2/a";
    "d1/a";
    "d1/b";
    "l";
    "d0/l";
  |]

let intermediates = [ "d0"; "d1"; "d0/d2" ]
let targets = [| "a"; "b"; "zz"; "l"; "d2" |]
let fperms = [| 0o644; 0o600; 0o444; 0o200; 0o755; 0o000; 0o666 |]
let dperms = [| 0o755; 0o700; 0o555; 0o777; 0o311 |]

let existing m =
  let acc = ref [] in
  let rec go pre d =
    SM.iter
      (fun n e ->
        let p = if pre = "" then n else pre ^ "/" ^ n in
        if Array.mem p paths then acc := p :: !acc;
        match e with D d -> go p d | _ -> ())
      d.ents
  in
  go "" m.root;
  Array.of_list !acc

let gen_op ~crash ~users ~alice ~dirsync m rng =
  let pick a = a.(Random.State.int rng (Array.length a)) in
  let ex = existing m in
  let path () =
    if Array.length ex > 0 && Random.State.int rng 10 < 6 then pick ex
    else pick paths
  in
  let leaf () =
    let p = path () in
    if List.mem p intermediates then "l" else p
  in
  let data () =
    String.init
      (1 + Random.State.int rng 12)
      (fun _ -> Char.chr (97 + Random.State.int rng 26))
  in
  let who =
    if alice || (users && Random.State.int rng 3 = 0) then Alice else Root
  in
  let op =
    match if dirsync then Random.State.int rng 6 else 5 with
    | 0 -> Sync_dir (pick [| ""; "d0"; "d1"; "d0/d2" |])
    | 1 -> Rename { src = path (); dst = path () }
    | 2 -> Mkdir { p = pick [| "d0"; "d1"; "d0/d2" |]; perm = 0o755 }
    | _ -> (
        match Random.State.int rng (if crash then 18 else 17) with
        | 0 | 1 ->
            Create
              {
                p = path ();
                mode = pick [| `Excl; `Ifm; `Trunc |];
                data = data ();
                sync = Random.State.bool rng;
              }
        | 2 ->
            Pwrite
              { p = path (); off = Random.State.int rng 20; data = data () }
        | 3 -> Append { p = path (); data = data () }
        | 4 -> Trunc { p = path (); len = Random.State.int rng 20 }
        | 5 -> Rename { src = path (); dst = path () }
        | 6 -> Unlink (path ())
        | (7 | 16) when (not crash) || Random.State.bool rng ->
            Mkdir { p = path (); perm = (if users then pick dperms else 0o755) }
        | 8 -> Rmdir (path ())
        | 9 -> Symlink { p = leaf (); target = pick targets }
        | 10 -> Fsync (path ())
        | 11 ->
            Chmod
              {
                p = path ();
                perm =
                  (if Random.State.bool rng then pick fperms else pick dperms);
              }
        | 12 -> Hopen { h = Random.State.int rng 3; p = path () }
        | 13 | 14 ->
            Hwrite
              {
                h = Random.State.int rng 3;
                off = Random.State.int rng 20;
                data = data ();
              }
        | 15 ->
            if Random.State.bool rng then Hsync (Random.State.int rng 3)
            else Hclose (Random.State.int rng 3)
        | _ -> Commit)
  in
  (who, op)

(* would [op] put a non-directory at an intermediate name, or traverse one? *)
let unsafe m op =
  let node_at p =
    try
      let comps, n = split p in
      let d = get_dir Root m.root comps in
      SM.find_opt n d.ents
    with Err _ -> None
  in
  match op with
  | Rename { src; dst } when List.mem dst intermediates -> (
      match node_at src with Some (L _) -> true | _ -> false)
  | _ -> false

(* ---------- run one workload ---------- *)
type handle = {
  file : Eio.File.rw_ty Eio.Resource.t;
  id : int;
  sw_close : unit -> unit;
}

let real_op ~dir ~alice_dir who op =
  let d = match who with Root -> dir | Alice -> alice_dir in
  match op with
  | Create { p; mode; data; sync } ->
      let create =
        match mode with
        | `Excl -> `Exclusive 0o644
        | `Ifm -> `If_missing 0o644
        | `Trunc -> `Or_truncate 0o644
      in
      Eio.Path.with_open_out ~create (d // p) (fun f ->
          Eio.Flow.copy_string data f;
          if sync then Eio.File.sync f)
  | Pwrite { p; off; data } ->
      Eio.Path.with_open_out ~create:`Never (d // p) (fun f ->
          Eio.File.pwrite_all f ~file_offset:(i63 off)
            [ Cstruct.of_string data ])
  | Append { p; data } ->
      Eio.Path.with_open_out ~append:true ~create:`Never (d // p) (fun f ->
          Eio.Flow.copy_string data f)
  | Trunc { p; len } ->
      Eio.Path.with_open_out ~create:`Never (d // p) (fun f ->
          Eio.File.truncate f (i63 len))
  | Rename { src; dst } -> Eio.Path.rename (d // src) (d // dst)
  | Unlink p -> Eio.Path.unlink (d // p)
  | Mkdir { p; perm } -> Eio.Path.mkdir ~perm (d // p)
  | Rmdir p -> Eio.Path.rmdir (d // p)
  | Symlink { p; target } -> Eio.Path.symlink ~link_to:target (d // p)
  | Fsync p ->
      Eio.Path.with_open_out ~create:`Never (d // p) (fun f -> Eio.File.sync f)
  | Chmod { p; perm } -> Eio.Path.chmod ~follow:true ~perm (d // p)
  | Hopen _ | Hwrite _ | Hsync _ | Hclose _ | Commit | Sync_dir _ ->
      assert false

let model_op m who op =
  match op with
  | Create { p; mode; data; sync } ->
      let create =
        match mode with
        | `Excl -> `Excl 0o644
        | `Ifm -> `Ifm 0o644
        | `Trunc -> `Trunc 0o644
      in
      let id = m_open m who p ~create in
      if data <> "" then m_write m id (String.length (finfo m id).data * 0) data;
      if sync then m_sync m id
  | Pwrite { p; off; data } ->
      let id = m_open m who p ~create:`Never in
      m_write m id off data
  | Append { p; data } ->
      let id = m_open m who p ~create:`Never in
      m_write m id (String.length (finfo m id).data) data
  | Trunc { p; len } ->
      let id = m_open m who p ~create:`Never in
      m_trunc m id len
  | Rename { src; dst } -> m_rename m who src dst
  | Unlink p -> m_unlink m who p
  | Mkdir { p; perm } -> m_mkdir m who p perm
  | Rmdir p -> m_rmdir m who p
  | Symlink { p; target } -> m_symlink m who p target
  | Fsync p ->
      let id = m_open m who p ~create:`Never in
      m_sync m id
  | Chmod { p; perm } -> m_chmod m who p perm
  | Hopen _ | Hwrite _ | Hsync _ | Hclose _ | Commit | Sync_dir _ ->
      assert false

let new_model () =
  let root = { ents = SM.empty; dperm = 0o755; duid = 0; dgid = 0 } in
  {
    root;
    files = Hashtbl.create 16;
    next_id = 0;
    durable = Hashtbl.create 16;
    pending = Hashtbl.create 16;
    committed = (root, []);
    meta_states = [];
    fsynced = Hashtbl.create 16;
  }

(* model of a crash: versions for block check *)
let apply_change s = function
  | W (o, d) -> write_data s o d
  | R n -> trunc_data s n

exception Mismatch of string

let fail_with trace msg =
  raise
    (Mismatch
       (Fmt.str "%s@.trace:@.%a" msg Fmt.string
          (String.concat "\n" (List.rev trace))))

(* The files of a model tree, with their paths. *)
let model_files root =
  let rec go pre d acc =
    SM.fold
      (fun n e acc ->
        let p = if pre = "" then n else pre ^ "/" ^ n in
        match e with F id -> (p, id) :: acc | D d -> go p d acc | L _ -> acc)
      d.ents acc
  in
  go "" root []

let run_workload ?env ?(alice = false) ~mode ~seed ~len () =
  let rng = Random.State.make [| seed |] in
  let crash = mode <> `Nocrash in
  let users = mode = `Nocrash || alice in
  let dirsync =
    match mode with
    | `Full | `Barriers_drop | `Barriers_reorder _ -> true
    | `Nocrash | `Drop | `Reorder _ -> false
  in
  (match env with None -> Eio_mock.Backend.run | Some _ -> fun f -> f ())
  @@ fun () ->
  let admin =
    match env with
    | None -> (Eio_fs.Fs.v () :> Eio.Fs.dir_ty Eio.Path.t)
    | Some (root, sub) ->
        Eio.Path.mkdir ~perm:0o755 (root // sub);
        root // sub
  in
  let ctl =
    match mode with
    | `Nocrash -> None
    | `Drop -> Some (Crash.v ())
    | `Reorder (s, bs) ->
        Some (Crash.v ~policy:(Crash.Reorder_unsynced s) ~block_size:bs ())
    | `Full -> Some (Crash.v ~policy:(Crash.Survive (fun _ -> true)) ())
    | `Barriers_drop -> Some (Crash.v ())
    | `Barriers_reorder s ->
        Some (Crash.v ~policy:(Crash.Reorder_unsynced s) ())
  in
  let m = new_model () in
  if users then begin
    Eio.Path.chmod ~follow:true ~perm:0o777 admin;
    m.root <- { m.root with dperm = 0o777 };
    m.committed <- (m.root, snd m.committed)
  end;
  let as_alice =
    (Eio_fs.Fs.as_user alice_user admin :> Eio.Fs.dir_ty Eio.Path.t)
  in
  (* As a user, the crash model wraps that user's view of the tree. *)
  let dir, alice_dir =
    match ctl with
    | None -> if alice then (as_alice, as_alice) else (admin, as_alice)
    | Some c when alice ->
        let w = (Crash.wrap c as_alice :> Eio.Fs.dir_ty Eio.Path.t) in
        (w, w)
    | Some c -> ((Crash.wrap c admin :> Eio.Fs.dir_ty Eio.Path.t), as_alice)
  in
  let handles = Array.make 3 None in
  let trace = ref [] in
  (* The inode of each file the model knows, which a rename keeps. *)
  let ino_of_id = Hashtbl.create 16 in
  let note_inos () =
    List.iter
      (fun (p, id) ->
        match Eio.Path.stat ~follow:false (admin // p) with
        | st -> Hashtbl.replace ino_of_id id st.ino
        | exception Eio.Io _ -> ())
      (model_files m.root)
  in
  Eio.Switch.run @@ fun sw ->
  for _ = 1 to len do
    let who, op = gen_op ~crash ~users ~alice ~dirsync m rng in
    if not (unsafe m op) then begin
      let snapshot_files = Hashtbl.copy m.files in
      let saved =
        ( m.root,
          m.next_id,
          Hashtbl.copy m.durable,
          Hashtbl.copy m.pending,
          m.committed,
          m.meta_states )
      in
      let restore () =
        let r, n, du, pe, c, ms = saved in
        m.root <- r;
        m.next_id <- n;
        Hashtbl.reset m.files;
        Hashtbl.iter
          (fun k v -> Hashtbl.replace m.files k { v with data = v.data })
          snapshot_files;
        Hashtbl.reset m.durable;
        Hashtbl.iter (Hashtbl.replace m.durable) du;
        Hashtbl.reset m.pending;
        Hashtbl.iter (Hashtbl.replace m.pending) pe;
        m.committed <- c;
        m.meta_states <- ms
      in
      (* deep copy finfo records so restore works *)
      Hashtbl.filter_map_inplace
        (fun _ v -> Some { v with data = v.data })
        snapshot_files;
      let tag =
        Fmt.str "%s %a"
          (match who with Root -> "root " | Alice -> "alice")
          pp_op op
      in
      let expected =
        match op with
        | Hopen { h; p } -> (
            match m_open m (if alice then who else Root) p ~create:`Never with
            | id -> Ok (`Open (h, id))
            | exception Err e -> Error e
            | exception Skip -> Error "skip")
        | Hwrite { h; off; data } -> (
            match handles.(h) with
            | None -> Error "nohandle"
            | Some hd ->
                m_write m hd.id off data;
                Ok `Unit)
        | Hsync h -> (
            match handles.(h) with
            | None -> Error "nohandle"
            | Some hd ->
                m_sync m hd.id;
                Ok `Unit)
        | Hclose h -> (
            match handles.(h) with
            | None -> Error "nohandle"
            | Some _ -> Ok `Unit)
        | Commit ->
            m_commit m;
            Ok `Unit
        | Sync_dir _ -> Ok `Unit
        | op -> (
            match model_op m who op with
            | () -> Ok `Unit
            | exception Err e ->
                restore ();
                Error e
            | exception Skip ->
                restore ();
                Error "skip")
      in
      if expected <> Error "skip" && expected <> Error "nohandle" then begin
        let actual =
          match op with
          | Hopen { h; p } -> (
              match Eio.Path.open_out ~sw ~create:`Never (dir // p) with
              | f ->
                  (match handles.(h) with
                  | Some hd -> hd.sw_close ()
                  | None -> ());
                  Ok
                    (fun id ->
                      handles.(h) <-
                        Some
                          {
                            file = (f :> Eio.File.rw_ty Eio.Resource.t);
                            id;
                            sw_close = (fun () -> Eio.Resource.close f);
                          })
              | exception (Eio.Io _ as e) -> Error (Printexc.to_string e))
          | Hwrite { h; off; data } -> (
              let hd = Option.get handles.(h) in
              match
                Eio.File.pwrite_all hd.file ~file_offset:(i63 off)
                  [ Cstruct.of_string data ]
              with
              | () -> Ok ignore
              | exception (Eio.Io _ as e) -> Error (Printexc.to_string e))
          | Hsync h -> (
              let hd = Option.get handles.(h) in
              match Eio.File.sync hd.file with
              | () -> Ok ignore
              | exception (Eio.Io _ as e) -> Error (Printexc.to_string e))
          | Hclose h ->
              (Option.get handles.(h)).sw_close ();
              handles.(h) <- None;
              Ok ignore
          | Commit ->
              Crash.sync_metadata (Option.get ctl);
              Ok ignore
          | Sync_dir p -> (
              let d = match who with Root -> dir | Alice -> alice_dir in
              match Eio_fs_sync.sync_dir (if p = "" then d else d // p) with
              | () -> Ok ignore
              | exception Eio.Io _ -> Ok ignore)
          | op -> (
              match real_op ~dir ~alice_dir who op with
              | () -> Ok ignore
              | exception (Eio.Io _ as e) -> Error (Printexc.to_string e))
        in
        trace :=
          (tag ^ match actual with Ok _ -> "  [ok]" | Error _ -> "  [err]")
          :: !trace;
        (match (expected, actual) with
        | Ok (`Open (_, id)), Ok k -> k id
        | Ok _, Ok _ | Error _, Error _ -> ()
        | Ok _, Error e ->
            fail_with !trace (Fmt.str "model ok, real raised %s" e)
        | Error e, Ok _ ->
            fail_with !trace (Fmt.str "model raised %s, real ok" e));
        (* compare live tree *)
        let exp =
          render_model m.root
            (fun id -> (finfo m id).data)
            (fun id ->
              let f = finfo m id in
              (f.fperm, f.fuid))
        in
        let got = render_real admin in
        if exp <> got then
          fail_with !trace
            (Fmt.str "live tree differs@.expected:@.%s@.got:@.%s" exp got);
        note_inos ()
      end
      else restore ()
    end
  done;
  match ctl with
  | None -> ()
  | Some ctl -> (
      let live = render_real admin in
      (* Read the image as root: a user may not read all of it. *)
      let pc = Eio_fs.Fs.as_user Eio_fs.root (Crash.post_crash ctl) in
      let got = render_real pc in
      (* Invariants on the image, by inode. *)
      let image = Eio_fs.Chaos.Private.crash_image ctl in
      let inos = List.map (fun (_, ino, _) -> ino) image in
      (match
         List.find_opt
           (fun i -> List.length (List.filter (Int64.equal i) inos) > 1)
           inos
       with
      | Some i ->
          fail_with !trace (Fmt.str "inode %Ld is twice in the image@.%s" i got)
      | None -> ());
      let id_at ino =
        Hashtbl.fold
          (fun id i acc -> if Int64.equal i ino then Some id else acc)
          ino_of_id None
      in
      (* A file durable at the last snapshot and still live is in the image,
         under some name: nothing that survived removed it, unless it now
         lives below a directory made since, which a barrier may have moved
         it into while that directory is not durable. *)
      let committed_dirs =
        let rec go pre d acc =
          SM.fold
            (fun n e acc ->
              match e with
              | D d ->
                  let p = if pre = "" then n else pre ^ "/" ^ n in
                  go p d (p :: acc)
              | F _ | L _ -> acc)
            d.ents acc
        in
        go "" (fst m.committed) []
      in
      let under_new_dir p =
        let rec ups acc = function
          | [] | [ _ ] -> acc
          | x :: rest ->
              let up = match acc with [] -> x | u :: _ -> u ^ "/" ^ x in
              ups (up :: acc) rest
        in
        List.exists
          (fun d -> not (List.mem d committed_dirs))
          (ups [] (String.split_on_char '/' p))
      in
      let live_ids =
        List.filter_map
          (fun (p, id) -> if under_new_dir p then None else Some id)
          (model_files m.root)
      in
      List.iter
        (fun (_, id) ->
          if List.mem id live_ids then
            match Hashtbl.find_opt ino_of_id id with
            | Some ino when not (List.mem ino inos) ->
                fail_with !trace
                  (Fmt.str "durable file %d (inode %Ld) is lost@.%s" id ino got)
            | Some _ | None -> ())
        (model_files (fst m.committed));
      (* Under Drop_unsynced, a file holds its durable bytes under whatever
         name the image gives it. *)
      (match mode with
      | `Drop | `Barriers_drop ->
          List.iter
            (fun (p, ino, kind) ->
              match (kind, id_at ino) with
              | `File _, Some id ->
                  let bytes = Eio.Path.load (pc // p) in
                  let durable = Hashtbl.find m.durable id in
                  if bytes <> durable then
                    fail_with !trace
                      (Fmt.str "%s (file %d) holds %S, not its durable %S@.%s" p
                         id bytes durable got)
              | _ -> ())
            image
      | `Nocrash | `Reorder _ | `Full | `Barriers_reorder _ -> ());
      let perm_of perms id =
        try List.assoc id perms with Not_found -> 0o7777
      in
      (* A sync made a file's mode durable before its name: it overrides the
         mode of a state from before it. *)
      let synced ?(state = 0) perms =
        List.map
          (fun (id, p) ->
            match Hashtbl.find_opt m.fsynced id with
            | Some (q, at) when at >= state -> (id, q)
            | Some _ | None -> (id, p))
          perms
      in
      match mode with
      | `Full ->
          if got <> live then
            fail_with !trace
              (Fmt.str
                 "every change survived, but the image is not the live \
                  tree@.live:@.%s@.image:@.%s"
                 live got)
      | `Drop ->
          let tree, perms = m.committed in
          let perms = synced perms in
          let exp =
            render_model tree
              (fun id -> Hashtbl.find m.durable id)
              (fun id -> (perm_of perms id, (finfo m id).fuid))
          in
          if exp <> got then
            fail_with !trace
              (Fmt.str "Drop_unsynced image differs@.expected:@.%s@.got:@.%s"
                 exp got)
      | `Reorder (_, bs) ->
          (* candidate states: committed, then each meta state oldest first *)
          let states = m.committed :: List.rev m.meta_states in
          let versions id =
            let d = Hashtbl.find m.durable id in
            let ch =
              List.rev
                (Option.value ~default:[] (Hashtbl.find_opt m.pending id))
            in
            let vs, _ =
              List.fold_left
                (fun (acc, cur) c ->
                  let n = apply_change cur c in
                  (n :: acc, n))
                ([ d ], d) ch
            in
            List.rev vs
          in
          let block s b =
            let off = b * bs in
            let l = max 0 (min bs (String.length s - off)) in
            if l = 0 then "" else String.sub s off l
          in
          (* A crash keeps one length, the one version [i] left, and each
             block as some version [v] left it; a shrink between [v] and [i]
             has landed, so the block reads zeros past it. *)
          let file_ok id content =
            let vs = Array.of_list (versions id) in
            let sizes = Array.map String.length vs in
            let lo = Array.fold_left min max_int sizes
            and hi = Array.fold_left max 0 sizes in
            let n = String.length content in
            let shrinks =
              List.filter_map
                (fun k ->
                  if sizes.(k) < sizes.(k - 1) then Some (k, sizes.(k))
                  else None)
                (List.init (Array.length vs - 1) (fun k -> k + 1))
            in
            let lengths =
              List.filter
                (fun i ->
                  sizes.(i) = n
                  || n mod bs = 0
                     && i > 0
                     && sizes.(i - 1) < n
                     && n < sizes.(i))
                (List.init (Array.length vs) Fun.id)
            in
            let size_ok = lengths <> [] || (n mod bs = 0 && n > lo && n < hi) in
            let nb = if n = 0 then 0 else ((n - 1) / bs) + 1 in
            let block_ok i b =
              let got = block content b in
              Array.to_list (Array.mapi (fun v s -> (v, s)) vs)
              |> List.exists (fun (v, s) ->
                  let vb = block s b in
                  let vb =
                    Bytes.of_string
                      (vb
                      ^ String.make
                          (max 0 (String.length got - String.length vb))
                          '\000')
                  in
                  List.iter
                    (fun (k, m) ->
                      if v < k && k <= i then
                        for o = max 0 (m - (b * bs)) to Bytes.length vb - 1 do
                          Bytes.set vb o '\000'
                        done)
                    shrinks;
                  Bytes.sub_string vb 0 (String.length got) = got)
            in
            let blocks_ok i = List.for_all (block_ok i) (List.init nb Fun.id) in
            let indexes =
              if lengths = [] then List.init (Array.length vs) Fun.id
              else lengths
            in
            size_ok && List.exists blocks_ok indexes
          in
          (* parse got into path -> line *)
          let ok_state1 (tree, perms) =
            let shape =
              render_model tree
                (fun _ -> "")
                (fun id -> (perm_of perms id, (finfo m id).fuid))
            in
            let strip s =
              String.split_on_char '\n' s
              |> List.map (fun l ->
                  match String.index_opt l '"' with
                  | Some i -> String.sub l 0 i
                  | None -> l)
              |> String.concat "\n"
            in
            strip shape = strip got
            &&
            (* content check per file *)
            let rec files path d acc =
              SM.fold
                (fun n e acc ->
                  let p = path ^ "/" ^ n in
                  match e with
                  | D d -> files p d acc
                  | F id -> (p, id) :: acc
                  | L _ -> acc)
                d.ents acc
            in
            List.for_all
              (fun (p, id) ->
                let content =
                  Eio.Path.load (pc // String.sub p 1 (String.length p - 1))
                in
                file_ok id content)
              (files "" tree [])
          in
          let ok_state state (tree, perms) =
            ok_state1 (tree, synced ~state perms)
          in
          if
            not
              (List.exists
                 (fun (i, st) -> ok_state i st)
                 (List.mapi (fun i st -> (i, st)) states))
          then
            fail_with !trace
              (Fmt.str
                 "Reorder image matches no prefix \
                  state@.got:@.%s@.committed:@.%s"
                 got
                 (render_model (fst m.committed)
                    (fun id -> Hashtbl.find m.durable id)
                    (fun _ -> (0, 0))))
      | `Nocrash | `Barriers_drop | `Barriers_reorder _ -> ())

let modes =
  [
    ("no crash, root and a user", false, fun _ -> `Nocrash);
    ("Drop_unsynced", false, fun _ -> `Drop);
    ("Drop_unsynced, as a user", true, fun _ -> `Drop);
    ( "Reorder_unsynced, 4096-byte blocks",
      false,
      fun seed -> `Reorder (seed, 4096) );
    ( "Reorder_unsynced, 4096-byte blocks, as a user",
      true,
      fun seed -> `Reorder (seed, 4096) );
    ("Reorder_unsynced, 1-byte blocks", false, fun seed -> `Reorder (seed, 1));
    ("every change survives, with directory barriers", false, fun _ -> `Full);
    ( "every change survives, with directory barriers, as a user",
      true,
      fun _ -> `Full );
    ("no crash, a user alone", true, fun _ -> `Nocrash);
    ("Drop_unsynced, with directory barriers", false, fun _ -> `Barriers_drop);
    ( "Drop_unsynced, with directory barriers, as a user",
      true,
      fun _ -> `Barriers_drop );
    ( "Reorder_unsynced, with directory barriers",
      false,
      fun seed -> `Barriers_reorder seed );
  ]

(* Each mode runs 1000 seeds of 40 operations. *)
let test (alice, mode) () =
  for seed = 0 to 999 do
    match run_workload ~alice ~mode:(mode seed) ~seed ~len:40 () with
    | () -> ()
    | exception Mismatch s -> Alcotest.failf "seed %d: %s" seed s
  done

let suite =
  ( "model",
    List.map
      (fun (name, alice, mode) ->
        Alcotest.test_case name `Quick (test (alice, mode)))
      modes )
