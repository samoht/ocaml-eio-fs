(* A second reference crash model, written from lib/chaos.mli, POSIX fsync
   and rename, and ext4 data=writeback, apart from the first one and from
   the crash model it checks. It runs a workload on a logical filesystem and
   answers, for a crash at the end of it, which post-crash images the
   documented envelope permits. *)

type op =
  | Mkdir of string
  | Rmdir of string
  | Creat of string
  | Write of string * int * string
  | Append of string * string
  | Truncate of string * int
  | Rename of string * string
  | Unlink of string
  | Symlink of string * string (* target, path *)
  | Chmod of string * int
  | Chown of string * int * int
  | Fsync of string
  | Fsync_ro of string
  | Fsync_dir of string
  | Fsync_fail of string
  | Sync_meta
  | Hopen of int * string
  | Hwrite of int * int * string
  | Hsync of int
  | Dopen of int * string
  | Dsync of int

let pp_op ppf = function
  | Mkdir p -> Fmt.pf ppf "mkdir %s" p
  | Rmdir p -> Fmt.pf ppf "rmdir %s" p
  | Creat p -> Fmt.pf ppf "creat %s" p
  | Write (p, o, d) -> Fmt.pf ppf "pwrite %s @%d %S" p o d
  | Append (p, d) -> Fmt.pf ppf "append %s %S" p d
  | Truncate (p, n) -> Fmt.pf ppf "truncate %s %d" p n
  | Rename (a, b) -> Fmt.pf ppf "rename %s %s" a b
  | Unlink p -> Fmt.pf ppf "unlink %s" p
  | Symlink (t, p) -> Fmt.pf ppf "symlink %s -> %s" p t
  | Chmod (p, m) -> Fmt.pf ppf "chmod %s %o" p m
  | Chown (p, u, g) -> Fmt.pf ppf "chown %s %d:%d" p u g
  | Fsync p -> Fmt.pf ppf "fsync %s" p
  | Fsync_ro p -> Fmt.pf ppf "fsync(ro) %s" p
  | Fsync_dir p -> Fmt.pf ppf "fsyncdir %s" p
  | Fsync_fail p -> Fmt.pf ppf "fsync %s FAILS" p
  | Sync_meta -> Fmt.pf ppf "sync_metadata"
  | Hopen (h, p) -> Fmt.pf ppf "h%d = open %s" h p
  | Hwrite (h, o, d) -> Fmt.pf ppf "pwrite h%d @%d %S" h o d
  | Hsync h -> Fmt.pf ppf "fsync h%d" h
  | Dopen (h, p) -> Fmt.pf ppf "d%d = opendir %s" h p
  | Dsync h -> Fmt.pf ppf "fsyncdir d%d" h

type init =
  | I_dir of string
  | I_file of string * string
  | I_link of string * string

type user = { uid : int; gid : int; groups : int list; is_root : bool }

(* {1 Logical filesystem} *)

type k =
  | File of { mutable data : string }
  | Dir of { mutable ents : (string * int) list }
  | Link of string

type node = {
  id : int;
  mutable k : k;
  mutable mode : int;
  mutable uid : int;
  mutable gid : int;
}

exception Fail of string

let fail fmt = Fmt.kstr (fun s -> raise (Fail s)) fmt

(* {1 Journal} *)

type ek =
  | Add of { dir : int; name : string; node : int }
  | Del of { dir : int; name : string; node : int; is_dir : bool }
  | Mv of {
      sdir : int;
      sname : string;
      ddir : int;
      dname : string;
      node : int;
      victim : int option;
      victim_dir : bool;
      anc : int list;
    }
  | Chmod_e of { node : int; perm : int }
  | Chown_e of { node : int; uid : int; gid : int; clear : bool }
  | Suid_clear of { node : int }

type ent = {
  seq : int;
  ek : ek;
  dirs : int list;
  owner : int option;
  attr_node : int option;
  tag : string;
}

type change = W of int * string | T of int

type fdata = {
  mutable durable : string;
  mutable shadow : string; (* durable + non-lost pending changes *)
  mutable cache : string;
      (* the page cache: what reads see, lost writes included *)
  mutable base_cache : string; (* the page cache at the last sync *)
  mutable evicted : string;
      (* the durable bytes had the pages a failed sync lost been evicted
         before a later write to them *)
  mutable cached : int list;
      (* truncations a failed sync left pending, which the page cache holds *)
  mutable pend : (int * change * string * string) list;
      (* seq, change, shadow after, page cache after *)
  mutable pending_clear : int option; (* seq of the write that cleared setuid *)
}

type barrier = { bdir : int; covered : int list (* seqs *) }

type t = {
  bs : int; (* the page size a sync after a failed one writes whole *)
  user : user;
  nodes : (int, node) Hashtbl.t;
  mutable next_id : int;
  mutable clock : int;
  mutable journal : ent list; (* reversed *)
  mutable forced : int list; (* seqs committed by an unconditional barrier *)
  mutable barriers : barrier list;
  initial : (int, unit) Hashtbl.t; (* nodes in the durable starting image *)
  fdata : (int, fdata) Hashtbl.t;
  handles : (int, int) Hashtbl.t; (* handle -> inode *)
  dhandles : (int, int) Hashtbl.t;
  init_attrs : (int, int * int * int) Hashtbl.t; (* durable mode, uid, gid *)
  mutable data_log : (int * int * change * string) list;
      (* reversed: seq, node, change, resolved path *)
  create_attrs : (int, int * int * int) Hashtbl.t;
  init_maps : (int, (string * int) list) Hashtbl.t;
}

let node t id = Hashtbl.find t.nodes id

let new_node t k mode =
  let id = t.next_id in
  t.next_id <- id + 1;
  let n = { id; k; mode; uid = t.user.uid; gid = t.user.gid } in
  Hashtbl.replace t.nodes id n;
  Hashtbl.replace t.create_attrs id (mode, n.uid, n.gid);
  n

let ents t d = match (node t d).k with Dir r -> r.ents | _ -> fail "ENOTDIR"

let set_ents t d e =
  match (node t d).k with Dir r -> r.ents <- e | _ -> assert false

let lookup t d name = List.assoc_opt name (ents t d)

let parent_of t id =
  Hashtbl.fold
    (fun pid n acc ->
      match n.k with
      | Dir { ents } when List.exists (fun (_, c) -> c = id) ents -> Some pid
      | _ -> acc)
    t.nodes None

let rec ancestors t id =
  match parent_of t id with None -> [ id ] | Some p -> id :: ancestors t p

let split p =
  let comps =
    String.split_on_char '/' p |> List.filter (fun s -> s <> "" && s <> ".")
  in
  comps

(* Resolve all but the last component, following links. Returns (dir, last). *)
let rec walk t depth dir comps =
  if depth > 16 then fail "ELOOP";
  match comps with
  | [] -> (dir, None)
  | [ x ] -> (dir, Some x)
  | x :: rest ->
      let d = enter t depth dir x in
      walk t depth d rest

and enter t depth dir x =
  if x = ".." then match parent_of t dir with Some p -> p | None -> dir
  else
    match lookup t dir x with
    | None -> fail "ENOENT %s" x
    | Some id -> (
        match (node t id).k with
        | Dir _ -> id
        | Link tgt -> (
            let d, last = walk t (depth + 1) dir (split tgt) in
            match last with None -> d | Some l -> enter t (depth + 1) d l)
        | File _ -> fail "ENOTDIR")

let resolve_parent t p =
  match walk t 0 0 (split p) with _, None -> fail "root" | d, Some l -> (d, l)

let rec resolve_follow t depth p_dir name =
  if depth > 16 then fail "ELOOP";
  if name = ".." then
    match parent_of t p_dir with Some p -> Some p | None -> Some p_dir
  else
    match lookup t p_dir name with
    | None -> None
    | Some id -> (
        match (node t id).k with
        | Link tgt -> (
            match walk t (depth + 1) p_dir (split tgt) with
            | d, None -> Some d
            | d, Some l -> resolve_follow t (depth + 1) d l)
        | _ -> Some id)

let resolve t p =
  match split p with
  | [] -> 0
  | _ -> (
      let d, l = resolve_parent t p in
      match resolve_follow t 0 d l with None -> fail "ENOENT" | Some id -> id)

(* The path of a node from the root, its links resolved. *)
let path_of t id =
  let rec go id acc =
    match parent_of t id with
    | None -> acc
    | Some p ->
        let name = fst (List.find (fun (_, c) -> c = id) (ents t p)) in
        go p (name :: acc)
  in
  String.concat "/" (go id [])

let fd t id =
  match Hashtbl.find_opt t.fdata id with
  | Some f -> f
  | None ->
      let f =
        {
          durable = "";
          shadow = "";
          cache = "";
          base_cache = "";
          cached = [];
          evicted = "";
          pend = [];
          pending_clear = None;
        }
      in
      Hashtbl.replace t.fdata id f;
      f

let apply_change s = function
  | W (off, d) ->
      let len = max (String.length s) (off + String.length d) in
      let b = Bytes.make len '\000' in
      Bytes.blit_string s 0 b 0 (String.length s);
      Bytes.blit_string d 0 b off (String.length d);
      Bytes.to_string b
  | T n ->
      if n <= String.length s then String.sub s 0 n
      else s ^ String.make (n - String.length s) '\000'

let journal t ?at ?owner ?attr_node ~dirs ~tag ek =
  let seq =
    match at with
    | Some s -> s
    | None ->
        let s = t.clock in
        t.clock <- s + 1;
        s
  in
  t.journal <- { seq; ek; dirs; owner; attr_node; tag } :: t.journal;
  seq

let v ?(bs = 4096) ~user init =
  let t =
    {
      bs;
      user;
      nodes = Hashtbl.create 16;
      next_id = 0;
      clock = 0;
      journal = [];
      forced = [];
      barriers = [];
      initial = Hashtbl.create 16;
      fdata = Hashtbl.create 16;
      handles = Hashtbl.create 4;
      dhandles = Hashtbl.create 4;
      init_attrs = Hashtbl.create 16;
      data_log = [];
      create_attrs = Hashtbl.create 16;
      init_maps = Hashtbl.create 16;
    }
  in
  let root = new_node t (Dir { ents = [] }) 0o755 in
  ignore root;
  List.iter
    (fun i ->
      let p, k, mode =
        match i with
        | I_dir p -> (p, Dir { ents = [] }, 0o755)
        | I_file (p, c) -> (p, File { data = c }, 0o644)
        | I_link (p, tg) -> (p, Link tg, 0o777)
      in
      let d, l = resolve_parent t p in
      let n = new_node t k mode in
      set_ents t d (ents t d @ [ (l, n.id) ]);
      match k with
      | File { data } ->
          let f = fd t n.id in
          f.durable <- data;
          f.shadow <- data;
          f.cache <- data;
          f.base_cache <- data;
          f.evicted <- data
      | _ -> ())
    init;
  Hashtbl.iter
    (fun id n ->
      Hashtbl.replace t.initial id ();
      Hashtbl.replace t.init_attrs id (n.mode, n.uid, n.gid);
      match n.k with
      | Dir { ents } -> Hashtbl.replace t.init_maps id ents
      | _ -> ())
    t.nodes;
  t

let umask = 0o022

(* The blocks [ch] dirties in [before], as chaos.mli says: a write those it
   covers and, past the end, those of the hole it leaves; a shrink only the
   block it cuts mid-way; a growth the blocks it adds. *)
let blocks_dirtied ~bs before ch =
  let ol = String.length before in
  let lo, hi =
    match ch with
    | W (off, d) -> (min ol off, off + String.length d - 1)
    | T n when n < ol -> if n mod bs = 0 then (1, 0) else (n, n)
    | T n -> (ol, n - 1)
  in
  if hi < lo then []
  else List.init ((hi / bs) - (lo / bs) + 1) (fun i -> (lo / bs) + i)

(* [durable] with the pending changes [chs] (seq, change) replayed, where a
   block they dirty in the page cache, [base_cache] with the same changes,
   is written whole from it: it still holds what a failed sync lost. A
   truncation in [cached] is one the page cache already holds. *)
let merged ~bs ~durable ~base_cache ~cached chs =
  let plain, cache, dirty =
    List.fold_left
      (fun (plain, cache, dirty) (seq, ch) ->
        let c = List.mem seq cached in
        ( apply_change plain ch,
          (if c then cache else apply_change cache ch),
          blocks_dirtied ~bs (if c then plain else cache) ch @ dirty ))
      (durable, base_cache, []) chs
  in
  let b = Bytes.of_string plain in
  List.iter
    (fun i ->
      for x = i * bs to ((i + 1) * bs) - 1 do
        if x < Bytes.length b && x < String.length cache then
          Bytes.set b x cache.[x]
      done)
    dirty;
  Bytes.to_string b

let record_data t id ch =
  let f = fd t id in
  let seq = t.clock in
  t.clock <- seq + 1;
  f.shadow <- apply_change f.shadow ch;
  f.cache <- apply_change f.cache ch;
  f.pend <- f.pend @ [ (seq, ch, f.shadow, f.cache) ];
  t.data_log <- (seq, id, ch, path_of t id) :: t.data_log;
  seq

let do_write t id ch =
  let n = node t id in
  (match n.k with
  | File r -> r.data <- apply_change r.data ch
  | _ -> fail "EISDIR");
  let seq = record_data t id ch in
  (* A write by a non-root user clears setuid, and setgid when group-exec. *)
  match ch with
  | W _ when not t.user.is_root ->
      let clr = if n.mode land 0o010 <> 0 then 0o6000 else 0o4000 in
      if n.mode land clr <> 0 then begin
        n.mode <- n.mode land lnot clr;
        let f = fd t id in
        if f.pending_clear = None then f.pending_clear <- Some seq
      end
  | _ -> ()

let file_sync t id ~ok =
  let f = fd t id in
  let n = node t id in
  (match n.k with File _ -> () | _ -> ());
  if ok then begin
    f.evicted <-
      List.fold_left (fun c (_, ch, _, _) -> apply_change c ch) f.evicted f.pend;
    f.durable <-
      merged ~bs:t.bs ~durable:f.durable ~base_cache:f.base_cache
        ~cached:f.cached
        (List.map (fun (s, c, _, _) -> (s, c)) f.pend);
    f.shadow <- f.durable;
    f.base_cache <- f.cache;
    f.cached <- [];
    f.pend <- [];
    (* the write's setuid clear enters the journal now, committed *)
    (match f.pending_clear with
    | Some wseq ->
        let s =
          journal t ~at:wseq ~attr_node:id
            ~owner:(Option.value ~default:0 (parent_of t id))
            ~dirs:[] ~tag:"suidclear"
            (Suid_clear { node = id })
        in
        t.forced <- s :: t.forced;
        f.pending_clear <- None
    | None -> ());
    (* A sync of a file commits a chmod or chown of it. *)
    List.iter
      (fun e -> if e.attr_node = Some id then t.forced <- e.seq :: t.forced)
      t.journal
  end
  else begin
    (* A failed sync loses the data writes; a truncation is metadata, and
       stays pending. *)
    let kept =
      List.filter (function _, T _, _, _ -> true | _ -> false) f.pend
    in
    f.pend <- [];
    f.shadow <- f.durable;
    f.base_cache <- f.cache;
    List.iter
      (fun (seq, ch, _, _) ->
        f.shadow <- apply_change f.shadow ch;
        f.pend <- f.pend @ [ (seq, ch, f.shadow, f.cache) ];
        f.cached <- seq :: f.cached)
      kept
  end

(* A barrier on directory [d]: covers its entries and a chmod/chown of one of
   them. [self_attr]: also a chmod/chown of [d] itself (POSIX fsync of the
   directory's inode). *)
let self_attr = ref true

let dir_barrier t d =
  let covered =
    List.filter_map
      (fun e ->
        if
          List.mem d e.dirs
          || (e.owner = Some d && e.attr_node <> None)
          || (!self_attr && e.attr_node = Some d)
        then Some e.seq
        else None)
      t.journal
  in
  t.barriers <- { bdir = d; covered } :: t.barriers

let is_dir t id = match (node t id).k with Dir _ -> true | _ -> false

let rec is_under t ~anc id =
  id = anc
  || match parent_of t id with None -> false | Some p -> is_under t ~anc p

let inherit_gid t d (n : node) ~dir =
  let pd = node t d in
  if pd.mode land 0o2000 <> 0 then begin
    n.gid <- pd.gid;
    if dir then n.mode <- n.mode lor 0o2000;
    Hashtbl.replace t.create_attrs n.id (n.mode, n.uid, n.gid)
  end

let run_op t op =
  match op with
  | Mkdir p ->
      let d, l = resolve_parent t p in
      if lookup t d l <> None then fail "EEXIST";
      let n = new_node t (Dir { ents = [] }) (0o755 land lnot umask) in
      inherit_gid t d n ~dir:true;
      set_ents t d (ents t d @ [ (l, n.id) ]);
      ignore
        (journal t ~dirs:[ d ] ~tag:("mkdir " ^ p)
           (Add { dir = d; name = l; node = n.id }))
  | Creat p ->
      let d, l = resolve_parent t p in
      if lookup t d l <> None then fail "EEXIST";
      let n = new_node t (File { data = "" }) (0o644 land lnot umask) in
      inherit_gid t d n ~dir:false;
      set_ents t d (ents t d @ [ (l, n.id) ]);
      ignore (fd t n.id);
      ignore
        (journal t ~dirs:[ d ] ~tag:("creat " ^ p)
           (Add { dir = d; name = l; node = n.id }))
  | Symlink (tg, p) ->
      let d, l = resolve_parent t p in
      if lookup t d l <> None then fail "EEXIST";
      let n = new_node t (Link tg) 0o777 in
      set_ents t d (ents t d @ [ (l, n.id) ]);
      ignore
        (journal t ~dirs:[ d ] ~tag:("symlink " ^ p)
           (Add { dir = d; name = l; node = n.id }))
  | Unlink p ->
      let d, l = resolve_parent t p in
      let id =
        match lookup t d l with None -> fail "ENOENT" | Some id -> id
      in
      if is_dir t id then fail "EISDIR";
      set_ents t d (List.remove_assoc l (ents t d));
      ignore
        (journal t ~dirs:[ d ] ~tag:("unlink " ^ p)
           (Del { dir = d; name = l; node = id; is_dir = false }))
  | Rmdir p ->
      let d, l = resolve_parent t p in
      let id =
        match lookup t d l with None -> fail "ENOENT" | Some id -> id
      in
      if not (is_dir t id) then fail "ENOTDIR";
      if ents t id <> [] then fail "ENOTEMPTY";
      set_ents t d (List.remove_assoc l (ents t d));
      ignore
        (journal t ~dirs:[ d ] ~tag:("rmdir " ^ p)
           (Del { dir = d; name = l; node = id; is_dir = true }))
  | Rename (a, b) ->
      let sd, sl = resolve_parent t a in
      let id =
        match lookup t sd sl with None -> fail "ENOENT" | Some id -> id
      in
      let dd, dl = resolve_parent t b in
      let victim = lookup t dd dl in
      if victim = Some id then ()
      else begin
        if is_dir t id && is_under t ~anc:id dd then fail "EINVAL";
        (match victim with
        | Some v when is_dir t v ->
            if not (is_dir t id) then fail "EISDIR";
            if ents t v <> [] then fail "ENOTEMPTY"
        | Some _ -> if is_dir t id then fail "ENOTDIR"
        | None -> ());
        set_ents t sd (List.remove_assoc sl (ents t sd));
        set_ents t dd (List.remove_assoc dl (ents t dd) @ [ (dl, id) ]);
        let anc = ancestors t dd in
        let victim_dir =
          match victim with Some v -> is_dir t v | None -> false
        in
        ignore
          (journal t
             ~dirs:(if sd = dd then [ sd ] else [ sd; dd ])
             ~tag:(Fmt.str "rename %s %s" a b)
             (Mv
                {
                  sdir = sd;
                  sname = sl;
                  ddir = dd;
                  dname = dl;
                  node = id;
                  victim;
                  victim_dir;
                  anc;
                }))
      end
  | Chmod (p, perm) ->
      let id = resolve t p in
      let n = node t id in
      if (not t.user.is_root) && n.uid <> t.user.uid then fail "EPERM";
      n.mode <- perm;
      ignore
        (journal t ?owner:(parent_of t id) ~attr_node:id ~dirs:[]
           ~tag:("chmod " ^ p)
           (Chmod_e { node = id; perm }))
  | Chown (p, uid, gid) ->
      let id = resolve t p in
      let n = node t id in
      if not t.user.is_root then begin
        if n.uid <> t.user.uid || uid <> n.uid then fail "EPERM";
        if not (List.mem gid (t.user.gid :: t.user.groups)) then fail "EPERM"
      end;
      n.uid <- uid;
      n.gid <- gid;
      let clear = match n.k with File _ -> true | _ -> false in
      if clear then
        n.mode <-
          n.mode land lnot (if n.mode land 0o010 <> 0 then 0o6000 else 0o4000);
      ignore
        (journal t ?owner:(parent_of t id) ~attr_node:id ~dirs:[]
           ~tag:("chown " ^ p)
           (Chown_e { node = id; uid; gid; clear }))
  | Write (p, off, d) -> do_write t (resolve t p) (W (off, d))
  | Append (p, d) ->
      let id = resolve t p in
      let len =
        match (node t id).k with
        | File r -> String.length r.data
        | _ -> fail "EISDIR"
      in
      do_write t id (W (len, d))
  | Truncate (p, n) ->
      let id = resolve t p in
      (match (node t id).k with
      | File r -> r.data <- apply_change r.data (T n)
      | _ -> fail "EISDIR");
      let seq = record_data t id (T n) in
      let nd = node t id in
      if not t.user.is_root then begin
        let clr = if nd.mode land 0o010 <> 0 then 0o6000 else 0o4000 in
        if nd.mode land clr <> 0 then begin
          nd.mode <- nd.mode land lnot clr;
          let f = fd t id in
          if f.pending_clear = None then f.pending_clear <- Some seq
        end
      end
  | Fsync p ->
      let id = resolve t p in
      if is_dir t id then fail "EISDIR";
      file_sync t id ~ok:true
  | Fsync_ro p ->
      let id = resolve t p in
      if is_dir t id then dir_barrier t id else file_sync t id ~ok:true
  | Fsync_fail p ->
      let id = resolve t p in
      if is_dir t id then fail "EISDIR";
      file_sync t id ~ok:false;
      fail "injected EIO"
  | Fsync_dir p ->
      let id = resolve t p in
      if not (is_dir t id) then fail "ENOTDIR";
      dir_barrier t id
  | Sync_meta ->
      (* A journal commit carries the inodes writes dirtied: their setuid
         clears. *)
      Hashtbl.iter
        (fun id f ->
          match f.pending_clear with
          | Some wseq ->
              ignore
                (journal t ~at:wseq ~attr_node:id
                   ~owner:(Option.value ~default:0 (parent_of t id))
                   ~dirs:[] ~tag:"suidclear"
                   (Suid_clear { node = id }));
              f.pending_clear <- None
          | None -> ())
        t.fdata;
      List.iter (fun e -> t.forced <- e.seq :: t.forced) t.journal
  | Hopen (h, p) ->
      let id = resolve t p in
      if is_dir t id then fail "EISDIR";
      Hashtbl.replace t.handles h id
  | Hwrite (h, off, d) -> (
      match Hashtbl.find_opt t.handles h with
      | None -> fail "EBADF"
      | Some id -> do_write t id (W (off, d)))
  | Hsync h -> (
      match Hashtbl.find_opt t.handles h with
      | None -> fail "EBADF"
      | Some id -> file_sync t id ~ok:true)
  | Dopen (h, p) ->
      let id = resolve t p in
      if not (is_dir t id) then fail "ENOTDIR";
      Hashtbl.replace t.dhandles h id
  | Dsync h -> (
      match Hashtbl.find_opt t.dhandles h with
      | None -> fail "EBADF"
      | Some id -> dir_barrier t id)

(* {1 Crash states} *)

let entries t = List.sort (fun a b -> compare a.seq b.seq) t.journal
let find_ent t s = List.find (fun e -> e.seq = s) t.journal

module IS = Set.Make (Int)

let needs t e =
  let earlier = List.filter (fun e' -> e'.seq < e.seq) (entries t) in
  let in_dir d =
    List.filter_map
      (fun e' -> if List.mem d e'.dirs then Some e'.seq else None)
      earlier
  in
  let a = List.concat_map in_dir e.dirs in
  let b =
    match e.ek with
    | Del { node; is_dir = true; _ } -> in_dir node
    | Mv { victim = Some v; victim_dir = true; _ } -> in_dir v
    | _ -> []
  in
  let c =
    match e.ek with
    | Mv { anc; _ } ->
        List.filter_map
          (fun e' ->
            match e'.ek with
            | Mv { node; _ } when List.mem node anc -> Some e'.seq
            | _ -> None)
          earlier
    | _ -> []
  in
  a @ b @ c

let rec close t s =
  let s' =
    IS.fold
      (fun x acc ->
        List.fold_left (fun acc y -> IS.add y acc) acc (needs t (find_ent t x)))
      s s
  in
  if IS.equal s s' then s else close t s'

let dir_durable t s d =
  Hashtbl.mem t.initial d
  || IS.exists
       (fun x ->
         match (find_ent t x).ek with Add { node; _ } -> node = d | _ -> false)
       s

(* Whether an entry a sync or a barrier on [bdir] reached holds in [s]: a
   chmod or chown once its node is durable, another entry once that
   directory or one it changes is. *)
let holds t s ~bdir e =
  match e.attr_node with
  | Some n -> dir_durable t s n
  | None ->
      (match bdir with Some d -> dir_durable t s d | None -> true)
      || List.exists (dir_durable t s) e.dirs

(* The entries committed with the uncommitted prefix [kept] surviving. *)
let committed_with t kept =
  let forced_attr, forced =
    List.partition (fun x -> (find_ent t x).attr_node <> None) t.forced
  in
  let reached =
    List.map (fun x -> (None, x)) forced_attr
    @ List.concat_map
        (fun b -> List.map (fun x -> (Some b.bdir, x)) b.covered)
        t.barriers
  in
  let s = ref (close t (IS.union (IS.of_list forced) kept)) in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (bdir, x) ->
        if (not (IS.mem x !s)) && holds t !s ~bdir (find_ent t x) then begin
          s := close t (IS.add x !s);
          changed := true
        end)
      reached
  done;
  !s

let committed t = committed_with t IS.empty

let uncommitted t =
  let c = committed t in
  List.filter (fun e -> not (IS.mem e.seq c)) (entries t)

let survivors t k =
  let u = uncommitted t in
  let pre = List.filteri (fun i _ -> i < k) u |> List.map (fun e -> e.seq) in
  committed_with t (IS.of_list pre)

(* {2 Metadata image} *)

type img = {
  present : (int, unit) Hashtbl.t;
  maps : (int, (string * int) list) Hashtbl.t;
  attrs : (int, int * int * int) Hashtbl.t;
}

let clear_mode m = m land lnot (if m land 0o010 <> 0 then 0o6000 else 0o4000)

let meta_image t s =
  let present = Hashtbl.create 16
  and maps = Hashtbl.copy t.init_maps
  and attrs = Hashtbl.copy t.init_attrs in
  Hashtbl.iter (fun id () -> Hashtbl.replace present id ()) t.initial;
  let detach node =
    Hashtbl.filter_map_inplace
      (fun _ m -> Some (List.filter (fun (_, c) -> c <> node) m))
      maps
  in
  let get d = Option.value ~default:[] (Hashtbl.find_opt maps d) in
  List.iter
    (fun e ->
      if IS.mem e.seq s then
        match e.ek with
        | Add { dir; name; node } ->
            Hashtbl.replace present node ();
            Hashtbl.replace attrs node (Hashtbl.find t.create_attrs node);
            if is_dir t node && not (Hashtbl.mem maps node) then
              Hashtbl.replace maps node [];
            if Hashtbl.mem present dir then
              Hashtbl.replace maps dir
                (List.remove_assoc name (get dir) @ [ (name, node) ])
        | Del { dir; name; node; is_dir } ->
            if
              Hashtbl.mem present dir
              && List.assoc_opt name (get dir) = Some node
            then
              if is_dir && get node <> [] then ()
              else Hashtbl.replace maps dir (List.remove_assoc name (get dir))
        | Mv { ddir; dname; node; _ } ->
            if Hashtbl.mem present node then begin
              detach node;
              if Hashtbl.mem present ddir then
                Hashtbl.replace maps ddir
                  (List.remove_assoc dname (get ddir) @ [ (dname, node) ])
            end
        | Chmod_e { node; perm } -> (
            match Hashtbl.find_opt attrs node with
            | Some (_, u, g) when Hashtbl.mem present node ->
                Hashtbl.replace attrs node (perm, u, g)
            | _ -> ())
        | Chown_e { node; uid; gid; clear } -> (
            match Hashtbl.find_opt attrs node with
            | Some (m, _, _) when Hashtbl.mem present node ->
                Hashtbl.replace attrs node
                  ((if clear then clear_mode m else m), uid, gid)
            | _ -> ())
        | Suid_clear { node } -> (
            match Hashtbl.find_opt attrs node with
            | Some (m, u, g) when Hashtbl.mem present node ->
                Hashtbl.replace attrs node (clear_mode m, u, g)
            | _ -> ()))
    (entries t);
  { present; maps; attrs }

(* Visible entries: (path, node) reachable from the root. *)
let visible t img =
  let rec go prefix d acc =
    List.fold_left
      (fun acc (name, c) ->
        let p = if prefix = "" then name else prefix ^ "/" ^ name in
        let acc = (p, c) :: acc in
        if is_dir t c then go p c acc else acc)
      acc
      (Option.value ~default:[] (Hashtbl.find_opt img.maps d))
  in
  List.rev (go "" 0 [ ("", 0) ])

(* {2 Data} *)

type entry_desc =
  | D of int * int * int
  | F of int * int * int * string
  | L of string

let pp_desc ppf = function
  | D (m, u, g) -> Fmt.pf ppf "dir %o %d:%d" m u g
  | F (m, u, g, c) -> Fmt.pf ppf "file %o %d:%d %S" m u g c
  | L tg -> Fmt.pf ppf "link -> %s" tg

type image = (string * entry_desc) list

let pp_image ppf (i : image) =
  Fmt.pf ppf "@[<v>%a@]" Fmt.(list (pair ~sep:(any ": ") string pp_desc)) i

(* Pending (un-synced, not lost) data changes in program order. *)
let pending_data t =
  Hashtbl.fold
    (fun id f acc -> List.map (fun (s, c, _, _) -> (s, id, c)) f.pend @ acc)
    t.fdata []
  |> List.sort compare

let content_survive t id subset =
  let f = fd t id in
  merged ~bs:t.bs ~durable:f.durable ~base_cache:f.base_cache ~cached:f.cached
    (List.filter_map
       (fun (s, ch, _, _) -> if IS.mem s subset then Some (s, ch) else None)
       f.pend)

let image_with t s content : image =
  let img = meta_image t s in
  visible t img
  |> List.map (fun (p, id) ->
      let m, u, g = Hashtbl.find img.attrs id in
      match (node t id).k with
      | Dir _ -> (p, D (m, u, g))
      | Link tg -> (p, L tg)
      | File _ -> (p, F (m, u, g, content id)))
  |> List.sort compare

(* Exact state for a Survive choice: [k] uncommitted entries kept, data
   changes in [subset] kept. *)
let survive_state t ~k ~subset =
  image_with t (survivors t k) (fun id -> content_survive t id subset)

(* {2 Reorder_unsynced at block size [bs]} *)

let block_bytes ~bs c i =
  String.init bs (fun j ->
      let x = (i * bs) + j in
      if x < String.length c then c.[x] else '\000')

(* Length candidates and per-block version candidates: as the page cache
   kept what a failed sync lost, or, [evicted], as it reloaded those pages
   from disk. *)
let reorder_cands ?(evicted = false) t ~bs id =
  let f = fd t id in
  let durable = if evicted then f.evicted else f.durable in
  let lens = ref [ String.length durable ] in
  let blocks : (int, string list) Hashtbl.t = Hashtbl.create 8 in
  let add_block i v =
    let cur =
      Option.value
        ~default:[ block_bytes ~bs durable i ]
        (Hashtbl.find_opt blocks i)
    in
    if not (List.mem v cur) then Hashtbl.replace blocks i (cur @ [ v ])
  in
  let prev = ref durable
  and cprev = ref (if evicted then durable else f.base_cache) in
  List.iter
    (fun (seq, ch, _, cache) ->
      let after = apply_change !prev ch in
      let cache = if evicted then after else cache in
      let ol = String.length !prev and nl = String.length after in
      lens := nl :: !lens;
      (match ch with
      | W _ when nl > ol ->
          let b = ref (((ol / bs) + 1) * bs) in
          while !b < nl do
            lens := !b :: !lens;
            b := !b + bs
          done
      | _ -> ());
      (* Dirt is counted in the page cache. *)
      let c = (not evicted) && List.mem seq f.cached in
      List.iter
        (fun i -> add_block i (block_bytes ~bs cache i))
        (blocks_dirtied ~bs (if c then !prev else !cprev) ch);
      prev := after;
      cprev := cache)
    f.pend;
  ( List.sort_uniq compare !lens,
    fun i ->
      Option.value
        ~default:[ block_bytes ~bs durable i ]
        (Hashtbl.find_opt blocks i) )

let reorder_member_in ~evicted t ~bs id s =
  let lens, cands = reorder_cands ~evicted t ~bs id in
  let len = String.length s in
  List.mem len lens
  &&
  let nb = (len + bs - 1) / bs in
  let ok = ref true in
  for i = 0 to nb - 1 do
    let lo = i * bs in
    let sl = String.sub s lo (min bs (len - lo)) in
    if
      not
        (List.exists
           (fun v -> String.sub v 0 (String.length sl) = sl)
           (cands i))
    then ok := false
  done;
  !ok

let reorder_member t ~bs id s =
  reorder_member_in ~evicted:false t ~bs id s
  || reorder_member_in ~evicted:true t ~bs id s

(* Survive membership: some subset of pending changes yields [s]. *)
let survive_member t id s =
  let f = fd t id in
  let rec go kept = function
    | [] ->
        merged ~bs:t.bs ~durable:f.durable ~base_cache:f.base_cache
          ~cached:f.cached (List.rev kept)
        = s
    | (seq, ch, _, _) :: rest -> go kept rest || go ((seq, ch) :: kept) rest
  in
  go [] f.pend

let n_uncommitted t = List.length (uncommitted t)

(* Every metadata image over k, with its inode per path. *)
let meta_choices t =
  let n = n_uncommitted t in
  List.init (n + 1) (fun k ->
      let s = survivors t k in
      let img = meta_image t s in
      (k, s, img, visible t img))

let skeleton t img vis =
  List.map
    (fun (p, id) ->
      let m, u, g = Hashtbl.find img.attrs id in
      match (node t id).k with
      | Dir _ -> (p, `D (m, u, g))
      | Link tg -> (p, `L tg)
      | File _ -> (p, `F (m, u, g)))
    vis
  |> List.sort compare

let obs_skeleton (i : image) =
  List.map
    (function
      | p, D (m, u, g) -> (p, `D (m, u, g))
      | p, L tg -> (p, `L tg)
      | p, F (m, u, g, _) -> (p, `F (m, u, g)))
    i

(* [member t ~data obs]: some k and data choice give [obs]. [data] is the
   per-inode content membership test. *)
let member t ~data (obs : image) =
  let sk = obs_skeleton obs in
  List.exists
    (fun (_, _, img, vis) ->
      skeleton t img vis = sk
      && List.for_all
           (fun (p, d) ->
             match d with
             | F (_, _, _, c) -> data (List.assoc p vis) c
             | _ -> true)
           obs)
    (meta_choices t)

(* Which k values match the skeleton. *)
let matching_ks t (obs : image) =
  let sk = obs_skeleton obs in
  List.filter_map
    (fun (k, _, img, vis) -> if skeleton t img vis = sk then Some k else None)
    (meta_choices t)
