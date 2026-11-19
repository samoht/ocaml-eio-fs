open! Hermetic
open Eio.Std
open Blocks

type boxed = Wrapper.base_dir =
  | Base_dir : (module Eio.Fs.Pi.DIR with type t = 'a) * 'a -> boxed

let raise_power_lost () = Wrapper.raise_io `Eio "power lost"

let err_not_wrapped () =
  invalid_arg
    "Eio_fs_chaos.Power.cut: t wraps no directory, so it has nothing to cut"

let err_base_not_dir () =
  invalid_arg
    "Eio_fs_chaos.Power.wrap: base is not a directory, so a cut has no tree to \
     rebuild"

(* An independent copy of the first [n] bytes offered by [bufs]. The caller's
   buffers may be reused after the write returns, so the log must not alias
   them. *)
let capture bufs n =
  let b = Cstruct.create n in
  let (_ : int * Cstruct.t list) = Cstruct.fillv ~src:bufs ~dst:b in
  b

(* The bytes of [ro] up to its end as the reads find it. Another process may
   rewrite the file while it is read, so the size [stat] answers sizes the
   first buffer only: what is read is one of the images the rewrite passes
   through, whole or torn. *)
let read_all (ro : Eio.File.ro_ty r) =
  let size = Optint.Int63.to_int (Eio.File.stat ro).size in
  let pread off bufs =
    Eio.File.pread ro ~file_offset:(Optint.Int63.of_int off) bufs
  in
  (* A byte past the buffer tells a file that grew from one that did not,
     without a larger buffer for the common case. *)
  let probe = Cstruct.create 1 in
  let rec loop buf off =
    if off < Cstruct.length buf then
      match pread off [ Cstruct.sub buf off (Cstruct.length buf - off) ] with
      | got -> loop buf (off + got)
      | exception End_of_file -> Cstruct.sub buf 0 off
    else
      match pread off [ probe ] with
      | exception End_of_file -> buf
      | _ ->
          let bigger = Cstruct.create (max 4096 (2 * off)) in
          Cstruct.blit buf 0 bigger 0 off;
          Cstruct.blit probe 0 bigger off 1;
          loop bigger (off + 1)
  in
  loop (if size <= 0 then Cstruct.empty else Cstruct.create size) 0

(* The model observes the disk as a disk, not as a user: a file whose mode
   denies reading (a write-only spool) is still bytes on the platter. Grant
   owner read for the duration of the model's own reads and restore the
   exact mode after. *)
let with_readable (Base_dir ((module B), bv)) path fn =
  let restore =
    match B.stat bv ~follow:false path with
    | exception Eio.Io _ -> None
    | { kind = `Regular_file; perm; _ } when perm land 0o400 = 0 ->
        B.chmod bv ~follow:true ~perm:(perm lor 0o400) path;
        Some perm
    | _ -> None
  in
  Fun.protect
    ~finally:(fun () ->
      match restore with
      | Some perm -> (
          try B.chmod bv ~follow:true ~perm path
          with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ())
      | None -> ())
    fn

(* The base content before a truncating open, used to seed a file's durable
   baseline. A missing path, or one that is not a regular file, seeds an empty
   baseline. *)
let read_existing (Base_dir ((module B), bv) as owner) path =
  match B.stat bv ~follow:true path with
  | exception Eio.Io _ -> Cstruct.empty
  | { kind = `Regular_file; _ } -> (
      try
        with_readable owner path (fun () ->
            Switch.run (fun sw -> read_all (B.open_in bv ~sw ~follow:true path)))
      with Eio.Io _ -> Cstruct.empty)
  | _ -> Cstruct.empty

(* [durable] is the content as of the last [sync]; the pending changes live in
   the controller's log. [owner] and [rel] locate the file under the wrapped
   root so a crash can rewrite it after intermediate subtrees close; both move
   on rename. *)
type fstate = {
  id : int;
  mutable owner : boxed;
  mutable rel : string;
  mutable perm : int;
  durable : image;
  mutable pending : entry list; (* this file's pending changes, newest first *)
  mutable gen : int;
      (* Bumped when the pending changes go, durable or lost: an entry of an
         older generation in the controller's log is stale. *)
  mutable cache : Cstruct.t option;
      (* The page cache's bytes as of the last sync, where a failed sync
         left them apart from the durable ones: it still holds what that
         sync lost. *)
  mutable mode : int;
      (* The file's mode as last seen, to tell when a write clears setuid
         or setgid. *)
  mutable evicted : Cstruct.t option;
      (* The durable bytes had the pages a failed sync lost been evicted
         before a later write to them, where they differ. *)
  mutable partial : (int * int * Cstruct.t) list;
      (* The blocks failed syncs lost, with the seq each failed at and as
         they stood then: a writeback that failed only partly may have
         written any of them, the one of any failure. *)
  mutable held : Extents.t;
      (* The byte ranges durable writes covered: the disk holds the pages
         they touch, and no other. *)
  mutable freed : (int * Cstruct.t) list;
      (* The pages durable shrinks freed, by index, newest first, as the
         disk held them. *)
  mutable synced : Eio.File.Stat.t option; (* its stat at its last sync *)
  mutable flushed : int;
      (* On macOS, the last change a plain fsync flushed to the drive's
         cache: no later failure loses it. *)
}

and entry = {
  efs : fstate;
  epath : string; (* the file's path when the change was made *)
  change : change;
  seq : int;
  egen : int;
  cached : bool;
      (* A change a failed sync kept pending, which the page cache already
         holds: a truncation, or on macOS a change an fsync flushed before.
         It dirties no block. *)
}

let next_id = Atomic.make 0

let fresh_file ?held ~owner ~rel ~perm ~durable () =
  {
    id = Atomic.fetch_and_add next_id 1;
    owner;
    rel;
    perm;
    durable = image_of durable;
    pending = [];
    gen = 0;
    cache = None;
    mode = 0;
    evicted = None;
    partial = [];
    held =
      (let n = Cstruct.length durable in
       match held with
       | Some h -> Extents.cut n h
       | None -> Extents.(add 0 n empty));
    freed = [];
    synced = None;
    flushed = 0;
  }

(* Maps keyed by a path, where the keys at or under one path are a range. *)
module Paths = Map.Make (String)
module Ids = Map.Make (Int)

(* The bindings of [m] at or under the path [k]. *)
let under k m =
  let pre = if String.equal k "" || String.equal k "/" then k else k ^ "/" in
  let exact =
    match Paths.find_opt k m with
    | Some v when not (String.equal pre k) -> [ (k, v) ]
    | Some _ | None -> []
  in
  let rec go acc seq =
    match seq () with
    | Seq.Cons ((k', v), rest) when String.starts_with ~prefix:pre k' ->
        go ((k', v) :: acc) rest
    | Seq.Cons _ | Seq.Nil -> acc
  in
  go exact (Paths.to_seq_from pre m)

let is_pending e = e.egen = e.efs.gen

(* What kind of unsynced change a journal entry records, as seen by a
   scripted survival predicate. *)
type op_kind =
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

type unsynced = {
  path : string;
  kind : op_kind;
  file_offset : int;
  length : int;
  seq : int;
}

(* An inode's times, as its version a journal commits holds them. *)
type times = { atime : float; mtime : float; ctime : float }

let times_of (st : Eio.File.Stat.t) =
  { atime = st.atime; mtime = st.mtime; ctime = st.ctime }

(* A directory tree as an image holds it: the durable image, and the
   subtrees a directory operation touched. *)
type tree =
  | File of {
      data : Cstruct.t;
      perm : int;
      uid : int64;
      gid : int64;
      ino : int64;
      times : times;
    }
  | Dir of {
      perm : int;
      uid : int64;
      gid : int64;
      ino : int64;
      times : times;
      entries : tree Paths.t;
    }
  | Link of {
      target : string;
      uid : int64;
      gid : int64;
      ino : int64;
      times : times;
    }

let ino_of = function File f -> f.ino | Dir d -> d.ino | Link l -> l.ino

let times_of_node = function
  | File f -> f.times
  | Dir d -> d.times
  | Link l -> l.times

module Inos = Map.Make (Int64)

type location = fstate * string

(* Where each tracked file sits in an image, by its path in the image, and
   that path by the file: each file sits at one place at most. *)
type locs = { at : location list Paths.t; where : string Ids.t }

let no_locs = { at = Paths.empty; where = Ids.empty }

let unlocate (fs : fstate) t =
  match Ids.find_opt fs.id t.where with
  | None -> t
  | Some k ->
      let at =
        Paths.update k
          (function
            | None -> None
            | Some ls -> (
                match List.filter (fun (f, _) -> f != fs) ls with
                | [] -> None
                | ls -> Some ls))
          t.at
      in
      { at; where = Ids.remove fs.id t.where }

let locate_at k ((fs, _) as l) t =
  let t = unlocate fs t in
  {
    at = Paths.update k (fun o -> Some (l :: Option.value ~default:[] o)) t.at;
    where = Ids.add fs.id k t.where;
  }

let unlocate_under k t =
  List.fold_left
    (fun t (_, ls) -> List.fold_left (fun t (fs, _) -> unlocate fs t) t ls)
    t (under k t.at)

(* An image of the tree: the tree, where each tracked file sits in it, and
   where each inode sits. *)
type state = { tree : tree; locs : locs; inos : string list Inos.t }

(* What a directory operation did, by inode: a node put in the directory
   [parent] under the path's last name, a node taken out wherever it is,
   or new attributes on a node wherever it is. The path is where it was
   when the operation ran. *)
type edit =
  | Place of { parent : int64; node : tree }
  | Move of { parent : int64; ino : int64 }
  | Remove of int64
  | Attrs of {
      ino : int64;
      perm : int;
      uid : int64;
      gid : int64;
      times : times;
    }
  | Times of { ino : int64; times : times }

type delta = (string list * edit) list

(* A journal entry keeps only what its operation changed. The durable
   image is the last whole snapshot with the committed entries replayed
   over it in program order, and a crash replays the committed and the
   surviving ones the same way. [recorded] is where the files under the
   paths [delta] touched sat when the entry was recorded. *)
type op = {
  meta : unsynced;
  delta : delta;
  paths : string list list; (* the paths it names, below the root *)
  mutable recorded : location list;
}

type metadata_entry = op Journal.entry

(* Documented in chaos.mli. *)
type policy =
  | Drop_unsynced
  | Reorder_unsynced of int
  | Survive of (unsynced -> bool)
  | Choose of (int -> int)

(* Documented in chaos.mli. *)
type outside = Dropped | Kept

type base_dir = Eio.Fs.dir_ty Eio.Path.t
(** The backing store handed back after a crash. *)

(* Where an open subtree sits: the boxed directory the controller was wrapped
   over and the subtree's path from it. A rename through the wrapper moves
   it with its directory, as it moves a tracked file. *)
type place = { mutable proot : boxed; mutable prefix : string }

(* What a sync makes durable, as of the change [upto] in program order: a
   file's data and attributes ([stat] is the file as the sync found it), the
   attributes alone of a file the model does not track, or the entries of a
   directory. *)
module Commit = struct
  type t =
    | File of { fs : fstate; upto : int; stat : Eio.File.Stat.t }
    | Attrs of { ino : int64; upto : int }
    | Dir of { ino : int64; upto : int }
end

type t = {
  policy : policy;
  outside : outside;
  block_size : int;
  mutable base : base_dir option;
  files : (string, fstate) Hashtbl.t;
  known : (int, fstate) Hashtbl.t; (* every file tracked, by id *)
  mutable subtrees : place list;
  mutable log : entry list;
      (* Every file's changes, newest first, stale ones included until the
         log is compacted. *)
  mutable logged : int; (* the log's length *)
  mutable pending_changes : int; (* its entries still pending *)
  journal : op Journal.t;
      (* Every entry since the last whole snapshot, committed or not. *)
  mutable snapshot : state option; (* the last whole snapshot *)
  mutable snapped : (string * Cstruct.t) Inos.t;
      (* The path and bytes of every file a whole snapshot held, by inode:
         once removed, its blocks are free, even after the snapshot moved
         on. *)
  mutable left : string list;
      (* What the crash left of the unsynced writes, file by file. *)
  mutable durable : state option;
      (* The snapshot with the committed entries replayed over it, or
         [None] until it is built again. *)
  mutable root_box : boxed option; (* the base given to [wrap] *)
  mutable admin : boxed option;
      (* The base with root's rights, which the model reads and rebuilds
         the tree through: the code under test may run as a user who cannot
         read all of it. *)
  mutable by_rel : fstate list Paths.t; (* the files of [files] by path *)
  mutable held : string -> Extents.t option;
      (* The byte ranges writes covered in a file of an in-memory base, by
         path: the rest of it is a hole the disk does not hold. *)
  mutable set_times : string -> times -> unit;
      (* Sets the times of an entry of an in-memory base, by path. *)
  mutable seq : int;
  mutable effects : int;
      (* The commits, barriers and failures of syncs, and the files and
         modes opens met, so far: with [seq], what tells a sync or an open
         that changed nothing a cut leaves. *)
  opened : (int64, int) Hashtbl.t;
      (* The handles open on each inode: ext4 frees a removed file's blocks
         at its last close. *)
  file_inos : (int, int64) Hashtbl.t; (* the inode of each file, by id *)
  os : [ `Linux | `Macos | `Windows ];
  mutable queued : Commit.t list;
      (* The commits of the plain syncs a full one has not drained yet,
         newest first: on macOS, fsync(2) stops in the drive's cache. *)
  pause : unit -> unit;
      (* The time a sync takes, after it took what it covers. *)
  mutable crashed : bool;
  mutable image :
    (string * int64 * [ `File of string | `Dir | `Link of string ]) list;
      (* The image the crash rebuilt, by path below the root, with the
         inode each entry had live, for this package's tests. *)
}

(* [owner] with root's rights when it is the wrapped base. *)
let as_admin ctl owner =
  match (ctl.root_box, ctl.admin) with
  | Some proot, Some admin when proot == owner -> admin
  | _ -> owner

let push ctl fs e =
  ctl.log <- e :: ctl.log;
  fs.pending <- e :: fs.pending;
  ctl.logged <- ctl.logged + 1;
  ctl.pending_changes <- ctl.pending_changes + 1

let clog_change ctl fs change =
  ctl.seq <- ctl.seq + 1;
  push ctl fs
    {
      efs = fs;
      epath = fs.rel;
      change;
      seq = ctl.seq;
      egen = fs.gen;
      cached = false;
    }

(* An empty write changes nothing: Linux returns before the file system
   sees it. *)
let clog_write ctl fs ~offset data =
  if Cstruct.length data > 0 then clog_change ctl fs (Write { offset; data })

let unsynced_of_entry e =
  match e.change with
  | Write { offset; data } ->
      {
        path = e.epath;
        kind = Data;
        file_offset = offset;
        length = Cstruct.length data;
        seq = e.seq;
      }
  | Resize n ->
      {
        path = e.epath;
        kind = Truncate;
        file_offset = n;
        length = 0;
        seq = e.seq;
      }

let unsynced ctl =
  let data =
    List.filter_map
      (fun e -> if is_pending e then Some (unsynced_of_entry e) else None)
      ctl.log
  in
  let metadata =
    Journal.fold
      (fun (e : metadata_entry) acc ->
        if e.committed then acc else e.op.meta :: acc)
      ctl.journal []
  in
  List.sort
    (fun (a : unsynced) (b : unsynced) -> Int.compare a.seq b.seq)
    (data @ metadata)

(* The bytes of the regular file [path], read with the owner's read bit. *)
let current_data (Base_dir ((module B), bv) as owner) path =
  with_readable owner path (fun () ->
      Switch.run (fun sw -> read_all (B.open_in bv ~sw ~follow:false path)))

(* Rewrite the base file to [content], unless it holds it already: truncate
   and re-lay the bytes. A file staged read-only or write-only cannot be
   reopened O_RDWR, so grant the owner read and write bits for the duration
   of the rewrite and restore the exact mode after. *)
let rewrite ctl fs content =
  let (Base_dir ((module B), bv) as owner) = as_admin ctl fs.owner in
  let holds =
    match B.stat bv ~follow:false fs.rel with
    | { kind = `Regular_file; _ } ->
        Cstruct.equal (current_data owner fs.rel) content
    | _ | (exception Eio.Io _) -> false
  in
  if not holds then begin
    let restore =
      match B.stat bv ~follow:true fs.rel with
      | exception Eio.Io _ -> None
      | { kind = `Regular_file; perm; _ } when perm land 0o600 <> 0o600 ->
          B.chmod bv ~follow:true ~perm:(perm lor 0o600) fs.rel;
          Some perm
      | _ -> None
    in
    Fun.protect ~finally:(fun () ->
        match restore with
        | Some perm -> B.chmod bv ~follow:true ~perm fs.rel
        | None -> ())
    @@ fun () ->
    Switch.run @@ fun sw ->
    let f =
      B.open_out bv ~sw ~follow:true ~append:false
        ~create:(`Or_truncate fs.perm) fs.rel
    in
    if Cstruct.length content > 0 then
      Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [ content ]
  end

let box_dir (Eio.Resource.T (v, ops)) =
  Base_dir (Eio.Resource.get ops Eio.Fs.Pi.Dir, v)

let is_gone = function
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true
  | _ -> false

(* The tree at [path] as the reads find it. Other processes may write the
   tree while it is read: an entry one of them removes between the listing
   and its own read is not in the snapshot, as it is not on the disk. *)
let rec snapshot_at (Base_dir ((module B), bv) as owner) path =
  match B.stat bv ~follow:false path with
  | { kind = `Regular_file; perm; uid; gid; ino; _ } as st ->
      let data =
        with_readable owner path (fun () ->
            Switch.run (fun sw -> read_all (B.open_in bv ~sw ~follow:true path)))
        |> copy_cs
      in
      File { data; perm; uid; gid; ino; times = times_of st }
  | { kind = `Directory; perm; uid; gid; ino; _ } as st ->
      let entries =
        B.read_dir bv path
        |> List.fold_left
             (fun m name ->
               let child = Eio_utils.Posix_path.join path name in
               match snapshot_at owner child with
               | tree -> Paths.add name tree m
               | exception e when is_gone e -> m)
             Paths.empty
      in
      Dir { perm; uid; gid; ino; times = times_of st; entries }
  | { kind = `Symbolic_link; uid; gid; ino; _ } as st ->
      Link { target = B.read_link bv path; uid; gid; ino; times = times_of st }
  | _ ->
      Fmt.invalid_arg
        "Eio_fs_chaos.Power.wrap: base's entry %S is neither a regular file, a \
         directory nor a symbolic link, so a cut cannot rebuild it"
        path

(* The entry at [path] given back to its owner, unless it has them: a host
   process that is not root may give a file only to itself. *)
let restore_owner (Base_dir ((module B), bv)) path ~uid ~gid =
  let s = B.stat bv ~follow:false path in
  if not (Int64.equal s.uid uid && Int64.equal s.gid gid) then
    B.chown ~follow:false ~uid ~gid bv path

(* [path]'s mode made [perm], unless it is: a chmod moves the change
   time even to the same mode. *)
let restore_perm (Base_dir ((module B), bv)) path perm =
  if not (Int.equal (B.stat bv ~follow:false path).perm perm) then
    B.chmod bv ~follow:false ~perm path

let write_file (Base_dir ((module B), bv)) path ~perm data =
  Switch.run @@ fun sw ->
  let f =
    B.open_out bv ~sw ~follow:false ~append:false ~create:(`Or_truncate perm)
      path
  in
  if Cstruct.length data > 0 then
    Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [ data ]

(* The base at [path] made [tree], in place: an entry that is still the one
   the image names (its kind and inode, and a file no other name links) is
   kept, its inode and times with it, as a power cut spares them on a disk,
   and only what differs is rewritten. Any other entry is replaced. *)
let rec restore_at ~stamp owner path tree =
  restore_node ~stamp owner path tree;
  stamp path (times_of_node tree)

and restore_node ~stamp (Base_dir ((module B), bv) as owner) path tree =
  let now =
    match B.stat bv ~follow:false path with
    | st -> Some st
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  in
  match (tree, now) with
  | File { data; perm; uid; gid; ino; _ }, Some st
    when st.kind = `Regular_file && Int64.equal st.ino ino
         && Int64.equal st.nlink 1L ->
      if not (Cstruct.equal (current_data owner path) data) then begin
        if st.perm land 0o600 <> 0o600 then
          B.chmod bv ~follow:false ~perm:(st.perm lor 0o600) path;
        write_file owner path ~perm data
      end;
      (* The owner first: a chown clears setuid and setgid. *)
      restore_owner owner path ~uid ~gid;
      restore_perm owner path perm
  | File { data; perm; uid; gid; _ }, _ ->
      if Option.is_some now then Wrapper.remove_at owner path;
      write_file owner path ~perm data;
      restore_owner owner path ~uid ~gid;
      B.chmod bv ~follow:false ~perm path
  | Link { target; uid; gid; _ }, Some st
    when st.kind = `Symbolic_link && String.equal (B.read_link bv path) target
    ->
      restore_owner owner path ~uid ~gid
  | Link { target; uid; gid; _ }, _ ->
      if Option.is_some now then Wrapper.remove_at owner path;
      B.symlink ~link_to:target bv path;
      restore_owner owner path ~uid ~gid
  | Dir d, Some st when st.kind = `Directory && Int64.equal st.ino d.ino ->
      refill ~stamp owner path st d.entries;
      restore_owner owner path ~uid:d.uid ~gid:d.gid;
      restore_perm owner path d.perm
  | Dir { perm; uid; gid; entries; _ }, _ ->
      if Option.is_some now then Wrapper.remove_at owner path;
      (* Writable while it fills; the mode, which the umask also narrows,
         is set after. *)
      B.mkdir bv ~perm:(perm lor 0o700) path;
      Paths.iter
        (fun name child ->
          restore_at ~stamp owner (Eio_utils.Posix_path.join path name) child)
        entries;
      restore_owner owner path ~uid ~gid;
      B.chmod bv ~follow:false ~perm path

(* The directory [path], whose stat is [st], made to hold [entries]: what it
   holds that they do not name goes, each of them is restored in place. *)
and refill ~stamp (Base_dir ((module B), bv) as owner) path
    (st : Eio.File.Stat.t) entries =
  if st.perm land 0o700 <> 0o700 then
    B.chmod bv ~follow:false ~perm:(st.perm lor 0o700) path;
  B.read_dir bv path
  |> List.iter (fun name ->
      if not (Paths.mem name entries) then
        Wrapper.remove_at owner (Eio_utils.Posix_path.join path name));
  Paths.iter
    (fun name child ->
      restore_at ~stamp owner (Eio_utils.Posix_path.join path name) child)
    entries

let restore_root ~stamp (Base_dir ((module B), bv) as owner) path = function
  | Dir { perm; uid; gid; entries; times; _ } ->
      let st = B.stat bv ~follow:true path in
      refill ~stamp owner path st entries;
      if String.equal path "" then restore_perm owner path st.perm
      else begin
        restore_owner owner path ~uid ~gid;
        B.chmod bv ~follow:true ~perm path
      end;
      stamp path times
  | File _ | Link _ -> err_base_not_dir ()

let locations ctl =
  Hashtbl.fold (fun _ fs acc -> (fs, fs.rel) :: acc) ctl.files []

(* The tracked files at or under [rel]. *)
let files_under ctl rel = List.concat_map snd (under rel ctl.by_rel)

let root ctl =
  match (ctl.admin, ctl.base) with
  | Some admin, Some (_, path) -> (admin, path)
  | _ -> err_not_wrapped ()

(* A journal path as the model records it: root-relative, '/'-separated, ""
   for the root itself. A barrier names its directory in the caller's
   spelling ("." for a root-level file, a trailing slash after a subtree
   prefix), so every path reaching the journal is reduced to one key. *)
let path_segments path =
  String.split_on_char '/' path
  |> List.filter (fun seg ->
      (not (String.equal seg "")) && not (String.equal seg "."))

let path_key path = String.concat "/" (path_segments path)

(* The journal names an entry as the base directory resource sees it: the path
   {!wrap} was handed, with the entry below it. The durable image is a
   snapshot rooted at that same path, so indexing the image means dropping the
   root's own segments first. A path that is not under the root, such as a
   rename out of the wrapped tree, has no place in the image. *)
let tree_path ~root path =
  let rec strip root segs =
    match (root, segs) with
    | [], segs -> Some segs
    | r :: root, s :: segs when String.equal r s -> strip root segs
    | _ -> None
  in
  strip root (path_segments path)

(* [resolve ~root owner path] is [path] with every symbolic link below
   [root] replaced by its target, the last one too unless [follow_last] is
   [false], and its [.], [..] and empty components gone, as
   path_resolution(7) walks it: one spelling per file. A component that does
   not resolve is kept, and so is a path outside [root]. *)
let resolve_at ?(follow_last = true) ?(spell = fun ~dir:_ name -> name) ~root
    (Base_dir ((module B), bv)) path =
  let render abs rev =
    let p = String.concat "/" (List.rev rev) in
    if abs then "/" ^ p else p
  in
  let link_at p =
    match B.stat bv ~follow:false p with
    | { kind = `Symbolic_link; _ } -> (
        try Some (B.read_link bv p) with Eio.Io _ -> None)
    | _ -> None
    | exception Eio.Io _ -> None
  in
  let rec go hops abs rev = function
    | [] -> render abs rev
    | ("" | ".") :: rest -> go hops abs rev rest
    | ".." :: rest -> (
        match rev with
        | name :: up when not (String.equal name "..") -> go hops abs up rest
        | _ when abs -> go hops abs rev rest
        | _ -> go hops abs (".." :: rev) rest)
    | name :: rest -> (
        let name = spell ~dir:(render abs rev) name in
        let p = render abs (name :: rev) in
        let target =
          if hops <= 0 || (rest = [] && not follow_last) then None
          else link_at p
        in
        match target with
        | None -> go hops abs (name :: rev) rest
        | Some target ->
            let segs = String.split_on_char '/' target in
            if String.starts_with ~prefix:"/" target then
              go (hops - 1) true [] (segs @ rest)
            else go (hops - 1) abs rev (segs @ rest))
  in
  let root = path_segments root in
  match tree_path ~root path with
  | None -> path
  | Some rest ->
      go 40 (String.starts_with ~prefix:"/" path) (List.rev root) rest

let rec tree_find tree segs =
  match (tree, segs) with
  | _, [] -> Some tree
  | (File _ | Link _), _ -> None
  | Dir { entries; _ }, name :: rest -> (
      match Paths.find_opt name entries with
      | Some child -> tree_find child rest
      | None -> None)

(* [tree_replace tree segs node] is [tree] with the entry at [segs] set to
   [node], or freed when [node] is [None]. A path whose parent the tree does
   not hold, or holds as something other than a directory, leaves the tree
   untouched: the entry went with the directory that was to hold it. *)
let rec tree_replace tree segs node =
  match (tree, segs) with
  | _, [] -> tree
  | (File _ | Link _), _ -> tree
  | Dir d, [ name ] ->
      let entries =
        match node with
        | None -> Paths.remove name d.entries
        | Some child -> Paths.add name child d.entries
      in
      Dir { d with entries }
  | Dir d, name :: rest -> (
      match Paths.find_opt name d.entries with
      | None -> tree
      | Some child ->
          let child = tree_replace child rest node in
          Dir { d with entries = Paths.add name child d.entries })

let rec is_prefix prefix segs =
  match (prefix, segs) with
  | [], _ -> true
  | p :: prefix, s :: segs when String.equal p s -> is_prefix prefix segs
  | _ -> false

(* [f] on each node of [tree], whose path is [segs], and on its path. *)
let rec fold_nodes f segs tree acc =
  let acc = f segs tree acc in
  match tree with
  | Dir d ->
      Paths.fold
        (fun name child acc -> fold_nodes f (segs @ [ name ]) child acc)
        d.entries acc
  | File _ | Link _ -> acc

let index segs tree inos =
  fold_nodes (fun s n m -> Inos.add (ino_of n) s m) segs tree inos

let unindex tree inos =
  fold_nodes (fun _ n m -> Inos.remove (ino_of n) m) [] tree inos

let state_of tree locs = { tree; locs; inos = index [] tree Inos.empty }

(* [st] with [node] at [segs], whose parent it holds, in place of what was
   there. *)
let set_node st segs node =
  let inos =
    match tree_find st.tree segs with
    | Some old -> unindex old st.inos
    | None -> st.inos
  in
  let tree = tree_replace st.tree segs node in
  let inos = match node with Some n -> index segs n inos | None -> inos in
  { st with tree; inos }

let remove_path st segs =
  let st = { st with locs = unlocate_under (String.concat "/" segs) st.locs } in
  set_node st segs None

let with_attrs ~perm ~uid ~gid ~times = function
  | File f -> File { f with perm; uid; gid; times }
  | Dir d -> Dir { d with perm; uid; gid; times }
  | Link l -> Link { l with uid; gid; times }

let with_times times = function
  | File f -> File { f with times }
  | Dir d -> Dir { d with times }
  | Link l -> Link { l with times }

(* Where the directory [parent], which held the entry at [segs] when the
   operation ran, is in [st]: at that path if it is still there, or
   wherever that inode is now. *)
let target_dir st segs parent =
  let dir_at at =
    match tree_find st.tree at with
    | Some (Dir d) -> Int64.equal d.ino parent
    | Some (File _ | Link _) | None -> false
  in
  match List.rev segs with
  | [] -> None
  | name :: up -> (
      let up = List.rev up in
      if dir_at up then Some (up @ [ name ])
      else
        match Inos.find_opt parent st.inos with
        | Some at when dir_at at -> Some (at @ [ name ])
        | Some _ | None -> None)

(* Whether every half of [e] lands in [st]: an entry takes effect whole or
   not at all, so a node put in a directory the image does not hold is not
   made, and an rmdir of a directory that still holds entries there removes
   nothing. A rename always lands. *)
let applies st (e : metadata_entry) =
  List.for_all
    (fun (segs, edit) ->
      match edit with
      | Place { parent; _ } -> Option.is_some (target_dir st segs parent)
      | Move _ -> true
      | Remove ino -> (
          match Inos.find_opt ino st.inos with
          | Some at -> (
              match tree_find st.tree at with
              | Some (Dir d) -> Paths.is_empty d.entries
              | Some (File _ | Link _) | None -> true)
          | None -> true)
      | Attrs _ | Times _ -> true)
    e.op.delta

(* A node a rename took where the image has no directory for it stays in
   the image under the root, by a name no directory entry can have, so a
   later rename can bring it back with what it holds. A crash leaves them
   out. *)
let limbo ino = "\000" ^ Int64.to_string ino
let is_limbo name = String.length name > 0 && Char.equal name.[0] '\000'
let in_limbo = function name :: _ -> is_limbo name | [] -> false

(* [st] with the node at [from], and what it holds, at [at] instead. *)
let move_node st ~from ~at =
  match tree_find st.tree from with
  | None -> st
  | Some node ->
      let moved = under (String.concat "/" from) st.locs.at in
      let st = remove_path st from in
      let st = remove_path st at in
      let st = set_node st at (Some node) in
      let n = List.length from in
      let locs =
        List.fold_left
          (fun locs (k, ls) ->
            let below = List.drop n (path_segments k) in
            let key = String.concat "/" (at @ below) in
            List.fold_left (fun locs l -> locate_at key l locs) locs ls)
          st.locs moved
      in
      { st with locs }

(* [st] with [node], made at [segs], at [at], and the files [recorded]
   below [segs] located below [at]. *)
let put ~root st ~segs ~at ~node ~recorded =
  let st = remove_path st at in
  let st = set_node st at (Some node) in
  let locs =
    List.fold_left
      (fun locs ((_, rel) as l) ->
        match tree_path ~root rel with
        | Some s when is_prefix segs s ->
            locate_at
              (String.concat "/" (at @ List.drop (List.length segs) s))
              l locs
        | Some _ | None -> locs)
      st.locs recorded
  in
  { st with locs }

(* [st] with [f] applied to the node [ino], wherever it is. *)
let on_node st ino f =
  match Inos.find_opt ino st.inos with
  | None -> st
  | Some at -> (
      match tree_find st.tree at with
      | None -> st
      | Some node -> (
          let node = f node in
          match at with
          | [] -> { st with tree = node }
          | _ -> { st with tree = tree_replace st.tree at (Some node) }))

(* Replay [e]'s delta onto an image, whole or not at all, each half finding
   its node by inode. A moved node the image lacks is put as [made] gives
   it; one moved into a directory the image lacks, or below itself, goes to
   limbo. *)
let replay ~root ~made st (e : metadata_entry) =
  if not (applies st e) then st
  else
    List.fold_left
      (fun st (segs, edit) ->
        match edit with
        | Remove ino -> (
            match Inos.find_opt ino st.inos with
            | Some (_ :: _ as at) -> remove_path st at
            | Some [] | None -> st)
        | Attrs { ino; perm; uid; gid; times } ->
            on_node st ino (with_attrs ~perm ~uid ~gid ~times)
        | Times { ino; times } -> on_node st ino (with_times times)
        | Move { parent; ino } -> (
            let at =
              match target_dir st segs parent with
              | Some at -> at
              | None -> [ limbo ino ]
            in
            match Inos.find_opt ino st.inos with
            | Some [] -> st
            | Some from when List.equal String.equal from at -> st
            | Some from ->
                let at = if is_prefix from at then [ limbo ino ] else at in
                move_node st ~from ~at
            | None -> (
                match made ino with
                | Some (segs, node, recorded) ->
                    put ~root st ~segs ~at ~node ~recorded
                | None -> st))
        | Place { parent; node } -> (
            match target_dir st segs parent with
            | None -> st
            | Some _ when Inos.mem (ino_of node) st.inos -> st
            | Some at -> put ~root st ~segs ~at ~node ~recorded:e.op.recorded))
      st e.op.delta

(* The path of [p]'s directory, spelt as [p] is. *)
let parent_path p =
  let segs = path_segments p in
  let up = match List.rev segs with [] -> [] | _ :: up -> List.rev up in
  let s = String.concat "/" up in
  if String.starts_with ~prefix:"/" p then "/" ^ s else s

let ino_at (Base_dir ((module B), bv)) p =
  match B.stat bv ~follow:false p with
  | st -> Some st.ino
  | exception Eio.Io _ -> None

(* Whether [loc] lies at or under a node [delta] makes. *)
let covers ~root delta (_, rel) =
  match tree_path ~root rel with
  | None -> false
  | Some s ->
      List.exists
        (fun (segs, edit) ->
          match edit with
          | Place _ -> is_prefix segs s
          | Move _ | Remove _ | Attrs _ | Times _ -> false)
        delta

(* [locs] in an image keyed below [root]. *)
let locs_of ~root locs =
  List.fold_left
    (fun t ((_, rel) as l) ->
      match tree_path ~root rel with
      | Some s -> locate_at (String.concat "/" s) l t
      | None -> t)
    no_locs locs

let metadata_unsynced ~path ~kind ~seq =
  { path; kind; file_offset = 0; length = 0; seq }

let root_segs ctl =
  match ctl.base with
  | Some (_, root_path) -> path_segments root_path
  | None -> []

let snap_files segs node acc =
  fold_nodes
    (fun segs n acc ->
      match n with
      | File f -> Inos.add f.ino ("/" ^ String.concat "/" segs, f.data) acc
      | _ -> acc)
    segs node acc

let snap ctl st =
  ctl.snapshot <- Some st;
  ctl.durable <- Some st;
  ctl.snapped <- snap_files [] st.tree ctl.snapped

let commit_metadata ctl =
  let owner, root_path = root ctl in
  let st =
    state_of
      (snapshot_at owner root_path)
      (locs_of ~root:(path_segments root_path) (locations ctl))
  in
  snap ctl st;
  Journal.reset ~upto:ctl.seq ctl.journal

(* The proper prefixes of [segs], outermost first: the directories above
   the entry it names. *)
let above segs =
  let rec go acc pre = function
    | [] | [ _ ] -> List.rev acc
    | s :: rest ->
        let pre = pre @ [ s ] in
        go (pre :: acc) pre rest
  in
  go [] [] segs

(* [touched ctl node segs] is whether the model holds a change at the path
   [segs] below the root, or for the [node] the store holds there: an entry of the journal ([~committed]
   counts the committed ones, which a crash replays over the snapshot too)
   names the path or a directory above it, or makes, moves or changes the
   attributes of the node, wherever a later rename took it; or a file there
   has unsynced writes, or writes a failed sync lost. *)
let touched ?(committed = false) ctl =
  let root = root_segs ctl in
  let paths = Hashtbl.create 16 and inos = Hashtbl.create 16 in
  Journal.fold
    (fun (e : metadata_entry) () ->
      if committed || not e.committed then begin
        List.iter (fun segs -> Hashtbl.replace paths segs ()) e.op.paths;
        List.iter (fun ino -> Hashtbl.replace inos ino ()) (e.nodes @ e.moved);
        Option.iter (fun ino -> Hashtbl.replace inos ino ()) e.attrs_ino
      end)
    ctl.journal ();
  Hashtbl.iter
    (fun _ (fs : fstate) ->
      if
        fs.pending <> [] || fs.partial <> [] || Option.is_some fs.cache
        || Option.is_some fs.evicted
      then
        Option.iter
          (fun segs -> Hashtbl.replace paths segs ())
          (tree_path ~root fs.rel))
    ctl.files;
  fun node segs ->
    Hashtbl.mem paths segs
    || List.exists (Hashtbl.mem paths) (above segs)
    || Option.fold ~none:false ~some:(fun n -> Hashtbl.mem inos (ino_of n)) node

(* The image under [Kept]: at a path the model holds a change for, what
   [model] holds there; at any other, what the store holds once the outside
   writers have stopped. *)
let rec kept_entry ~touched ~model segs store =
  if touched store segs then tree_find model segs
  else
    match store with
    | None | Some (File _ | Link _) -> store
    | Some (Dir _ as d) -> Some (kept_dir ~touched ~model segs d)

and kept_dir ~touched ~model segs = function
  | File _ | Link _ -> err_base_not_dir ()
  | Dir d ->
      let names m = Paths.fold (fun name _ acc -> name :: acc) m [] in
      let model_names =
        match tree_find model segs with
        | Some (Dir m) -> names m.entries
        | Some (File _ | Link _) | None -> []
      in
      let all = List.sort_uniq String.compare (names d.entries @ model_names) in
      let entries =
        List.fold_left
          (fun acc name ->
            match
              kept_entry ~touched ~model (segs @ [ name ])
                (Paths.find_opt name d.entries)
            with
            | Some tree -> Paths.add name tree acc
            | None -> acc)
          Paths.empty all
      in
      (* A directory the model changed has the times the model holds. *)
      let times =
        match tree_find model segs with
        | Some (Dir m)
          when List.exists (fun name -> touched None (segs @ [ name ])) all ->
            m.times
        | Some _ | None -> d.times
      in
      Dir { d with entries; times }

(* Under [Kept], the store at a path the model holds no change for is the
   outside writers', durable as they leave it. [adopt ctl paths] runs
   before the wrapper changes [paths]: the entry the store holds at each
   such path, with what the model holds below it where it holds a change,
   and each directory above it the snapshot lacks, enter the snapshot, so
   that the wrapper's own change starts from what the store
   held there, and a barrier on a directory an outside writer made commits
   what the wrapper put in it. *)
let adopt ctl paths =
  match (ctl.outside, ctl.snapshot) with
  | Dropped, _ | Kept, None -> ()
  | Kept, Some _ ->
      let owner, root_path = root ctl in
      let root = path_segments root_path in
      let touched = touched ~committed:true ctl in
      let (Base_dir ((module B), bv)) = owner in
      let at segs =
        Eio_utils.Posix_path.join root_path (String.concat "/" segs)
      in
      List.iter
        (fun path ->
          match tree_path ~root path with
          | None | Some [] -> ()
          | Some segs when touched None segs -> ()
          | Some segs ->
              let st = Option.get ctl.snapshot in
              let dir st pre =
                match tree_find st.tree pre with
                | Some _ -> st
                | None -> (
                    if touched None pre then st
                    else
                      match B.stat bv ~follow:false (at pre) with
                      | { kind = `Directory; perm; uid; gid; ino; _ } as s ->
                          set_node st pre
                            (Some
                               (Dir
                                  {
                                    perm;
                                    uid;
                                    gid;
                                    ino;
                                    times = times_of s;
                                    entries = Paths.empty;
                                  }))
                      | _ | (exception Eio.Io _) -> st)
              in
              let st = List.fold_left dir st (above segs) in
              let node =
                match snapshot_at owner path with
                | node -> kept_entry ~touched ~model:st.tree segs (Some node)
                | exception e when is_gone e -> None
              in
              let st = set_node st segs node in
              ctl.snapshot <- Some st;
              ctl.durable <- None;
              Option.iter
                (fun node -> ctl.snapped <- snap_files segs node ctl.snapped)
                node)
        paths

(* How the entry that made [ino] put it: where, as what, and the files it
   recorded below it. *)
let made_node ctl ino =
  match Journal.made ctl.journal ino with
  | None -> None
  | Some e ->
      List.find_map
        (fun (segs, edit) ->
          match edit with
          | Place { node; _ } when Int64.equal (ino_of node) ino ->
              Some (segs, node, e.op.recorded)
          | Place _ | Move _ | Remove _ | Attrs _ | Times _ -> None)
        e.op.delta

let replay_in ctl st e = replay ~root:(root_segs ctl) ~made:(made_node ctl) st e

(* The durable image, built from the snapshot when it is not kept. *)
let durable ctl =
  match (ctl.durable, ctl.snapshot) with
  | Some st, _ -> st
  | None, None -> err_not_wrapped ()
  | None, Some snapshot ->
      let st =
        Journal.fold
          (fun (e : metadata_entry) st ->
            if e.committed then replay_in ctl st e else st)
          ctl.journal snapshot
      in
      ctl.durable <- Some st;
      st

(* The committed entries before the first pending one go into the
   snapshot: nothing replays them again. *)
let fold_committed ctl =
  match ctl.snapshot with
  | None -> ()
  | Some snapshot ->
      let rec go st =
        match Journal.pop_committed ctl.journal with
        | Some e ->
            let st = replay_in ctl st e in
            List.iter
              (fun n -> if Inos.mem n st.inos then Journal.forget ctl.journal n)
              e.nodes;
            go st
        | None -> st
      in
      ctl.snapshot <- Some (go snapshot)

(* Whether [e], which a barrier or a sync reached, holds in [st]: a change
   of attributes once its node is there, and an entry a barrier reached
   once a directory it changes is there, as the entry changes that one even
   where the other is not durable yet. *)
let reached st (e : metadata_entry) =
  let held ino =
    match Inos.find_opt ino st.inos with
    | Some at -> not (in_limbo at)
    | None -> false
  in
  (match e.attrs_ino with Some ino -> held ino | None -> true)
  && ((e.barred <> [] && List.exists held e.dirs)
     || (e.synced && Option.is_some e.attrs_ino))

(* fsync(2) on a directory commits that directory's entries and nothing
   else: the journal entries that changed it, keyed by its inode, so a
   barrier follows the directory through a rename and never reaches
   another of the same name. An entry of a directory whose own entry is not
   durable cannot be durable yet: after the power cut there is no directory
   to hold it. The barrier is remembered on the entry, which becomes
   durable as soon as a directory it changes is, with the pending entries
   before it that it needs.

   The entries a barrier commits are not in general a journal prefix, and
   one committed late is replayed over the durable image as it stands: it
   changes no directory a later committed entry changes, as that entry
   would have pulled it in, so it lands as it would in program order. *)
let settle ctl =
  (* The image the commits replay over, built before any of them. *)
  let (_ : state) = durable ctl in
  Journal.settle ctl.journal
    ~reached:(fun e -> reached (durable ctl) e)
    ~commit:(fun newly ->
      ctl.effects <- ctl.effects + List.length newly;
      ctl.durable <- Some (List.fold_left (replay_in ctl) (durable ctl) newly));
  fold_committed ctl

(* Hold a barrier on the directory [ino] for the entries that changed it,
   then commit what it can. *)
let commit_dir_ino ?upto ctl ino =
  ctl.effects <- ctl.effects + Journal.bar ?upto ctl.journal ino;
  settle ctl

let dir_ino ctl dir =
  let owner, _ = root ctl in
  ino_at owner dir

(* fsync(2) of a file commits its inode, attributes included: they hold
   once the image holds the file. It covers the changes made before
   [upto], as a barrier on a directory does. *)
let commit_attrs ~upto ctl ~ino =
  if Journal.has_attrs ctl.journal then begin
    ctl.effects <- ctl.effects + Journal.sync_attrs ~upto ctl.journal ino;
    settle ctl
  end

(* A write or a truncation that cleared setuid or setgid changed the
   file's mode, which ext4 journals at once, in program order: a pending
   change of attributes, committed by the file's sync and by any later
   commit. *)
let record_clear ctl fs (st : Eio.File.Stat.t) =
  ctl.seq <- ctl.seq + 1;
  let segs =
    Option.value ~default:[] (tree_path ~root:(root_segs ctl) fs.rel)
  in
  Journal.add ctl.journal
    (Journal.entry ~seq:ctl.seq ~attrs_ino:st.ino ~clear:true
       {
         meta =
           metadata_unsynced ~path:fs.rel
             ~kind:(Chmod { perm = st.perm })
             ~seq:ctl.seq;
         paths = [ segs ];
         delta =
           [
             ( segs,
               Attrs
                 {
                   ino = st.ino;
                   perm = st.perm;
                   uid = st.uid;
                   gid = st.gid;
                   times = times_of st;
                 } );
           ];
         recorded = [];
       })

(* A file the wrapper first meets on an open that finds it on disk was made
   by nothing the model recorded. It is the file an image holds at its
   path if that node is its inode, or wherever the image holds that inode:
   the snapshot and the durable image, and the nodes pending entries put.
   Locating it there lets a crash rewrite it with what it synced. *)
let locate_preexisting ctl ((_, rel) as location) ~ino =
  let root = root_segs ctl in
  match tree_path ~root rel with
  | None -> ()
  | Some segs ->
      let key = String.concat "/" segs in
      let mine tree =
        match tree_find tree segs with
        | Some (File f) -> Int64.equal f.ino ino
        | Some (Dir _ | Link _) | None -> false
      in
      (* By inode: an uncommitted rename of the file, or of a directory
         above it, leaves it under its old name in the image. *)
      let locate st =
        if mine st.tree then { st with locs = locate_at key location st.locs }
        else
          match Inos.find_opt ino st.inos with
          | Some (_ :: _ as at) -> (
              match tree_find st.tree at with
              | Some (File f) when Int64.equal f.ino ino ->
                  {
                    st with
                    locs = locate_at (String.concat "/" at) location st.locs;
                  }
              | Some _ | None -> st)
          | Some [] | None -> st
      in
      ctl.snapshot <- Option.map locate ctl.snapshot;
      ctl.durable <- Option.map locate ctl.durable;
      let in_delta (e : metadata_entry) =
        List.exists
          (fun (s, edit) ->
            match edit with
            | Place { node; _ } when is_prefix s segs -> (
                match tree_find node (List.drop (List.length s) segs) with
                | Some (File f) -> Int64.equal f.ino ino
                | Some (Dir _ | Link _) | None -> false)
            | Place _ | Move _ | Remove _ | Attrs _ | Times _ -> false)
          e.op.delta
      in
      Journal.fold
        (fun (e : metadata_entry) () ->
          if in_delta e then e.op.recorded <- location :: e.op.recorded)
        ctl.journal ()

let page_of c p =
  if Cstruct.length c > p * page then
    Some (Cstruct.sub c (p * page) (min page (Cstruct.length c - (p * page))))
  else None

let pages c =
  List.init
    ((Cstruct.length c + page - 1) / page)
    (fun p -> (p, Option.get (page_of c p)))

(* The contents of [l] that differ, the first [max_stale] of them. *)
let distinct l =
  let rec go n acc = function
    | _ when n = max_stale -> List.rev acc
    | [] -> List.rev acc
    | ((pg, _) as c) :: rest ->
        if List.exists (fun (q, _) -> Cstruct.equal pg q) acc then go n acc rest
        else go (n + 1) (c :: acc) rest
  in
  go 0 [] l

(* What a cut decided of the journal, from which it draws the files' data. *)
type verdict = {
  ctl : t;
  data_rng : draw option;
  bytes : bool;
  lost : int;
      (* Where the cut first loses a journal entry: a length a change from
         there on sets does not land, nor a block it allocates. An overwrite
         within the length may land without it. *)
  kept : int;
      (* The last place the cut keeps: a truncation up to it lands, as ext4
         journals it in the syscall's own handle. *)
  removed : (string * Cstruct.t) list;
      (* The files the image no longer holds, whose blocks are free, by
         path. *)
  unsynced_removed : (string * Cstruct.t * Cstruct.t) list;
      (* Those that held unsynced pages, with their durable and live
         bytes. *)
  elsewhere : (Cstruct.t * string) list Lazy.t;
  olds : (int * int, (Cstruct.t * string) list) Hashtbl.t;
  contents : (int, Cstruct.t) Hashtbl.t;
  mutable described : string list;
      (* What the cut left of each file's unsynced writes, latest first. *)
}

(* What a block the page [p] of [fs] may have been given held: a page a
   shrink of [fs] freed, else the first removed file that had one there; and
   the pages a removed file held unsynced, which writeback may have put on
   disk before the removal. *)
let same verdict (fs : fstate) p =
  (match List.assoc_opt p fs.freed with
    | Some pg -> [ (pg, fs.rel ^ " held there before its truncation") ]
    | None ->
        Option.to_list
          (List.find_map
             (fun (path, c) ->
               Option.map
                 (fun pg -> (pg, "the removed " ^ path ^ " held there"))
                 (page_of c p))
             verdict.removed))
  @ List.filter_map
      (fun (path, durable, live) ->
        match (page_of live p, page_of durable p) with
        | Some pg, Some d when Cstruct.equal pg d -> None
        | Some pg, _ -> Some (pg, "the removed " ^ path ^ " held there unsynced")
        | None, _ -> None)
      verdict.unsynced_removed

(* As ext4 may give a freed block to any offset of any file, the pages other
   shrinks freed, and those of removed files. *)
let elsewhere ctl ~removed ~unsynced_removed =
  let freed =
    Hashtbl.fold (fun _ (fs : fstate) acc -> fs :: acc) ctl.known []
    |> List.sort (fun (a : fstate) b -> Int.compare b.id a.id)
    |> List.concat_map (fun (fs : fstate) ->
        List.map
          (fun (p, pg) ->
            (pg, Fmt.str "%s held at %d before its truncation" fs.rel (p * page)))
          fs.freed)
  and gone =
    List.concat_map
      (fun (path, c) ->
        List.map
          (fun (p, pg) ->
            (pg, Fmt.str "the removed %s held at %d" path (p * page)))
          (pages c))
      removed
  and unsynced =
    List.concat_map
      (fun (path, durable, live) ->
        List.filter_map
          (fun (p, pg) ->
            match page_of durable p with
            | Some d when Cstruct.equal pg d -> None
            | _ ->
                Some
                  ( pg,
                    Fmt.str "the removed %s held at %d unsynced" path (p * page)
                  ))
          (pages live))
      unsynced_removed
  in
  gone @ unsynced @ freed

(* The stale contents the page [p] of [fs] may hold: [same], then
   [elsewhere], each content once. *)
let old verdict (fs : fstate) p =
  match Hashtbl.find_opt verdict.olds (fs.id, p) with
  | Some l -> l
  | None ->
      let l = distinct (same verdict fs p @ Lazy.force verdict.elsewhere) in
      Hashtbl.replace verdict.olds (fs.id, p) l;
      l

let describe verdict (fs : fstate) mine size runs =
  let durable = Cstruct.length (bytes_of fs.durable) in
  let run (off, n, src) =
    let at = Fmt.str "@%d+%d" off n in
    match src with
    | `Durable when off >= durable -> Some (at ^ " zeros")
    | `Durable -> None
    | `Change k -> (
        match (List.nth mine k : entry).change with
        | Write { offset; data } ->
            Fmt.kstr Option.some "%s as the write @%d+%d left it" at offset
              (Cstruct.length data)
        | Resize m when off >= m ->
            Fmt.kstr Option.some "%s zeros, from the truncation to %d" at m
        | Resize m when m < off + n ->
            Fmt.kstr Option.some
              "%s as the truncation to %d left it, zeros from %d" at m m
        | Resize m ->
            Fmt.kstr Option.some "%s as the truncation to %d left it" at m)
    | `Over -> Some (at ^ " as a failed sync wrote it")
    | `Stale None -> Fmt.kstr Option.some "%s stale %C" at stale
    | `Stale (Some from) -> Fmt.kstr Option.some "%s stale (what %s)" at from
  in
  let parts = List.filter_map run runs in
  let parts =
    match List.length parts with
    | n when n > 8 -> List.take 8 parts @ [ Fmt.str "and %d more runs" (n - 8) ]
    | _ -> parts
  in
  let held =
    if parts <> [] then ": " ^ String.concat "; " parts
    else if size = durable then ", as its last sync left it"
    else ""
  in
  verdict.described <-
    Fmt.str "%s, %d bytes%s" fs.rel size held :: verdict.described

(* Under a scripted policy, a change from [upto] on that would set the
   length does not survive. *)
let keep_length (fs : fstate) ~upto survivors =
  match upto with
  | None -> survivors
  | Some c ->
      let _, kept =
        List.fold_left
          (fun (len, acc) (e : entry) ->
            let len' =
              match e.change with
              | Write { offset; data } -> max len (offset + Cstruct.length data)
              | Resize m -> m
            in
            if e.seq >= c && len' <> len then (len, acc) else (len', e :: acc))
          (Cstruct.length (bytes_of fs.durable), [])
          survivors
      in
      List.rev kept

(* The bytes of [fs] with the changes of [mine] for which [pred] holds and
   the truncations that land. *)
let survive verdict (fs : fstate) ~mine ~truncated ~upto pred =
  let kept =
    List.filter (fun e -> pred e || truncated e) mine |> keep_length fs ~upto
  in
  List.map (fun e -> (e.change, e.cached)) kept
  |> replay_cached ~bs:verdict.ctl.block_size ~cache:fs.cache
       (bytes_of fs.durable)
  |> fst
  |> sectors_together ~cache:fs.cache (bytes_of fs.durable)
       (List.map (fun e -> (e.change, e.cached, List.memq e kept)) mine)

(* The bytes of [fs] with the changes of [mine] written back in blocks drawn
   from [rng]. *)
let written_back verdict (fs : fstate) ~mine ~len_upto ~len_from rng =
  let changes = List.map (fun e -> (e.change, e.cached)) mine in
  (* A writeback that failed only partly wrote any of the blocks it lost. *)
  let over =
    List.sort_uniq Int.compare (List.map (fun (b, _, _) -> b) fs.partial)
    |> List.filter_map (fun b ->
        (* Any failed writeback's version of the block, or none. *)
        let vs =
          List.filter_map
            (fun (b', _, blk) -> if b' = b then Some blk else None)
            fs.partial
        in
        match rng.int (List.length vs + 1) with
        | 0 -> None
        | i -> Some (b, List.nth vs (i - 1)))
  in
  (* The page cache kept what a failed sync lost, or evicted it, whether a
     later sync ran or not. *)
  if (Option.is_some fs.cache || Option.is_some fs.evicted) && rng.bool () then
    writeback ~over ~len_upto ~len_from ~bytes:verdict.bytes ~held:fs.held
      ~old:(old verdict fs) ~describe:(describe verdict fs mine) ~rng
      ~bs:verdict.ctl.block_size ~cache:None
      (Option.value ~default:(bytes_of fs.durable) fs.evicted)
      (List.map (fun (c, _) -> (c, false)) changes)
  else
    writeback ~over ~len_upto ~len_from ~bytes:verdict.bytes ~held:fs.held
      ~old:(old verdict fs) ~describe:(describe verdict fs mine) ~rng
      ~bs:verdict.ctl.block_size ~cache:fs.cache (bytes_of fs.durable) changes

(* What the cut left in [fs], drawn once. *)
let content verdict (fs : fstate) =
  match Hashtbl.find_opt verdict.contents fs.id with
  | Some data -> data
  | None ->
      let mine = List.rev fs.pending in
      let truncated (e : entry) =
        match e.change with
        | Resize _ -> e.seq <= verdict.kept
        | Write _ -> false
      in
      (* The changes up to the last truncation that lands set the length,
         wherever the cut first loses an entry. *)
      let from =
        List.fold_left
          (fun m (e : entry) -> if truncated e then e.seq else m)
          min_int mine
      in
      let count s =
        List.length (List.filter (fun (e : entry) -> e.seq < s) mine)
      in
      let upto =
        if verdict.lost = max_int then None
        else Some (max verdict.lost (from + 1))
      in
      let len_upto = match upto with None -> max_int | Some s -> count s in
      let len_from = if from = min_int then 0 else count (from + 1) in
      let data =
        match (verdict.ctl.policy, verdict.data_rng) with
        | (Reorder_unsynced _ | Choose _), Some rng ->
            written_back verdict fs ~mine ~len_upto ~len_from rng
        | Survive pred, _ ->
            survive verdict fs ~mine ~truncated ~upto (fun e ->
                pred (unsynced_of_entry e))
        | Drop_unsynced, _ when List.exists truncated mine ->
            survive verdict fs ~mine ~truncated ~upto (fun _ -> false)
        | _ -> copy_cs (bytes_of fs.durable)
      in
      Hashtbl.replace verdict.contents fs.id data;
      data

(* The uncommitted entries of [pending] the policy keeps: a prefix of
   program order. *)
let surviving ctl pending =
  match ctl.policy with
  | Drop_unsynced -> []
  | Survive pred ->
      List.take_while (fun (e : metadata_entry) -> pred e.op.meta) pending
  | Reorder_unsynced seed ->
      (* "META" (0x4d455441) as a shift: the literal overflows a 31-bit int
         (js_of_ocaml, wasm_of_ocaml). *)
      let salt = (0x4d45 lsl 16) lor 0x5441 in
      let rng = Random.State.make [| seed; salt |] in
      (* A uniform length: late crash points are as likely as early ones. *)
      let n = Random.State.int rng (List.length pending + 1) in
      List.take n pending
  | Choose f ->
      let n = f (List.length pending + 1) in
      List.filteri (fun i _ -> i < n) pending

(* The snapshot with, in program order, every committed entry, every
   surviving one, and every one a barrier reached that holds in the image by
   then, with the entries it needs, as [settle] commits it: a deferred
   barrier holds once its directory does, wherever the cut fell. An entry
   pulled in that way may come before entries already replayed, so the
   replay starts again with it. The entries it replayed are by seq in
   [included]. *)
let replay_cut ctl journal ~survives =
  let included = Hashtbl.create 16 in
  let snapshot =
    match ctl.snapshot with Some st -> st | None -> assert false
  in
  List.iter
    (fun (e : metadata_entry) ->
      if e.committed || survives e then Hashtbl.replace included e.seq ())
    journal;
  let rec build () =
    let again = ref false in
    let st =
      List.fold_left
        (fun st (e : metadata_entry) ->
          if !again then st
          else if Hashtbl.mem included e.seq then replay_in ctl st e
          else if reached st e then begin
            let clears = Journal.clears_before ctl.journal e.seq in
            List.iter
              (fun (d : metadata_entry) ->
                if not (Hashtbl.mem included d.seq) then begin
                  Hashtbl.replace included d.seq ();
                  if d != e then again := true
                end)
              (clears @ Journal.closure ctl.journal e);
            replay_in ctl st e
          end
          else st)
        snapshot journal
    in
    if !again then build () else st
  in
  (build (), included)

(* The files [tree] no longer holds, whose blocks are free, by path, and
   those of them that held unsynced pages, with their durable and their live
   bytes. *)
let removed_files ctl tree locs =
  let inos = index [] tree Inos.empty in
  let located = Hashtbl.create 8 in
  Paths.iter
    (fun _ ls -> List.iter (fun (fs, _) -> Hashtbl.replace located fs.id ()) ls)
    locs.at;
  (* A file the program still names keeps its blocks, even where the cut
     lost its creation: only a removal frees them. *)
  let live = Hashtbl.create 8 in
  Hashtbl.iter (fun _ (fs : fstate) -> Hashtbl.replace live fs.id ()) ctl.files;
  (* Nor does a removed file still open: ext4 frees its blocks at its last
     close. *)
  let is_open ino = Hashtbl.mem ctl.opened ino in
  let kept id =
    Hashtbl.mem located id || Hashtbl.mem live id
    || Option.fold ~none:false ~some:is_open (Hashtbl.find_opt ctl.file_inos id)
  in
  let gone_files =
    Hashtbl.fold
      (fun id (fs : fstate) acc ->
        if kept id then acc else (fs.rel, bytes_of fs.durable) :: acc)
      ctl.known []
  and gone_nodes =
    Inos.fold
      (fun ino (path, data) acc ->
        if Inos.mem ino inos || is_open ino then acc else (path, data) :: acc)
      ctl.snapped []
  in
  let unsynced_removed =
    Hashtbl.fold
      (fun id (fs : fstate) acc ->
        if kept id || fs.pending = [] then acc
        else
          let durable = bytes_of fs.durable in
          let pending =
            List.rev_map (fun e -> (e.change, e.cached)) fs.pending
          in
          let _, live =
            replay_cached ~bs:ctl.block_size durable ~cache:fs.cache pending
          in
          (fs.rel, durable, live) :: acc)
      ctl.known []
  in
  (gone_files @ gone_nodes, unsynced_removed)

(* The tree to restore, and whether the cut rewrites the file at a key:
   under [Kept], what the store holds where the model touched nothing
   stays. *)
let with_outside ctl ~owner ~root_path tree =
  match ctl.outside with
  | Dropped -> (tree, fun _ -> true)
  | Kept -> (
      let touched = touched ctl in
      match snapshot_at owner root_path with
      | Dir _ as d ->
          let tree = kept_dir ~touched ~model:tree [] d in
          ( tree,
            fun key ->
              let segs = path_segments key in
              touched (tree_find tree segs) segs )
      | File _ | Link _ -> err_base_not_dir ())

(* Each tracked file is rewritten where the image holds it, which a rename
   may have made another name than the one it was met at. *)
let rewrite_files verdict ~owner ~root_path ~tree ~locs ~rewrites =
  let rel_of key = Eio_utils.Posix_path.join root_path key in
  let proot = Option.get verdict.ctl.root_box in
  Paths.iter
    (fun key ls ->
      if (not (in_limbo (path_segments key))) && rewrites key then
        let rel = rel_of key in
        List.iter
          (fun (fs, _) ->
            let content = content verdict fs in
            let (Base_dir ((module B), bv)) = owner in
            match B.stat bv ~follow:false rel with
            | { kind = `Regular_file; _ } ->
                fs.owner <- proot;
                fs.rel <- rel;
                rewrite verdict.ctl fs content;
                (* The later of the versions the image and its last sync
                   hold. *)
                let times =
                  List.filter_map Fun.id
                    [
                      Option.map times_of_node
                        (tree_find tree (path_segments key));
                      Option.map times_of fs.synced;
                    ]
                  |> List.sort (fun a b -> Float.compare b.ctime a.ctime)
                in
                Option.iter (verdict.ctl.set_times rel) (List.nth_opt times 0)
            | _ | (exception Eio.Io _) -> ())
          ls)
    locs.at

let rebuilt_image verdict ~tree ~locs =
  fold_nodes
    (fun segs node acc ->
      match segs with
      | [] -> acc
      | _ ->
          let key = String.concat "/" segs in
          let kind =
            match node with
            | Dir _ -> `Dir
            | Link l -> `Link l.target
            | File f -> (
                match Paths.find_opt key locs.at with
                | Some ((fs, _) :: _) ->
                    `File (Cstruct.to_string (content verdict fs))
                | Some [] | None -> `File (Cstruct.to_string f.data))
          in
          (key, ino_of node, kind) :: acc)
    [] tree []

(* The draws the files' data is made of under a policy that writes back in
   blocks. *)
let data_draws = function
  | Reorder_unsynced seed -> Some (draw_of_state (Random.State.make [| seed |]))
  | Choose f -> Some (draw_of_fun f)
  | Drop_unsynced | Survive _ -> None

(* [tree] without the nodes a rename left in limbo. *)
let without_limbo = function
  | Dir d ->
      Dir
        {
          d with
          entries = Paths.filter (fun k _ -> not (is_limbo k)) d.entries;
        }
  | t -> t

(* The image is rebuilt whole even in a fiber being cancelled, as the one
   whose operation cuts the power often is. *)
let cut ctl =
  if not ctl.crashed then begin
    if Option.is_none ctl.base then err_not_wrapped ();
    ctl.crashed <- true;
    ctl.queued <- [];
    Eio.Cancel.protect @@ fun () ->
    let data_rng = data_draws ctl.policy in
    let bytes = match ctl.policy with Choose _ -> true | _ -> false in
    let journal =
      List.rev
        (Journal.fold (fun (e : metadata_entry) acc -> e :: acc) ctl.journal [])
    in
    let pending =
      List.filter (fun (e : metadata_entry) -> not e.committed) journal
    in
    let survivors = surviving ctl pending in
    let survives e = List.memq e survivors in
    let owner, root_path = root ctl in
    let { tree; locs; _ }, included = replay_cut ctl journal ~survives in
    let in_cut (e : metadata_entry) = Hashtbl.mem included e.seq in
    let lost = Journal.first_lost ctl.journal ~kept:in_cut in
    let kept = Journal.last_kept ctl.journal ~kept:in_cut in
    let tree = without_limbo tree in
    let removed, unsynced_removed = removed_files ctl tree locs in
    let tree, rewrites = with_outside ctl ~owner ~root_path tree in
    restore_root ~stamp:ctl.set_times owner root_path tree;
    let verdict =
      {
        ctl;
        data_rng;
        bytes;
        lost;
        kept;
        removed;
        unsynced_removed;
        elsewhere = lazy (elsewhere ctl ~removed ~unsynced_removed);
        olds = Hashtbl.create 8;
        contents = Hashtbl.create 16;
        described = [];
      }
    in
    rewrite_files verdict ~owner ~root_path ~tree ~locs ~rewrites;
    ctl.left <- List.rev verdict.described;
    ctl.image <- rebuilt_image verdict ~tree ~locs
  end

(* The inodes of the directories a rename to [dst] lands under, from the
   root down. *)
let rename_ancestors owner ~root dst =
  let segs = path_segments (parent_path dst) in
  let lead = if String.starts_with ~prefix:"/" dst then "/" else "" in
  List.init
    (List.length segs + 1 - List.length root)
    (fun i -> List.length root + i)
  |> List.filter_map (fun n ->
      ino_at owner (lead ^ String.concat "/" (List.take n segs)))

(* New attributes on the node: committed by a barrier on the node itself,
   for a directory, or by its sync, for a file. A barrier on its parent does
   not: ext4's fsync of a directory completes only that directory's own last
   transaction. *)
let attrs_delta owner ~root p =
  let (Base_dir ((module B), bv)) = owner in
  let st = B.stat bv ~follow:false p in
  match tree_path ~root p with
  | Some segs ->
      let edit =
        Attrs
          {
            ino = st.ino;
            perm = st.perm;
            uid = st.uid;
            gid = st.gid;
            times = times_of st;
          }
      in
      ([ (segs, edit) ], [ st.ino ], Some st.ino)
  | None -> ([], [], None)

(* A rename moves the image's own node: nothing of what it holds live comes
   with it. It is committed by a barrier on either directory. *)
let rename_delta ~gone ?victim ~root ~parent src dst =
  match
    (List.assoc_opt src gone, tree_path ~root dst, parent src, parent dst)
  with
  | Some ino, Some segs, Some from, Some up ->
      (* The node it replaces goes first: a directory the image still fills
         stops the rename, and the entries that emptied it come with it. *)
      let replaced =
        match victim with
        | Some v when not (Int64.equal v ino) -> [ (segs, Remove v) ]
        | Some _ | None -> []
      in
      (replaced @ [ (segs, Move { parent = up; ino }) ], [ from; up ], None)
  | _ -> ([], [], None)

(* Each path [resolved] names leaves its directory, or enters it as the
   store now holds it. *)
let place_delta ~gone owner ~root ~parent resolved =
  let halves =
    List.filter_map
      (fun p ->
        match (tree_path ~root p, parent p) with
        | None, _ | _, None -> None
        | Some segs, Some up -> (
            match List.assoc_opt p gone with
            | Some ino -> Some (up, (segs, Remove ino))
            | None -> (
                match snapshot_at owner p with
                | node -> Some (up, (segs, Place { parent = up; node }))
                | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None)))
      resolved
  in
  (List.map snd halves, List.map fst halves, None)

(* The times the change gave the directories it changed, and the node a
   rename moved: they land with it, as ext4 journals them together. *)
let stamps owner ~kind ~delta resolved =
  match (kind, delta) with
  | (Chmod _ | Chown _), _ | _, [] -> []
  | _ ->
      let (Base_dir ((module B), bv)) = owner in
      let moved =
        match (kind, resolved) with Rename _, [ _; dst ] -> [ dst ] | _ -> []
      in
      List.filter_map
        (fun p ->
          match B.stat bv ~follow:false p with
          | st -> Some ([], Times { ino = st.ino; times = times_of st })
          | exception Eio.Io _ -> None)
        (List.map parent_path resolved @ moved)

(* [path] and [kind] as the caller spelt them, [resolved] the paths the
   operation touched, resolved: its own first. [gone] gives the inode a
   path held before an operation that took it away, and [victim] the node
   a rename replaced. *)
let record_metadata ?(gone = []) ?victim ctl ~path ~kind ~resolved =
  ctl.seq <- ctl.seq + 1;
  let owner, root_path = root ctl in
  let meta = metadata_unsynced ~path ~kind ~seq:ctl.seq in
  let root = path_segments root_path in
  let parent p = ino_at owner (parent_path p) in
  let ancestors =
    match (kind, resolved) with
    | Rename _, [ _; dst ] -> rename_ancestors owner ~root dst
    | _ -> []
  in
  let delta, dirs, attrs_ino =
    match (kind, resolved) with
    | (Chmod _ | Chown _), [ p ] -> attrs_delta owner ~root p
    | Rename _, [ src; dst ] -> rename_delta ~gone ?victim ~root ~parent src dst
    | _ -> place_delta ~gone owner ~root ~parent resolved
  in
  let recorded =
    List.concat_map (files_under ctl) resolved
    |> List.sort_uniq (fun a b -> Int.compare a.id b.id)
    |> List.map (fun fs -> (fs, fs.rel))
    |> List.filter (covers ~root delta)
  in
  let nodes =
    List.filter_map
      (fun (_, edit) ->
        match edit with
        | Place { node; _ } -> Some (ino_of node)
        | Move _ | Remove _ | Attrs _ | Times _ -> None)
      delta
  in
  let parents =
    match kind with
    | Chmod _ | Chown _ -> []
    | _ -> List.sort_uniq Int64.compare dirs
  in
  let delta = delta @ stamps owner ~kind ~delta resolved in
  let moved, removed =
    List.fold_right
      (fun (_, edit) (moved, removed) ->
        match edit with
        | Move { ino; _ } -> (ino :: moved, removed)
        | Remove ino -> (moved, ino :: removed)
        | Place _ | Attrs _ | Times _ -> (moved, removed))
      delta ([], [])
  in
  Journal.add ctl.journal
    (Journal.entry ~seq:ctl.seq ~dirs ?attrs_ino ~nodes ~parents ~ancestors
       ~moved ~removed
       {
         meta;
         delta;
         paths = List.filter_map (tree_path ~root) resolved;
         recorded;
       })

(* One record backs both file wrappers. [cfs] is present exactly for a
   read-write file (the tracked durability state); reads work on either. *)
type cfile = {
  ctl : t;
  cfs : fstate option;
  cro : Eio.File.ro_ty r;
  crw : Eio.File.rw_ty r option;
  cappend : bool;
  cpos : int ref;
  cino : int64;
  mutable closed : bool;
}

let guard cf = if cf.ctl.crashed then raise_power_lost ()

let open_ino ctl base =
  let ino = (Eio.File.stat base).ino in
  Hashtbl.replace ctl.opened ino
    (1 + Option.value ~default:0 (Hashtbl.find_opt ctl.opened ino));
  ino

(* A handle closes by its own close, or with the switch the base file
   beneath it was opened in. *)
let release cf =
  if not cf.closed then begin
    cf.closed <- true;
    match Hashtbl.find_opt cf.ctl.opened cf.cino with
    | Some n when n > 1 -> Hashtbl.replace cf.ctl.opened cf.cino (n - 1)
    | Some _ | None -> Hashtbl.remove cf.ctl.opened cf.cino
  end

let close_cfile cf =
  release cf;
  Wrapper.close_resource cf.cro

(* fsyncgate: a failed fsync(2) marks the file's dirty pages clean without
   writing them, so they are lost whatever a later sync reports. *)
let lose ctl fs =
  ctl.pending_changes <- ctl.pending_changes - List.length fs.pending;
  fs.pending <- [];
  fs.gen <- fs.gen + 1;
  (* Compact once stale entries outnumber pending ones. *)
  if ctl.logged > (2 * ctl.pending_changes) + 64 then begin
    ctl.log <- List.filter is_pending ctl.log;
    ctl.logged <- ctl.pending_changes
  end

(* The pending changes made after [upto] leave [fs.pending], for
   [keep_young] to log again once the older ones went: a sync covers
   the changes made before it, not those made while it runs. *)
let split_young ctl fs ~upto =
  let young, old =
    List.partition (fun (e : entry) -> e.seq > upto) fs.pending
  in
  fs.pending <- old;
  ctl.pending_changes <- ctl.pending_changes - List.length young;
  young

let keep_young ctl fs young =
  List.iter (fun e -> push ctl fs { e with egen = fs.gen }) (List.rev young)

(* Evicted, the pages that failed read as the disk holds them: the durable
   bytes, with what was flushed and the truncations kept, at the length [len]
   the file has. None of their writeback landed. *)
let evicted_pages (fs : fstate) ~flushed ~len pending =
  let e =
    replay_changes (bytes_of fs.durable)
      (List.filter_map
         (fun e ->
           match e.change with
           | _ when flushed e -> Some e.change
           | Resize _ -> Some e.change
           | Write _ -> None)
         pending)
  in
  let img = Cstruct.create len in
  Cstruct.blit e 0 img 0 (min len (Cstruct.length e));
  img

(* The blocks of a failed sync's pages, which are clean and unwritten: the
   dirty changes', over the length [cache] and the flushed changes give. *)
let failed_blocks ctl ~cache ~flushed pending =
  let bs = ctl.block_size in
  let flushed, dirty = List.partition flushed pending in
  let uncached l =
    List.filter_map (fun e -> if e.cached then None else Some e.change) l
  in
  let len =
    List.fold_left
      (fun n -> function
        | Write { offset; data } -> max n (offset + Cstruct.length data)
        | Resize m -> m)
      (Cstruct.length cache) (uncached flushed)
  in
  dirty_blocks ~writes:true ~bs len (uncached dirty)

(* The versions of the block [b] a partly failed writeback may have put on
   disk: as it stands in [live], and in [evicted] where it differs. *)
let block_versions ctl ~live ~evicted b =
  let bs = ctl.block_size in
  let version src b =
    let off = b * bs in
    let n = min bs (Cstruct.length src - off) in
    (* A page cut away by then is written no more; the tail of the last
       one past the end is zero. *)
    if n <= 0 then None else Some (b, ctl.seq, copy_cs (Cstruct.sub src off n))
  in
  let v = version live b in
  match Option.bind evicted (fun e -> version e b) with
  | Some (_, _, e) as w
    when not
           (Option.fold ~none:false
              ~some:(fun (_, _, l) -> Cstruct.equal l e)
              v) ->
      Option.to_list v @ Option.to_list w
  | _ -> Option.to_list v

(* A failed barrier loses the pending writes, which the page cache keeps:
   a later change that dirties one of their blocks writes it whole. On
   macOS, the changes an earlier fsync flushed are in the drive's cache,
   not lost: they stay pending, as the page cache holds them. *)
let fail_sync ?(upto = max_int) ?(evict = false) ?(relive = ignore) ctl fs =
  let young = split_young ctl fs ~upto in
  ctl.effects <- ctl.effects + 1;
  let cache = Option.value ~default:(bytes_of fs.durable) fs.cache in
  let pending = List.rev fs.pending in
  let flushed (e : entry) = e.seq <= fs.flushed in
  (* A kept truncation is in the page cache already. *)
  let changes =
    List.filter_map (fun e -> if e.cached then None else Some e.change) pending
  in
  let live = replay_changes cache changes in
  (* Had an earlier failure's pages been evicted, the changes, kept
     truncations included, landed over the bytes re-read from disk. *)
  let evicted =
    match (fs.cache, fs.evicted) with
    | None, None -> None
    | _, e ->
        Some
          (replay_changes
             (Option.value ~default:(bytes_of fs.durable) e)
             (List.map (fun e -> e.change) pending))
  in
  let live =
    if not evict then live
    else
      let img = evicted_pages fs ~flushed ~len:(Cstruct.length live) pending in
      relive img;
      fs.evicted <- None;
      img
  in
  fs.cache <- Some live;
  let written = failed_blocks ctl ~cache ~flushed pending in
  if not evict then
    fs.partial <-
      List.concat_map (block_versions ctl ~live ~evicted) written @ fs.partial;
  lose ctl fs;
  (* A truncation is metadata, journalled apart from the data: the failure
     does not lose it, and it stays pending in program order. *)
  List.iter
    (fun e ->
      match e.change with
      | Write _ when not (flushed e) -> ()
      | Write _ | Resize _ ->
          push ctl fs { e with egen = fs.gen; cached = true })
    pending;
  keep_young ctl fs young

(* A block written now is no longer one a failed sync may have left. *)
let drop_partial ctl (fs : fstate) pending =
  match fs.partial with
  | [] -> ()
  | partial ->
      let cache = Option.value ~default:(bytes_of fs.durable) fs.cache in
      let written =
        dirty_blocks ~bs:ctl.block_size (Cstruct.length cache)
          (List.filter_map
             (fun (c, cached) -> if cached then None else Some c)
             pending)
      in
      (* A shrink made durable now frees the blocks past it that a
         failure captured before it, even on a block boundary, where
         it dirties nothing. *)
      let shrinks =
        List.filter_map
          (fun e ->
            match e.change with Resize m -> Some (e.seq, m) | Write _ -> None)
          fs.pending
      in
      fs.partial <-
        List.filter
          (fun (b, at, _) ->
            (not (List.mem b written))
            && not
                 (List.exists
                    (fun (seq, m) -> seq > at && b * ctl.block_size >= m)
                    shrinks))
          partial

(* The pages a shrink freed, as the disk held them: only those from the
   lowest shrink on may be. *)
let free_pages (fs : fstate) pending =
  let held = fs.held and before = bytes_of fs.durable in
  fs.held <- List.fold_left (fun h (c, _) -> hold h c) fs.held pending;
  let from =
    List.fold_left
      (fun m -> function
        | Resize n, _ -> min m ((n + page - 1) / page * page) | Write _, _ -> m)
      max_int pending
  in
  Extents.fold ~from
    (fun x y () ->
      for p = max (x / page) (from / page) to (y - 1) / page do
        let off = p * page in
        if (not (held_at fs.held off (off + 1))) && off < Cstruct.length before
        then
          fs.freed <-
            ( p,
              copy_cs
                (Cstruct.sub before off
                   (min page (Cstruct.length before - off))) )
            :: List.remove_assoc p fs.freed
      done)
    held ()

let make_durable ctl (fs : fstate) pending =
  match (fs.cache, fs.evicted) with
  | None, None -> List.iter (fun (c, _) -> apply fs.durable c) pending
  | cache, evicted ->
      let c, live =
        replay_cached ~bs:ctl.block_size ~cache (bytes_of fs.durable) pending
      in
      (* Had the lost pages been evicted, a later write lands over the
         bytes on disk. *)
      let e =
        replay_changes
          (Option.value ~default:(bytes_of fs.durable) evicted)
          (List.map fst pending)
      in
      fs.durable.buf <- c;
      fs.durable.len <- Cstruct.length c;
      fs.cache <- (if Cstruct.equal c live then None else Some live);
      fs.evicted <- (if Cstruct.equal c e then None else Some e)

(* A barrier makes the file's pending changes durable, replayed over its
   durable bytes, with the lost bytes in the blocks they dirty: a change
   lost to a failed barrier, or made around the wrapper, never becomes
   durable by itself. *)
let commit_file ctl fs ~upto ~(stat : Eio.File.Stat.t) =
  let young = split_young ctl fs ~upto in
  if
    fs.pending <> [] || fs.partial <> [] || Option.is_some fs.cache
    || Option.is_some fs.evicted
  then ctl.effects <- ctl.effects + 1;
  let pending = List.rev_map (fun e -> (e.change, e.cached)) fs.pending in
  drop_partial ctl fs pending;
  free_pages fs pending;
  make_durable ctl fs pending;
  lose ctl fs;
  keep_young ctl fs young;
  fs.synced <- Some stat;
  commit_attrs ~upto ctl ~ino:stat.ino

let apply_commit ctl = function
  | Commit.File { fs; upto; stat } -> commit_file ctl fs ~upto ~stat
  | Attrs { ino; upto } -> commit_attrs ~upto ctl ~ino
  | Dir { ino; upto } -> commit_dir_ino ~upto ctl ino

(* What a sync makes durable lands now, as FlushFileBuffers lands it on
   Windows, but on macOS only through a full sync, F_FULLFSYNC: fsync(2)
   reaches the drive's cache, which a cut loses. A full sync drains the plain ones before it, of any file: each
   commits the changes made before its call, which [fail_sync] keeps. *)
let commit ctl ~full commit =
  match ctl.os with
  | `Linux | `Windows -> apply_commit ctl commit
  | `Macos when not full ->
      ctl.effects <- ctl.effects + 1;
      ctl.queued <- commit :: ctl.queued
  | `Macos ->
      let queued = List.rev ctl.queued in
      ctl.queued <- [];
      List.iter (apply_commit ctl) queued;
      apply_commit ctl commit

let barrier ~upto ?(full = false) ctl fs ~stat sync =
  match sync () with
  | () ->
      if ctl.os = `Macos && not full then fs.flushed <- max fs.flushed upto;
      commit ctl ~full (Commit.File { fs; upto; stat })
  | exception (Eio.Io _ as e) ->
      fail_sync ~upto ctl fs;
      raise e

let fstate_of cf = match cf.cfs with Some fs -> fs | None -> assert false
let writable cf = match cf.crw with Some rw -> rw | None -> assert false

module Cro_impl = struct
  type t = cfile

  let read_methods = []

  let single_read cf buf =
    guard cf;
    let got = Eio.Flow.single_read cf.cro buf in
    cf.cpos := !(cf.cpos) + got;
    got

  let pread cf ~file_offset bufs =
    guard cf;
    Eio.File.pread cf.cro ~file_offset bufs

  let stat cf = Eio.File.stat cf.cro
  let seek cf off cmd = Wrapper.do_seek ~pos:cf.cpos cf.cro off cmd
  let close cf = close_cfile cf
end

(* The bytes a read of the file now sees, after its pages were evicted:
   through the handle, or by the path that still names it. The file is
   emptied first: on an append handle the write lands at the end. *)
let relive_by_handle cf img =
  let rw = writable cf in
  Eio.File.truncate rw Optint.Int63.zero;
  Eio.File.pwrite_all rw ~file_offset:Optint.Int63.zero [ img ]

let relive_by_path cf fs img =
  let (Base_dir ((module B), bv)) = as_admin cf.ctl fs.owner in
  match B.stat bv ~follow:false fs.rel with
  | { kind = `Regular_file; ino; _ } when ino = (Eio.File.stat cf.cro).ino ->
      rewrite cf.ctl fs img
  | _ | (exception Eio.Io _) -> ()

(* A sync covers the changes made before it: it takes them, and the
   file's stat, then takes its time, while other fibers run, then makes
   them durable. *)
let paused cf =
  guard cf;
  let upto = cf.ctl.seq and st = Eio.File.stat cf.cro in
  cf.ctl.pause ();
  guard cf;
  (upto, st)

(* fsync(2) flushes the file, whichever descriptor names it, so a barrier
   through a read-only handle commits the file as one through a read-write
   handle does. A file the model does not track was never written through
   the wrapper and is durable already. *)
let sync_ro ~full cf fs =
  let upto, stat = paused cf in
  match fs with
  | None -> commit cf.ctl ~full (Commit.Attrs { ino = cf.cino; upto })
  | Some fs ->
      barrier ~upto ~full cf.ctl fs ~stat (fun () ->
          Wrapper.sync_base ~full:false cf.cro)

(* A directory opened as a file syncs as that directory's barrier, on the
   inode it opened. *)
let wrap_cfile_ro ~sw ?fs ?dir ctl base : Eio.File.ro_ty r =
  let cf =
    {
      ctl;
      cfs = None;
      cro = base;
      crw = None;
      cappend = false;
      cpos = ref 0;
      cino = open_ino ctl base;
      closed = false;
    }
  in
  Option.iter
    (fun (fs : fstate) -> Hashtbl.replace ctl.file_inos fs.id cf.cino)
    fs;
  Switch.on_release sw (fun () -> release cf);
  let sync ~full cf =
    match dir with
    | Some dir ->
        let upto, _ = paused cf in
        commit ctl ~full (Commit.Dir { ino = dir; upto })
    | None -> sync_ro ~full cf fs
  in
  Eio.Resource.T
    ( cf,
      Eio.Resource.handler
        (Eio.Resource.bindings (Eio.File.Pi.ro (module Cro_impl))
        @ [
            Eio.Resource.H (Eio_fs_sync.File_sync, fun cf ~full -> sync ~full cf);
            Eio.Resource.H
              ( Eio_fs_internal.Sync_failed,
                fun cf ~evict ->
                  Option.iter
                    (fun fs ->
                      fail_sync ~evict ~relive:(relive_by_path cf fs) cf.ctl fs)
                    fs );
          ]) )

module Crw_impl = struct
  include Cro_impl

  (* A write that cleared setuid or setgid, as a user's does. *)
  let note_clear cf fs =
    if fs.mode land 0o6000 <> 0 then begin
      let st = Eio.File.stat cf.cro in
      if not (Int.equal st.perm fs.mode) then begin
        fs.mode <- st.perm;
        record_clear cf.ctl fs st
      end
    end

  let single_write cf bufs =
    guard cf;
    let fs = fstate_of cf in
    let off = Wrapper.write_offset ~append:cf.cappend ~pos:cf.cpos cf.cro in
    let got = Eio.Flow.single_write (writable cf) bufs in
    clog_write cf.ctl fs ~offset:off (capture bufs got);
    note_clear cf fs;
    cf.cpos := off + got;
    got

  let copy cf ~src = Eio.Flow.Pi.simple_copy ~single_write cf ~src

  let pwrite cf ~file_offset bufs =
    guard cf;
    let fs = fstate_of cf in
    let off =
      if cf.cappend then Wrapper.write_offset ~append:true ~pos:cf.cpos cf.cro
      else Optint.Int63.to_int file_offset
    in
    let got = Eio.File.pwrite_single (writable cf) ~file_offset bufs in
    clog_write cf.ctl fs ~offset:off (capture bufs got);
    note_clear cf fs;
    got

  (* The sync barrier: the live content becomes durable and the file's pending
     changes leave the log, so a later crash can neither drop nor reorder
     them. It commits this file's data and nothing else: the directory
     entries a name lives in are the journal's, committed by [sync_metadata]
     or a registered [Dir_sync], as fsync(2) divides them. *)
  let sync_full ~full cf =
    let upto, stat = paused cf in
    let fs = fstate_of cf in
    barrier ~upto ~full cf.ctl fs ~stat (fun () -> Eio.File.sync (writable cf))

  let sync cf = sync_full ~full:false cf

  let truncate cf len =
    guard cf;
    let fs = fstate_of cf in
    Eio.File.truncate (writable cf) len;
    clog_change cf.ctl fs (Resize (Optint.Int63.to_int len));
    note_clear cf fs
end

let wrap_cfile_rw ~sw ctl fs ~append base : Eio.File.rw_ty r =
  let state =
    {
      ctl;
      cfs = Some fs;
      cro = (base :> Eio.File.ro_ty r);
      crw = Some base;
      cappend = append;
      cpos = ref 0;
      cino = open_ino ctl (base :> Eio.File.ro_ty r);
      closed = false;
    }
  in
  Hashtbl.replace ctl.file_inos fs.id state.cino;
  Switch.on_release sw (fun () -> release state);
  Wrapper.rw_boxed
    ~extra:
      [
        Eio.Resource.H
          ( Eio_fs_internal.Sync_failed,
            fun cf ~evict ->
              fail_sync ~evict ~relive:(relive_by_handle cf) cf.ctl fs );
      ]
    (module Crw_impl)
    ~barrier:(fun cf ~full -> Crw_impl.sync_full ~full cf)
    state ~base

(* [dplace] is where this directory sits under the boxed directory the
   controller was wrapped over: tracked files are addressed through that
   long-lived root so a crash can rewrite them after any intermediate staging
   subtree is closed, and a subtree held across the rename of its directory
   follows it there, as the descriptor beneath it does. *)
type cdir = {
  dctl : t;
  dbase : Eio.Fs.dir_ty r;
  dboxed : boxed;
  dplace : place;
  dino : int64 option; (* the inode a subtree opened *)
}

(* Expose the whole crash directory so a rename whose target is itself a
   crash directory reaches both its base (for the backend call) and its
   root-relative location (to rehome durability state). *)
type (_, _, _) Eio.Resource.pi +=
  | Crash_dir : ('t, 't -> cdir, [> `Crash_dir ]) Eio.Resource.pi

let dir_hook :
    (t ->
    ino:int64 option ->
    root:boxed ->
    prefix:string ->
    base:Eio.Fs.dir_ty r ->
    [ `Close | Eio.Fs.dir_ty ] r)
    ref =
  ref (fun _ ~ino:_ ~root:_ ~prefix:_ ~base:_ -> assert false)

let is_trunc = function
  | `Or_truncate _ | `Exclusive _ -> true
  | `Never | `If_missing _ -> false

let perm_of = function
  | `Never -> 0o644
  | `If_missing p | `Or_truncate p | `Exclusive p -> p

(* Compose a path opened through a subtree with the subtree's own path from
   the wrap root. *)
let at prefix path = Eio_utils.Posix_path.join prefix path

(* One key per file, whatever spelling [path] is. *)
let file_key (Base_dir ((module B), bv)) path =
  match B.native bv path with
  | Some native -> native
  | None -> path_key path
  | exception Eio.Io _ -> path_key path

let path_is_at_or_under ~parent path =
  String.equal parent path
  ||
  let n = String.length parent in
  String.length path > n
  && String.starts_with ~prefix:parent path
  && path.[n] = '/'

let track ctl fs =
  Hashtbl.replace ctl.files (file_key fs.owner fs.rel) fs;
  Hashtbl.replace ctl.known fs.id fs;
  ctl.by_rel <-
    Paths.update fs.rel
      (fun o -> Some (fs :: Option.value ~default:[] o))
      ctl.by_rel

let untrack ctl fs =
  let key = file_key fs.owner fs.rel in
  (match Hashtbl.find_opt ctl.files key with
  | Some fs' when fs' == fs -> Hashtbl.remove ctl.files key
  | Some _ | None -> ());
  ctl.by_rel <-
    Paths.update fs.rel
      (function
        | None -> None
        | Some l -> (
            match List.filter (fun f -> f != fs) l with
            | [] -> None
            | l -> Some l))
      ctl.by_rel

let move_state ctl ~old_owner ~old_rel ~new_owner ~new_rel =
  let mine owner = List.filter (fun fs -> fs.owner == owner) in
  let moved = mine old_owner (files_under ctl old_rel) in
  let replaced =
    mine new_owner (files_under ctl new_rel)
    |> List.filter (fun fs -> not (List.memq fs moved))
  in
  (* A replaced file leaves the name table but keeps its pending writes: a
     handle may still sync them, and a crash that loses the rename brings
     the file back under its name. *)
  List.iter (untrack ctl) replaced;
  List.iter
    (fun fs ->
      untrack ctl fs;
      let suffix =
        String.sub fs.rel (String.length old_rel)
          (String.length fs.rel - String.length old_rel)
      in
      fs.owner <- new_owner;
      fs.rel <- new_rel ^ suffix;
      track ctl fs)
    moved;
  List.iter
    (fun place ->
      if
        place.proot == old_owner
        && path_is_at_or_under ~parent:old_rel place.prefix
      then begin
        let suffix =
          String.sub place.prefix (String.length old_rel)
            (String.length place.prefix - String.length old_rel)
        in
        place.proot <- new_owner;
        place.prefix <- new_rel ^ suffix
      end)
    ctl.subtrees

(* After the crash nothing reaches the store: a killed process does nothing
   more, so every operation through the wrapper fails, a directory operation
   as a read or a write does. *)
let live d = if d.dctl.crashed then raise_power_lost ()

(* The inode at [p] before an operation takes it away. *)
let gone d p =
  match ino_at (as_admin d.dctl d.dplace.proot) p with
  | Some ino -> [ (p, ino) ]
  | None -> []

(* On macOS and Windows, the spelling of the entry [name] matches in [dir],
   so that two spellings of one name are one file to the model. *)
let spelling os d ~dir name =
  let (Base_dir ((module B), bv)) = as_admin d.dctl d.dplace.proot in
  let key = Low_level.name_key os name in
  match B.read_dir bv dir with
  | names -> (
      match
        List.find_opt
          (fun n -> String.equal (Low_level.name_key os n) key)
          names
      with
      | Some n -> n
      | None -> name)
  | exception Eio.Io _ -> name

(* A path from [d]'s root, resolved below the wrapped root. *)
let resolve ?follow_last d path =
  match d.dctl.base with
  | None -> path
  | Some (_, root) ->
      let spell =
        match d.dctl.os with
        | `Linux -> None
        | (`Macos | `Windows) as os -> Some (spelling os d)
      in
      resolve_at ?follow_last ?spell ~root d.dplace.proot path

(* The file an open for writing of [rel] meets: the one the model tracks
   under [key], with the open's mode, or a new one holding [prev], placed
   where a file found there sat. *)
let opened_file d ~key ~known ~rel ~perm ~prev found =
  match Hashtbl.find_opt d.dctl.files key with
  | Some fs ->
      if not (Int.equal fs.perm perm) then d.dctl.effects <- d.dctl.effects + 1;
      fs.perm <- perm;
      fs
  | None ->
      d.dctl.effects <- d.dctl.effects + 1;
      let fs =
        fresh_file
          ?held:(if known then None else d.dctl.held rel)
          ~owner:d.dplace.proot ~rel ~perm ~durable:prev ()
      in
      track d.dctl fs;
      Option.iter
        (fun (st : Eio.File.Stat.t) ->
          locate_preexisting d.dctl (fs, rel) ~ino:st.ino)
        found;
      fs

(* As [opened_file], with the mode the file has, [found]'s or the open's. *)
let opened_with_mode d ~key ~known ~rel ~perm ~prev found =
  let fs = opened_file d ~key ~known ~rel ~perm ~prev found in
  let mode =
    match found with Some st -> st.Eio.File.Stat.perm | None -> perm
  in
  if not (Int.equal fs.mode mode) then d.dctl.effects <- d.dctl.effects + 1;
  fs.mode <- mode;
  fs

module Cdir_impl = struct
  type t = cdir

  let open_in d ~sw ~follow path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    let rel = resolve d (at d.dplace.prefix path) in
    let fs = Hashtbl.find_opt d.dctl.files (file_key d.dplace.proot rel) in
    let file = B.open_in bv ~sw ~follow path in
    let dir =
      match Eio.File.stat file with
      | { kind = `Directory; ino; _ } -> Some ino
      | _ -> None
    in
    wrap_cfile_ro ~sw ?fs ?dir d.dctl file

  let open_out d ~sw ~follow ~append ~create path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    let spelt = at d.dplace.prefix path in
    let rel = resolve d spelt in
    adopt d.dctl [ rel ];
    let key = file_key d.dplace.proot rel in
    (* A dangling link names a file the open creates. *)
    let found =
      match B.stat bv ~follow:true path with
      | stat -> Some stat
      | exception Eio.Io _ -> None
    in
    let existed = Option.is_some found in
    (* A file's bytes are read once, when the model first meets it, before
       a truncating open loses them. *)
    let known = Hashtbl.mem d.dctl.files key in
    let prev =
      if known then Cstruct.empty
      else read_existing (as_admin d.dctl d.dplace.proot) rel
    in
    let had_bytes =
      match found with
      | Some { size; _ } -> Optint.Int63.compare size Optint.Int63.zero > 0
      | None -> false
    in
    let base = B.open_out bv ~sw ~follow ~append ~create path in
    let fs =
      opened_with_mode d ~key ~known ~rel ~perm:(perm_of create) ~prev found
    in
    (* An unsynced truncation of pre-existing durable data is itself a change
       that a crash may lose. *)
    if is_trunc create && had_bytes then clog_change d.dctl fs (Resize 0);
    if not existed then begin
      record_metadata d.dctl ~path:spelt ~kind:Create ~resolved:[ rel ]
    end;
    wrap_cfile_rw ~sw d.dctl fs ~append base

  (* A file of the base the caller holds, tracked as an open of [path] that
     creates and truncates nothing. The file it is is the one the caller
     holds, whatever [path] names now. *)
  let adopt_file d ~sw ~path file =
    live d;
    let rel = resolve d (at d.dplace.prefix path) in
    adopt d.dctl [ rel ];
    let key = file_key d.dplace.proot rel in
    let known = Hashtbl.mem d.dctl.files key in
    let prev =
      if known then Cstruct.empty
      else read_existing (as_admin d.dctl d.dplace.proot) rel
    in
    let base = Eio_fs_sync.adopt ~sw (d.dbase, path) file in
    let found = Some (Eio.File.stat base) in
    let fs =
      opened_with_mode d ~key ~known ~rel ~perm:(perm_of `Never) ~prev found
    in
    wrap_cfile_rw ~sw d.dctl fs ~append:false base

  let open_subtree d ~sw path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    let sub = B.open_subtree bv ~sw path in
    let ino = (B.stat bv ~follow:true path).ino in
    !dir_hook d.dctl ~ino:(Some ino) ~root:d.dplace.proot
      ~prefix:(resolve d (at d.dplace.prefix path))
      ~base:(sub :> Eio.Fs.dir_ty r)

  let stat d ~follow path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    B.stat bv ~follow path

  let mkdir d ~perm path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    let rel = at d.dplace.prefix path in
    let resolved = [ resolve ~follow_last:false d rel ] in
    adopt d.dctl resolved;
    B.mkdir bv ~perm path;
    record_metadata d.dctl ~path:rel ~kind:Mkdir ~resolved

  let read_dir d path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    B.read_dir bv path

  let with_dir_entries d path fn =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    B.with_dir_entries bv path fn

  let unlink d path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    let rel = at d.dplace.prefix path in
    let resolved = resolve ~follow_last:false d rel in
    adopt d.dctl [ resolved ];
    let gone = gone d resolved in
    B.unlink bv path;
    Option.iter (untrack d.dctl)
      (Hashtbl.find_opt d.dctl.files (file_key d.dplace.proot resolved));
    record_metadata d.dctl ~gone ~path:rel ~kind:Unlink ~resolved:[ resolved ]

  let rmdir d path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    let rel = at d.dplace.prefix path in
    let p = resolve ~follow_last:false d rel in
    adopt d.dctl [ p ];
    let gone = gone d p in
    B.rmdir bv path;
    record_metadata d.dctl ~gone ~path:rel ~kind:Rmdir ~resolved:[ p ]

  let rename_with exclusive d old_path new_dir new_path =
    live d;
    let (Eio.Resource.T (nv, nops)) = new_dir in
    let target_cdir =
      Option.map (fun f -> f nv) (Eio.Resource.get_opt nops Crash_dir)
    in
    let target =
      match target_cdir with
      | Some cd -> cd.dbase
      | None -> (new_dir :> Eio.Fs.dir_ty r)
    in
    let (Base_dir ((module B), bv)) = d.dboxed in
    let spelt = at d.dplace.prefix old_path in
    let old_rel = resolve ~follow_last:false d spelt in
    (* A crash-wrapped target rehomes the file root-relative, so its rewrite
       does not depend on the target subtree staying open. *)
    let owner, dest, rel =
      match target_cdir with
      | Some cd ->
          let dest = at cd.dplace.prefix new_path in
          (cd.dplace.proot, dest, resolve ~follow_last:false cd dest)
      | None -> (box_dir target, new_path, new_path)
    in
    adopt d.dctl
      (match target_cdir with
      | Some _ -> [ old_rel; rel ]
      | None -> [ old_rel ]);
    let gone = gone d old_rel in
    (* The node the rename replaces, if any. *)
    let victim = ino_at (as_admin d.dctl owner) rel in
    (match exclusive with
    | None -> B.rename bv old_path target new_path
    | Some backend ->
        Eio_fs_sync.Rename.exclusive backend (d.dbase, old_path)
          (target, new_path));
    (* A successful rename moves the durability identity with the file. A
       tracked target it replaced keeps its own, for a crash that loses the
       rename. *)
    (* A rename of an entry onto itself does nothing. *)
    if not (owner == d.dplace.proot && String.equal old_rel rel) then begin
      move_state d.dctl ~old_owner:d.dplace.proot ~old_rel ~new_owner:owner
        ~new_rel:rel;
      let kind = Rename { dest } in
      record_metadata d.dctl ~gone ?victim ~path:spelt ~kind
        ~resolved:[ old_rel; rel ]
    end

  let rename d old_path new_dir new_path =
    rename_with None d old_path new_dir new_path

  let rename_exclusive d backend ~source ~target:(new_dir, new_path) =
    rename_with (Some backend) d source new_dir new_path

  let read_link d path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    B.read_link bv path

  let symlink ~link_to d path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    let rel = at d.dplace.prefix path in
    let resolved = [ resolve ~follow_last:false d rel ] in
    adopt d.dctl resolved;
    B.symlink ~link_to bv path;
    let kind = Symlink { target = link_to } in
    record_metadata d.dctl ~path:rel ~kind ~resolved

  (* A change of mode or owner is journalled as a directory operation is. *)
  let attrs d ~follow path kind =
    let rel = at d.dplace.prefix path in
    let resolved = [ resolve ~follow_last:follow d rel ] in

    record_metadata d.dctl ~path:rel ~kind ~resolved

  let chmod d ~follow ~perm path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    adopt d.dctl [ resolve ~follow_last:follow d (at d.dplace.prefix path) ];
    B.chmod bv ~follow ~perm path;
    (* A tracked file's mode, to tell when a write clears setuid. *)
    (match
       Hashtbl.find_opt d.dctl.files
         (file_key d.dplace.proot
            (resolve ~follow_last:follow d (at d.dplace.prefix path)))
     with
    | Some fs -> fs.mode <- (B.stat bv ~follow path).perm
    | None -> ());
    attrs d ~follow path (Chmod { perm = perm land 0o7777 })

  let chown ~follow ?uid ?gid d path =
    live d;
    let (Base_dir ((module B), bv)) = d.dboxed in
    adopt d.dctl [ resolve ~follow_last:follow d (at d.dplace.prefix path) ];
    B.chown ~follow ?uid ?gid bv path;
    let st = B.stat bv ~follow path in
    attrs d ~follow path (Chown { uid = st.uid; gid = st.gid })

  (* A directory barrier commits the entries of the directory it names,
     exactly as the platform's directory fsync would, and the journal is
     asked root-relative so a barrier issued through a staging subtree still
     names the same directory. Through an opened subtree, a barrier on the
     subtree itself is on the inode it opened, even once removed or
     replaced: it commits that directory's own entries. After the power cut it fails like every other
     operation. *)
  let barrier ~full d path =
    live d;
    let upto = d.dctl.seq in
    d.dctl.pause ();
    live d;
    (* The base's barrier first: it refuses a directory that is missing or
       that the caller cannot read. *)
    (let (Base_dir ((module B), bv)) = d.dboxed in
     B.sync_dir bv path);
    match (d.dino, path_segments path) with
    | Some ino, [] -> commit d.dctl ~full (Commit.Dir { ino; upto })
    | _ ->
        let dir = resolve d (at d.dplace.prefix path) in
        Option.iter
          (fun ino -> commit d.dctl ~full (Commit.Dir { ino; upto }))
          (dir_ino d.dctl dir)

  let sync_dir d path = barrier ~full:false d path

  let native d path =
    let (Base_dir ((module B), bv)) = d.dboxed in
    B.native bv path

  let pp ppf d =
    let (Base_dir ((module B), bv)) = d.dboxed in
    B.pp ppf bv

  include Eio_utils.Posix_path
end

let rec cdir_handler :
    ( cdir,
      [ `Dir | `Close | `Crash_dir | `Dir_sync | `Process_local ] )
    Eio.Resource.handler
    Lazy.t =
  lazy
    (Eio.Resource.handler
       [
         H (Eio.Fs.Pi.Dir, (module Cdir_impl));
         H (Eio.Resource.Close, fun d -> Wrapper.close_resource d.dbase);
         H (Crash_dir, fun d -> d);
         H (Eio_fs_sync.Rename.Exclusive, Cdir_impl.rename_exclusive);
         (* Who can open these files is the base's to say: the model
            tracks what is written through it and adds no opener of its
            own. *)
         H
           ( Eio_fs_sync.Process_local,
             fun d -> Eio_fs_sync.is_process_local d.dbase );
         H (Eio_fs_sync.Modeless, fun d -> Eio_fs_sync.is_modeless d.dbase);
         H
           ( Eio_fs_sync.Dir_sync,
             fun d ~path ~full -> Cdir_impl.barrier ~full d path );
         H (Eio_fs_sync.Adopt, Cdir_impl.adopt_file);
         (* The same place in the model, over another base. *)
         H
           ( Eio_fs_internal.Rebase,
             fun d f ->
               let base = (f d.dbase :> Eio.Fs.dir_ty r) in
               (Eio.Resource.T
                  ( { d with dbase = base; dboxed = box_dir base },
                    Lazy.force cdir_handler )
                 :> [ `Close | Eio.Fs.dir_ty ] r) );
       ])

let wrap_cdir ctl ~ino ~root ~prefix ~base : [ `Close | Eio.Fs.dir_ty ] r =
  let place = { proot = root; prefix } in
  ctl.subtrees <- place :: ctl.subtrees;
  (Eio.Resource.T
     ( {
         dctl = ctl;
         dbase = base;
         dboxed = box_dir base;
         dplace = place;
         dino = ino;
       },
       Lazy.force cdir_handler )
    :> [ `Close | Eio.Fs.dir_ty ] r)

let () = dir_hook := wrap_cdir

let v ?os ?(policy = Drop_unsynced) ?(outside = Dropped) ?(block_size = 4096)
    ?(pause = ignore) () =
  if block_size <= 0 then
    Fmt.invalid_arg
      "Eio_fs_chaos.Private.Crash.v: block_size is %d, must be positive"
      block_size;
  let os = match os with Some (#Eio_chaos.os as os) -> os | None -> `Linux in
  {
    os;
    queued = [];
    policy;
    outside;
    block_size;
    base = None;
    files = Hashtbl.create 8;
    known = Hashtbl.create 8;
    log = [];
    journal = Journal.v ();
    snapshot = None;
    snapped = Inos.empty;
    left = [];
    durable = None;
    by_rel = Paths.empty;
    held = (fun _ -> None);
    set_times = (fun _ _ -> ());
    root_box = None;
    admin = None;
    subtrees = [];
    seq = 0;
    effects = 0;
    logged = 0;
    pending_changes = 0;
    opened = Hashtbl.create 8;
    file_inos = Hashtbl.create 8;
    pause;
    crashed = false;
    image = [];
  }

let wrap ctl (base, path) =
  (* One controller, one wrapper: a second would reset the durable image
     and split the files' state between the two. *)
  if Option.is_some ctl.base then
    invalid_arg
      "Eio_fs_chaos.Power.wrap: t already wraps a directory, and a controller \
       tracks one";
  let base = (base :> Eio.Fs.dir_ty r) in
  ctl.base <- Some (base, path);
  let proot = box_dir base in
  let admin =
    match Eio_fs_internal.admin base with
    | Some admin -> box_dir admin
    | None -> proot
  in
  ctl.root_box <- Some proot;
  ctl.admin <- Some admin;
  ctl.held <- (fun rel -> Memfs.held (base, rel));
  ctl.set_times <-
    (fun rel t ->
      Memfs.set_times (base, rel) ~atime:t.atime ~mtime:t.mtime ~ctime:t.ctime);
  snap ctl (state_of (snapshot_at admin path) no_locs);
  (wrap_cdir ctl ~ino:None ~root:proot ~prefix:"" ~base, path)

let is_crashed ctl = ctl.crashed

let sync_metadata ctl =
  if ctl.crashed then raise_power_lost ();
  commit_metadata ctl

let post_crash ctl =
  cut ctl;
  Option.get ctl.base

let left ctl = ctl.left
let seq ctl = ctl.seq
let stamp ctl = ctl.seq + ctl.effects
let cached ctl = ctl.queued <> []

let pp_unsynced ppf u =
  match u.kind with
  | Data -> Fmt.pf ppf "%s @%d+%d #%d" u.path u.file_offset u.length u.seq
  | Truncate -> Fmt.pf ppf "truncate %s to %d #%d" u.path u.file_offset u.seq
  | Create -> Fmt.pf ppf "create %s #%d" u.path u.seq
  | Mkdir -> Fmt.pf ppf "mkdir %s #%d" u.path u.seq
  | Unlink -> Fmt.pf ppf "unlink %s #%d" u.path u.seq
  | Rmdir -> Fmt.pf ppf "rmdir %s #%d" u.path u.seq
  | Symlink { target } -> Fmt.pf ppf "symlink %s -> %s #%d" u.path target u.seq
  | Rename { dest } -> Fmt.pf ppf "rename %s -> %s #%d" u.path dest u.seq
  | Chmod { perm } -> Fmt.pf ppf "chmod %s %o #%d" u.path perm u.seq
  | Chown { uid; gid } -> Fmt.pf ppf "chown %s %Ld:%Ld #%d" u.path uid gid u.seq

let image ctl = List.rev ctl.image
