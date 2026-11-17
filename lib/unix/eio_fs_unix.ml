include Lock

external darwin_full_fsync : Unix.file_descr -> unit
  = "caml_eio_fs_darwin_full_fsync"

external pread_native : Unix.file_descr -> int -> bytes -> int -> int -> int
  = "caml_eio_fs_pread"

external pread_cached_native :
  Unix.file_descr -> int -> bytes -> int -> int -> int
  = "caml_eio_fs_pread_cached"

external pwrite_native : Unix.file_descr -> int -> string -> int -> int -> int
  = "caml_eio_fs_pwrite"

module type POSITIONED = sig
  type t

  val pread :
    t -> cached:bool -> file_offset:int -> bytes -> pos:int -> len:int -> int

  val pwrite : t -> file_offset:int -> string -> pos:int -> len:int -> int
end

type (_, _, _) Eio.Resource.pi +=
  | Positioned :
      ( 't,
        (module POSITIONED with type t = 't),
        [> `Positioned ] )
      Eio.Resource.pi

(* The most a positioned call moves, as the native calls bound it. *)
let positioned_max = 65536

let check_span name ~file_offset ~length ~pos ~len =
  if file_offset < 0 then
    Fmt.invalid_arg "%s: file_offset is %d, must not be negative" name
      file_offset;
  if pos < 0 || len < 0 || pos > length - len then
    Fmt.invalid_arg "%s: pos is %d and len is %d, outside the %d-byte buffer"
      name pos len length

let on_descriptor name fd f =
  eio_unix_errors @@ fun () -> Eio_unix.Fd.use_exn name fd f

let pread ?(cached = false) (file : _ Eio.File.ro) ~file_offset buf ~pos ~len =
  check_span "Eio_fs_unix.pread" ~file_offset ~length:(Bytes.length buf) ~pos
    ~len;
  let (Eio.Resource.T (t, ops)) = file in
  match Eio.Resource.get_opt ops Positioned with
  | Some (module P) -> P.pread t ~cached ~file_offset buf ~pos ~len
  | None -> (
      match Eio_unix.Resource.fd_opt file with
      | Some fd ->
          on_descriptor "Eio_fs_unix.pread" fd @@ fun fd ->
          if cached then pread_cached_native fd file_offset buf pos len
          else pread_native fd file_offset buf pos len
      | None when len = 0 -> 0
      | None -> (
          let c = Cstruct.create (min len positioned_max) in
          match
            Eio.File.pread file
              ~file_offset:(Optint.Int63.of_int file_offset)
              [ c ]
          with
          | n ->
              Cstruct.blit_to_bytes c 0 buf pos n;
              n
          | exception End_of_file -> 0))

let pwrite (file : _ Eio.File.rw) ~file_offset buf ~pos ~len =
  check_span "Eio_fs_unix.pwrite" ~file_offset ~length:(String.length buf) ~pos
    ~len;
  let (Eio.Resource.T (t, ops)) = file in
  match Eio.Resource.get_opt ops Positioned with
  | Some (module P) -> P.pwrite t ~file_offset buf ~pos ~len
  | None -> (
      match Eio_unix.Resource.fd_opt file with
      | Some fd ->
          on_descriptor "Eio_fs_unix.pwrite" fd @@ fun fd ->
          pwrite_native fd file_offset buf pos len
      | None when len = 0 -> 0
      | None ->
          Eio.File.pwrite_single file
            ~file_offset:(Optint.Int63.of_int file_offset)
            [ Cstruct.of_string buf ~off:pos ~len:(min len positioned_max) ])

let full_fsync fd =
  if not (is_darwin ()) then retry_eintr Unix.fsync fd
  else
    match retry_eintr darwin_full_fsync fd with
    | () -> ()
    | exception
        Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.EINVAL | Unix.ENOTTY), _, _) ->
        (* Some filesystems cannot flush a drive cache. Fall back to the
           strongest barrier that volume offers, matching SQLite's practice. *)
        retry_eintr Unix.fsync fd

type statvfs = {
  block_size : int64;
  total_blocks : int64;
  free_blocks : int64;
  available_blocks : int64;
}

(* [fn dir name] on the directory [path] is in, opened through the capability
   [path] carries, and on [path]'s last component in it: ["."] when [path]
   names that directory itself. Eio resolves the directory as it resolves any
   path, so a confined capability refuses a [..] above it, an absolute path and
   a symbolic link leading out, and [fn] never follows [name]. A native name
   would reach whatever the string names from the process's own root. *)
let with_entry ~label (path : _ Eio.Path.t) fn =
  let path = (path :> Eio.Fs.dir_ty Eio.Path.t) in
  if String.contains (snd path) '\000' then
    Fmt.invalid_arg "Eio_fs_unix.%s: path is %S, which holds a NUL byte" label
      (snd path);
  let dir, name =
    match Eio.Path.split path with
    | Some (dir, name) when name <> "." && name <> ".." -> (dir, name)
    | _ -> (path, ".")
  in
  Eio.Switch.run @@ fun sw ->
  let opened = Eio.Path.open_dir ~sw dir in
  match Eio_unix.Resource.fd_opt (Eio.Path.open_in ~sw opened) with
  | None ->
      raise (Eio.Fs.err (Eio.Fs.Not_native (Fmt.to_to_string Eio.Path.pp path)))
  | Some fd -> Eio_unix.Fd.use_exn label fd @@ fun fd -> fn fd name

external statvfs_at : Unix.file_descr -> string -> statvfs
  = "caml_eio_fs_statvfsat"

let statvfs path =
  with_entry ~label:"statvfs" path @@ fun dir name ->
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-statvfs" @@ fun () ->
  statvfs_at dir name

external volume_used_at : Unix.file_descr -> string -> int64
  = "caml_eio_fs_volume_used_at"

let volume_used path =
  with_entry ~label:"volume_used" path @@ fun dir name ->
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-volume-used" @@ fun () ->
  volume_used_at dir name

external clone_at :
  Unix.file_descr -> string -> Unix.file_descr -> string -> bool
  = "caml_eio_fs_clone_at"

let clone_tree src dst =
  with_entry ~label:"clone_tree" src @@ fun src_dir src_name ->
  with_entry ~label:"clone_tree" dst @@ fun dst_dir dst_name ->
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-clone-tree" @@ fun () ->
  clone_at src_dir src_name dst_dir dst_name

let clone_trees src dst names =
  let undo =
    List.iter (fun n ->
        try Eio.Path.rmtree ~missing_ok:true Eio.Path.(dst / n)
        with Eio.Io _ -> ())
  in
  let rec go cloned = function
    | [] -> true
    | n :: rest -> (
        match
          Option.iter
            (fun (parent, _) ->
              Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent)
            (Eio.Path.split Eio.Path.(dst / n));
          clone_tree Eio.Path.(src / n) Eio.Path.(dst / n)
        with
        | true -> go (n :: cloned) rest
        | false | (exception Eio.Io _) ->
            undo (n :: cloned);
            false)
  in
  go [] names

type extent = { logical : int; physical : int; length : int }

external seek_data_native : Unix.file_descr -> int -> int option
  = "caml_eio_fs_seek_data"

external seek_hole_native : Unix.file_descr -> int -> int option
  = "caml_eio_fs_seek_hole"

external extents_native : Unix.file_descr -> int -> int -> int -> extent list
  = "caml_eio_fs_extents"

(* Each runs on a system thread whose stub releases the runtime system around
   the syscall. The hop has a price, which is why the extent map takes a batch:
   one hop per batch covers many extents where one hop each would spend the
   time in scheduling alone. *)
let seek_data fd ~file_offset =
  Eio_unix.run_in_systhread ~label:"eio-fs-seek-data" @@ fun () ->
  seek_data_native fd file_offset

let seek_hole fd ~file_offset =
  Eio_unix.run_in_systhread ~label:"eio-fs-seek-hole" @@ fun () ->
  seek_hole_native fd file_offset

let extents fd ~file_offset ~length ~max =
  Eio_unix.run_in_systhread ~label:"eio-fs-extents" @@ fun () ->
  extents_native fd file_offset length max

(* A registered barrier keeps its own semantics. A plain POSIX directory is
   opened through the path's capability without following a final link, so a
   name a link has taken is refused, and pushed through to stable storage off
   the calling fiber's thread. A handle with no descriptor takes the portable
   barrier. *)
let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match Eio_fs_sync.dir_barrier res with
  | Some barrier -> barrier ~path ~full:true
  | None -> (
      Eio.Switch.run @@ fun sw ->
      let handle = Eio.Path.open_in ~sw ~follow:false p in
      match Eio_unix.Resource.fd_opt handle with
      | None -> Eio.Path.sync_dir p
      | Some fd ->
          Eio_unix.Fd.use_exn "eio-fs-sync-dir" fd @@ fun fd ->
          eio_unix_errors @@ fun () ->
          Eio_unix.run_in_systhread ~label:"eio-fs-sync-dir" @@ fun () ->
          full_fsync fd)

(* The registration is asked before the descriptor: a descriptor a wrapper
   carries over is a second way past the wrapper, so a barrier taken on it is
   one the wrapper never saw. A file opened for reading only, as a published
   generation's are, carries its descriptor all the same; one with neither a
   registration nor a descriptor takes the portable barrier. *)
let sync_file (Eio.Resource.T (v, ops) as file) =
  match Eio_fs_sync.file_barrier file with
  | Some barrier -> barrier ~full:true
  | None -> (
      match Eio_unix.Resource.fd_opt file with
      | None -> (
          match Eio.Resource.get_opt ops Eio.File.Pi.Write with
          | Some (module W) -> W.sync v
          | None ->
              invalid_arg
                "Eio_fs_unix.sync_file: file has no descriptor, no sync and no \
                 Eio_fs.Sync.File_sync barrier")
      | Some fd ->
          Eio_unix.Fd.use_exn "eio-fs-sync-file" fd @@ fun fd ->
          eio_unix_errors @@ fun () ->
          Eio_unix.run_in_systhread ~label:"eio-fs-sync-file" @@ fun () ->
          full_fsync fd)

module Full_barrier = struct
  type t = unit

  let sync_file () file = sync_file file
  let sync_dir () dir = sync_dir dir
end

let barrier =
  let module B = Eio_fs_sync.Barrier.Make (Full_barrier) in
  B.v ()

(* The directory beneath any wrapper of this package in front of [dir]. A
   wrapper hands its base to this package alone, through an interface no
   other library can name. *)
let rec unwrapped_dir dir =
  match Eio_fs_internal.base_dir dir with
  | Some base -> unwrapped_dir base
  | None -> dir

external rename_exclusive_at :
  Unix.file_descr -> string -> Unix.file_descr -> string -> unit
  = "caml_eio_fs_rename_exclusive"

module Exclusive_rename = struct
  type t = unit

  let parent path =
    match Eio.Path.split path with
    | Some (parent, name) when name <> "" && name <> "." && name <> ".." ->
        (parent, name)
    | _ ->
        Fmt.invalid_arg
          "Eio_fs_unix.exclusive_rename: path is %a, which ends in no file name"
          Eio.Path.pp path

  let descriptor path file =
    match Eio_unix.Resource.fd_opt file with
    | Some fd -> fd
    | None ->
        Fmt.invalid_arg
          "Eio_fs_unix.exclusive_rename: path is %a, in a directory with no \
           Unix descriptor"
          Eio.Path.pp path

  let rename () source target =
    let source_parent, source_name = parent source in
    let target_parent, target_name = parent target in
    Eio.Switch.run @@ fun sw ->
    (* Resolve each parent through its capability once, then keep that open
       directory as the authority for the atomic operation on its basename. *)
    let source_dir = Eio.Path.open_dir ~sw source_parent in
    let target_dir = Eio.Path.open_dir ~sw target_parent in
    let source_file = Eio.Path.open_in ~sw source_dir in
    let target_file = Eio.Path.open_in ~sw target_dir in
    let source_fd = descriptor source source_file in
    let target_fd = descriptor target target_file in
    eio_unix_errors @@ fun () ->
    Eio_unix.Fd.use_exn "exclusive rename source" source_fd @@ fun source_fd ->
    Eio_unix.Fd.use_exn "exclusive rename target" target_fd @@ fun target_fd ->
    Eio_unix.run_in_systhread ~label:"exclusive rename" @@ fun () ->
    rename_exclusive_at source_fd source_name target_fd target_name
end

module Rename_backend = Eio_fs_sync.Rename.Make (Exclusive_rename)

let exclusive_rename = Rename_backend.v ()
let inline_fs = Inline.fs

type regular = Regular.t = { contents : string; perm : Eio.File.Unix_perm.t }

let read_regular = Regular.read

(* {1 Extended attributes} *)

external listxattr_at : Unix.file_descr -> string -> string
  = "caml_eio_fs_listxattrat"

external getxattr_at : Unix.file_descr -> string -> string -> string
  = "caml_eio_fs_getxattrat"

external setxattr_at : Unix.file_descr -> string -> string -> string -> unit
  = "caml_eio_fs_setxattrat"

(* The kernel lists names as a run of NUL-terminated strings. *)
let names_of_list s =
  String.split_on_char '\000' s |> List.filter (fun n -> n <> "")

external enoattr : unit -> int = "caml_eio_fs_enoattr" [@@noalloc]

let enoattr = enoattr ()

(* A name removed between the list and the get is dropped. *)
let xattrs path =
  with_entry ~label:"xattrs" path @@ fun dir entry ->
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-xattrs" @@ fun () ->
  names_of_list (listxattr_at dir entry)
  |> List.sort String.compare
  |> List.filter_map (fun name ->
      match getxattr_at dir entry name with
      | v -> Some (name, v)
      | exception Unix.Unix_error (Unix.EUNKNOWNERR n, _, _) when n = enoattr ->
          None)

let set_xattr path name v =
  with_entry ~label:"set_xattr" path @@ fun dir entry ->
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-set-xattr" @@ fun () ->
  setxattr_at dir entry name v

module Batch = struct
  external create_native : Unix.file_descr -> string -> int -> string -> unit
    = "caml_eio_fs_batch_create"

  external unlink_native : Unix.file_descr -> string -> bool -> unit
    = "caml_eio_fs_batch_unlink"

  external rename_native :
    Unix.file_descr -> string -> Unix.file_descr -> string -> unit
    = "caml_eio_fs_batch_rename"

  external link_native :
    Unix.file_descr -> string -> Unix.file_descr -> string -> unit
    = "caml_eio_fs_batch_link"

  type t =
    | Native of Eio_unix.Fd.t
    | Portable of Eio.Fs.dir_ty Eio.Path.t * bool ref

  let v ~sw (dir : [> Eio.Fs.dir_ty ] Eio.Path.t) =
    let dir = (dir :> Eio.Fs.dir_ty Eio.Path.t) in
    let opened =
      match dir with
      | root, "" -> fst (Eio.Path.open_dir ~sw Eio.Path.(of_dir root / "."))
      | _ -> fst (Eio.Path.open_dir ~sw dir)
    in
    let file = Eio.Path.open_in ~sw (Eio.Path.of_dir (unwrapped_dir opened)) in
    match Eio_unix.Resource.fd_opt file with
    | Some fd -> Native fd
    | None ->
        let live = ref true in
        Eio.Switch.on_release sw (fun () -> live := false);
        Portable (Eio.Path.of_dir (opened :> Eio.Fs.dir_ty Eio.Resource.t), live)

  type op =
    | Mkdir of { path : string; perm : int }
    | Create of { path : string; perm : int; contents : string }
    | Symlink of { path : string; target : string }
    | Unlink of { path : string; dir : bool }
    | Rename of { src : string; dst : string }
    | Link of { src : string; dst : string }
    | Lstat of string

  type outcome = Done | Stat of Eio.File.Stat.t
  type error = [ `Refused of string | `Unix of Unix.error * string * string ]

  let pp_error ppf = function
    | `Refused reason -> Fmt.pf ppf "refused: %s" reason
    | `Unix (code, call, arg) ->
        Fmt.pf ppf "%s %s: %s" call arg (Unix.error_message code)

  let hop_count = Atomic.make 0
  let hops () = Atomic.get hop_count

  let hop fn =
    Atomic.incr hop_count;
    Eio_unix.run_in_systhread ~label:"eio-fs-batch" fn

  exception Refused of string

  let fail_refused fmt = Fmt.kstr (fun reason -> raise (Refused reason)) fmt

  (* The parent components and the last one, or a refusal for any path that
     could name something outside the root. *)
  let split path =
    match Eio_fs_internal.beneath path with
    | Ok parts -> parts
    | Error reason -> raise (Refused reason)

  (* The directories one run has opened, by their path beneath the root. A
     rename or a directory removal may move or delete any of them, so either
     empties the table. *)
  type run = {
    root : Unix.file_descr;
    dirs : (string, Unix.file_descr) Hashtbl.t;
  }

  let forget run =
    Hashtbl.iter (fun _ fd -> Unix.close fd) run.dirs;
    Hashtbl.reset run.dirs

  (* [parent run path] is the directory holding [path]'s last component and
     that component, opened one component at a time with O_NOFOLLOW. *)
  let parent run path =
    let parents, leaf = split path in
    let step (fd, prefix) name =
      let key = if String.equal prefix "" then name else prefix ^ "/" ^ name in
      match Hashtbl.find_opt run.dirs key with
      | Some fd -> (fd, key)
      | None -> (
          match At.open_dir fd name with
          | next ->
              Hashtbl.add run.dirs key next;
              (next, key)
          | exception
              (Unix.Unix_error ((Unix.ELOOP | Unix.ENOTDIR | Unix.EMLINK), _, _)
               as e) -> (
              match At.lstat fd name with
              | { mode; _ }
                when At.kind_of_mode (Int64.to_int mode) = `Symbolic_link ->
                  fail_refused "%S is a symbolic link" key
              | _ | (exception Unix.Unix_error _) -> raise e))
    in
    let fd, _ = List.fold_left step (run.root, "") parents in
    (fd, leaf)

  let perform run = function
    | Mkdir { path; perm } ->
        let dir, leaf = parent run path in
        At.mkdir dir leaf perm;
        Done
    | Create { path; perm; contents } ->
        let dir, leaf = parent run path in
        create_native dir leaf perm contents;
        Done
    | Symlink { path; target } ->
        let dir, leaf = parent run path in
        At.symlink target dir leaf;
        Done
    | Unlink { path; dir = is_dir } ->
        let dir, leaf = parent run path in
        unlink_native dir leaf is_dir;
        if is_dir then forget run;
        Done
    | Rename { src; dst } ->
        let src_dir, src_leaf = parent run src in
        let dst_dir, dst_leaf = parent run dst in
        rename_native src_dir src_leaf dst_dir dst_leaf;
        forget run;
        Done
    | Link { src; dst } ->
        let src_dir, src_leaf = parent run src in
        let dst_dir, dst_leaf = parent run dst in
        link_native src_dir src_leaf dst_dir dst_leaf;
        Done
    | Lstat path ->
        let dir, leaf = parent run path in
        Stat (At.stat_of_raw (At.lstat dir leaf))

  let attempt run op =
    match perform run op with
    | outcome -> Ok outcome
    | exception Refused reason -> Error (`Refused reason)
    | exception Unix.Unix_error (code, call, arg) ->
        Error (`Unix (code, call, arg))

  let portable root = function
    | Mkdir { path; perm } ->
        Eio_fs.Nofollow.with_parent
          Eio.Path.(root / path)
          (Eio.Path.mkdir ~perm);
        Done
    | Create { path; perm; contents } ->
        Eio_fs.Nofollow.with_parent
          Eio.Path.(root / path)
          (fun p ->
            Eio.Path.save ~follow:false ~create:(`Exclusive perm) p contents);
        Done
    | Symlink { path; target } ->
        Eio_fs.Nofollow.with_parent
          Eio.Path.(root / path)
          (Eio.Path.symlink ~link_to:target);
        Done
    | Unlink { path; dir } ->
        Eio_fs.Nofollow.with_parent
          Eio.Path.(root / path)
          (fun p ->
            if dir then Eio.Path.rmdir p
            else if Eio.Path.kind ~follow:false p = `Directory then
              raise (Unix.Unix_error (Unix.EISDIR, "unlink", path))
            else Eio.Path.unlink p);
        Done
    | Rename { src; dst } ->
        Eio_fs.Nofollow.with_parent
          Eio.Path.(root / src)
          (fun src ->
            Eio_fs.Nofollow.with_parent
              Eio.Path.(root / dst)
              (Eio.Path.rename src));
        Done
    | Link _ -> fail_refused "hard links require a native directory"
    | Lstat path ->
        Eio_fs.Nofollow.with_parent
          Eio.Path.(root / path)
          (fun p -> Stat (Eio.Path.stat ~follow:false p))

  let portable_attempt root op =
    match portable root op with
    | outcome -> Ok outcome
    | exception Refused reason -> Error (`Refused reason)
    | exception Unix.Unix_error (code, call, arg) ->
        Error (`Unix (code, call, arg))
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) ->
        Error (`Unix (Unix.EEXIST, "batch", ""))
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
        Error (`Unix (Unix.ENOENT, "batch", ""))
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
        Error (`Unix (Unix.EACCES, "batch", ""))
    | exception Eio.Io (err, _) ->
        Error (`Refused (Fmt.to_to_string Eio.Exn.pp_err err))

  let run t ops =
    match (t, ops) with
    | _, [] -> []
    | Portable (_, live), _ when not !live ->
        invalid_arg
          "Eio_fs_unix.Batch.run: t's switch has finished, which closed its \
           directories"
    | Portable (root, _), ops -> List.map (portable_attempt root) ops
    | Native fd, ops ->
        Eio_unix.Fd.use_exn "Eio_fs_unix.Batch.run" fd @@ fun root ->
        hop @@ fun () ->
        let run = { root; dirs = Hashtbl.create 16 } in
        Fun.protect
          ~finally:(fun () -> forget run)
          (fun () ->
            List.rev
              (List.fold_left (fun acc op -> attempt run op :: acc) [] ops))
end

type removed = { entries : int; bytes : int; refused : string list }

(* Four hops at once make most of the gain in a tree's time: eight spend half
   as much system time again on the volume's locks. *)
let remove_workers = 4

(* A removal under way: a directory goes in hops of its own, its entries
   stated in one and unlinked in the next, so a tree costs a few hops per
   directory whatever each holds, and the hops of a tree's directories run at
   once, [workers] at a time. A cancellation ends it at the next hop. *)
type removal = {
  root : Eio.Fs.dir_ty Eio.Path.t;
  grant : bool;
  workers : Eio.Semaphore.t;
  mutable entries : int;
  mutable bytes : int;
  mutable refusals : string list;  (** Newest first. *)
}

let refuse r p why =
  r.refusals <- Fmt.str "remove %a: %s" Eio.Path.pp p why :: r.refusals;
  false

let refuse_io r p err = refuse r p (Fmt.to_to_string Eio.Exn.pp_err err)

let hop r f =
  Eio.Semaphore.acquire r.workers;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release r.workers) f

let counted r size =
  r.entries <- r.entries + 1;
  r.bytes <- r.bytes + size

let granted r p perm =
  if r.grant && perm land 0o700 <> 0o700 then
    Eio.Path.chmod ~follow:false ~perm:(perm lor 0o700) p

(* Whether the failure of [rel] is an entry gone meanwhile; one that is not
   is refused. *)
let gone r rel = function
  | `Unix (Unix.ENOENT, _, _) -> true
  | e -> refuse r Eio.Path.(r.root / rel) (Fmt.to_to_string Batch.pp_error e)

(* The directories and the other entries of [paths], stated in one hop, and
   whether no statement was refused. *)
let stated r b paths =
  let stats =
    hop r @@ fun () -> Batch.run b (List.map (fun p -> Batch.Lstat p) paths)
  in
  List.fold_left2
    (fun (dirs, files, clear) p -> function
      | Ok (Batch.Stat { kind = `Directory; perm; _ }) ->
          ((p, perm) :: dirs, files, clear)
      | Ok (Batch.Stat { size; _ }) ->
          (dirs, (p, Optint.Int63.to_int size) :: files, clear)
      | Ok Batch.Done -> (dirs, files, clear)
      | Error e -> (dirs, files, gone r p e && clear))
    ([], [], true) paths stats

(* Whether [files], each with its size, went, in one hop. *)
let unlinked r b files =
  let unlinks =
    hop r @@ fun () ->
    Batch.run b
      (List.map (fun (p, _) -> Batch.Unlink { path = p; dir = false }) files)
  in
  List.fold_left2
    (fun clear (p, size) -> function
      | Ok _ ->
          counted r size;
          clear
      | Error e -> gone r p e && clear)
    true files unlinks

let removed_dir r b rel =
  match
    hop r @@ fun () -> Batch.run b [ Batch.Unlink { path = rel; dir = true } ]
  with
  | [ Ok _ ] ->
      counted r 0;
      true
  | [ Error e ] -> gone r rel e
  | l ->
      Fmt.invalid_arg
        "Eio_fs_unix.rmtree: Batch.run answered %d results, not one per \
         operation"
        (List.length l)

(* Whether nothing is left of the entries [paths]: those that are no directory
   first, then the others, all at once. *)
let rec removed_entries r b paths =
  let dirs, files, clear = stated r b paths in
  let clear = unlinked r b files && clear in
  List.for_all Fun.id (Eio.Fiber.List.map (removed_tree r b) dirs) && clear

and removed_tree r b (rel, perm) =
  let dir = Eio.Path.(r.root / rel) in
  match
    granted r dir perm;
    hop r @@ fun () -> Eio.Path.read_dir dir
  with
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> true
  | exception Eio.Io (err, _) -> refuse_io r dir err
  | names ->
      removed_entries r b (List.map (fun n -> rel ^ "/" ^ n) names)
      && removed_dir r b rel

let rmtree ?(grant = false) ?last (path : [> Eio.Fs.dir_ty ] Eio.Path.t) =
  let root = (path :> Eio.Fs.dir_ty Eio.Path.t) in
  let r =
    {
      root;
      grant;
      workers = Eio.Semaphore.make remove_workers;
      entries = 0;
      bytes = 0;
      refusals = [];
    }
  in
  let contents sw =
    let b = Batch.v ~sw root in
    let first, last =
      List.partition (fun n -> Some n <> last) (Eio.Path.read_dir root)
    in
    removed_entries r b first && removed_entries r b last
  in
  (match
     match Eio.Path.stat ~follow:false root with
     | { kind = `Directory; perm; _ } ->
         granted r root perm;
         if Eio.Switch.run contents then (
           Eio.Path.rmdir root;
           counted r 0)
     | { size; _ } ->
         Eio.Path.unlink root;
         counted r (Optint.Int63.to_int size)
   with
  | () | (exception Eio.Io (Eio.Fs.E (Not_found _), _)) -> ()
  | exception Eio.Io (err, _) -> ignore (refuse_io r root err));
  { entries = r.entries; bytes = r.bytes; refused = List.rev r.refusals }

module Private = struct
  let batch_hops = Batch.hops
end
