external is_darwin : unit -> bool = "caml_eio_fs_is_darwin"

(* A call that takes an [Eio.Path.t] or an [Eio.File.t] reports its failure as
   Eio's own backends do: the [Unix.Unix_error] beneath becomes [Eio.Io]. *)
let eio_unix_errors fn =
  match fn () with
  | value -> value
  | exception Unix.Unix_error (code, operation, argument) ->
      raise (Eio_unix.Err.v code operation argument)

type flock_mode = Exclusive | Shared

external flock_native : Unix.file_descr -> flock_mode -> bool -> unit
  = "caml_eio_fs_flock"

external flock_unlock_native : Unix.file_descr -> unit
  = "caml_eio_fs_flock_unlock"

external darwin_full_fsync : Unix.file_descr -> unit
  = "caml_eio_fs_darwin_full_fsync"

external record_lock_holder : Unix.file_descr -> int
  = "caml_eio_fs_record_lock_holder"

external lock_range_native : Unix.file_descr -> int -> int -> int -> unit
  = "caml_eio_fs_lock_range"

external range_locked_native : Unix.file_descr -> int -> int -> bool
  = "caml_eio_fs_range_locked"

external range_holder_native : Unix.file_descr -> int -> int -> int
  = "caml_eio_fs_range_holder"

let lock_range fd kind ~file_offset ~length =
  let kind = match kind with `Read -> 0 | `Write -> 1 | `Unlock -> 2 in
  lock_range_native fd kind file_offset length

let range_locked fd ~file_offset ~length =
  range_locked_native fd file_offset length

let range_holder fd ~file_offset ~length =
  match range_holder_native fd file_offset length with
  | -1 -> `Free
  | 0 -> `Held None
  | pid -> `Held (Some pid)

external pread_head : Unix.file_descr -> int -> string
  = "caml_eio_fs_pread_head"

external pread_native : Unix.file_descr -> int -> bytes -> int -> int -> int
  = "caml_eio_fs_pread"

let pread fd ~file_offset buf ~pos ~len =
  if file_offset < 0 || pos < 0 || len < 0 || pos > Bytes.length buf - len then
    invalid_arg "Eio_fs_unix.pread";
  pread_native fd file_offset buf pos len

type record_lock_wait

external record_lock_wait_start :
  Unix.file_descr -> Unix.file_descr -> bool -> record_lock_wait
  = "caml_eio_fs_record_lock_wait_start"

external record_lock_wait_end : record_lock_wait -> bool -> bool
  = "caml_eio_fs_record_lock_wait_end"

let rec retry_eintr f fd =
  match f fd with
  | () -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> retry_eintr f fd

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

external statvfs_native : string -> statvfs = "caml_eio_fs_statvfs"

let statvfs path =
  let native = Eio.Path.native_exn path in
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-statvfs" @@ fun () ->
  statvfs_native native

external clone_tree_native : string -> string -> bool = "caml_eio_fs_clone_tree"

let clone_tree src dst =
  let src = Eio.Path.native_exn src and dst = Eio.Path.native_exn dst in
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-clone-tree" @@ fun () ->
  clone_tree_native src dst

let rec grant_owner p =
  match Eio.Path.stat ~follow:false p with
  | { kind = `Directory; perm; _ } -> (
      (if perm land 0o700 <> 0o700 then
         try Eio.Path.chmod ~follow:false ~perm:(perm lor 0o700) p
         with Eio.Io _ -> ());
      match Eio.Path.read_dir p with
      | names -> List.iter (fun n -> grant_owner Eio.Path.(p / n)) names
      | exception Eio.Io _ -> ())
  | _ | (exception Eio.Io _) -> ()

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

(* A registered barrier keeps its own semantics; a plain POSIX directory is
   opened through the capability the path carries, without following a final
   link -- so a name a link has taken is refused rather than fsynced instead of
   the caller's directory -- and pushed through to stable storage off the
   calling fiber's thread. A handle with no descriptor takes the portable
   barrier. *)
let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match Eio_fs_sync.dir_barrier res with
  | Some barrier -> barrier ~path
  | None -> (
      Eio.Switch.run @@ fun sw ->
      let handle = Eio.Path.open_in ~sw ~follow:false p in
      match Eio_unix.Resource.fd_opt handle with
      | None -> Eio_fs_sync.sync_dir p
      | Some fd ->
          Eio_unix.Fd.use_exn "eio-fs-sync-dir" fd @@ fun fd ->
          eio_unix_errors @@ fun () ->
          Eio_unix.run_in_systhread ~label:"eio-fs-sync-dir" @@ fun () ->
          full_fsync fd)

(* The registration is asked before the descriptor: a descriptor a wrapper
   carries over is a second way past the wrapper, so a barrier taken on it is
   one the wrapper never saw. A file with neither a registration nor a
   descriptor takes the portable barrier, its own sync. *)
let sync_file_or ~portable file =
  match Eio_fs_sync.file_barrier file with
  | Some barrier -> barrier ()
  | None -> (
      match Eio_unix.Resource.fd_opt file with
      | None -> portable file
      | Some fd ->
          Eio_unix.Fd.use_exn "eio-fs-sync-file" fd @@ fun fd ->
          eio_unix_errors @@ fun () ->
          Eio_unix.run_in_systhread ~label:"eio-fs-sync-file" @@ fun () ->
          full_fsync fd)

let sync_file file = sync_file_or ~portable:Eio_fs_sync.sync_file file

(* The pair a publication issues at full platform strength. A file opened for
   reading only, as a published generation's are, carries its descriptor all
   the same; one with neither a registration nor a descriptor takes the
   portable barrier. *)
module Full_barrier = struct
  type t = unit

  let sync_file () file =
    sync_file_or
      ~portable:(Eio_fs_sync.Barrier.sync_file Eio_fs_sync.Barrier.portable)
      file

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

(* The host descriptor beneath [file], through any wrapper of this package in
   front of it: the descriptor a lock is taken on. *)
let rec file_fd : 'a. 'a Eio.Resource.t -> Eio_unix.Fd.t option =
 fun file ->
  match Eio_unix.Resource.fd_opt file with
  | Some _ as fd -> fd
  | None -> Option.bind (Eio_fs_internal.base_file file) file_fd

external rename_exclusive_at :
  Unix.file_descr -> string -> Unix.file_descr -> string -> unit
  = "caml_eio_fs_rename_exclusive"

(* The backend's own file over the host's descriptor, as it opens files
   itself: the descriptor is its resource's, tagged [`Unix_fd], so
   {!sync_file} and {!Eio_unix.Resource.fd_opt} reach it. *)
let import_file ~sw ~close_unix fd = Eio_unix.File.import_rw ~sw ~close_unix fd

module Exclusive_rename = struct
  type t = unit

  let parent path =
    match Eio.Path.split path with
    | Some (parent, name) when name <> "" && name <> "." && name <> ".." ->
        (parent, name)
    | _ -> invalid_arg "Eio_fs_unix.exclusive_rename: expected a file name"

  let descriptor file =
    match Eio_unix.Resource.fd_opt file with
    | Some fd -> fd
    | None ->
        invalid_arg
          "Eio_fs_unix.exclusive_rename: unsupported directory backend"

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
    let source_fd = descriptor source_file in
    let target_fd = descriptor target_file in
    eio_unix_errors @@ fun () ->
    Eio_unix.Fd.use_exn "exclusive rename source" source_fd @@ fun source_fd ->
    Eio_unix.Fd.use_exn "exclusive rename target" target_fd @@ fun target_fd ->
    Eio_unix.run_in_systhread ~label:"exclusive rename" @@ fun () ->
    rename_exclusive_at source_fd source_name target_fd target_name
end

module Rename_backend = Eio_fs_sync.Rename.Make (Exclusive_rename)

let exclusive_rename = Rename_backend.v ()

(* System calls on descriptors and beneath directory descriptors, each the
   one call its name says, with the runtime released around it. They block, so
   a fiber makes them from a system thread; they perform no effect, so any Eio
   backend runs them. *)
module At = struct
  type stat = {
    dev : int64;
    ino : int64;
    mode : int64;
    nlink : int64;
    uid : int64;
    gid : int64;
    rdev : int64;
    size : int64;
    blksize : int64;
    blocks : int64;
    atime : float;
    mtime : float;
    ctime : float;
  }

  (* [openat(O_RDONLY | O_DIRECTORY | O_NOFOLLOW)]. *)
  external open_dir : Unix.file_descr -> string -> Unix.file_descr
    = "caml_eio_fs_batch_open_dir"

  (* [openat(O_RDONLY | O_NOFOLLOW | O_NONBLOCK)]. *)
  external open_leaf : Unix.file_descr -> string -> Unix.file_descr
    = "caml_eio_fs_open_leaf"

  external mkdir : Unix.file_descr -> string -> int -> unit
    = "caml_eio_fs_batch_mkdir"

  external symlink : string -> Unix.file_descr -> string -> unit
    = "caml_eio_fs_batch_symlink"

  external read_link : Unix.file_descr -> string -> string
    = "caml_eio_fs_readlinkat"

  (* [fstatat(AT_SYMLINK_NOFOLLOW)]. *)
  external lstat : Unix.file_descr -> string -> stat = "caml_eio_fs_batch_lstat"

  (* [fstat], its times to the nanosecond as Eio's own stat has them. *)
  external fstat : Unix.file_descr -> stat = "caml_eio_fs_fstat"

  let kind_of_mode mode =
    match mode land 0o170000 with
    | 0o040000 -> `Directory
    | 0o100000 -> `Regular_file
    | 0o120000 -> `Symbolic_link
    | 0o010000 -> `Fifo
    | 0o020000 -> `Character_special
    | 0o060000 -> `Block_device
    | 0o140000 -> `Socket
    | _ -> `Unknown

  let to_stat (r : stat) : Eio.File.Stat.t =
    let mode = Int64.to_int r.mode in
    {
      dev = r.dev;
      ino = r.ino;
      kind = kind_of_mode mode;
      perm = mode land 0o7777;
      nlink = r.nlink;
      uid = r.uid;
      gid = r.gid;
      rdev = r.rdev;
      size = Optint.Int63.of_int64 r.size;
      blksize = r.blksize;
      blocks = r.blocks;
      atime = r.atime;
      mtime = r.mtime;
      ctime = r.ctime;
    }
end

(* Why a [with_parent] refused a path: it names nothing beneath its
   directory, or a parent on the way is not a directory it may descend. *)
type Eio.Exn.Backend.t += Unsafe_parent of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Unsafe_parent reason ->
        Fmt.string f reason;
        true
    | _ -> false)

let unsafe_parent reason =
  raise (Eio.Fs.err (Eio.Fs.Permission_denied (Unsafe_parent reason)))

module Link = struct
  type t = { parent : Eio_unix.Fd.t; name : string; path : string }
  type publication = Created | Present

  let err_refused path reason =
    Fmt.kstr
      (fun message -> Error message)
      "cannot publish symbolic link %s: %s; repair or remove the conflicting \
       entry before retrying"
      path reason

  let components path =
    if Eio_fs_path.is_abs path then Error "expected a relative path"
    else
      match Eio_fs_path.canonical ("/" ^ path) with
      | Ok parts -> Ok parts
      | Error _ -> Error "expected a non-empty canonical path without NUL"

  (* [fn] applied to [dir]'s descriptor on a system thread, [dir] kept open
     meanwhile. *)
  let at dir fn =
    Eio_unix.Fd.use_exn "Eio_fs_unix.Link" dir @@ fun dir ->
    Eio_unix.run_in_systhread ~label:"eio-fs-link" (fun () -> fn dir)

  (* A missing parent is made as [create] says, and one another caller made
     meanwhile is opened all the same. *)
  let rec open_parent ~create fd name =
    match at fd (fun dir -> At.open_dir dir name) with
    | opened -> opened
    | exception (Unix.Unix_error (Unix.ENOENT, _, _) as e) -> (
        match create with
        | `Never -> raise e
        | `If_missing perm ->
            (try at fd (fun dir -> At.mkdir dir name perm)
             with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
            open_parent ~create:`Never fd name)

  let with_parent ?(create = `Never) (input : _ Eio.Path.t) fn =
    let dir, path = (input :> Eio.Fs.dir_ty Eio.Path.t) in
    let original = Eio_fs.Path.name input in
    let components =
      match components path with
      | Ok components -> components
      | Error reason ->
          Fmt.kstr unsafe_parent "%s: %s; name a path beneath the directory"
            original reason
    in
    Eio.Switch.run @@ fun sw ->
    let rec descend fd = function
      | [] -> assert false
      | [ name ] -> { parent = fd; name; path = original }
      | name :: rest ->
          let next = open_parent ~create fd name in
          descend (Eio_unix.Fd.of_unix ~sw ~close_unix:true next) rest
    in
    let parent =
      try
        let directory = Eio.Path.open_dir ~sw (dir, ".") in
        let descriptor =
          match Eio_unix.Resource.fd_opt (Eio.Path.open_in ~sw directory) with
          | Some fd -> fd
          | None ->
              invalid_arg
                "Eio_fs_unix.Link.with_parent: native directory required"
        in
        eio_unix_errors (fun () -> descend descriptor components)
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "opening the parents of %s" original
    in
    fn parent

  let publish t ~target =
    let existing () =
      let stat = at t.parent (fun dir -> At.lstat dir t.name) in
      match At.kind_of_mode (Int64.to_int stat.mode) with
      | `Symbolic_link ->
          let held = at t.parent (fun dir -> At.read_link dir t.name) in
          if String.equal held target then Ok Present
          else
            err_refused t.path
              (Fmt.str "symbolic link points at %S, expected %S" held target)
      | `Regular_file -> err_refused t.path "regular file already exists"
      | `Directory -> err_refused t.path "directory already exists"
      | _ -> err_refused t.path "foreign filesystem entry already exists"
    in
    if String.equal target "" || String.contains target '\000' then
      err_refused t.path "target must be non-empty and contain no NUL"
    else
      match
        match at t.parent (fun dir -> At.symlink target dir t.name) with
        | () -> Ok Created
        | exception Unix.Unix_error (Unix.EEXIST, _, _) -> existing ()
      with
      | result -> result
      | exception Unix.Unix_error (code, operation, argument) ->
          err_refused t.path
            (Fmt.str "%s %s: %s" operation argument (Unix.error_message code))
      | exception (Eio.Io _ as ex) -> err_refused t.path (Printexc.to_string ex)

  let rec read_into fd (buffer : Cstruct.t) =
    match Unix.read_bigarray fd buffer.buffer buffer.off buffer.len with
    | got -> got
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_into fd buffer

  (* The leaf is opened, weighed and read on system threads, one per call, so
     a cancel is seen between reads; it is closed before the result. *)
  let read_regular ~max_bytes t =
    if max_bytes < 0 || max_bytes = max_int then
      invalid_arg "Eio_fs_unix.Link.read_regular: invalid byte bound";
    let err_read fmt = Fmt.kstr (fun message -> Error message) fmt in
    let opened =
      match at t.parent (fun dir -> At.open_leaf dir t.name) with
      | fd -> Ok (Some fd)
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
      | exception Unix.Unix_error (code, operation, argument) ->
          err_read "%s: %s %s: %s" t.path operation argument
            (Unix.error_message code)
      | exception (Eio.Io _ as ex) ->
          err_read "%s: %s" t.path (Printexc.to_string ex)
    in
    match opened with
    | Error _ as error -> error
    | Ok None -> Ok None
    | Ok (Some fd) -> (
        Fun.protect ~finally:(fun () ->
            try Unix.close fd with Unix.Unix_error _ -> ())
        @@ fun () ->
        let in_systhread fn =
          eio_unix_errors @@ fun () ->
          Eio_unix.run_in_systhread ~label:"eio-fs-link-read" fn
        in
        let stat = At.to_stat (in_systhread (fun () -> At.fstat fd)) in
        let size = Optint.Int63.to_int64 stat.size in
        if stat.kind <> `Regular_file then
          err_read "%s is not a regular file" t.path
        else if size < 0L || size > Int64.of_int max_bytes then
          err_read "%s is too large to read" t.path
        else
          match
            Eio_fs.Nofollow.run ~max_bytes:(Int64.to_int size)
              ~validate:(fun _ -> Ok ())
              ~stat
              ~read:(fun buffer -> in_systhread (fun () -> read_into fd buffer))
          with
          | Ok bytes -> Ok (Some (Cstruct.to_string bytes))
          | Error `Too_large -> err_read "%s changed size while read" t.path
          | Error (`Invalid reason) -> err_read "%s: %s" t.path reason)
end

type refusal = { path : string; holder : int option; record : string }

exception Lock_unavailable of refusal

(* What the holder wrote: the record's first line that is not blank. *)
let pp_doing f record =
  match
    List.find_opt
      (fun l -> String.trim l <> "")
      (String.split_on_char '\n' record)
  with
  | Some line -> Fmt.pf f ": %s" (String.trim line)
  | None -> ()

(* POSIX record locks belong to a process, so the kernel never names this
   process as the holder: this process is named only for a fiber's hold. *)
let pp_refusal f { path; holder; record } =
  match holder with
  | Some pid when pid = Unix.getpid () ->
      Fmt.pf f "%s is held by another fiber of this process%a" path pp_doing
        record
  | Some pid -> Fmt.pf f "%s is held by process %d" path pid
  | None -> Fmt.pf f "%s is held" path

let () =
  Printexc.register_printer (function
    | Lock_unavailable refusal ->
        Fmt.kstr Option.some "Eio_fs_unix.Lock_unavailable: %a" pp_refusal
          refusal
    | _ -> None)

(* The holder's record is read without the lock, so it is bounded: a record
   longer than a diagnostic is cut, not a cost the asker pays in full. *)
let record_bound = 4096

let read_record file =
  let buf = Cstruct.create record_bound in
  let rec fill off =
    if off >= record_bound then off
    else
      match
        Eio.File.pread file ~file_offset:(Optint.Int63.of_int off)
          [ Cstruct.shift buf off ]
      with
      | 0 -> off
      | got -> fill (off + got)
      | exception End_of_file -> off
  in
  match fill 0 with
  | got -> Cstruct.to_string ~len:got buf
  | exception Eio.Io _ -> ""

(* The holder's record read through [file], or through [unix], the Unix
   descriptor an ask keeps open with [Eio_unix.Fd.use]: an Eio read would take
   a use of its own, which a close by the holder refuses even while the ask's
   use keeps the descriptor open. *)
let read_held_record file = function
  | None -> read_record file
  | Some unix -> (
      try pread_head unix record_bound with Unix.Unix_error _ -> "")

(* A fiber holder is this process; another process's hold is named by the
   kernel, and [None] means it let go before it could be asked. *)
let refusal ~name ~holder file =
  Lock_unavailable { path = name; holder; record = read_record file }

let held_by_a_fiber () = Some (Unix.getpid ())

let held_by_a_process fd =
  match record_lock_holder fd with
  | 0 -> None
  | pid -> Some pid
  | exception Unix.Unix_error _ -> None

(* The fiber exclusion is keyed by device and inode, not by a name: two names
   can reach one file (a hard link), and POSIX record locks do not exclude
   within a process. The file stays open while its key is in use, so no inode
   number is reused under the table. *)
type lock_key = int64 * int64
(** The device and inode a record lock is held on. *)

type local_lock = {
  mutex : Eio.Mutex.t;
  mutable users : int;
  mutable readers : int;
  changed : Eio.Condition.t;
  mutable holding : bool;
      (** A fiber holds [mutex] and may hold the record lock. *)
  mutable parked : Unix.file_descr list;
      (** Descriptors on the file that other callers let go of while it was
          held, closed by the holder once its record lock is released. *)
  mutable file : Eio.File.ro_ty Eio.Resource.t option;
      (** The holder's own descriptor on the file, once it has opened it: an ask
          from this process reads it rather than opening one of its own. *)
}

let local_locks : (lock_key, local_lock) Hashtbl.t = Hashtbl.create 16
let entry_gates : (string, local_lock) Hashtbl.t = Hashtbl.create 16
let tables_mutex = Stdlib.Mutex.create ()

(* A mutex per key, kept only while a caller uses it. *)
let shared_mutex table key =
  Stdlib.Mutex.protect tables_mutex @@ fun () ->
  match Hashtbl.find_opt table key with
  | Some lock ->
      lock.users <- lock.users + 1;
      lock
  | None ->
      let lock =
        {
          mutex = Eio.Mutex.create ();
          users = 1;
          readers = 0;
          changed = Eio.Condition.create ();
          holding = false;
          parked = [];
          file = None;
        }
      in
      Hashtbl.add table key lock;
      lock

let done_with_shared_mutex table key lock =
  Stdlib.Mutex.protect tables_mutex @@ fun () ->
  lock.users <- lock.users - 1;
  if lock.users = 0 then Hashtbl.remove table key

let use_shared_mutex table key fn =
  let lock = shared_mutex table key in
  Fun.protect
    ~finally:(fun () -> done_with_shared_mutex table key lock)
    (fun () -> fn lock)

let use_local_lock (key : lock_key) fn = use_shared_mutex local_locks key fn

(* POSIX drops every record lock a process holds on a file when the process
   closes any descriptor for it, so a caller that leaves without the fiber
   mutex -- refused, out of patience, cancelled -- never closes its descriptor
   while a fiber of this process holds the file: it hands the descriptor to
   that holder, which closes it once its own record lock is released. *)
let let_go_of local file =
  match file_fd file with
  | None -> ()
  | Some fd -> (
      let park = Stdlib.Mutex.protect tables_mutex @@ fun () -> local.holding in
      if park then
        Eio.Cancel.protect @@ fun () ->
        match Eio_unix.Fd.remove fd with
        | None -> ()
        | Some unix_fd ->
            let closes_now =
              Stdlib.Mutex.protect tables_mutex @@ fun () ->
              if local.holding then begin
                local.parked <- unix_fd :: local.parked;
                false
              end
              else true
            in
            if closes_now then Unix.close unix_fd)

(* The holder's side: set once the mutex is taken, and cleared, with every
   parked descriptor closed, once the record lock is let go. *)
let start_holding local =
  Stdlib.Mutex.protect tables_mutex (fun () -> local.holding <- true)

let stop_holding local =
  let parked =
    Stdlib.Mutex.protect tables_mutex @@ fun () ->
    let parked = local.parked in
    local.holding <- false;
    local.parked <- [];
    local.file <- None;
    parked
  in
  List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) parked

let set_held_file local file =
  Stdlib.Mutex.protect tables_mutex (fun () -> local.file <- file)

(* [fn file fd] over the holder's descriptor, kept open for the call, with
   [fd] its Unix descriptor when it has one; [otherwise ()] when no fiber
   holds the file with a descriptor open, or the holder let go of it. [fn]
   reads through [fd] when there is one ({!read_held_record}), since the
   holder may close [file] during the call. An ask from this process answered
   here opens nothing, so it leaves nothing parked with the holder, however
   long the holder holds. *)
let with_held_file local ~otherwise fn =
  let file =
    Stdlib.Mutex.protect tables_mutex @@ fun () ->
    if local.holding then local.file else None
  in
  match file with
  | None -> otherwise ()
  | Some file -> (
      match file_fd file with
      | None -> fn file None
      | Some fd ->
          Eio_unix.Fd.use fd ~if_closed:otherwise (fun unix ->
              fn file (Some unix)))

(* A fiber of this process holds the file: its record is read through the
   holder's descriptor. *)
let refused_by_a_fiber ~name local =
  let record =
    with_held_file local ~otherwise:(fun () -> "") read_held_record
  in
  Lock_unavailable { path = name; holder = held_by_a_fiber (); record }

(* One budget covers both halves of the exclusion, so it is a moment both watch
   for rather than a duration each is handed. A daemon of the switch sleeps it
   on the scheduler's monotonic clock, from the first wait: an acquisition that
   never waits starts no timer. *)
type patience =
  | Wait  (** wait for the holder, however long it holds *)
  | Refuse  (** answer at once *)
  | Until of unit Eio.Promise.t Lazy.t  (** wait until the budget lapses *)

let budget ~sw seconds =
  Until
    (lazy
      (let lapsed, set_lapsed = Eio.Promise.create () in
       Eio.Fiber.fork_daemon ~sw (fun () ->
           Eio_unix.sleep seconds;
           Eio.Promise.resolve set_lapsed ();
           `Stop_daemon);
       lapsed))

(* [on_taken] runs the moment the mutex is held, so a caller records how to
   give it back before anything else can raise. A wait behind a holder, bounded
   or not, calls [on_wait ()] first. *)
let take_available ?(on_wait = ignore) ~patience ~refused ~on_taken ~try_take
    ~wait () =
  match patience with
  | Wait ->
      if not (try_take ()) then begin
        on_wait ();
        wait ()
      end;
      on_taken ()
  | Refuse -> if try_take () then on_taken () else raise (refused ())
  | Until _ when try_take () -> on_taken ()
  | Until lapsed ->
      on_wait ();
      (* [Fiber.first] cancels the loser, and a fiber cancelled inside
         [Eio.Mutex.lock] may have been granted the mutex all the same: the
         unlocker's wake and the cancellation race for one atomic and either
         can win. So what was taken is remembered rather than read off which
         fiber came back, and a mutex granted as the bound lapsed is held --
         and given back by the caller -- rather than dropped with nobody left
         to unlock it. *)
      let taken = ref false in
      let granted =
        Eio.Fiber.first
          (fun () ->
            wait ();
            taken := true;
            true)
          (fun () ->
            Eio.Promise.await (Lazy.force lapsed);
            false)
      in
      if granted || !taken then on_taken () else raise (refused ())

let take_mutex ?on_wait ~patience ~refused ~on_taken mutex =
  take_available ?on_wait ~patience ~refused ~on_taken
    ~try_take:(fun () -> Eio.Mutex.try_lock mutex)
    ~wait:(fun () -> Eio.Mutex.lock mutex)
    ()

let unlock_local local =
  Eio.Mutex.unlock local.mutex;
  Eio.Condition.broadcast local.changed

(* Readers share the existing inode gate, but each takes its own kernel lock.
   The gate excludes record-lock users too, whose locks are process-owned. *)
let take_shared_local ~patience ~refused ~on_wait ~on_taken local =
  let try_take () =
    let acquired =
      Stdlib.Mutex.protect tables_mutex @@ fun () ->
      if local.readers > 0 || Eio.Mutex.try_lock local.mutex then begin
        local.readers <- local.readers + 1;
        true
      end
      else false
    in
    if acquired then Eio.Condition.broadcast local.changed;
    acquired
  in
  let wait () =
    Eio.Condition.loop_no_mutex local.changed (fun () ->
        if try_take () then Some () else None)
  in
  take_available ~on_wait ~patience ~refused ~try_take ~wait
    ~on_taken:(fun () ->
      let active = Atomic.make true in
      on_taken (fun () ->
          if Atomic.exchange active false then begin
            let last =
              Stdlib.Mutex.protect tables_mutex @@ fun () ->
              local.readers <- local.readers - 1;
              local.readers = 0
            in
            if last then unlock_local local
          end))
    ()

(* Entering is queued too, on a mutex keyed by the caller's name: the stat or
   the open that keys the file suspends, so without this the order fibers
   reach the file's mutex is the order those came back in. The gate is held
   only across that, since a name is not a file. *)
let in_entry_order name fn =
  use_shared_mutex entry_gates name @@ fun { mutex; _ } ->
  let entered = ref false in
  Fun.protect
    ~finally:(fun () -> if !entered then Eio.Mutex.unlock mutex)
    (fun () ->
      Eio.Mutex.lock mutex;
      entered := true;
      fn ())

let lockf fd command = retry_eintr (fun fd -> Unix.lockf fd command 0) fd

let os_unlock fd =
  Eio.Cancel.protect @@ fun () ->
  Eio_unix.Fd.use_exn "Eio_fs_unix.unlock" fd @@ fun fd ->
  Eio_unix.run_in_systhread ~label:"eio-fs-unlock" (fun () ->
      lockf fd Unix.F_ULOCK)

(* The kernel's blocking record lock over [fd], which answers when the holder
   lets go: [true] once granted. The request waits on a thread of its own
   (record_lock_wait_start), which writes to a pipe when answered, so the
   fiber waits on the pipe and a cancel reaches it; the cancel then withdraws
   the request before the fiber goes, and a lock granted in that instant is
   let go. *)
let await_record_lock ?(mode = `Exclusive) fd =
  let shared = match mode with `Shared -> true | `Exclusive -> false in
  let answered, notify = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () ->
      Unix.close answered;
      Unix.close notify)
  @@ fun () ->
  let wait = record_lock_wait_start fd notify shared in
  match Eio_unix.await_readable answered with
  | () -> ignore (record_lock_wait_end wait false : bool)
  | exception (Eio.Cancel.Cancelled _ as e) ->
      if record_lock_wait_end wait true then lockf fd Unix.F_ULOCK;
      raise e

(* [on_taken] runs the moment the record lock is held, as {!take_mutex}'s
   does. A wait asks once without waiting, calls [on_wait] if the lock is
   held, then makes the kernel's blocking request; a bounded wait races that
   request against its lapse, which withdraws it. *)
let take_os_lock ~patience ~refused ~on_taken ~on_wait fd =
  let acquired = ref false in
  let ask () =
    Eio.Cancel.protect (fun () ->
        Eio_unix.Fd.use_exn "Eio_fs_unix.lock" fd @@ fun fd ->
        match
          Eio_unix.run_in_systhread ~label:"eio-fs-lock" (fun () ->
              lockf fd Unix.F_TLOCK)
        with
        | () ->
            acquired := true;
            on_taken ()
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> ())
  in
  let wait ~lapsed ~granted =
    ask ();
    if (not !acquired) && not (lapsed ()) then begin
      on_wait ();
      Eio_unix.Fd.use_exn "Eio_fs_unix.lock" fd @@ fun fd ->
      if granted fd then begin
        acquired := true;
        on_taken ()
      end
    end
  in
  (match patience with
  | Wait ->
      wait
        ~lapsed:(fun () -> false)
        ~granted:(fun fd ->
          await_record_lock fd;
          true)
  | Refuse -> ask ()
  | Until lapse ->
      wait
        ~lapsed:(fun () -> Eio.Promise.is_resolved (Lazy.force lapse))
        ~granted:(fun fd ->
          Eio.Fiber.first
            (fun () ->
              await_record_lock fd;
              true)
            (fun () ->
              Eio.Promise.await (Lazy.force lapse);
              false)));
  if not !acquired then raise (refused ())

(* A planted link under the rendezvous name would send the record lock to
   whatever it names. Creating is an exclusive open (O_CREAT|O_EXCL refuses an
   existing name, a dangling link included), and the opened descriptor's device
   and inode are then weighed against the name's own. Weighing the name before
   the open is only an early refusal. *)

type Eio.Exn.Backend.t += Not_a_lock_file of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Not_a_lock_file path ->
        Fmt.pf f "Not_a_lock_file %S" path;
        true
    | _ -> false)

let not_a_lock_file name =
  raise (Eio.Fs.err (Eio.Fs.Permission_denied (Not_a_lock_file name)))

(* A backend with no native path is named as it prints itself. *)
let lock_name path =
  match Eio.Path.native path with
  | Some native -> native
  | None -> Fmt.str "%a" Eio.Path.pp path

let is_regular : Eio.File.Stat.kind -> bool = function
  | `Regular_file -> true
  | _ -> false

(* An absent name is the ordinary first acquisition: [create] decides whether
   the open makes one, and the descriptor is weighed either way. *)
let named_kind path =
  match Eio.Path.stat ~follow:false path with
  | stat -> Some stat
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let check_lock_name ~name path =
  match named_kind path with
  | None -> ()
  | Some stat -> if not (is_regular stat.kind) then not_a_lock_file name

(* The device and inode of the file the name holds, refusing a name that holds
   no regular file; [None] while it holds nothing. *)
let named_key ~name path =
  match named_kind path with
  | None -> None
  | Some stat ->
      if not (is_regular stat.kind) then not_a_lock_file name;
      Some (stat.dev, stat.ino)

(* The file opened is the regular file the name holds. *)
let check_same_file ~name ~(opened : Eio.File.Stat.t) (named : Eio.File.Stat.t)
    =
  if
    (not (is_regular opened.kind))
    || (not (is_regular named.kind))
    || Int64.compare opened.dev named.dev <> 0
    || Int64.compare opened.ino named.ino <> 0
  then not_a_lock_file name

let check_locked_file ~name ~opened path =
  match named_kind path with
  | None -> not_a_lock_file name
  | Some named -> check_same_file ~name ~opened named

let update_lock_contents file contents =
  ignore (Eio.File.seek file Optint.Int63.zero `Set : Optint.Int63.t);
  Eio.Flow.copy_string contents file;
  Eio.File.truncate file (Optint.Int63.of_int (String.length contents))

(* An absent rendezvous is created exclusively, so the kernel refuses the name
   a link holds rather than following it; an existing one is opened without
   creating and its identity settled by [check_locked_file]. *)
let open_lock_file ~sw ~name
    ~(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ]) path =
  match create with
  | `Never -> Eio.Path.open_out ~sw ~create:`Never path
  | `If_missing perm -> (
      match Eio.Path.open_out ~sw ~create:(`Exclusive perm) path with
      | file -> file
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> (
          match Eio.Path.open_out ~sw ~create:`Never path with
          | file -> file
          | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
              not_a_lock_file name))

(* What is held once the exclusion is taken, whichever locks carry it: the
   descriptor is weighed against the name it was opened by, the holder's
   diagnostics are written through it, and the caller runs. *)
let settle ~name ~opened ~contents ~file path =
  check_locked_file ~name ~opened path;
  Option.iter (update_lock_contents file) contents

let block_patience ~sw = function
  | `Yes -> (Wait, Wait)
  | `No -> (Refuse, Refuse)
  | `Fibers -> (Wait, Refuse)
  | `For seconds ->
      let until = budget ~sw seconds in
      (until, until)

(* An acquisition keeps how to give back each thing it took, the latest
   first. Letting go runs them in that order, and a refusal part-way gives
   back what was already taken. *)
let let_go steps =
  Eio.Cancel.protect @@ fun () ->
  eio_unix_errors @@ fun () ->
  let taken = !steps in
  steps := [];
  List.iter (fun give_back -> give_back ()) taken

(* The name was given to another file between its key and its open. *)
exception Renamed

(* The key of the file the name holds, read before anything is opened; a
   name that holds nothing yet is opened (or created) first and keyed by its
   descriptor, which is returned with it. *)
let key_of_name ~taken ~name path open_rendezvous =
  in_entry_order name @@ fun () ->
  match named_key ~name path with
  | Some key -> (key, None)
  | None ->
      let file, (opened : Eio.File.Stat.t) = open_rendezvous () in
      taken (fun () -> Eio.Resource.close file);
      ((opened.dev, opened.ino), Some (file, opened))

(* The rendezvous opened once the mutex is held, which must be the file
   [key] names: a name given to another file in between raises [Renamed]. *)
let open_keyed ~taken ~key open_rendezvous =
  let file, (opened : Eio.File.Stat.t) = open_rendezvous () in
  let opened_key = (opened.dev, opened.ino) in
  if opened_key <> key then (
    (* Another fiber of this process may hold the file now named. *)
    use_local_lock opened_key (fun other -> let_go_of other file);
    Eio.Resource.close file;
    raise_notrace Renamed);
  taken (fun () -> Eio.Resource.close file);
  (file, opened)

module Flock = struct
  let rec acquire ~shared fd ~nonblocking =
    match
      flock_native fd (if shared then Shared else Exclusive) nonblocking
    with
    | () -> ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        acquire ~shared fd ~nonblocking

  let flock ~shared fd ~nonblocking =
    Eio_unix.run_in_systhread ~label:"eio-fs-lease-lock" (fun () ->
        acquire ~shared fd ~nonblocking)

  (* The kernel's blocking [flock], which answers when the holder lets go, on a
     thread of its own: a request in the kernel cannot be withdrawn, so a
     cancelled caller leaves it to that thread and answers at once. The thread
     asks through a duplicate of [fd], which shares its open file description
     and so the lock the kernel grants, and closes it once answered: a lock
     granted after the cancel goes when the caller's [fd] closes too. *)
  let await_release ~shared fd =
    let copy = Unix.dup ~cloexec:true fd in
    let answer, set_answer = Eio.Promise.create () in
    let _ : Thread.t =
      Thread.create
        (fun () ->
          let granted =
            match acquire ~shared copy ~nonblocking:false with
            | () -> Ok ()
            | exception (Unix.Unix_error _ as e) -> Error e
          in
          Unix.close copy;
          Eio.Promise.resolve set_answer granted)
        ()
    in
    match Eio.Promise.await answer with Ok () -> () | Error e -> raise e

  let use_os_lock ~shared ~patience ~refused ~on_wait fd fn =
    let acquired = ref false in
    let ask ~nonblocking =
      Eio.Cancel.protect @@ fun () ->
      match flock ~shared fd ~nonblocking with
      | () -> acquired := true
      | exception
          Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EACCES), _, _)
        ->
          ()
    in
    (match patience with
    | Wait ->
        ask ~nonblocking:true;
        if not !acquired then begin
          on_wait ();
          await_release ~shared fd;
          acquired := true
        end
    | Refuse -> ask ~nonblocking:true
    | Until lapsed ->
        ask ~nonblocking:true;
        if (not !acquired) && not (Eio.Promise.is_resolved (Lazy.force lapsed))
        then begin
          on_wait ();
          acquired :=
            Eio.Fiber.first
              (fun () ->
                await_release ~shared fd;
                true)
              (fun () ->
                Eio.Promise.await (Lazy.force lapsed);
                false)
        end);
    if not !acquired then raise (refused ());
    fn ()
end

(* The record lock on the rendezvous's descriptor. A rendezvous with none has
   no record lock to take, which only a process-local backend allows. *)
let take_record_lock ~taken ~patience ~on_wait ~name file path =
  match file_fd file with
  | Some fd ->
      (* Darwin's record locks already conflict with BSD flock; taking both
         on one descriptor would conflict with our own lock. Linux keeps the
         two families independent, so its record-lock owner takes both. *)
      if not (is_darwin ()) then
        Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd (fun raw_fd ->
            Flock.use_os_lock ~shared:false ~patience
              ~refused:(fun () ->
                refusal ~name ~holder:(held_by_a_process raw_fd) file)
              ~on_wait raw_fd
              (fun () ->
                taken (fun () ->
                    Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock.unlock" fd
                      flock_unlock_native)));
      take_os_lock ~patience
        ~refused:(fun () ->
          let holder =
            Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd held_by_a_process
          in
          refusal ~name ~holder file)
        ~on_taken:(fun () -> taken (fun () -> os_unlock fd))
        ~on_wait fd
  | None ->
      if not (Eio_fs_sync.is_process_local (fst path)) then
        Fmt.invalid_arg
          "Eio_fs_unix.with_lock: %s: the rendezvous carries no Unix file \
           descriptor and its backend registers no Eio_fs_sync.Process_local"
          name

(* The mutex is keyed by the file the name holds, read before anything is
   opened, so an ask that waits for or is refused by a fiber of this process
   opens no descriptor to leave with the holder: POSIX drops the record lock
   on any close, so such a descriptor could only be parked until the hold
   ends. The rendezvous is opened once the mutex is held, on [sw], which
   closes it, and a name given to another file in between is keyed again. A
   name that holds nothing yet is opened (or created) first and keyed by its
   descriptor. A rendezvous with no descriptor has no record lock to take: a
   process-local backend makes the mutex the whole exclusion, and any other is
   refused. [block] is answered per half: the mutex is held by a fiber, the
   record lock by another process; its bound is kept on a switch of its own,
   which ends with the acquisition. *)
let take_inode_gate ~taken ~shared ~patience ~on_wait ~name ~opened_first local
    =
  let release_reader = ref ignore in
  let entered = ref false in
  Option.iter
    (fun (file, _) ->
      taken (fun () -> if not !entered then let_go_of local file))
    opened_first;
  let refused () =
    match opened_first with
    | Some (file, _) -> refusal ~name ~holder:(held_by_a_fiber ()) file
    | None -> refused_by_a_fiber ~name local
  in
  if shared then
    take_shared_local ~patience ~refused ~on_wait local
      ~on_taken:(fun release ->
        entered := true;
        release_reader := release;
        taken release)
  else begin
    take_mutex ~on_wait ~patience ~refused
      ~on_taken:(fun () ->
        entered := true;
        taken (fun () -> unlock_local local))
      local.mutex;
    start_holding local;
    taken (fun () -> stop_holding local)
  end;
  !release_reader

let take_lease_lock ~shared ~patience ~on_wait ~name file =
  match file_fd file with
  | Some fd ->
      Eio_unix.Fd.use_exn "Eio_fs_unix.Lease.acquire" fd @@ fun raw_fd ->
      Flock.use_os_lock ~shared ~patience
        ~refused:(fun () -> refusal ~name ~holder:None file)
        ~on_wait raw_fd ignore
  | None ->
      Fmt.invalid_arg
        "Eio_fs_unix.Lease: %s: the rendezvous carries no Unix file descriptor"
        name

let rec acquire ~sw ?lease
    ?(block : [ `Yes | `No | `Fibers | `For of float ] = `Yes)
    ?(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ] =
      `If_missing 0o600) ?contents ?(on_wait = ignore) path =
  let name = lock_name path in
  let steps = ref [] in
  let taken give_back = steps := give_back :: !steps in
  let shared = lease = Some `Shared in
  let open_rendezvous () =
    let file = open_lock_file ~sw ~name ~create path in
    (file, (Eio.File.stat file : Eio.File.Stat.t))
  in
  match
    eio_unix_errors @@ fun () ->
    Eio.Switch.run @@ fun budget_sw ->
    let fibers, processes = block_patience ~sw:budget_sw block in
    let key, opened_first = key_of_name ~taken ~name path open_rendezvous in
    let local = shared_mutex local_locks key in
    taken (fun () -> done_with_shared_mutex local_locks key local);
    let release_reader =
      take_inode_gate ~taken ~shared ~patience:fibers ~on_wait ~name
        ~opened_first local
    in
    let file, (opened : Eio.File.Stat.t) =
      match opened_first with
      | Some opened -> opened
      | None -> open_keyed ~taken ~key open_rendezvous
    in
    if not shared then begin
      set_held_file local (Some (file :> Eio.File.ro_ty Eio.Resource.t));
      taken (fun () -> set_held_file local None)
    end;
    (* An explicitly unlocked reader may finish while another fiber holds a
       record lock. Closing its descriptor must not drop that writer's lock. *)
    taken (fun () -> let_go_of local file);
    (match lease with
    | None ->
        take_record_lock ~taken ~patience:processes ~on_wait ~name file path
    | Some _ -> take_lease_lock ~shared ~patience:processes ~on_wait ~name file);
    settle ~name ~opened ~contents ~file path;
    (file, release_reader)
  with
  | file, release_reader -> (file, (fun () -> let_go steps), release_reader)
  | exception Renamed ->
      let_go steps;
      acquire ~sw ?lease ~block ~create ?contents ~on_wait path
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      let_go steps;
      Printexc.raise_with_backtrace e bt

let with_lock ?block ?create ?contents ?on_wait path fn =
  Eio.Switch.run @@ fun sw ->
  let _, let_go, _ = acquire ~sw ?block ?create ?contents ?on_wait path in
  Fun.protect ~finally:let_go fn

(* A held lock is an acquisition whose giving back waits for {!Held.release}
   or for [sw]'s release hooks, which run after every fiber of [sw] has
   finished: a hook the holder registered after taking it runs first, while
   the lock is still held. Every write through the file is serialised. *)
module Held = struct
  type t = {
    name : string;
    mutable file : Eio.File.rw_ty Eio.Resource.t option;
    let_go : unit -> unit;
    writing : Eio.Mutex.t;
  }

  (* A refused restate is the caller's mistake and poisons nothing. *)
  let writing t fn =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.lock t.writing;
    Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.writing) fn

  let release ?contents t =
    writing t @@ fun () ->
    match t.file with
    | None -> ()
    | Some file ->
        t.file <- None;
        Fun.protect ~finally:t.let_go (fun () ->
            Option.iter (update_lock_contents file) contents)

  let take ~sw ?block ?create ?contents ?on_wait path =
    let file, let_go, _ = acquire ~sw ?block ?create ?contents ?on_wait path in
    let t =
      {
        name = lock_name path;
        file = Some (file :> Eio.File.rw_ty Eio.Resource.t);
        let_go;
        writing = Eio.Mutex.create ();
      }
    in
    Eio.Switch.on_release sw (fun () -> release t);
    t

  (* Taking the record lock again on the held descriptor changes nothing while
     it is held, and restores it after a close elsewhere in this process
     dropped it; a process that took it in between is named, and nothing is
     written. *)
  let take_back t file =
    match file_fd file with
    | None -> ()
    | Some fd ->
        take_os_lock ~patience:Refuse
          ~refused:(fun () ->
            let holder =
              Eio_unix.Fd.use_exn "Eio_fs_unix.Held.restate" fd
                held_by_a_process
            in
            refusal ~name:t.name ~holder file)
          ~on_taken:ignore ~on_wait:ignore fd

  let restate t contents =
    writing t @@ fun () ->
    match t.file with
    | None -> invalid_arg "Eio_fs_unix.Held.restate: the lock has been released"
    | Some file ->
        eio_unix_errors (fun () -> take_back t file);
        update_lock_contents file contents
end

(* The kernel is asked first: it names another process holding the record
   lock, and a fiber of this process waiting for that process holds the mutex
   and not the lock. A fiber holder is this process, which the kernel never
   reports. While a fiber holds the file, both are asked through the holder's
   descriptor, so the probe opens none of its own. *)
(* The lock over the open lock [file], whose stat is [opened]. *)
let probe_file ~name file (opened : Eio.File.Stat.t) =
  use_local_lock (opened.dev, opened.ino) @@ fun local ->
  Fun.protect ~finally:(fun () -> let_go_of local file) @@ fun () ->
  let another =
    match file_fd file with
    | None -> None
    | Some fd -> Eio_unix.Fd.use_exn "Eio_fs_unix.probe" fd held_by_a_process
  in
  let holder =
    match another with
    | Some _ -> another
    | None ->
        if Stdlib.Mutex.protect tables_mutex (fun () -> local.holding) then
          held_by_a_fiber ()
        else None
  in
  let record = read_record file in
  match holder with
  | None -> `Free record
  | Some _ -> `Held { path = name; holder; record }

(* The probe of the file the name holds once opened. A lock file that went
   after the open, as a ticket its holder removes, is absent, as it would be
   had it gone before; a name given to another regular file meanwhile, as a
   ticket's number given to the next ticket, is [`Renamed], for the caller
   to probe the file it holds now. *)
let probe_opened ~name path =
  Eio.Switch.run @@ fun sw ->
  check_lock_name ~name path;
  let file = Eio.Path.open_in ~sw path in
  let opened = Eio.File.stat file in
  match named_kind path with
  | None ->
      raise
        (Eio.Fs.err
           (Eio.Fs.Not_found (Eio_unix.Unix_error (Unix.ENOENT, "stat", name))))
  | Some named
    when is_regular opened.kind && is_regular named.kind
         && (Int64.compare opened.dev named.dev <> 0
            || Int64.compare opened.ino named.ino <> 0) ->
      `Renamed
  | Some named ->
      check_same_file ~name ~opened named;
      `Answer (probe_file ~name file opened)

(* The probes a name given to new files over and over may take before it is
   refused: each needs the name to be given to another file while the probe
   reads it. *)
let probe_attempts = 8

let rec probe_at ~attempts path =
  let name = lock_name path in
  let from_the_holder =
    match named_key ~name path with
    | None -> None
    | Some key -> (
        match
          Stdlib.Mutex.protect tables_mutex (fun () ->
              Hashtbl.find_opt local_locks key)
        with
        | None -> None
        | Some local ->
            with_held_file local
              ~otherwise:(fun () -> None)
              (fun file fd ->
                let holder =
                  match Option.bind fd held_by_a_process with
                  | Some _ as another -> another
                  | None -> held_by_a_fiber ()
                in
                Some
                  (`Held
                     { path = name; holder; record = read_held_record file fd }))
        )
  in
  match from_the_holder with
  | Some answer -> answer
  | None -> (
      match probe_opened ~name path with
      | `Answer answer -> answer
      | `Renamed when attempts > 1 -> probe_at ~attempts:(attempts - 1) path
      | `Renamed -> not_a_lock_file name)

let probe path = probe_at ~attempts:probe_attempts path

module Lease = struct
  type t = {
    fd : Eio_unix.Fd.t;
    release_reader : unit -> unit;
    let_go : unit -> unit;
  }

  type block = [ `Yes | `No | `Fibers | `For of float ]

  let fd t = t.fd

  let unlock t =
    Eio.Cancel.protect @@ fun () ->
    eio_unix_errors (fun () ->
        Eio_unix.Fd.use_exn "Eio_fs_unix.Lease.unlock" t.fd flock_unlock_native);
    t.release_reader ()

  let release t = t.let_go ()

  let taken ~sw ~lease ?block ?create ?contents ?on_wait path =
    let file, let_go, release_reader =
      acquire ~sw ~lease ?block ?create ?contents ?on_wait path
    in
    match file_fd file with
    | Some fd ->
        let t = { fd; release_reader; let_go } in
        Eio.Switch.on_release sw (fun () -> release t);
        t
    | None ->
        let_go ();
        invalid_arg "Eio_fs_unix.Lease: acquired descriptor is unavailable"

  (* Whether only shared holders hold [path]: a shared lease tried without
     waiting and let go at once is granted beside them, and refused by an
     exclusive holder. An absent rendezvous has none. *)
  let only_shared path =
    match
      Eio.Switch.run @@ fun sw ->
      release (taken ~sw ~lease:`Shared ~block:`No ~create:`Never path)
    with
    | () -> true
    | exception Lock_unavailable _ -> false
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true

  let take_mode ~sw ~lease ?block ?create ?contents ?on_wait path =
    match block with
    | Some `Readers -> (
        match taken ~sw ~lease ~block:`No ?create ?contents path with
        | t -> t
        | exception (Lock_unavailable _ as refused) ->
            if only_shared path then
              taken ~sw ~lease ~block:`Yes ?create ?contents ?on_wait path
            else raise refused)
    | Some (#block as block) ->
        taken ~sw ~lease ~block ?create ?contents ?on_wait path
    | None -> taken ~sw ~lease ?create ?contents ?on_wait path

  let take ~sw ?block ?create ?contents ?on_wait path =
    take_mode ~sw ~lease:`Exclusive ?block ?create ?contents ?on_wait path

  let with_mode ~lease ?block ?create ?contents ?on_wait path fn =
    Eio.Switch.run @@ fun sw ->
    let t = take_mode ~sw ~lease ?block ?create ?contents ?on_wait path in
    Fun.protect ~finally:(fun () -> release t) (fun () -> fn t)

  let with_exclusive ?block ?create ?contents ?on_wait path fn =
    with_mode ~lease:`Exclusive ?block ?create ?contents ?on_wait path fn

  let with_shared ?block ?create ?on_wait path fn =
    with_mode ~lease:`Shared ?block ?create ?on_wait path fn
end

(* A backend-free component takes its critical section as an
   [Eio_fs_sync.Lock.t]; this is that section over a rendezvous of this
   filesystem's. Everything [with_lock] raises before the body begins becomes
   [Eio_fs_sync.Lock.Unavailable]; what the body raises travels out untouched. *)
module Rendezvous = struct
  (* The rendezvous is captured in the closure, keeping the section
     polymorphic in its result. *)
  type t = { enter : 'a. (unit -> 'a) -> 'a }

  let use t f = t.enter f
end

module Section = Eio_fs_sync.Lock.Make (Rendezvous)

let fail_unavailable fmt =
  Fmt.kstr (fun m -> raise (Eio_fs_sync.Lock.Unavailable m)) fmt

let enter_lock ?block ?create ?contents path f =
  let began = ref false in
  let section () =
    began := true;
    f ()
  in
  match with_lock ?block ?create ?contents path section with
  | answer -> answer
  | exception Lock_unavailable refusal when not !began ->
      fail_unavailable "%a" pp_refusal refusal
  | exception
      Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied (Not_a_lock_file name)), _)
    when not !began ->
      fail_unavailable "%s is not the lock's own regular file" name
  | exception (Eio.Io _ as e) when not !began ->
      fail_unavailable "%s cannot be locked: %s" (lock_name path)
        (Printexc.to_string e)
  | exception Invalid_argument m when not !began -> fail_unavailable "%s" m

let lock_section ?block ?create ?contents path =
  Section.v
    { Rendezvous.enter = (fun f -> enter_lock ?block ?create ?contents path f) }

(* {1 Extended attributes} *)

external llistxattr : string -> string = "caml_eio_fs_llistxattr"
external lgetxattr : string -> string -> string = "caml_eio_fs_lgetxattr"

external lsetxattr : string -> string -> string -> unit
  = "caml_eio_fs_lsetxattr"

(* The kernel lists names as a run of NUL-terminated strings. *)
let names_of_list s =
  String.split_on_char '\000' s |> List.filter (fun n -> n <> "")

let xattrs path =
  let native = Eio.Path.native_exn path in
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-xattrs" @@ fun () ->
  names_of_list (llistxattr native)
  |> List.sort String.compare
  |> List.map (fun name -> (name, lgetxattr native name))

let set_xattr path name v =
  let native = Eio.Path.native_exn path in
  eio_unix_errors @@ fun () ->
  Eio_unix.run_in_systhread ~label:"eio-fs-set-xattr" @@ fun () ->
  lsetxattr native name v

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
    if String.equal path "" then fail_refused "empty path"
    else if Char.equal path.[0] '/' then fail_refused "%S is absolute" path
    else if String.contains path '\000' then
      fail_refused "%S holds a NUL byte" path
    else
      let parts = String.split_on_char '/' path in
      List.iter
        (function
          | "" -> fail_refused "%S has an empty component" path
          | ("." | "..") as c -> fail_refused "%S has a %s component" path c
          | _ -> ())
        parts;
      match List.rev parts with
      | leaf :: rev_parents -> (List.rev rev_parents, leaf)
      | [] -> fail_refused "empty path"

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
        Stat (At.to_stat (At.lstat dir leaf))

  let attempt run op =
    match perform run op with
    | outcome -> Ok outcome
    | exception Refused reason -> Error (`Refused reason)
    | exception Unix.Unix_error (code, call, arg) ->
        Error (`Unix (code, call, arg))

  let with_parent (input : _ Eio.Path.t) fn =
    let root, path = (input :> Eio.Fs.dir_ty Eio.Path.t) in
    let denied reason = unsafe_parent reason in
    let parents, leaf =
      match split path with
      | parts -> parts
      | exception Refused reason -> denied reason
    in
    let rec descend (dir : Eio.Fs.dir_ty Eio.Path.t) = function
      | [] -> fn Eio.Path.(dir / leaf)
      | name :: rest ->
          let next = Eio.Path.(dir / name) in
          let before = Eio.Path.stat ~follow:false next in
          if before.kind <> `Directory then
            Fmt.kstr denied "%S has a non-directory parent" path;
          Eio.Path.with_open_dir next (fun held ->
              let after = Eio.Path.stat ~follow:false held in
              if
                not
                  (Int64.equal before.dev after.dev
                  && Int64.equal before.ino after.ino
                  && after.kind = `Directory)
              then Fmt.kstr denied "%S changed while opening its parent" path;
              descend (held :> Eio.Fs.dir_ty Eio.Path.t) rest)
    in
    match parents with
    | [] ->
        Eio.Path.with_open_dir
          Eio.Path.(of_dir root / ".")
          (fun held -> descend (held :> Eio.Fs.dir_ty Eio.Path.t) [])
    | _ -> descend (Eio.Path.of_dir root) parents

  let portable root = function
    | Mkdir { path; perm } ->
        with_parent Eio.Path.(root / path) (Eio.Path.mkdir ~perm);
        Done
    | Create { path; perm; contents } ->
        with_parent
          Eio.Path.(root / path)
          (fun p ->
            Eio.Path.save ~follow:false ~create:(`Exclusive perm) p contents);
        Done
    | Symlink { path; target } ->
        with_parent Eio.Path.(root / path) (Eio.Path.symlink ~link_to:target);
        Done
    | Unlink { path; dir } ->
        with_parent
          Eio.Path.(root / path)
          (fun p ->
            if dir then Eio.Path.rmdir p
            else if Eio.Path.kind ~follow:false p = `Directory then
              raise (Unix.Unix_error (Unix.EISDIR, "unlink", path))
            else Eio.Path.unlink p);
        Done
    | Rename { src; dst } ->
        with_parent
          Eio.Path.(root / src)
          (fun src -> with_parent Eio.Path.(root / dst) (Eio.Path.rename src));
        Done
    | Link _ -> fail_refused "hard links require a native directory"
    | Lstat path ->
        with_parent
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
        Error (`Refused (Fmt.str "%a" Eio.Exn.pp_err err))

  let run t ops =
    match (t, ops) with
    | _, [] -> []
    | Portable (_, live), _ when not !live ->
        invalid_arg "Eio_fs_unix.Batch.run: switch finished"
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

module Private = struct
  let batch_hops = Batch.hops
end
