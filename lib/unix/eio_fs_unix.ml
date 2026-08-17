external is_darwin : unit -> bool = "caml_eio_fs_is_darwin"

external darwin_full_fsync : Unix.file_descr -> unit
  = "caml_eio_fs_darwin_full_fsync"

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
  available_blocks : int64;
}

external statvfs_native : string -> statvfs = "caml_eio_fs_statvfs"

let statvfs path =
  let native = Eio.Path.native_exn path in
  Eio_unix.run_in_systhread ~label:"eio-fs-statvfs" @@ fun () ->
  statvfs_native native

(* A registered barrier keeps its own semantics (the crash model commits its
   journal, the in-memory backend does nothing); a plain POSIX directory is
   opened natively -- Eio directory resources do not expose their descriptor
   -- and pushed through to stable storage, which on Darwin means F_FULLFSYNC
   where the portable fallback's fsync only reaches the drive's cache.

   That barrier is the slowest call the process makes, so it runs on a system
   thread: a fiber's own thread carries the domain's scheduler, and holding it
   inside the fsync would stop every other fiber -- the timers, the connections
   -- for the length of a write to the platter. *)
let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match Eio_fs_sync.find res with
  | Some barrier -> barrier ~path
  | None ->
      let native = Eio.Path.native_exn p in
      Eio_unix.run_in_systhread ~label:"eio-fs-sync-dir" @@ fun () ->
      let fd = Unix.openfile native [ Unix.O_RDONLY ] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> full_fsync fd)

exception Lock_unavailable of string

type local_lock = { mutex : Eio.Mutex.t; mutable users : int }

let local_locks = Hashtbl.create 16
let local_locks_mutex = Stdlib.Mutex.create ()

let use_local_lock key fn =
  let lock =
    Stdlib.Mutex.protect local_locks_mutex @@ fun () ->
    match Hashtbl.find_opt local_locks key with
    | Some lock ->
        lock.users <- lock.users + 1;
        lock
    | None ->
        let lock = { mutex = Eio.Mutex.create (); users = 1 } in
        Hashtbl.add local_locks key lock;
        lock
  in
  Fun.protect
    ~finally:(fun () ->
      Stdlib.Mutex.protect local_locks_mutex @@ fun () ->
      lock.users <- lock.users - 1;
      if lock.users = 0 then Hashtbl.remove local_locks key)
    (fun () -> fn lock.mutex)

let canonical_lock_path path =
  Eio_unix.run_in_systhread ~label:"eio-fs-lock-path" @@ fun () ->
  match Unix.realpath path with
  | path -> path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Filename.concat
        (Unix.realpath (Filename.dirname path))
        (Filename.basename path)

let use_mutex ~block ~path mutex fn =
  let locked = ref false in
  Fun.protect
    ~finally:(fun () -> if !locked then Eio.Mutex.unlock mutex)
    (fun () ->
      if block then begin
        Eio.Mutex.lock mutex;
        locked := true
      end
      else if Eio.Mutex.try_lock mutex then locked := true
      else raise (Lock_unavailable path);
      fn ())

let lockf fd command = retry_eintr (fun fd -> Unix.lockf fd command 0) fd

let use_os_lock ~block ~path fd fn =
  let acquired = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !acquired then
        Eio.Cancel.protect @@ fun () ->
        Eio_unix.run_in_systhread ~label:"eio-fs-unlock" (fun () ->
            lockf fd Unix.F_ULOCK))
    (fun () ->
      Eio.Cancel.protect (fun () ->
          match
            Eio_unix.run_in_systhread ~label:"eio-fs-lock" (fun () ->
                lockf fd (if block then Unix.F_LOCK else Unix.F_TLOCK))
          with
          | () -> acquired := true
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
              raise (Lock_unavailable path));
      fn ())

(* ---- the lock path is the lock's own file ----

   A rendezvous sits in a directory another party writes, which is exactly
   where an entry gets planted under the name a locker is about to use.
   [Eio.Path] resolves a name for its caller and follows a link it finds at the
   end, so a link left under that name sends the record lock to whatever the
   link names: two processes then hold two locks over one resource, both read
   the same state and both publish, with nothing in either run to say the
   exclusion was absent.

   The name is weighed before the file is opened, so [create] never creates the
   file a dangling link points at, and the descriptor is weighed again once the
   lock is held, because a name is resolved a second time by the open and an
   entry that took it in between would leave the lock on an inode the name no
   longer reaches. Comparing the descriptor's device and inode with the name's
   own is what settles that: on a symbolic link the two differ, since the name
   carries the link's inode and the descriptor the target's. *)

type Eio.Exn.Backend.t += Not_a_lock_file of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Not_a_lock_file path ->
        Fmt.pf f "Not_a_lock_file %S" path;
        true
    | _ -> false)

let not_a_lock_file native =
  raise (Eio.Fs.err (Eio.Fs.Permission_denied (Not_a_lock_file native)))

let is_regular : Eio.File.Stat.kind -> bool = function
  | `Regular_file -> true
  | _ -> false

(* An absent name is the ordinary first acquisition: [create] decides whether
   the open makes one, and the descriptor is weighed either way. *)
let named_kind path =
  match Eio.Path.stat ~follow:false path with
  | stat -> Some stat
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let check_lock_name ~native path =
  match named_kind path with
  | None -> ()
  | Some stat -> if not (is_regular stat.kind) then not_a_lock_file native

let check_locked_file ~native path file =
  let opened = Eio.File.stat file in
  match named_kind path with
  | None -> not_a_lock_file native
  | Some named ->
      if
        (not (is_regular opened.kind))
        || (not (is_regular named.kind))
        || Int64.compare opened.dev named.dev <> 0
        || Int64.compare opened.ino named.ino <> 0
      then not_a_lock_file native

let update_lock_contents file contents =
  ignore (Eio.File.seek file Optint.Int63.zero `Set : Optint.Int63.t);
  Eio.Flow.copy_string contents file;
  Eio.File.truncate file (Optint.Int63.of_int (String.length contents))

let with_lock ?(block = true)
    ?(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ] =
      `If_missing 0o600) ?contents path fn =
  let native_path = Eio.Path.native_exn path in
  let key = canonical_lock_path native_path in
  use_local_lock key @@ fun mutex ->
  use_mutex ~block ~path:native_path mutex @@ fun () ->
  check_lock_name ~native:native_path path;
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_out ~sw ~create:(create :> Eio.Fs.create) path in
  match Eio_unix.Resource.fd_opt file with
  | None ->
      invalid_arg "Eio_fs_unix.with_lock: path has no Unix file descriptor"
  | Some fd ->
      Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd @@ fun fd ->
      use_os_lock ~block ~path:native_path fd @@ fun () ->
      check_locked_file ~native:native_path path file;
      Option.iter (update_lock_contents file) contents;
      fn ()
