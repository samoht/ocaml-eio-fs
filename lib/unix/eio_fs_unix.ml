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

let statvfs _ = invalid_arg "Eio_fs_unix.statvfs: not implemented"

(* A registered barrier keeps its own semantics (the crash model commits its
   journal, the in-memory backend does nothing); a plain POSIX directory is
   opened natively -- Eio directory resources do not expose their descriptor
   -- and pushed through to stable storage, which on Darwin means F_FULLFSYNC
   where the portable fallback's fsync only reaches the drive's cache. *)
let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match Eio_fs_sync.find res with
  | Some barrier -> barrier ~path
  | None ->
      let fd = Unix.openfile (Eio.Path.native_exn p) [ Unix.O_RDONLY ] 0 in
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
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_out ~sw ~create:(create :> Eio.Fs.create) path in
  match Eio_unix.Resource.fd_opt file with
  | None ->
      invalid_arg "Eio_fs_unix.with_lock: path has no Unix file descriptor"
  | Some fd ->
      Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd @@ fun fd ->
      use_os_lock ~block ~path:native_path fd @@ fun () ->
      Option.iter (update_lock_contents file) contents;
      fn ()
