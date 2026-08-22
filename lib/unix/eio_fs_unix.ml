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
   opened and pushed through to stable storage, which on Darwin means
   F_FULLFSYNC where the portable fallback's fsync only reaches the drive's
   cache.

   The open goes through the directory capability the path carries, because
   the barrier belongs to the directory the caller holds and not to whatever
   its name resolves to at that instant. Eio says as much of the flattening:
   [Path.native] "does not check for confinement (the resulting path might not
   be accessible via [t] itself)", and answers a directory opened with
   [open_subtree] and later renamed with the old name. [Eio.Path.open_in]
   instead opens every component beneath that capability, so a name a link has
   taken is refused rather than fsynced in place of the caller's directory,
   and a directory whose name is gone is still barriered.

   That barrier is the slowest call the process makes, so it runs on a system
   thread: a fiber's own thread carries the domain's scheduler, and holding it
   inside the fsync would stop every other fiber -- the timers, the connections
   -- for the length of a write to the platter. The open is off that thread
   already, eio running its own openat in a worker.

   A directory handle carrying no descriptor cannot take the platform-strength
   barrier, so it takes the portable one, which syncs the handle itself and
   fails loudly if the backend cannot do that either. *)
let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match Eio_fs_sync.find res with
  | Some barrier -> barrier ~path
  | None -> (
      Eio.Switch.run @@ fun sw ->
      let handle = Eio.Path.open_in ~sw p in
      match Eio_unix.Resource.fd_opt handle with
      | None -> Eio_fs_sync.sync_dir p
      | Some fd ->
          Eio_unix.Fd.use_exn "eio-fs-sync-dir" fd @@ fun fd ->
          Eio_unix.run_in_systhread ~label:"eio-fs-sync-dir" @@ fun () ->
          full_fsync fd)

exception Lock_unavailable of string

(* ---- the exclusion is the file's, not the name's ----

   The mutex two fibers of this process contend for is keyed by the file the
   record lock is taken on, its device and inode, and not by a name that
   reaches that file. A name does not identify a file: two of them lead to one
   file wherever a hard link does, and canonicalising a name does not merge
   those, [Unix.realpath] resolving symbolic links and a hard link being none
   of them.

   Nothing else carries this half of the exclusion. POSIX record locks do not
   exclude within a process -- a second [lockf] over one file, through a second
   descriptor, is granted to the process that already holds it, and closing any
   one of those descriptors drops the lock for all of them -- so a key that
   misses hands one file to two fibers, each believing it holds the lock.

   The file stays open for as long as its key is in use, so no inode number is
   reused under the table while it names a held lock. *)
type lock_key = int64 * int64
(** The device and inode a record lock is held on. *)

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

   Two things settle it, and the check of the name is neither of them. Creating
   the rendezvous is an exclusive open: O_CREAT|O_EXCL refuses a name that
   exists, and a symbolic link at the last component counts as existing even
   when it dangles, so a creating open never lands at the far end of one. The
   descriptor is then weighed once the lock is held, because the open resolves
   the name a second time and an entry that took it in between would leave the
   lock on an inode the name no longer reaches. Comparing the descriptor's
   device and inode with the name's own is what settles that: on a symbolic
   link the two differ, since the name carries the link's inode and the
   descriptor the target's.

   Weighing the name before the open is an early refusal and nothing rests on
   it: it answers the ordinary planted link in the interface's own vocabulary
   rather than as whichever [open] error the platform picks, but the name can
   change the moment after it is read. *)

type Eio.Exn.Backend.t += Not_a_lock_file of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Not_a_lock_file path ->
        Fmt.pf f "Not_a_lock_file %S" path;
        true
    | _ -> false)

let not_a_lock_file name =
  raise (Eio.Fs.err (Eio.Fs.Permission_denied (Not_a_lock_file name)))

(* The name a refusal carries. A backend with no native path is named as it
   prints itself, rather than taken out through the exception [native_exn]
   raises for one. *)
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

let check_locked_file ~name ~(opened : Eio.File.Stat.t) path =
  match named_kind path with
  | None -> not_a_lock_file name
  | Some named ->
      if
        (not (is_regular opened.kind))
        || (not (is_regular named.kind))
        || Int64.compare opened.dev named.dev <> 0
        || Int64.compare opened.ino named.ino <> 0
      then not_a_lock_file name

let update_lock_contents file contents =
  ignore (Eio.File.seek file Optint.Int63.zero `Set : Optint.Int63.t);
  Eio.Flow.copy_string contents file;
  Eio.File.truncate file (Optint.Int63.of_int (String.length contents))

(* An absent rendezvous is created exclusively, so the kernel refuses the name
   a link holds rather than following it. A name that is already there is the
   ordinary second acquisition and is opened without creating, its identity
   settled afterwards by [check_locked_file]. A name that exists for the
   exclusive open and is gone for the plain one is whatever took it in between
   -- a dangling link, or an entry another party removed -- and is not the
   lock's own regular file. *)
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
let hold ~name ~opened ~contents ~file path fn =
  check_locked_file ~name ~opened path;
  Option.iter (update_lock_contents file) contents;
  fn ()

(* The rendezvous is opened before the mutex is taken, because the key the
   mutex is found by comes from the opened descriptor. Opening is not holding:
   the exclusion begins at the mutex and, across processes, at the record lock,
   both of which are taken on the file this open settled.

   A rendezvous carrying no descriptor has no record lock to take, and the
   backend that handed it out is the only party that knows what that costs. One
   whose files no process outside this one can open declares itself
   process-local, and there the mutex is not the weaker half of the exclusion
   but the whole of it. Any other backend is refused, since running under the
   mutex alone would put this caller in a critical section another process can
   be inside with nothing in either run to say so. *)
let with_lock ?(block = true)
    ?(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ] =
      `If_missing 0o600) ?contents path fn =
  let name = lock_name path in
  check_lock_name ~name path;
  Eio.Switch.run @@ fun sw ->
  let file = open_lock_file ~sw ~name ~create path in
  let opened : Eio.File.Stat.t = Eio.File.stat file in
  use_local_lock ((opened.dev, opened.ino) : lock_key) @@ fun mutex ->
  use_mutex ~block ~path:name mutex @@ fun () ->
  match Eio_unix.Resource.fd_opt file with
  | Some fd ->
      Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd @@ fun fd ->
      use_os_lock ~block ~path:name fd @@ fun () ->
      hold ~name ~opened ~contents ~file path fn
  | None ->
      if Eio_fs_sync.is_process_local (fst path) then
        hold ~name ~opened ~contents ~file path fn
      else
        Fmt.invalid_arg
          "Eio_fs_unix.with_lock: %s: the rendezvous carries no Unix file \
           descriptor and its backend registers no Eio_fs_sync.Process_local"
          name
