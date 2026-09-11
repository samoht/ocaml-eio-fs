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
  free_blocks : int64;
  available_blocks : int64;
}

external statvfs_native : string -> statvfs = "caml_eio_fs_statvfs"

let statvfs path =
  let native = Eio.Path.native_exn path in
  Eio_unix.run_in_systhread ~label:"eio-fs-statvfs" @@ fun () ->
  statvfs_native native

type extent = { logical : int64; physical : int64; length : int64 }

external seek_data_native : Unix.file_descr -> int64 -> int64 option
  = "caml_eio_fs_seek_data"

external seek_hole_native : Unix.file_descr -> int64 -> int64 option
  = "caml_eio_fs_seek_hole"

external extents_native :
  Unix.file_descr -> int64 -> int64 -> int -> extent list
  = "caml_eio_fs_extents"

(* Each of these runs on a system thread, and its stub releases the runtime
   system around the syscall. Both halves are needed and neither is enough on
   its own: the systhread takes the call off the thread the domain's scheduler
   runs on, and the release is what lets that scheduler take the runtime lock
   back while the call is in flight. The barrier below says the same thing
   about one fsync; a sparse map says it about millions of calls.

   The hop has a price, and it is why the extent map takes a batch. Measured
   2026-09-07 on macOS/arm64: 9.2 us for a round trip through
   [Eio_unix.run_in_systhread] against 0.31 us for the same lseek made
   directly on the calling thread. One hop per extent over two million extents
   is 19 s of overhead and nothing else; one hop per batch of a thousand is
   19 ms. *)

let seek_data fd offset =
  Eio_unix.run_in_systhread ~label:"eio-fs-seek-data" @@ fun () ->
  seek_data_native fd offset

let seek_hole fd offset =
  Eio_unix.run_in_systhread ~label:"eio-fs-seek-hole" @@ fun () ->
  seek_hole_native fd offset

let extents fd ~offset ~length ~max =
  Eio_unix.run_in_systhread ~label:"eio-fs-extents" @@ fun () ->
  extents_native fd offset length max

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
  match Eio_fs_sync.dir_barrier res with
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

(* The file half of the same barrier, and the registration is asked before the
   descriptor. That order is the whole of it: a descriptor a wrapper carries
   over is the operations the operating system offers on the open file, not a
   second way past the wrapper, so a barrier taken on it is one the wrapper
   never saw -- a model of power loss records none and drops at the cut the
   bytes the caller was told had reached the platter, while every real
   filesystem still looks correct. Reaching for the descriptor first is
   therefore not the stronger barrier, it is the barrier made invisible to
   exactly the wrapper written to observe it.

   A backend that declares nothing has its descriptor taken, and there the
   platform barrier is the right one: on Darwin F_FULLFSYNC, through to the
   platter, where [Eio.File.sync] stops at the drive's cache. As for the
   directory barrier it runs on a system thread, a fiber's own thread carrying
   the domain's scheduler. A file with neither a registration nor a descriptor
   takes the portable barrier, which is its own sync. *)
let sync_file file =
  match Eio_fs_sync.file_barrier file with
  | Some barrier -> barrier ()
  | None -> (
      match Eio_unix.Resource.fd_opt file with
      | None -> Eio_fs_sync.sync_file file
      | Some fd ->
          Eio_unix.Fd.use_exn "eio-fs-sync-file" fd @@ fun fd ->
          Eio_unix.run_in_systhread ~label:"eio-fs-sync-file" @@ fun () ->
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

let local_locks : (lock_key, local_lock) Hashtbl.t = Hashtbl.create 16
let entry_gates : (string, local_lock) Hashtbl.t = Hashtbl.create 16
let tables_mutex = Stdlib.Mutex.create ()

(* A mutex per key, kept for as long as a caller is using it. The tables are
   process-wide, so an entry that outlived its last user would grow with every
   file a program has ever locked. *)
let use_shared_mutex table key fn =
  let lock =
    Stdlib.Mutex.protect tables_mutex @@ fun () ->
    match Hashtbl.find_opt table key with
    | Some lock ->
        lock.users <- lock.users + 1;
        lock
    | None ->
        let lock = { mutex = Eio.Mutex.create (); users = 1 } in
        Hashtbl.add table key lock;
        lock
  in
  Fun.protect
    ~finally:(fun () ->
      Stdlib.Mutex.protect tables_mutex @@ fun () ->
      lock.users <- lock.users - 1;
      if lock.users = 0 then Hashtbl.remove table key)
    (fun () -> fn lock.mutex)

let use_local_lock (key : lock_key) fn = use_shared_mutex local_locks key fn

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

(* ---- entering is queued, so the order granted is the order entered ----

   The exclusion is the file's, and which file a name reaches only the open can
   say, so the open comes first -- and it suspends, eio running it off the
   calling fiber. Two fibers entering in order therefore reach the file's mutex
   in whatever order their opens came back in, which is no order at all. A
   caller that sequences its work by the order it was served, a spool numbering
   its writers as it admits them, gets that sequence back scrambled, and it has
   nowhere of its own to stand between entering and holding.

   So entering is queued as well, on a mutex keyed by the name the caller
   passed. It is taken before the name is weighed, and left as the file's own
   mutex is about to be taken, with no suspension in between: the order fibers
   queue at the file's mutex is then the order they entered.

   The queue is left there rather than held for the length of the critical
   section, because a name is not a file. Two names reach one file wherever a
   hard link does, and a backend with no native path names its files as they
   print, so one name can stand for files of two unrelated filesystems. A gate
   held across [fn] would let a caller's critical section keep an unrelated
   lock waiting on a name it happens to share; held only across the open, what
   a collision costs is one open. *)
let in_entry_order name fn =
  use_shared_mutex entry_gates name @@ fun mutex ->
  let entered = ref false in
  Fun.protect
    ~finally:(fun () -> if !entered then Eio.Mutex.unlock mutex)
    (fun () ->
      Eio.Mutex.lock mutex;
      entered := true;
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
   both of which are taken on the file this open settled. What the open does
   settle is the caller's turn: it is made inside the name's entry queue, so a
   fiber that entered earlier reaches the file's mutex earlier.

   A rendezvous carrying no descriptor has no record lock to take, and the
   backend that handed it out is the only party that knows what that costs. One
   whose files no process outside this one can open declares itself
   process-local, and there the mutex is not the weaker half of the exclusion
   but the whole of it. Any other backend is refused, since running under the
   mutex alone would put this caller in a critical section another process can
   be inside with nothing in either run to say so. *)
let with_lock ?(block : [ `Yes | `No | `Fibers | `For of float ] = `Yes)
    ?(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ] =
      `If_missing 0o600) ?contents path fn =
  (* [block] is answered per half, because each half is a different holder. The
     mutex is held by a fiber of this program, which [`Fibers] waits for; the
     record lock is held by another process, which it answers about. *)
  let block_fibers, block_processes =
    match block with
    | `Yes -> (true, true)
    | `No -> (false, false)
    | `Fibers -> (true, false)
    | `For _ ->
        failwith
          "Eio_fs_unix.with_lock: `For: a bounded wait for a held lock is not \
           implemented"
  in
  let name = lock_name path in
  Eio.Switch.run @@ fun sw ->
  let file, (opened : Eio.File.Stat.t) =
    in_entry_order name @@ fun () ->
    check_lock_name ~name path;
    let file = open_lock_file ~sw ~name ~create path in
    (file, Eio.File.stat file)
  in
  use_local_lock (opened.dev, opened.ino) @@ fun mutex ->
  use_mutex ~block:block_fibers ~path:name mutex @@ fun () ->
  match Eio_unix.Resource.fd_opt file with
  | Some fd ->
      Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd @@ fun fd ->
      use_os_lock ~block:block_processes ~path:name fd @@ fun () ->
      hold ~name ~opened ~contents ~file path fn
  | None ->
      if Eio_fs_sync.is_process_local (fst path) then
        hold ~name ~opened ~contents ~file path fn
      else
        Fmt.invalid_arg
          "Eio_fs_unix.with_lock: %s: the rendezvous carries no Unix file \
           descriptor and its backend registers no Eio_fs_sync.Process_local"
          name

(* ---- the rendezvous, as a section a backend-free component runs inside ----

   A component that keeps files but cannot name a lock library takes its
   critical section as an [Eio_fs_sync.Lock.t] instead, and this is that
   section over a rendezvous of this filesystem's. Being the one place that
   knows both vocabularies, it is where every way [with_lock] can refuse
   becomes [Eio_fs_sync.Lock.Unavailable] naming what refused: the component on
   the other side has no other exception it can catch, and one travelling out
   through an interface whose type promises a result is the exclusion failing
   silently from that interface's point of view.

   The translation covers only what is raised before the section's body begins.
   What the body raises is the component's own and travels out as the body
   raised it: a write that failed under a lock the caller was granted is not
   the lock refusing, and reporting it as one would send an operator looking
   for a holder that is not there. *)
module Rendezvous = struct
  (* [with_lock] takes a path at whatever capability type the caller holds, so
     the rendezvous is captured in the closure rather than carried as a value.
     The explicit [ 'a. ] is what keeps the packed section polymorphic in its
     result, as [Eio_fs_sync.Lock.Make] requires. *)
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
  | exception Lock_unavailable name when not !began ->
      fail_unavailable "%s is held by another process" name
  | exception
      Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied (Not_a_lock_file name)), _)
    when not !began ->
      fail_unavailable "%s is not the lock's own regular file" name
  | exception (Eio.Io _ as e) when not !began ->
      fail_unavailable "%s cannot be locked: %s" (lock_name path)
        (Printexc.to_string e)
  | exception Unix.Unix_error (e, _, _) when not !began ->
      fail_unavailable "%s cannot be locked: %s" (lock_name path)
        (Unix.error_message e)
  | exception Invalid_argument m when not !began -> fail_unavailable "%s" m

let lock_section ?block ?create ?contents path =
  Section.v
    { Rendezvous.enter = (fun f -> enter_lock ?block ?create ?contents path f) }
