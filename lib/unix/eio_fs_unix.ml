external is_darwin : unit -> bool = "caml_eio_fs_is_darwin"

external flock_exclusive_native : Unix.file_descr -> bool -> unit
  = "caml_eio_fs_flock_exclusive"

external darwin_full_fsync : Unix.file_descr -> unit
  = "caml_eio_fs_darwin_full_fsync"

external record_lock_holder : Unix.file_descr -> int
  = "caml_eio_fs_record_lock_holder"

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

(* Each runs on a system thread whose stub releases the runtime system around
   the syscall. The hop has a price, which is why the extent map takes a batch:
   one hop per batch covers many extents where one hop each would spend the
   time in scheduling alone. *)
let seek_data fd offset =
  Eio_unix.run_in_systhread ~label:"eio-fs-seek-data" @@ fun () ->
  seek_data_native fd offset

let seek_hole fd offset =
  Eio_unix.run_in_systhread ~label:"eio-fs-seek-hole" @@ fun () ->
  seek_hole_native fd offset

let extents fd ~offset ~length ~max =
  Eio_unix.run_in_systhread ~label:"eio-fs-extents" @@ fun () ->
  extents_native fd offset length max

(* A registered barrier keeps its own semantics; a plain POSIX directory is
   opened through the capability the path carries -- so a name a link has taken
   is refused rather than fsynced instead of the caller's directory -- and
   pushed through to stable storage off the calling fiber's thread. A handle
   with no descriptor takes the portable barrier. *)
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

(* The registration is asked before the descriptor: a descriptor a wrapper
   carries over is a second way past the wrapper, so a barrier taken on it is
   one the wrapper never saw. A file with neither a registration nor a
   descriptor takes the portable barrier, its own sync. *)
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

let float_of_time seconds nanoseconds =
  let seconds = Int64.to_float seconds in
  let value = seconds +. (float nanoseconds /. 1e9) in
  if floor value = seconds then value else Float.pred value

let stat_of_fd fd =
  let stat = Eio_posix.Low_level.create_stat () in
  Eio_posix.Low_level.fstat ~buf:stat fd;
  Eio.File.Stat.
    {
      dev = Eio_posix.Low_level.dev stat;
      ino = Eio_posix.Low_level.ino stat;
      kind = Eio_posix.Low_level.kind stat;
      perm = Eio_posix.Low_level.perm stat;
      nlink = Eio_posix.Low_level.nlink stat;
      uid = Eio_posix.Low_level.uid stat;
      gid = Eio_posix.Low_level.gid stat;
      rdev = Eio_posix.Low_level.rdev stat;
      size = Eio_posix.Low_level.size stat |> Optint.Int63.of_int64;
      blksize = Eio_posix.Low_level.blksize stat;
      blocks = Eio_posix.Low_level.blocks stat;
      atime =
        float_of_time
          (Eio_posix.Low_level.atime_sec stat)
          (Eio_posix.Low_level.atime_nsec stat);
      mtime =
        float_of_time
          (Eio_posix.Low_level.mtime_sec stat)
          (Eio_posix.Low_level.mtime_nsec stat);
      ctime =
        float_of_time
          (Eio_posix.Low_level.ctime_sec stat)
          (Eio_posix.Low_level.ctime_nsec stat);
    }

let rec unwrapped_dir dir =
  match Eio_fs.Chaos.base_dir dir with
  | Some base -> unwrapped_dir base
  | None -> dir

let eio_unix_errors fn =
  match fn () with
  | value -> value
  | exception Unix.Unix_error (code, operation, argument) ->
      raise (Eio_unix.Err.v code operation argument)

let read_file_nofollow ~max_bytes ~validate path =
  if max_bytes < 0 || max_bytes = max_int then
    invalid_arg "Eio_fs_unix.read_file_nofollow: invalid byte bound";
  match Eio.Path.split path with
  | None ->
      invalid_arg "Eio_fs_unix.read_file_nofollow: path has no final component"
  | Some (parent, name) -> (
      if String.equal name "." || String.equal name ".." then
        invalid_arg
          "Eio_fs_unix.read_file_nofollow: final component is not a file name";
      Eio.Switch.run @@ fun sw ->
      let opened_parent = Eio.Path.open_dir ~sw parent in
      let wrapped_dir, relative = Eio.Path.(opened_parent / name) in
      (* Opening the parent through the supplied capability is the path
         operation a wrapper must observe. Only then unwrap its already-open
         directory so a backend extension or POSIX descriptor can open the
         final basename without resolving the parent a second time. *)
      let dir = unwrapped_dir wrapped_dir in
      match Eio_fs.Nofollow.find dir with
      | Some reader -> reader ~sw ~path:relative ~max_bytes ~validate
      | None -> (
          (* Eio's directory resource does not export its POSIX descriptor.
             Open that already-resolved directory as a file to obtain one;
             this duplicates the handle without resolving [parent] again. *)
          let parent_file = Eio.Path.open_in ~sw (Eio.Path.of_dir dir) in
          match Eio_unix.Resource.fd_opt parent_file with
          | None ->
              invalid_arg
                "Eio_fs_unix.read_file_nofollow: unsupported directory backend"
          | Some parent_fd ->
              eio_unix_errors @@ fun () ->
              let flags = Eio_posix.Low_level.Open_flags.(rdonly + nofollow) in
              let fd =
                Eio_posix.Low_level.Resolve.open_unconfined ~sw ~mode:0
                  (Some parent_fd) relative flags
              in
              let stat = stat_of_fd fd in
              Eio_fs.Nofollow.run ~max_bytes ~validate ~stat
                ~read:(fun buffer -> Eio_posix.Low_level.readv fd [| buffer |]))
      )

external rename_exclusive_at :
  Unix.file_descr -> string -> Unix.file_descr -> string -> unit
  = "caml_eio_fs_rename_exclusive"

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

type refusal = { path : string; holder : int option; record : string }

exception Lock_unavailable of refusal

let pp_refusal f { path; holder; record = _ } =
  match holder with
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

type local_lock = { mutex : Eio.Mutex.t; mutable users : int }

let local_locks : (lock_key, local_lock) Hashtbl.t = Hashtbl.create 16
let entry_gates : (string, local_lock) Hashtbl.t = Hashtbl.create 16
let tables_mutex = Stdlib.Mutex.create ()

(* A mutex per key, kept only while a caller uses it. *)
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

(* One budget covers both halves of the exclusion, so it is a moment both watch
   for rather than a duration each is handed. A daemon of the switch sleeps it
   on the scheduler's monotonic clock. *)
type patience =
  | Wait  (** wait for the holder, however long it holds *)
  | Refuse  (** answer at once *)
  | Until of unit Eio.Promise.t  (** wait until the budget lapses *)

let budget ~sw seconds =
  let lapsed, set_lapsed = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio_unix.sleep seconds;
      Eio.Promise.resolve set_lapsed ();
      `Stop_daemon);
  Until lapsed

let use_mutex ~patience ~refused mutex fn =
  let locked = ref false in
  Fun.protect
    ~finally:(fun () -> if !locked then Eio.Mutex.unlock mutex)
    (fun () ->
      (match patience with
      | Wait ->
          Eio.Mutex.lock mutex;
          locked := true
      | Refuse ->
          if Eio.Mutex.try_lock mutex then locked := true
          else raise (refused ())
      | Until lapsed ->
          (* [Fiber.first] cancels the loser, and a fiber cancelled inside
             [Eio.Mutex.lock] may have been granted the mutex all the same:
             the unlocker's wake and the cancellation race for one atomic and
             either can win. So what was taken is remembered rather than read
             off which fiber came back, and a mutex granted as the bound
             lapsed is held -- and released by the [finally] above -- rather
             than dropped with nobody left to unlock it. *)
          let taken = ref false in
          let granted =
            Eio.Fiber.first
              (fun () ->
                Eio.Mutex.lock mutex;
                taken := true;
                true)
              (fun () ->
                Eio.Promise.await lapsed;
                false)
          in
          if granted || !taken then locked := true else raise (refused ()));
      fn ())

(* Entering is queued too, on a mutex keyed by the caller's name: the open
   suspends, so without this the order fibers reach the file's mutex is the
   order their opens came back in. The gate is held only across the open, since
   a name is not a file. *)
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

(* POSIX has no portable timed record lock, so a bounded wait asks again; it
   also answers a holder that died, whose lock the kernel drops. *)
let ask_again_every = 0.05

let use_os_lock ~patience ~refused fd fn =
  let acquired = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !acquired then
        Eio.Cancel.protect @@ fun () ->
        Eio_unix.run_in_systhread ~label:"eio-fs-unlock" (fun () ->
            lockf fd Unix.F_ULOCK))
    (fun () ->
      (* The acquiring [lockf] is not cancellable -- it runs in a systhread and
         a cancellation cannot reach into one -- so a bounded wait never sits
         in it: it asks with [F_TLOCK], which answers, and does its waiting in
         a sleep, which cancels. *)
      let ask command =
        Eio.Cancel.protect (fun () ->
            match
              Eio_unix.run_in_systhread ~label:"eio-fs-lock" (fun () ->
                  lockf fd command)
            with
            | () -> acquired := true
            | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
                ())
      in
      (match patience with
      | Wait -> ask Unix.F_LOCK
      | Refuse -> ask Unix.F_TLOCK
      | Until lapsed ->
          let rec poll () =
            ask Unix.F_TLOCK;
            if (not !acquired) && not (Eio.Promise.is_resolved lapsed) then begin
              (* Waking on the lapse rather than sleeping through it is what
                 keeps the answer inside one further ask of the bound. *)
              Eio.Fiber.first
                (fun () -> Eio_unix.sleep ask_again_every)
                (fun () -> Eio.Promise.await lapsed);
              poll ()
            end
          in
          poll ());
      if not !acquired then raise (refused fd);
      fn ())

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
let hold ~name ~opened ~contents ~file path fn =
  check_locked_file ~name ~opened path;
  Option.iter (update_lock_contents file) contents;
  fn ()

(* The rendezvous is opened before the mutex is taken, the mutex key coming
   from the opened descriptor. A rendezvous with no descriptor has no record
   lock to take: a process-local backend makes the mutex the whole exclusion,
   and any other is refused. *)
let with_lock ?(block : [ `Yes | `No | `Fibers | `For of float ] = `Yes)
    ?(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ] =
      `If_missing 0o600) ?contents path fn =
  (* [block] is answered per half: the mutex is held by a fiber, the record
     lock by another process. *)
  let name = lock_name path in
  Eio.Switch.run @@ fun sw ->
  let fibers, processes =
    match block with
    | `Yes -> (Wait, Wait)
    | `No -> (Refuse, Refuse)
    | `Fibers -> (Wait, Refuse)
    | `For seconds ->
        let until = budget ~sw seconds in
        (until, until)
  in
  let file, (opened : Eio.File.Stat.t) =
    in_entry_order name @@ fun () ->
    check_lock_name ~name path;
    let file = open_lock_file ~sw ~name ~create path in
    (file, Eio.File.stat file)
  in
  use_local_lock (opened.dev, opened.ino) @@ fun mutex ->
  use_mutex ~patience:fibers
    ~refused:(fun () -> refusal ~name ~holder:(held_by_a_fiber ()) file)
    mutex
  @@ fun () ->
  match Eio_unix.Resource.fd_opt file with
  | Some fd ->
      Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd @@ fun fd ->
      use_os_lock ~patience:processes
        ~refused:(fun fd -> refusal ~name ~holder:(held_by_a_process fd) file)
        fd
      @@ fun () -> hold ~name ~opened ~contents ~file path fn
  | None ->
      if Eio_fs_sync.is_process_local (fst path) then
        hold ~name ~opened ~contents ~file path fn
      else
        Fmt.invalid_arg
          "Eio_fs_unix.with_lock: %s: the rendezvous carries no Unix file \
           descriptor and its backend registers no Eio_fs_sync.Process_local"
          name

module Lease = struct
  type t = { fd : Eio_unix.Fd.t }

  let fd t = t.fd

  let flock fd ~nonblocking =
    let rec acquire () =
      match flock_exclusive_native fd nonblocking with
      | () -> ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> acquire ()
    in
    Eio_unix.run_in_systhread ~label:"eio-fs-lease-lock" acquire

  let use_os_lock ~patience ~refused fd fn =
    let acquired = ref false in
    let ask ~nonblocking =
      Eio.Cancel.protect @@ fun () ->
      match flock fd ~nonblocking with
      | () -> acquired := true
      | exception
          Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EACCES), _, _)
        ->
          ()
    in
    (match patience with
    | Wait -> ask ~nonblocking:false
    | Refuse -> ask ~nonblocking:true
    | Until lapsed ->
        let rec poll () =
          ask ~nonblocking:true;
          if (not !acquired) && not (Eio.Promise.is_resolved lapsed) then begin
            Eio.Fiber.first
              (fun () -> Eio_unix.sleep ask_again_every)
              (fun () -> Eio.Promise.await lapsed);
            poll ()
          end
        in
        poll ());
    if not !acquired then raise (refused ());
    fn ()

  let with_exclusive ?(block : [ `Yes | `No | `Fibers | `For of float ] = `Yes)
      ?(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ] =
        `If_missing 0o600) ?contents path fn =
    let name = lock_name path in
    Eio.Switch.run @@ fun sw ->
    let fibers, processes =
      match block with
      | `Yes -> (Wait, Wait)
      | `No -> (Refuse, Refuse)
      | `Fibers -> (Wait, Refuse)
      | `For seconds ->
          let until = budget ~sw seconds in
          (until, until)
    in
    let file, (opened : Eio.File.Stat.t) =
      in_entry_order name @@ fun () ->
      check_lock_name ~name path;
      let file = open_lock_file ~sw ~name ~create path in
      (file, Eio.File.stat file)
    in
    use_local_lock (opened.dev, opened.ino) @@ fun mutex ->
    use_mutex ~patience:fibers
      ~refused:(fun () -> refusal ~name ~holder:(held_by_a_fiber ()) file)
      mutex
    @@ fun () ->
    match Eio_unix.Resource.fd_opt file with
    | Some fd ->
        Eio_unix.Fd.use_exn "Eio_fs_unix.Lease.with_exclusive" fd
        @@ fun raw_fd ->
        (* The kernel names no holder of an [flock]. *)
        use_os_lock ~patience:processes
          ~refused:(fun () -> refusal ~name ~holder:None file)
          raw_fd
        @@ fun () ->
        hold ~name ~opened ~contents ~file path (fun () -> fn { fd })
    | None ->
        Fmt.invalid_arg
          "Eio_fs_unix.Lease.with_exclusive: %s: the rendezvous carries no \
           Unix file descriptor"
          name
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
  | exception Unix.Unix_error (e, _, _) when not !began ->
      fail_unavailable "%s cannot be locked: %s" (lock_name path)
        (Unix.error_message e)
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
  Eio_unix.run_in_systhread ~label:"eio-fs-xattrs" @@ fun () ->
  names_of_list (llistxattr native)
  |> List.sort String.compare
  |> List.map (fun name -> (name, lgetxattr native name))

let set_xattr path name v =
  let native = Eio.Path.native_exn path in
  Eio_unix.run_in_systhread ~label:"eio-fs-set-xattr" @@ fun () ->
  lsetxattr native name v
