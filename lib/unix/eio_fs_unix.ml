external is_darwin : unit -> bool = "caml_eio_fs_is_darwin"

external flock_exclusive_native : Unix.file_descr -> bool -> unit
  = "caml_eio_fs_flock_exclusive"

external darwin_full_fsync : Unix.file_descr -> unit
  = "caml_eio_fs_darwin_full_fsync"

external record_lock_holder : Unix.file_descr -> int
  = "caml_eio_fs_record_lock_holder"

external pread_head : Unix.file_descr -> int -> string
  = "caml_eio_fs_pread_head"

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

external clone_tree_native : string -> string -> bool = "caml_eio_fs_clone_tree"

let clone_tree src dst =
  let src = Eio.Path.native_exn src and dst = Eio.Path.native_exn dst in
  Eio_unix.run_in_systhread ~label:"eio-fs-clone-tree" @@ fun () ->
  clone_tree_native src dst

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

module Link = struct
  module Low = Eio_posix.Low_level

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

  let with_parent ?mkdir ~root ~path fn =
    let original = Eio_fs.Path.name Eio.Path.(root / path) in
    match components path with
    | Error reason -> err_refused original reason
    | Ok components -> (
        Eio.Switch.run @@ fun sw ->
        let open_parent () =
          let directory = Eio.Path.open_dir ~sw root in
          let file = Eio.Path.open_in ~sw directory in
          let descriptor =
            match Eio_unix.Resource.fd_opt file with
            | Some fd -> fd
            | None ->
                invalid_arg
                  "Eio_fs_unix.Link.with_parent: native directory required"
          in
          let rec descend fd = function
            | [] -> assert false
            | [ name ] -> { parent = fd; name; path = original }
            | name :: rest ->
                let parent = Low.Fd fd in
                let open_component () =
                  Low.openat ~sw ~mode:0 parent name
                    Low.Open_flags.(rdonly + directory + nofollow)
                in
                let next =
                  match open_component () with
                  | opened -> opened
                  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
                      match mkdir with
                      | None ->
                          raise (Unix.Unix_error (Unix.ENOENT, "openat", name))
                      | Some mode ->
                          (try Low.mkdir ~mode parent name
                           with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
                          open_component ())
                in
                descend next rest
          in
          descend descriptor components
        in
        let opened =
          match open_parent () with
          | handle -> Ok handle
          | exception Unix.Unix_error (code, operation, argument) ->
              err_refused original
                (Fmt.str "%s %s: %s" operation argument
                   (Unix.error_message code))
          | exception (Eio.Io _ as ex) ->
              err_refused original (Printexc.to_string ex)
        in
        match opened with Error _ as error -> error | Ok handle -> fn handle)

  let publish t ~target =
    let parent = Low.Fd t.parent in
    let existing () =
      let stat = Low.create_stat () in
      Low.fstatat ~buf:stat ~follow:false parent t.name;
      match Low.kind stat with
      | `Symbolic_link ->
          let held = Low.read_link parent t.name in
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
        match Low.symlink ~link_to:target parent t.name with
        | () -> Ok Created
        | exception Unix.Unix_error (Unix.EEXIST, _, _) -> existing ()
      with
      | result -> result
      | exception Unix.Unix_error (code, operation, argument) ->
          err_refused t.path
            (Fmt.str "%s %s: %s" operation argument (Unix.error_message code))
      | exception (Eio.Io _ as ex) -> err_refused t.path (Printexc.to_string ex)
end

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
  match Eio_unix.Resource.fd_opt file with
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
      match Eio_unix.Resource.fd_opt file with
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

(* [on_taken] runs the moment the mutex is held, so a caller records how to
   give it back before anything else can raise. *)
let take_mutex ~patience ~refused ~on_taken mutex =
  match patience with
  | Wait ->
      Eio.Mutex.lock mutex;
      on_taken ()
  | Refuse ->
      if Eio.Mutex.try_lock mutex then on_taken () else raise (refused ())
  | Until lapsed ->
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
            Eio.Mutex.lock mutex;
            taken := true;
            true)
          (fun () ->
            Eio.Promise.await lapsed;
            false)
      in
      if granted || !taken then on_taken () else raise (refused ())

let use_mutex ~patience ~refused mutex fn =
  let locked = ref false in
  Fun.protect
    ~finally:(fun () -> if !locked then Eio.Mutex.unlock mutex)
    (fun () ->
      take_mutex ~patience ~refused ~on_taken:(fun () -> locked := true) mutex;
      fn ())

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

(* POSIX has no portable timed record lock, so a bounded wait asks again; it
   also answers a holder that died, whose lock the kernel drops. *)
let ask_again_every = 0.05

let os_unlock fd =
  Eio.Cancel.protect @@ fun () ->
  Eio_unix.Fd.use_exn "Eio_fs_unix.unlock" fd @@ fun fd ->
  Eio_unix.run_in_systhread ~label:"eio-fs-unlock" (fun () ->
      lockf fd Unix.F_ULOCK)

(* [on_taken] runs the moment the record lock is held, as {!take_mutex}'s
   does. *)
let take_os_lock ~patience ~refused ~on_taken fd =
  let acquired = ref false in
  (* The acquiring [lockf] is not cancellable -- it runs in a systhread and a
     cancellation cannot reach into one -- so no wait sits in it: every ask is
     an [F_TLOCK], which answers, and the waiting is done in a sleep, which a
     bound, a Ctrl-C or a fiber giving up cancels. *)
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
  let rec wait_for lapsed =
    ask ();
    if not !acquired then
      match lapsed with
      | None ->
          Eio_unix.sleep ask_again_every;
          wait_for lapsed
      | Some lapse ->
          if not (Eio.Promise.is_resolved lapse) then begin
            (* Waking on the lapse rather than sleeping through it is what
               keeps the answer inside one further ask of the bound. *)
            Eio.Fiber.first
              (fun () -> Eio_unix.sleep ask_again_every)
              (fun () -> Eio.Promise.await lapse);
            wait_for lapsed
          end
  in
  (match patience with
  | Wait -> wait_for None
  | Refuse -> ask ()
  | Until lapse -> wait_for (Some lapse));
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
let settle ~name ~opened ~contents ~file path =
  check_locked_file ~name ~opened path;
  Option.iter (update_lock_contents file) contents

let run_held ~name ~opened ~contents ~file path fn =
  settle ~name ~opened ~contents ~file path;
  fn ()

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

(* The record lock on the rendezvous's descriptor. A rendezvous with none has
   no record lock to take, which only a process-local backend allows. *)
let take_record_lock ~taken ~patience ~name file path =
  match Eio_unix.Resource.fd_opt file with
  | Some fd ->
      take_os_lock ~patience
        ~refused:(fun () ->
          let holder =
            Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd held_by_a_process
          in
          refusal ~name ~holder file)
        ~on_taken:(fun () -> taken (fun () -> os_unlock fd))
        fd
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
let rec acquire ~sw ?(block : [ `Yes | `No | `Fibers | `For of float ] = `Yes)
    ?(create : [ `Never | `If_missing of Eio.File.Unix_perm.t ] =
      `If_missing 0o600) ?contents path =
  let name = lock_name path in
  let steps = ref [] in
  let taken give_back = steps := give_back :: !steps in
  let open_rendezvous () =
    let file = open_lock_file ~sw ~name ~create path in
    (file, (Eio.File.stat file : Eio.File.Stat.t))
  in
  match
    Eio.Switch.run @@ fun budget_sw ->
    let fibers, processes = block_patience ~sw:budget_sw block in
    let key, opened_first = key_of_name ~taken ~name path open_rendezvous in
    let local = shared_mutex local_locks key in
    taken (fun () -> done_with_shared_mutex local_locks key local);
    let entered = ref false in
    Option.iter
      (fun (file, _) ->
        taken (fun () -> if not !entered then let_go_of local file))
      opened_first;
    take_mutex ~patience:fibers
      ~refused:(fun () ->
        match opened_first with
        | Some (file, _) -> refusal ~name ~holder:(held_by_a_fiber ()) file
        | None -> refused_by_a_fiber ~name local)
      ~on_taken:(fun () ->
        entered := true;
        taken (fun () -> Eio.Mutex.unlock local.mutex))
      local.mutex;
    start_holding local;
    taken (fun () -> stop_holding local);
    let file, (opened : Eio.File.Stat.t) =
      match opened_first with
      | Some opened -> opened
      | None -> open_keyed ~taken ~key open_rendezvous
    in
    set_held_file local (Some (file :> Eio.File.ro_ty Eio.Resource.t));
    taken (fun () -> set_held_file local None);
    take_record_lock ~taken ~patience:processes ~name file path;
    settle ~name ~opened ~contents ~file path;
    file
  with
  | file -> (file, fun () -> let_go steps)
  | exception Renamed ->
      let_go steps;
      acquire ~sw ~block ~create ?contents path
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      let_go steps;
      Printexc.raise_with_backtrace e bt

let with_lock ?block ?create ?contents path fn =
  Eio.Switch.run @@ fun sw ->
  let _, let_go = acquire ~sw ?block ?create ?contents path in
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

  let take ~sw ?block ?create ?contents path =
    let file, let_go = acquire ~sw ?block ?create ?contents path in
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
    match Eio_unix.Resource.fd_opt file with
    | None -> ()
    | Some fd ->
        take_os_lock ~patience:Refuse
          ~refused:(fun () ->
            let holder =
              Eio_unix.Fd.use_exn "Eio_fs_unix.Held.restate" fd
                held_by_a_process
            in
            refusal ~name:t.name ~holder file)
          ~on_taken:ignore fd

  let restate t contents =
    writing t @@ fun () ->
    match t.file with
    | None -> invalid_arg "Eio_fs_unix.Held.restate: the lock has been released"
    | Some file ->
        take_back t file;
        update_lock_contents file contents
end

let hold ~sw ?block ?create ?contents path =
  let (_ : Held.t) = Held.take ~sw ?block ?create ?contents path in
  ()

(* The kernel is asked first: it names another process holding the record
   lock, and a fiber of this process waiting for that process holds the mutex
   and not the lock. A fiber holder is this process, which the kernel never
   reports. While a fiber holds the file, both are asked through the holder's
   descriptor, so the probe opens none of its own. *)
let probe path =
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
      Eio.Switch.run @@ fun sw ->
      check_lock_name ~name path;
      let file = Eio.Path.open_in ~sw path in
      let opened = Eio.File.stat file in
      check_locked_file ~name ~opened path;
      use_local_lock (opened.dev, opened.ino) @@ fun local ->
      Fun.protect ~finally:(fun () -> let_go_of local file) @@ fun () ->
      let another =
        match Eio_unix.Resource.fd_opt file with
        | None -> None
        | Some fd ->
            Eio_unix.Fd.use_exn "Eio_fs_unix.probe" fd held_by_a_process
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
      | Some _ -> `Held { path = name; holder; record })

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
    use_local_lock (opened.dev, opened.ino) @@ fun { mutex; _ } ->
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
        run_held ~name ~opened ~contents ~file path (fun () -> fn { fd })
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
