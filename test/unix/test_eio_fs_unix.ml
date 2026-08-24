(* Smoke tests for [Eio_fs_unix]: the barriers run against a real filesystem
   and surface errors as documented. Durability itself is not observable
   without cutting power; what is testable is that the barrier accepts the
   states a commit protocol hands it. *)

module Chaos = Eio_fs.Chaos
module Crash = Chaos.Crash

let ( / ) = Eio.Path.( / )

let mentions ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  at 0

let with_real_dir_env fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-dir" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () -> fn env (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t)

let with_real_dir fn = with_real_dir_env (fun _env dir -> fn dir)

(* The commit-protocol shape: write a temporary, fsync it, rename it into
   place, barrier the directory. Every step must succeed and the published
   file must read back whole. *)
let test_sync_dir_after_rename () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "f.tmp") "payload";
  Eio.Path.rename (dir / "f.tmp") (dir / "f");
  Eio_fs_unix.sync_dir dir;
  Alcotest.(check string)
    "published file whole after the barrier" "payload"
    (Eio.Path.load (dir / "f"))

(* The barrier is idempotent and accepts an empty directory. *)
let test_sync_dir_empty_and_repeated () =
  with_real_dir @@ fun dir ->
  Eio_fs_unix.sync_dir dir;
  Eio_fs_unix.sync_dir dir

(* ---- the barrier lands on the directory the caller holds ----

   [sync_dir] is handed an [Eio.Path.t]: a directory capability and a name
   resolved beneath it. Flattening that pair into a native string and opening
   the string from [/] hands the barrier to whatever the name resolves to at
   that instant. Eio says as much of the flattening itself -- [Path.native]
   "does not check for confinement (the resulting path might not be accessible
   via [t] itself)", and "if a directory was opened with [open_subtree] and
   later renamed, this might use the old name" -- and the last step of a commit
   protocol is exactly where that matters, since what a barrier reports durable
   has to be the directory whose entry was just renamed into place.

   Which inode an [fsync] reached is not observable from a program: nothing
   short of cutting power tells a barrier on one directory from a barrier on
   another. The resolution is observable, and these two cases pin it from both
   sides.

   Here the capability outlives the name. The directory is opened, then its
   name is taken away; the caller still holds the directory, so the barrier it
   asks for is the one that directory can give and there is nothing to fail. A
   barrier that re-resolves the name instead finds nothing under it and fails a
   publication that is already complete. *)
let test_sync_dir_barriers_the_held_directory () =
  with_real_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o700 (root / "d");
  Eio.Path.with_open_dir (root / "d") @@ fun d ->
  Eio.Path.rename (root / "d") (root / "moved");
  Eio_fs_unix.sync_dir d

(* And here the name outlives the directory, which is the shape that costs a
   publication rather than merely failing one. The store is moved aside and a
   link to a directory of the adversary's own left under its name: re-resolving
   the name follows the link, fsyncs a directory the caller never named, and
   returns, so the caller is told its rename reached stable storage while
   nothing of its own was flushed at all. Resolving through the capability
   refuses the link, its target being outside the tree the capability names, so
   the caller learns rather than being handed a barrier it did not get. *)
let test_sync_dir_refuses_a_name_a_link_took () =
  with_real_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o700 (root / "tree");
  Eio.Path.mkdir ~perm:0o700 (root / "elsewhere");
  Eio.Path.with_open_dir (root / "tree") @@ fun tree ->
  Eio.Path.mkdir ~perm:0o700 (tree / "store");
  (* The adversary writes this directory: the store goes aside and a link to a
     directory of their own takes its name. *)
  Eio.Path.rename (tree / "store") (tree / "moved");
  Eio.Path.symlink
    ~link_to:(Eio.Path.native_exn (root / "elsewhere"))
    (tree / "store");
  match Eio_fs_unix.sync_dir (tree / "store") with
  | () ->
      Alcotest.fail
        "the barrier reported a directory the caller never named durable"
  | exception (Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) as e) ->
      Alcotest.(check bool)
        "the refusal names the directory it was asked to barrier" true
        (mentions ~needle:"store" (Printexc.to_string e))

(* The barrier runs on a fiber that shares its domain with every other fiber of
   the program: a server publishes while it serves. So the open and the fsync
   belong on a system thread, not on the fiber's own thread -- an inline pair
   holds the domain's scheduler inside the C call for the whole barrier, which
   on Darwin is F_FULLFSYNC, through to the platter and the slowest call the
   process makes. Here one fiber issues a run of barriers while a sibling
   counts its own turns; the sibling gets none at all while the pair runs
   inline. *)
let test_sync_dir_lets_a_sibling_fiber_run () =
  with_real_dir @@ fun dir ->
  (* One turn every other barrier is a floor the fixed shape clears with room
     to spare -- it suspends once per barrier -- and the inline pair cannot
     reach at all, having no suspension point between the first barrier and
     the last. ([/] is [Eio.Path.( / )] here, so the floor is spelt out.) *)
  let barriers = 200 in
  let floor = 100 in
  let started = ref false in
  let finished = ref false in
  let turns = ref 0 in
  Eio.Fiber.both
    (fun () ->
      started := true;
      for _ = 1 to barriers do
        Eio_fs_unix.sync_dir dir
      done;
      finished := true)
    (fun () ->
      while not !started do
        Eio.Fiber.yield ()
      done;
      while not !finished do
        incr turns;
        Eio.Fiber.yield ()
      done);
  if !turns < floor then
    Alcotest.failf "sibling fiber ran %d times during %d barriers, wanted %d"
      !turns barriers floor

(* full_fsync accepts a regular file descriptor, including a read-only one. *)
let test_full_fsync_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "f") "x";
  let path = Eio.Path.native_exn (dir / "f") in
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Eio_fs_unix.full_fsync fd)

(* full_fsync pushes a real file's data to stable storage and surfaces the OS
   error on a dead descriptor -- on Darwin this exercises the F_FULLFSYNC path
   (and its fsync fallback), elsewhere plain fsync. Power-cut durability
   itself is not unit-testable; what is pinned here is that the barrier is
   issued on the right descriptor and failures are not swallowed. *)
let test_full_fsync_surfaces_errors () =
  let path = Filename.temp_file "eio-fs-fullfsync" ".dat" in
  let fd = Unix.openfile path [ Unix.O_WRONLY ] 0o644 in
  let written = Unix.write_substring fd "durable bytes" 0 13 in
  Alcotest.(check int) "wrote the payload" 13 written;
  Eio_fs_unix.full_fsync fd;
  Unix.close fd;
  Sys.remove path;
  match Eio_fs_unix.full_fsync fd with
  | () -> Alcotest.fail "full_fsync accepted a closed descriptor"
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> ()

(* ---- the file barrier asks the registration before the descriptor ----

   A wrapper that models power loss hands out the base's own descriptor, so a
   store can take a record lock on the file it is publishing or hand it to a
   launcher. That descriptor is the operations the operating system offers on
   an open file and not a second way past the wrapper: an fsync issued on it
   is a barrier the wrapper never saw, so the model records none and drops the
   bytes at the cut while the store was told they were durable. Reaching for
   the descriptor before asking the registration is therefore not a shortcut
   to the strongest barrier, it is the defect.

   Which descriptor an fsync reached is no more observable from a program than
   which inode a directory barrier reached, so the ordering is pinned the way
   it can be: this file carries a descriptor of this process that is already
   closed, which [Eio_unix.Fd.use_exn] refuses. Reaching for it raises, and
   answering elsewhere returns, so the same file tells both halves of the
   dispatch apart depending on whether it registers [File_sync]. *)
module Registered_file = struct
  type t = {
    base : Eio.File.rw_ty Eio.Resource.t;
    fd : Eio_unix.Fd.t;
    calls : int ref;
  }

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t.base buf
  let pread t ~file_offset bufs = Eio.File.pread t.base ~file_offset bufs
  let stat t = Eio.File.stat t.base
  let seek t off cmd = Eio.File.seek t.base off cmd
  let close t = Eio.Resource.close t.base
  let single_write t bufs = Eio.Flow.single_write t.base bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let pwrite t ~file_offset bufs =
    Eio.File.pwrite_single t.base ~file_offset bufs

  let sync t = Eio.File.sync t.base
  let truncate t len = Eio.File.truncate t.base len
end

let registered_file ~sw ~register base =
  let fd =
    Eio_unix.Fd.of_unix ~sw ~close_unix:true
      (Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0)
  in
  Eio_unix.Fd.close fd;
  let state = Registered_file.{ base; fd; calls = ref 0 } in
  let registration =
    if register then
      [
        Eio.Resource.H
          (Eio_fs_sync.File_sync, fun (t : Registered_file.t) -> incr t.calls);
      ]
    else []
  in
  let file : Eio.File.rw_ty Eio.Resource.t =
    Eio.Resource.T
      ( state,
        Eio.Resource.handler
          (registration
          @ [
              Eio.Resource.H
                (Eio_unix.Resource.T, fun (t : Registered_file.t) -> t.fd);
            ]
          @ Eio.Resource.bindings (Eio.File.Pi.rw (module Registered_file))) )
  in
  (file, state)

let test_sync_file_asks_the_registration_before_the_descriptor () =
  with_real_dir @@ fun dir ->
  Eio.Switch.run @@ fun sw ->
  let base = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (dir / "f") in
  let file, state = registered_file ~sw ~register:true base in
  Alcotest.(check bool)
    "the file carries a descriptor for the barrier to find" true
    (Option.is_some (Eio_unix.Resource.fd_opt file));
  (match Eio_fs_unix.sync_file file with
  | () -> ()
  | exception e ->
      Alcotest.failf
        "the barrier reached past the registration for the descriptor: %s"
        (Printexc.to_string e));
  Alcotest.(check int)
    "the registration answered the barrier" 1
    !(state.Registered_file.calls)

(* The other half, and the reason this entry point exists at all rather than
   deferring to the portable one. A file that registers no barrier is where the
   platform's own belongs: on Darwin F_FULLFSYNC, through to the platter, where
   the file's own sync stops at the drive's cache. That the descriptor is
   reached for is what the same closed descriptor says here, from the other
   side -- the barrier must raise. The portable barrier over the same file is
   what says the raise can only be the descriptor: it takes the file's own
   sync, which succeeds. So an implementation that quietly deferred to the
   portable route -- the durability guarantee traded away for a greener suite
   -- returns here where this one raises. *)
let test_sync_file_takes_the_descriptor_when_nothing_is_registered () =
  with_real_dir @@ fun dir ->
  Eio.Switch.run @@ fun sw ->
  let base = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (dir / "f") in
  let file, _ = registered_file ~sw ~register:false base in
  Eio_fs_sync.sync_file file;
  match Eio_fs_unix.sync_file file with
  | () ->
      Alcotest.fail
        "the barrier skipped the descriptor of a file that registers none"
  | exception Invalid_argument _ -> ()

(* The same ordering in the shape a store publishes in, over the real
   filesystem it runs on. The crash wrapper's file carries the host's own
   descriptor, so a barrier that reached for it would be a real F_FULLFSYNC
   the model never recorded -- and [Drop_unsynced] would then drop the image
   bytes the store was told had reached the platter.

   Two files take the same cut, and only one is barriered. The unbarriered one
   is the control: it says the model still drops what no barrier covered, so
   the barriered one surviving is the barrier being seen and not the model
   having gone soft. Both entries are committed, the names being the metadata
   journal's business, so what differs between the two is the data barrier
   alone. *)
let test_sync_file_under_the_crash_model_survives_the_cut () =
  with_real_dir @@ fun dir ->
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let wdir = Crash.wrap ctl dir in
  let write ~barrier name payload =
    Eio.Switch.run @@ fun sw ->
    let file =
      Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (wdir / name)
    in
    Alcotest.(check bool)
      "the wrapped file carries the host descriptor" true
      (Option.is_some (Eio_unix.Resource.fd_opt file));
    Eio.Flow.copy_string payload file;
    if barrier then Eio_fs_unix.sync_file file
  in
  write ~barrier:true "barriered" "payload";
  write ~barrier:false "unbarriered" "payload";
  Eio_fs_unix.sync_dir wdir;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the barriered bytes survived the cut" "payload"
    (Eio.Path.load (post / "barriered"));
  Alcotest.(check string)
    "and the unbarriered bytes did not" ""
    (Eio.Path.load (post / "unbarriered"))

(* A plain POSIX file registers nothing, so the barrier is its descriptor's
   own -- F_FULLFSYNC on Darwin. Power-cut durability is not unit-testable;
   what is pinned here is that the barrier is issued and the file reads back
   whole after it, the file half of [test_sync_dir_after_rename]. *)
let test_sync_file_on_a_plain_posix_file () =
  with_real_dir @@ fun dir ->
  Eio.Switch.run (fun sw ->
      let file =
        Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (dir / "f")
      in
      Eio.Flow.copy_string "payload" file;
      Eio_fs_unix.sync_file file);
  Alcotest.(check string)
    "the file reads back whole after its barrier" "payload"
    (Eio.Path.load (dir / "f"))

let test_statvfs_reports_raw_block_capacity () =
  with_real_dir @@ fun dir ->
  let usage = Eio_fs_unix.statvfs dir in
  Alcotest.(check bool)
    "filesystem block size is positive" true
    (Int64.compare usage.block_size 0L > 0);
  Alcotest.(check bool)
    "filesystem has blocks" true
    (Int64.compare usage.total_blocks 0L > 0);
  Alcotest.(check bool)
    "available blocks do not exceed total" true
    (Int64.compare usage.available_blocks usage.total_blocks <= 0)

(* The in-memory backend registers its barrier as a no-op -- volatile storage
   has nothing to sync -- so the platter-strength entry point runs unchanged
   over it too. *)
let test_sync_dir_in_memory_no_op () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> Eio_fs_unix.sync_dir env#fs

(* A second process holding the record lock over the rendezvous: the opener the
   mutex in this process cannot see, and the only one that tells a real
   exclusion from an in-process one. The child takes the lock, reports [ready],
   and holds it for [seconds]; the caller resumes once the lock is taken. *)
let hold_lock_in_another_process ~sw ~env ~clock ~lock ~ready ~seconds =
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      ~executable:self
      [
        self;
        "--hold-eio-fs-lock";
        Eio.Path.native_exn lock;
        Eio.Path.native_exn ready;
        seconds;
      ]
  in
  let rec await_ready () =
    if Eio.Path.is_file ready then ()
    else begin
      Eio.Time.sleep clock 0.01;
      await_ready ()
    end
  in
  await_ready ();
  child

let await_holder child =
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "lock holder exited %d" n
  | `Signaled n -> Alcotest.failf "lock holder took signal %d" n

let test_lock_wait_does_not_block_eio () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let child =
    hold_lock_in_another_process ~sw ~env ~clock ~lock:(dir / "lock")
      ~ready:(dir / "ready") ~seconds:"0.25"
  in
  let progressed = ref false in
  Eio.Switch.run @@ fun tick_sw ->
  Eio.Fiber.fork ~sw:tick_sw (fun () ->
      Eio.Time.sleep clock 0.05;
      progressed := true);
  Eio_fs_unix.with_lock (dir / "lock") (fun () -> ());
  Alcotest.(check bool)
    "another fiber progressed while waiting" true !progressed;
  await_holder child

let test_lock_serializes_fibers () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* ---- the exclusion is the file's, not the name's ----

   POSIX record locks do not exclude within a process: a second [lockf] over
   one file, taken through a second descriptor, is granted to the process that
   already holds it, and closing any one of those descriptors drops the lock
   for all of them. Between two fibers of this process the mutex is therefore
   the whole of the exclusion, and it excludes exactly what its key identifies.

   A name does not identify a file. Here one file is reached by two of them --
   a hard link, which is what a snapshot, a stale deployment, or a party who
   writes the rendezvous directory leaves behind -- and canonicalising does not
   merge them, [Unix.realpath] resolving symbolic links and a hard link being
   none of those. Two keys are two mutexes; the operating system grants the
   second acquisition; and both callers run their critical section over one
   file at once, with nothing in either run to say the exclusion was absent. *)
let test_lock_excludes_another_name_for_the_locked_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (dir / "lock") "";
  Unix.link
    (Eio.Path.native_exn (dir / "lock"))
    (Eio.Path.native_exn (dir / "alias"));
  let held, hold = Eio.Promise.create () in
  let checked, check = Eio.Promise.create () in
  let granted = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock (dir / "lock") @@ fun () ->
      Eio.Promise.resolve hold ();
      Eio.Promise.await checked)
    (fun () ->
      Eio.Promise.await held;
      (match
         Eio_fs_unix.with_lock ~block:false (dir / "alias") (fun () -> ())
       with
      | () -> granted := true
      | exception Eio_fs_unix.Lock_unavailable _ -> ());
      Eio.Promise.resolve check ());
  Alcotest.(check bool)
    "a second name for the held file was not granted the lock" false !granted

(* The key is resolved unconfined as well. [Unix.realpath] walks the name from
   [/], through the directories a party other than the caller writes, where the
   open that follows it resolves every component beneath the capability the
   path carries. Where the two disagree, realpath answers first and answers for
   the lock: a rendezvous under a parent link that points nowhere ends the
   acquisition in [Unix.Unix_error (ENOENT, "realpath", ...)] before the lock's
   own resolution has said anything -- a failure of the key, and none of the
   answers this interface gives, which are Eio's own for a path it declines and
   [Not_found] for a rendezvous that is not there. *)
let test_lock_answers_a_parent_link_that_leads_nowhere () =
  with_real_dir @@ fun dir ->
  Eio.Path.symlink ~link_to:"gone" (dir / "parent");
  match Eio_fs_unix.with_lock (dir / "parent" / "lock") (fun () -> ()) with
  | () ->
      Alcotest.fail "a rendezvous under a link that points nowhere was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let test_lock_updates_contents_in_place () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock ~contents:"first\n" lock (fun () -> ());
  let before = Eio.Path.stat ~follow:false lock in
  Eio_fs_unix.with_lock ~contents:"second\n" lock (fun () ->
      let while_locked = Eio.Path.stat ~follow:false lock in
      Alcotest.(check int64) "same locked inode" before.ino while_locked.ino;
      Alcotest.(check string)
        "current diagnostic" "second\n" (Eio.Path.load lock));
  let after = Eio.Path.stat ~follow:false lock in
  Alcotest.(check int64) "inode survives acquisition" before.ino after.ino

let test_lock_non_blocking_unavailable () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child =
    hold_lock_in_another_process ~sw ~env ~clock ~lock ~ready:(dir / "ready")
      ~seconds:"0.15"
  in
  (match Eio_fs_unix.with_lock ~block:false lock (fun () -> ()) with
  | () -> Alcotest.fail "non-blocking lock unexpectedly succeeded"
  | exception Eio_fs_unix.Lock_unavailable path ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  await_holder child

let test_lock_existing_does_not_create () =
  with_real_dir @@ fun dir ->
  let lock = dir / "absent" in
  (match Eio_fs_unix.with_lock ~create:`Never lock (fun () -> ()) with
  | () -> Alcotest.fail "an absent rendezvous was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ());
  Alcotest.(check bool)
    "probing does not create the rendezvous" false (Eio.Path.is_file lock)

(* ---- the lock path is the lock's own file ----

   A rendezvous lives in a directory another party writes, which is exactly
   where an entry gets planted under the name a locker is about to use. If the
   lock follows what it finds there, the exclusive lock lands on the entry's
   target: two processes then hold two locks over one resource, both read the
   same state and both publish, with nothing in either run to say the exclusion
   was absent. So the lock path must be its own regular file, and anything else
   under that name is refused.

   The refusal is Eio's own vocabulary for a path it declines to resolve --
   [Eio.Io (Eio.Fs.E (Permission_denied _))], as Eio raises for a name outside a
   sandbox -- and it names the lock path, because an operator reading the failure
   needs to know which entry is wrong. *)

let check_refusal what path fn =
  let native = Eio.Path.native_exn path in
  match fn () with
  | () -> Alcotest.failf "%s at the lock path was locked" what
  | exception (Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) as e) ->
      Alcotest.(check bool)
        (what ^ ": the refusal names the lock path")
        true
        (mentions ~needle:native (Printexc.to_string e))

(* A link to a sibling regular file: the shape an adversary plants, and the
   shape a stale deployment leaves behind. The sibling must come back neither
   locked nor written. *)
let test_lock_refuses_a_symlink_at_the_lock_path () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (dir / "target") "target\n";
  Eio.Path.symlink ~link_to:"target" (dir / "lock");
  check_refusal "a symbolic link" (dir / "lock") (fun () ->
      Eio_fs_unix.with_lock ~contents:"holder\n" (dir / "lock") (fun () -> ()));
  Alcotest.(check string)
    "the link's target was not written through the link" "target\n"
    (Eio.Path.load (dir / "target"));
  (* The target's own path still locks, so the refusal took nothing with it. *)
  Eio_fs_unix.with_lock (dir / "target") (fun () -> ())

(* A dangling link is the sharper case: creating the rendezvous through it
   creates a file wherever the link points, so the refusal has to come before
   the open, not after it. *)
let test_lock_refuses_a_dangling_symlink_without_creating_its_target () =
  with_real_dir @@ fun dir ->
  Eio.Path.symlink ~link_to:"elsewhere" (dir / "lock");
  check_refusal "a dangling symbolic link" (dir / "lock") (fun () ->
      Eio_fs_unix.with_lock (dir / "lock") (fun () -> ()));
  Alcotest.(check bool)
    "the link's target was not created" false
    (Eio.Path.is_file (dir / "elsewhere"))

(* ---- the window between the name check and the open ----

   The name is weighed before the file is opened and the descriptor after the
   lock is held, and in between the open resolves that same name a second time.
   An entry planted in that window is what the second resolution follows. It is
   a real window rather than a theoretical one: eio runs [stat] and [openat] on
   a system thread, so the fiber suspends at each and another fiber of the same
   domain -- let alone another process sharing the rendezvous directory -- runs
   between them.

   A test cannot hook between two calls it does not make, so the directory
   itself does the planting: [Plant_after_stat] answers a stat about the base
   directory as it stands, then takes the name it was asked about with a
   dangling symbolic link before returning. Everything else, including the
   descriptor an open returns, is the base directory's own. *)
module Plant_after_stat = struct
  type t = {
    base : Eio.Fs.dir_ty Eio.Resource.t;
    link_to : string;
    planted : bool ref;
  }

  let at t path = (t.base, path)

  let plant t path =
    if not !(t.planted) then begin
      t.planted := true;
      Eio.Path.symlink ~link_to:t.link_to (at t path)
    end

  let stat t ~follow path =
    Fun.protect
      ~finally:(fun () -> plant t path)
      (fun () -> Eio.Path.stat ~follow (at t path))

  let open_in t ~sw path = Eio.Path.open_in ~sw (at t path)

  let open_out t ~sw ~append ~create path =
    Eio.Path.open_out ~sw ~append ~create (at t path)

  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "plant-after-stat"

  let open_subtree _ ~sw:_ _ =
    failwith "Plant_after_stat: the lock opens no subtree"
end

let plant_after_stat ~link_to ((base, _) : Eio.Fs.dir_ty Eio.Path.t) =
  let t = { Plant_after_stat.base; link_to; planted = ref false } in
  (Eio.Resource.T
     ( t,
       Eio.Resource.handler
         [ Eio.Resource.H (Eio.Fs.Pi.Dir, (module Plant_after_stat)) ] )
    : Eio.Fs.dir_ty Eio.Resource.t)

(* A link planted in that window is followed by the open, and a creating open
   that follows one creates the file at the far end: an empty, owner-writable
   file wherever the party that planted the link chose, left behind after the
   lock itself was refused. Refusing is only half the guarantee; the other half
   is that the attempt created nothing. *)
let test_lock_creates_nothing_through_a_link_planted_after_the_check () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let racing =
    (plant_after_stat ~link_to:"victim" dir, Eio.Path.native_exn lock)
  in
  check_refusal "a link planted after the name check" lock (fun () ->
      Eio_fs_unix.with_lock racing (fun () -> ()));
  Alcotest.(check bool)
    "the planted link took the name" true
    ((Eio.Path.stat ~follow:false lock).kind = `Symbolic_link);
  Alcotest.(check bool)
    "the link's target was not created" false
    (Eio.Path.is_file (dir / "victim"))

(* Neither a directory nor a FIFO carries the record lock the interface
   promises, so both are refused in the same vocabulary rather than surfacing
   as whichever [open] error the platform happens to pick. *)
let test_lock_refuses_a_directory_or_a_fifo () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "as-a-directory");
  Unix.mkfifo (Eio.Path.native_exn (dir / "as-a-fifo")) 0o600;
  check_refusal "a directory" (dir / "as-a-directory") (fun () ->
      Eio_fs_unix.with_lock (dir / "as-a-directory") (fun () -> ()));
  check_refusal "a FIFO" (dir / "as-a-fifo") (fun () ->
      Eio_fs_unix.with_lock (dir / "as-a-fifo") (fun () -> ()))

(* An absent rendezvous is still created, as its own owner-only regular file:
   refusing what is not a regular file must not refuse the ordinary first
   acquisition. *)
let test_lock_creates_a_missing_rendezvous_as_a_regular_file () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Alcotest.(check bool)
    "no rendezvous before the first acquisition" false (Eio.Path.is_file lock);
  Eio_fs_unix.with_lock lock (fun () ->
      let held = Eio.Path.stat ~follow:false lock in
      Alcotest.(check bool)
        "the rendezvous is a regular file while held" true
        (held.kind = `Regular_file);
      Alcotest.(check int) "created owner-only" 0o600 held.perm);
  let after = Eio.Path.stat ~follow:false lock in
  Alcotest.(check bool)
    "the rendezvous survives as a regular file" true
    (after.kind = `Regular_file)

(* ---- a rendezvous that carries no descriptor ----

   The record lock is taken on a Unix descriptor, and a backend that is not the
   host filesystem hands out files carrying none. Two such backends exist here
   and one answer cannot serve both.

   This package's in-memory filesystem holds its files in this process's heap.
   No other process can open them: there is no name to open them by --
   [Eio.Path.native] answers [None] over it -- and no descriptor to inherit. So
   no opener outside this process exists to be excluded, and the in-process
   mutex is not a weaker lock there, it is the whole exclusion the backend
   admits. A unikernel filesystem is the same case.

   A wrapper over a real host directory is the other case and is nothing like
   it: the file it hands back is an ordinary host file, which any process opens
   by name and locks. Running under the mutex alone there puts two processes
   inside one critical section with nothing in either run to say the exclusion
   was absent -- the failure this interface exists to prevent. So a wrapper
   answers for the base it was given: over a host directory it passes that
   directory's own descriptors through, and the record lock taken under it is
   the host's; over a backend that admits no outside opener it forwards what
   the backend declares, and the mutex is the whole exclusion there as it is
   without the wrapper.

   So the missing descriptor is not the question. Whether the backend admits an
   opener outside this process is, and only the backend knows: one that admits
   none says so, and its rendezvous is held under the mutex alone. Every other
   rendezvous with no descriptor is refused -- loudly, as the barrier next door
   refuses a backend that registers none -- rather than locked in name only,
   silence being what turns a missing exclusion into a bug nobody sees. *)

let with_in_memory_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn env#fs

let test_lock_over_a_process_local_backend_serializes_fibers () =
  with_in_memory_fs @@ fun fs ->
  let lock = fs / "lock" in
  Alcotest.(check bool)
    "no name outside this process reaches the rendezvous" true
    (Option.is_none (Eio.Path.native lock));
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the critical section ran over a backend with no descriptors" true
    !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped;
  Alcotest.(check bool)
    "the rendezvous is the backend's own regular file" true
    ((Eio.Path.stat ~follow:false lock).kind = `Regular_file)

(* Holding under the mutex alone is not a licence to skip what the name is
   weighed for: the in-memory backend has links too, and a link at the lock
   name is not the lock's own file whatever the exclusion behind it is. *)
let test_lock_over_a_process_local_backend_refuses_a_symlink () =
  with_in_memory_fs @@ fun fs ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (fs / "target") "target\n";
  Eio.Path.symlink ~link_to:"target" (fs / "lock");
  (match
     Eio_fs_unix.with_lock ~contents:"holder\n" (fs / "lock") (fun () -> ())
   with
  | () -> Alcotest.fail "a symbolic link at the lock path was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
  Alcotest.(check string)
    "the link's target was not written through the link" "target\n"
    (Eio.Path.load (fs / "target"))

(* A wrapper over that backend is the same case, and it is the case a store
   under test is in: [Eio_fs.Chaos] perturbs or crash-models the tree beneath
   it, and over the in-memory backend the tree it perturbs is still this
   process's heap. Nothing the wrapper adds gives those files a name to open
   them by or a descriptor to inherit, so the exclusion they admit is the one
   the base declares, and the wrapper's business is to say so rather than to
   answer for a base it did not look at. A wrapper that declares nothing turns
   a rendezvous this interface holds under the mutex into one it refuses, which
   is the whole of a fault-injected store's locking gone. *)
let test_lock_over_a_wrapped_process_local_backend_serializes_fibers () =
  with_in_memory_fs @@ fun fs ->
  let wrapped = Chaos.wrap Chaos.Schedule.empty fs in
  let lock = wrapped / "lock" in
  Alcotest.(check bool)
    "the wrapper forwards what its base declares about outside openers" true
    (Eio_fs_sync.is_process_local (fst lock));
  Alcotest.(check bool)
    "no name outside this process reaches the rendezvous" true
    (Option.is_none (Eio.Path.native lock));
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the critical section ran through the wrapper" true !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* ---- the lock is granted in the order callers entered ----

   An acquisition suspends before it takes anything: the rendezvous is opened
   first, the key the fiber mutex is found by being the opened file's own
   device and inode, and a backend runs that open off the calling fiber. So the
   order two fibers reach the mutex in is the order their opens came back in,
   which is not the order they entered, and a caller cannot repair it from
   outside: between "I entered" and "I hold" it has no point to stand, short of
   a gate of its own around the whole call. What that costs is a queue that
   reorders its own writers -- a spool whose sequence numbers are issued inside
   the lock publishes them out of the order its callers were served in, which
   is what caravan's spool measured before it brought its own mutex
   (42a2799845).

   The suspension belongs to the backend, so a test that wants an exact
   interleaving brings its own: [Slow_open] delegates every operation to the
   in-memory backend and yields before an open, as many scheduler turns as the
   entering fiber asked for. Under [Eio_mock.Backend] the run queue is
   deterministic, so the fiber that enters first can be made to reach its own
   open last -- the reversal a real filesystem produces by itself, where the
   open is a worker thread's round trip and no order is promised at all. *)
module Slow_open = struct
  type t = { base : Eio.Fs.dir_ty Eio.Resource.t }

  (* How many scheduler turns the entering fiber spends inside each open. *)
  let turns : int Eio.Fiber.key = Eio.Fiber.create_key ()

  let dawdle () =
    match Eio.Fiber.get turns with
    | None | Some 0 -> ()
    | Some n ->
        for _ = 1 to n do
          Eio.Fiber.yield ()
        done

  let at t path = (t.base, path)

  let open_out t ~sw ~append ~create path =
    dawdle ();
    Eio.Path.open_out ~sw ~append ~create (at t path)

  let open_in t ~sw path =
    dawdle ();
    Eio.Path.open_in ~sw (at t path)

  let stat t ~follow path = Eio.Path.stat ~follow (at t path)
  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "slow-open"
  let open_subtree _ ~sw:_ _ = failwith "Slow_open: the lock opens no subtree"
end

let slow_open ((base, path) : Eio.Fs.dir_ty Eio.Path.t) =
  let dir =
    Eio.Resource.T
      ( { Slow_open.base },
        Eio.Resource.handler
          [
            Eio.Resource.H (Eio.Fs.Pi.Dir, (module Slow_open));
            (* The wrapper answers for the base it was given, as every other
               wrapper here does: over the in-memory backend the files it hands
               out are still this process's heap. *)
            Eio.Resource.H
              ( Eio_fs_sync.Process_local,
                fun (t : Slow_open.t) -> Eio_fs_sync.is_process_local t.base );
          ] )
  in
  ((dir :> Eio.Fs.dir_ty Eio.Resource.t), path)

let test_lock_is_granted_in_the_order_fibers_entered () =
  with_in_memory_fs @@ fun fs ->
  let lock = slow_open fs / "lock" in
  let entered = ref [] and held = ref [] in
  (* Each fiber dawdles for a different number of turns inside its opens, the
     first to enter for the longest, so entry order and open order are exact
     reverses of one another. *)
  let contend name turns () =
    Eio.Fiber.with_binding Slow_open.turns turns @@ fun () ->
    entered := name :: !entered;
    Eio_fs_unix.with_lock lock @@ fun () -> held := name :: !held
  in
  Eio.Fiber.all [ contend "a" 4; contend "b" 2; contend "c" 0 ];
  Alcotest.(check (list string))
    "the fibers entered in this order" [ "a"; "b"; "c" ] (List.rev !entered);
  Alcotest.(check (list string))
    "the lock was granted in the order the fibers entered" [ "a"; "b"; "c" ]
    (List.rev !held)

(* The other side of "a wrapper answers for its base": the crash model wraps a
   real host directory to put a store's durability under test, and the file it
   hands back is that directory's own file. Its exclusion is the host's record
   lock -- any process opens the file by name and takes it -- and the wrapper
   has none of its own to offer instead, so the descriptor passes through and
   the lock a store takes under the model is the lock it takes in production.

   Another process holds the record lock on that same host file throughout, so
   a non-blocking acquisition through the wrapper has one answer, and it names
   the rendezvous. Being granted the lock would mean the wrapper handed out an
   exclusion the operating system never gave; being refused for want of a
   descriptor would mean no crash-modelled store can lock at all. *)
let test_lock_through_a_crash_wrapper_takes_the_host_lock () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let child =
    hold_lock_in_another_process ~sw ~env ~clock ~lock:(dir / "lock")
      ~ready:(dir / "ready") ~seconds:"0.25"
  in
  let wrapped = Crash.wrap (Crash.v ~policy:Drop_unsynced ()) dir in
  let file =
    Eio.Path.open_out ~sw ~create:(`If_missing 0o600) (wrapped / "probe")
  in
  Alcotest.(check bool)
    "a file opened through the wrapper carries the host's descriptor" true
    (Option.is_some (Eio_unix.Resource.fd_opt file));
  (match
     Eio_fs_unix.with_lock ~block:false (wrapped / "lock") (fun () -> ())
   with
  | () ->
      Alcotest.fail
        "a wrapped rendezvous was locked while another process held its file"
  | exception Eio_fs_unix.Lock_unavailable path ->
      Alcotest.(check string)
        "the refusal names the host file the lock is held on"
        (Eio.Path.native_exn (dir / "lock"))
        path);
  await_holder child

(* What a lock writes is a write like any other. A holder that publishes its
   diagnostics through [~contents] writes them through the wrapper's own file,
   so the crash model sees that write and the policy decides whether it
   survives the cut -- there is nothing special about a rendezvous once the
   lock over it is taken. The rendezvous' own entry is committed in both runs
   below, so what differs between them is only the policy's answer about an
   un-synced write.

   This is what the refusal used to hide: before the descriptor passed through,
   the call was refused before it wrote anything, and what a lock journals
   could not be stated at all. *)
let crash_after_a_lock_published_its_contents ~policy =
  with_real_dir_env @@ fun _env dir ->
  let ctl = Crash.v ~policy () in
  let wrapped = Crash.wrap ctl dir in
  Eio_fs_unix.with_lock ~contents:"held\n" (wrapped / "lock") (fun () -> ());
  Alcotest.(check string)
    "the live rendezvous carries what the holder published" "held\n"
    (Eio.Path.load (dir / "lock"));
  Crash.sync_metadata ctl;
  Eio.Path.load (Crash.post_crash ctl / "lock")

let test_lock_contents_are_lost_by_an_unsynced_crash () =
  Alcotest.(check string)
    "the rendezvous keeps its committed entry and loses the un-synced write" ""
    (crash_after_a_lock_published_its_contents ~policy:Crash.Drop_unsynced)

let test_lock_contents_survive_a_policy_that_keeps_them () =
  Alcotest.(check string)
    "the write went through the wrapper, so the journal holds it" "held\n"
    (crash_after_a_lock_published_its_contents
       ~policy:(Crash.Survive (fun _ -> true)))

(* ---- a backend that hides descriptors and declares nothing ----

   Between the two answers above sits the wiring error they exist to tell
   apart: a directory whose files carry no descriptor and whose handler says
   nothing about who else can open them. It is built here rather than borrowed
   from a wrapper this package ships, because a wrapper that hides descriptors
   today passes them through tomorrow, and the case has to stay the case.

   Every operation is the host directory's own, the native name included, so
   the rendezvous is a real host file; only the files handed back are re-boxed
   behind the read and write operations alone, which is what an adapter written
   without a thought for descriptors produces. *)

module Hidden_ro = struct
  type t = Eio.File.ro_ty Eio.Resource.t

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t buf
  let pread t ~file_offset bufs = Eio.File.pread t ~file_offset bufs
  let stat t = Eio.File.stat t
  let seek t off cmd = Eio.File.seek t off cmd
  let close t = Eio.Resource.close t
end

module Hidden_rw = struct
  type t = Eio.File.rw_ty Eio.Resource.t

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t buf
  let pread t ~file_offset bufs = Eio.File.pread t ~file_offset bufs
  let stat t = Eio.File.stat t
  let seek t off cmd = Eio.File.seek t off cmd
  let close t = Eio.Resource.close t
  let single_write t bufs = Eio.Flow.single_write t bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let pwrite t ~file_offset bufs = Eio.File.pwrite_single t ~file_offset bufs
  let sync t = Eio.File.sync t
  let truncate t len = Eio.File.truncate t len
end

let hidden_ro base : Eio.File.ro_ty Eio.Resource.t =
  Eio.Resource.T (base, Eio.File.Pi.ro (module Hidden_ro))

let hidden_rw base : Eio.File.rw_ty Eio.Resource.t =
  Eio.Resource.T (base, Eio.File.Pi.rw (module Hidden_rw))

module Hide_descriptors = struct
  type t = Eio.Fs.dir_ty Eio.Resource.t

  let at t path = (t, path)
  let open_in t ~sw path = hidden_ro (Eio.Path.open_in ~sw (at t path))

  let open_out t ~sw ~append ~create path =
    hidden_rw (Eio.Path.open_out ~sw ~append ~create (at t path))

  let stat t ~follow path = Eio.Path.stat ~follow (at t path)
  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "hide-descriptors"

  let open_subtree _ ~sw:_ _ =
    failwith "Hide_descriptors: the lock opens no subtree"
end

let hide_descriptors ((base, path) : Eio.Fs.dir_ty Eio.Path.t) :
    Eio.Fs.dir_ty Eio.Path.t =
  ( (Eio.Resource.T
       ( base,
         Eio.Resource.handler
           [ Eio.Resource.H (Eio.Fs.Pi.Dir, (module Hide_descriptors)) ] )
      : Eio.Fs.dir_ty Eio.Resource.t),
    path )

(* The rendezvous is a host file another process is holding the record lock on
   right here, so the mutex in this process excludes nobody, and a caller let
   into its critical section would be the second writer of a resource one
   process already holds. It is refused instead, and the refusal names both the
   interface that made it and the rendezvous it was asked for, so a wiring
   error reads as one. *)
let test_lock_refuses_a_wrapper_that_hides_a_host_descriptor () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let child =
    hold_lock_in_another_process ~sw ~env ~clock ~lock:(dir / "lock")
      ~ready:(dir / "ready") ~seconds:"0.25"
  in
  let hidden = hide_descriptors dir in
  let ran = ref false in
  (match Eio_fs_unix.with_lock (hidden / "lock") (fun () -> ran := true) with
  | () ->
      Alcotest.fail
        "a rendezvous with no descriptor was locked while another process held \
         it"
  | exception Invalid_argument message ->
      Alcotest.(check bool)
        "the refusal names the interface that refused" true
        (mentions ~needle:"Eio_fs_unix.with_lock" message);
      Alcotest.(check bool)
        "the refusal names the rendezvous" true
        (mentions ~needle:(Eio.Path.native_exn (dir / "lock")) message));
  Alcotest.(check bool)
    "the critical section did not run over the held file" false !ran;
  await_holder child

let test_path_usage_counts_allocated_tree_without_following_links () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "a") "a";
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "sub/b") "payload";
  Eio.Path.symlink ~link_to:"sub" (dir / "link");
  let paths = [ dir; dir / "a"; dir / "sub"; dir / "sub/b"; dir / "link" ] in
  let expected_bytes, expected_mtime =
    List.fold_left
      (fun (bytes, mtime) path ->
        let stat = Eio.Path.stat ~follow:false path in
        ( Int64.add bytes (Int64.mul stat.blocks 512L),
          Float.max mtime stat.mtime ))
      (0L, 0.) paths
  in
  let usage = Eio_fs.Path.usage dir in
  Alcotest.(check int64)
    "allocated blocks, with the symlink counted once" expected_bytes
    usage.allocated_bytes;
  Alcotest.(check (float 0.))
    "latest tree mtime" expected_mtime usage.latest_mtime

(* A path reached straight through the unconfined filesystem is named by the
   spelling the host knows it under, so a message carrying that name hands its
   reader something to open, stat or remove rather than a label private to the
   program that printed it. *)
let test_name_is_the_host_spelling () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "image.raw") "payload";
  let name = Eio_fs.Path.name (dir / "image.raw") in
  Alcotest.(check string)
    "a path on the host filesystem is named by its native spelling"
    (Eio.Path.native_exn (dir / "image.raw"))
    name;
  Alcotest.(check bool)
    "the name picks the file out for anything outside the program" true
    (Sys.file_exists name)

(* A file reached through an opened directory has a second rendering, and it is
   the wrong one to name it by: a directory capability is labelled with the
   basename it was opened under, so Eio prints a member of it as
   "<label:member>", which points at no file on the host and matches no path the
   caller asked for. The name stays the full native path, so one file answers to
   one name whichever capability reached it. *)
let test_name_of_a_member_of_an_opened_subtree () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "shadow") "root:x:0:0:::\n";
  Eio.Switch.run @@ fun sw ->
  let member = Eio.Path.open_subtree ~sw dir / "shadow" in
  let name = Eio_fs.Path.name member in
  Alcotest.(check string)
    "a member of an opened subtree is named by its full native path"
    (Eio.Path.native_exn (dir / "shadow"))
    name;
  Alcotest.(check bool)
    "the name picks the file out for anything outside the program" true
    (Sys.file_exists name);
  Alcotest.(check string)
    "one file answers to one name, whichever capability reached it"
    (Eio_fs.Path.name (dir / "shadow"))
    name

let suite =
  ( "eio_fs_unix",
    [
      Alcotest.test_case "name is the host's spelling of the path" `Quick
        test_name_is_the_host_spelling;
      Alcotest.test_case "name of a member of an opened subtree" `Quick
        test_name_of_a_member_of_an_opened_subtree;
      Alcotest.test_case "barrier after rename" `Quick
        test_sync_dir_after_rename;
      Alcotest.test_case "barrier empty and repeated" `Quick
        test_sync_dir_empty_and_repeated;
      Alcotest.test_case "barrier lets a sibling fiber run" `Quick
        test_sync_dir_lets_a_sibling_fiber_run;
      Alcotest.test_case "barrier reaches the directory held" `Quick
        test_sync_dir_barriers_the_held_directory;
      Alcotest.test_case "barrier refuses a name a link took" `Quick
        test_sync_dir_refuses_a_name_a_link_took;
      Alcotest.test_case "file barrier asks the registration first" `Quick
        test_sync_file_asks_the_registration_before_the_descriptor;
      Alcotest.test_case "file barrier takes an unregistered file's descriptor"
        `Quick test_sync_file_takes_the_descriptor_when_nothing_is_registered;
      Alcotest.test_case "file barrier survives a modelled cut" `Quick
        test_sync_file_under_the_crash_model_survives_the_cut;
      Alcotest.test_case "file barrier on a plain POSIX file" `Quick
        test_sync_file_on_a_plain_posix_file;
      Alcotest.test_case "full_fsync on a file" `Quick test_full_fsync_file;
      Alcotest.test_case "full_fsync surfaces errors" `Quick
        test_full_fsync_surfaces_errors;
      Alcotest.test_case "statvfs reports raw block capacity" `Quick
        test_statvfs_reports_raw_block_capacity;
      Alcotest.test_case "in-memory barrier is a no-op" `Quick
        test_sync_dir_in_memory_no_op;
      Alcotest.test_case "lock wait does not block Eio" `Quick
        test_lock_wait_does_not_block_eio;
      Alcotest.test_case "lock serializes fibers" `Quick
        test_lock_serializes_fibers;
      Alcotest.test_case "lock excludes another name for its file" `Quick
        test_lock_excludes_another_name_for_the_locked_file;
      Alcotest.test_case "lock answers a parent link that leads nowhere" `Quick
        test_lock_answers_a_parent_link_that_leads_nowhere;
      Alcotest.test_case "lock contents stay on the locked inode" `Quick
        test_lock_updates_contents_in_place;
      Alcotest.test_case "non-blocking lock reports unavailable" `Quick
        test_lock_non_blocking_unavailable;
      Alcotest.test_case "existing-only lock does not create" `Quick
        test_lock_existing_does_not_create;
      Alcotest.test_case "lock refuses a symlink at the lock path" `Quick
        test_lock_refuses_a_symlink_at_the_lock_path;
      Alcotest.test_case "lock refuses a dangling symlink" `Quick
        test_lock_refuses_a_dangling_symlink_without_creating_its_target;
      Alcotest.test_case
        "lock creates nothing through a link planted after the check" `Quick
        test_lock_creates_nothing_through_a_link_planted_after_the_check;
      Alcotest.test_case "lock refuses a directory or a FIFO" `Quick
        test_lock_refuses_a_directory_or_a_fifo;
      Alcotest.test_case "missing rendezvous is created as a regular file"
        `Quick test_lock_creates_a_missing_rendezvous_as_a_regular_file;
      Alcotest.test_case "lock serializes fibers over a process-local backend"
        `Quick test_lock_over_a_process_local_backend_serializes_fibers;
      Alcotest.test_case "a process-local backend still refuses a symlink"
        `Quick test_lock_over_a_process_local_backend_refuses_a_symlink;
      Alcotest.test_case
        "lock serializes fibers over a wrapped process-local backend" `Quick
        test_lock_over_a_wrapped_process_local_backend_serializes_fibers;
      Alcotest.test_case "lock is granted in the order fibers entered" `Quick
        test_lock_is_granted_in_the_order_fibers_entered;
      Alcotest.test_case "lock through a crash wrapper takes the host lock"
        `Quick test_lock_through_a_crash_wrapper_takes_the_host_lock;
      Alcotest.test_case "an un-synced crash loses a lock's contents" `Quick
        test_lock_contents_are_lost_by_an_unsynced_crash;
      Alcotest.test_case "a lock's contents survive a policy that keeps them"
        `Quick test_lock_contents_survive_a_policy_that_keeps_them;
      Alcotest.test_case "lock refuses a wrapper that hides a host descriptor"
        `Quick test_lock_refuses_a_wrapper_that_hides_a_host_descriptor;
      Alcotest.test_case "path usage is allocated and symlink-safe" `Quick
        test_path_usage_counts_allocated_tree_without_following_links;
    ] )
