(* Smoke tests for [Eio_fs_unix]: the barriers run against a real filesystem
   and surface errors as documented. Durability itself is not observable
   without cutting power; what is testable is that the barrier accepts the
   states a commit protocol hands it. *)

let ( / ) = Eio.Path.( / )

let with_real_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-dir" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () -> fn (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t)

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

let test_lock_wait_does_not_block_eio () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let root = Filename.temp_dir "eio-fs-lock" "" in
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / root))
  @@ fun () ->
  let lock = Filename.concat root "lock" in
  let ready = Filename.concat root "ready" in
  Eio.Switch.run @@ fun sw ->
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      ~executable:self
      [ self; "--hold-eio-fs-lock"; lock; ready; "0.25" ]
  in
  let rec await_ready () =
    if Eio.Path.is_file Eio.Path.(fs / ready) then ()
    else begin
      Eio.Time.sleep clock 0.01;
      await_ready ()
    end
  in
  await_ready ();
  let progressed = ref false in
  Eio.Switch.run @@ fun tick_sw ->
  Eio.Fiber.fork ~sw:tick_sw (fun () ->
      Eio.Time.sleep clock 0.05;
      progressed := true);
  Eio_fs_unix.with_lock Eio.Path.(fs / lock) (fun () -> ());
  Alcotest.(check bool)
    "another fiber progressed while waiting" true !progressed;
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "lock holder exited %d" n
  | `Signaled n -> Alcotest.failf "lock holder took signal %d" n

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
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let root = Filename.temp_dir "eio-fs-lock" "" in
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / root))
  @@ fun () ->
  let lock = Filename.concat root "lock" in
  let ready = Filename.concat root "ready" in
  Eio.Switch.run @@ fun sw ->
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      ~executable:self
      [ self; "--hold-eio-fs-lock"; lock; ready; "0.15" ]
  in
  let rec await_ready () =
    if Eio.Path.is_file Eio.Path.(fs / ready) then ()
    else begin
      Eio.Time.sleep clock 0.01;
      await_ready ()
    end
  in
  await_ready ();
  (match
     Eio_fs_unix.with_lock ~block:false Eio.Path.(fs / lock) (fun () -> ())
   with
  | () -> Alcotest.fail "non-blocking lock unexpectedly succeeded"
  | exception Eio_fs_unix.Lock_unavailable path ->
      Alcotest.(check string) "unavailable path" lock path);
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "lock holder exited %d" n
  | `Signaled n -> Alcotest.failf "lock holder took signal %d" n

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

let mentions ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  at 0

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

let suite =
  ( "eio_fs_unix",
    [
      Alcotest.test_case "barrier after rename" `Quick
        test_sync_dir_after_rename;
      Alcotest.test_case "barrier empty and repeated" `Quick
        test_sync_dir_empty_and_repeated;
      Alcotest.test_case "barrier lets a sibling fiber run" `Quick
        test_sync_dir_lets_a_sibling_fiber_run;
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
      Alcotest.test_case "path usage is allocated and symlink-safe" `Quick
        test_path_usage_counts_allocated_tree_without_following_links;
    ] )
