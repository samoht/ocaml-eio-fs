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
      Alcotest.test_case "path usage is allocated and symlink-safe" `Quick
        test_path_usage_counts_allocated_tree_without_following_links;
    ] )
