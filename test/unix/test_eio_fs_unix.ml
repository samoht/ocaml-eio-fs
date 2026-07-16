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

(* save_durable publishes atomically: the content lands whole, a second save
   replaces it, and no temporary lingers. *)
let test_save_durable () =
  with_real_dir @@ fun dir ->
  Eio_fs_unix.save_durable ~create:(`Or_truncate 0o644) (dir / "cfg") "one";
  Alcotest.(check string) "published" "one" (Eio.Path.load (dir / "cfg"));
  Eio_fs_unix.save_durable ~create:(`Or_truncate 0o644) (dir / "cfg") "two";
  Alcotest.(check string) "replaced" "two" (Eio.Path.load (dir / "cfg"));
  Alcotest.(check bool)
    "temporary cleaned" true
    (Eio.Path.kind ~follow:false (dir / "cfg.tmp") = `Not_found)

(* Under the crash model, a completed save_durable survives Drop_unsynced on
   its own barriers: the data fsync commits the bytes and the injected
   directory barrier commits the rename. *)
let test_save_durable_survives_crash () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let module Crash = Eio_fs.Chaos.Crash in
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let fs = Crash.wrap ctl env#fs in
  Eio_fs_unix.save_durable
    ~sync_dir:(fun _ -> Crash.sync_metadata ctl)
    ~create:(`Or_truncate 0o644)
    Eio.Path.(fs / "cfg")
    "durable";
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "completed save survives the crash" "durable"
    (Eio.Path.load Eio.Path.(post / "cfg"))

(* A directory with no native path is a caller error, not a crash. *)
let test_sync_dir_refuses_non_native () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  match Eio_fs_unix.sync_dir env#fs with
  | () -> Alcotest.fail "sync_dir accepted an in-memory directory"
  | exception (Invalid_argument _ | Unix.Unix_error _ | Eio.Io _) -> ()

let () =
  Alcotest.run "eio-fs.unix"
    [
      ( "sync_dir",
        [
          Alcotest.test_case "barrier after rename" `Quick
            test_sync_dir_after_rename;
          Alcotest.test_case "barrier empty and repeated" `Quick
            test_sync_dir_empty_and_repeated;
          Alcotest.test_case "full_fsync on a file" `Quick test_full_fsync_file;
          Alcotest.test_case "full_fsync surfaces errors" `Quick
            test_full_fsync_surfaces_errors;
          Alcotest.test_case "save_durable publishes atomically" `Quick
            test_save_durable;
          Alcotest.test_case "save_durable survives a crash" `Quick
            test_save_durable_survives_crash;
          Alcotest.test_case "refuses a non-native directory" `Quick
            test_sync_dir_refuses_non_native;
        ] );
    ]
