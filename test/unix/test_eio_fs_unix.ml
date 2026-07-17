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

(* The in-memory backend registers its barrier as a no-op -- volatile storage
   has nothing to sync -- so the platter-strength entry point runs unchanged
   over it too. *)
let test_sync_dir_in_memory_no_op () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> Eio_fs_unix.sync_dir env#fs

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
      Alcotest.test_case "in-memory barrier is a no-op" `Quick
        test_sync_dir_in_memory_no_op;
    ] )
