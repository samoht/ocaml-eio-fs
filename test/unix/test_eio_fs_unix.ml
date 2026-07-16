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
          Alcotest.test_case "refuses a non-native directory" `Quick
            test_sync_dir_refuses_non_native;
        ] );
    ]
