let ( / ) = Eio.Path.( / )

let expect_io_error expected fn =
  match fn () with
  | () -> Alcotest.failf "expected %s" expected
  | exception Eio.Io (Eio.Fs.E error, _) -> (
      match (expected, error) with
      | "not found", Eio.Fs.Not_found _ -> ()
      | "already exists", Eio.Fs.Already_exists _ -> ()
      | "permission denied", Eio.Fs.Permission_denied _ -> ()
      | _ -> Alcotest.failf "expected %s, got another filesystem error" expected
      )

let with_env ?cwd fn = Eio_mock.Backend.run @@ fun () -> Eio_fs.run ?cwd fn

let test_missing_file () =
  with_env @@ fun env ->
  expect_io_error "not found" (fun () ->
      ignore (Eio.Path.load Eio.Path.(env#fs / "missing")))

let test_exclusive_create () =
  with_env @@ fun env ->
  let path = env#fs / "exists" in
  Eio.Path.save ~create:(`Or_truncate 0o600) path "first";
  expect_io_error "already exists" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o600) path "second");
  Alcotest.(check string) "original contents" "first" (Eio.Path.load path)

let test_cwd_parent_escape () =
  with_env ~cwd:"/sandbox" @@ fun env ->
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / "/sandbox");
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(env#fs / "/outside")
    "secret";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load Eio.Path.(env#cwd / "../outside")))

let test_cwd_symlink_escape () =
  with_env ~cwd:"/sandbox" @@ fun env ->
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / "/sandbox");
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(env#fs / "/outside")
    "secret";
  Eio.Path.symlink ~link_to:"../outside" Eio.Path.(env#fs / "/sandbox/link");
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load Eio.Path.(env#cwd / "link")))

(* {2 POSIX permission semantics}

   The in-memory backend must model open(2) faithfully enough for
   fault-injection tests to be meaningful: a created file records the mode the
   caller asked for, an existing file without the owner-write bit cannot be
   reopened for writing (EACCES), and the open that creates a file is never
   restricted by the mode it sets. *)

(* open(2): "the mode argument specifies the file mode bits to be applied when
   a new file is created". The recorded mode is what the caller passed, not the
   open flags. *)
let test_create_mode_recorded () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o444) (env#fs / "ro") "x";
  let stat = Eio.Path.stat ~follow:true (env#fs / "ro") in
  Alcotest.(check int) "created file records its mode" 0o444 stat.perm

(* open(2): EACCES when "the file exists and the permissions specified by
   oflag are denied". Reopening an existing 0o444 file for writing must fail,
   even for the owner. *)
let test_readonly_reopen_denied () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o444) (env#fs / "ro") "x";
  expect_io_error "permission denied" (fun () ->
      Eio.Path.with_open_out ~create:(`Or_truncate 0o444) (env#fs / "ro")
        (fun _ -> ()))

(* open(2): the mode of a freshly created file does not restrict the creating
   open itself -- O_CREAT|O_RDWR with mode 0444 yields a writable fd. *)
let test_readonly_fresh_create_writable () =
  with_env @@ fun env ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o444) (env#fs / "ro") (fun f ->
      Eio.Flow.copy_string "data" f);
  Alcotest.(check string)
    "fresh read-only file accepted writes" "data"
    (Eio.Path.load (env#fs / "ro"))

(* chmod(2) restores writability: after adding the owner-write bit an existing
   read-only file reopens for writing. *)
let test_chmod_restores_writability () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o444) (env#fs / "ro") "x";
  Eio.Path.chmod ~follow:true ~perm:0o644 (env#fs / "ro");
  Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "ro") "y";
  Alcotest.(check string) "rewritten" "y" (Eio.Path.load (env#fs / "ro"))

(* open(2): EACCES also when read permission is missing. A write-only file
   can be created -- the creating open is unrestricted -- but not reopened:
   this backend opens files O_RDONLY or O_RDWR, and both need the read bit. *)
let test_unreadable_reopen_denied () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o200) (env#fs / "wo") "x";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load (env#fs / "wo")));
  expect_io_error "permission denied" (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o200) (env#fs / "wo") "y")

(* A directory capability opened for a switch dies with the switch, as a real
   O_DIRECTORY fd does: using it afterwards is EBADF, not silent success. *)
let test_subtree_fd_dies_with_switch () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "d/f") "x";
  let leaked = ref None in
  Eio.Switch.run (fun sw ->
      leaked := Some (Eio.Path.open_dir ~sw (env#fs / "d")));
  let stale = Option.get !leaked in
  match Eio.Path.load Eio.Path.(stale / "f") with
  | (_ : string) ->
      Alcotest.fail "stale directory fd still resolves after its switch exited"
  | exception Eio.Io _ -> ()

(* {2 Descriptors that are not regular files}

   Opening a directory as a flow is a supported operation: eio-fs.sync reaches a
   directory's write pi that way, and both real Eio backends allow the O_RDONLY
   open. The descriptor that comes back still answers the whole file interface,
   so every operation on it has to fail the way a real backend fails one.
   read(2) and write(2) on a directory descriptor are EISDIR, which Eio raises
   as an [Eio.Io]; a bare [Failure] would leave a caller that handles Eio's
   error model correctly crashing on this backend alone. *)

let expect_eio_io what fn =
  match fn () with
  | () -> Alcotest.failf "expected %s to fail" what
  | exception Eio.Io _ -> ()

let test_read_directory_fd () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  Eio.Path.with_open_in (env#fs / "d") @@ fun flow ->
  let buf = Cstruct.create 8 in
  expect_eio_io "reading a directory descriptor" (fun () ->
      ignore (Eio.Flow.single_read flow buf : int));
  expect_eio_io "positioned read of a directory descriptor" (fun () ->
      ignore (Eio.File.pread flow ~file_offset:Optint.Int63.zero [ buf ] : int))

let test_write_directory_fd () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  Eio.Path.with_open_out ~create:`Never (env#fs / "d") @@ fun flow ->
  let buf = Cstruct.of_string "x" in
  expect_eio_io "writing to a directory descriptor" (fun () ->
      ignore (Eio.Flow.single_write flow [ buf ] : int));
  expect_eio_io "positioned write to a directory descriptor" (fun () ->
      ignore
        (Eio.File.pwrite_single flow ~file_offset:Optint.Int63.zero [ buf ]
          : int))

(* open(2) with O_CREAT answers ENOTDIR when a component of the path prefix is
   not a directory. This backend already reports a walk through a regular file
   as a path that does not resolve; creating under one must agree instead of
   escaping the error model. *)
let test_create_under_regular_file () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (env#fs / "reg") "x";
  expect_eio_io "creating a file under a regular file" (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o600) (env#fs / "reg/child") "y")

(* ENOTDIR is an error of the filesystem's own model, not a caller mistake:
   with a regular file in the tree, every directory operation that walks
   through it must answer through [Eio.Io], not an [Invalid_argument] this
   backend alone raises. The five below are one operation set and must agree,
   so [Not_found] is pinned for all five. *)
let test_dir_ops_under_regular_file () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Or_truncate 0o600) (fs / "reg") "x";
  expect_io_error "not found" (fun () ->
      Eio.Path.mkdir ~perm:0o755 (fs / "reg" / "sub"));
  expect_io_error "not found" (fun () -> Eio.Path.unlink (fs / "reg" / "child"));
  expect_io_error "not found" (fun () ->
      Eio.Path.symlink ~link_to:"t" (fs / "reg" / "l"));
  expect_io_error "not found" (fun () -> Eio.Path.rmdir (fs / "reg" / "sub"));
  expect_io_error "not found" (fun () ->
      ignore (Eio.Path.read_dir (fs / "reg" / "sub") : string list))

let suite =
  ( "low_level",
    [
      Alcotest.test_case "missing file" `Quick test_missing_file;
      Alcotest.test_case "exclusive create" `Quick test_exclusive_create;
      Alcotest.test_case "cwd parent escape" `Quick test_cwd_parent_escape;
      Alcotest.test_case "cwd symlink escape" `Quick test_cwd_symlink_escape;
      Alcotest.test_case "create records mode" `Quick test_create_mode_recorded;
      Alcotest.test_case "read-only reopen denied" `Quick
        test_readonly_reopen_denied;
      Alcotest.test_case "unreadable reopen denied" `Quick
        test_unreadable_reopen_denied;
      Alcotest.test_case "fresh read-only create writable" `Quick
        test_readonly_fresh_create_writable;
      Alcotest.test_case "chmod restores writability" `Quick
        test_chmod_restores_writability;
      Alcotest.test_case "subtree fd dies with switch" `Quick
        test_subtree_fd_dies_with_switch;
      Alcotest.test_case "read directory fd" `Quick test_read_directory_fd;
      Alcotest.test_case "write directory fd" `Quick test_write_directory_fd;
      Alcotest.test_case "create under regular file" `Quick
        test_create_under_regular_file;
      Alcotest.test_case "directory ops under a regular file" `Quick
        test_dir_ops_under_regular_file;
    ] )
