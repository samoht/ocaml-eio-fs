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
  | exception exn ->
      Alcotest.failf "expected %s, got %s" expected (Printexc.to_string exn)

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

let suite =
  ( "low_level",
    [
      Alcotest.test_case "missing file" `Quick test_missing_file;
      Alcotest.test_case "exclusive create" `Quick test_exclusive_create;
      Alcotest.test_case "cwd parent escape" `Quick test_cwd_parent_escape;
      Alcotest.test_case "cwd symlink escape" `Quick test_cwd_symlink_escape;
    ] )
