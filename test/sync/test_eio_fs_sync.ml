(* Tests for the portable barrier: dispatch to the backend's registered
   operation, the loud failure when none is registered, and the durable-save
   protocol over a registering backend. The POSIX facade on top is covered by
   test/unix. *)

let ( / ) = Eio.Path.( / )

(* The in-memory backend registers its barrier (a no-op: volatile storage has
   nothing to sync), so the portable entry points work unchanged over it. *)
let test_registered_backend () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio_fs_sync.sync_dir env#fs;
  Eio_fs_sync.save_durable ~create:(`Or_truncate 0o644) (env#fs / "cfg") "one";
  Alcotest.(check string) "published" "one" (Eio.Path.load (env#fs / "cfg"));
  Eio_fs_sync.save_durable ~create:(`Or_truncate 0o644) (env#fs / "cfg") "two";
  Alcotest.(check string) "replaced" "two" (Eio.Path.load (env#fs / "cfg"));
  Alcotest.(check bool)
    "temporary cleaned" true
    (Eio.Path.kind ~follow:false (env#fs / "cfg.tmp") = `Not_found)

(* A backend with no registration is a wiring error, loudly: silence would
   silently lose durability. The plain POSIX backend registers nothing (the
   capability comes from Eio_fs_unix.wrap, tested in test/unix). *)
let test_unregistered_backend_raises () =
  Eio_main.run @@ fun env ->
  match Eio_fs_sync.sync_dir (Eio.Stdenv.fs env) with
  | () -> Alcotest.fail "sync_dir accepted an unregistered backend"
  | exception Failure _ -> ()

(* A failed write cleans its temporary up and the failure propagates. *)
let test_failed_save_cleans_temporary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  (match
     Eio_fs_sync.save_durable ~create:(`Or_truncate 0o644)
       (env#fs / "missing-dir" / "f")
       "x"
   with
  | () -> Alcotest.fail "save_durable wrote into a missing directory"
  | exception Eio.Io _ -> ());
  Alcotest.(check bool)
    "no temporary left behind" true
    (Eio.Path.kind ~follow:false (env#fs / "missing-dir") = `Not_found)

let suite =
  ( "eio_fs_sync",
    [
      Alcotest.test_case "registered backend" `Quick test_registered_backend;
      Alcotest.test_case "unregistered backend raises" `Quick
        test_unregistered_backend_raises;
      Alcotest.test_case "failed save cleans temporary" `Quick
        test_failed_save_cleans_temporary;
    ] )
