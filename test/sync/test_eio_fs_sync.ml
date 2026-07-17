(* Tests for the portable barrier: dispatch to the backend's registered
   operation, the POSIX route for a backend that registers nothing, the loud
   failure when the barrier cannot be issued, and the durable-save protocol.
   The full-strength POSIX facade on top is covered by test/unix. *)

let ( / ) = Eio.Path.( / )

let contains ~affix s =
  let n = String.length affix and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
  at 0

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

(* A plain POSIX directory needs no registration and no wrapper: the barrier
   opens the directory read-only and fsyncs its handle. This pins the backend
   behavior the route rests on (a directory opens, its handle syncs), so an
   eio upgrade that drops either fails here and not in a store. *)
let test_posix_needs_no_registration () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-posix" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () ->
  Eio_fs_sync.sync_dir (fs / dir);
  Eio_fs_sync.save_durable ~create:(`Or_truncate 0o644) (fs / dir / "cfg") "x";
  Alcotest.(check string) "published" "x" (Eio.Path.load (fs / dir / "cfg"))

(* A barrier that cannot be issued surfaces loudly, with context pointing at
   the Dir_sync registration -- never a silent no-op. *)
let test_unissuable_barrier_raises () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  match Eio_fs_sync.sync_dir (fs / "/no-such-dir-for-eio-fs-sync") with
  | () -> Alcotest.fail "sync_dir accepted a missing directory"
  | exception (Eio.Io _ as e) ->
      Alcotest.(check bool)
        "error context names Dir_sync" true
        (contains ~affix:"Dir_sync" (Printexc.to_string e))

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
      Alcotest.test_case "posix needs no registration" `Quick
        test_posix_needs_no_registration;
      Alcotest.test_case "unissuable barrier raises" `Quick
        test_unissuable_barrier_raises;
      Alcotest.test_case "failed save cleans temporary" `Quick
        test_failed_save_cleans_temporary;
    ] )
