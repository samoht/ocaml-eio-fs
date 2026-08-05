(* Tests for the portable barrier: dispatch to the backend's registered
   operation, the POSIX route for a backend that registers nothing, the loud
   failure when the barrier cannot be issued, and the durable-save protocol.
   The full-strength POSIX facade on top is covered by test/unix. *)

let ( / ) = Eio.Path.( / )

let contains ~affix s =
  let n = String.length affix and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
  at 0

let config max_bytes = Eio_fs_sync.{ durable = true; perm = 0o644; max_bytes }

(* The in-memory backend registers its barrier (a no-op: volatile storage has
   nothing to sync), so the portable entry points work unchanged over it. *)
let test_registered_backend () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio_fs_sync.sync_dir env#fs;
  Eio_fs_sync.publish ~config:(config 3L) (env#fs / "cfg") (fun f ->
      Eio.Flow.copy_string "one" f);
  Alcotest.(check string) "published" "one" (Eio.Path.load (env#fs / "cfg"));
  Eio_fs_sync.publish ~config:(config 3L) (env#fs / "cfg") (fun f ->
      Eio.Flow.copy_string "two" f);
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
  Eio_fs_sync.publish ~config:(config 1L)
    (fs / dir / "cfg")
    (fun f -> Eio.Flow.copy_string "x" f);
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
     Eio_fs_sync.publish ~config:(config 1L)
       (env#fs / "missing-dir" / "f")
       (fun f -> Eio.Flow.copy_string "x" f)
   with
  | () -> Alcotest.fail "publish wrote into a missing directory"
  | exception Eio.Io _ -> ());
  Alcotest.(check bool)
    "no temporary left behind" true
    (Eio.Path.kind ~follow:false (env#fs / "missing-dir") = `Not_found)

(* publish takes a caller-chosen temporary, possibly in another directory on
   the same filesystem, so a watched target directory never sees temporary
   churn. The temporary is consumed by the rename. *)
let test_publish_with_remote_tmp () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "tmp");
  Eio.Path.mkdir ~perm:0o755 (env#fs / "live");
  Eio_fs_sync.publish ~config:(config 7L)
    ~tmp:(env#fs / "tmp" / "stage-1")
    (env#fs / "live" / "cfg")
    (fun f -> Eio.Flow.copy_string "payload" f);
  Alcotest.(check string)
    "published" "payload"
    (Eio.Path.load (env#fs / "live" / "cfg"));
  Alcotest.(check bool)
    "temporary consumed" true
    (Eio.Path.kind ~follow:false (env#fs / "tmp" / "stage-1") = `Not_found)

let test_publish_with_stream () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let result =
    Eio_fs_sync.publish ~config:(config 7L) ~tmp:(env#fs / "stream.tmp")
      (env#fs / "stream") (fun file ->
        Eio.Flow.copy_string "one" file;
        Eio.Flow.copy_string "-two" file;
        42)
  in
  Alcotest.(check int) "callback result" 42 result;
  Alcotest.(check string)
    "streamed payload" "one-two"
    (Eio.Path.load (env#fs / "stream"));
  Alcotest.(check bool)
    "temporary consumed" true
    (Eio.Path.kind ~follow:false (env#fs / "stream.tmp") = `Not_found)

let test_publish_with_failure_preserves_target () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "live") "old";
  (match
     Eio_fs_sync.publish ~config:(config 7L) ~tmp:(env#fs / "live.tmp")
       (env#fs / "live") (fun file ->
         Eio.Flow.copy_string "partial" file;
         raise Exit)
   with
  | _ -> Alcotest.fail "failing producer unexpectedly published"
  | exception Exit -> ());
  Alcotest.(check string)
    "old target preserved" "old"
    (Eio.Path.load (env#fs / "live"));
  Alcotest.(check bool)
    "failed temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "live.tmp") = `Not_found)

let test_publish_rejects_target_as_temporary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let live = env#fs / "live" in
  Eio.Path.save ~create:(`Or_truncate 0o644) live "old";
  (match
     Eio_fs_sync.publish ~config:(config 3L) ~tmp:live live
       (Eio.Flow.copy_string "new")
   with
  | () -> Alcotest.fail "target accepted as its own temporary"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string) "target unchanged" "old" (Eio.Path.load live)

(* Two independently wrapped capabilities can name the same path without being
   physically equal. The post-open identity check must still reject that alias,
   and exclusive creation makes the absent-target case recoverable. *)
let test_publish_rejects_aliased_target () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio.Path.with_subtree env#fs @@ fun left ->
  Eio.Path.with_subtree env#fs @@ fun right ->
  (match
     Eio_fs_sync.publish ~config:(config 3L) ~tmp:(left / "same")
       (right / "same")
       (Eio.Flow.copy_string "new")
   with
  | () -> Alcotest.fail "aliased target accepted as its own temporary"
  | exception Invalid_argument _ -> ());
  Alcotest.(check bool)
    "partial target removed" true
    (Eio.Path.kind ~follow:false (env#fs / "same") = `Not_found)

let test_publish_does_not_remove_unowned_temporary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let tmp = env#fs / "foreign.tmp" in
  Eio.Path.save ~create:(`Or_truncate 0o644) tmp "foreign";
  (match
     Eio_fs_sync.publish ~config:(config 3L) ~tmp (env#fs / "live")
       (Eio.Flow.copy_string "new")
   with
  | () -> Alcotest.fail "exclusive publish replaced a foreign temporary"
  | exception Eio.Io _ -> ());
  Alcotest.(check string)
    "foreign temporary preserved" "foreign" (Eio.Path.load tmp)

let test_publish_enforces_byte_limit () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let live = env#fs / "live" in
  Eio.Path.save ~create:(`Or_truncate 0o644) live "old";
  (match
     Eio_fs_sync.publish ~config:(config 4L) live (fun file ->
         Eio.Flow.copy_string "123" file;
         Eio.Flow.copy_string "45" file)
   with
  | () -> Alcotest.fail "publication exceeded its byte limit"
  | exception Eio_fs_sync.Limit_exceeded { max_bytes; attempted } ->
      Alcotest.(check int64) "limit" 4L max_bytes;
      Alcotest.(check int64) "attempted" 5L attempted);
  Alcotest.(check string) "old target preserved" "old" (Eio.Path.load live);
  Alcotest.(check bool)
    "oversized temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "live.tmp") = `Not_found)

(* The durability flag has teeth: under the crash model a default publish
   survives Drop_unsynced on its own barriers, and an explicitly non-durable
   one loses its rename -- the window the caller opted into. *)
let test_publish_durability_flag () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let module Crash = Eio_fs.Chaos.Crash in
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let fs = Crash.wrap ctl env#fs in
  Eio_fs_sync.publish ~config:(config 1L) ~tmp:(fs / "safe-tmp") (fs / "safe")
    (fun f -> Eio.Flow.copy_string "s" f);
  Eio_fs_sync.publish
    ~config:Eio_fs_sync.{ durable = false; perm = 0o644; max_bytes = 1L }
    ~tmp:(fs / "fast-tmp") (fs / "fast")
    (fun f -> Eio.Flow.copy_string "f" f);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "durable publish survives" "s"
    (Eio.Path.load Eio.Path.(post / "safe"));
  Alcotest.(check bool)
    "non-durable publish lost" true
    (Eio.Path.kind ~follow:false Eio.Path.(post / "fast") = `Not_found)

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
      Alcotest.test_case "publish with a remote temporary" `Quick
        test_publish_with_remote_tmp;
      Alcotest.test_case "publish with a streaming producer" `Quick
        test_publish_with_stream;
      Alcotest.test_case "failed producer preserves target" `Quick
        test_publish_with_failure_preserves_target;
      Alcotest.test_case "temporary differs from target" `Quick
        test_publish_rejects_target_as_temporary;
      Alcotest.test_case "aliased temporary differs from target" `Quick
        test_publish_rejects_aliased_target;
      Alcotest.test_case "foreign temporary is preserved" `Quick
        test_publish_does_not_remove_unowned_temporary;
      Alcotest.test_case "publication enforces byte limit" `Quick
        test_publish_enforces_byte_limit;
      Alcotest.test_case "publish durability flag" `Quick
        test_publish_durability_flag;
    ] )
