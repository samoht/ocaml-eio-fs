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

(* The published file carries exactly [config.perm]. A create's mode is
   narrowed by the process umask (open(2): "the mode of the created file is
   (mode & ~umask)"), so a mode the caller named survives only if publish
   restates it on the temporary before the rename. The umask here clears the
   group and other read bits a 0o644 names, so a publish that leaves the
   create's mode alone reads 0o600 back. *)
let test_publish_honours_perm_under_umask () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-perm" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  let previous = Unix.umask 0o077 in
  Fun.protect ~finally:(fun () ->
      ignore (Unix.umask previous : int);
      Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () ->
  Eio_fs_sync.publish
    ~config:Eio_fs_sync.{ durable = true; perm = 0o644; max_bytes = 1L }
    (fs / dir / "cfg")
    (fun f -> Eio.Flow.copy_string "x" f);
  let stat = Eio.Path.stat ~follow:false (fs / dir / "cfg") in
  Alcotest.(check int) "published mode is config.perm" 0o644 stat.perm

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

(* ---- the file-data barrier dispatches like the directory one ----

   A commit protocol makes two things durable: the bytes it wrote and the
   directory entry that names them. [sync_file] is the first half, and it
   dispatches the way [sync_dir] does -- through the operation the backend
   registers, [File_sync]. A file is itself the resource, so that operation
   takes no name where the directory's takes a [~path].

   The registration is what makes the barrier visible to a wrapper, and a
   wrapper that models power loss has to see the barrier the protocol issued
   or it drops data the protocol was told was durable. So it is answered
   whole: a caller that also synced the file behind the registration would
   hand that model a barrier it never recorded, which is the same defect from
   the other end.

   One wrapper pins both sides. It counts what the registration was asked and
   what reached the file underneath it, so a barrier that answers in the wrong
   place is a count in the wrong counter rather than a silent pass. *)
module Counting_file = struct
  type t = {
    base : Eio.File.rw_ty Eio.Resource.t;
    registered : int ref;  (** barriers answered through [File_sync]. *)
    delegated : int ref;  (** barriers that reached the file's own sync. *)
  }

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t.base buf
  let pread t ~file_offset bufs = Eio.File.pread t.base ~file_offset bufs
  let stat t = Eio.File.stat t.base
  let seek t off cmd = Eio.File.seek t.base off cmd
  let close t = Eio.Resource.close t.base
  let single_write t bufs = Eio.Flow.single_write t.base bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let pwrite t ~file_offset bufs =
    Eio.File.pwrite_single t.base ~file_offset bufs

  let sync t =
    incr t.delegated;
    Eio.File.sync t.base

  let truncate t len = Eio.File.truncate t.base len
end

let counting_file ~register base =
  let state = Counting_file.{ base; registered = ref 0; delegated = ref 0 } in
  let registration =
    if register then
      [
        Eio.Resource.H
          (Eio_fs_sync.File_sync, fun (t : Counting_file.t) -> incr t.registered);
      ]
    else []
  in
  let file : Eio.File.rw_ty Eio.Resource.t =
    Eio.Resource.T
      ( state,
        Eio.Resource.handler
          (registration
          @ Eio.Resource.bindings (Eio.File.Pi.rw (module Counting_file))) )
  in
  (file, state)

let with_in_memory_file fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  fn (Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (env#fs / "f"))

let test_sync_file_answers_through_the_registration () =
  with_in_memory_file @@ fun base ->
  let file, state = counting_file ~register:true base in
  Eio_fs_sync.sync_file file;
  Alcotest.(check int)
    "the registration answered the barrier" 1
    !(state.Counting_file.registered);
  Alcotest.(check int)
    "and nothing reached the file behind it" 0
    !(state.Counting_file.delegated)

(* A backend that registers nothing keeps the plain barrier: a file's own sync
   is what makes its data durable, where a directory's had to be reached by
   opening the directory read-only and fsyncing that handle. Silence is
   therefore safe here, and the in-memory backend needs no registration of its
   own -- its files' sync is already the no-op volatile storage deserves. *)
let test_sync_file_without_a_registration_syncs_the_file () =
  with_in_memory_file @@ fun base ->
  let file, state = counting_file ~register:false base in
  Eio_fs_sync.sync_file file;
  Alcotest.(check int)
    "the file's own sync is the barrier" 1
    !(state.Counting_file.delegated)

(* What the registration is for. Under the crash model a file's writes stay
   un-synced until the model sees a barrier on that file, and [Drop_unsynced]
   loses every un-synced write. Bytes barriered with [sync_file] must survive
   the cut; had the barrier answered anywhere but through the registration the
   model would have recorded none and the bytes would be gone. *)
let test_sync_file_reaches_the_crash_model () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let module Crash = Eio_fs.Chaos.Crash in
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let fs = Crash.wrap ctl env#fs in
  Eio.Switch.run (fun sw ->
      let file =
        Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (fs / "image")
      in
      Eio.Flow.copy_string "payload" file;
      Eio_fs_sync.sync_file file);
  (* The name the bytes live under is the metadata journal's business, as
     fsync(2) divides them, so the entry takes the directory barrier. *)
  Eio_fs_sync.sync_dir fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the barriered bytes survived the cut" "payload"
    (Eio.Path.load Eio.Path.(post / "image"))

(* An exclusive publication follows renameat2's RENAME_NOREPLACE contract:
   an existing name is never overwritten, including a dangling symlink. *)
let test_exclusive_publication () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let path = env#fs / "state" in
  let publish path text =
    Eio_fs_sync.publish ~config:(config 3L) ~mode:`Exclusive path
      (Eio.Flow.copy_string text)
  in
  publish path "old";
  (match publish path "new" with
  | () -> Alcotest.fail "exclusive publication replaced a file"
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  Alcotest.(check string) "old contents retained" "old" (Eio.Path.load path);
  let link = env#fs / "link" in
  Eio.Path.symlink ~link_to:"missing" link;
  (match publish link "new" with
  | () -> Alcotest.fail "exclusive publication replaced a dangling link"
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  Alcotest.(check string) "link retained" "missing" (Eio.Path.read_link link);
  Alcotest.(check (list string))
    "temporary files removed" [ "link"; "state" ]
    (List.sort String.compare (Eio.Path.read_dir env#fs))

let test_exclusive_publication_survives_crash () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let module Crash = Eio_fs.Chaos.Crash in
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl env#fs in
  Eio_fs_sync.publish ~config:(config 7L) ~mode:`Exclusive (fs / "state")
    (Eio.Flow.copy_string "payload");
  Crash.crash ctl;
  Alcotest.(check string)
    "published data survives its barriers" "payload"
    (Eio.Path.load (Crash.post_crash ctl / "state"))

let suite =
  ( "eio_fs_sync",
    [
      Alcotest.test_case "exclusive publication refuses existing names" `Quick
        test_exclusive_publication;
      Alcotest.test_case "exclusive publication survives crash" `Quick
        test_exclusive_publication_survives_crash;
      Alcotest.test_case "registered backend" `Quick test_registered_backend;
      Alcotest.test_case "posix needs no registration" `Quick
        test_posix_needs_no_registration;
      Alcotest.test_case "published mode ignores the umask" `Quick
        test_publish_honours_perm_under_umask;
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
      Alcotest.test_case "file barrier answers through the registration" `Quick
        test_sync_file_answers_through_the_registration;
      Alcotest.test_case "file barrier without a registration syncs the file"
        `Quick test_sync_file_without_a_registration_syncs_the_file;
      Alcotest.test_case "file barrier reaches the crash model" `Quick
        test_sync_file_reaches_the_crash_model;
    ] )
