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

(* [unsyncable dir] is [dir] with no Dir_sync barrier and with handles that
   cannot sync: [dir]'s own operations, its files opened read-only. *)
let unsyncable ((Eio.Resource.T (v, ops), path) : _ Eio.Path.t) =
  let unsyncable (type a) (v : a) (ops : (a, _) Eio.Resource.handler) =
    let module D = (val Eio.Resource.get ops Eio.Fs.Pi.Dir) in
    let module Read_only = struct
      include D

      let open_in t ~sw ~follow path =
        let (Eio.Resource.T (f, file)) = D.open_in t ~sw ~follow path in
        let module R = (val Eio.Resource.get file Eio.File.Pi.Read) in
        Eio.Resource.T (f, Eio.File.Pi.ro (module R))
    end in
    let barrier : type i g. (a, i, g) Eio.Resource.pi -> bool = function
      | Eio_fs_sync.Dir_sync -> true
      | _ -> false
    in
    let kept =
      List.filter
        (fun (Eio.Resource.H (pi, _)) -> not (barrier pi))
        (Eio.Resource.bindings ops)
    in
    let handler : (a, Eio.Fs.dir_ty) Eio.Resource.handler =
      Eio.Resource.handler
        (Eio.Resource.H (Eio.Fs.Pi.Dir, (module Read_only)) :: kept)
    in
    (Eio.Resource.T (v, handler), path)
  in
  unsyncable v ops

(* A backend that registers no barrier and whose directory handle cannot sync
   is wired wrongly, an error in the program rather than in the filesystem. *)
let test_unsyncable_backend_is_invalid () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  match Eio_fs_sync.sync_dir (unsyncable env#fs) with
  | () -> Alcotest.fail "sync_dir skipped the barrier"
  | exception Invalid_argument reason ->
      Alcotest.(check bool)
        "names Dir_sync" true
        (contains ~affix:"Dir_sync" reason)

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

(* [save] publishes a string: the target holds it whole, replaced on a
   second save, and an empty string is a file of no bytes. *)
let test_save () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let p = env#fs / "state.json" in
  Eio_fs_sync.save p "{\"a\":1}";
  Alcotest.(check string) "saved" "{\"a\":1}" (Eio.Path.load p);
  Eio_fs_sync.save ~durable:false p "{}";
  Alcotest.(check string) "replaced" "{}" (Eio.Path.load p);
  Eio_fs_sync.save p "";
  Alcotest.(check string) "empty" "" (Eio.Path.load p)

(* -- Appending lines -- *)

let with_mem f =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> f (env#fs / "log.jsonl")

let test_append_creates () =
  with_mem @@ fun p ->
  Eio_fs_sync.append_lines p [ {|{"a":1}|}; {|{"b":2}|} ];
  Alcotest.(check string) "created" "{\"a\":1}\n{\"b\":2}\n" (Eio.Path.load p);
  Eio_fs_sync.append_lines ~durable:false p [ {|{"c":3}|} ];
  Alcotest.(check string)
    "appended" "{\"a\":1}\n{\"b\":2}\n{\"c\":3}\n" (Eio.Path.load p)

(* A crash part way through an append leaves a last line with no LF. The
   next line starts on a line of its own, so it is not read as the torn
   line's continuation, and the torn bytes stay as they were. *)
let test_append_repairs_a_torn_line () =
  with_mem @@ fun p ->
  Eio.Path.save ~create:(`Exclusive 0o644) p "{\"a\":1}\n{\"b\":";
  Eio_fs_sync.append_lines p [ {|{"c":3}|} ];
  Alcotest.(check string)
    "torn line ended" "{\"a\":1}\n{\"b\":\n{\"c\":3}\n" (Eio.Path.load p);
  Eio_fs_sync.append_lines p [ {|{"d":4}|} ];
  Alcotest.(check string)
    "an LF is not added twice" "{\"a\":1}\n{\"b\":\n{\"c\":3}\n{\"d\":4}\n"
    (Eio.Path.load p)

let test_append_to_empty () =
  with_mem @@ fun p ->
  Eio.Path.save ~create:(`Exclusive 0o644) p "";
  Eio_fs_sync.append_lines p [ "x" ];
  Alcotest.(check string) "no leading LF" "x\n" (Eio.Path.load p)

let test_append_nothing () =
  with_mem @@ fun p ->
  Eio_fs_sync.append_lines p [];
  Alcotest.(check bool)
    "no file" true
    (Eio.Path.kind ~follow:true p = `Not_found)

let test_append_refuses_lf () =
  with_mem @@ fun p ->
  Eio.Path.save ~create:(`Exclusive 0o644) p "a\n";
  (match Eio_fs_sync.append_lines p [ "b"; "c\nd" ] with
  | () -> Alcotest.fail "a line holding LF was appended"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string) "unchanged" "a\n" (Eio.Path.load p)

let test_append_posix () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-append" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () ->
  let p = fs / dir / "log.jsonl" in
  Eio_fs_sync.append_lines p [ "1" ];
  Eio.Path.with_open_out ~append:true ~create:`Never p (fun f ->
      Eio.Flow.copy_string "2" f);
  Eio_fs_sync.append_lines p [ "3" ];
  Alcotest.(check string) "repaired" "1\n2\n3\n" (Eio.Path.load p)

(* The directory barrier that makes a created log's entry durable can fail
   after the file is written. The next durable append barriers the directory
   again, or the file it was told is durable is lost at a crash. *)
let test_append_retries_a_failed_parent_barrier () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let dir_syncs = ref 0 in
  let schedule =
    Eio_fs.Chaos.Schedule.observing
      (fun (op : Eio_fs.Chaos.Op.t) ->
        if op.kind = `Sync && op.path <> "log.jsonl" then incr dir_syncs)
      (Eio_fs.Chaos.Schedule.scripted
         [
           {
             trigger = Op_on_path { kind = `Sync; path = "" };
             fault = Sync_error `Eio;
           };
         ])
  in
  let p = Eio_fs.Chaos.wrap schedule env#fs / "log.jsonl" in
  (match Eio_fs_sync.append_lines p [ "1" ] with
  | () -> Alcotest.fail "the directory barrier did not fail"
  | exception Eio.Io _ -> ());
  dir_syncs := 0;
  Eio_fs_sync.append_lines p [ "2" ];
  Alcotest.(check bool) "the directory is barriered again" true (!dir_syncs > 0);
  Alcotest.(check string) "both lines" "1\n2\n" (Eio.Path.load p)

(* POSIX truncate leaves unwritten ranges zero. The independent operation
   trace pins the data extents, so filling those gaps with zero writes fails
   even if the published bytes still look right. *)
let test_sparse_cost size =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir =
    fs / Filename.get_temp_dir_name () / Eio_fs_sync.fresh_name "sparse"
  in
  Eio.Path.mkdir ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree dir) @@ fun () ->
  let writes = ref [] in
  let schedule =
    Eio_fs.Chaos.Schedule.observing
      (fun (op : Eio_fs.Chaos.Op.t) ->
        if op.kind = `Write then
          writes := (op.file_offset, op.length) :: !writes)
      Eio_fs.Chaos.Schedule.empty
  in
  (* The wrapper closes what it wraps: an opened directory, which the switch
     closes, not the root of [fs]. *)
  Eio.Switch.run @@ fun sw ->
  let observed = Eio_fs.Chaos.wrap schedule (Eio.Path.open_dir ~sw dir) in
  Eio_fs_sync.publish_sparse
    ~config:(config (Int64.of_int size))
    ~rename:Eio_fs_unix.exclusive_rename ~mode:`Exclusive
    ~size_bytes:(Int64.of_int size) (observed / "image")
    [
      (0L, Cstruct.of_string "HDR");
      (Int64.of_int (size - 4), Cstruct.of_string "TAIL");
    ];
  Alcotest.(check (list (pair int int)))
    "only seven supplied bytes are written, independent of logical size"
    [ (0, 3); (size - 4, 4) ]
    (List.rev !writes);
  Eio.Path.with_open_in (dir / "image") @@ fun file ->
  Alcotest.(check int64)
    "logical size" (Int64.of_int size)
    (Optint.Int63.to_int64 (Eio.File.size file));
  let read offset length =
    let data = Cstruct.create length in
    Eio.File.pread_exact file ~file_offset:(Optint.Int63.of_int offset) [ data ];
    Cstruct.to_string data
  in
  Alcotest.(check string) "header" "HDR" (read 0 3);
  Alcotest.(check string)
    "hole reads as zero" (String.make 4 '\000') (read 4096 4);
  Alcotest.(check string) "tail at the final extent" "TAIL" (read (size - 4) 4)

let test_sparse_bounds_before_io () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let operations = ref [] in
  let schedule =
    Eio_fs.Chaos.Schedule.observing
      (fun op -> operations := op :: !operations)
      Eio_fs.Chaos.Schedule.empty
  in
  let fs = Eio_fs.Chaos.wrap schedule env#fs in
  let byte = Cstruct.of_string "x" in
  List.iter
    (fun (name, ceiling, size, spans, limit) ->
      operations := [];
      (match
         Eio_fs_sync.publish_sparse ~config:(config ceiling) ~size_bytes:size
           (fs / "image") spans
       with
      | () -> Alcotest.failf "%s was accepted" name
      | exception Eio_fs_sync.Limit_exceeded { max_bytes; attempted } when limit
        ->
          Alcotest.(check int64) "ceiling" ceiling max_bytes;
          Alcotest.(check int64) "attempted logical size" size attempted
      | exception Invalid_argument _ when not limit -> ());
      Alcotest.(check int)
        (name ^ " refuses before any filesystem operation")
        0 (List.length !operations))
    [
      ("logical ceiling", 7L, 8L, [], true);
      ("negative ceiling", -1L, 0L, [], false);
      ("negative size", 8L, -1L, [], false);
      ("unrepresentable size", Int64.max_int, Int64.max_int, [], false);
      ("negative offset", 8L, 8L, [ (-1L, byte) ], false);
      ("offset beyond end", 8L, 8L, [ (Int64.max_int, byte) ], false);
      ("data beyond end", 8L, 8L, [ (8L, byte) ], false);
      ("overlap", 8L, 8L, [ (0L, byte); (0L, byte) ], false);
      ("unordered", 8L, 8L, [ (2L, byte); (0L, byte) ], false);
    ]

let test_sparse_exclusive () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Exclusive 0o600) (fs / "image") "foreign bytes";
  Eio.Path.symlink ~link_to:"missing" (fs / "link");
  List.iter
    (fun name ->
      (match
         Eio_fs_sync.publish_sparse ~config:(config 16L) ~mode:`Exclusive
           ~size_bytes:16L (fs / name)
           [ (0L, Cstruct.of_string "new") ]
       with
      | () -> Alcotest.failf "sparse publication replaced %s" name
      | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
      Alcotest.(check string)
        "foreign file retained byte for byte" "foreign bytes"
        (Eio.Path.load (fs / "image"));
      Alcotest.(check string)
        "dangling symlink retained" "missing"
        (Eio.Path.read_link (fs / "link"));
      Alcotest.(check (list string))
        "no failed-publication temporary remains" [ "image"; "link" ]
        (List.sort String.compare (Eio.Path.read_dir fs)))
    [ "image"; "link" ]

let test_sparse_write_failure () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio.Path.save ~create:(`Exclusive 0o600) (env#fs / "image") "foreign bytes";
  let schedule =
    Eio_fs.Chaos.Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Write; path = "draft" };
          fault = Torn_write 1;
        };
      ]
  in
  let fs = Eio_fs.Chaos.wrap schedule env#fs in
  (match
     Eio_fs_sync.publish_sparse ~config:(config 16L) ~tmp:(fs / "draft")
       ~size_bytes:16L (fs / "image")
       [ (4L, Cstruct.of_string "new") ]
   with
  | () -> Alcotest.fail "torn extent was published"
  | exception Eio.Io _ -> ());
  Alcotest.(check string)
    "failed producer preserves the target" "foreign bytes"
    (Eio.Path.load (env#fs / "image"));
  Alcotest.(check bool)
    "failed temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "draft") = `Not_found)

let test_sparse_durable_and_empty () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let module Crash = Eio_fs.Chaos.Crash in
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl env#fs in
  Eio_fs_sync.publish_sparse ~config:(config 16L) ~mode:`Exclusive
    ~size_bytes:16L (fs / "image")
    [ (0L, Cstruct.of_string "HDR"); (12L, Cstruct.of_string "TAIL") ];
  Eio_fs_sync.publish_sparse ~config:(config 0L) ~mode:`Exclusive ~size_bytes:0L
    (fs / "empty") [];
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "sparse bytes survive file and directory barriers"
    ("HDR" ^ String.make 9 '\000' ^ "TAIL")
    (Eio.Path.load (post / "image"));
  Alcotest.(check string)
    "zero-size file survives" ""
    (Eio.Path.load (post / "empty"))

let test_sparse_cancellation () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  Eio.Path.save ~create:(`Exclusive 0o600) (env#fs / "image") "foreign bytes";
  let ready, start = Eio.Promise.create () in
  let blocked, hold = Eio.Promise.create () in
  let schedule =
    Eio_fs.Chaos.Schedule.observing
      (fun (op : Eio_fs.Chaos.Op.t) ->
        if op.kind = `Write then begin
          Eio.Promise.resolve start ();
          Eio.Promise.await blocked
        end)
      Eio_fs.Chaos.Schedule.empty
  in
  let fs = Eio_fs.Chaos.wrap schedule env#fs in
  Eio.Fiber.first
    (fun () ->
      Eio_fs_sync.publish_sparse ~config:(config 16L) ~tmp:(fs / "draft")
        ~size_bytes:16L (fs / "image")
        [ (4L, Cstruct.of_string "new") ];
      Alcotest.fail "gated producer completed before cancellation")
    (fun () -> Eio.Promise.await ready);
  Eio.Promise.resolve hold ();
  Alcotest.(check string)
    "cancelled producer preserves target bytes" "foreign bytes"
    (Eio.Path.load (env#fs / "image"));
  Alcotest.(check bool)
    "cancelled temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "draft") = `Not_found)

exception Publication_origin

let[@inline never] raise_publication_origin () = raise Publication_origin

let test_error_backtrace generation () =
  let previous = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  Fun.protect ~finally:(fun () -> Printexc.record_backtrace previous)
  @@ fun () ->
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  match
    if generation then
      Eio_fs_sync.publish_generation ~durable:false (env#fs / "generation")
        (fun _ -> raise_publication_origin ())
    else
      Eio_fs_sync.publish ~config:(config 1L) (env#fs / "image") (fun _ ->
          raise_publication_origin ())
  with
  | () -> Alcotest.fail "producer exception was swallowed"
  | exception Publication_origin ->
      let trace = Printexc.get_raw_backtrace () in
      Alcotest.(check bool)
        "cleanup retains the producer's original throw site" true
        (contains ~affix:"raise_publication_origin"
           (Printexc.raw_backtrace_to_string trace))

let suite =
  ( "eio_fs_sync",
    [
      Alcotest.test_case "sparse 8 MiB extents and cost" `Quick (fun () ->
          test_sparse_cost (8 * 1024 * 1024));
      Alcotest.test_case "sparse 80 MiB extents and cost" `Quick (fun () ->
          test_sparse_cost (80 * 1024 * 1024));
      Alcotest.test_case "sparse bounds precede all IO" `Quick
        test_sparse_bounds_before_io;
      Alcotest.test_case "sparse exclusive publication" `Quick
        test_sparse_exclusive;
      Alcotest.test_case "sparse write failure" `Quick test_sparse_write_failure;
      Alcotest.test_case "sparse durable and empty files" `Quick
        test_sparse_durable_and_empty;
      Alcotest.test_case "sparse cancellation removes temporary" `Quick
        test_sparse_cancellation;
      Alcotest.test_case "exclusive publication refuses existing names" `Quick
        test_exclusive_publication;
      Alcotest.test_case "exclusive publication survives crash" `Quick
        test_exclusive_publication_survives_crash;
      Alcotest.test_case "registered backend" `Quick test_registered_backend;
      Alcotest.test_case "save a string" `Quick test_save;
      Alcotest.test_case "append lines" `Quick test_append_creates;
      Alcotest.test_case "append ends a torn line" `Quick
        test_append_repairs_a_torn_line;
      Alcotest.test_case "append to an empty file" `Quick test_append_to_empty;
      Alcotest.test_case "append nothing" `Quick test_append_nothing;
      Alcotest.test_case "append refuses a line holding LF" `Quick
        test_append_refuses_lf;
      Alcotest.test_case "append on posix" `Quick test_append_posix;
      Alcotest.test_case "append retries a failed parent barrier" `Quick
        test_append_retries_a_failed_parent_barrier;
      Alcotest.test_case "posix needs no registration" `Quick
        test_posix_needs_no_registration;
      Alcotest.test_case "published mode ignores the umask" `Quick
        test_publish_honours_perm_under_umask;
      Alcotest.test_case "unissuable barrier raises" `Quick
        test_unissuable_barrier_raises;
      Alcotest.test_case "unsyncable backend is invalid" `Quick
        test_unsyncable_backend_is_invalid;
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
      Alcotest.test_case "file cleanup retains producer backtrace" `Quick
        (test_error_backtrace false);
      Alcotest.test_case "generation cleanup retains producer backtrace" `Quick
        (test_error_backtrace true);
    ] )

module Links = struct
  let fixture test =
    Eio_mock.Backend.run @@ fun () ->
    Eio_fs.run @@ fun env -> test env#fs

  let publish fs ?(durable = false) expected target =
    Eio_fs_sync.publish_link ~durable ~lock:Eio_fs_sync.Lock.none ~expected
      ~link_to:target (fs / "portal")

  let test_raw () =
    fixture @@ fun fs ->
    let raw = "../old library//./checkout" in
    Eio.Path.symlink ~link_to:raw (fs / "portal");
    ignore (Result.get_ok (publish fs (Some raw) "/selected/lib"));
    Alcotest.(check string)
      "whole selected link" "/selected/lib"
      (Eio.Path.read_link (fs / "portal"));
    ignore (Result.get_ok (publish fs (Some "/selected/lib") raw));
    Alcotest.(check string)
      "exact original raw bytes" raw
      (Eio.Path.read_link (fs / "portal"))

  let test_refusal () =
    fixture @@ fun fs ->
    Eio.Path.symlink ~link_to:"../foreign" (fs / "portal");
    (match publish fs (Some "../old") "/new" with
    | Error (`Changed (Some "../foreign")) -> ()
    | _ -> Alcotest.fail "changed portal replaced");
    Alcotest.(check string)
      "foreign target survives" "../foreign"
      (Eio.Path.read_link (fs / "portal"));
    (match publish fs (Some "../old") "../foreign" with
    | Error (`Changed (Some "../foreign")) -> ()
    | _ -> Alcotest.fail "changed portal taken as already published");
    Eio.Path.unlink (fs / "portal");
    (match publish fs (Some "../old") "/new" with
    | Error (`Changed None) -> ()
    | _ -> Alcotest.fail "missing portal replaced");
    Eio.Path.save ~create:(`Exclusive 0o644) (fs / "portal") "foreign file";
    (match publish fs None "/new" with
    | Error (`Invalid _) -> ()
    | _ -> Alcotest.fail "regular file replaced");
    Alcotest.(check string)
      "foreign file survives" "foreign file"
      (Eio.Path.load (fs / "portal"))

  let test_unwritable_target () =
    fixture @@ fun fs ->
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    List.iter
      (fun target ->
        match publish fs (Some "../old") target with
        | Error (`Invalid _) -> ()
        | _ -> Alcotest.failf "target %S published" target)
      [ ""; "../new\000library" ];
    Alcotest.(check string)
      "old target survives" "../old"
      (Eio.Path.read_link (fs / "portal"));
    Alcotest.(check (list string))
      "nothing staged" [ "portal" ] (Eio.Path.read_dir fs)

  let test_crash () =
    fixture @@ fun fs ->
    let module Crash = Eio_fs.Chaos.Crash in
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    Eio_fs_sync.sync_dir fs;
    let ctl = Crash.v () in
    let wrapped = Crash.wrap ctl fs in
    ignore
      (Result.get_ok
         (publish wrapped ~durable:true (Some "../old")
            "../new library//./checkout"));
    Crash.crash ctl;
    Alcotest.(check string)
      "raw link survives native metadata barriers" "../new library//./checkout"
      (Eio.Path.read_link (Crash.post_crash ctl / "portal"))

  let test_durable_retry () =
    fixture @@ fun fs ->
    let module Crash = Eio_fs.Chaos.Crash in
    let module Chaos = Eio_fs.Chaos in
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    let ctl = Crash.v () in
    let disk = Crash.wrap ctl fs in
    let syncs = ref 0 in
    let schedule =
      Chaos.Schedule.observing
        (fun (op : Chaos.Op.t) ->
          if op.kind = `Sync then (
            incr syncs;
            if !syncs = 2 then failwith "fixture post-rename barrier"))
        Chaos.Schedule.empty
    in
    let faults = Chaos.wrap schedule disk in
    (match publish faults ~durable:true (Some "../old") "../new" with
    | _ -> Alcotest.fail "post-publication barrier failure hidden"
    | exception Failure reason ->
        Alcotest.(check string)
          "uncertain durability reported" "fixture post-rename barrier" reason);
    Alcotest.(check string)
      "new link already visible" "../new"
      (Eio.Path.read_link (fs / "portal"));
    ignore (Result.get_ok (publish disk ~durable:true (Some "../new") "../new"));
    Crash.crash ctl;
    Alcotest.(check string)
      "durable no-op redeems publication" "../new"
      (Eio.Path.read_link (Crash.post_crash ctl / "portal"))

  let test_last_observation () =
    fixture @@ fun fs ->
    let module Chaos = Eio_fs.Chaos in
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    let changed = ref false in
    let schedule =
      Chaos.Schedule.observing
        (fun (op : Chaos.Op.t) ->
          if op.kind = `Sync && not !changed then (
            changed := true;
            Eio.Path.unlink (fs / "portal");
            Eio.Path.symlink ~link_to:"../foreign" (fs / "portal")))
        Chaos.Schedule.empty
    in
    let faults = Chaos.wrap schedule fs in
    (match publish faults ~durable:true (Some "../old") "../new" with
    | Error (`Changed (Some "../foreign")) -> ()
    | _ -> Alcotest.fail "final raw observation ignored");
    Alcotest.(check string)
      "late foreign target preserved" "../foreign"
      (Eio.Path.read_link (fs / "portal"));
    Alcotest.(check (list string))
      "owned staging cleaned" [ "portal" ] (Eio.Path.read_dir fs)

  let test_posix () =
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let dir = Filename.temp_file "eio-fs-raw-link" "" in
    Sys.remove dir;
    Eio.Path.mkdir ~perm:0o755 (fs / dir);
    Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
    @@ fun () ->
    let root = fs / dir in
    Eio.Path.symlink ~link_to:"../old library//./checkout" (root / "portal");
    ignore
      (Result.get_ok
         (publish root ~durable:true (Some "../old library//./checkout")
            "/selected/library"));
    Alcotest.(check string)
      "native POSIX raw switch" "/selected/library"
      (Eio.Path.read_link (root / "portal"));
    Alcotest.(check (list string))
      "no native staging remains" [ "portal" ] (Eio.Path.read_dir root)

  let test_lock () =
    fixture @@ fun fs ->
    let module Refuses = struct
      type t = unit

      let use () _ = raise (Eio_fs_sync.Lock.Unavailable "fixture lease held")
    end in
    let module Packed = Eio_fs_sync.Lock.Make (Refuses) in
    (match
       Eio_fs_sync.publish_link ~durable:false ~lock:(Packed.v ())
         ~expected:None ~link_to:"../new" (fs / "portal")
     with
    | _ -> Alcotest.fail "held caller lease ignored"
    | exception Eio_fs_sync.Lock.Unavailable reason ->
        Alcotest.(check string)
          "exact lease refusal" "fixture lease held" reason);
    Alcotest.(check bool)
      "no link or staging published" true
      (Eio.Path.read_dir fs = [])
end

let link_suite =
  ( "publish_link",
    [
      Alcotest.test_case "durable retry after uncertain publication" `Quick
        Links.test_durable_retry;
      Alcotest.test_case "final raw observation preserves late foreign target"
        `Quick Links.test_last_observation;
      Alcotest.test_case "native POSIX raw link publication" `Quick
        Links.test_posix;
      Alcotest.test_case "exact raw publication and restoration" `Quick
        Links.test_raw;
      Alcotest.test_case "changed missing and regular portals refuse" `Quick
        Links.test_refusal;
      Alcotest.test_case "durable raw link survives crash" `Quick
        Links.test_crash;
      Alcotest.test_case "empty and NUL targets refuse" `Quick
        Links.test_unwritable_target;
      Alcotest.test_case "caller lease refuses before writes" `Quick
        Links.test_lock;
    ] )
