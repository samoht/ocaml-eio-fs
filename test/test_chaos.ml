(** Fault-injection tests for {!Eio_fs.Chaos}: each fault kind is pinned with a
    scripted schedule and asserted to occur exactly, and a seeded schedule is
    shown to reproduce deterministically. *)

module Chaos = Eio_fs.Chaos
module Schedule = Chaos.Schedule
module Fault = Chaos.Fault
module Op = Chaos.Op

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* [raw] is the un-perturbed base (for setup and for reading back what actually
   committed); [chaos] wraps the same in-memory tree with [schedule]. *)
let with_chaos schedule fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  let chaos = Chaos.wrap schedule raw in
  fn ~raw ~chaos

let expect_io name fn =
  match fn () with
  | () -> Alcotest.failf "%s: expected Eio.Io, got success" name
  | exception Eio.Io _ -> ()
  | exception exn ->
      Alcotest.failf "%s: expected Eio.Io, got %s" name (Printexc.to_string exn)

(* {2 Scripted, exact-scenario faults} *)

let test_short_read () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read; path = "f" };
          fault = Short_read 3;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Path.with_open_in (chaos / "f") @@ fun file ->
  let buf = Cstruct.create 5 in
  let got = Eio.Flow.single_read file buf in
  Alcotest.(check int) "read split to 3 bytes" 3 got;
  Alcotest.(check string)
    "first 3 bytes correct" "hel"
    (Cstruct.to_string (Cstruct.sub buf 0 got))

let test_short_write () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Write; path = "f" };
          fault = Short_write 2;
        };
      ]
  in
  with_chaos sched @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  let got =
    Eio.File.pwrite_single file ~file_offset:(i63 0)
      [ Cstruct.of_string "hello" ]
  in
  Alcotest.(check int) "write accepted only 2 bytes" 2 got;
  Alcotest.(check int)
    "only 2 bytes committed" 2
    (Optint.Int63.to_int (Eio.File.size file))

(* The durability hazard: a torn write commits its prefix to the store and then
   reports failure. Pinned at offset 512 during a 1024-byte "superblock" flush. *)
let test_torn_write () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Write_covering { path = "sb"; file_offset = 512 };
          fault = Torn_write 512;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  let block = Cstruct.create 1024 in
  Cstruct.memset block 0xAB;
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "sb")
    (fun file ->
      expect_io "torn write" (fun () ->
          Eio.File.pwrite_all file ~file_offset:(i63 0) [ block ]));
  (* 512 bytes reached the store despite the raised error. *)
  let committed = Eio.Path.load (raw / "sb") in
  Alcotest.(check int) "prefix length committed" 512 (String.length committed);
  Alcotest.(check string)
    "prefix bytes committed" (String.make 512 '\xAB') committed

let test_write_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Write; path = "f" };
          fault = Write_error `Enospc;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f") (fun file ->
      expect_io "enospc" (fun () ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "data" ]));
  Alcotest.(check string) "nothing committed" "" (Eio.Path.load (raw / "f"))

let test_sync_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Sync; path = "f" };
          fault = Sync_error `Eio;
        };
      ]
  in
  with_chaos sched @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  (* the write itself is not perturbed *)
  Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
  expect_io "fsync failure" (fun () -> Eio.File.sync file)

let test_open_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open; path = "secret" };
          fault = Open_error `Enoent;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "secret") "x";
  expect_io "open" (fun () -> ignore (Eio.Path.load (chaos / "secret")))

let test_stat_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "f" };
          fault = Stat_error `Eacces;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "x";
  expect_io "stat" (fun () ->
      ignore (Eio.Path.stat ~follow:false (chaos / "f")))

(* {2 Schedule as an inspectable value} *)

(* A scripted rule fires the first time its trigger matches, then is spent. *)
let test_scripted_fires_once () =
  let sched =
    Schedule.scripted [ { trigger = Nth_op 2; fault = Short_read 1 } ]
  in
  let op = { Op.kind = `Read; path = "f"; file_offset = 0; length = 10 } in
  let check name expected =
    Alcotest.(check bool)
      name true
      (match (Schedule.next sched op, expected) with
      | None, None -> true
      | Some a, Some b -> Fault.equal a b
      | _ -> false)
  in
  check "op1 passes through" None;
  check "op2 fires" (Some (Fault.Short_read 1));
  check "op3 passes through (rule spent)" None

(* {2 Seeded determinism} *)

let ops_fixture =
  let mk kind path file_offset length : Op.t =
    { kind; path; file_offset; length }
  in
  [
    mk `Open "a" 0 0;
    mk `Write "a" 0 4096;
    mk `Write "a" 4096 4096;
    mk `Sync "a" 0 0;
    mk `Read "a" 0 8192;
    mk `Stat "b" 0 0;
    mk `Open "b" 0 0;
    mk `Write "b" 0 512;
    mk `Read "b" 0 512;
    mk `Sync "b" 0 0;
  ]

let run_schedule seed =
  let s = Schedule.seeded ~seed ~probability:0.7 () in
  List.map (Schedule.next s) ops_fixture

let test_seed_determinism () =
  let a = run_schedule 1234 and b = run_schedule 1234 in
  Alcotest.(check int) "same length" (List.length a) (List.length b);
  List.iter2
    (fun x y ->
      Alcotest.(check bool)
        "same fault at step" true
        (match (x, y) with
        | None, None -> true
        | Some fx, Some fy -> Fault.equal fx fy
        | _ -> false))
    a b;
  (* non-vacuous: at this probability at least one op must have faulted *)
  Alcotest.(check bool)
    "faults actually occur" true
    (List.exists Option.is_some a)

(* End-to-end determinism over the composed stack: the same seed drives the
   same committed bytes and the same success/failure outcome, and some seed
   perturbs the clean result (so the wrapper really injects). *)
let run_workload schedule =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let fs = Chaos.wrap schedule env#fs in
  let outcome =
    try
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "f")
        (fun file ->
          for i = 0 to 9 do
            Eio.File.pwrite_all file
              ~file_offset:(i63 (i * 16))
              [ Fmt.kstr Cstruct.of_string "%016d" i ]
          done;
          Eio.File.sync file);
      "ok"
    with Eio.Io _ -> "io-error"
  in
  (* read back the truly-committed bytes through the un-perturbed base *)
  let committed =
    try Eio.Path.load (env#fs / "f") with Eio.Io _ -> "<none>"
  in
  (outcome, committed)

let test_seed_reproduces () =
  let a = run_workload (Schedule.seeded ~seed:99 ~probability:0.4 ()) in
  let b = run_workload (Schedule.seeded ~seed:99 ~probability:0.4 ()) in
  Alcotest.(check (pair string string)) "same outcome and bytes" a b

let test_chaos_perturbs () =
  let clean = run_workload Schedule.empty in
  Alcotest.(check (pair string string))
    "clean run succeeds fully"
    ( "ok",
      "0000000000000000000000000000000100000000000000020000000000000003000000000000000400000000000000050000000000000006000000000000000700000000000000080000000000000009"
    )
    clean;
  let perturbed =
    List.init 40 (fun seed ->
        run_workload (Schedule.seeded ~seed ~probability:0.5 ()))
  in
  Alcotest.(check bool)
    "some seed perturbs the clean result" true
    (List.exists (fun r -> r <> clean) perturbed)

(* {2 Crash / durability model} *)

module Crash = Chaos.Crash

(* [raw] is the un-perturbed base; [fs] is the same in-memory tree with
   durability tracked by [ctl]. After a crash, reads go through [raw] (or the
   handle [Crash.post_crash] returns), never through the dead [fs]. *)
let with_crash ctl fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  let fs = Crash.wrap ctl raw in
  fn ~raw ~fs

(* A write with no intervening sync is lost after a crash. *)
let test_crash_unsynced_lost () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "unsynced write lost after crash" ""
    (Eio.Path.load (post / "img"))

(* A write followed by a sync survives a crash. *)
let test_crash_synced_survives () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced write survives crash" "data"
    (Eio.Path.load (post / "img"))

(* The barrier is respected: everything before a sync survives, the unsynced
   suffix is dropped. Nothing before the barrier can be lost. *)
let test_crash_barrier_respected () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced prefix survived, unsynced suffix lost" "AAAA"
    (Eio.Path.load (post / "img"))

(* Even when a later, un-synced write is kept, the synced data before the
   barrier is never reordered away: it stays durable underneath. *)
let test_crash_barrier_not_reordered () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced data present under a kept unsynced suffix" "AAAABBBB"
    (Eio.Path.load (post / "img"))

(* The missing-barrier hazard in miniature: a data block is written, then a
   pointer to it, with NO sync between. A crash that keeps the pointer write but
   drops the data write leaves a pointer referencing bytes that never became
   durable. *)
let test_crash_pointer_without_data () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun u -> u.offset = 0)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      (* data block first ... *)
      Eio.File.pwrite_all file ~file_offset:(i63 512)
        [ Cstruct.of_string (String.make 512 'D') ];
      (* ... then the pointer, with no fsync between: the missing barrier *)
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string "PTR->512" ]);
  let post = Crash.post_crash ctl in
  let img = Eio.Path.load (post / "img") in
  Alcotest.(check string)
    "generation pointer landed" "PTR->512" (String.sub img 0 8);
  Alcotest.(check int)
    "referenced data block never became durable" 8 (String.length img)

(* Same seed reproduces the same post-crash image. *)
let reorder_image seed =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let ctl = Crash.v ~policy:(Crash.Reorder_unsynced seed) () in
  let fs = Crash.wrap ctl env#fs in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      for i = 0 to 7 do
        Eio.File.pwrite_all file
          ~file_offset:(i63 (i * 8))
          [ Fmt.kstr Cstruct.of_string "%08d" i ]
      done);
  let post = Crash.post_crash ctl in
  Eio.Path.load (post / "img")

let test_crash_determinism () =
  Alcotest.(check string)
    "same seed same post-crash image" (reorder_image 7) (reorder_image 7);
  (* non-vacuous: the reordered image is a genuine perturbation of the clean,
     fully-written result *)
  let full = String.concat "" (List.init 8 (fun i -> Fmt.str "%08d" i)) in
  Alcotest.(check bool)
    "seeded drop/reorder perturbs the clean image" true
    (reorder_image 7 <> full)

(* {2 Crash rewrite of read-only and subtree-staged files}

   A store that stages immutable artefacts writes them read-only (0o444) and
   through short-lived subtree capabilities. The crash model must rewrite such
   files all the same: the post-crash image reflects the durability barrier,
   the file keeps its mode, and a staging subtree closed long before the crash
   does not strand the file. *)

(* A read-only file whose writes were all synced survives a crash byte-for-byte
   with its mode intact. The rewrite must not fail with EACCES just because the
   file cannot be reopened for writing. *)
let test_crash_readonly_synced () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o444) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  (* an unsynced write on a second file forces the crash to rewrite state *)
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "scratch")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "tmp" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced read-only file survives" "data"
    (Eio.Path.load (post / "img"));
  Alcotest.(check int)
    "read-only mode preserved" 0o444
    (Eio.Path.stat ~follow:true (post / "img")).perm

(* A read-only file with an unsynced tail is rewritten to its synced prefix:
   the model grants itself write access for the rewrite and restores the mode
   after. *)
let test_crash_readonly_unsynced_tail () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o444) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "read-only file rewritten to its synced prefix" "AAAA"
    (Eio.Path.load (post / "img"));
  Alcotest.(check int)
    "read-only mode preserved after rewrite" 0o444
    (Eio.Path.stat ~follow:true (post / "img")).perm

(* A file staged through a short-lived subtree ([with_open_dir]) is rewritten
   through the long-lived wrap root: the subtree fd is closed well before the
   crash fires and must not strand the file. *)
let test_crash_closed_subtree () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_dir (fs / "stage") (fun stage ->
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
        Eio.Path.(stage / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ]));
  (* the staging subtree is closed here; only now does the crash fire *)
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "subtree-staged file rewritten through the root" "AAAA"
    (Eio.Path.load (post / "stage" / "img"))

(* The composed hazard, as an OTA release store stages images: a read-only
   file created inside a short-lived staging subtree. *)
let test_crash_readonly_in_closed_subtree () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_dir (fs / "stage") (fun stage ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o444)
        Eio.Path.(stage / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "data" ];
          Eio.File.sync file;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "junk" ]));
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "read-only subtree-staged file rewritten" "data"
    (Eio.Path.load (post / "stage" / "img"));
  Alcotest.(check int)
    "mode preserved" 0o444
    (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm

(* {2 The same hazards against the real filesystem}

   The kernel is the authority on EACCES and fd invalidation; the in-memory
   runs above only count because the backend models both. These runs pin the
   primitive against the real thing. *)

(* Run [fn] over a crash-wrapped private temp directory on the real
   filesystem, then hand the recovered directory to [check]. *)
let with_real_crash ctl fn ~check =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-crash" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  let real = (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t) in
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () ->
  let wrapped = Crash.wrap ctl real in
  fn wrapped;
  check (Crash.post_crash ctl)

let test_crash_readonly_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o444) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ]))
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: read-only file rewritten to its synced prefix" "AAAA"
        (Eio.Path.load (post / "img"));
      Alcotest.(check int)
        "real fs: mode preserved" 0o444
        (Eio.Path.stat ~follow:true (post / "img")).perm)

let test_crash_closed_subtree_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.mkdir ~perm:0o755 (fs / "stage");
      Eio.Path.with_open_dir (fs / "stage") (fun stage ->
          Eio.Path.with_open_out ~create:(`Exclusive 0o444)
            Eio.Path.(stage / "img")
            (fun file ->
              Eio.File.pwrite_all file ~file_offset:(i63 0)
                [ Cstruct.of_string "data" ];
              Eio.File.sync file;
              Eio.File.pwrite_all file ~file_offset:(i63 4)
                [ Cstruct.of_string "junk" ])))
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: read-only subtree-staged file rewritten" "data"
        (Eio.Path.load (post / "stage" / "img"));
      Alcotest.(check int)
        "real fs: mode preserved" 0o444
        (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm)

(* An [After_write] trigger crashes the moment the covering write commits; the
   next op through the wrapper fails, and the unsynced block is lost. *)
let test_crash_after_write_trigger () =
  let ctl =
    Crash.v ~policy:Drop_unsynced
      ~trigger:(Crash.After_write { path = "img"; file_offset = 0 })
      ()
  in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  (try
     Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img")
       (fun file ->
         Eio.File.pwrite_all file ~file_offset:(i63 0)
           [ Cstruct.of_string "block" ];
         (* the crash has fired; a further op must fail *)
         Eio.File.pwrite_all file ~file_offset:(i63 5)
           [ Cstruct.of_string "more" ])
   with Eio.Io _ -> ());
  Alcotest.(check bool) "crash fired on the write" true (Crash.is_crashed ctl);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "unsynced block lost after auto-crash" ""
    (Eio.Path.load (post / "img"))

let suite =
  ( "chaos",
    [
      Alcotest.test_case "short read" `Quick test_short_read;
      Alcotest.test_case "short write" `Quick test_short_write;
      Alcotest.test_case "torn write" `Quick test_torn_write;
      Alcotest.test_case "write error" `Quick test_write_error;
      Alcotest.test_case "sync error" `Quick test_sync_error;
      Alcotest.test_case "open error" `Quick test_open_error;
      Alcotest.test_case "stat error" `Quick test_stat_error;
      Alcotest.test_case "scripted fires once" `Quick test_scripted_fires_once;
      Alcotest.test_case "seed determinism" `Quick test_seed_determinism;
      Alcotest.test_case "seed reproduces" `Quick test_seed_reproduces;
      Alcotest.test_case "chaos perturbs" `Quick test_chaos_perturbs;
      Alcotest.test_case "crash: unsynced write lost" `Quick
        test_crash_unsynced_lost;
      Alcotest.test_case "crash: synced write survives" `Quick
        test_crash_synced_survives;
      Alcotest.test_case "crash: barrier respected" `Quick
        test_crash_barrier_respected;
      Alcotest.test_case "crash: barrier not reordered" `Quick
        test_crash_barrier_not_reordered;
      Alcotest.test_case "crash: pointer without data" `Quick
        test_crash_pointer_without_data;
      Alcotest.test_case "crash: seeded determinism" `Quick
        test_crash_determinism;
      Alcotest.test_case "crash: after-write trigger" `Quick
        test_crash_after_write_trigger;
      Alcotest.test_case "crash: read-only synced file" `Quick
        test_crash_readonly_synced;
      Alcotest.test_case "crash: read-only unsynced tail" `Quick
        test_crash_readonly_unsynced_tail;
      Alcotest.test_case "crash: closed staging subtree" `Quick
        test_crash_closed_subtree;
      Alcotest.test_case "crash: read-only in closed subtree" `Quick
        test_crash_readonly_in_closed_subtree;
      Alcotest.test_case "crash: read-only file (real fs)" `Quick
        test_crash_readonly_real;
      Alcotest.test_case "crash: closed subtree (real fs)" `Quick
        test_crash_closed_subtree_real;
    ] )
