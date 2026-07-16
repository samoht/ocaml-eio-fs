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
    ] )
