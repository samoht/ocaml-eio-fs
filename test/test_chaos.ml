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

(* A [Swap_symlink] fault reproduces a TOCTOU: the scripted [`Stat] on "victim"
   still returns the original regular file, then the path becomes a symlink to
   the secret, so a follow-up open on the same path -- as a builder that stats
   then opens would do -- resolves to what was swapped in. The check saw one
   thing; the use sees another. *)
let test_swap_symlink () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "victim" };
          fault = Swap_symlink "secret";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "victim") "safe";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "secret") "SECRET";
  (* The check: the stat through chaos still sees the original regular file... *)
  let st = Eio.Path.stat ~follow:false (chaos / "victim") in
  Alcotest.(check bool)
    "the checked path is a regular file at stat time" true
    (match st.Eio.File.Stat.kind with `Regular_file -> true | _ -> false);
  (* ...but the fault swapped it underneath: the path is now a symlink to the
     secret, so the use that follows the check reads what was swapped in. *)
  Alcotest.(check bool)
    "the path became a symlink after the stat" true
    (match Eio.Path.kind ~follow:false (raw / "victim") with
    | `Symbolic_link -> true
    | _ -> false);
  Alcotest.(check string)
    "the swapped-in link points at the secret" "secret"
    (Eio.Path.read_link (raw / "victim"))

(* The whole of a file, read through the handle rather than through its name. *)
let contents file =
  let len = Optint.Int63.to_int (Eio.File.size file) in
  let buf = Cstruct.create len in
  Eio.File.pread_exact file ~file_offset:(i63 0) [ buf ];
  Cstruct.to_string buf

(* An open leaves the same window a stat does, and the descriptor it hands back
   is what makes it worth exploiting: the caller keeps the file the name meant at
   the open, while the name goes on to mean the target, so everything the caller
   asks the path afterwards -- how long it is, what it holds -- is answered about
   another file. Scripted on the [`Open], the fault pins that window for a
   reader. *)
let test_swap_symlink_on_open_in () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open; path = "current" };
          fault = Swap_symlink "decoy";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "log") "the original";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "decoy") "decoy";
  Eio.Path.symlink ~link_to:"log" (raw / "current");
  let read = Eio.Path.with_open_in (chaos / "current") contents in
  Alcotest.(check string)
    "the handle reads the file the name meant at the open" "the original" read;
  Alcotest.(check string)
    "and the name points at the target from then on" "decoy"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "so the path answers about the other file" "decoy"
    (Eio.Path.load (raw / "current"))

(* The writer's half of the window, which costs more: the bytes go to the file
   the open returned, and the name they were written under has come to mean
   another file, so a reader following the name finds neither them nor the file
   they landed in. *)
let test_swap_symlink_on_open_out () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open; path = "current" };
          fault = Swap_symlink "decoy";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "log") "";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "decoy") "decoy";
  Eio.Path.symlink ~link_to:"log" (raw / "current");
  Eio.Path.with_open_out ~create:(`If_missing 0o644) (chaos / "current")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string "written" ]);
  Alcotest.(check string)
    "the write lands in the file the name meant at the open" "written"
    (Eio.Path.load (raw / "log"));
  Alcotest.(check string)
    "the file the name means now is untouched" "decoy"
    (Eio.Path.load (raw / "decoy"));
  Alcotest.(check string)
    "and the name points at it" "decoy"
    (Eio.Path.read_link (raw / "current"))

(* A subtree opens the same way, and everything reached through the handle keeps
   resolving under the directory it was opened on while the name it was opened
   under leads elsewhere. *)
let test_swap_symlink_on_open_subtree () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open; path = "current" };
          fault = Swap_symlink "other";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.symlink ~link_to:"tree" (raw / "current");
  let read =
    Eio.Path.with_open_dir (chaos / "current") @@ fun dir ->
    Eio.Path.load (dir / "f")
  in
  Alcotest.(check string)
    "the handle reads under the directory the name meant at the open"
    "in the tree" read;
  Alcotest.(check string)
    "and the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* {2 A swap over a real POSIX filesystem}

   Unlinking a directory is EPERM on Darwin and EISDIR on Linux, so freeing a
   name with a plain unlink is portable for a symlink and a regular file and
   nothing else. The swap must free its name kind-aware: clear the entries,
   then [try rmdir with Eio.Io _ -> unlink]. The two directory cases are
   separate on purpose, since a staging directory a store swaps under a caller
   is never empty. *)

(* Run [fn] over a private temp directory on the real filesystem, opened as a
   subtree so the paths the schedule matches are relative to it. *)
let with_real_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_file "eio-fs-chaos-swap" "" in
  Eio.Path.unlink (fs / name);
  Eio.Path.mkdir ~perm:0o755 (fs / name);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / name))
  @@ fun () -> Eio.Path.with_open_dir (fs / name) fn

let swap_on_open path target =
  Schedule.scripted
    [
      {
        trigger = Op_on_path { kind = `Open; path };
        fault = Swap_symlink target;
      };
    ]

(* The name the swap frees is a symlink: the portable case, and the guard rail
   for the fixture -- if this one fails the real-filesystem harness is broken,
   not the directory cases under it. *)
let test_swap_on_real_symlink_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.symlink ~link_to:"tree" (raw / "current");
  let read =
    Eio.Path.with_open_dir (chaos / "current") @@ fun dir ->
    Eio.Path.load (dir / "f")
  in
  Alcotest.(check string)
    "real fs: the handle reads under the tree the name meant at the open"
    "in the tree" read;
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* An empty true directory. The open completes, the name is a symlink to the
   target afterwards, and every path-based follow-up is answered about the
   target -- the whole point of the fault. *)
let test_swap_on_real_empty_directory_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "current");
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.with_open_dir (chaos / "current") (fun (_ : _ Eio.Path.t) -> ());
  Alcotest.(check bool)
    "real fs: the empty directory name became a symlink" true
    (Eio.Path.kind ~follow:false (raw / "current") = `Symbolic_link);
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* A populated true directory -- the shape a store's staging directory has, and
   the shape the in-memory subtree case above uses. Freeing this name takes the
   entries with it, so the swap must clear them rather than stop at ENOTEMPTY.
*)
let test_swap_on_real_populated_directory_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "current");
  Eio.Path.save ~create:(`Or_truncate 0o644)
    (raw / "current" / "f")
    "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.with_open_dir (chaos / "current") (fun (_ : _ Eio.Path.t) -> ());
  Alcotest.(check bool)
    "real fs: the populated directory name became a symlink" true
    (Eio.Path.kind ~follow:false (raw / "current") = `Symbolic_link);
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

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

let absent p = Eio.Path.kind ~follow:false p = `Not_found

(* A write with no intervening sync is lost after a crash -- and so is the
   file itself: its creation is an uncommitted directory operation, so no
   empty ghost file lingers. *)
let test_crash_unsynced_lost () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "uncommitted file gone after crash" true
    (absent (post / "img"))

(* A write followed by a sync survives a crash. The metadata barrier is what
   makes the new file's name durable; the file sync covers its contents. Every
   scenario below that creates a file and expects to find it again needs both,
   as a store writing a new file on real hardware does. *)
let test_crash_synced_survives () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
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
  (* the creation itself may not survive the seeded journal cut *)
  try Eio.Path.load (post / "img") with Eio.Io _ -> "<absent>"

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

(* A file already on disk when the model wraps the tree is as durable as its
   name: nothing the wrapper saw created it. Bytes written through the wrapper
   and synced survive the cut whether or not a metadata barrier came after the
   open, since the open changed no directory entry (fsync(2) commits a file's
   data; its name was never pending). [policy] picks which post-crash image
   the model rebuilds from: [Drop_unsynced] the last committed one, and
   [Survive] every journal entry's, here a mkdir recorded before the open. *)
let preexisting_file_synced_after_wrap ~policy ~pending_entry () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "journal") "first";
  let ctl = Crash.v ~policy () in
  let fs = Crash.wrap ctl raw in
  if pending_entry then Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`If_missing 0o644) (fs / "journal")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 5)
        [ Cstruct.of_string "second" ];
      Eio.File.sync file);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced bytes of a pre-existing file survive" "firstsecond"
    (Eio.Path.load (post / "journal"))

(* A read-only file whose writes were all synced survives a crash byte-for-byte
   with its mode intact. The rewrite must not fail with EACCES just because the
   file cannot be reopened for writing. *)
let test_crash_readonly_synced () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o444) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "read-only subtree-staged file rewritten" "data"
    (Eio.Path.load (post / "stage" / "img"));
  Alcotest.(check int)
    "mode preserved" 0o444
    (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm

(* {2 Directory-operation durability}

   Directory operations -- file creation, mkdir, unlink, rmdir, symlink,
   rename -- are journaled in program order and commit on [sync_metadata] (the
   stand-in for a directory fsync), never on a file sync: fsync(2) on a
   descriptor makes that file's data durable and says nothing about the
   directory its name lives in. A crash rolls back the uncommitted suffix,
   whole. The spec is the journal model in chaos.mli. *)

(* An unlink committed by the metadata barrier stays deleted: the crash must
   not resurrect the file from its tracked durable content. *)
let test_crash_committed_unlink_stays_deleted () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.unlink (fs / "f");
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "committed unlink stays deleted" true
    (absent (post / "f"))

(* Syncing a file commits that file's data, not the directory's entries:
   fsync(2) on a descriptor says nothing about the directory a name lives in.
   So an unlink still pending when some *other* file is synced must roll back,
   exactly as it would had the machine lost power right there.

   Conflating the two hides a class of bug rather than a corner of one. A store
   that removes a journal or a log and reaches for the file barrier instead of
   the directory barrier tests green here and loses data on real hardware: that
   is how the WAL half of a durability bug in ocaml-sqlite stayed invisible
   while the journal half was caught. *)
let test_crash_file_sync_leaves_metadata_pending () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  Eio.Path.unlink (fs / "f");
  (* the barrier below is on a different file, so it must not commit the
     unlink above *)
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "other") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ];
      Eio.File.sync file);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "another file's sync did not commit the unlink" "data"
    (Eio.Path.load (post / "f"))

(* An unlink that never committed rolls back: the file reappears with its
   durable content, exactly as a lost directory update leaves it on disk. *)
let test_crash_uncommitted_unlink_rolls_back () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  Eio.Path.unlink (fs / "f");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "uncommitted unlink rolled back" "data"
    (Eio.Path.load (post / "f"))

(* The tmp-file-then-rename commit protocol with no barrier after the rename:
   the swap reverts whole. The file is back at the temporary path, the final
   path is absent -- never a mixture, never both. *)
let test_crash_uncommitted_rename_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  Eio.Path.rename (fs / "tmp") (fs / "final");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "file back at the temporary path" "NEW"
    (Eio.Path.load (post / "tmp"));
  Alcotest.(check bool)
    "final path absent after reverted swap" true
    (absent (post / "final"))

(* The same protocol with the barrier: [sync_metadata] after the rename pins
   the swap, exactly as fsyncing the parent directory would. *)
let test_crash_sync_metadata_commits_rename () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  Eio.Path.rename (fs / "tmp") (fs / "final");
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "committed swap survives" "NEW"
    (Eio.Path.load (post / "final"));
  Alcotest.(check bool)
    "temporary path gone after committed swap" true
    (absent (post / "tmp"))

(* A reverted rename restores the entry it replaced: the old target contents
   come back, and the source keeps its bytes at its original path. *)
let test_crash_reverted_rename_restores_victim () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "final") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "OLD" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  Eio.Path.rename (fs / "tmp") (fs / "final");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "clobbered target restored" "OLD"
    (Eio.Path.load (post / "final"));
  Alcotest.(check string)
    "source back at its path" "NEW"
    (Eio.Path.load (post / "tmp"))

(* An uncommitted mkdir takes its whole subtree with it: the directory and the
   file created inside both vanish, since neither entry ever became durable. *)
let test_crash_uncommitted_mkdir_vanishes () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool) "uncommitted directory gone" true (absent (post / "d"))

(* The metadata barrier commits the journal whole: the mkdir and the creation
   inside it become durable together, and the file's synced bytes come back with
   them. Syncing the file alone would leave both directory entries pending, and
   the crash would take the whole subtree with them. *)
let test_crash_sync_metadata_commits_mkdir () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "metadata barrier committed the enclosing mkdir" "x"
    (Eio.Path.load (post / "d" / "f"))

(* An uncommitted symlink vanishes at the crash. *)
let test_crash_uncommitted_symlink_vanishes () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.symlink ~link_to:"target" (fs / "lnk");
  let post = Crash.post_crash ctl in
  Alcotest.(check bool) "uncommitted symlink gone" true (absent (post / "lnk"))

(* The current-pointer swap of a release store: replacing a committed symlink
   through a temporary name reverts whole when uncommitted -- the old pointer
   target is served, and the temporary never lingers. *)
let test_crash_symlink_swap_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.symlink ~link_to:"releases/a" (fs / "current");
  Crash.sync_metadata ctl;
  Eio.Path.symlink ~link_to:"releases/b" (fs / ".tmp");
  Eio.Path.rename (fs / ".tmp") (fs / "current");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "old pointer target restored" "releases/a"
    (Eio.Path.read_link (post / "current"));
  Alcotest.(check bool) "temporary symlink gone" true (absent (post / ".tmp"))

(* An uncommitted rmdir rolls back: the (empty) directory reappears. *)
let test_crash_uncommitted_rmdir_restored () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Crash.sync_metadata ctl;
  Eio.Path.rmdir (fs / "d");
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "uncommitted rmdir rolled back" true
    (Eio.Path.kind ~follow:false (post / "d") = `Directory)

(* A directory rename carries its tracked children. Uncommitted, the whole
   subtree reverts to the staging path; committed, it lives at the final
   path -- exactly the staging-directory publish of a release store. *)
let test_crash_dir_rename_uncommitted_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "stage" / "img")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  Eio.Path.rename (fs / "stage") (fs / "final");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "subtree back at the staging path" "data"
    (Eio.Path.load (post / "stage" / "img"));
  Alcotest.(check bool) "final directory absent" true (absent (post / "final"))

let test_crash_dir_rename_committed_moves () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "stage" / "img")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.rename (fs / "stage") (fs / "final");
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "subtree at the final path" "data"
    (Eio.Path.load (post / "final" / "img"));
  Alcotest.(check bool)
    "staging directory absent" true
    (absent (post / "stage"))

(* Metadata survival is a prefix of program order: refusing an early directory
   op drops everything after it, even ops the predicate would accept, because
   a journal never commits out of order. *)
let test_crash_survive_prefix_rule () =
  let pred (u : Crash.unsynced) =
    match u.kind with Crash.Mkdir -> false | _ -> true
  in
  let ctl = Crash.v ~policy:(Crash.Survive pred) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "rejected mkdir cuts the journal before the accepted create" true
    (absent (post / "d"))

(* Directory operations count as durability ops for the [Nth_op] trigger. *)
let test_crash_nth_op_counts_dir_ops () =
  let ctl = Crash.v ~policy:Drop_unsynced ~trigger:(Crash.Nth_op 1) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Alcotest.(check bool) "mkdir fired the trigger" true (Crash.is_crashed ctl)

(* The pending-change listing names directory operations in program order. *)
let test_crash_unsynced_lists_dir_ops () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "xyz" ]);
  let kinds = List.map (fun u -> u.Crash.kind) (Crash.unsynced ctl) in
  Alcotest.(check bool)
    "journal lists mkdir, create, data in program order" true
    (kinds = [ Crash.Mkdir; Crash.Create; Crash.Data ])

(* {2 The directory barrier is scoped to one directory}

   [Eio_fs_sync.sync_dir] through the crash wrapper is the model's metadata
   barrier, and fsync(2) fixes what one barrier covers: syncing directory D
   makes the entries OF D durable -- the journal entries whose recorded path's
   parent is D -- and says nothing about D's own entry in its parent, nor about
   a sibling of D, nor about an entry one level further down. A barrier that
   commits the whole journal cannot tell a store that barriered the right
   directory from one that barriered any directory at all, so a missing barrier
   on one level of a mkdirs reads as durable.

   The tightening leaves correct call sites alone: a store barriers the parent
   directory of the file it just published, which is exactly the directory that
   holds that file's entry, so ocaml-sqlite's crash tests stay green. It does
   not close the window that a crash trigger on a directory operation opens
   either -- there both operations are entries of the SAME directory, so the
   barrier that commits one commits the other whatever its scope. Both gaps are
   real and they need different changes. *)

(* Syncing a directory does not commit that directory's own name: [a]'s entry
   lives in the root, and only the root's barrier makes it durable. The sibling
   [b] is pending for the same reason. *)
let test_crash_dir_sync_leaves_its_own_entry_pending () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio_fs_sync.sync_dir (fs / "a");
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the barriered directory's own entry stayed pending" true
    (absent (post / "a"));
  Alcotest.(check bool) "and so did its sibling" true (absent (post / "b"))

(* The entries the barrier does commit are the ones inside it: with [a] made
   durable by the root's barrier, syncing [a] commits the file created in [a]
   and leaves [b], created in the root, pending. *)
let test_crash_dir_sync_commits_its_own_entries () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio_fs_sync.sync_dir fs;
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "a" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio_fs_sync.sync_dir (fs / "a");
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the entry created in the barriered directory survives" "data"
    (Eio.Path.load (post / "a" / "f"));
  Alcotest.(check bool)
    "the sibling created in the root did not" true
    (absent (post / "b"))

(* The root's barrier commits the root's entries, both of them: scoping the
   commit must not shrink it to one path. *)
let test_crash_root_sync_commits_both_siblings () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio_fs_sync.sync_dir fs;
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "first sibling durable" true
    (Eio.Path.kind ~follow:false (post / "a") = `Directory);
  Alcotest.(check bool)
    "second sibling durable" true
    (Eio.Path.kind ~follow:false (post / "b") = `Directory)

(* One level, not the whole subtree: syncing [a] commits the mkdir of [a/sub],
   whose entry is in [a], and not the file created inside [a/sub], whose entry
   is in [a/sub]. A store that barriers the top of a mkdirs and expects the
   bottom to be durable is the bug the model has to show. *)
let test_crash_dir_sync_does_not_reach_a_grandchild () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio_fs_sync.sync_dir fs;
  Eio.Path.mkdir ~perm:0o755 (fs / "a" / "sub");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "a" / "sub" / "g")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio_fs_sync.sync_dir (fs / "a");
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the child directory, an entry of the barriered one, is durable" true
    (Eio.Path.kind ~follow:false (post / "a" / "sub") = `Directory);
  Alcotest.(check bool)
    "the file one level further down is not" true
    (absent (post / "a" / "sub" / "g"))

(* A write-only (0o200) file is still rewritten at crash time: the rewrite
   opens O_RDWR, which needs the read bit too, so the model grants owner
   read and write for the duration and restores the exact mode after. *)
let test_crash_write_only_file () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o200) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check int)
    "write-only mode preserved after rewrite" 0o200
    (Eio.Path.stat ~follow:true (post / "img")).perm;
  Eio.Path.chmod ~follow:true ~perm:0o644 (post / "img");
  Alcotest.(check string)
    "write-only file rewritten to its synced prefix" "AAAA"
    (Eio.Path.load (post / "img"))

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
            [ Cstruct.of_string "BBBB" ]);
      Crash.sync_metadata ctl)
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
                [ Cstruct.of_string "junk" ]));
      Crash.sync_metadata ctl)
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: read-only subtree-staged file rewritten" "data"
        (Eio.Path.load (post / "stage" / "img"));
      Alcotest.(check int)
        "real fs: mode preserved" 0o444
        (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm)

(* The kernel-side variant of the reverted tmp-rename: the rollback must
   physically undo a rename on the real filesystem. *)
let test_crash_rename_revert_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "NEW" ];
          Eio.File.sync file);
      Crash.sync_metadata ctl;
      Eio.Path.rename (fs / "tmp") (fs / "final"))
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: file back at the temporary path" "NEW"
        (Eio.Path.load (post / "tmp"));
      Alcotest.(check bool)
        "real fs: final path absent" true
        (absent (post / "final")))

(* {2 The model's authority is the wrapper, not the descriptor}

   Over a host directory the wrapper hands back the base's own file, and a
   consumer that needs the descriptor underneath -- to take a record lock, to
   give a launcher a console -- gets the host's. That descriptor names the same
   bytes the model is tracking, so what it is for has to be settled: it carries
   the operations the operating system offers on an open file, and it is not a
   second way into the durability journal. A write issued on it is not a write
   the model saw; a barrier issued on it is not the model's barrier. The two
   runs below take the descriptor the wrapper carried over -- the same open
   file its own writes go through -- and put it next to those writes under the
   two policies that tell them apart. *)

(* A file opened through the wrapper over a host directory carries the host's
   descriptor; a run that could not take it would be testing nothing, so the
   absence is a failure and not a skip. *)
let carried_over_fd file =
  match Eio_unix.Resource.fd_opt file with
  | Some fd -> fd
  | None -> Alcotest.fail "the wrapper carried no descriptor over"

(* Survive-everything keeps every change the journal holds, so what is missing
   from the recovered image is exactly what never entered the journal. The live
   file carries all three writes; the image carries the two that went through
   the wrapper. *)
let test_crash_omits_a_write_through_a_bare_descriptor () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          Crash.sync_metadata ctl;
          (* through the wrapper: unsynced, journalled, kept by this policy *)
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ];
          (* on the descriptor the wrapper carried over: never journalled *)
          Eio_unix.Fd.use_exn "bare descriptor" (carried_over_fd file)
            (fun raw ->
              ignore (Unix.lseek raw 8 Unix.SEEK_SET : int);
              Alcotest.(check int)
                "the bare descriptor wrote its bytes" 4
                (Unix.write_substring raw "XXXX" 0 4)));
      Alcotest.(check string)
        "the live file carries all three writes" "AAAABBBBXXXX"
        (Eio.Path.load (fs / "img")))
    ~check:(fun post ->
      Alcotest.(check string)
        "recovery replays the journal, which the bare descriptor never entered"
        "AAAABBBB"
        (Eio.Path.load (post / "img")))

(* The barrier half: an fsync on the carried-over descriptor pushes the host
   file's bytes to the platter, which is what the model must not read as a sync
   of its own. What marks a file's writes durable is the wrapper's [sync];
   after a barrier the model never saw, the write is still unsynced and a
   Drop_unsynced crash takes it. *)
let test_crash_ignores_a_barrier_through_a_bare_descriptor () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          Crash.sync_metadata ctl;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ];
          Eio_unix.Fd.use_exn "bare descriptor" (carried_over_fd file)
            Unix.fsync);
      Alcotest.(check string)
        "the live file carries the unsynced write" "AAAABBBB"
        (Eio.Path.load (fs / "img")))
    ~check:(fun post ->
      Alcotest.(check string)
        "a barrier the model never saw left the write unsynced" "AAAA"
        (Eio.Path.load (post / "img")))

(* The by-name half, and the sharper one: a descriptor the wrapper carried over
   at least names a file the model is tracking, but a name reaches the tree
   whether or not the model has ever heard of what it reaches. A crash rebuilds
   the whole wrapped tree from the model, so an entry made under a native name
   after the wrap -- by a child process, by a device-image builder, by a test's
   own setup -- is not in the post-crash image, whatever the policy would have
   kept. Survive-everything keeps every change the journal holds, so the file
   that is missing here is missing for having never entered it.

   The boundary is what {!Chaos.Crash.wrap} states; this pins it, because a
   workload that reaches the tree by name reads a recovered image that never
   held its bytes and calls that a durability result. A test that must plant
   something by name plants it before the wrap, where the snapshot picks it
   up. *)
let test_crash_drops_an_entry_planted_by_native_name () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  let planted = ref "" in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file);
      Crash.sync_metadata ctl;
      (* by name, as a child process writing into the tree would: the wrapper
         is not on the path this write takes *)
      planted := Eio.Path.native_exn (fs / "device.img");
      let oc = open_out_bin !planted in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc "DEVICE");
      Alcotest.(check bool)
        "the planted entry is live in the tree" true (Sys.file_exists !planted))
    ~check:(fun post ->
      Alcotest.(check string)
        "what the model journalled comes back" "AAAA"
        (Eio.Path.load (post / "img"));
      Alcotest.(check bool)
        "an entry planted by name after the wrap is not in the image" true
        (absent (post / "device.img")))

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
  Alcotest.(check bool)
    "uncommitted file gone after auto-crash" true
    (absent (post / "img"))

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
      Alcotest.test_case "swap-symlink TOCTOU" `Quick test_swap_symlink;
      Alcotest.test_case "swap-symlink on open_in" `Quick
        test_swap_symlink_on_open_in;
      Alcotest.test_case "swap-symlink on open_out" `Quick
        test_swap_symlink_on_open_out;
      Alcotest.test_case "swap-symlink on open_subtree" `Quick
        test_swap_symlink_on_open_subtree;
      Alcotest.test_case "swap on a symlink subtree (real fs)" `Quick
        test_swap_on_real_symlink_subtree;
      Alcotest.test_case "swap on an empty directory subtree (real fs)" `Quick
        test_swap_on_real_empty_directory_subtree;
      Alcotest.test_case "swap on a populated directory subtree (real fs)"
        `Quick test_swap_on_real_populated_directory_subtree;
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
      Alcotest.test_case "pre-existing file synced after the wrap" `Quick
        (preexisting_file_synced_after_wrap ~policy:Crash.Drop_unsynced
           ~pending_entry:false);
      Alcotest.test_case "pre-existing file synced behind a pending entry"
        `Quick
        (preexisting_file_synced_after_wrap
           ~policy:(Crash.Survive (fun _ -> true))
           ~pending_entry:true);
      Alcotest.test_case "crash: read-only synced file" `Quick
        test_crash_readonly_synced;
      Alcotest.test_case "crash: read-only unsynced tail" `Quick
        test_crash_readonly_unsynced_tail;
      Alcotest.test_case "crash: closed staging subtree" `Quick
        test_crash_closed_subtree;
      Alcotest.test_case "crash: read-only in closed subtree" `Quick
        test_crash_readonly_in_closed_subtree;
      Alcotest.test_case "crash: write-only file" `Quick
        test_crash_write_only_file;
      Alcotest.test_case "crash: read-only file (real fs)" `Quick
        test_crash_readonly_real;
      Alcotest.test_case "crash: closed subtree (real fs)" `Quick
        test_crash_closed_subtree_real;
      Alcotest.test_case "crash: committed unlink stays deleted" `Quick
        test_crash_committed_unlink_stays_deleted;
      Alcotest.test_case "crash: uncommitted unlink rolls back" `Quick
        test_crash_uncommitted_unlink_rolls_back;
      Alcotest.test_case "crash: a file sync leaves metadata pending" `Quick
        test_crash_file_sync_leaves_metadata_pending;
      Alcotest.test_case "crash: uncommitted rename reverts" `Quick
        test_crash_uncommitted_rename_reverts;
      Alcotest.test_case "crash: sync_metadata commits rename" `Quick
        test_crash_sync_metadata_commits_rename;
      Alcotest.test_case "crash: reverted rename restores victim" `Quick
        test_crash_reverted_rename_restores_victim;
      Alcotest.test_case "crash: uncommitted mkdir vanishes" `Quick
        test_crash_uncommitted_mkdir_vanishes;
      Alcotest.test_case "crash: sync_metadata commits mkdir" `Quick
        test_crash_sync_metadata_commits_mkdir;
      Alcotest.test_case "crash: uncommitted symlink vanishes" `Quick
        test_crash_uncommitted_symlink_vanishes;
      Alcotest.test_case "crash: symlink swap reverts" `Quick
        test_crash_symlink_swap_reverts;
      Alcotest.test_case "crash: uncommitted rmdir restored" `Quick
        test_crash_uncommitted_rmdir_restored;
      Alcotest.test_case "crash: dir rename uncommitted reverts" `Quick
        test_crash_dir_rename_uncommitted_reverts;
      Alcotest.test_case "crash: dir rename committed moves" `Quick
        test_crash_dir_rename_committed_moves;
      Alcotest.test_case "crash: survive prefix rule" `Quick
        test_crash_survive_prefix_rule;
      Alcotest.test_case "crash: nth-op counts dir ops" `Quick
        test_crash_nth_op_counts_dir_ops;
      Alcotest.test_case "crash: unsynced lists dir ops" `Quick
        test_crash_unsynced_lists_dir_ops;
      Alcotest.test_case "crash: dir sync leaves its own entry pending" `Quick
        test_crash_dir_sync_leaves_its_own_entry_pending;
      Alcotest.test_case "crash: dir sync commits its own entries" `Quick
        test_crash_dir_sync_commits_its_own_entries;
      Alcotest.test_case "crash: root sync commits both siblings" `Quick
        test_crash_root_sync_commits_both_siblings;
      Alcotest.test_case "crash: dir sync does not reach a grandchild" `Quick
        test_crash_dir_sync_does_not_reach_a_grandchild;
      Alcotest.test_case "crash: rename revert (real fs)" `Quick
        test_crash_rename_revert_real;
      Alcotest.test_case "crash: a bare descriptor's write is not journalled"
        `Quick test_crash_omits_a_write_through_a_bare_descriptor;
      Alcotest.test_case "crash: a bare descriptor's barrier is not the model's"
        `Quick test_crash_ignores_a_barrier_through_a_bare_descriptor;
      Alcotest.test_case "crash: an entry planted by name is not in the image"
        `Quick test_crash_drops_an_entry_planted_by_native_name;
    ] )
