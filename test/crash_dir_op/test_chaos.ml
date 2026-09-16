(* [Crash.After_dir_op { kind; path }] crashes once that directory operation,
   matched by kind and path as the metadata journal records them, has entered
   the journal and before anything can commit it. It is the only way to look
   inside the window a directory operation with no barrier of its own leaves
   open.

   The other triggers cannot reach it: [Manual] fires after the workload,
   [After_write] keys on a byte range an unlink never writes, and [Nth_op] is a
   magic number a refactor of the commit path moves silently. *)

module Crash = Eio_fs.Chaos.Crash

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int
let absent p = Eio.Path.kind ~follow:false p = `Not_found

(* An in-memory tree with durability tracked by [ctl]: after a crash it holds
   what a barrier committed and nothing else. *)
let with_crash ctl fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn (Crash.wrap ctl env#fs)

let save_synced path content =
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) path (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string content ];
      Eio.File.sync file)

(* The operation the trigger names: the machine loses power while it runs, so
   it may return or raise. What is pinned is that the clock stopped there. *)
let fires fn = try fn () with Eio.Io _ -> ()

(* The unlink case, the shape [relay_at_page_size] has: a removal with no
   barrier of its own, followed by an operation whose barrier would commit it.
   The crash fires between them, so the removal rolls back and the operation
   that would have hidden it never runs. *)
let test_after_unlink () =
  let ctl =
    Crash.v ~policy:Drop_unsynced
      ~trigger:(Crash.After_dir_op { kind = Crash.Unlink; path = "a" })
      ()
  in
  with_crash ctl @@ fun fs ->
  save_synced (fs / "a") "A";
  save_synced (fs / "b") "B";
  Crash.sync_metadata ctl;
  fires (fun () -> Eio.Path.unlink (fs / "a"));
  Alcotest.(check bool)
    "the crash fired on the unlink itself" true (Crash.is_crashed ctl);
  (* whatever the store does next runs against a dead filesystem *)
  fires (fun () ->
      save_synced (fs / "c") "C";
      Crash.sync_metadata ctl);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the unbarriered removal rolled back" "A"
    (Eio.Path.load (post / "a"));
  Alcotest.(check string)
    "the file committed before it is untouched" "B"
    (Eio.Path.load (post / "b"));
  Alcotest.(check bool)
    "the barrier that would have committed the removal never ran" true
    (absent (post / "c"))

(* The same workload under [Manual]: the crash at the end sees a journal the
   following barrier already committed, so the removal is durable and the
   missing barrier is invisible. *)
let test_manual_cannot_see_the_window () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  save_synced (fs / "a") "A";
  save_synced (fs / "b") "B";
  Crash.sync_metadata ctl;
  Eio.Path.unlink (fs / "a");
  save_synced (fs / "c") "C";
  Crash.sync_metadata ctl;
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the removal committed on the following barrier" true
    (absent (post / "a"));
  Alcotest.(check string)
    "and so did the file created after it" "C"
    (Eio.Path.load (post / "c"))

(* Kind and path both have to match: the journal's vocabulary is what the
   trigger speaks, so another path's unlink and another kind on the named path
   both leave the clock running. *)
let test_matches_kind_and_path () =
  let ctl =
    Crash.v ~policy:Drop_unsynced
      ~trigger:(Crash.After_dir_op { kind = Crash.Unlink; path = "a" })
      ()
  in
  with_crash ctl @@ fun fs ->
  save_synced (fs / "b") "B";
  Crash.sync_metadata ctl;
  Eio.Path.unlink (fs / "b");
  Alcotest.(check bool)
    "another path's unlink did not fire" false (Crash.is_crashed ctl);
  Eio.Path.symlink ~link_to:"b" (fs / "a");
  Alcotest.(check bool)
    "another kind on the named path did not fire" false (Crash.is_crashed ctl);
  fires (fun () -> Eio.Path.unlink (fs / "a"));
  Alcotest.(check bool) "the named operation fired" true (Crash.is_crashed ctl)

(* A directory removal reaching no barrier: the directory comes back. *)
let test_after_rmdir () =
  let ctl =
    Crash.v ~policy:Drop_unsynced
      ~trigger:(Crash.After_dir_op { kind = Crash.Rmdir; path = "sub" })
      ()
  in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "sub");
  Crash.sync_metadata ctl;
  fires (fun () -> Eio.Path.rmdir (fs / "sub"));
  Alcotest.(check bool)
    "the crash fired on the rmdir itself" true (Crash.is_crashed ctl);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the unbarriered removal rolled back" true
    (Eio.Path.kind ~follow:false (post / "sub") = `Directory)

(* A rename reaching no barrier -- the tmp-then-rename publish with the barrier
   left out. The journal records the rename under the source path with the
   destination in the kind, and the trigger names it the same way. *)
let test_after_rename () =
  let ctl =
    Crash.v ~policy:Drop_unsynced
      ~trigger:
        (Crash.After_dir_op { kind = Crash.Rename { dest = "y" }; path = "x" })
      ()
  in
  with_crash ctl @@ fun fs ->
  save_synced (fs / "x") "data";
  Crash.sync_metadata ctl;
  fires (fun () -> Eio.Path.rename (fs / "x") (fs / "y"));
  Alcotest.(check bool)
    "the crash fired on the rename itself" true (Crash.is_crashed ctl);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the source is back at its path" "data"
    (Eio.Path.load (post / "x"));
  Alcotest.(check bool)
    "the destination never appeared" true
    (absent (post / "y"))

let suite =
  ( "chaos",
    [
      Alcotest.test_case "after unlink" `Quick test_after_unlink;
      Alcotest.test_case "manual cannot see the window" `Quick
        test_manual_cannot_see_the_window;
      Alcotest.test_case "matches kind and path" `Quick
        test_matches_kind_and_path;
      Alcotest.test_case "after rmdir" `Quick test_after_rmdir;
      Alcotest.test_case "after rename" `Quick test_after_rename;
    ] )
