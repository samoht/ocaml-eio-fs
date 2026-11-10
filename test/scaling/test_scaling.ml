(* Each workload runs at a size where a cost linear in it stays far below
   its bound and a quadratic one far above, and the bound is on work, never
   on time: words the minor heap allocated, or bytes a replay copied. Both
   are the same on every run. *)

let ( / ) = Eio.Path.( / )

module Crash = Eio_fs.Chaos.Crash

let i63 = Optint.Int63.of_int

let words fn =
  let before = Gc.minor_words () in
  fn ();
  Gc.minor_words () -. before

let at_most what ~bound got =
  if got > bound then
    Alcotest.failf "%s: %.0f, over its bound of %.0f" what got bound

(* 32k appends replayed by a sync and by a seeded crash: a replay that
   reallocated the file per write copied it once per append, 10^11 bytes. *)
let test_crash_appends () =
  Eio_mock.Backend.run @@ fun () ->
  let ctl = Crash.v ~policy:(Crash.Reorder_unsynced 1) () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.v ()) in
  let line = String.make 100 'x' in
  let before = Eio_fs.Chaos.Private.replay_copied () in
  Eio.Switch.run (fun sw ->
      let f =
        Eio.Path.open_out ~sw ~append:true ~create:(`Exclusive 0o644)
          (fs / "log")
      in
      for _ = 1 to 32_000 do
        Eio.Flow.copy_string line f
      done;
      Eio.File.sync f;
      for _ = 1 to 32_000 do
        Eio.Flow.copy_string line f
      done);
  Crash.crash ctl;
  let copied = Eio_fs.Chaos.Private.replay_copied () - before in
  at_most "bytes copied, for 6.4 MB of appends" ~bound:(8. *. 6.4e6)
    (float copied)

(* Syncs of one file while another holds 64k pending writes: a sync that
   filtered every file's changes allocated a list of them each time. *)
let test_crash_syncs_beside_pending_writes () =
  Eio_mock.Backend.run @@ fun () ->
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.v ()) in
  Eio.Switch.run @@ fun sw ->
  let dirty = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (fs / "dirty") in
  let hot = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (fs / "hot") in
  for i = 1 to 64_000 do
    Eio.File.pwrite_all dirty ~file_offset:(i63 i) [ Cstruct.of_string "y" ]
  done;
  let w =
    words (fun () ->
        for _ = 1 to 1000 do
          Eio.File.pwrite_all hot ~file_offset:(i63 0) [ Cstruct.of_string "z" ];
          Eio.File.sync hot
        done)
  in
  at_most "words per sync, beside 64k pending writes" ~bound:20_000. (w /. 1000.)

(* 20k files made through the crash model, then a seeded crash: a model that
   snapshot the whole tree on every directory operation allocated it each
   time. *)
let test_crash_many_files () =
  Eio_mock.Backend.run @@ fun () ->
  let ctl = Crash.v ~policy:(Crash.Reorder_unsynced 1) () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.v ()) in
  let w =
    words (fun () ->
        for d = 0 to 99 do
          let dir = fs / string_of_int d in
          Eio.Path.mkdir ~perm:0o755 dir;
          for i = 1 to 100 do
            Eio.Path.save ~create:(`Exclusive 0o644) (dir / string_of_int i) "x"
          done
        done;
        Eio.Path.mkdir ~perm:0o755 (fs / "flat");
        for i = 1 to 10_000 do
          Eio.Path.save ~create:(`Exclusive 0o644)
            (fs / "flat" / string_of_int i)
            "x"
        done;
        Crash.crash ctl)
  in
  at_most "words per file, for 20k files and a crash" ~bound:50_000.
    (w /. 20_000.)

(* 8000 rounds of a create, a sync, a barrier, an unlink and a barrier in
   one directory, while one create elsewhere is never barriered: a barrier
   that scanned the whole journal, which that entry keeps from folding,
   allocated words in proportion to it on every round. *)
let test_crash_barriers_beside_a_pinned_entry () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o755 (raw / "D");
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "pin") "p";
  let w =
    words (fun () ->
        for i = 1 to 8000 do
          let f = fs / "D" / Printf.sprintf "f%d" (i mod 64) in
          Eio.Path.save ~create:(`Or_truncate 0o644) f "abc";
          Eio.Path.with_open_out ~create:`Never f Eio.File.sync;
          Eio_fs_sync.sync_dir (fs / "D");
          Eio.Path.unlink f;
          Eio_fs_sync.sync_dir (fs / "D")
        done)
  in
  at_most "words per round, beside a pinned entry" ~bound:50_000. (w /. 8000.)

let suite =
  ( "scaling",
    [
      Alcotest.test_case "crash: appends" `Quick test_crash_appends;
      Alcotest.test_case "crash: many files" `Quick test_crash_many_files;
      Alcotest.test_case "crash: syncs beside pending writes" `Quick
        test_crash_syncs_beside_pending_writes;
      Alcotest.test_case "crash: barriers beside a pinned entry" `Quick
        test_crash_barriers_beside_a_pinned_entry;
    ] )
