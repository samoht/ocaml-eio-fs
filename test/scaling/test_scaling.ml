(* Each bound is processor time, not wall time, and sits about a hundred
   times above what the linear code takes, so only a cost that grows with
   the square of the size can reach it. *)

let ( / ) = Eio.Path.( / )

let within ~seconds what fn =
  let t0 = Sys.time () in
  fn ();
  let dt = Sys.time () -. t0 in
  if dt > seconds then
    Alcotest.failf "%s took %.1fs of processor time, over its %.0fs bound" what
      dt seconds

(* 80k creates in one directory: a list of entries made each one a scan. *)
let test_one_directory () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  within ~seconds:10. "80k creates and stats in one directory" (fun () ->
      for i = 1 to 80_000 do
        Eio.Path.save ~create:(`Exclusive 0o644) (fs / string_of_int i) "x"
      done;
      for i = 1 to 80_000 do
        ignore (Eio.Path.stat ~follow:false (fs / string_of_int i))
      done)

module Crash = Eio_fs.Chaos.Crash

(* 32k appends replayed by a sync and by a seeded crash: a replay that
   reallocated the file per write copied it once per append. *)
let test_crash_appends () =
  Eio_mock.Backend.run @@ fun () ->
  let ctl = Crash.v ~policy:(Crash.Reorder_unsynced 1) () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.v ()) in
  let line = String.make 100 'x' in
  within ~seconds:10. "32k appends, a sync, 32k appends and a crash" (fun () ->
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
      Crash.crash ctl)

(* 16k syncs of one file while another holds 64k pending writes: a sync that
   scanned every file's changes did 10^9 steps. *)
let test_crash_syncs_beside_pending_writes () =
  Eio_mock.Backend.run @@ fun () ->
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.v ()) in
  let i63 = Optint.Int63.of_int in
  Eio.Switch.run @@ fun sw ->
  let dirty = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (fs / "dirty") in
  let hot = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (fs / "hot") in
  within ~seconds:10. "16k syncs beside 64k pending writes" (fun () ->
      for i = 1 to 64_000 do
        Eio.File.pwrite_all dirty ~file_offset:(i63 i) [ Cstruct.of_string "y" ]
      done;
      for _ = 1 to 16_000 do
        Eio.File.pwrite_all hot ~file_offset:(i63 0) [ Cstruct.of_string "z" ];
        Eio.File.sync hot
      done)

let suite =
  ( "scaling",
    [
      Alcotest.test_case "one directory" `Slow test_one_directory;
      Alcotest.test_case "crash: appends" `Slow test_crash_appends;
      Alcotest.test_case "crash: syncs beside pending writes" `Slow
        test_crash_syncs_beside_pending_writes;
    ] )
