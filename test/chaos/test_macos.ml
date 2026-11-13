(* Power cuts under [~os:`Macos]: fsync(2) stops at the drive's cache, which
   a cut loses, and F_FULLFSYNC goes through it. *)

module P = Eio.Path

type barrier = {
  file : Eio.File.rw_ty Eio.Resource.t -> unit;
  dir : Eio.Fs.dir_ty P.t -> unit;
}

(* fsync(2), as [Eio.File.sync] and the portable barriers issue it. *)
let plain = { file = Eio.File.sync; dir = Eio_fs_sync.sync_dir }

(* F_FULLFSYNC on Darwin. *)
let full = { file = Eio_fs_unix.sync_file; dir = Eio_fs_unix.sync_dir }

(* A value made durable with [b], then acknowledged: recover must find it. *)
let save ?os b () =
  Eio_fs.Chaos.Crash.check ?os
    (fun ~ack env ->
      match
        P.with_open_out ~create:(`Exclusive 0o644)
          P.(env#fs / "v")
          (fun f ->
            Eio.Flow.copy_string "v1" f;
            b.file f);
        b.dir env#fs
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if acked <> [] && P.load P.(env#fs / "v") <> "v1" then failwith "lost")

(* Two files synced with fsync(2), then a third with F_FULLFSYNC, which
   drains the drive's queue: the first two are durable. *)
let drained () =
  Eio_fs.Chaos.Crash.check ~os:`Macos
    (fun ~ack env ->
      let write name sync =
        P.with_open_out ~create:(`Exclusive 0o644)
          P.(env#fs / name)
          (fun f ->
            Eio.Flow.copy_string name f;
            sync f)
      in
      match
        write "a" Eio.File.sync;
        write "b" Eio.File.sync;
        Eio_fs_sync.sync_dir env#fs;
        write "c" Eio_fs_unix.sync_file
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if acked <> [] then
        List.iter
          (fun n -> if P.load P.(env#fs / n) <> n then failwith (n ^ " lost"))
          [ "a"; "b" ])

(* A record removed, then a new log written past what is durable: a sector
   of the log may hold the removed record, which a recover that trusts what
   it reads replays. Copy-on-write does not rule it out on APFS. *)
let replay ?os () =
  let record = String.make 512 'R' in
  Eio_fs.Chaos.Crash.check ?os
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "old") record)
    (fun ~ack:_ env ->
      try
        P.unlink P.(env#fs / "old");
        Eio_fs_sync.sync_dir env#fs;
        P.save ~create:(`Exclusive 0o644)
          P.(env#fs / "log")
          (String.make 1024 'N')
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      match P.load P.(env#fs / "log") with
      | s when String.length s >= 512 && String.sub s 0 512 = record ->
          failwith "replayed a removed record"
      | _ | (exception Eio.Io _) -> ())

let replayed os () =
  let m = Expect.report (replay ~os) in
  Alcotest.(check string)
    "error" "Failure(\"replayed a removed record\")" (Expect.field "error" m)

let suite =
  ( "macos",
    [
      Alcotest.test_case "fsync is durable on Linux" `Quick
        (Expect.passes (save plain));
      Alcotest.test_case "fsync is not durable on macOS" `Quick
        (Expect.caught ~tier:"linear" (save ~os:`Macos plain));
      Alcotest.test_case "F_FULLFSYNC is durable on macOS" `Quick
        (Expect.passes (save ~os:`Macos full));
      Alcotest.test_case "F_FULLFSYNC drains earlier fsyncs" `Quick
        (Expect.passes drained);
      Alcotest.test_case "a stale record is replayed on Linux" `Quick
        (replayed `Linux);
      Alcotest.test_case "a stale record is replayed on macOS" `Quick
        (replayed `Macos);
    ] )
