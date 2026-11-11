(* The states a cut may leave, as the sampled tier reaches them. *)

module P = Eio.Path

(* A file [keep] setup made, so recover has something to check. *)
let setup env = Eio_fs_sync.save P.(env#fs / "keep") "k"
let keep env = if P.load P.(env#fs / "keep") <> "k" then failwith "keep lost"

(* A new file whose first sector is stale and whose others hold the write:
   the cut's first state drops the file, so its draws say little of how many
   states the cut allows. *)
let stale_head () =
  let a = String.make 1100 'A' in
  let bad = String.make 512 '7' ^ String.sub a 512 588 in
  Eio_fs.Chaos.Crash.check ~setup
    (fun ~ack:_ env ->
      try P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") a
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      keep env;
      match P.load P.(env#fs / "f") with
      | s when s = bad -> failwith "stale head"
      | _ | (exception Eio.Io _) -> ())

(* A log reset by a synced truncation, acknowledged, then a new frame with
   no sync: the page the frame lands in may hold the old frame the truncation
   freed, which a log with no checksum replays. *)
let frame c = "rec:" ^ String.make 508 c

let old_frame () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env ->
      setup env;
      Eio_fs_sync.save P.(env#fs / "wal") (frame 'A'))
    (fun ~ack env ->
      try
        P.with_open_out ~create:`Never P.(env#fs / "wal") @@ fun f ->
        Eio.File.truncate f Optint.Int63.zero;
        Eio.File.sync f;
        ack ();
        Eio.Flow.copy_string (frame 'B') f
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      keep env;
      if acked <> [] && P.load P.(env#fs / "wal") = frame 'A' then
        failwith "an old frame replayed")

let suite =
  ( "states",
    [
      Alcotest.test_case "a stale sector of a new file is reached" `Quick
        (fun () ->
          let m = Expect.report stale_head in
          Alcotest.(check string)
            "error" "Failure(\"stale head\")" (Expect.field "error" m));
      Alcotest.test_case "a fresh sector may hold what a truncation freed"
        `Quick (fun () ->
          let m = Expect.report old_frame in
          Alcotest.(check string)
            "error" "Failure(\"an old frame replayed\")"
            (Expect.field "error" m));
    ] )
