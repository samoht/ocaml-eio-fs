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

(* A compaction wrote [new] and made it durable, then removed [old] and
   committed the removal: an unsynced append to [new] lands in a block the
   removal freed, and may show the record [old] held there. *)
let sectors s = String.concat "" (List.map (fun c -> String.make 512 c) s)

let freed_by_unlink () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env ->
      setup env;
      Eio_fs_sync.save
        P.(env#fs / "old")
        (sectors [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' ]);
      Eio_fs_sync.save P.(env#fs / "new") (String.make 4096 'n'))
    (fun ~ack env ->
      try
        P.unlink P.(env#fs / "old");
        Eio_fs_sync.sync_dir env#fs;
        ack ();
        P.with_open_out ~append:true ~create:`Never P.(env#fs / "new")
        @@ fun f -> Eio.Flow.copy_string (String.make 512 'x') f
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      keep env;
      match P.load P.(env#fs / "new") with
      | s when String.length s > 4096 && s.[4096] = 'I' ->
          failwith "a removed record replayed"
      | _ | (exception Eio.Io _) -> ())

(* A new page with no sync: its first sector stale under the written ones.
   The cut's first state drops the file, and the states that vary the first
   sector come last one by one, past what the check enumerates. *)
let stale_first () =
  Eio_fs.Chaos.Crash.check ~setup
    (fun ~ack:_ env ->
      try
        P.save ~create:(`Exclusive 0o644)
          P.(env#fs / "f")
          (String.make 4096 'A')
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      keep env;
      match P.load P.(env#fs / "f") with
      | s when String.length s = 4096 && s.[0] = '7' && s.[4095] = 'A' ->
          failwith "stale first sector"
      | _ | (exception Eio.Io _) -> ())

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
      Alcotest.test_case "a fresh sector may hold what a removal freed" `Quick
        (fun () ->
          let m = Expect.report freed_by_unlink in
          Alcotest.(check string)
            "error" "Failure(\"a removed record replayed\")"
            (Expect.field "error" m));
      Alcotest.test_case "a stale first sector of a large new page is reached"
        `Quick (fun () ->
          let m = Expect.report stale_first in
          Alcotest.(check string)
            "error" "Failure(\"stale first sector\")" (Expect.field "error" m));
    ] )
