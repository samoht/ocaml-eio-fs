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

(* A temporary file written with no sync, then abandoned and removed, the
   removal committed: writeback may have put its records on disk before the
   removal, and an unsynced append to [new] that lands in its freed block
   may show them. *)
let freed_unsynced () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env ->
      setup env;
      Eio_fs_sync.save P.(env#fs / "new") (String.make 4096 'n'))
    (fun ~ack env ->
      try
        P.save ~create:(`Exclusive 0o644)
          P.(env#fs / "tmp")
          (sectors [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' ]);
        P.unlink P.(env#fs / "tmp");
        Eio_fs_sync.sync_dir env#fs;
        ack ();
        P.with_open_out ~append:true ~create:`Never P.(env#fs / "new")
        @@ fun f -> Eio.Flow.copy_string (String.make 512 'x') f
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      keep env;
      match P.load P.(env#fs / "new") with
      | s when String.length s > 4096 && s.[4096] = 'I' ->
          failwith "an aborted record replayed"
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

(* An unsynced overwrite of eight sectors the disk holds leaves 256 states,
   each sector old or new, which the tier has room to try one by one: one
   mix of them breaks recover. *)
let mix () =
  let old = String.make 4096 'o' and planted = "onoonoon" in
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "f") old)
    (fun ~ack:_ env ->
      try
        P.with_open_out ~create:`Never P.(env#fs / "f") @@ fun f ->
        Eio.Flow.copy_string (String.make 4096 'n') f
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      let s = P.load P.(env#fs / "f") in
      if String.init 8 (fun i -> s.[i * 512]) = planted then
        failwith "a planted mix")

(* An unsynced overwrite of 64 sectors allows 2^64 states, drawn at random:
   the draws give fair weight to those with a shape, an in-order writeback
   that landed a prefix, and a single sector left behind. *)
let overwrite ?replay ~recover () =
  let old = String.make 32768 'o' in
  Eio_fs.Chaos.Crash.check ?replay
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "f") old)
    (fun ~ack:_ env ->
      try
        P.with_open_out ~create:`Never P.(env#fs / "f") @@ fun f ->
        Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero
          [ Cstruct.of_string (String.make 32768 'n') ]
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      let s = P.load P.(env#fs / "f") in
      recover (List.init 64 (fun i -> s.[i * 512] = 'n')))

(* The first k sectors new, the rest old, at a k no torn write leaves: the
   tears fall after 1, 16, 32 and 48 sectors. *)
let prefix ?replay () =
  overwrite ?replay
    ~recover:(fun news ->
      let k = List.length (List.filter Fun.id news) in
      if
        k > 1
        && k mod 16 <> 0
        && List.for_all Fun.id (List.mapi (fun i n -> n = (i < k)) news)
      then failwith "a prefix landed")
    ()

let one_old () =
  overwrite
    ~recover:(fun news ->
      if List.length (List.filter not news) = 1 then failwith "one sector left")
    ()

let suite =
  ( "states",
    [
      Alcotest.test_case "a prefix of an overwrite is drawn" `Quick (fun () ->
          let m = Expect.report prefix in
          Alcotest.(check string)
            "error" "Failure(\"a prefix landed\")" (Expect.field "error" m));
      Alcotest.test_case "a replay of a drawn state runs it" `Quick (fun () ->
          let m = Expect.report prefix in
          let replay =
            Scanf.sscanf (Expect.field "replay" m) "~replay:%S" Fun.id
          in
          let again = Expect.report (prefix ~replay) in
          Alcotest.(check string)
            "cut" (Expect.field "cut" m) (Expect.field "cut" again));
      Alcotest.test_case "a single sector left of an overwrite is drawn" `Quick
        (fun () ->
          let m = Expect.report one_old in
          Alcotest.(check string)
            "error" "Failure(\"one sector left\")" (Expect.field "error" m));
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
      Alcotest.test_case
        "a fresh sector may hold what an unsynced removed file had" `Quick
        (fun () ->
          let m = Expect.report freed_unsynced in
          Alcotest.(check string)
            "error" "Failure(\"an aborted record replayed\")"
            (Expect.field "error" m));
      Alcotest.test_case "a stale first sector of a large new page is reached"
        `Quick (fun () ->
          let m = Expect.report stale_first in
          Alcotest.(check string)
            "error" "Failure(\"stale first sector\")" (Expect.field "error" m));
      Alcotest.test_case "every mix of an overwrite's sectors is tried" `Quick
        (fun () ->
          let m = Expect.report mix in
          Alcotest.(check string)
            "error" "Failure(\"a planted mix\")" (Expect.field "error" m));
      Alcotest.test_case "a report says where a stale sector came from" `Quick
        (fun () ->
          let field f m = Expect.field m (Expect.report f) in
          Alcotest.(check string)
            "truncated"
            "/wal, 512 bytes: @0+512 stale (what /wal held there before its \
             truncation)"
            (field old_frame "left");
          Alcotest.(check string)
            "removed"
            "/new, 4608 bytes: @4096+512 stale (what the removed /old held \
             there)"
            (field freed_by_unlink "left");
          Alcotest.(check string)
            "acked" "1 value, after #2"
            (field freed_by_unlink "acked"));
      Alcotest.test_case "a report's trace holds the truncation" `Quick
        (fun () ->
          let m = Expect.report old_frame in
          let line = "\n    #2 truncate /wal to 0\n" in
          let n = String.length m and k = String.length line in
          let rec has i =
            i + k <= n && (String.sub m i k = line || has (i + 1))
          in
          if not (has 0) then Alcotest.failf "no truncation in %s" m);
    ] )
