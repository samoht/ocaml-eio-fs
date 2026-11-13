(* Operations take time, so other fibers run during a slow sync. *)

module P = Eio.Path

(* A checkpointer syncs a log while a writer appends to it, then records
   the log's size as durable. The size read after the sync counts an
   append made during it, which the sync does not cover. *)
let checkpoint ~size_after () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env ->
      Eio_fs_sync.save P.(env#fs / "log") "";
      Eio_fs_sync.save P.(env#fs / "ckpt") "0")
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let log =
          P.open_out ~sw ~append:true ~create:`Never P.(env#fs / "log")
        in
        Eio.Fiber.both
          (fun () ->
            Eio.Time.sleep env#clock 0.0005;
            Eio.Flow.copy_string "x" log)
          (fun () ->
            let size () = Optint.Int63.to_int (Eio.File.stat log).size in
            let before = size () in
            Eio.File.sync log;
            let n = if size_after then size () else before in
            Eio_fs_sync.save P.(env#fs / "ckpt") (string_of_int n);
            ack n)
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      let n = int_of_string (P.load P.(env#fs / "ckpt")) in
      if String.length (P.load P.(env#fs / "log")) < n then
        failwith "a checkpointed byte is lost")

(* A compaction merges the entries it saw, and saves the result. An entry
   added while the save syncs is lost if the compaction then keeps only
   what it saw. *)
let compaction ~merge () =
  Eio_fs.Chaos.check (fun env ->
      let live = ref [ "a" ] in
      Eio.Fiber.both
        (fun () ->
          Eio.Time.sleep env#clock 0.0005;
          live := !live @ [ "b" ])
        (fun () ->
          let seen = !live in
          (try Eio_fs_sync.save P.(env#fs / "merged") (String.concat "" seen)
           with Eio.Io _ -> ());
          live :=
            if merge then
              seen @ List.filter (fun x -> not (List.mem x seen)) !live
            else seen);
      if not (List.mem "b" !live) then failwith "an entry added is lost")

(* [change] runs in another fiber while [sync] runs, then [after], then the
   change is acknowledged: a sync covers only what came before its call, so
   a recover that holds the change durable fails. With [before], the change
   comes first, and the sync covers it. *)
let overlap ?os ?(before = false) ?(after = ignore) ~sync ~change ~held () =
  Eio_fs.Chaos.Crash.check ?os
    ~setup:(fun env ->
      Eio_fs_sync.save ~perm:0o600 P.(env#fs / "f") "data";
      Eio_fs_sync.save P.(env#fs / "g") "")
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        if before then begin
          change env;
          sync ~sw env
        end
        else
          Eio.Fiber.both
            (fun () -> sync ~sw env)
            (fun () ->
              Eio.Time.sleep env#clock 0.0005;
              change env);
        after env;
        ack ()
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if acked <> [] && not (held env) then failwith "a change is lost")

let fsync ~sw env =
  Eio.File.sync (P.open_out ~sw ~create:`Never P.(env#fs / "f"))

(* fsync(2) through a handle open for reading only. *)
let fsync_ro ~sw env =
  Eio_fs_sync.Barrier.(sync_file portable) (P.open_in ~sw P.(env#fs / "f"))

let dirsync ~sw:_ env = Eio_fs_sync.sync_dir env#fs

(* F_FULLFSYNC of another file: on macOS it drains every fsync(2) before. *)
let drain env =
  P.with_open_out ~create:`Never P.(env#fs / "g") Eio_fs_unix.sync_file

let chmod env = P.chmod ~follow:false ~perm:0o400 P.(env#fs / "f")
let chmodded env = (P.stat ~follow:false P.(env#fs / "f")).perm = 0o400

let rewrite env =
  P.with_open_out ~create:`Never
    P.(env#fs / "f")
    (fun f -> Eio.Flow.copy_string "DATA" f)

let rewritten env = P.load P.(env#fs / "f") = "DATA"
let make env = P.save ~create:(`Exclusive 0o644) P.(env#fs / "h") ""

let made env =
  match P.kind ~follow:false P.(env#fs / "h") with
  | `Regular_file -> true
  | _ -> false

(* An append made while the log syncs moves its times, which a cut that
   loses the append does not keep. *)
let times () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "log") "a")
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let log =
          P.open_out ~sw ~append:true ~create:`Never P.(env#fs / "log")
        in
        let mtime = (Eio.File.stat log).mtime in
        Eio.Fiber.both
          (fun () -> Eio.File.sync log)
          (fun () ->
            Eio.Time.sleep env#clock 0.0005;
            Eio.Flow.copy_string "b" log);
        ack mtime
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let log = P.(env#fs / "log") in
      match acked with
      | [ mtime ] when P.load log = "a" ->
          if (P.stat ~follow:false log).mtime <> mtime then
            failwith "the times of a lost append"
      | _ -> ())

let suite =
  ( "time",
    [
      Alcotest.test_case "a chmod during a sync is not covered" `Quick
        (Expect.caught ~tier:"linear"
           (overlap ~sync:fsync ~change:chmod ~held:chmodded));
      Alcotest.test_case "a chmod before a sync is covered" `Quick
        (Expect.passes
           (overlap ~before:true ~sync:fsync ~change:chmod ~held:chmodded));
      Alcotest.test_case "a chmod during fsync is not drained on macOS" `Quick
        (Expect.caught ~tier:"linear"
           (overlap ~os:`Macos ~after:drain ~sync:fsync_ro ~change:chmod
              ~held:chmodded));
      Alcotest.test_case "a chmod before fsync is drained on macOS" `Quick
        (Expect.passes
           (overlap ~os:`Macos ~before:true ~after:drain ~sync:fsync_ro
              ~change:chmod ~held:chmodded));
      Alcotest.test_case "a chmod fsync alone covers is lost on macOS" `Quick
        (Expect.caught ~tier:"linear"
           (overlap ~os:`Macos ~before:true ~sync:fsync_ro ~change:chmod
              ~held:chmodded));
      Alcotest.test_case "a write during fsync is not drained on macOS" `Quick
        (Expect.caught ~tier:"linear"
           (overlap ~os:`Macos ~after:drain ~sync:fsync ~change:rewrite
              ~held:rewritten));
      Alcotest.test_case "a write before fsync is drained on macOS" `Quick
        (Expect.passes
           (overlap ~os:`Macos ~before:true ~after:drain ~sync:fsync
              ~change:rewrite ~held:rewritten));
      Alcotest.test_case "an entry made during a directory sync is not covered"
        `Quick
        (Expect.caught ~tier:"linear"
           (overlap ~sync:dirsync ~change:make ~held:made));
      Alcotest.test_case "an append during a sync does not move its times"
        `Quick (Expect.passes times);
      Alcotest.test_case "an append during a sync is not covered" `Quick
        (Expect.caught ~tier:"linear" (checkpoint ~size_after:true));
      Alcotest.test_case "a size read before a sync is covered" `Quick
        (Expect.passes (checkpoint ~size_after:false));
      Alcotest.test_case "a fiber runs while another one syncs" `Quick
        (Expect.refused
           "Eio_fs.Chaos.check: the run with no fault failed: Failure(\"an \
            entry added is lost\")"
           (compaction ~merge:false));
      Alcotest.test_case "a merge keeps what was added during a sync" `Quick
        (Expect.passes (compaction ~merge:true));
    ] )
