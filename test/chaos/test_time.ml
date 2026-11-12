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

let suite =
  ( "time",
    [
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
