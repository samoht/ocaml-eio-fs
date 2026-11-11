(* Three passes are by design. A recover that checks only that indexed
   blobs exist lets a blob with no sync through: the guards see that recover
   reads [acked], not what it checks. [Crash.check] fails no read, so a
   program that wipes its data on a read error passes it, and [check]
   catches it. [check] does not swap a stat's path for a link, so a test of a
   time-of-check to time-of-use window asks for the swap with [wrap]. *)
let suite =
  ( "review2",
    [
      Alcotest.test_case "lock: correct" `Quick
        (Expect.passes (fun () -> Lock.test ()));
      Alcotest.test_case "lock: an empty lock file is held, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (Lock.test ~bug:`Empty_is_held));
      Alcotest.test_case "lock: no stale check, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Lock.test ~bug:`No_stale_check));
      Alcotest.test_case "lock, errors: correct" `Quick
        (Expect.passes (fun () -> Lock.test_errors ()));
      Alcotest.test_case "lock, errors: any error is held, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear"
           (Lock.test_errors ~bug:`Any_error_is_held));
      Alcotest.test_case "blobs: correct" `Quick
        (Expect.passes (fun () -> Blobs.test ()));
      Alcotest.test_case "blobs: no blob sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Blobs.test ~put_bug:`No_blob_sync));
      Alcotest.test_case "blobs: no directory sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Blobs.test ~put_bug:`No_dir_sync));
      Alcotest.test_case "blobs: a partial line, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Blobs.test ~index_bug:`Partial_line));
      Alcotest.test_case "blobs: a weak recover misses no blob sync" `Quick
        (Expect.passes (Blobs.test_weak ~put_bug:`No_blob_sync));
      Alcotest.test_case "config: correct" `Quick
        (Expect.passes (fun () -> Conf.test ()));
      Alcotest.test_case "config: in place, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Conf.test ~bug:`In_place));
      Alcotest.test_case "config: no data sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Conf.test ~bug:`No_data_sync));
      Alcotest.test_case "config: delete first, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Conf.test ~bug:`Delete_first));
      Alcotest.test_case "config: no switch sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (Conf.test ~bug:`No_switch_sync));
      Alcotest.test_case "an idempotent step: correct" `Quick
        (Expect.passes (fun () -> Probe.guard_idempotent ()));
      Alcotest.test_case "an idempotent step in place, by the sampled tier"
        `Quick
        (Expect.caught ~tier:"sampled" (Probe.guard_idempotent ~bug:`In_place));
      Alcotest.test_case "a sector holds two writes together" `Quick
        (Expect.passes Probe.sector);
      Alcotest.test_case "toctou: the check does not swap links" `Quick
        (Expect.passes (fun () -> Probe.toctou ()));
      Alcotest.test_case "toctou: a swap asked for catches the bug" `Quick
        (fun () ->
          match Probe.toctou_swapped () with
          | () -> Alcotest.fail "the swap did not catch the bug"
          | exception Failure _ -> ());
      Alcotest.test_case "toctou: no link followed" `Quick
        (Expect.passes (fun () -> Probe.toctou_swapped ~bug:false ()));
      Alcotest.test_case "reinit: a power cut fails no read" `Quick
        (Expect.passes Probe.reinit_crash);
      Alcotest.test_case "reinit: by the linear tier of check" `Quick
        (Expect.caught ~tier:"linear" Probe.reinit_check);
      Alcotest.test_case "mkdir: no parent sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" Probe.mkdir_no_parent_sync);
      Alcotest.test_case "log: 50 synced appends" `Quick
        (Expect.passes (Probe.log_budget 50));
      Alcotest.test_case "log: 400 synced appends fit the default budget" `Quick
        (Expect.passes (Probe.log_budget 400));
      Alcotest.test_case "log: 300 records of varying lengths fit it" `Quick
        (Expect.passes (Probe.log_varying 300));
      Alcotest.test_case "records: a crash leaves a length the file had" `Quick
        (Expect.passes Probe.fixed_records);
    ] )
