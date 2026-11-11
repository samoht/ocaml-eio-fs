open Review1

let suite =
  ( "review1",
    [
      Alcotest.test_case "kv: correct" `Quick
        (Expect.passes (fun () -> After.test_kv ()));
      Alcotest.test_case "kv-reopen: correct, its ack checks nothing" `Quick
        (Expect.passes (fun () -> kv_reopen ()));
      Alcotest.test_case "kv-reopen2: correct" `Quick
        (Expect.passes (fun () -> kv_reopen2 ()));
      Alcotest.test_case
        "kv-reopen2: a recovery with no sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (kv_reopen2 ~bug:`Recovery_no_sync));
      Alcotest.test_case "kv-reopen3: correct" `Quick
        (Expect.passes (fun () -> kv_reopen3 ()));
      Alcotest.test_case
        "kv-reopen3: a recovery with no sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (kv_reopen3 ~bug:`Recovery_no_sync));
      Alcotest.test_case "big: correct" `Quick
        (Expect.passes (fun () -> big ()));
      Alcotest.test_case
        "big: a snapshot sync error ignored, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (big ~bug:`Ignore_snap_sync_error));
      Alcotest.test_case "retry: correct" `Quick
        (Expect.passes (fun () -> After.test_retry ()));
      Alcotest.test_case "retry: a short count unchecked, by the sampled tier"
        `Quick
        (Expect.caught ~tier:"sampled" (After.test_retry ~bug:`Unchecked_short));
      Alcotest.test_case "retry, pinned: correct" `Quick
        (Expect.passes (fun () -> retry_pinned ()));
      Alcotest.test_case "retry, pinned: a short count unchecked" `Quick
        (fun () ->
          match retry_pinned ~bug:`Unchecked_short () with
          | () -> Alcotest.fail "the pinned test passed"
          | exception Failure _ -> ());
    ] )
