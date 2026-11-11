open After

let save d v = App.save_config d v
let append f r = App.Log.append f r
let recover d = App.Log.recover d
let get d k ~compute = App.Cache.get d k ~compute

let suite =
  ( "after",
    [
      Alcotest.test_case "save: correct" `Quick (Expect.passes (test_a ~save));
      Alcotest.test_case "save: a recover that ignores acked checks the rest"
        `Quick
        (Expect.passes (test_a_weak ~save));
      Alcotest.test_case "save: no file sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_a ~save:(App.save_config ~bug:`No_file_sync)));
      Alcotest.test_case "save: no directory sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_a ~save:(App.save_config ~bug:`No_dir_sync)));
      Alcotest.test_case "log: correct" `Quick
        (Expect.passes (test_b ~append ~recover));
      Alcotest.test_case "log: no sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_b ~append:(App.Log.append ~bug:`No_sync) ~recover));
      Alcotest.test_case "log: a retried sync, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_b ~append:(App.Log.append ~bug:`Retry_sync) ~recover));
      Alcotest.test_case "log: trusting the marker, by the sampled tier" `Quick
        (Expect.caught ~tier:"sampled"
           (test_b ~append ~recover:(App.Log.recover ~bug:`Marker_only)));
      Alcotest.test_case "log: no checksum, by the sampled tier" `Quick
        (Expect.caught ~tier:"sampled"
           (test_b ~append ~recover:(App.Log.recover ~bug:`No_checksum)));
      Alcotest.test_case "cache: correct" `Quick (Expect.passes (test_c ~get));
      Alcotest.test_case "cache: EACCES only, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_c ~get:(App.Cache.get ~bug:`Only_eacces)));
      Alcotest.test_case "cache: a direct write, by the linear tier" `Quick
        (Expect.caught ~tier:"linear"
           (test_c ~get:(App.Cache.get ~bug:`Direct_write)));
      Alcotest.test_case "kv: correct" `Quick
        (Expect.passes (fun () -> test_kv ()));
      Alcotest.test_case "kv: a snapshot sync error ignored, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (test_kv ~bug:`Ignore_snap_sync_error));
      Alcotest.test_case "kv: the log reset first, by the linear tier" `Quick
        (Expect.caught ~tier:"linear" (test_kv ~bug:`Wal_first));
      Alcotest.test_case "kv: a recovery with no sync, by the linear tier"
        `Quick
        (Expect.caught ~tier:"linear" (test_kv ~bug:`Recovery_no_sync));
      Alcotest.test_case "retry: correct" `Quick
        (Expect.passes (fun () -> test_retry ()));
      Alcotest.test_case "retry: a short count unchecked, by the sampled tier"
        `Quick
        (Expect.caught ~tier:"sampled" (test_retry ~bug:`Unchecked_short));
    ] )
