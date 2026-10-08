let () =
  Alcotest.run "eio-fs.sync"
    [
      Test_eio_fs_sync.suite;
      Test_eio_fs_sync.link_suite;
      Test_lock.suite;
      Test_rename.suite;
    ]
