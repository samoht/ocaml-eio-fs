let () = Alcotest.run "eio-fs.sync" [ Test_eio_fs_sync.suite; Test_lock.suite ]
