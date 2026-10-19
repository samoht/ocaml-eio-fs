let () = Alcobar.run "eio-fs" [ Fuzz_chaos.suite; Fuzz_eio_fs_sync.suite ]
