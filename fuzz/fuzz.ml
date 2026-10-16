let () = Alcobar.run "eio-fs" [ Fuzz_chaos.suite; Fuzz_append.suite ]
