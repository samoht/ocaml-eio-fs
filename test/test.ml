let () =
  Alcotest.run "eio-fs"
    [
      Test_eio_fs.suite;
      Test_low_level.suite;
      Test_chaos.suite;
      Test_crunch.suite;
    ]
