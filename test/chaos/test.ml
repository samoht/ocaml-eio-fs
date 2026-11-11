let () =
  Alcotest.run "eio-fs.chaos"
    [
      Test_after.suite;
      Test_review1.suite;
      Test_review2.suite;
      Test_guards.suite;
      Test_states.suite;
    ]
