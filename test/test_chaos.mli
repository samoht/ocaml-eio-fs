(** Fault-injection tests for {!Eio_fs.Chaos}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests the adversarial filesystem wrapper. *)
