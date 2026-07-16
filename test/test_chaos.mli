(** Fault-injection tests for {!Eio_fs.Chaos}. *)

val suite : string * unit Alcotest.test_case list
(** Tests for the adversarial filesystem wrapper. *)
