(** Operations that take time. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that other fibers run during an operation. *)
