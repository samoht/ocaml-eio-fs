(** Backend boundary and confinement tests. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests low-level filesystem errors and sandbox confinement. *)
