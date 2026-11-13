(** What a workload runs with. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that [env] mirrors [Eio_main]'s. *)
