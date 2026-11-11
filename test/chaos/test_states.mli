(** The states a cut may leave. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that the sampled tier reaches the states a cut allows. *)
