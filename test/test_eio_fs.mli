(** Public filesystem behavior tests. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests the Eio filesystem provider, including the crunch embedding.
*)
