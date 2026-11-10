(** One file's data under failed syncs, against a block-level reference. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks every workload of two small scopes, and that the states the
    reference allows are drawn. *)
