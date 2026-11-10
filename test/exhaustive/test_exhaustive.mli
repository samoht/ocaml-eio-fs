(** An exhaustive small-scope check of {!Eio_fs.Chaos.Crash} against invariants
    that follow from its documentation. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks every workload of two small scopes under every policy. *)
