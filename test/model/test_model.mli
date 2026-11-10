(** Model-based tests of the in-memory backend and of
    {!Eio_fs.Chaos.Private.Crash}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs seeded workloads against a reference model, with and without a
    crash. *)
