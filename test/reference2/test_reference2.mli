(** A differential of {!Eio_fs.Chaos.Private.Crash} against a second reference
    crash model, under every policy, with directed cases. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs random workloads and the directed cases against the reference.
*)
