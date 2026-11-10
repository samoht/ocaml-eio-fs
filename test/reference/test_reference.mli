(** A differential of {!Eio_fs.Chaos.Crash} against an independent reference
    crash model. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that every crash state eio-fs produces is one the reference
    allows, and counts the allowed states it never produces by class. *)
