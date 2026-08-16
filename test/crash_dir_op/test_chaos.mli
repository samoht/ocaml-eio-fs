(** Spec for the crash trigger that stops the clock on a directory operation. *)

val suite : string * unit Alcotest.test_case list
(** Tests for {!Eio_fs.Chaos.Crash.After_dir_op}. *)
