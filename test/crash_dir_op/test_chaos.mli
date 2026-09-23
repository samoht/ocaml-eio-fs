(** Spec for the crash trigger that stops the clock on a directory operation. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs.Chaos.Crash.After_dir_op}. *)
