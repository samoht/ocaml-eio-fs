(** The in-memory filesystem against Linux and eio_posix. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests stat answers, times across a power cut and absolute links. *)
