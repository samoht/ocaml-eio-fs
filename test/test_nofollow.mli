(** No-follow read tests. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs.Nofollow}. *)
