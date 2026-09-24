(** Tests for the POSIX durability barrier. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs_unix}. *)

val lease_suite : string * unit Alcotest.test_case list
(** The inherited-descriptor lifetime regression case. *)
