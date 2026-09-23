(** Tests for the lexical path arithmetic. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs_path}: what a path's components are, and which
    spellings of one path are answered for. *)
