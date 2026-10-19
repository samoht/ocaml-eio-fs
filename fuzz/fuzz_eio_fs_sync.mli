(** Fuzz tests for {!Eio_fs_sync.append_lines}: whatever a file held, torn or
    not, the lines appended to it are its last lines, and every byte it held
    before is kept. *)

val suite : string * Alcobar.test_case list
(** [suite] is the fuzz test suite. *)
