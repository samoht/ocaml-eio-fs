(** The root whose stat and opens run on the calling thread. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests that {!Eio_fs_unix.inline_fs} answers each call as the
    process's root does, its errors included, and makes no system-thread hop. *)
