(** The second review's tests and their mutants. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs a lock file, content-addressed blobs, a two-file config, a
    header in one sector, a time-of-check to time-of-use window and long logs,
    on correct programs and on each mutant. *)
