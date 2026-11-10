(** The design's tests and their mutants. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs the design's atomic save, log, cache, key-value store and retry
    tests on correct programs, and on each mutant with the tier that finds it.
*)
