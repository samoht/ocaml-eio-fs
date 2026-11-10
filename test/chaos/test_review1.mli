(** The first review's tests and their mutants. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs a store whose recovery compacts, a larger store and a retry
    path, on correct programs and on each mutant. *)
