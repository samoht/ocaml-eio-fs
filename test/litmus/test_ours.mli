(** Our litmus tests for the operations Ferrite lacks. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks mkdir, unlink, symlink, truncate, a cross-directory rename
    and fsync after a failed fsync against hand-argued ext4 and envelope outcome
    sets. *)
