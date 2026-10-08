(** Tests for the portable durability barrier. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs_sync}. *)

val link_suite : string * unit Alcotest.test_case list
(** [link_suite] checks exact raw publication, refusal and durable crash state.
*)
