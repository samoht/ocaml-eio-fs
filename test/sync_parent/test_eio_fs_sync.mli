(** Spec for the barrier on the directory that holds a path's entry. *)

val suite : string * unit Alcotest.test_case list
(** Tests for {!Eio_fs_sync.sync_parent}. *)
