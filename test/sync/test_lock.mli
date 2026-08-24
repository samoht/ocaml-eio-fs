(** Tests for {!Eio_fs_sync.Lock}: the critical section a caller supplies. *)

val suite : string * unit Alcotest.test_case list
(** The lock test suite. *)
