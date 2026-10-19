(** Spec for creating a directory path whose every new entry is durable. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs_sync.mkdirs}. *)
