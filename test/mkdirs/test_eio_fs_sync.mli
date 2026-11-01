(** Crash and native specifications for durable directory paths and retries. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs_sync.mkdirs}. *)
