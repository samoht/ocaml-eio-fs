(** Seeded file programs whose reads and writes meet
    {!Eio_fs_chaos.Fault.Cancel}, on the host's filesystem and on eio-fs.mem. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs the seeds that [CANCEL_SEEDS] counts (defaults to 16), or the
    one [CANCEL_SEED] names, on each filesystem, and fails with the seeds whose
    files lost, duplicated or garbled a byte, that breached the cancel's
    contract, raised or did not end within [CANCEL_TIMEOUT] seconds (defaults to
    60). A filesystem that cannot read or write at an offset, as eio_windows
    1.7, is read and written in sequence only. *)
