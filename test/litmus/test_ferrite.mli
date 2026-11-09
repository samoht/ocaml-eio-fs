(** Ferrite's litmus tests against {!Eio_fs.Chaos.Crash}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks each Ferrite litmus test for soundness against the envelope
    model and coverage of the ext4 model. *)
