(** Scaling tests: each workload runs at a size where a linear cost stays far
    below its bound and a quadratic one goes far above it. *)

val suite : string * unit Alcotest.test_case list
(** [suite] bounds the processor time of large workloads. *)
