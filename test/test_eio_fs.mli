(** Public filesystem behavior tests. *)

val suite : string * unit Alcotest.test_case list
(** Tests for the Eio filesystem provider. *)

module Crunch : sig
  val suite : string * unit Alcotest.test_case list
  (** Tests for {!Eio_fs.Crunch}. *)
end
