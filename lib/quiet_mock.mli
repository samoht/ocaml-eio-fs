(** A silent event loop with a virtual clock.

    A trimmed copy of Eio_mock's scheduler and clock (eio 1.6, ISC), whose clock
    prints nothing and stops at a time limit. *)

exception Deadlock_detected
(** Raised by {!run} when every fiber waits and no timer is due. *)

exception Time_limit
(** Raised by {!run} when every fiber waits and the next timer is due after the
    limit. *)

val run :
  limit:float ->
  (float Eio.Time.clock_ty Eio.Resource.t ->
  Eio.Time.Mono.ty Eio.Resource.t ->
  'a) ->
  'a
(** [run ~limit main] calls [main clock mono_clock] in a fresh event loop whose
    clocks start at [0.] and move only when every fiber waits, to the next
    timer, up to [limit] seconds. It returns what [main] returns, and raises
    what [main] raises.

    @raise Deadlock_detected if every fiber waits and no timer is due.
    @raise Time_limit
      if every fiber waits and the next timer is due after [limit]. *)
