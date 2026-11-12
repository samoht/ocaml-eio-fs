(** A silent event loop with a virtual clock.

    A trimmed copy of Eio_mock's scheduler and clock (eio 1.6, ISC), whose clock
    prints nothing and stops at a time limit. *)

exception Deadlock_detected
(** Raised by {!run} when every fiber waits and no timer is due. *)

exception Time_limit
(** Raised by {!run} when its clock would move past the limit. *)

val run :
  ?idle:(unit -> bool) ->
  limit:float ->
  (float Eio.Time.clock_ty Eio.Resource.t ->
  Eio.Time.Mono.ty Eio.Resource.t ->
  'a) ->
  'a
(** [run ~idle ~limit main] calls [main clock mono_clock] in a fresh event loop
    whose clocks start at [0.] and move up to [limit] seconds: to the next timer
    when every fiber waits, and, as real time passes while a program polls,
    after 1000 turns at one instant in which every runnable fiber had just
    yielded, by 10 ms, or to the next timer if that comes first. Once a fiber
    has yielded for 5 seconds with no wait in between, the step is a tenth of
    how long it has. When every fiber waits, [idle ()] runs first (defaults to
    doing nothing); if it is [true], it woke a fiber, and the clock stays. It
    returns what [main] returns, and raises what [main] raises.

    @raise Deadlock_detected if every fiber waits and no timer is due.
    @raise Time_limit if the clock would move past [limit]. *)
