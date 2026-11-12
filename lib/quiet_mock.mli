(** A silent event loop with a virtual clock.

    A trimmed copy of Eio_mock's scheduler and clock (eio 1.6, ISC), whose clock
    prints nothing and stops at a time limit. *)

exception Deadlock_detected
(** Raised by {!run} when every fiber waits and no timer is due. *)

exception Time_limit
(** Raised by {!run} when its clock would move past the limit. *)

exception Busy
(** Raised by {!run} when it resumes fibers 10 million times in a row with no
    fiber waiting 1 ms or more for a timer while every fiber waited. *)

exception Too_long
(** Raised by {!run} when it resumes fibers 10 million times, some fiber having
    waited 1 ms or more for a timer in between. *)

exception Stopped
(** Raised by {!run} when its [stop] holds. *)

val run :
  ?idle:(unit -> bool) ->
  ?stop:(unit -> bool) ->
  limit:float ->
  (float Eio.Time.clock_ty Eio.Resource.t ->
  Eio.Time.Mono.ty Eio.Resource.t ->
  'a) ->
  'a
(** [run ~idle ~stop ~limit main] calls [main clock mono_clock] in a fresh event
    loop whose clocks start at [0.] and move up to [limit] seconds: by 1 us each
    time a fiber resumes, from a yield, another fiber or a timer, and to the
    next timer when every fiber waits. When every fiber waits, [idle ()] runs
    first (defaults to doing nothing); if it is [true], it woke a fiber, and the
    clock stays. Before each turn, [stop ()] runs (defaults to [false]); if it
    is [true], the loop ends. It returns what [main] returns, and raises what
    [main] raises.

    @raise Stopped if [stop ()] holds.
    @raise Deadlock_detected if every fiber waits and no timer is due.
    @raise Time_limit if the clock would move past [limit].
    @raise Busy
      if it resumes fibers 10 million times in a row with no fiber waiting 1 ms
      or more for a timer while every fiber waited.
    @raise Too_long if it resumes fibers 10 million times otherwise. *)
