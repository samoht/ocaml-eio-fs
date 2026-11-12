open! Hermetic

(* A silent copy of Eio_mock's scheduler and clock.

   Copied from eio 1.6, lib_eio/mock/backend.ml and lib_eio/mock/clock.ml,
   trimmed to what Eio_fs.Chaos needs, with three changes:
   - moving the clock prints nothing. Eio_mock's clock calls [traceln] from
     the scheduler, where no fiber binding reaches, so a check built on it
     cannot keep stderr clean. A quiet mock clock belongs upstream; this copy
     stands in until then;
   - the scheduler stops instead of moving the clock past [limit], so a run
     that every fiber waits in reports as a hang, and one that keeps a timer
     going reports as a run past its time limit;
   - real time passes while a program polls: after [polls] turns at one
     instant in which every runnable fiber had just yielded, the clock moves
     on by 10 ms, or to the next timer if that comes first, as eio_posix
     fires a timer that is due between two rounds of its run queue. Once a
     fiber has yielded for 5 seconds, with no wait in between, the step is a
     tenth of how long it has, so that a program that polls for ever reaches
     the time limit.

   Hang detection and deterministic virtual time need a loop the check owns.
   eio-net carries the same copy, with a [wait_io] that an in-memory
   filesystem, which never yields, does not need. This one needs no
   eio.mock.

   Copyright (C) 2021 Anil Madhavapeddy
   Copyright (C) 2022 Thomas Leonard

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
   IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module Fiber_context = Eio.Private.Fiber_context
module Trace = Eio.Private.Trace
module Lf_queue = Eio_utils.Lf_queue
module Suspended = Eio_utils.Suspended

exception Deadlock_detected
exception Time_limit

(* {1 Clock} *)

module Mono = struct
  module Key = struct
    type t = < >

    let compare = compare
  end

  module Job = struct
    type t = { time : Mtime.t; resolver : unit Eio.Promise.u }

    let compare a b = Mtime.compare a.time b.time
  end

  module Q = Psq.Make (Key) (Job)

  module Impl = struct
    type time = Mtime.t
    type t = { mutable now : Mtime.t; mutable q : Q.t }

    let now t = t.now

    let sleep_until t time =
      if Mtime.compare time t.now <= 0 then Eio.Fiber.yield ()
      else
        let p, r = Eio.Promise.create () in
        let k = object end in
        t.q <- Q.add k { time; resolver = r } t.q;
        try Eio.Promise.await p
        with Eio.Cancel.Cancelled _ as ex ->
          t.q <- Q.remove k t.q;
          raise ex

    let set_time t time =
      let rec drain () =
        match Q.min t.q with
        | Some (_, v) when Mtime.compare v.time time <= 0 ->
            Eio.Promise.resolve v.resolver ();
            t.q <- Option.get (Q.rest t.q);
            drain ()
        | _ -> ()
      in
      drain ();
      t.now <- time

    (* Moves on by [step], or to the next timer if that comes first, never
       past [limit]. *)
    let poll t ~step ~limit =
      let target =
        Option.value (Mtime.add_span t.now step) ~default:Mtime.max_stamp
      in
      match Q.min t.q with
      | Some (_, v) when Mtime.compare v.time target <= 0 ->
          if Mtime.compare v.time limit > 0 then `Past_limit
          else (
            set_time t v.time;
            `Timer)
      | _ ->
          if Mtime.compare target limit > 0 then `Past_limit
          else (
            set_time t target;
            `Moved)

    (* Eio_mock's [try_advance], which does not move past [limit]. *)
    let try_advance t ~limit =
      match Q.min t.q with
      | None -> `Idle
      | Some (_, v) when Mtime.compare v.time limit > 0 -> `Past_limit
      | Some (_, v) ->
          set_time t v.time;
          `Advanced
  end

  let handler = Eio.Time.Pi.clock (module Impl)
end

module Wall = struct
  type t = Mono.Impl.t
  type time = float

  let wall_of_mtime m = Int64.to_float (Mtime.to_uint64_ns m) /. 1e9
  let wall_to_mtime w = Mtime.of_uint64_ns (Int64.of_float (w *. 1e9))
  let now t = wall_of_mtime (Mono.Impl.now t)
  let sleep_until t time = Mono.Impl.sleep_until t (wall_to_mtime time)
end

let wall_handler = Eio.Time.Pi.clock (module Wall)

(* {1 Scheduler} *)

type exit = [ `Exit_scheduler ]

(* [queued] counts what [run_q] holds, and [polling] the fibers among them
   that yielded. [polls] counts the turns at [polled_at] that found nothing
   else runnable. [pollers] holds, for each fiber whose last suspension was a
   yield, when it began to yield with no wait in between. *)
type t = {
  run_q : (unit -> exit) Lf_queue.t;
  clock : Mono.Impl.t;
  limit : Mtime.t;
  mutable past_limit : bool;
  queued : int Atomic.t;
  polling : int Atomic.t;
  mutable polls : int;
  mutable polled_at : Mtime.t;
  pollers : (Trace.id, Mtime.t) Hashtbl.t;
  idle : unit -> bool;
}

let polls = 1_000

(* A yield costs tens of nanoseconds on eio_posix, and a program that yields
   works between two: 10 us a turn. Faster, a client that polls for a
   datagram while its peer stalls for 50 ms would send more of them than a
   spin. *)
let step = Mtime.Span.(10 * ms)

(* How long a fiber yields at [step] before the step grows. *)
let patience = Mtime.Span.(5 * s)

(* How long the fiber that has yielded the longest has. *)
let longest_poll t now =
  Hashtbl.fold
    (fun _ start acc ->
      let d = Mtime.span start now in
      if Mtime.Span.compare d acc > 0 then d else acc)
    t.pollers Mtime.Span.zero

(* A fiber that waits or ends no longer polls. *)
let stopped t fiber = Hashtbl.remove t.pollers (Fiber_context.tid fiber)

let yielded t fiber =
  let id = Fiber_context.tid fiber in
  if not (Hashtbl.mem t.pollers id) then
    Hashtbl.add t.pollers id (Mono.Impl.now t.clock)

let push t ~yielded f =
  Atomic.incr t.queued;
  if yielded then Atomic.incr t.polling;
  Lf_queue.push t.run_q (fun () ->
      Atomic.decr t.queued;
      if yielded then Atomic.decr t.polling;
      f ())

let push_head t f =
  Atomic.incr t.queued;
  Lf_queue.push_head t.run_q (fun () ->
      Atomic.decr t.queued;
      f ())

(* Whether the run polls: every runnable fiber yielded, [polls] times at the
   clock's instant. *)
let polled t =
  let n = Atomic.get t.polling in
  n > 0
  && n = Atomic.get t.queued
  &&
  let now = Mono.Impl.now t.clock in
  if not (Mtime.equal now t.polled_at) then begin
    t.polled_at <- now;
    t.polls <- 0
  end;
  t.polls <- t.polls + 1;
  t.polls >= polls

let rec schedule t : exit =
  if polled t then begin
    let polled = longest_poll t (Mono.Impl.now t.clock) in
    let step =
      if Mtime.Span.compare polled patience < 0 then step
      else
        Mtime.Span.of_uint64_ns (Int64.div (Mtime.Span.to_uint64_ns polled) 10L)
    in
    match Mono.Impl.poll t.clock ~step ~limit:t.limit with
    | `Timer | `Moved -> next t
    | `Past_limit ->
        t.past_limit <- true;
        `Exit_scheduler
  end
  else next t

and next t =
  match Lf_queue.pop t.run_q with
  | Some f -> f ()
  | None when t.idle () -> schedule t
  | None -> (
      match Mono.Impl.try_advance t.clock ~limit:t.limit with
      | `Advanced -> schedule t
      | `Idle -> `Exit_scheduler
      | `Past_limit ->
          t.past_limit <- true;
          `Exit_scheduler)

let run ?(idle = fun () -> false) ~limit main =
  let clock = { Mono.Impl.now = Mtime.of_uint64_ns 0L; q = Mono.Q.empty } in
  let wall : float Eio.Time.clock_ty Eio.Resource.t =
    Eio.Resource.T (clock, wall_handler)
  in
  let mono : Eio.Time.Mono.ty Eio.Resource.t =
    Eio.Resource.T (clock, Mono.handler)
  in
  let t =
    {
      run_q = Lf_queue.create ();
      clock;
      limit = Wall.wall_to_mtime limit;
      past_limit = false;
      queued = Atomic.make 0;
      polling = Atomic.make 0;
      polls = 0;
      polled_at = Mtime.of_uint64_ns 0L;
      pollers = Hashtbl.create 7;
      idle;
    }
  in
  let rec fork ~new_fiber:fiber fn =
    Trace.fiber (Fiber_context.tid fiber);
    Effect.Deep.match_with fn ()
      {
        retc =
          (fun () ->
            stopped t fiber;
            Fiber_context.destroy fiber;
            schedule t);
        exnc =
          (fun ex ->
            let bt = Printexc.get_raw_backtrace () in
            stopped t fiber;
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex bt);
        effc =
          (fun (type a)
            (e : a Effect.t)
            :
            ((a, exit) Effect.Deep.continuation -> exit) option
          ->
            match e with
            | Eio.Private.Effects.Suspend f ->
                Some
                  (fun k ->
                    let k = { Suspended.k; fiber } in
                    (* A fiber resumed before [f] returns yielded. *)
                    let entering = ref true in
                    let resumed = ref false in
                    f fiber (fun result ->
                        if !entering then resumed := true;
                        push t ~yielded:!entering (fun () ->
                            Fiber_context.clear_cancel_fn fiber;
                            match result with
                            | Ok v -> Suspended.continue k v
                            | Error ex -> Suspended.discontinue k ex));
                    entering := false;
                    if !resumed then yielded t fiber else stopped t fiber;
                    schedule t)
            | Eio.Private.Effects.Fork (new_fiber, f) ->
                Some
                  (fun k ->
                    let k = { Suspended.k; fiber } in
                    push_head t (fun () -> Suspended.continue k ());
                    fork ~new_fiber f)
            | Eio.Private.Effects.Get_context ->
                Some (fun k -> Effect.Deep.continue k fiber)
            | _ -> None);
      }
  in
  let new_fiber = Fiber_context.make_root () in
  let result = ref None in
  let (`Exit_scheduler : exit) =
    Domain_local_await.using ~prepare_for_await:Eio_utils.Dla.prepare_for_await
      ~while_running:(fun () ->
        fork ~new_fiber (fun () -> result := Some (main wall mono)))
  in
  match !result with
  | Some x -> x
  | None -> if t.past_limit then raise Time_limit else raise Deadlock_detected
