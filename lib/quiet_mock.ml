open! Hermetic

(* A silent copy of Eio_mock's scheduler and clock.

   Copied from eio 1.6, lib_eio/mock/backend.ml and lib_eio/mock/clock.ml,
   trimmed to what Eio_fs.Chaos needs, with two changes:
   - moving the clock prints nothing. Eio_mock's clock calls [traceln] from
     the scheduler, where no fiber binding reaches, so a check built on it
     cannot keep stderr clean. A quiet mock clock belongs upstream; this copy
     stands in until then;
   - the scheduler stops instead of moving the clock past [limit], so a run
     that every fiber waits in reports as a hang, and one that keeps a timer
     going reports as a run past its time limit.

   Hang detection and deterministic virtual time need a loop the check owns.
   eio-net carries the same copy. This one needs no eio.mock.

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

type t = {
  run_q : (unit -> exit) Lf_queue.t;
  clock : Mono.Impl.t;
  limit : Mtime.t;
  mutable past_limit : bool;
  idle : unit -> bool;
}

let rec schedule t : exit =
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
      idle;
    }
  in
  let rec fork ~new_fiber:fiber fn =
    Trace.fiber (Fiber_context.tid fiber);
    Effect.Deep.match_with fn ()
      {
        retc =
          (fun () ->
            Fiber_context.destroy fiber;
            schedule t);
        exnc =
          (fun ex ->
            let bt = Printexc.get_raw_backtrace () in
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
                    f fiber (fun result ->
                        Lf_queue.push t.run_q (fun () ->
                            Fiber_context.clear_cancel_fn fiber;
                            match result with
                            | Ok v -> Suspended.continue k v
                            | Error ex -> Suspended.discontinue k ex));
                    schedule t)
            | Eio.Private.Effects.Fork (new_fiber, f) ->
                Some
                  (fun k ->
                    let k = { Suspended.k; fiber } in
                    Lf_queue.push_head t.run_q (Suspended.continue k);
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
