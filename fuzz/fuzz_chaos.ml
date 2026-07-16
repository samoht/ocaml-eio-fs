(** Fuzz tests for {!Eio_fs.Chaos}.

    Properties tested:

    + {b Crash-safety.} Over an arbitrary seed, fault probability and workload,
      the wrapper only ever surfaces [Eio.Io] or [End_of_file] -- never an
      assertion failure, [Invalid_argument], or any other escape. Any such
      escape propagates out of the property and fails the run.
    + {b Determinism.} A seeded schedule replayed with the same seed drives the
      exact same success/failure outcome and the exact same committed bytes. *)

open Alcobar
module Chaos = Eio_fs.Chaos

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* Run a fixed shape of workload -- a run of block writes, a sync, then a
   read-back -- over a chaos-wrapped in-memory filesystem, and report the
   outcome together with the bytes that actually reached the store. Every run
   starts from a fresh in-memory tree ([Eio_fs.run] resets it). *)
let run_workload ~seed ~probability ~nblocks ~block =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let schedule = Chaos.Schedule.seeded ~seed ~probability () in
  let fs = Chaos.wrap schedule env#fs in
  let outcome =
    try
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "f")
        (fun file ->
          for i = 0 to nblocks - 1 do
            let buf = Cstruct.create block in
            Cstruct.memset buf (i land 0xff);
            Eio.File.pwrite_all file ~file_offset:(i63 (i * block)) [ buf ]
          done;
          Eio.File.sync file);
      (* read the file back through the perturbing wrapper too *)
      Eio.Path.with_open_in (fs / "f") (fun file ->
          ignore (Eio.Flow.read_all file));
      "ok"
    with
    | Eio.Io _ -> "io-error"
    | End_of_file -> "eof"
  in
  let committed =
    try Eio.Path.load (env#fs / "f") with Eio.Io _ -> "<none>"
  in
  (outcome, committed)

let outcome : (string * string) printer =
 fun ppf (o, c) -> Fmt.pf ppf "%s/%dB" o (String.length c)

(* [prob_pct] in 0..100, [nblocks] in 1..16, [block] in 1..256; [seed] is any
   int (negatives seed the PRNG just as well). *)
let test_crash_safety_and_determinism seed prob_pct nblocks block =
  let probability = float_of_int prob_pct /. 100. in
  let a = run_workload ~seed ~probability ~nblocks ~block in
  let b = run_workload ~seed ~probability ~nblocks ~block in
  check_eq ~pp:outcome ~eq:( = ) a b

(* {2 Crash / durability model} *)

module Crash = Chaos.Crash

(* Write [nblocks] contiguous [block]-byte blocks (block [i] filled with byte
   [i]), syncing once right after writing block [sync_after] (a barrier), then
   crash under [policy] and return the post-crash image. A fresh tree each run
   ([Eio_fs.run] resets it). *)
let post_crash_image ~policy ~nblocks ~block ~sync_after =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let ctl = Crash.v ~policy () in
  let fs = Crash.wrap ctl env#fs in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "f") (fun file ->
      for i = 0 to nblocks - 1 do
        let buf = Cstruct.create block in
        Cstruct.memset buf (i land 0xff);
        Eio.File.pwrite_all file ~file_offset:(i63 (i * block)) [ buf ];
        if i = sync_after then Eio.File.sync file
      done);
  Eio.Path.load (Crash.post_crash ctl / "f")

(* Number of blocks that were synced (and so must survive), given the barrier
   position. [sync_after] is [-1] when no sync fired. *)
let synced_count ~nblocks ~sync_after =
  if sync_after >= 0 && sync_after < nblocks then sync_after + 1 else 0

(* {b Barrier invariant.} Under [Drop_unsynced] the post-crash image is exactly
   the synced prefix: every synced block survives whole, every un-synced block
   is gone. Synced data can never be lost, and un-synced data never lingers. *)
let test_crash_barrier _seed nblocks block sync_after =
  let sync_after = sync_after - 1 in
  let img =
    post_crash_image ~policy:Drop_unsynced ~nblocks ~block ~sync_after
  in
  let k = synced_count ~nblocks ~sync_after in
  let expected =
    String.concat ""
      (List.init k (fun i -> String.make block (Char.chr (i land 0xff))))
  in
  check_eq ~pp:Fmt.string ~eq:String.equal img expected

(* {b Determinism.} The same seed, workload and barrier reproduce the exact same
   post-crash image under the seeded drop/reorder policy. *)
let test_crash_determinism seed nblocks block sync_after =
  let sync_after = sync_after - 1 in
  let policy = Crash.Reorder_unsynced seed in
  let a = post_crash_image ~policy ~nblocks ~block ~sync_after in
  let b = post_crash_image ~policy ~nblocks ~block ~sync_after in
  check_eq ~pp:Fmt.string ~eq:String.equal a b

let suite =
  ( "chaos",
    [
      test_case "crash-safety and determinism"
        [ int; range 101; range ~min:1 16; range ~min:1 256 ]
        test_crash_safety_and_determinism;
      test_case "crash barrier invariant"
        [ int; range ~min:1 16; range ~min:1 64; range 18 ]
        test_crash_barrier;
      test_case "crash post-image determinism"
        [ int; range ~min:1 16; range ~min:1 64; range 18 ]
        test_crash_determinism;
    ] )
