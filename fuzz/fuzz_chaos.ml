(** Fuzz tests for {!Eio_fs.Chaos}.

    Properties tested:

    + {b Crash-safety.} Over an arbitrary seed, fault probability and workload
      -- including read-only files and files staged through short-lived subtrees
      -- the wrapper only ever surfaces [Eio.Io] or [End_of_file], never an
      assertion failure, [Invalid_argument], or any other escape. Any such
      escape propagates out of the property and fails the run.
    + {b Determinism.} A seeded schedule replayed with the same seed drives the
      exact same success/failure outcome and the exact same committed bytes.
    + {b Barrier invariant.} Under [Drop_unsynced] the post-crash image is
      exactly the synced prefix, and the file keeps its mode -- whatever the
      file's mode, staging subtree, or truncating reopens after the barrier. *)

open Alcobar
module Chaos = Eio_fs.Chaos

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* Run a fixed shape of workload -- a run of block writes, a sync, then a
   read-back -- over a chaos-wrapped in-memory filesystem, and report the
   outcome together with the bytes that actually reached the store. The file
   is created read-only (0o444) or writable, directly at the root or through a
   short-lived staging subtree. Every run starts from a fresh in-memory tree
   ([Eio_fs.run] resets it). *)
let run_workload ~seed ~probability ~nblocks ~block ~read_only ~via_subtree =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let schedule = Chaos.Schedule.seeded ~seed ~probability () in
  let fs = Chaos.wrap schedule env#fs in
  let perm = if read_only then 0o444 else 0o644 in
  let rel = if via_subtree then "stage/f" else "f" in
  let write_blocks path =
    Eio.Path.with_open_out ~create:(`Exclusive perm) path (fun file ->
        for i = 0 to nblocks - 1 do
          let buf = Cstruct.create block in
          Cstruct.memset buf (i land 0xff);
          Eio.File.pwrite_all file ~file_offset:(i63 (i * block)) [ buf ]
        done;
        Eio.File.sync file)
  in
  let outcome =
    try
      if via_subtree then begin
        Eio.Path.mkdir ~perm:0o755 (fs / "stage");
        Eio.Path.with_open_dir (fs / "stage") (fun stage ->
            write_blocks Eio.Path.(stage / "f"))
      end
      else write_blocks (fs / "f");
      (* read the file back through the perturbing wrapper too *)
      Eio.Path.with_open_in (fs / rel) (fun file ->
          ignore (Eio.Flow.read_all file));
      "ok"
    with
    | Eio.Io _ -> "io-error"
    | End_of_file -> "eof"
  in
  let committed =
    try Eio.Path.load (env#fs / rel) with Eio.Io _ -> "<none>"
  in
  (outcome, committed)

let outcome : (string * string) printer =
 fun ppf (o, c) -> Fmt.pf ppf "%s/%dB" o (String.length c)

(* [prob_pct] in 0..100, [nblocks] in 1..16, [block] in 1..256; [seed] is any
   int (negatives seed the PRNG just as well). *)
let test_crash_safety_and_determinism seed prob_pct nblocks block read_only
    via_subtree =
  let probability = float_of_int prob_pct /. 100. in
  let a =
    run_workload ~seed ~probability ~nblocks ~block ~read_only ~via_subtree
  in
  let b =
    run_workload ~seed ~probability ~nblocks ~block ~read_only ~via_subtree
  in
  check_eq ~pp:outcome ~eq:( = ) a b

(* {2 Crash / durability model} *)

module Crash = Chaos.Crash

(* Write [nblocks] contiguous [block]-byte blocks (block [i] filled with byte
   [i]), syncing once right after writing block [sync_after] (a barrier), then
   crash under [policy] and return the post-crash image and file mode. The
   adversarial dimensions of a release store: the file is created read-only
   (0o444) or writable, staged directly or through a short-lived subtree that
   closes before the crash fires, and -- for writable files -- reopened
   [Or_truncate] after the writer closed, followed by one more unsynced
   overwrite. A fresh tree each run ([Eio_fs.run] resets it). *)
let post_crash_image ~policy ~nblocks ~block ~sync_after ~read_only ~via_subtree
    ~trunc_reopen =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let ctl = Crash.v ~policy () in
  let fs = Crash.wrap ctl env#fs in
  let perm = if read_only then 0o444 else 0o644 in
  let rel = if via_subtree then "stage/f" else "f" in
  let write_blocks path =
    Eio.Path.with_open_out ~create:(`Exclusive perm) path (fun file ->
        for i = 0 to nblocks - 1 do
          let buf = Cstruct.create block in
          Cstruct.memset buf (i land 0xff);
          Eio.File.pwrite_all file ~file_offset:(i63 (i * block)) [ buf ];
          if i = sync_after then Eio.File.sync file
        done)
  in
  if via_subtree then begin
    Eio.Path.mkdir ~perm:0o755 (fs / "stage");
    Eio.Path.with_open_dir (fs / "stage") (fun stage ->
        write_blocks Eio.Path.(stage / "f"))
  end
  else write_blocks (fs / "f");
  (* A truncating reopen of a writable file discards the durable bytes and
     overwrites the head -- all unsynced, so all at the crash's mercy. *)
  if trunc_reopen && not read_only then
    Eio.Path.with_open_out ~create:(`Or_truncate perm) (fs / rel) (fun file ->
        let buf = Cstruct.create block in
        Cstruct.memset buf 0xee;
        Eio.File.pwrite_all file ~file_offset:(i63 0) [ buf ]);
  let post = Crash.post_crash ctl in
  let img = Eio.Path.load (post / rel) in
  let mode = (Eio.Path.stat ~follow:true (post / rel)).perm in
  (img, mode)

(* Number of blocks that were synced (and so must survive), given the barrier
   position. [sync_after] is [-1] when no sync fired. *)
let synced_count ~nblocks ~sync_after =
  if sync_after >= 0 && sync_after < nblocks then sync_after + 1 else 0

let image_and_mode : (string * int) printer =
 fun ppf (img, mode) -> Fmt.pf ppf "%dB mode=%o" (String.length img) mode

(* {b Barrier invariant.} Under [Drop_unsynced] the post-crash image is exactly
   the synced prefix: every synced block survives whole, every un-synced block
   is gone -- including anything a truncating reopen did after the barrier.
   Synced data can never be lost, un-synced data never lingers, and the file
   keeps its mode whether or not it was writable. *)
let test_crash_barrier _seed nblocks block sync_after read_only via_subtree
    trunc_reopen =
  let sync_after = sync_after - 1 in
  let got =
    post_crash_image ~policy:Drop_unsynced ~nblocks ~block ~sync_after
      ~read_only ~via_subtree ~trunc_reopen
  in
  let k = synced_count ~nblocks ~sync_after in
  let expected =
    String.concat ""
      (List.init k (fun i -> String.make block (Char.chr (i land 0xff))))
  in
  let mode = if read_only then 0o444 else 0o644 in
  check_eq ~pp:image_and_mode ~eq:( = ) got (expected, mode)

(* {b Determinism.} The same seed, workload and barrier reproduce the exact same
   post-crash image under the seeded drop/reorder policy, whatever the file's
   mode and staging path. *)
let test_crash_determinism seed nblocks block sync_after read_only via_subtree
    trunc_reopen =
  let sync_after = sync_after - 1 in
  let policy = Crash.Reorder_unsynced seed in
  let a =
    post_crash_image ~policy ~nblocks ~block ~sync_after ~read_only ~via_subtree
      ~trunc_reopen
  in
  let b =
    post_crash_image ~policy ~nblocks ~block ~sync_after ~read_only ~via_subtree
      ~trunc_reopen
  in
  check_eq ~pp:image_and_mode ~eq:( = ) a b

let suite =
  ( "chaos",
    [
      test_case "crash-safety and determinism"
        [ int; range 101; range ~min:1 16; range ~min:1 256; bool; bool ]
        test_crash_safety_and_determinism;
      test_case "crash barrier invariant"
        [ int; range ~min:1 16; range ~min:1 64; range 18; bool; bool; bool ]
        test_crash_barrier;
      test_case "crash post-image determinism"
        [ int; range ~min:1 16; range ~min:1 64; range 18; bool; bool; bool ]
        test_crash_determinism;
    ] )
