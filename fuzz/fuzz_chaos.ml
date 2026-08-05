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
      file's mode, staging subtree, or truncating reopens after the barrier. The
      name is pinned separately, by the directory barrier.
    + {b Reference model.} An arbitrary operation tape -- creates, writes,
      syncs, unlinks, mkdirs, rmdirs, symlinks, renames, metadata barriers --
      drives a crash-wrapped tree in lockstep with a pure model of the journal
      semantics; the recovered tree must equal the model's post-crash tree byte
      for byte, and the seeded reorder policy must reproduce the same recovered
      tree for the same seed. *)

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

(* The adversarial dimensions of a release store, as variants so a fuzzed case
   reads as what it drives rather than a row of booleans. *)
type access = Writable | Read_only
type placement = Direct | Via_subtree
type reopen = No_reopen | Trunc_reopen

let access_gen = map [ bool ] (fun b -> if b then Read_only else Writable)
let placement_gen = map [ bool ] (fun b -> if b then Via_subtree else Direct)
let reopen_gen = map [ bool ] (fun b -> if b then Trunc_reopen else No_reopen)

(* [prob_pct] in 0..100, [nblocks] in 1..16, [block] in 1..256; [seed] is any
   int (negatives seed the PRNG just as well). *)
let test_crash_safety_and_determinism seed prob_pct nblocks block access
    placement =
  let probability = float_of_int prob_pct /. 100. in
  let read_only = access = Read_only
  and via_subtree = placement = Via_subtree in
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
  (* The store's directory barrier, pinning the new name; what the file holds
     is the file sync's business, settled block by block above. *)
  Crash.sync_metadata ctl;
  (* A truncating reopen of a writable file discards the durable bytes and
     overwrites the head -- all unsynced, so all at the crash's mercy. *)
  if trunc_reopen && not read_only then
    Eio.Path.with_open_out ~create:(`Or_truncate perm) (fs / rel) (fun file ->
        let buf = Cstruct.create block in
        Cstruct.memset buf 0xee;
        Eio.File.pwrite_all file ~file_offset:(i63 0) [ buf ]);
  let post = Crash.post_crash ctl in
  match Eio.Path.kind ~follow:false (post / rel) with
  | `Not_found -> None
  | _ ->
      let img = Eio.Path.load (post / rel) in
      let mode = (Eio.Path.stat ~follow:true (post / rel)).perm in
      Some (img, mode)

(* Number of blocks that were synced (and so must survive), given the barrier
   position. [sync_after] is [-1] when no sync fired. *)
let synced_count ~nblocks ~sync_after =
  if sync_after >= 0 && sync_after < nblocks then sync_after + 1 else 0

let image_and_mode : (string * int) option printer =
 fun ppf -> function
  | None -> Fmt.string ppf "<absent>"
  | Some (img, mode) -> Fmt.pf ppf "%dB mode=%o" (String.length img) mode

(* {b Barrier invariant.} Under [Drop_unsynced] the post-crash image is exactly
   the synced prefix: every synced block survives whole, every un-synced block
   is gone -- including anything a truncating reopen did after the barrier.
   Synced data can never be lost, un-synced data never lingers, and the file
   keeps its mode whether or not it was writable. The name is pinned by the
   directory barrier, so the file is always there; when no sync ever fired it
   is there and empty. *)
let test_crash_barrier _seed nblocks block sync_after access placement reopen =
  let sync_after = sync_after - 1 in
  let got =
    post_crash_image ~policy:Drop_unsynced ~nblocks ~block ~sync_after
      ~read_only:(access = Read_only) ~via_subtree:(placement = Via_subtree)
      ~trunc_reopen:(reopen = Trunc_reopen)
  in
  let k = synced_count ~nblocks ~sync_after in
  let expected =
    let img =
      String.concat ""
        (List.init k (fun i -> String.make block (Char.chr (i land 0xff))))
    in
    let mode = match access with Read_only -> 0o444 | Writable -> 0o644 in
    Some (img, mode)
  in
  check_eq ~pp:image_and_mode ~eq:( = ) got expected

(* {2 Reference-model oracle}

   A pure model of the durability contract runs an arbitrary operation tape in
   lockstep with a crash-wrapped in-memory tree. At the crash the model
   computes the exact post-crash tree -- committed metadata shapes over
   per-file durable contents -- and the recovered tree must match it byte for
   byte. This is the widest net: any divergence between the implementation and
   the journal semantics in chaos.mli fails the run, anticipated or not. *)

module Model = struct
  module SM = Map.Make (String)
  module IM = Map.Make (Int)

  (* Shapes keyed by path; file contents keyed by a rename-stable id. *)
  type shape = File of int | Dir | Link of string

  type t = {
    mutable live : shape SM.t;
    mutable durable : shape SM.t;
    mutable journal : (shape SM.t -> shape SM.t) list; (* newest first *)
    mutable live_data : string IM.t;
    mutable durable_data : string IM.t;
    mutable next_id : int;
  }

  let v () =
    {
      live = SM.empty;
      durable = SM.empty;
      journal = [];
      live_data = IM.empty;
      durable_data = IM.empty;
      next_id = 0;
    }

  let log m edit = m.journal <- edit :: m.journal

  (* The metadata barrier: pending directory operations land in the durable
     world, oldest first. *)
  let commit m =
    m.durable <- List.fold_right (fun edit d -> edit d) m.journal m.durable;
    m.journal <- []

  let under prefix p =
    let n = String.length prefix in
    String.length p > n && String.equal (String.sub p 0 n) prefix && p.[n] = '/'

  (* Move the subtree at [src] over [dst], removing whatever the move
     replaces -- rename semantics. *)
  let move ~src ~dst map =
    let map =
      SM.filter (fun k _ -> not (String.equal k dst || under dst k)) map
    in
    SM.fold
      (fun k v acc ->
        if String.equal k src then SM.add dst v acc
        else if under src k then
          let tail =
            String.sub k (String.length src)
              (String.length k - String.length src)
          in
          SM.add (dst ^ tail) v acc
        else SM.add k v acc)
      map SM.empty

  let file_id m path =
    match SM.find path m.live with File id -> id | _ -> assert false

  let new_file m path =
    let id = m.next_id in
    m.next_id <- id + 1;
    m.live <- SM.add path (File id) m.live;
    m.live_data <- IM.add id "" m.live_data;
    m.durable_data <- IM.add id "" m.durable_data;
    log m (SM.add path (File id))

  let write m path s = m.live_data <- IM.add (file_id m path) s m.live_data

  (* A file sync makes that file's contents durable and nothing else: the
     directory entries naming it are the metadata journal's business, committed
     only by the barrier below, as fsync(2) divides them. *)
  let sync m path =
    let id = file_id m path in
    m.durable_data <- IM.add id (IM.find id m.live_data) m.durable_data

  let unlink m path =
    m.live <- SM.remove path m.live;
    log m (SM.remove path)

  let mkdir m path =
    m.live <- SM.add path Dir m.live;
    log m (SM.add path Dir)

  let rmdir m path =
    m.live <- SM.remove path m.live;
    log m (SM.remove path)

  let symlink m path target =
    m.live <- SM.add path (Link target) m.live;
    log m (SM.add path (Link target))

  let rename m ~src ~dst =
    m.live <- move ~src ~dst m.live;
    log m (move ~src ~dst)

  (* The exact post-crash tree under [Drop_unsynced]: committed shapes over
     per-file durable contents. *)
  let expected m =
    SM.map
      (function
        | File id -> `File (IM.find id m.durable_data)
        | Dir -> `Dir
        | Link target -> `Link target)
      m.durable
end

(* Read the real recovered tree into the model's representation. *)
let snapshot dir =
  let module SM = Model.SM in
  let rec walk acc rel p =
    List.fold_left
      (fun acc name ->
        let q = Eio.Path.(p / name) in
        let key = if String.equal rel "" then name else rel ^ "/" ^ name in
        match Eio.Path.kind ~follow:false q with
        | `Regular_file -> SM.add key (`File (Eio.Path.load q)) acc
        | `Directory -> walk (SM.add key `Dir acc) key q
        | `Symbolic_link -> SM.add key (`Link (Eio.Path.read_link q)) acc
        | _ -> acc)
      acc (Eio.Path.read_dir p)
  in
  walk SM.empty "" dir

let pp_node ppf = function
  | `File s ->
      Fmt.pf ppf "file[%dB:%a]" (String.length s) Fmt.(truncated ~max:12) s
  | `Dir -> Fmt.string ppf "dir"
  | `Link target -> Fmt.pf ppf "link->%s" target

let pp_tree ppf t =
  Fmt.pf ppf "@[<v>%a@]"
    (Fmt.list (fun ppf (k, v) -> Fmt.pf ppf "%s: %a" k pp_node v))
    (Model.SM.bindings t)

(* A rename pick is applicable when it moves to a genuinely new name outside
   its own subtree: a file or symlink may clobber a file or symlink, a
   directory only lands on a fresh name. *)
let rename_ok (m : Model.t) ~src ~dst =
  let src_is_dir =
    match Model.SM.find src m.Model.live with
    | Model.Dir -> true
    | Model.File _ | Model.Link _ -> false
  in
  (not (String.equal dst src))
  && (not (Model.under src dst))
  &&
  match Model.SM.find_opt dst m.Model.live with
  | None -> true
  | Some Model.Dir -> false
  | Some (Model.File _ | Model.Link _) -> not src_is_dir

(* Create [path] with unsynced [content], in the tree and the model. *)
let add_file fs (m : Model.t) path content =
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    Eio.Path.(fs / path)
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string content ]);
  Model.new_file m path;
  Model.write m path content

(* One tape step: four bytes pick an operation and its arguments. Picks that
   do not apply to the current tree (unlink with nothing to unlink, mkdir over
   an existing name) are skipped, so every tape is valid. Candidate lists come
   from the model's sorted map, so the same tape always drives the same
   operations. *)
let apply_op fs ctl (m : Model.t) b0 b1 b2 b3 =
  let module SM = Model.SM in
  let names = [| "a"; "b"; "c"; "d" |] in
  let join parent name = if parent = "" then name else parent ^ "/" ^ name in
  let keys pred =
    SM.fold (fun k v acc -> if pred k v then k :: acc else acc) m.Model.live []
    |> List.rev
  in
  let dirs () = "" :: keys (fun _ v -> v = Model.Dir) in
  let files () =
    keys (fun _ v -> match v with Model.File _ -> true | _ -> false)
  in
  let entries () =
    keys (fun _ v ->
        match v with Model.File _ | Model.Link _ -> true | _ -> false)
  in
  let pick lst k =
    match lst with
    | [] -> None
    | _ -> Some (List.nth lst (k mod List.length lst))
  in
  let fresh_path pk nk =
    Option.bind (pick (dirs ()) pk) @@ fun parent ->
    let path = join parent names.(nk mod 4) in
    if SM.mem path m.Model.live then None else Some path
  in
  match b0 mod 9 with
  | 0 -> (
      (* create a file with unsynced content *)
      match fresh_path b1 b2 with
      | None -> ()
      | Some path -> add_file fs m path (Fmt.str "c%d" b3))
  | 1 -> (
      (* rewrite an existing file, unsynced *)
      match pick (files ()) b1 with
      | None -> ()
      | Some path ->
          let content = Fmt.str "w%d" b2 in
          Eio.Path.save ~create:(`Or_truncate 0o644) (fs / path) content;
          Model.write m path content)
  | 2 -> (
      (* sync a file: its data becomes durable and the journal commits *)
      match pick (files ()) b1 with
      | None -> ()
      | Some path ->
          Eio.Path.with_open_out ~create:`Never (fs / path) (fun file ->
              Eio.File.sync file);
          Model.sync m path)
  | 3 -> (
      match pick (entries ()) b1 with
      | None -> ()
      | Some path ->
          Eio.Path.unlink (fs / path);
          Model.unlink m path)
  | 4 -> (
      match fresh_path b1 b2 with
      | None -> ()
      | Some path ->
          Eio.Path.mkdir ~perm:0o755 (fs / path);
          Model.mkdir m path)
  | 5 -> (
      (* remove an empty directory *)
      let empty_dirs =
        keys (fun k v ->
            v = Model.Dir
            && not (SM.exists (fun q _ -> Model.under k q) m.Model.live))
      in
      match pick empty_dirs b1 with
      | None -> ()
      | Some path ->
          Eio.Path.rmdir (fs / path);
          Model.rmdir m path)
  | 6 -> (
      match fresh_path b1 b2 with
      | None -> ()
      | Some path ->
          let target = names.(b3 mod 4) in
          Eio.Path.symlink ~link_to:target (fs / path);
          Model.symlink m path target)
  | 7 -> (
      (* rename an entry; a file or symlink may clobber a file or symlink,
         a directory only lands on a fresh name *)
      let sources = keys (fun _ _ -> true) in
      match pick sources b1 with
      | None -> ()
      | Some src -> (
          match pick (dirs ()) b2 with
          | None -> ()
          | Some parent ->
              let dst = join parent names.(b3 mod 4) in
              if rename_ok m ~src ~dst then begin
                Eio.Path.rename (fs / src) (fs / dst);
                Model.rename m ~src ~dst
              end))
  | _ ->
      (* the directory-fsync barrier *)
      Crash.sync_metadata ctl;
      Model.commit m

(* Run [tape] against a fresh crash-wrapped tree; return the model and the
   recovered tree. *)
let run_tape ~policy tape =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let ctl = Crash.v ~policy () in
  let fs = Crash.wrap ctl env#fs in
  let m = Model.v () in
  let len = String.length tape in
  let steps = min 32 (Int.div len 4) in
  for i = 0 to steps - 1 do
    let b j = Char.code tape.[(i * 4) + j] in
    apply_op fs ctl m (b 0) (b 1) (b 2) (b 3)
  done;
  let post = Crash.post_crash ctl in
  (m, snapshot post)

(* {b Reference model.} Under [Drop_unsynced] the recovered tree is exactly
   the model's committed world. *)
let test_crash_reference_model tape =
  let m, actual = run_tape ~policy:Drop_unsynced tape in
  check_eq ~pp:pp_tree ~eq:(Model.SM.equal ( = )) actual (Model.expected m)

(* {b Reorder determinism.} The same tape and seed recover the same tree, over
   workloads that include directory operations. *)
let test_crash_reorder_model_determinism tape seed =
  let _, a = run_tape ~policy:(Crash.Reorder_unsynced seed) tape in
  let _, b = run_tape ~policy:(Crash.Reorder_unsynced seed) tape in
  check_eq ~pp:pp_tree ~eq:(Model.SM.equal ( = )) a b

(* {b Determinism.} The same seed, workload and barrier reproduce the exact same
   post-crash image under the seeded drop/reorder policy, whatever the file's
   mode and staging path. *)
let test_crash_determinism seed nblocks block sync_after access placement reopen
    =
  let sync_after = sync_after - 1 in
  let policy = Crash.Reorder_unsynced seed in
  let read_only = access = Read_only
  and via_subtree = placement = Via_subtree
  and trunc_reopen = reopen = Trunc_reopen in
  let a =
    post_crash_image ~policy ~nblocks ~block ~sync_after ~read_only ~via_subtree
      ~trunc_reopen
  in
  let b =
    post_crash_image ~policy ~nblocks ~block ~sync_after ~read_only ~via_subtree
      ~trunc_reopen
  in
  check_eq ~pp:image_and_mode ~eq:( = ) a b

(* ── [Eio_fs_sync.publish] under crash and fault ──────────────────── *)

(* [publish] is the durability primitive the storage packages in this tree are
   built on: irmin's car, git and parquet stores, gauth's credential file, and
   the OCI registry all commit through it. Its guarantee is worth fuzzing
   directly rather than five times over in the consumers.

   The guarantee is atomic replacement: after a crash the target holds either
   the old content or the new one, never a splice of the two and never a
   truncated prefix. A credential file, a store snapshot or a manifest that
   comes back half-written is worse than one that never changed -- the old
   content was valid, and the caller has no way to tell the difference. *)

let publish_string path data =
  let config =
    Eio_fs_sync.
      {
        durable = true;
        perm = 0o644;
        max_bytes = Int64.of_int (String.length data);
      }
  in
  Eio_fs_sync.publish ~config path (Eio.Flow.copy_string data)

let test_publish_is_atomic old_body new_body =
  let cap s = if String.length s > 2048 then String.sub s 0 2048 else s in
  let old_body = cap old_body and new_body = cap new_body in
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let module Crash = Eio_fs.Chaos.Crash in
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let fs = Crash.wrap ctl env#fs in
  publish_string Eio.Path.(fs / "f") old_body;
  (* The second save is the one the crash interrupts. *)
  (try publish_string Eio.Path.(fs / "f") new_body with Eio.Io _ -> ());
  let post = Crash.post_crash ctl in
  match Eio.Path.kind ~follow:false Eio.Path.(post / "f") with
  | `Not_found ->
      (* Losing the file entirely would be a broken replacement: the old
         content was already durable before the second save began. *)
      fail "publish removed a file that was already durable"
  | _ ->
      let got = Eio.Path.load Eio.Path.(post / "f") in
      if got <> old_body && got <> new_body then
        failf
          "after the crash the file holds %d bytes that are neither the old \
           (%d) nor the new (%d) content: the replacement was not atomic"
          (String.length got) (String.length old_body) (String.length new_body)

(* When the filesystem refuses the write outright, the old content must still be
   there. A save that destroys the previous value on its way to failing is the
   worst outcome: the caller is told the save failed and has also lost what it
   had. *)
let test_failed_save_preserves_old seed old_body new_body =
  let cap s = if String.length s > 1024 then String.sub s 0 1024 else s in
  let old_body = cap old_body and new_body = cap new_body in
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  (* Write the original through a clean view, then fault the replacement. *)
  publish_string Eio.Path.(env#fs / "f") old_body;
  let sched =
    Eio_fs.Chaos.Schedule.seeded ~probability:1.0
      ~kinds:[ `Write_error; `Sync_error ]
      ~seed ()
  in
  let hostile = Eio_fs.Chaos.wrap sched env#fs in
  (try publish_string Eio.Path.(hostile / "f") new_body
   with Eio.Io _ | Sys_error _ -> ());
  match Eio.Path.kind ~follow:false Eio.Path.(env#fs / "f") with
  | `Not_found -> fail "a failed publish deleted the previous content"
  | _ ->
      let got = Eio.Path.load Eio.Path.(env#fs / "f") in
      if got <> old_body && got <> new_body then
        failf
          "a failed publish left %d bytes that are neither the old (%d) nor \
           the new (%d) content"
          (String.length got) (String.length old_body) (String.length new_body)

let suite =
  ( "chaos",
    [
      test_case "crash-safety and determinism"
        [
          int;
          range 101;
          range ~min:1 16;
          range ~min:1 256;
          access_gen;
          placement_gen;
        ]
        test_crash_safety_and_determinism;
      test_case "crash barrier invariant"
        [
          int;
          range ~min:1 16;
          range ~min:1 64;
          range 18;
          access_gen;
          placement_gen;
          reopen_gen;
        ]
        test_crash_barrier;
      test_case "crash post-image determinism"
        [
          int;
          range ~min:1 16;
          range ~min:1 64;
          range 18;
          access_gen;
          placement_gen;
          reopen_gen;
        ]
        test_crash_determinism;
      test_case "crash reference model" [ bytes ] test_crash_reference_model;
      test_case "crash reorder model determinism" [ bytes; int ]
        test_crash_reorder_model_determinism;
      test_case "publish survives a crash whole or not at all" [ bytes; bytes ]
        test_publish_is_atomic;
      test_case "a failed publish leaves the old content intact"
        [ int; bytes; bytes ] test_failed_save_preserves_old;
    ] )
