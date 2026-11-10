(** Fuzz tests for {!Eio_fs.Chaos}: crash-safety (only [Eio.Io] or [End_of_file]
    ever escapes), determinism (a seeded schedule replays the same outcome and
    bytes), the [Drop_unsynced] barrier invariant (the post-crash image is the
    synced prefix and keeps its mode), and a reference model (an arbitrary
    operation tape drives a crash-wrapped tree in lockstep with a pure model of
    the journal, byte for byte). *)

open Alcobar
module Chaos = Eio_fs.Chaos

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* Run a fixed shape of workload -- a run of block writes, a sync, then a
   read-back -- over a chaos-wrapped in-memory filesystem, and report the
   outcome together with the bytes that actually reached the store. The file
   is created read-only (0o444) or writable, directly at the root or through a
   short-lived staging subtree. Every run starts from a fresh in-memory tree
   ([Eio_fs.run] makes a new one). *)
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

(* Write [nblocks] contiguous [block]-byte blocks, syncing once after block
   [sync_after], then crash under [policy] and return the post-crash image and
   mode. The file may be read-only or writable, staged directly or through a
   short-lived subtree, and reopened [Or_truncate] after the writer closed. *)
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
   lockstep with a crash-wrapped tree; the recovered tree must equal the
   model's post-crash tree byte for byte. *)

module Model = struct
  module SM = Map.Make (String)
  module IM = Map.Make (Int)

  (* Shapes keyed by path. Every node has an id that a rename keeps, as an
     inode, which also keys a file's contents; the root's is [root]. *)
  type shape = File of int | Dir of int | Link of string * int

  let root = -1
  let id_of = function File i | Dir i | Link (_, i) -> i

  (* One half of a directory operation, by identity: a node put in the
     directory [parent] at [path], with what is below it, or a node taken
     out wherever it is. *)
  type half =
    | Place of { parent : int; path : string; node : shape }
    | Move of { parent : int; path : string; id : int }
    | Remove of int

  type entry = {
    seq : int;
    halves : half list;
    dirs : int list; (* the directories it changed *)
    mutable barred : int list; (* those a barrier has reached *)
    mutable committed : bool;
  }

  type t = {
    mutable live : shape SM.t;
    mutable snapshot : shape SM.t;
    mutable journal : entry list; (* since the snapshot, newest first *)
    mutable live_data : string IM.t;
    mutable durable_data : string IM.t;
    mutable next_id : int;
    mutable seq : int;
  }

  let v () =
    {
      live = SM.empty;
      snapshot = SM.empty;
      journal = [];
      live_data = IM.empty;
      durable_data = IM.empty;
      next_id = 0;
      seq = 0;
    }

  let under prefix p =
    let n = String.length prefix in
    String.length p > n && String.equal (String.sub p 0 n) prefix && p.[n] = '/'

  let at_or_under prefix p = String.equal prefix p || under prefix p

  let parent p =
    match String.rindex_opt p '/' with None -> "" | Some i -> String.sub p 0 i

  let base p =
    match String.rindex_opt p '/' with
    | None -> p
    | Some i -> String.sub p (i + 1) (String.length p - i - 1)

  let join up name = if String.equal up "" then name else up ^ "/" ^ name

  let fresh m =
    let id = m.next_id in
    m.next_id <- id + 1;
    id

  (* Where the node [id] is in [map]. *)
  let path_of map id =
    if id = root then Some ""
    else
      SM.fold
        (fun k v acc -> if acc = None && id_of v = id then Some k else acc)
        map None

  let is_dir map path id =
    if String.equal path "" then id = root
    else SM.find_opt path map = Some (Dir id)

  let dir_id map path =
    if String.equal path "" then root
    else match SM.find path map with Dir id -> id | _ -> assert false

  let remove_under p map = SM.filter (fun k _ -> not (at_or_under p k)) map

  (* Where a node put at [path] in the directory [pid] goes in [map]. *)
  let target map path pid =
    let up = parent path in
    if is_dir map up pid then Some (join up (base path))
    else
      match path_of map pid with
      | Some p when is_dir map p pid -> Some (join p (base path))
      | Some _ | None -> None

  (* An entry lands whole or not at all: a node is put in its directory by
     identity and an rmdir of a directory that still holds entries removes
     nothing. A moved node is the world's own with what it holds there, and
     leaves the world if moved where the world has no directory, or below
     itself. *)
  let applies map e =
    List.for_all
      (function
        | Place { parent = pid; path; _ } -> target map path pid <> None
        | Move _ -> true
        | Remove id -> (
            match path_of map id with
            | Some p -> not (SM.exists (fun k _ -> under p k) map)
            | None -> true))
      e.halves

  let land_half map = function
    | Remove id -> (
        match path_of map id with
        | Some p when not (String.equal p "") -> remove_under p map
        | Some _ | None -> map)
    | Place { parent = pid; path; node } -> (
        match target map path pid with
        | None -> map
        | Some at -> SM.add at node (remove_under at map))
    | Move { parent = pid; path; id } -> (
        match (path_of map id, target map path pid) with
        | Some from, Some at when String.equal from at -> map
        | Some from, Some at when under from at -> remove_under from map
        | Some from, None when not (String.equal from "") ->
            remove_under from map
        | Some from, Some at ->
            let below =
              SM.fold
                (fun k v acc ->
                  if at_or_under from k then
                    ( String.sub k (String.length from)
                        (String.length k - String.length from),
                      v )
                    :: acc
                  else acc)
                map []
            in
            let map = remove_under at (remove_under from map) in
            List.fold_left
              (fun map (suffix, v) -> SM.add (at ^ suffix) v map)
              map below
        | _ -> map)

  let replay map e =
    if applies map e then List.fold_left land_half map e.halves else map

  (* The durable world: the snapshot with every committed entry, in program
     order. *)
  let durable m =
    List.fold_left
      (fun map e -> if e.committed then replay map e else map)
      m.snapshot (List.rev m.journal)

  (* Log an operation: [halves] with the directories they change. *)
  let log m halves =
    m.seq <- m.seq + 1;
    m.journal <-
      {
        seq = m.seq;
        halves = List.map snd halves;
        dirs = List.map fst halves;
        barred = [];
        committed = false;
      }
      :: m.journal

  let place m path =
    let pid = dir_id m.live (parent path) in
    log m [ (pid, Place { parent = pid; path; node = SM.find path m.live }) ]

  (* The ids an entry makes, and those it moves or removes. *)
  let nodes e =
    List.filter_map
      (function
        | Place { node; _ } -> Some (id_of node) | Move _ | Remove _ -> None)
      e.halves

  let named e =
    List.filter_map
      (function Move { id; _ } | Remove id -> Some id | Place _ -> None)
      e.halves

  (* The pending entries before [e] it needs: those that changed a
     directory it changes, those that made a node it names, and for an rmdir
     those that changed the directory. *)
  let deps m e =
    let names = e.dirs @ named e in
    let removed =
      List.filter_map (function Remove id -> Some id | _ -> None) e.halves
    in
    List.filter
      (fun e' ->
        (not e'.committed) && e'.seq < e.seq
        && (List.exists (fun d -> List.mem d e.dirs) e'.dirs
           || List.exists (fun n -> List.mem n names) (nodes e')
           || List.exists (fun d -> List.mem d removed) e'.dirs))
      m.journal

  (* The metadata barrier: the durable world becomes the live one. *)
  let commit m =
    m.snapshot <- m.live;
    m.journal <- []

  (* The barrier on directory [dir]: the pending operations that changed it,
     by id, are barred, and every barred operation one of whose directories
     is durable commits, until none is left. *)
  let commit_dir m dir =
    let id = dir_id m.live dir in
    List.iter
      (fun e -> if List.mem id e.dirs then e.barred <- id :: e.barred)
      m.journal;
    let rec settle () =
      let d = durable m in
      let held id =
        match path_of d id with Some p -> is_dir d p id | None -> false
      in
      let ready =
        List.filter
          (fun e ->
            (not e.committed) && e.barred <> [] && List.exists held e.dirs)
          m.journal
      in
      if ready <> [] then begin
        let rec commit e =
          if not e.committed then begin
            e.committed <- true;
            List.iter commit (deps m e)
          end
        in
        List.iter commit ready;
        settle ()
      end
    in
    settle ()

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
    let id = fresh m in
    m.live <- SM.add path (File id) m.live;
    m.live_data <- IM.add id "" m.live_data;
    m.durable_data <- IM.add id "" m.durable_data;
    place m path

  let write m path s = m.live_data <- IM.add (file_id m path) s m.live_data
  let contents m path = IM.find (file_id m path) m.live_data

  (* A file sync makes that file's contents durable and nothing else: the
     directory entries naming it are the metadata journal's business, committed
     only by the barrier below, as fsync(2) divides them. *)
  let sync m path =
    let id = file_id m path in
    m.durable_data <- IM.add id (IM.find id m.live_data) m.durable_data

  let remove m path =
    let id = id_of (SM.find path m.live) in
    m.live <- SM.remove path m.live;
    log m [ (dir_id m.live (parent path), Remove id) ]

  let unlink = remove
  let rmdir = remove

  let mkdir m path =
    m.live <- SM.add path (Dir (fresh m)) m.live;
    place m path

  let symlink m path target =
    m.live <- SM.add path (Link (target, fresh m)) m.live;
    place m path

  let rename m ~src ~dst =
    let id = id_of (SM.find src m.live) in
    let from = dir_id m.live (parent src) in
    m.live <- move ~src ~dst m.live;
    let up = dir_id m.live (parent dst) in
    m.seq <- m.seq + 1;
    m.journal <-
      {
        seq = m.seq;
        halves = [ Move { parent = up; path = dst; id } ];
        dirs = [ from; up ];
        barred = [];
        committed = false;
      }
      :: m.journal

  let image shapes data =
    SM.map
      (function
        | File id -> `File (IM.find id data)
        | Dir _ -> `Dir
        | Link (target, _) -> `Link target)
      shapes

  (* The exact post-crash tree under [Drop_unsynced]: committed shapes over
     per-file durable contents. *)
  let expected m = image (durable m) m.durable_data

  (* The exact post-crash tree when every change survives: the live one. *)
  let live m = image m.live m.live_data
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
    | Model.Dir _ -> true
    | Model.File _ | Model.Link _ -> false
  in
  (not (String.equal dst src))
  && (not (Model.under src dst))
  &&
  match Model.SM.find_opt dst m.Model.live with
  | None -> true
  | Some (Model.Dir _) -> false
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

(* The names a tape step creates or moves to. *)
let names = [| "a"; "b"; "c"; "d" |]

let join parent name =
  if String.equal parent "" then name else parent ^ "/" ^ name

(* The live paths whose shape satisfies [pred], in key order. *)
let keys (m : Model.t) pred =
  Model.SM.fold
    (fun k v acc -> if pred k v then k :: acc else acc)
    m.Model.live []
  |> List.rev

(* The live directories, the root [""] first. *)
let dirs m =
  "" :: keys m (fun _ v -> match v with Model.Dir _ -> true | _ -> false)

let files m =
  keys m (fun _ v -> match v with Model.File _ -> true | _ -> false)

(* The live entries an unlink removes: files and symbolic links. *)
let entries m =
  keys m (fun _ v ->
      match v with Model.File _ | Model.Link _ -> true | _ -> false)

let pick lst k =
  match lst with [] -> None | _ -> Some (List.nth lst (k mod List.length lst))

(* A path not yet live: a name picked by [nk] in a directory picked by [pk]. *)
let fresh_path (m : Model.t) pk nk =
  Option.bind (pick (dirs m) pk) @@ fun parent ->
  let path = join parent names.(nk mod 4) in
  if Model.SM.mem path m.Model.live then None else Some path

(* Rewrite the file [path] whole with unsynced [content]. *)
let rewrite fs m path content =
  Eio.Path.save ~create:(`Or_truncate 0o644) (fs / path) content;
  Model.write m path content

(* Append [more] to the file [path], unsynced: the write lands at the end. *)
let append fs m path more =
  Eio.Path.with_open_out ~append:true ~create:`Never (fs / path) (fun file ->
      ignore (Eio.File.seek file (i63 0) `Set : Optint.Int63.t);
      Eio.Flow.copy_string more file);
  Model.write m path (Model.contents m path ^ more)

(* Seek into the file [path] at an offset picked by [k] and write [data] there,
   unsynced. *)
let write_at fs m path k data =
  let old = Model.contents m path in
  let at = k mod (String.length old + 1) in
  Eio.Path.with_open_out ~create:`Never (fs / path) (fun file ->
      ignore (Eio.File.seek file (i63 at) `Set : Optint.Int63.t);
      Eio.Flow.copy_string data file);
  let tail_at = at + String.length data in
  let tail =
    if tail_at >= String.length old then ""
    else String.sub old tail_at (String.length old - tail_at)
  in
  Model.write m path (String.sub old 0 at ^ data ^ tail)

(* One tape step: four bytes pick an operation and its arguments. Picks that
   do not apply to the current tree (unlink with nothing to unlink, mkdir over
   an existing name) are skipped, so every tape is valid. Candidate lists come
   from the model's sorted map, so the same tape always drives the same
   operations. *)
let apply_op ~dir_barriers fs ctl (m : Model.t) b0 b1 b2 b3 =
  match b0 mod 12 with
  | 0 -> (
      (* create a file with unsynced content *)
      match fresh_path m b1 b2 with
      | None -> ()
      | Some path -> add_file fs m path (Fmt.str "c%d" b3))
  | 1 -> (
      (* rewrite an existing file, unsynced *)
      match pick (files m) b1 with
      | None -> ()
      | Some path -> rewrite fs m path (Fmt.str "w%d" b2))
  | 2 -> (
      (* sync a file: its data becomes durable and the journal commits *)
      match pick (files m) b1 with
      | None -> ()
      | Some path ->
          Eio.Path.with_open_out ~create:`Never (fs / path) (fun file ->
              Eio.File.sync file);
          Model.sync m path)
  | 3 -> (
      match pick (entries m) b1 with
      | None -> ()
      | Some path ->
          Eio.Path.unlink (fs / path);
          Model.unlink m path)
  | 4 -> (
      match fresh_path m b1 b2 with
      | None -> ()
      | Some path ->
          Eio.Path.mkdir ~perm:0o755 (fs / path);
          Model.mkdir m path)
  | 5 -> (
      (* remove an empty directory *)
      let empty_dirs =
        keys m (fun k v ->
            (match v with Model.Dir _ -> true | _ -> false)
            && not (Model.SM.exists (fun q _ -> Model.under k q) m.Model.live))
      in
      match pick empty_dirs b1 with
      | None -> ()
      | Some path ->
          Eio.Path.rmdir (fs / path);
          Model.rmdir m path)
  | 6 -> (
      match fresh_path m b1 b2 with
      | None -> ()
      | Some path ->
          let target = names.(b3 mod 4) in
          Eio.Path.symlink ~link_to:target (fs / path);
          Model.symlink m path target)
  | 7 -> (
      (* rename an entry; a file or symlink may clobber a file or symlink,
         a directory only lands on a fresh name *)
      let sources = keys m (fun _ _ -> true) in
      match pick sources b1 with
      | None -> ()
      | Some src -> (
          match pick (dirs m) b2 with
          | None -> ()
          | Some parent ->
              let dst = join parent names.(b3 mod 4) in
              if rename_ok m ~src ~dst then begin
                Eio.Path.rename (fs / src) (fs / dst);
                Model.rename m ~src ~dst
              end))
  | 9 -> (
      (* append to a file, unsynced *)
      match pick (files m) b1 with
      | None -> ()
      | Some path -> append fs m path (Fmt.str "a%d" b2))
  | 10 -> (
      (* seek into a file and write there, unsynced *)
      match pick (files m) b1 with
      | None -> ()
      | Some path -> write_at fs m path b2 (Fmt.str "s%d" b3))
  | 8 ->
      (* the barrier on every directory *)
      Crash.sync_metadata ctl;
      Model.commit m
  | _ -> (
      (* the barrier on one directory: its own entries only *)
      match pick (dirs m) b1 with
      | Some dir when dir_barriers ->
          Eio_fs_sync.sync_dir (if String.equal dir "" then fs else fs / dir);
          Model.commit_dir m dir
      | Some _ | None -> ())

(* Run [tape] against a fresh crash-wrapped tree; return the model and the
   recovered tree. *)
let run_tape ?(dir_barriers = true) ~policy tape =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let ctl = Crash.v ~policy () in
  let fs = Crash.wrap ctl env#fs in
  let m = Model.v () in
  let len = String.length tape in
  let steps = min 32 (Int.div len 4) in
  for i = 0 to steps - 1 do
    let b j = Char.code tape.[(i * 4) + j] in
    apply_op ~dir_barriers fs ctl m (b 0) (b 1) (b 2) (b 3)
  done;
  let post = Crash.post_crash ctl in
  (m, snapshot post)

(* {b Reference model.} Under [Drop_unsynced] the recovered tree is exactly
   the model's committed world. *)
let test_crash_reference_model tape =
  let m, actual = run_tape ~policy:Drop_unsynced tape in
  check_eq ~pp:pp_tree ~eq:(Model.SM.equal ( = )) actual (Model.expected m)

(* {b Every change kept.} When the policy keeps every pending change, each
   write replayed where it landed and each directory operation in program
   order, the recovered tree is the live one, whatever barriers ran. *)
let test_crash_keep_all_model tape =
  let m, actual = run_tape ~policy:(Crash.Survive (fun _ -> true)) tape in
  check_eq ~pp:pp_tree ~eq:(Model.SM.equal ( = )) actual (Model.live m)

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

(* [Eio_fs_sync.publish] under crash and fault.

   [publish] is the durability primitive the storage packages here build on.
   After a crash the target holds the old content or the new one, never a
   splice and never a truncated prefix. *)

let publish_string path data =
  let config =
    Eio_fs_sync.config ~max_bytes:(Int64.of_int (String.length data)) ()
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
      test_case "crash keep-all model" [ bytes ] test_crash_keep_all_model;
      test_case "crash reorder model determinism" [ bytes; int ]
        test_crash_reorder_model_determinism;
      test_case "publish survives a crash whole or not at all" [ bytes; bytes ]
        test_publish_is_atomic;
      test_case "a failed publish leaves the old content intact"
        [ int; bytes; bytes ] test_failed_save_preserves_old;
    ] )
