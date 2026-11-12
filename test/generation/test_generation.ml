(* A generation is a directory tree published whole. POSIX rename(2) keeps a
   replaced name visible throughout, referring to the old file or the new one
   and never to neither ("a link named new shall remain visible to other
   threads throughout the renaming operation and refer either to the file
   referred to by new or old before the operation began"), so a pointer renamed
   over a pointer is the one switch a tree can take atomically. Durability is
   the crash model's: a file's bytes survive only up to its last sync, a
   directory's entries only up to its last barrier, and uncommitted directory
   operations survive as a prefix of program order (Chaos.Crash).

   Each case crashes at every durability operation of one publication in turn
   and reads the post-crash tree back through the pointer: it is the whole old
   generation or the whole new one. *)

module Crash = Eio_fs.Chaos.Private.Crash

let ( / ) = Eio.Path.( / )

(* The two trees differ in every file's length and in their file sets, so a
   torn file, a lost file and a stray one each read as neither. *)
let old_tree = [ ("a", "old-a"); ("sub/b", "old-b") ]
let new_tree = [ ("a", "new-a-longer"); ("c", "new-c"); ("sub/b", "new-bb") ]

(* The producer writes and never syncs: making the bytes durable is the
   publication's job. *)
let populate tree dir =
  List.iter
    (fun (rel, content) ->
      (match Filename.dirname rel with
      | "." -> ()
      | sub -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (dir / sub));
      Eio.Path.save ~create:(`Exclusive 0o644) (dir / rel) content)
    tree

let rec files prefix p =
  List.concat_map
    (fun name ->
      let rel = if prefix = "" then name else prefix ^ "/" ^ name in
      match Eio.Path.kind ~follow:false (p / name) with
      | `Directory -> files rel (p / name)
      | _ -> [ (rel, Eio.Path.load (p / name)) ])
    (List.sort String.compare (Eio.Path.read_dir p))

(* What a reader resolving the published name sees. *)
let read_back p =
  match Eio.Path.kind ~follow:true p with
  | `Not_found -> None
  | _ -> Some (files "" p)

let tree = Alcotest.(option (list (pair string string)))

let pp_tree ppf = function
  | None -> Fmt.string ppf "absent"
  | Some t -> Fmt.(Dump.list (Dump.pair string string)) ppf t

let publish ~durable fs tree =
  Eio_fs_sync.publish_generation ~durable (fs / "out") (populate tree)

(* A base holding [old_tree], published and committed. *)
let with_old fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let ctl = Crash.v () in
  publish ~durable:true (Crash.wrap ctl env#fs) old_tree;
  fn (Crash.post_crash ctl)

let with_empty fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn env#fs

(* Crash the publication of [new_tree] over [setup]'s base at its [n]th
   durability operation, for every [n] until one runs to completion. Each
   crashed image must read as one of [before]; the completed one as the new
   tree. Returns the outcome of every crash point. *)
let every_crash_point ~policy ~setup ~before =
  let rec at n acc =
    if n > 1000 then Alcotest.fail "the publication never completed";
    setup @@ fun base ->
    let ctl = Crash.v ~policy ~trigger:(Crash.Nth_op n) () in
    (try publish ~durable:true (Crash.wrap ctl base) new_tree
     with Eio.Io _ when Crash.is_crashed ctl -> ());
    let completed = not (Crash.is_crashed ctl) in
    let seen = read_back (Crash.post_crash ctl / "out") in
    if completed then begin
      Alcotest.check tree "a completed publication reads as the new tree"
        (Some new_tree) seen;
      List.rev acc
    end
    else begin
      if not (List.exists (( = ) seen) before) then
        Alcotest.failf "crash at operation %d: the tree read as %a" n pp_tree
          seen;
      at (n + 1) (seen :: acc)
    end
  in
  at 1 []

let check_windows ~name outcomes ~expect =
  Alcotest.(check bool)
    (name ^ ": the crash points include one that keeps the previous tree")
    true
    (List.exists (( = ) expect) outcomes);
  Alcotest.(check bool)
    (name ^ ": and one after the switch that reads the new tree")
    true
    (List.exists (( = ) (Some new_tree)) outcomes)

let test_replace_drop () =
  let outcomes =
    every_crash_point ~policy:Crash.Drop_unsynced ~setup:with_old
      ~before:[ Some old_tree; Some new_tree ]
  in
  check_windows ~name:"drop" outcomes ~expect:(Some old_tree)

(* The seeded policy keeps a random subset of unsynced writes, in a shuffled
   order, and a random prefix of the uncommitted directory operations. *)
let test_replace_reorder () =
  for seed = 1 to 8 do
    ignore
      (every_crash_point ~policy:(Crash.Reorder_unsynced seed) ~setup:with_old
         ~before:[ Some old_tree; Some new_tree ])
  done

let test_first_publication () =
  let outcomes =
    every_crash_point ~policy:Crash.Drop_unsynced ~setup:with_empty
      ~before:[ None; Some new_tree ]
  in
  (* Nothing follows the last barrier of a first publication, so no crash
     point lands after its switch commits; the completed run reads the new
     tree. *)
  Alcotest.(check bool)
    "first: the crash points include one that leaves the name absent" true
    (List.exists Option.is_none outcomes)

(* The generation a publication replaces is reclaimed once the switch is
   committed, and a crash leaves nothing a later publication trips on. *)
let test_previous_reclaimed () =
  with_old @@ fun base ->
  publish ~durable:true base new_tree;
  let entries = List.sort String.compare (Eio.Path.read_dir base) in
  let target = Eio.Path.read_link (base / "out") in
  Alcotest.(check (list string))
    "the pointer and its generation, nothing else"
    (List.sort String.compare [ "out"; target ])
    entries;
  Alcotest.check tree "the new tree" (Some new_tree) (read_back (base / "out"))

(* A link a caller made by hand names a tree this publication does not own:
   the switch replaces the link and leaves the tree. *)
let test_foreign_target_kept () =
  with_empty @@ fun fs ->
  populate [ ("elsewhere/x", "kept") ] fs;
  Eio.Path.symlink ~link_to:"elsewhere" (fs / "out");
  publish ~durable:true fs new_tree;
  Alcotest.check tree "the new tree" (Some new_tree) (read_back (fs / "out"));
  Alcotest.(check string)
    "the foreign tree stands" "kept"
    (Eio.Path.load (fs / "elsewhere" / "x"));
  (* Nor is another name's generation this name's to reclaim. *)
  let other = ".other.gen.1.2" in
  populate [ (other ^ "/x", "theirs") ] fs;
  Eio.Path.unlink (fs / "out");
  Eio.Path.symlink ~link_to:other (fs / "out");
  publish ~durable:true fs new_tree;
  Alcotest.(check string)
    "another name's generation stands" "theirs"
    (Eio.Path.load (fs / other / "x"))

(* A listing of the pointers' directory tells the pointers from their
   generations: the generation is hidden and carries its suffix, and no name a
   caller would choose reads as one. *)
let test_generation_names () =
  with_empty @@ fun fs ->
  publish ~durable:true fs new_tree;
  let target = Eio.Path.read_link (fs / "out") in
  Alcotest.(check bool)
    "the link names a generation" true
    (Eio_fs_sync.is_generation target);
  Alcotest.(check bool)
    "which is hidden" true
    (String.length target > 0 && target.[0] = '.');
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " is not a generation")
        false
        (Eio_fs_sync.is_generation name))
    [
      "out";
      ".out";
      "out.gen.1.2";
      ".out.gen.1";
      ".out.gen.x.2";
      ".out.gen.1.";
      "..gen.1.2";
      ".out.gen.1.2/x";
    ];
  Alcotest.(check bool)
    "a basename holding the infix still parses" true
    (Eio_fs_sync.is_generation ".a.gen.5.gen.12.3")

let test_failed_populate () =
  with_old @@ fun base ->
  let before = List.sort String.compare (Eio.Path.read_dir base) in
  (match
     Eio_fs_sync.publish_generation ~durable:true (base / "out") (fun dir ->
         populate new_tree dir;
         failwith "producer failed")
   with
  | () -> Alcotest.fail "a failed producer published"
  | exception Failure _ -> ());
  Alcotest.(check (list string))
    "the staged generation is removed" before
    (List.sort String.compare (Eio.Path.read_dir base));
  Alcotest.check tree "the previous tree stands" (Some old_tree)
    (read_back (base / "out"))

(* A plain directory is not a pointer: replacing it would need the rename-aside
   this primitive exists to remove, so it is refused and left as it was. *)
let test_refuses_plain_directory () =
  with_empty @@ fun fs ->
  populate [ ("out/x", "legacy") ] fs;
  let ran = ref false in
  (match
     Eio_fs_sync.publish_generation ~durable:true (fs / "out") (fun _ ->
         ran := true)
   with
  | () -> Alcotest.fail "a plain directory was replaced"
  | exception Invalid_argument _ -> ());
  Alcotest.(check bool) "the producer never ran" false !ran;
  Alcotest.(check (list string))
    "nothing was staged beside it" [ "out" ] (Eio.Path.read_dir fs);
  Alcotest.(check string)
    "the directory is untouched" "legacy"
    (Eio.Path.load (fs / "out" / "x"))

let with_posix fn =
  Eio_main.run @@ fun env ->
  let root = Eio.Stdenv.fs env / Filename.temp_dir "generation-readonly-" "" in
  Fun.protect
    ~finally:(fun () ->
      Eio_fs.Path.grant_owner root;
      Eio.Path.rmtree root)
    (fun () -> fn root)

let readonly tree dir =
  populate tree dir;
  Eio.Path.chmod ~follow:false ~perm:0o555 (dir / "readonly")

(* rename(2) can switch a rootfs whose entries are not writable. Reclaiming the
   old owned generation must work too, without following its foreign links. *)
let test_posix_readonly_reclaimed () =
  with_posix @@ fun root ->
  populate [ ("foreign/kept", "outside the generation") ] root;
  Eio.Path.chmod ~follow:false ~perm:0o500 (root / "foreign");
  let foreign = Eio.Path.stat ~follow:false (root / "foreign") in
  Eio_fs_sync.publish_generation ~durable:true (root / "out") (fun dir ->
      readonly [ ("readonly/file", "old rootfs") ] dir;
      Eio.Path.symlink ~link_to:"../foreign" (dir / "link"));
  let old = Eio.Path.read_link (root / "out") in
  Eio_fs_sync.publish_generation ~durable:true (root / "out")
    (readonly [ ("readonly/file", "complete replacement") ]);
  Alcotest.(check string)
    "replacement is complete" "complete replacement"
    (Eio.Path.load (root / "out/readonly/file"));
  Alcotest.(check int)
    "the published directory retains its mode" 0o555
    ((Eio.Path.stat ~follow:false (root / "out/readonly")).perm land 0o777);
  Alcotest.(check bool)
    "old read-only generation is reclaimed" false
    (Eio.Path.is_directory (root / old));
  let actual = Eio.Path.stat ~follow:false (root / "foreign") in
  Alcotest.(check (pair int64 int64))
    "foreign directory identity is untouched" (foreign.dev, foreign.ino)
    (actual.dev, actual.ino);
  Alcotest.(check int)
    "foreign directory permissions are untouched" 0o500 (actual.perm land 0o777);
  Alcotest.(check string)
    "foreign link target is untouched" "outside the generation"
    (Eio.Path.load (root / "foreign/kept"))

(* Failed population can already have copied read-only directories. Removing
   those staged entries must leave the previous generation and modes intact. *)
let test_posix_readonly_failed_populate () =
  with_posix @@ fun root ->
  Eio_fs_sync.publish_generation ~durable:true (root / "out")
    (readonly [ ("readonly/file", "previous complete rootfs") ]);
  let target = Eio.Path.read_link (root / "out") in
  let before = Eio.Path.stat ~follow:false (root / target) in
  let entries = List.sort String.compare (Eio.Path.read_dir root) in
  (match
     Eio_fs_sync.publish_generation ~durable:true (root / "out") (fun dir ->
         readonly [ ("readonly/file", "never published") ] dir;
         failwith "fixture population failed")
   with
  | () -> Alcotest.fail "a failed population was published"
  | exception Failure why ->
      Alcotest.(check string)
        "the original error is preserved" "fixture population failed" why);
  Alcotest.(check (list string))
    "the read-only stage is fully removed" entries
    (List.sort String.compare (Eio.Path.read_dir root));
  Alcotest.(check string)
    "the exact old pointer remains" target
    (Eio.Path.read_link (root / "out"));
  let actual = Eio.Path.stat ~follow:false (root / target) in
  Alcotest.(check (pair int64 int64))
    "the previous generation identity remains" (before.dev, before.ino)
    (actual.dev, actual.ino);
  Alcotest.(check int)
    "old permissions are not changed before publication" 0o555
    ((Eio.Path.stat ~follow:false (root / "out/readonly")).perm land 0o777);
  Alcotest.(check string)
    "old content remains complete" "previous complete rootfs"
    (Eio.Path.load (root / "out/readonly/file"))

(* A fault wrapper opens files for reading without a barrier of their own: a
   durable publication through it syncs each file all the same. *)
let test_durable_through_a_fault_wrapper () =
  with_empty @@ fun fs ->
  let wrapped =
    Eio_fs.Chaos.Private.wrap Eio_fs.Chaos.Private.Schedule.empty fs
  in
  publish ~durable:true wrapped new_tree;
  Alcotest.check tree "published" (Some new_tree) (read_back (fs / "out"))

let suite =
  ( "generation",
    [
      Alcotest.test_case "replace, every crash point, drop" `Quick
        test_replace_drop;
      Alcotest.test_case "replace, every crash point, reorder" `Quick
        test_replace_reorder;
      Alcotest.test_case "first publication, every crash point" `Quick
        test_first_publication;
      Alcotest.test_case "previous generation reclaimed" `Quick
        test_previous_reclaimed;
      Alcotest.test_case "generation names" `Quick test_generation_names;
      Alcotest.test_case "foreign link target kept" `Quick
        test_foreign_target_kept;
      Alcotest.test_case "failed producer keeps the previous tree" `Quick
        test_failed_populate;
      Alcotest.test_case "plain directory refused" `Quick
        test_refuses_plain_directory;
      Alcotest.test_case "POSIX read-only generation reclaimed" `Quick
        test_posix_readonly_reclaimed;
      Alcotest.test_case "POSIX failed read-only population leaves no stage"
        `Quick test_posix_readonly_failed_populate;
      Alcotest.test_case "durable through a fault wrapper" `Quick
        test_durable_through_a_fault_wrapper;
    ] )
