(* [Eio_fs_sync.mkdirs ~perm p] creates [p] and its missing parents, and
   barriers every traversed directory entry, including existing ones, parent first. A
   barrier on the last directory alone is not enough: an entry of a directory
   whose own entry is not durable stays pending with it, so a file published
   durably under a fresh tree is lost with the tree.

   The crash model is the oracle: after the power cut the tree holds what a
   barrier committed and nothing else. *)

module Crash = Eio_fs.Chaos.Crash

let ( / ) = Eio.Path.( / )

let with_crash ctl fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn (Crash.wrap ctl env#fs)

let pending ctl =
  List.map (fun (u : Crash.unsynced) -> u.path) (Crash.unsynced ctl)

let is_directory p = Eio.Path.kind ~follow:false p = `Directory

(* A state directory created this boot and a record published durably in it:
   the record survives the power cut. *)
let test_a_fresh_tree_keeps_what_is_published_in_it () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio_fs_sync.mkdirs ~perm:0o700 (fs / "a" / "b" / "c");
  Alcotest.(check (list string))
    "every created entry is committed" [] (pending ctl);
  Eio_fs_sync.save (fs / "a" / "b" / "c" / "record") "data";
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the record survives the power cut" "data"
    (Eio.Path.load (post / "a" / "b" / "c" / "record"))

(* The control: the same workload through [Eio.Path.mkdirs], whose entries no
   barrier commits. The record's own publish barriers only its directory, so
   the cut takes the tree and the record with it. *)
let test_without_it_the_tree_is_lost () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdirs ~perm:0o700 (fs / "a" / "b" / "c");
  Eio_fs_sync.save (fs / "a" / "b" / "c" / "record") "data";
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool) "the tree is gone" false (is_directory (post / "a"))

(* A durable existing prefix is preserved, and its new child is committed. *)
let test_an_existing_prefix () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio_fs_sync.sync_dir fs;
  Eio_fs_sync.mkdirs ~perm:0o700 (fs / "a" / "b");
  Alcotest.(check (list string)) "the new entry is committed" [] (pending ctl);
  Crash.crash ctl;
  Alcotest.(check bool)
    "it survives the power cut" true
    (is_directory (Crash.post_crash ctl / "a" / "b"))

(* [exists_ok] as [Eio.Path.mkdirs] has it: a directory already there is
   refused by default and accepted with it. *)
let test_exists_ok () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio_fs_sync.mkdirs ~perm:0o700 (fs / "a");
  (match Eio_fs_sync.mkdirs ~perm:0o700 (fs / "a") with
  | () -> Alcotest.fail "an existing directory was accepted without exists_ok"
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 (fs / "a");
  Alcotest.(check bool) "it is still a directory" true (is_directory (fs / "a"))

(* A visible prefix can be from a writer whose parent barrier failed. *)
let test_unsynced_prefix () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdirs ~perm:0o755 (fs / "a" / "b");
  Eio_fs_sync.mkdirs ~perm:0o700 (fs / "a" / "b" / "c");
  Eio_fs_sync.save (fs / "a" / "b" / "c" / "record") "prefix retained";
  Crash.crash ctl;
  Alcotest.(check string)
    "all existing ancestors survive" "prefix retained"
    (Eio.Path.load (Crash.post_crash ctl / "a" / "b" / "c" / "record"))

let test_barrier_retry () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  let target = fs / "a" in
  let parent, _ = Option.get (Eio.Path.split target) in
  let _, path = parent in
  let module Chaos = Eio_fs.Chaos in
  let schedule =
    Chaos.Schedule.scripted
      [
        { trigger = Op_on_path { kind = `Sync; path }; fault = Sync_error `Eio };
      ]
  in
  let faulted = Chaos.wrap schedule fs in
  (match Eio_fs_sync.mkdirs ~perm:0o700 (faulted / "a") with
  | () -> Alcotest.fail "the directory barrier failure was swallowed"
  | exception Eio.Io _ -> ());
  Alcotest.(check bool)
    "mkdir succeeded before the barrier failed" true (is_directory target);
  Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 (faulted / "a");
  Eio_fs_sync.save (faulted / "a" / "record") "retry retained";
  Crash.crash ctl;
  Alcotest.(check string)
    "retry establishes the existing entry's durability" "retry retained"
    (Eio.Path.load (Crash.post_crash ctl / "a" / "record"))

let test_racing_prefix () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio_fs_sync.sync_parent (fs / "a");
  let _, parent_path = fs / "a" in
  let raced = ref false in
  let module Chaos = Eio_fs.Chaos in
  let schedule =
    Chaos.Schedule.observing
      (fun (op : Chaos.Op.t) ->
        if op.kind = `Stat && String.equal op.path parent_path && not !raced
        then (
          raced := true;
          Eio.Path.mkdir ~perm:0o755 (fs / "a" / "b")))
      Chaos.Schedule.empty
  in
  let observed = Chaos.wrap schedule fs in
  Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 (observed / "a" / "b");
  Alcotest.(check bool) "the other writer created the entry" true !raced;
  Alcotest.(check int)
    "an existing directory's mode is unchanged" 0o755
    (Eio.Path.stat ~follow:true (fs / "a" / "b")).perm;
  Eio_fs_sync.save (observed / "a" / "b" / "record") "race retained";
  Crash.crash ctl;
  Alcotest.(check string)
    "the competing entry was barriered" "race retained"
    (Eio.Path.load (Crash.post_crash ctl / "a" / "b" / "record"))

let test_non_directory () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio_fs_sync.save (fs / "file") "original file";
  List.iter
    (fun target ->
      match Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 target with
      | () -> Alcotest.fail "an existing file was accepted as a directory"
      | exception Eio.Io _ -> ())
    [ fs / "file"; fs / "file" / "child" ];
  Alcotest.(check string)
    "the existing file is unchanged" "original file"
    (Eio.Path.load (fs / "file"))

let test_symbolic_link () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio_fs_sync.mkdirs ~perm:0o700 (fs / "target");
  Eio.Path.symlink ~link_to:"target" (fs / "link");
  List.iter
    (fun path ->
      match Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 path with
      | () -> Alcotest.fail "a symlink's target durability was assumed"
      | exception Eio.Io _ -> ())
    [ fs / "link"; fs / "link/"; fs / "link//"; fs / "link" / "child" ];
  Alcotest.(check string)
    "the symbolic link is unchanged" "target"
    (Eio.Path.read_link (fs / "link"));
  Alcotest.(check (list string))
    "nothing was created through the link" []
    (Eio.Path.read_dir (fs / "target"))

let test_posix_symbolic_link () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let root =
    fs / Filename.get_temp_dir_name () / Eio_fs_sync.fresh_name "mkdirs"
  in
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree root) @@ fun () ->
  let parent, _ = Option.get (Eio.Path.split root) in
  Eio_fs_unix.sync_dir parent;
  Eio.Path.mkdir ~perm:0o700 (root / "target");
  Eio.Path.symlink ~link_to:"target" (root / "link");
  Eio_fs_unix.sync_dir root;
  Eio.Path.with_open_dir root @@ fun anchor ->
  let syncs = ref 0 in
  let module Chaos = Eio_fs.Chaos in
  let schedule =
    Chaos.Schedule.observing
      (fun (op : Chaos.Op.t) -> if op.kind = `Sync then incr syncs)
      Chaos.Schedule.empty
  in
  let observed = Chaos.wrap schedule anchor in
  List.iter
    (fun name ->
      match
        Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 (observed / name)
      with
      | () -> Alcotest.failf "the POSIX symlink %S was followed" name
      | exception Eio.Io _ -> ())
    [ "link"; "link/"; "link//"; "link/child" ];
  Alcotest.(check int) "refusal happens before any directory barrier" 0 !syncs;
  Alcotest.(check (list string))
    "nothing was created through the POSIX link" []
    (Eio.Path.read_dir (root / "target"))

let test_parent_order () =
  List.iter
    (fun depth ->
      let ctl = Crash.v ~policy:Drop_unsynced () in
      with_crash ctl @@ fun fs ->
      let target, parents =
        List.fold_left
          (fun (path, parents) index ->
            let child = path / string_of_int index in
            let parent, _ = Option.get (Eio.Path.split child) in
            (child, snd parent :: parents))
          (fs, []) (List.init depth Fun.id)
      in
      Eio.Path.mkdirs ~perm:0o755 target;
      let syncs = ref [] in
      let module Chaos = Eio_fs.Chaos in
      let schedule =
        Chaos.Schedule.observing
          (fun (op : Chaos.Op.t) ->
            if op.kind = `Sync then syncs := op.path :: !syncs)
          Chaos.Schedule.empty
      in
      let _, target_path = target in
      let observed = Chaos.wrap schedule fs in
      Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 (fst observed, target_path);
      Alcotest.(check (list string))
        "exactly one barrier per traversed entry, parent first"
        (List.rev parents) (List.rev !syncs);
      Crash.crash ctl;
      let post = Crash.post_crash ctl in
      Alcotest.(check bool)
        "the complete path survives" true
        (is_directory (fst post, target_path)))
    [ 10; 100 ]

let test_capability_root () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio_fs_sync.mkdirs ~exists_ok:true ~perm:0o700 fs;
  Alcotest.(check bool)
    "the existing capability root is accepted" true (is_directory fs)

let test_borrowed_root () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio_fs_sync.mkdirs ~perm:0o700 (fs / "anchor");
  Eio.Path.with_open_dir (fs / "anchor") (fun anchor ->
      let syncs = ref [] in
      let module Chaos = Eio_fs.Chaos in
      let schedule =
        Chaos.Schedule.observing
          (fun (op : Chaos.Op.t) ->
            if op.kind = `Sync then syncs := op.path :: !syncs)
          Chaos.Schedule.empty
      in
      let local = Chaos.wrap schedule anchor in
      Eio_fs_sync.mkdirs ~perm:0o700 (local / "child" / "leaf");
      Alcotest.(check (list string))
        "barriers stay below the borrowed root" [ ""; "child" ]
        (List.rev !syncs);
      Eio_fs_sync.save (local / "child" / "leaf" / "record") "confined");
  Crash.crash ctl;
  Alcotest.(check string)
    "the confined record survives" "confined"
    (Eio.Path.load
       (Crash.post_crash ctl / "anchor" / "child" / "leaf" / "record"))

let suite =
  ( "eio_fs_sync",
    [
      Alcotest.test_case "a fresh tree keeps what is published in it" `Quick
        test_a_fresh_tree_keeps_what_is_published_in_it;
      Alcotest.test_case "without it the tree is lost" `Quick
        test_without_it_the_tree_is_lost;
      Alcotest.test_case "an existing prefix" `Quick test_an_existing_prefix;
      Alcotest.test_case "exists_ok" `Quick test_exists_ok;
      Alcotest.test_case "capability root" `Quick test_capability_root;
      Alcotest.test_case "borrowed durable root" `Quick test_borrowed_root;
      Alcotest.test_case "an unsynced existing prefix" `Quick
        test_unsynced_prefix;
      Alcotest.test_case "barrier failure and retry" `Quick test_barrier_retry;
      Alcotest.test_case "another writer creates the prefix" `Quick
        test_racing_prefix;
      Alcotest.test_case "a non-directory is refused" `Quick test_non_directory;
      Alcotest.test_case "symbolic links are refused" `Quick test_symbolic_link;
      Alcotest.test_case "POSIX trailing-slash links are refused" `Quick
        test_posix_symbolic_link;
      Alcotest.test_case "parent barriers at two path sizes" `Quick
        test_parent_order;
    ] )
