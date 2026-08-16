(* [Eio_fs_sync.sync_parent p] barriers the directory that holds [p]'s entry:
   the typed answer to "make this name durable". It walks [Eio.Path.split], so
   the parent it syncs keeps the directory capability [p] was expressed
   against, where taking the parent through the path's string half
   ([Filename.dirname]) drops that confinement. A path with no parent is the
   root of the capability itself, and its barrier is [sync_dir] of that path:
   there is nothing above it a confined path may reach for.

   The crash model is the oracle. [Eio.File.sync] makes a file's bytes durable
   and says nothing about the directory entry that names it, so whether the
   file is there after the power cut is exactly whether the parent's barrier
   ran, and [Crash.unsynced] shows the journal between the two. *)

module Crash = Eio_fs.Chaos.Crash

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int
let absent p = Eio.Path.kind ~follow:false p = `Not_found

(* An in-memory tree with durability tracked by [ctl]: after a crash it holds
   what a barrier committed and nothing else. *)
let with_crash ctl fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn (Crash.wrap ctl env#fs)

(* Write [content] and make the bytes durable, which leaves the entry naming
   the file pending -- the state a commit protocol is in when it reaches for
   the directory barrier. *)
let save_synced path content =
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) path (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string content ];
      Eio.File.sync file)

(* The paths the journal still holds, in program order. *)
let pending ctl =
  List.map (fun (u : Crash.unsynced) -> u.path) (Crash.unsynced ctl)

(* The barrier a store issues after publishing a file: the directory that holds
   the new entry, reached from the file's own path. *)
let test_commits_the_entry_it_names () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Alcotest.(check (list string)) "the mkdir is pending" [ "d" ] (pending ctl);
  Eio_fs_sync.sync_dir fs;
  Alcotest.(check (list string))
    "the root's barrier made the directory durable" [] (pending ctl);
  save_synced (fs / "d" / "f") "data";
  Alcotest.(check (list string))
    "the file's own sync left the entry naming it pending" [ "d/f" ]
    (pending ctl);
  Eio_fs_sync.sync_parent (fs / "d" / "f");
  Alcotest.(check (list string))
    "the parent's barrier committed the creation" [] (pending ctl);
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the published file survives the power cut" "data"
    (Eio.Path.load (post / "d" / "f"))

(* The same workload without the barrier: the bytes were durable, the name was
   not, and the power cut takes the file with the entry that named it. *)
let test_without_it_the_entry_is_lost () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio_fs_sync.sync_dir fs;
  save_synced (fs / "d" / "f") "data";
  Alcotest.(check (list string))
    "the creation is still pending at the crash" [ "d/f" ] (pending ctl);
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the file is gone after the power cut" true
    (absent (post / "d" / "f"));
  Alcotest.(check bool)
    "the directory whose barrier did run is not" true
    (Eio.Path.kind ~follow:false (post / "d") = `Directory)

(* A single-component path: its parent is the root the capability was expressed
   against ([Eio.Path.split (fs, "top")] is [Some ((fs, ""), "top")], and a
   backend answers that empty path with the directory itself), so the barrier
   is the root's. *)
let test_single_component_path () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  save_synced (fs / "top") "data";
  Alcotest.(check (list string))
    "the creation is pending" [ "top" ] (pending ctl);
  Eio_fs_sync.sync_parent (fs / "top");
  Alcotest.(check (list string))
    "the root's barrier committed it" [] (pending ctl);
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the top-level file survives the power cut" "data"
    (Eio.Path.load (post / "top"))

(* The capability root has no parent at all. [sync_parent] on it is the root's
   own barrier -- not a raise, and not a silent no-op: a store that barriers
   the directory it was handed must get that directory synced. *)
let test_capability_root () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio_fs_sync.sync_parent fs;
  Alcotest.(check (list string)) "the root's own barrier ran" [] (pending ctl);
  Crash.crash ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the entry it committed survives the power cut" true
    (Eio.Path.kind ~follow:false (post / "a") = `Directory)

let suite =
  ( "eio_fs_sync",
    [
      Alcotest.test_case "commits the entry it names" `Quick
        test_commits_the_entry_it_names;
      Alcotest.test_case "without it the entry is lost" `Quick
        test_without_it_the_entry_is_lost;
      Alcotest.test_case "single-component path" `Quick
        test_single_component_path;
      Alcotest.test_case "capability root" `Quick test_capability_root;
    ] )
