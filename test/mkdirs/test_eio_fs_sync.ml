(* [Eio_fs_sync.mkdirs ~perm p] creates [p] and its missing parents, and
   barriers the directory holding each entry it creates, outermost first. A
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

(* Only what it creates is barriered and created: a prefix already there is
   left as it is, and the components below it are committed. *)
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

let suite =
  ( "eio_fs_sync",
    [
      Alcotest.test_case "a fresh tree keeps what is published in it" `Quick
        test_a_fresh_tree_keeps_what_is_published_in_it;
      Alcotest.test_case "without it the tree is lost" `Quick
        test_without_it_the_tree_is_lost;
      Alcotest.test_case "an existing prefix" `Quick test_an_existing_prefix;
      Alcotest.test_case "exists_ok" `Quick test_exists_ok;
    ] )
