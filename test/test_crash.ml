(* The crash model: what a power cut leaves of files and directories, the
   barriers that make them durable, on memory and on the real filesystem,
   and the authority the wrapper keeps. *)

module Chaos = Eio_fs_chaos
module Op = Chaos.Op
module Crash = Chaos.Private.Crash
open Chaos_helpers

let ( / ) = Eio.Path.( / )

(* {2 Crash / durability model} *)

(* A controller that was never wrapped has no image to rebuild, so a crash
   is refused, and the refusal leaves it uncrashed. *)
let test_crash_unwrapped_refused () =
  let ctl = Crash.v () in
  let refused f =
    match f () with _ -> false | exception Invalid_argument _ -> true
  in
  Alcotest.(check bool) "crash refused" true (refused (fun () -> Crash.cut ctl));
  Alcotest.(check bool) "not crashed" false (Crash.is_crashed ctl);
  Alcotest.(check bool)
    "post_crash refused" true
    (refused (fun () -> ignore (Crash.post_crash ctl)))

(* A write with no sync after it is lost in a crash, and so is the file: its
   creation is an uncommitted directory operation. *)
let test_crash_unsynced_lost () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "uncommitted file gone after crash" true
    (absent (post / "img"))

(* A write followed by a sync survives a crash. The metadata barrier is what
   makes the new file's name durable; the file sync covers its contents. Every
   scenario below that creates a file and expects to find it again needs both,
   as a store writing a new file on real hardware does. *)
let test_crash_synced_survives () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced write survives crash" "data"
    (Eio.Path.load (post / "img"))

(* The barrier is respected: everything before a sync survives, the unsynced
   suffix is dropped. Nothing before the barrier can be lost. *)
let test_crash_barrier_respected () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced prefix survived, unsynced suffix lost" "AAAA"
    (Eio.Path.load (post / "img"))

(* Even when a later, un-synced write is kept, the synced data before the
   barrier is never reordered away: it stays durable underneath. *)
let test_crash_barrier_not_reordered () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced data present under a kept unsynced suffix" "AAAABBBB"
    (Eio.Path.load (post / "img"))

(* The missing-barrier hazard in miniature: a data block is written, then a
   pointer to it, with NO sync between. A crash that keeps the pointer write but
   drops the data write leaves a pointer referencing bytes that never became
   durable. *)
let test_crash_pointer_without_data () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun u -> u.file_offset = 0)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      (* data block first ... *)
      Eio.File.pwrite_all file ~file_offset:(i63 512)
        [ Cstruct.of_string (String.make 512 'D') ];
      (* ... then the pointer, with no fsync between: the missing barrier *)
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string "PTR->512" ]);
  let post = Crash.post_crash ctl in
  let img = Eio.Path.load (post / "img") in
  Alcotest.(check string)
    "generation pointer landed" "PTR->512" (String.sub img 0 8);
  Alcotest.(check int)
    "referenced data block never became durable" 8 (String.length img)

(* Same seed reproduces the same post-crash image. *)
let reorder_image seed =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let ctl = Crash.v ~policy:(Crash.Reorder_unsynced seed) () in
  let fs = Crash.wrap ctl env#fs in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img") (fun file ->
      for i = 0 to 7 do
        Eio.File.pwrite_all file
          ~file_offset:(i63 (i * 8))
          [ Fmt.kstr Cstruct.of_string "%08d" i ]
      done);
  let post = Crash.post_crash ctl in
  (* the creation itself may not survive the seeded journal cut *)
  try Eio.Path.load (post / "img") with Eio.Io _ -> "<absent>"

let test_crash_determinism () =
  Alcotest.(check string)
    "same seed same post-crash image" (reorder_image 7) (reorder_image 7);
  (* non-vacuous: the reordered image is a genuine perturbation of the clean,
     fully-written result *)
  let full = String.concat "" (List.init 8 (fun i -> Fmt.str "%08d" i)) in
  Alcotest.(check bool)
    "seeded drop/reorder perturbs the clean image" true
    (reorder_image 7 <> full)

(* {2 Crash rewrite of read-only and subtree-staged files}

   A store that stages immutable artefacts writes them read-only (0o444) and
   through short-lived subtree capabilities. The crash model must rewrite such
   files all the same: the post-crash image reflects the durability barrier,
   the file keeps its mode, and a staging subtree closed long before the crash
   does not strand the file. *)

(* A file already on disk when the model wraps the tree is as durable as its
   name: nothing the wrapper saw created it. Bytes written through the wrapper
   and synced survive the cut whether or not a metadata barrier came after the
   open, since the open changed no directory entry (fsync(2) commits a file's
   data; its name was never pending). [policy] picks which post-crash image
   the model rebuilds from: [Drop_unsynced] the last committed one, and
   [Survive] every journal entry's, here a mkdir recorded before the open. *)
let preexisting_file_synced_after_wrap ~policy ~pending_entry () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let raw = env#fs in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "journal") "first";
  let ctl = Crash.v ~policy () in
  let fs = Crash.wrap ctl raw in
  if pending_entry then Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`If_missing 0o644) (fs / "journal")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 5)
        [ Cstruct.of_string "second" ];
      Eio.File.sync file);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced bytes of a pre-existing file survive" "firstsecond"
    (Eio.Path.load (post / "journal"))

(* A read-only file whose writes were all synced survives a crash byte-for-byte
   with its mode intact. The rewrite must not fail with EACCES just because the
   file cannot be reopened for writing. *)
let test_crash_readonly_synced () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o444) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  sync_dirs fs;
  (* an unsynced write on a second file forces the crash to rewrite state *)
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "scratch")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "tmp" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "synced read-only file survives" "data"
    (Eio.Path.load (post / "img"));
  Alcotest.(check int)
    "read-only mode preserved" 0o444
    (Eio.Path.stat ~follow:true (post / "img")).perm

(* A read-only file with an unsynced tail is rewritten to its synced prefix:
   the model grants itself write access for the rewrite and restores the mode
   after. *)
let test_crash_readonly_unsynced_tail () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o444) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "read-only file rewritten to its synced prefix" "AAAA"
    (Eio.Path.load (post / "img"));
  Alcotest.(check int)
    "read-only mode preserved after rewrite" 0o444
    (Eio.Path.stat ~follow:true (post / "img")).perm

(* A file staged through a short-lived subtree ([with_open_dir]) is rewritten
   through the long-lived wrap root: the subtree fd is closed well before the
   crash fires and must not strand the file. *)
let test_crash_closed_subtree () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_dir (fs / "stage") (fun stage ->
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
        Eio.Path.(stage / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ]));
  sync_dirs fs;
  (* the staging subtree is closed here; only now does the crash fire *)
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "subtree-staged file rewritten through the root" "AAAA"
    (Eio.Path.load (post / "stage" / "img"))

(* The composed hazard, as an OTA release store stages images: a read-only
   file created inside a short-lived staging subtree. *)
let test_crash_readonly_in_closed_subtree () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_dir (fs / "stage") (fun stage ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o444)
        Eio.Path.(stage / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "data" ];
          Eio.File.sync file;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "junk" ]));
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "read-only subtree-staged file rewritten" "data"
    (Eio.Path.load (post / "stage" / "img"));
  Alcotest.(check int)
    "mode preserved" 0o444
    (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm

(* {2 Directory-operation durability}

   Directory operations (create, mkdir, unlink, rmdir, symlink, rename) are
   journalled in program order and commit on a sync of their directory, never
   on a file sync. A crash rolls back the uncommitted suffix, whole. *)

(* An unlink committed by the metadata barrier stays deleted: the crash must
   not resurrect the file from its tracked durable content. *)
let test_crash_committed_unlink_stays_deleted () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.unlink (fs / "f");
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "committed unlink stays deleted" true
    (absent (post / "f"))

(* A file sync commits that file's data, not a directory's entries: an
   unlink pending when another file is synced rolls back. *)
let test_crash_file_sync_leaves_metadata_pending () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  sync_dirs fs;
  Eio.Path.unlink (fs / "f");
  (* the barrier below is on a different file, so it must not commit the
     unlink above *)
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "other") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ];
      Eio.File.sync file);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "another file's sync did not commit the unlink" "data"
    (Eio.Path.load (post / "f"))

(* An unlink that never committed rolls back: the file reappears with its
   durable content, exactly as a lost directory update leaves it on disk. *)
let test_crash_uncommitted_unlink_rolls_back () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  sync_dirs fs;
  Eio.Path.unlink (fs / "f");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "uncommitted unlink rolled back" "data"
    (Eio.Path.load (post / "f"))

(* A rename into place with no barrier after it reverts whole: the file is
   back at the temporary path and the final path is absent. *)
let test_crash_uncommitted_rename_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  sync_dirs fs;
  Eio.Path.rename (fs / "tmp") (fs / "final");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "file back at the temporary path" "NEW"
    (Eio.Path.load (post / "tmp"));
  Alcotest.(check bool)
    "final path absent after reverted swap" true
    (absent (post / "final"))

(* The same protocol with the barrier: a sync of the parent directory after
   the rename pins the swap. *)
let test_crash_sync_dir_commits_rename () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  Eio.Path.rename (fs / "tmp") (fs / "final");
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "committed swap survives" "NEW"
    (Eio.Path.load (post / "final"));
  Alcotest.(check bool)
    "temporary path gone after committed swap" true
    (absent (post / "tmp"))

(* A reverted rename restores the entry it replaced: the old target contents
   come back, and the source keeps its bytes at its original path. *)
let test_crash_reverted_rename_restores_victim () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "final") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "OLD" ];
      Eio.File.sync file);
  sync_dirs fs;
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  sync_dirs fs;
  Eio.Path.rename (fs / "tmp") (fs / "final");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "clobbered target restored" "OLD"
    (Eio.Path.load (post / "final"));
  Alcotest.(check string)
    "source back at its path" "NEW"
    (Eio.Path.load (post / "tmp"))

(* An uncommitted mkdir takes its whole subtree with it: the directory and the
   file created inside both vanish, since neither entry ever became durable. *)
let test_crash_uncommitted_mkdir_vanishes () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool) "uncommitted directory gone" true (absent (post / "d"))

(* Barriers on the root and on the new directory make the mkdir and the
   creation inside it durable, and the file's synced bytes come back with
   them. Syncing the file alone would leave both directory entries pending, and
   the crash would take the whole subtree with them. *)
let test_crash_sync_dir_commits_mkdir () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ];
      Eio.File.sync file);
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "metadata barrier committed the enclosing mkdir" "x"
    (Eio.Path.load (post / "d" / "f"))

(* An uncommitted symlink vanishes at the crash. *)
let test_crash_uncommitted_symlink_vanishes () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.symlink ~link_to:"target" (fs / "lnk");
  let post = Crash.post_crash ctl in
  Alcotest.(check bool) "uncommitted symlink gone" true (absent (post / "lnk"))

(* A symbolic link replaced through a temporary name reverts whole when
   uncommitted: the old target is served, and the temporary is gone. *)
let test_crash_symlink_swap_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.symlink ~link_to:"releases/a" (fs / "current");
  sync_dirs fs;
  Eio.Path.symlink ~link_to:"releases/b" (fs / ".tmp");
  Eio.Path.rename (fs / ".tmp") (fs / "current");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "old pointer target restored" "releases/a"
    (Eio.Path.read_link (post / "current"));
  Alcotest.(check bool) "temporary symlink gone" true (absent (post / ".tmp"))

(* An uncommitted rmdir rolls back: the (empty) directory reappears. *)
let test_crash_uncommitted_rmdir_restored () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  sync_dirs fs;
  Eio.Path.rmdir (fs / "d");
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "uncommitted rmdir rolled back" true
    (Eio.Path.kind ~follow:false (post / "d") = `Directory)

(* A directory rename carries its tracked children: uncommitted, the subtree
   reverts to the staging path; committed, it is at the final path. *)
let test_crash_dir_rename_uncommitted_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "stage" / "img")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  sync_dirs fs;
  Eio.Path.rename (fs / "stage") (fs / "final");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "subtree back at the staging path" "data"
    (Eio.Path.load (post / "stage" / "img"));
  Alcotest.(check bool) "final directory absent" true (absent (post / "final"))

let test_crash_dir_rename_committed_moves () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "stage" / "img")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.rename (fs / "stage") (fs / "final");
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "subtree at the final path" "data"
    (Eio.Path.load (post / "final" / "img"));
  Alcotest.(check bool)
    "staging directory absent" true
    (absent (post / "stage"))

(* A subtree held open across the rename of its directory follows the
   directory, as a descriptor does: a file opened through it afterwards is
   the file under the new name, and the model tracks it there. A Drop_unsynced
   crash so drops that file's unsynced write and leaves the new entry at the
   old name to its own bytes. *)
let test_crash_held_subtree_follows_a_rename () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  sync_dirs fs;
  Eio.Switch.run (fun sw ->
      let stage = Eio.Path.open_dir ~sw (fs / "stage") in
      Eio.Path.rename (fs / "stage") (fs / "final");
      Eio.Path.mkdir ~perm:0o755 (fs / "stage");
      sync_dirs fs;
      Eio.Path.with_open_out ~create:(`Exclusive 0o644) (stage / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "synced" ];
          Eio.File.sync file;
          sync_dirs fs;
          Eio.Path.save ~create:(`Exclusive 0o644)
            (fs / "stage" / "img")
            "other";
          Eio.Path.with_open_out ~create:`Never
            (fs / "stage" / "img")
            Eio.File.sync;
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "LOST!!" ]);
      Eio.Path.mkdir ~perm:0o755 (fs / "later");
      sync_dirs fs);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the held subtree's file keeps what it synced" "synced"
    (Eio.Path.load (post / "final" / "img"));
  Alcotest.(check string)
    "the entry at the old name keeps its own bytes" "other"
    (Eio.Path.load (post / "stage" / "img"))

(* Metadata survival is a prefix of program order: refusing an early directory
   op drops everything after it, even ops the predicate would accept, because
   a journal never commits out of order. *)
(* A rename over a file whose new data was never synced: the default policy
   shows only the state at the last barriers, and seeds show the rename
   landing before the data. *)
let rename_before_data policy =
  let ctl = Crash.v ?policy () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio_fs.Sync.save (fs / "state") "v1";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "state.tmp") "v2";
  Eio.Path.rename (fs / "state.tmp") (fs / "state");
  let post = Crash.post_crash ctl in
  Eio.Path.load (post / "state")

let test_crash_default_is_one_state () =
  Alcotest.(check string) "the last barrier" "v1" (rename_before_data None);
  let seeds =
    List.init 100 (fun seed ->
        rename_before_data (Some (Crash.Reorder_unsynced seed)))
  in
  Alcotest.(check bool) "some seed loses the data" true (List.mem "" seeds)

(* The same seed and workload rebuild the same image, temporaries of
   [Eio_fs.Sync.save] included. *)
let image_of_seed seed =
  let ctl = Crash.v ~policy:(Reorder_unsynced seed) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio_fs.Sync.save ~barrier:Eio_fs.Sync.Barrier.none (fs / "state") "v1";
  let post = Crash.post_crash ctl in
  List.sort String.compare (Eio.Path.read_dir post)

let test_crash_seed_reproduces_save () =
  for seed = 0 to 50 do
    Alcotest.(check (list string))
      (Fmt.str "seed %d" seed) (image_of_seed seed) (image_of_seed seed)
  done

(* The surviving prefix of the journal has a uniform length: the last of
   twelve pending directory operations survives in about one seed in
   thirteen. *)
let test_crash_reorder_prefix_uniform () =
  let full = ref 0 in
  for seed = 0 to 999 do
    let ctl = Crash.v ~policy:(Crash.Reorder_unsynced seed) () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    for i = 1 to 12 do
      Eio.Path.mkdir ~perm:0o755 (fs / Fmt.str "d%02d" i)
    done;
    let post = Crash.post_crash ctl in
    if not (absent (post / "d12")) then incr full
  done;
  Alcotest.(check bool)
    (Fmt.str "late entries survive (%d/1000)" !full)
    true
    (!full > 40 && !full < 120)

let test_crash_survive_prefix_rule () =
  let pred (u : Crash.unsynced) =
    match u.kind with Crash.Mkdir -> false | _ -> true
  in
  let ctl = Crash.v ~policy:(Crash.Survive pred) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "rejected mkdir cuts the journal before the accepted create" true
    (absent (post / "d"))

(* A file whose bytes were synced and whose name a barrier on its directory
   made durable keeps its bytes, even behind an older operation in another
   directory still pending: the image a crash rebuilds from that pending
   entry holds the file, so it must hold the file's bytes too. *)
let test_crash_synced_file_behind_older_pending_entry () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d1");
  Eio.Path.mkdir ~perm:0o755 (fs / "d2");
  sync_dirs fs;
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "d2" / "b") "b";
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d1" / "a")
    (fun f ->
      Eio.Flow.copy_string "a" f;
      Eio.File.sync f);
  Eio.Path.sync_dir (fs / "d1");
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "a's bytes" "a" (Eio.Path.load (post / "d1" / "a"));
  Alcotest.(check string) "b's bytes" "b" (Eio.Path.load (post / "d2" / "b"))

(* After the crash fires nothing reaches the store: a directory operation
   through the dead wrapper fails as a read or a write does, and the image is
   the one the crash left. A process killed at that point does nothing more
   either, so a cleanup handler the failure runs cannot undo the crash. *)
let test_crash_stops_directory_ops () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "x") "x";
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.mkdir ~perm:0o755 (fs / "e");
  Crash.cut ctl;
  let refused label f =
    Alcotest.(check bool)
      label true
      (match f () with () -> false | exception Eio.Io _ -> true)
  in
  refused "unlink fails" (fun () -> Eio.Path.unlink (fs / "x"));
  refused "rmdir fails" (fun () -> Eio.Path.rmdir (fs / "e"));
  refused "rename fails" (fun () -> Eio.Path.rename (fs / "d") (fs / "moved"));
  refused "mkdir fails" (fun () -> Eio.Path.mkdir ~perm:0o755 (fs / "f"));
  refused "symlink fails" (fun () -> Eio.Path.symlink ~link_to:"x" (fs / "l"));
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "x kept" "x" (Eio.Path.load (post / "x"));
  Alcotest.(check (list string))
    "the image the crash left" [ "d"; "e"; "x" ]
    (List.sort String.compare (Eio.Path.read_dir post))

(* The pending-change listing names directory operations in program order. *)
let test_crash_unsynced_lists_dir_ops () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "xyz" ]);
  let kinds = List.map (fun u -> u.Crash.kind) (Crash.unsynced ctl) in
  Alcotest.(check bool)
    "journal lists mkdir, create, data in program order" true
    (kinds = [ Crash.Mkdir; Crash.Create; Crash.Data ])

(* {2 The directory barrier is scoped to one directory}

   A directory barrier through the crash wrapper commits the entries of that
   directory only: not its own entry in its parent, not a sibling's, not an
   entry one level down. *)

(* Syncing a directory does not commit that directory's own name: [a]'s entry
   lives in the root, and only the root's barrier makes it durable. The sibling
   [b] is pending for the same reason. *)
let test_crash_dir_sync_leaves_its_own_entry_pending () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio.Path.sync_dir (fs / "a");
  Crash.cut ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the barriered directory's own entry stayed pending" true
    (absent (post / "a"));
  Alcotest.(check bool) "and so did its sibling" true (absent (post / "b"))

(* The entries the barrier does commit are the ones inside it: with [a] made
   durable by the root's barrier, syncing [a] commits the file created in [a]
   and leaves [b], created in the root, pending. *)
let test_crash_dir_sync_commits_its_own_entries () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.sync_dir fs;
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "a" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.sync_dir (fs / "a");
  Crash.cut ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the entry created in the barriered directory survives" "data"
    (Eio.Path.load (post / "a" / "f"));
  Alcotest.(check bool)
    "the sibling created in the root did not" true
    (absent (post / "b"))

(* The root's barrier commits the root's entries, both of them: scoping the
   commit must not shrink it to one path. *)
let test_crash_root_sync_commits_both_siblings () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio.Path.sync_dir fs;
  Crash.cut ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "first sibling durable" true
    (Eio.Path.kind ~follow:false (post / "a") = `Directory);
  Alcotest.(check bool)
    "second sibling durable" true
    (Eio.Path.kind ~follow:false (post / "b") = `Directory)

(* One level, not the whole subtree: syncing [a] commits the mkdir of [a/sub],
   whose entry is in [a], and not the file created inside [a/sub], whose entry
   is in [a/sub]. A store that barriers the top of a mkdirs and expects the
   bottom to be durable is the bug the model has to show. *)
let test_crash_dir_sync_does_not_reach_a_grandchild () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.sync_dir fs;
  Eio.Path.mkdir ~perm:0o755 (fs / "a" / "sub");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "a" / "sub" / "g")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.sync_dir (fs / "a");
  Crash.cut ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the child directory, an entry of the barriered one, is durable" true
    (Eio.Path.kind ~follow:false (post / "a" / "sub") = `Directory);
  Alcotest.(check bool)
    "the file one level further down is not" true
    (absent (post / "a" / "sub" / "g"))

(* A streaming write lands at the descriptor's offset, which a seek moves,
   and through an append descriptor at the end of the file. The journal logs
   each write where it landed, so a crash that keeps them replays them
   there. *)
let streaming_writes fs =
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun file ->
      ignore (Eio.File.seek file (i63 2) `Set : Optint.Int63.t);
      Eio.Flow.copy_string "XY" file);
  Eio.Path.with_open_out ~append:true ~create:`Never (fs / "f") (fun file ->
      Eio.Flow.copy_string "gh" file)

let test_crash_streaming_write_offsets () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.Flow.copy_string "abcdef" file;
      Eio.File.sync file);
  sync_dirs fs;
  streaming_writes fs;
  Alcotest.(check (list (pair int int)))
    "logged where they landed"
    [ (2, 2); (6, 2) ]
    (List.map
       (fun u -> (u.Crash.file_offset, u.Crash.length))
       (Crash.unsynced ctl));
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "replayed where they landed" "abXYefgh"
    (Eio.Path.load (post / "f"))

(* The schedule is told the same offsets. *)
let test_streaming_write_offsets () =
  let offsets = ref [] in
  let hook (op : Op.t) =
    if op.kind = `Write then offsets := op.offset :: !offsets;
    None
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "abcdef";
  streaming_writes chaos;
  Alcotest.(check (list int)) "write offsets" [ 2; 6 ] (List.rev !offsets);
  Alcotest.(check string) "the file" "abXYefgh" (Eio.Path.load (raw / "f"))

(* A truncation is a change of its own kind, not a write: a survival
   predicate keeps it and drops the write that follows it at the same
   offset. *)
let test_crash_truncation_kind () =
  let ctl =
    Crash.v ~policy:(Crash.Survive (fun u -> u.Crash.kind <> Crash.Data)) ()
  in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.Flow.copy_string "abc" file;
      Eio.File.sync file);
  sync_dirs fs;
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun file ->
      Eio.File.truncate file (i63 1);
      Eio.File.pwrite_all file ~file_offset:(i63 1) [ Cstruct.of_string "Z" ]);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the truncation kept, the write dropped" "a"
    (Eio.Path.load (post / "f"))

(* A barrier on one directory leaves a rename in another pending, so a file
   renamed after its name was made durable is still at its old name in the
   durable image, and that is where a crash puts its synced bytes. *)
let test_crash_dir_sync_keeps_a_pending_rename () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "x");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string "synced" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "LOST" ]);
  sync_dirs fs;
  Eio.Path.rename (fs / "f") (fs / "g");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "x" / "h") "h";
  Eio.Path.sync_dir (fs / "x");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the synced bytes, at the durable name" "synced"
    (Eio.Path.load (post / "f"));
  Alcotest.(check bool) "the pending name is gone" true (absent (post / "g"))

(* The same with a rename that replaces a file: until a barrier on their
   directory, both files keep their own name and their own synced bytes. *)
let test_crash_dir_sync_keeps_a_pending_replacement () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "x");
  let synced name content =
    Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / name) (fun file ->
        Eio.Flow.copy_string content file;
        Eio.File.sync file)
  in
  synced "a" "A";
  synced "b" "B";
  sync_dirs fs;
  Eio.Path.rename (fs / "b") (fs / "a");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "x" / "h") "h";
  Eio.Path.sync_dir (fs / "x");
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "a" "A" (Eio.Path.load (post / "a"));
  Alcotest.(check string) "b" "B" (Eio.Path.load (post / "b"))

(* A write-only (0o200) file is still rewritten at crash time: the rewrite
   opens O_RDWR, which needs the read bit too, so the model grants owner
   read and write for the duration and restores the exact mode after. *)
let test_crash_write_only_file () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o200) (fs / "img") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "AAAA" ];
      Eio.File.sync file;
      Eio.File.pwrite_all file ~file_offset:(i63 4) [ Cstruct.of_string "BBBB" ]);
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check int)
    "write-only mode preserved after rewrite" 0o200
    (Eio.Path.stat ~follow:true (post / "img")).perm;
  Eio.Path.chmod ~follow:true ~perm:0o644 (post / "img");
  Alcotest.(check string)
    "write-only file rewritten to its synced prefix" "AAAA"
    (Eio.Path.load (post / "img"))

(* {2 The same hazards against the real filesystem}

   The kernel is the authority on EACCES and fd invalidation; the in-memory
   runs above only count because the backend models both. These runs pin the
   primitive against the real thing. *)

let test_crash_readonly_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o444) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ]);
      sync_dirs fs)
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: read-only file rewritten to its synced prefix" "AAAA"
        (Eio.Path.load (post / "img"));
      Alcotest.(check int)
        "real fs: mode preserved" 0o444
        (Eio.Path.stat ~follow:true (post / "img")).perm)

let test_crash_closed_subtree_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.mkdir ~perm:0o755 (fs / "stage");
      Eio.Path.with_open_dir (fs / "stage") (fun stage ->
          Eio.Path.with_open_out ~create:(`Exclusive 0o444)
            Eio.Path.(stage / "img")
            (fun file ->
              Eio.File.pwrite_all file ~file_offset:(i63 0)
                [ Cstruct.of_string "data" ];
              Eio.File.sync file;
              Eio.File.pwrite_all file ~file_offset:(i63 4)
                [ Cstruct.of_string "junk" ]));
      sync_dirs fs)
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: read-only subtree-staged file rewritten" "data"
        (Eio.Path.load (post / "stage" / "img"));
      Alcotest.(check int)
        "real fs: mode preserved" 0o444
        (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm)

(* A file the crash spared keeps its inode and modification time, as on a
   disk: the image is restored in place, and an entry that already holds what
   the image says is left as it stands. *)
let test_crash_spared_identity_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  let before = ref None in
  let identity (st : Eio.File.Stat.t) = (st.ino, st.mtime) in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "kept")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "data" ];
          Eio.File.sync file);
      Crash.sync_metadata ctl;
      before := Some (identity (Eio.Path.stat ~follow:false (fs / "kept")));
      Eio.Path.save ~create:(`Exclusive 0o644) (fs / "lost") "junk")
    ~check:(fun post ->
      Alcotest.(check bool)
        "real fs: the unsynced file is gone" true
        (absent (post / "lost"));
      Alcotest.(check (option (pair int64 (float 0.))))
        "real fs: the spared file's inode and mtime" !before
        (Some (identity (Eio.Path.stat ~follow:false (post / "kept")))))

(* A crash an operation fires in a fiber being cancelled, as the operation
   that cuts the power is when a sibling fails, restores its whole image:
   the synced bytes come back under the unsynced ones. *)
let test_crash_cancelled_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "BBBB" ]);
      sync_dirs fs;
      match
        Eio.Cancel.sub (fun cc ->
            Eio.Cancel.cancel cc Exit;
            Crash.cut ctl)
      with
      | () | (exception Eio.Cancel.Cancelled _) -> ())
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: the synced bytes, restored under a cancel" "AAAA"
        (Eio.Path.load (post / "img")))

(* A file renamed over another, its directory synced, while an earlier
   directory operation elsewhere is still pending: the image that keeps the
   pending one holds the new file's bytes at the name, never the bytes of
   the file it replaced. *)
let test_crash_replaced_under_pending_real () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.mkdir ~perm:0o755 (fs / "refs");
      Eio.Path.with_open_out ~create:(`Exclusive 0o644)
        (fs / "refs" / "x")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "old" ];
          Eio.File.sync file);
      sync_dirs fs;
      Eio.Path.with_open_out ~create:(`Exclusive 0o644)
        (fs / "refs" / "x.lock")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "new" ];
          Eio.File.sync file);
      Eio.Path.mkdir ~perm:0o755 (fs / "other");
      Eio.Path.rename (fs / "refs" / "x.lock") (fs / "refs" / "x");
      Eio.Path.sync_dir (fs / "refs");
      Crash.cut ctl)
    ~check:(fun post ->
      Alcotest.(check bool)
        "real fs: the pending directory kept" true
        (Eio.Path.kind ~follow:false (post / "other") = `Directory);
      Alcotest.(check string)
        "real fs: the new bytes at the replaced name" "new"
        (Eio.Path.load (post / "refs" / "x")))

(* The kernel-side variant of the reverted tmp-rename: the rollback must
   physically undo a rename on the real filesystem. *)
let test_crash_rename_revert_real () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "NEW" ];
          Eio.File.sync file);
      sync_dirs fs;
      Eio.Path.rename (fs / "tmp") (fs / "final"))
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: file back at the temporary path" "NEW"
        (Eio.Path.load (post / "tmp"));
      Alcotest.(check bool)
        "real fs: final path absent" true
        (absent (post / "final")))

(* {2 A file is the file, whatever name opened it} *)

(* An unsynced write through a symbolic link is a write to its target. *)
let test_crash_write_through_a_symlink () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let raw = env#fs in
  write_new (raw / "real") "old";
  Eio.Path.symlink ~link_to:"real" (raw / "link");
  let fs = Crash.wrap ctl raw in
  overwrite (fs / "link") "NEW";
  Eio.Path.mkdir ~perm:0o755 (fs / "x");
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the target's write is lost" "old"
    (Eio.Path.load (post / "real"))

(* fsync(2) flushes the file whatever name opened it. *)
let test_crash_sync_through_another_spelling () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "f") "";
  Eio.Path.sync_dir fs;
  overwrite (fs / "f") "data";
  Eio.Switch.run (fun sw ->
      let h = Eio.Path.open_in ~sw (fs / "./f") in
      sync_read_only h);
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "durable" "data" (Eio.Path.load (post / "f"))

(* Opening a file under another spelling makes none of its bytes durable. *)
let test_crash_open_through_another_spelling () =
  List.iter
    (fun alias ->
      let ctl = Crash.v ~policy:Drop_unsynced () in
      with_crash ctl @@ fun ~raw:_ ~fs ->
      write_new ~sync:true (fs / "f") "";
      Eio.Path.sync_dir fs;
      overwrite (fs / "f") "secret";
      Eio.Path.with_open_out ~create:`Never (fs / alias) (fun _ -> ());
      let post = Crash.post_crash ctl in
      Alcotest.(check string) alias "" (Eio.Path.load (post / "f")))
    [ "./f"; ".//f"; "././f" ]

(* fsync(2) on a directory opened with open_in is that directory's barrier. *)
let test_crash_sync_through_a_directory_handle () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  sync_dirs fs;
  write_new ~sync:true (fs / "d" / "f") "data";
  Eio.Switch.run (fun sw ->
      let h = Eio.Path.open_in ~sw (fs / "d") in
      sync_read_only h);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the entry is durable" "data"
    (Eio.Path.load (post / "d" / "f"))

(* A barrier on a directory not yet durable is kept: once the parent's entry
   is durable, the child's entries are too. *)
let test_crash_bottom_up_barriers () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  write_new ~sync:true (fs / "a" / "f") "data";
  Eio.Path.sync_dir (fs / "a");
  write_new ~sync:true (fs / "a" / "g") "late";
  Eio.Path.sync_dir fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "a/f" "data" (Eio.Path.load (post / "a" / "f"));
  Alcotest.(check bool)
    "a/g, created after a's barrier" true
    (absent (post / "a" / "g"))

(* A barrier on a rename's destination commits it. *)
let test_crash_rename_destination_barrier () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio.Path.sync_dir fs;
  write_new ~sync:true (fs / "a" / "f") "data";
  Eio.Path.sync_dir (fs / "a");
  Eio.Path.rename (fs / "a" / "f") (fs / "b" / "f");
  Eio.Path.sync_dir (fs / "b");
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "b/f" "data" (Eio.Path.load (post / "b" / "f"));
  Alcotest.(check bool) "a/f" true (absent (post / "a" / "f"))

(* fsyncgate: Linux marks the pages of a failed fsync clean, so a retry that
   succeeds does not write them; the page cache still holds their bytes,
   so a later write to the same page writes them with it, and one to
   another page does not. *)
let test_crash_sync_retry_after_failure () =
  let run off =
    let ctl = Crash.v ~policy:Drop_unsynced () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") (String.make 12288 'a');
    Eio.Path.sync_dir fs;
    let cfs = Chaos.wrap (at `Sync "f" (Fail `Eio)) fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0)
          [ Cstruct.of_string "record" ];
        (try Eio.File.sync f with Eio.Io _ -> ());
        Eio.File.sync f;
        Eio.File.pwrite_all f ~file_offset:(i63 off)
          [ Cstruct.of_string "next" ];
        Eio.File.sync f);
    let got = Eio.Path.load (Crash.post_crash ctl / "f") in
    String.sub got 0 6
  in
  Alcotest.(check (pair string string))
    "the same page, then another" ("record", "aaaaaa")
    (run 6, run 8192)

(* The same with the failure beneath the model. *)
let test_crash_sync_failure_beneath () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let fs =
    Crash.wrap ctl (Chaos.wrap (at ~nth:2 `Sync "f" (Fail `Eio)) env#fs)
  in
  write_new ~sync:true (fs / "f") "";
  Eio.Path.sync_dir fs;
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "record" ];
      (try Eio.File.sync f with Eio.Io _ -> ());
      Eio.File.sync f);
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "lost" "" (Eio.Path.load (post / "f"))

(* {2 The model's authority is the wrapper, not the descriptor}

   A write or a sync on the host descriptor beneath a wrapped file is not one
   the model saw. *)

(* A file opened through the wrapper over a host directory has the host's
   descriptor beneath it; a run that could not take it would be testing
   nothing, so the absence is a failure and not a skip. *)
let carried_over_fd file =
  match
    Option.bind (Eio_fs_internal.base_file file) Eio_unix.Resource.fd_opt
  with
  | Some fd -> fd
  | None -> Alcotest.fail "no descriptor beneath the wrapper's file"

(* Survive-everything keeps every change the journal holds, so what is missing
   from the recovered image is exactly what never entered the journal. The live
   file carries all three writes; the image carries the two that went through
   the wrapper. *)
let test_crash_omits_a_write_through_a_bare_descriptor () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          sync_dirs fs;
          (* through the wrapper: unsynced, journalled, kept by this policy *)
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ];
          (* on the descriptor the wrapper carried over: never journalled *)
          Eio_unix.Fd.use_exn "bare descriptor" (carried_over_fd file)
            (fun raw ->
              ignore (Unix.lseek raw 8 Unix.SEEK_SET : int);
              Alcotest.(check int)
                "the bare descriptor wrote its bytes" 4
                (Unix.write_substring raw "XXXX" 0 4)));
      Alcotest.(check string)
        "the live file carries all three writes" "AAAABBBBXXXX"
        (Eio.Path.load (fs / "img")))
    ~check:(fun post ->
      Alcotest.(check string)
        "recovery replays the journal, which the bare descriptor never entered"
        "AAAABBBB"
        (Eio.Path.load (post / "img")))

(* The barrier half: an fsync on the carried-over descriptor pushes the host
   file's bytes to the platter, which is what the model must not read as a sync
   of its own. What marks a file's writes durable is the wrapper's [sync];
   after a barrier the model never saw, the write is still unsynced and a
   Drop_unsynced crash takes it. *)
let test_crash_ignores_a_barrier_through_a_bare_descriptor () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file;
          sync_dirs fs;
          Eio.File.pwrite_all file ~file_offset:(i63 4)
            [ Cstruct.of_string "BBBB" ];
          Eio_unix.Fd.use_exn "bare descriptor" (carried_over_fd file)
            Unix.fsync);
      Alcotest.(check string)
        "the live file carries the unsynced write" "AAAABBBB"
        (Eio.Path.load (fs / "img")))
    ~check:(fun post ->
      Alcotest.(check string)
        "a barrier the model never saw left the write unsynced" "AAAA"
        (Eio.Path.load (post / "img")))

(* An entry made by name after the wrap is not in the post-crash image, even
   under [Survive] of everything. *)
let test_crash_drops_an_entry_planted_by_native_name () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  let planted = ref "" in
  with_real_crash ctl
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file);
      sync_dirs fs;
      (* by name, as a child process writing into the tree would: the wrapper
         is not on the path this write takes *)
      planted := Eio.Path.native_exn (fs / "device.img");
      let oc = open_out_bin !planted in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc "DEVICE");
      Alcotest.(check bool)
        "the planted entry is live in the tree" true (Sys.file_exists !planted))
    ~check:(fun post ->
      Alcotest.(check string)
        "what the model journalled comes back" "AAAA"
        (Eio.Path.load (post / "img"));
      Alcotest.(check bool)
        "an entry planted by name after the wrap is not in the image" true
        (absent (post / "device.img")))

(* A cut over a host directory, through the public controller. *)
let with_real_cut policy fn ~check =
  with_real_dir @@ fun real ->
  let ctl = Chaos.Power.v policy in
  fn (Chaos.Power.wrap ctl real);
  check (Chaos.Power.cut ctl)

(* A workload whose processes write the tree by name beside the wrapper
   keeps their writes, as a disk keeps what a process that syncs its own
   writes left there. *)
let test_crash_keeps_an_entry_planted_by_native_name () =
  with_real_cut Lose_unsynced
    (fun fs ->
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "AAAA" ];
          Eio.File.sync file);
      sync_dirs fs;
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) (fs / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "BBBB" ]);
      Out_channel.with_open_bin
        (Eio.Path.native_exn (fs / "device.img"))
        (fun oc -> output_string oc "DEVICE"))
    ~check:(fun post ->
      Alcotest.(check string)
        "the wrapper's unsynced rewrite is the policy's to drop" "AAAA"
        (Eio.Path.load (post / "img"));
      Alcotest.(check string)
        "the planted entry is in the image" "DEVICE"
        (Eio.Path.load (post / "device.img")))

(* A child makes a directory and a file in it by name, and the run then
   replaces the file through the wrapper with a lock renamed over it. A
   barrier on the directory the child made commits the rename; with no
   barrier the rename is lost, and the image holds the child's file, never no
   file. *)
let rename_over_a_childs_file ~barrier =
  with_real_cut Lose_unsynced
    (fun fs ->
      let dir = Eio.Path.native_exn fs ^ "/made" in
      Unix.mkdir dir 0o755;
      Out_channel.with_open_bin (dir ^ "/ref") (fun oc ->
          output_string oc "child");
      Eio.Path.with_open_out ~create:(`Exclusive 0o644)
        (fs / "made" / "ref.lock")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "run" ];
          Eio.File.sync file);
      Eio.Path.rename (fs / "made" / "ref.lock") (fs / "made" / "ref");
      if barrier then Eio.Path.sync_dir (fs / "made"))
    ~check:(fun post ->
      Alcotest.(check string)
        "the file the image holds"
        (if barrier then "run" else "child")
        (Eio.Path.load (post / "made" / "ref"));
      Alcotest.(check bool)
        "no lock is left" true
        (absent (post / "made" / "ref.lock")))

let test_crash_kept_barrier_on_a_childs_directory () =
  rename_over_a_childs_file ~barrier:true

let test_crash_kept_rename_over_a_childs_file_lost () =
  rename_over_a_childs_file ~barrier:false

(* A file of the base the caller holds already, given to [adopt] through the
   wrapper, is tracked as one opened through it, where a held descriptor's
   writes would be another writer's: its sync makes what it wrote durable,
   and its unsynced write is the policy's to drop, where the image keeps what
   other writers left. *)
let test_crash_tracks_an_adopted_file () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-crash" "" in
  Sys.remove dir;
  let base = Eio.Path.(fs / dir) in
  Eio.Path.mkdir ~perm:0o755 base;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true base)
  @@ fun () ->
  let ctl = Crash.v ~policy:Drop_unsynced ~outside:Kept () in
  let wrapped = Crash.wrap ctl base in
  Eio.Path.save ~create:(`Exclusive 0o644) (base / "img") "AAAA";
  Eio.Switch.run (fun sw ->
      let held =
        (Eio.Path.open_out ~sw ~create:`Never (base / "img")
          :> Eio.File.rw_ty Eio.Resource.t)
      in
      let img = Eio_fs.Sync.adopt ~sw (wrapped / "img") held in
      let write s =
        Eio.File.pwrite_all img ~file_offset:(i63 0) [ Cstruct.of_string s ]
      in
      write "BBBB";
      Eio.File.sync img;
      write "CCCC");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the adopted file's synced write is kept and its unsynced one dropped"
    "BBBB"
    (Eio.Path.load (post / "img"))

(* A file made through the wrapper, synced, and named by a barrier on its
   directory is one the cut spares: on a disk it keeps its inode and its
   modification time. *)
let test_crash_cut_spares_a_new_file () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-crash" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () ->
  let ctl = Eio_fs_chaos.Power.(v Lose_unsynced) in
  let wrapped = Eio_fs_chaos.Power.wrap ctl Eio.Path.(fs / dir) in
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (wrapped / "kept")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.sync_dir wrapped;
  let identity p =
    let st = Eio.Path.stat ~follow:false p in
    (st.ino, st.mtime)
  in
  let before = identity (wrapped / "kept") in
  Eio.Path.save ~create:(`Exclusive 0o644) (wrapped / "lost") "junk";
  let post = Eio_fs_chaos.Power.cut ctl in
  Alcotest.(check bool)
    "the unsynced file is gone" true
    (absent (post / "lost"));
  Alcotest.(check string)
    "the spared file's bytes" "data"
    (Eio.Path.load (post / "kept"));
  Alcotest.(check (pair int64 (float 0.)))
    "the spared file's inode and mtime" before
    (identity (post / "kept"))

(* A controller built through the public API keeps what a child writes by
   name beside its wrapper, while the wrapper's own unsynced rewrite is the
   policy's to drop. *)
let test_crash_public_controller_keeps_a_childs_entry () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-crash" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () ->
  let ctl = Eio_fs_chaos.Power.(v Lose_unsynced) in
  let wrapped = Eio_fs_chaos.Power.wrap ctl Eio.Path.(fs / dir) in
  let write create s =
    Eio.Path.with_open_out ~create (wrapped / "img") (fun file ->
        Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string s ];
        if s = "AAAA" then Eio.File.sync file)
  in
  write (`Exclusive 0o600) "AAAA";
  Eio.Path.sync_dir wrapped;
  write (`Or_truncate 0o600) "BBBB";
  Out_channel.with_open_bin
    (Eio.Path.native_exn (wrapped / "device.img"))
    (fun oc -> output_string oc "DEVICE");
  let post = Eio_fs_chaos.Power.cut ctl in
  Alcotest.(check string)
    "the wrapper's unsynced rewrite is dropped" "AAAA"
    (Eio.Path.load (post / "img"));
  Alcotest.(check string)
    "the child's entry is kept" "DEVICE"
    (Eio.Path.load (post / "device.img"))

(* A snapshot reads the tree while the run's other processes write it: a
   file a child rewrites between the snapshot's stat and its read is read as
   it stands, one of the images the rewrite passes through, and a snapshot is
   no failure of the workload. The rewrite through [raw], once the snapshot
   the wrap takes reads the file, stands in for the child. *)
let test_crash_snapshot_reads_a_file_being_rewritten () =
  let ctl = Crash.v () in
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let raw = env#fs in
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "img") "AAAAAAAA";
  let armed = ref true in
  let rewrite (op : Op.t) =
    match op.kind with
    | `Read when !armed && String.equal (Filename.basename op.path) "img" ->
        armed := false;
        Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "img") "BB"
    | _ -> ()
  in
  let fs =
    Crash.wrap ctl
      (Chaos.wrap
         (fun op ->
           rewrite op;
           None)
         raw)
  in
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Alcotest.(check bool) "the rewrite ran during the snapshot" false !armed;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the image holds the file as it stood" "BB"
    (Eio.Path.load (post / "img"))

(* {2 A wrapper hands out nothing behind it}

   Code under test given a wrapper holds that wrapper and nothing more: no
   handler it can rebuild from the wrapper's own reaches the base, so no
   operation steps around the schedule or the journal. Each case drops one
   binding at a time from what the wrapper hands back and asks the rest for
   what the dropped one answered. *)

(* [r] with its [k]th binding dropped, for each [k]. An operation the stripped
   handler refuses raises [Eio.Io], the fault the schedule injects, or
   [Failure], what Eio raises for an operation its handler lacks. *)
let each_without (Eio.Resource.T (v, ops)) fn =
  let bindings = Eio.Resource.bindings ops in
  List.iteri
    (fun k _ ->
      let rest = List.filteri (fun i _ -> i <> k) bindings in
      fn k (Eio.Resource.T (v, Eio.Resource.handler rest)))
    bindings

(* Every operation of [kinds] fails with [code]. *)
let every kinds code (op : Op.t) =
  if List.mem op.kind kinds then Some (Chaos.Fault.Fail code) else None

let test_wrapped_dir_hides_its_base () =
  with_chaos (every [ `Open_read; `Open_write ] `Eacces) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "base";
  let dir, path = chaos in
  each_without dir (fun k stripped ->
      let stripped = (stripped :> Eio.Fs.dir_ty Eio.Resource.t) in
      (match Eio.Path.load ((stripped, path) / "f") with
      | data ->
          Alcotest.failf "without binding %d, a read past the schedule: %S" k
            data
      | exception (Eio.Io _ | Failure _) -> ());
      match
        Eio.Path.save ~create:(`Or_truncate 0o644) ((stripped, path) / "f") "x"
      with
      | () -> Alcotest.failf "without binding %d, a write past the schedule" k
      | exception (Eio.Io _ | Failure _) -> ());
  Alcotest.(check string)
    "the base is untouched" "base"
    (Eio.Path.load (raw / "f"))

let test_wrapped_file_hides_its_base () =
  with_chaos (every [ `Write; `Truncate ] `Eio) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "base";
  Eio.Path.with_open_out ~create:`Never (chaos / "f") @@ fun file ->
  each_without file (fun k stripped ->
      let stripped = (stripped :> Eio.File.rw_ty Eio.Resource.t) in
      match Eio.Flow.copy_string "x" stripped with
      | () -> Alcotest.failf "without binding %d, a write past the schedule" k
      | exception (Eio.Io _ | Failure _) -> ());
  Alcotest.(check string)
    "the base is untouched" "base"
    (Eio.Path.load (raw / "f"))

(* A write that reaches the store reaches the journal: a handler rebuilt from
   a crash wrapper's file either refuses the write or journals it. *)
let test_crash_file_hides_its_base () =
  let ctl = Crash.v () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") @@ fun file ->
  let journalled () =
    List.length
      (List.filter
         (fun (u : Crash.unsynced) -> u.kind = Crash.Data)
         (Crash.unsynced ctl))
  in
  each_without file (fun k stripped ->
      let stripped = (stripped :> Eio.File.rw_ty Eio.Resource.t) in
      let before = journalled () in
      match Eio.Flow.copy_string "x" stripped with
      | () ->
          if journalled () = before then
            Alcotest.failf "without binding %d, a write the journal missed" k
      | exception (Eio.Io _ | Failure _) -> ())

(* A rename from a directory of the base backend into a Chaos directory
   lands in the tree both share, as a rename between two directories of one
   in-memory filesystem does. *)
let test_rename_into_a_wrapper () =
  with_chaos (fun _ -> None) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "a") "moved";
  Eio.Path.rename (raw / "a") (chaos / "b");
  Alcotest.(check string) "at its new name" "moved" (Eio.Path.load (raw / "b"));
  Alcotest.(check bool)
    "gone from its old name" true
    (Eio.Path.kind ~follow:false (raw / "a") = `Not_found)

let suite =
  ( "crash",
    [
      Alcotest.test_case "rename into a wrapper" `Quick
        test_rename_into_a_wrapper;
      Alcotest.test_case "a wrapped directory hides its base" `Quick
        test_wrapped_dir_hides_its_base;
      Alcotest.test_case "a wrapped file hides its base" `Quick
        test_wrapped_file_hides_its_base;
      Alcotest.test_case "crash: a wrapped file hides its base" `Quick
        test_crash_file_hides_its_base;
      Alcotest.test_case "crash: the surviving prefix is uniform" `Quick
        test_crash_reorder_prefix_uniform;
      Alcotest.test_case "crash: a seed reproduces a save's image" `Quick
        test_crash_seed_reproduces_save;
      Alcotest.test_case "crash: the default policy is one state" `Quick
        test_crash_default_is_one_state;
      Alcotest.test_case "crash: a write through a symbolic link" `Quick
        test_crash_write_through_a_symlink;
      Alcotest.test_case "crash: a sync through another spelling" `Quick
        test_crash_sync_through_another_spelling;
      Alcotest.test_case "crash: an open through another spelling" `Quick
        test_crash_open_through_another_spelling;
      Alcotest.test_case "crash: a sync through a directory handle" `Quick
        test_crash_sync_through_a_directory_handle;
      Alcotest.test_case "crash: bottom-up directory barriers" `Quick
        test_crash_bottom_up_barriers;
      Alcotest.test_case "crash: a barrier on a rename's destination" `Quick
        test_crash_rename_destination_barrier;
      Alcotest.test_case "crash: a sync retried after a failure" `Quick
        test_crash_sync_retry_after_failure;
      Alcotest.test_case "crash: a sync failure beneath the model" `Quick
        test_crash_sync_failure_beneath;
      Alcotest.test_case "crash: an unwrapped controller is refused" `Quick
        test_crash_unwrapped_refused;
      Alcotest.test_case "crash: unsynced write lost" `Quick
        test_crash_unsynced_lost;
      Alcotest.test_case "crash: synced write survives" `Quick
        test_crash_synced_survives;
      Alcotest.test_case "crash: barrier respected" `Quick
        test_crash_barrier_respected;
      Alcotest.test_case "crash: barrier not reordered" `Quick
        test_crash_barrier_not_reordered;
      Alcotest.test_case "crash: pointer without data" `Quick
        test_crash_pointer_without_data;
      Alcotest.test_case "crash: seeded determinism" `Quick
        test_crash_determinism;
      Alcotest.test_case "pre-existing file synced after the wrap" `Quick
        (preexisting_file_synced_after_wrap ~policy:Crash.Drop_unsynced
           ~pending_entry:false);
      Alcotest.test_case "pre-existing file synced behind a pending entry"
        `Quick
        (preexisting_file_synced_after_wrap
           ~policy:(Crash.Survive (fun _ -> true))
           ~pending_entry:true);
      Alcotest.test_case "crash: read-only synced file" `Quick
        test_crash_readonly_synced;
      Alcotest.test_case "crash: read-only unsynced tail" `Quick
        test_crash_readonly_unsynced_tail;
      Alcotest.test_case "crash: closed staging subtree" `Quick
        test_crash_closed_subtree;
      Alcotest.test_case "crash: read-only in closed subtree" `Quick
        test_crash_readonly_in_closed_subtree;
      Alcotest.test_case "crash: write-only file" `Quick
        test_crash_write_only_file;
      Alcotest.test_case "crash: read-only file (real fs)" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_readonly_real);
      Alcotest.test_case "crash: closed subtree (real fs)" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_closed_subtree_real);
      Alcotest.test_case "crash: a spared file keeps its identity (real fs)"
        `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_spared_identity_real);
      Alcotest.test_case "crash: a cancelled crash restores its image (real fs)"
        `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_cancelled_real);
      Alcotest.test_case
        "crash: a replaced file under a pending operation (real fs)" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_replaced_under_pending_real);
      Alcotest.test_case "crash: committed unlink stays deleted" `Quick
        test_crash_committed_unlink_stays_deleted;
      Alcotest.test_case "crash: uncommitted unlink rolls back" `Quick
        test_crash_uncommitted_unlink_rolls_back;
      Alcotest.test_case "crash: a file sync leaves metadata pending" `Quick
        test_crash_file_sync_leaves_metadata_pending;
      Alcotest.test_case "crash: uncommitted rename reverts" `Quick
        test_crash_uncommitted_rename_reverts;
      Alcotest.test_case "crash: sync_dir commits rename" `Quick
        test_crash_sync_dir_commits_rename;
      Alcotest.test_case "crash: reverted rename restores victim" `Quick
        test_crash_reverted_rename_restores_victim;
      Alcotest.test_case "crash: uncommitted mkdir vanishes" `Quick
        test_crash_uncommitted_mkdir_vanishes;
      Alcotest.test_case "crash: sync_dir commits mkdir" `Quick
        test_crash_sync_dir_commits_mkdir;
      Alcotest.test_case "crash: uncommitted symlink vanishes" `Quick
        test_crash_uncommitted_symlink_vanishes;
      Alcotest.test_case "crash: symlink swap reverts" `Quick
        test_crash_symlink_swap_reverts;
      Alcotest.test_case "crash: uncommitted rmdir restored" `Quick
        test_crash_uncommitted_rmdir_restored;
      Alcotest.test_case "crash: dir rename uncommitted reverts" `Quick
        test_crash_dir_rename_uncommitted_reverts;
      Alcotest.test_case "crash: dir rename committed moves" `Quick
        test_crash_dir_rename_committed_moves;
      Alcotest.test_case "crash: a held subtree follows a rename" `Quick
        test_crash_held_subtree_follows_a_rename;
      Alcotest.test_case "crash: survive prefix rule" `Quick
        test_crash_survive_prefix_rule;
      Alcotest.test_case "crash: no directory op after the crash" `Quick
        test_crash_stops_directory_ops;
      Alcotest.test_case "crash: a synced file behind an older pending entry"
        `Quick test_crash_synced_file_behind_older_pending_entry;
      Alcotest.test_case "crash: unsynced lists dir ops" `Quick
        test_crash_unsynced_lists_dir_ops;
      Alcotest.test_case "crash: dir sync leaves its own entry pending" `Quick
        test_crash_dir_sync_leaves_its_own_entry_pending;
      Alcotest.test_case "crash: dir sync commits its own entries" `Quick
        test_crash_dir_sync_commits_its_own_entries;
      Alcotest.test_case "crash: root sync commits both siblings" `Quick
        test_crash_root_sync_commits_both_siblings;
      Alcotest.test_case "crash: dir sync does not reach a grandchild" `Quick
        test_crash_dir_sync_does_not_reach_a_grandchild;
      Alcotest.test_case "crash: a truncation is its own kind" `Quick
        test_crash_truncation_kind;
      Alcotest.test_case "crash: streaming write offsets" `Quick
        test_crash_streaming_write_offsets;
      Alcotest.test_case "streaming write offsets" `Quick
        test_streaming_write_offsets;
      Alcotest.test_case "crash: dir sync keeps a pending rename" `Quick
        test_crash_dir_sync_keeps_a_pending_rename;
      Alcotest.test_case "crash: dir sync keeps a pending replacement" `Quick
        test_crash_dir_sync_keeps_a_pending_replacement;
      Alcotest.test_case "crash: rename revert (real fs)" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_rename_revert_real);
      Alcotest.test_case "crash: a bare descriptor's write is not journalled"
        `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_omits_a_write_through_a_bare_descriptor);
      Alcotest.test_case "crash: a bare descriptor's barrier is not the model's"
        `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_ignores_a_barrier_through_a_bare_descriptor);
      Alcotest.test_case "crash: an entry planted by name is dropped" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_drops_an_entry_planted_by_native_name);
      Alcotest.test_case "crash: kept, an entry planted by name is in the image"
        `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_keeps_an_entry_planted_by_native_name);
      Alcotest.test_case "crash: an adopted file is tracked" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_tracks_an_adopted_file);
      Alcotest.test_case "crash: kept, a barrier on a child's directory" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_kept_barrier_on_a_childs_directory);
      Alcotest.test_case "crash: kept, a lost rename over a child's file" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_kept_rename_over_a_childs_file_lost);
      Alcotest.test_case "crash: a public controller keeps a child's entry"
        `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_public_controller_keeps_a_childs_entry);
      Alcotest.test_case "a cut spares a new file's identity" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_crash_cut_spares_a_new_file);
      Alcotest.test_case "crash: a snapshot reads a file being rewritten" `Quick
        test_crash_snapshot_reads_a_file_being_rewritten;
    ] )
