(** Fault-injection tests for {!Eio_fs_chaos.wrap}: each fault is injected at
    one operation and asserted to occur exactly. *)

module Chaos = Eio_fs_chaos
module Fault = Chaos.Fault
module Op = Chaos.Op
open Chaos_helpers

let ( / ) = Eio.Path.( / )

let expect_io name fn =
  match fn () with
  | () -> Alcotest.failf "%s: expected Eio.Io, got success" name
  | exception Eio.Io _ -> ()

(* {2 Faults} *)

let test_short_read () =
  with_chaos (at `Read "f" (Short 3)) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Path.with_open_in (chaos / "f") @@ fun file ->
  let buf = Cstruct.create 5 in
  let got = Eio.Flow.single_read file buf in
  Alcotest.(check int) "read split to 3 bytes" 3 got;
  Alcotest.(check string)
    "first 3 bytes correct" "hel"
    (Cstruct.to_string (Cstruct.sub buf 0 got))

(* A short read of an empty buffer has nothing to shorten: the read answers
   what the base answers, and the fault's arithmetic raises nothing. *)
let test_short_read_empty () =
  with_chaos (at `Read "f" (Short 3)) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  let answer dir =
    Eio.Path.with_open_in (dir / "f") @@ fun file ->
    match Eio.Flow.single_read file Cstruct.empty with
    | n -> Fmt.str "%d" n
    | exception ((End_of_file | Eio.Io _) as ex) -> Printexc.to_string ex
  in
  let expected = answer raw in
  Alcotest.(check string) "as the base answers" expected (answer chaos)

let test_read_error positional () =
  let observed = ref [] in
  let hook (op : Op.t) =
    if op.kind = `Read then observed := op :: !observed;
    at `Read "f" (Fail `Eio) op
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Path.with_open_in (chaos / "f") @@ fun file ->
  let buf = Cstruct.create 3 in
  let read () =
    if positional then Eio.File.pread file ~file_offset:(i63 1) [ buf ]
    else Eio.Flow.single_read file buf
  in
  expect_io "read failure" (fun () -> ignore (read ()));
  Alcotest.(check string)
    "failure delivers no bytes" "\000\000\000" (Cstruct.to_string buf);
  (match !observed with
  | [ op ] ->
      Alcotest.(check string) "read retains opened path" "f" op.Op.path;
      Alcotest.(check int)
        "requested read offset"
        (if positional then 1 else 0)
        op.Op.offset;
      Alcotest.(check int) "requested read length" 3 op.Op.length
  | _ -> Alcotest.fail "expected exactly one observed read");
  Alcotest.(check int) "next read reaches unchanged data" 3 (read ());
  (match !observed with
  | [ next; _ ] ->
      Alcotest.(check int)
        "failed read preserves scheduled cursor"
        (if positional then 1 else 0)
        next.Op.offset
  | _ -> Alcotest.fail "expected exactly two observed reads");
  Alcotest.(check string)
    "failed read preserves streaming cursor"
    (if positional then "ell" else "hel")
    (Cstruct.to_string buf)

let test_short_write () =
  with_chaos (at `Write "f" (Short 2)) @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  let got =
    Eio.File.pwrite_single file ~file_offset:(i63 0)
      [ Cstruct.of_string "hello" ]
  in
  Alcotest.(check int) "write accepted only 2 bytes" 2 got;
  Alcotest.(check int)
    "only 2 bytes committed" 2
    (Optint.Int63.to_int (Eio.File.size file))

(* The durability hazard: a torn write commits its prefix to the store and then
   reports failure. Pinned at offset 512 during a 1024-byte "superblock" flush. *)
let test_torn_write () =
  with_chaos (at `Write "sb" (Torn 512)) @@ fun ~raw ~chaos ->
  let block = Cstruct.create 1024 in
  Cstruct.memset block 0xAB;
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "sb")
    (fun file ->
      expect_io "torn write" (fun () ->
          Eio.File.pwrite_all file ~file_offset:(i63 0) [ block ]));
  (* 512 bytes reached the store despite the raised error. *)
  let committed = Eio.Path.load (raw / "sb") in
  Alcotest.(check int) "prefix length committed" 512 (String.length committed);
  Alcotest.(check string)
    "prefix bytes committed" (String.make 512 '\xAB') committed

let test_write_error () =
  with_chaos (at `Write "f" (Fail `Enospc)) @@ fun ~raw ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f") (fun file ->
      expect_io "enospc" (fun () ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "data" ]));
  Alcotest.(check string) "nothing committed" "" (Eio.Path.load (raw / "f"))

let test_sync_error () =
  with_chaos (at `Sync "f" (Fail `Eio)) @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  (* the write itself is not perturbed *)
  Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
  expect_io "fsync failure" (fun () -> Eio.File.sync file)

(* An injected error carries the code the wrapper exports, with the error
   and the path it names, as a disk's error carries its backend's. *)
let test_injected_code () =
  with_chaos (at `Sync "f" (Fail `Eio)) @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  match Eio.File.sync file with
  | () -> Alcotest.fail "the sync succeeded"
  | exception Eio.Io (Eio.Exn.X (Chaos.Injected_fault what), _) ->
      Alcotest.(check bool)
        "names EIO" true
        (String.starts_with ~prefix:"EIO " what)

(* A file opened read-only answers the barrier too, as fsync(2) does, and the
   schedule fails it as it fails a read-write one. *)
let test_read_only_file_sync () =
  with_chaos (at ~nth:2 `Sync "f" (Fail `Eio)) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o444) (raw / "f") "data";
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw (chaos / "f") in
  match Eio_fs.Sync.file_barrier file with
  | None -> Alcotest.fail "a read-only file registers no barrier"
  | Some barrier ->
      barrier ~full:false;
      expect_io "the scheduled sync failure" (fun () -> barrier ~full:false)

let test_open_error () =
  with_chaos (at `Open_read "secret" (Fail `Enoent)) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "secret") "x";
  expect_io "open" (fun () -> ignore (Eio.Path.load (chaos / "secret")))

(* A chmod and a chown change an entry: a fault on [`Change] fails them
   before they are made. *)
let test_change_error_on_chmod_chown () =
  let hook (op : Op.t) =
    if op.kind = `Change then Some (Fault.Fail `Eperm) else None
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "x";
  expect_io "chmod" (fun () ->
      Eio.Path.chmod ~follow:false ~perm:0o600 (chaos / "f"));
  expect_io "chown" (fun () ->
      Eio.Path.chown ~follow:false ~uid:1L ~gid:1L (chaos / "f"));
  let st = Eio.Path.stat ~follow:false (raw / "f") in
  Alcotest.(check (pair int int64))
    "mode and owner" (0o644, 0L) (st.perm, st.uid)

(* An open for reading and one that may write are told apart. *)
let test_open_kinds () =
  let seen = ref [] in
  let hook (op : Op.t) =
    (match op.kind with
    | (`Open_read | `Open_write) as k -> seen := (k, op.path) :: !seen
    | _ -> ());
    None
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "d");
  Eio.Path.save ~create:(`Exclusive 0o644) (chaos / "f") "x";
  ignore (Eio.Path.load (chaos / "f"));
  Eio.Path.with_open_dir (chaos / "d") ignore;
  let name = function `Open_read -> "read" | `Open_write -> "write" in
  Alcotest.(check (list (pair string string)))
    "opens"
    [ ("write", "f"); ("read", "f"); ("read", "d") ]
    (List.rev_map (fun (k, p) -> (name k, p)) !seen)

(* A file of the base the caller holds already, given to [adopt], is asked
   about as an open of its path that may write: a refusal of that open
   refuses the adoption, and the adopted file's writes are the wrapper's as
   an opened one's are. A directory that registers no [Adopt], the backend's
   own, hands the file out as it is. *)
let test_adopt () =
  let seen = ref [] in
  let hook (op : Op.t) =
    seen := (op.kind, op.path) :: !seen;
    match (op.kind, op.path) with
    | `Open_write, "refused" -> Some (Fault.Fail `Eacces)
    | `Write, "f" -> Some (Fault.Short 2)
    | _ -> None
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "xxxx";
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "refused") "x";
  Eio.Switch.run @@ fun sw ->
  let held name =
    (Eio.Path.open_out ~sw ~create:`Never (raw / name)
      :> Eio.File.rw_ty Eio.Resource.t)
  in
  let file = held "f" in
  Alcotest.(check bool)
    "the backend's own directory hands the file out as it is" true
    (Eio_fs.Sync.adopt ~sw (raw / "f") file == file);
  expect_io "a refused open refuses the adoption" (fun () ->
      ignore (Eio_fs.Sync.adopt ~sw (chaos / "refused") (held "refused")));
  seen := [];
  let adopted = Eio_fs.Sync.adopt ~sw (chaos / "f") file in
  Alcotest.(check bool)
    "the adoption is asked about as an open that may write" true
    (List.mem (`Open_write, "f") !seen);
  Alcotest.(check int)
    "the wrapper shortens a write through it" 2
    (Eio.File.pwrite_single adopted ~file_offset:(i63 0)
       [ Cstruct.of_string "abcd" ]);
  Alcotest.(check string)
    "the write reached the held file" "abxx"
    (Eio.Path.load (raw / "f"))

let test_descriptor_stat_error writable () =
  let observed = ref [] in
  let hook (op : Op.t) =
    if op.kind = `Stat then observed := op.path :: !observed;
    at `Stat "f" (Fail `Eacces) op
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Switch.run @@ fun sw ->
  let file =
    if writable then
      (Eio.Path.open_out ~sw ~create:`Never (chaos / "f")
        :> Eio.File.ro_ty Eio.Resource.t)
    else Eio.Path.open_in ~sw (chaos / "f")
  in
  expect_io "descriptor metadata" (fun () -> ignore (Eio.File.size file));
  Alcotest.(check (list string))
    "one descriptor stat retains its opened path" [ "f" ] (List.rev !observed);
  Alcotest.(check int)
    "the unchanged descriptor metadata succeeds afterward" 5
    (Optint.Int63.to_int (Eio.File.size file))

let test_stat_error () =
  with_chaos (at `Stat "f" (Fail `Eacces)) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "x";
  expect_io "stat" (fun () ->
      ignore (Eio.Path.stat ~follow:false (chaos / "f")))

let directory_names entries path =
  if entries then Eio.Path.read_dir_entries path |> List.map snd
  else Eio.Path.read_dir path

let test_directory_error entries () =
  let observed = ref [] in
  let hook op =
    observed := op :: !observed;
    at `Read_dir "tree" (Fail `Eacces) op
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "payload";
  let callbacks = ref 0 in
  expect_io "directory refusal" (fun () ->
      if entries then
        Eio.Path.with_dir_entries (chaos / "tree") (fun _ -> incr callbacks)
      else ignore (directory_names false (chaos / "tree")));
  Alcotest.(check int) "no callback before refusal" 0 !callbacks;
  (match !observed with
  | [ op ] ->
      Alcotest.(check string) "directory path" "tree" op.Op.path;
      Alcotest.(check bool) "directory operation" true (op.kind = `Read_dir);
      Alcotest.(check int) "no byte offset" 0 op.offset;
      Alcotest.(check int) "no byte length" 0 op.length
  | _ -> Alcotest.fail "expected exactly one directory opportunity");
  Alcotest.(check (list string))
    "subsequent read recovers" [ "f" ]
    (directory_names entries (chaos / "tree"));
  Alcotest.(check string)
    "refusal preserves payload" "payload"
    (Eio.Path.load (raw / "tree" / "f"))

(* A short read applies to a file's bytes, not to a directory listing. *)
let test_directory_short_read_inapplicable () =
  with_chaos (fun _ -> Some (Short 1)) @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Alcotest.(check (list string))
    "directory read is not a byte read" []
    (directory_names false (chaos / "tree"))

(* [Swap_symlink] on a [`Stat]: the stat sees the regular file, and a later
   open of the same path reaches the secret. *)
let test_swap_symlink () =
  with_chaos (at `Stat "victim" (Swap_symlink "secret")) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "victim") "safe";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "secret") "SECRET";
  (* The check: the stat through chaos still sees the original regular file... *)
  let st = Eio.Path.stat ~follow:false (chaos / "victim") in
  Alcotest.(check bool)
    "the checked path is a regular file at stat time" true
    (match st.Eio.File.Stat.kind with `Regular_file -> true | _ -> false);
  (* ...but the fault swapped it underneath: the path is now a symlink to the
     secret, so the use that follows the check reads what was swapped in. *)
  Alcotest.(check bool)
    "the path became a symlink after the stat" true
    (match Eio.Path.kind ~follow:false (raw / "victim") with
    | `Symbolic_link -> true
    | _ -> false);
  Alcotest.(check string)
    "the swapped-in link points at the secret" "secret"
    (Eio.Path.read_link (raw / "victim"))

(* [Swap_symlink] on an [`Open]: the descriptor keeps the file, and the path
   then names the target. *)
let test_swap_symlink_on_open_in () =
  with_chaos (at `Open_read "current" (Swap_symlink "decoy"))
  @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "log") "the original";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "decoy") "decoy";
  Eio.Path.symlink ~link_to:"log" (raw / "current");
  let read = Eio.Path.with_open_in (chaos / "current") contents in
  Alcotest.(check string)
    "the handle reads the file the name meant at the open" "the original" read;
  Alcotest.(check string)
    "and the name points at the target from then on" "decoy"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "so the path answers about the other file" "decoy"
    (Eio.Path.load (raw / "current"))

(* The writer's half of the window, which costs more: the bytes go to the file
   the open returned, and the name they were written under has come to mean
   another file, so a reader following the name finds neither them nor the file
   they landed in. *)
let test_swap_symlink_on_open_out () =
  with_chaos (at `Open_write "current" (Swap_symlink "decoy"))
  @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "log") "";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "decoy") "decoy";
  Eio.Path.symlink ~link_to:"log" (raw / "current");
  Eio.Path.with_open_out ~create:(`If_missing 0o644) (chaos / "current")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string "written" ]);
  Alcotest.(check string)
    "the write lands in the file the name meant at the open" "written"
    (Eio.Path.load (raw / "log"));
  Alcotest.(check string)
    "the file the name means now is untouched" "decoy"
    (Eio.Path.load (raw / "decoy"));
  Alcotest.(check string)
    "and the name points at it" "decoy"
    (Eio.Path.read_link (raw / "current"))

(* A subtree opens the same way, and everything reached through the handle keeps
   resolving under the directory it was opened on while the name it was opened
   under leads elsewhere. *)
let test_swap_symlink_on_open_subtree () =
  with_chaos (at `Open_read "current" (Swap_symlink "other"))
  @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.symlink ~link_to:"tree" (raw / "current");
  let read =
    Eio.Path.with_open_dir (chaos / "current") @@ fun dir ->
    Eio.Path.load (dir / "f")
  in
  Alcotest.(check string)
    "the handle reads under the directory the name meant at the open"
    "in the tree" read;
  Alcotest.(check string)
    "and the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* {2 A swap over a real POSIX filesystem}

   Unlinking a directory is EPERM on Darwin and EISDIR on Linux, so freeing a
   name with a plain unlink is portable for a symlink and a regular file and
   nothing else. The swap must free its name kind-aware: clear the entries,
   then [try rmdir with Eio.Io _ -> unlink]. The two directory cases are
   separate on purpose, since a staging directory a store swaps under a caller
   is never empty. *)

(* Run [fn] over a private temp directory on the real filesystem, opened as a
   subtree so the paths the schedule matches are relative to it. *)
let with_real_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_file "eio-fs-chaos-swap" "" in
  Eio.Path.unlink (fs / name);
  Eio.Path.mkdir ~perm:0o755 (fs / name);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / name))
  @@ fun () -> Eio.Path.with_open_dir (fs / name) fn

(* A wrapper carries over none of the backend's own interfaces: a host
   directory behind it is no working directory a child is started in, the
   child being a way around the schedule. *)
let test_real_child_cwd () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_file "eio-fs-chaos-child" "" in
  Eio.Path.unlink (fs / name);
  Eio.Path.mkdir ~perm:0o755 (fs / name);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / name))
  @@ fun () ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    (fs / name / "payload")
    "the child's exact cwd payload\n";
  let chaos = Chaos.wrap (fun _ -> None) (fs / name) in
  match
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      ~cwd:chaos Eio.Buf_read.take_all [ "cat"; "payload" ]
  with
  | got -> Alcotest.failf "a child ran in the wrapped directory: %S" got
  | exception Invalid_argument _ -> ()

let swap_on_open path target = at `Open_read path (Swap_symlink target)

(* The name the swap frees is a symbolic link. *)
let test_swap_on_real_symlink_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.symlink ~link_to:"tree" (raw / "current");
  let read =
    Eio.Path.with_open_dir (chaos / "current") @@ fun dir ->
    Eio.Path.load (dir / "f")
  in
  Alcotest.(check string)
    "real fs: the handle reads under the tree the name meant at the open"
    "in the tree" read;
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* An empty directory: after the open, the name is a link to the target. *)
let test_swap_on_real_empty_directory_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "current");
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.with_open_dir (chaos / "current") (fun (_ : _ Eio.Path.t) -> ());
  Alcotest.(check bool)
    "real fs: the empty directory name became a symlink" true
    (Eio.Path.kind ~follow:false (raw / "current") = `Symbolic_link);
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* A directory with entries: the swap removes them with it. *)
let test_swap_on_real_populated_directory_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "current");
  Eio.Path.save ~create:(`Or_truncate 0o644)
    (raw / "current" / "f")
    "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.with_open_dir (chaos / "current") (fun (_ : _ Eio.Path.t) -> ());
  Alcotest.(check bool)
    "real fs: the populated directory name became a symlink" true
    (Eio.Path.kind ~follow:false (raw / "current") = `Symbolic_link);
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* {2 Operations} *)

(* The third open of "f" fails, and no other operation: not the opens
   before it, not an open of another path between them, not a read of "f",
   and not the open after it. *)
let test_nth_op_on_path () =
  with_chaos (at ~nth:3 `Open_read "f" (Fail `Enoent)) @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "g") "y";
  let opens =
    List.map
      (fun name ->
        match Eio.Path.with_open_in (chaos / name) contents with
        | _ -> false
        | exception Eio.Io _ -> true)
      [ "f"; "g"; "f"; "f"; "f" ]
  in
  Alcotest.(check (list bool))
    "only the third open of f fails"
    [ false; false; false; true; false ]
    opens

(* A wrapper with no fault changes nothing. *)
let test_pass_through () =
  with_chaos (fun _ -> None) @@ fun ~raw ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f") (fun file ->
      for i = 0 to 9 do
        Eio.File.pwrite_all file
          ~file_offset:(i63 (i * 16))
          [ Fmt.kstr Cstruct.of_string "%016d" i ]
      done;
      Eio.File.sync file);
  Alcotest.(check string)
    "every byte reaches the base"
    (String.concat "" (List.init 10 (Fmt.str "%016d")))
    (Eio.Path.load (raw / "f"))

(* Permission faults: a directory change or an open for writing refused with
   EACCES, EPERM or EROFS, and nothing changed. *)
let test_change_error () =
  let hook (op : Op.t) : Fault.t option =
    match (op.kind, op.path, op.nth) with
    | `Change, "f", 1 -> Some (Fail `Erofs)
    | `Change, "d", 1 -> Some (Fail `Eperm)
    | `Change, "f", 2 -> Some (Fail `Eacces)
    | `Open_write, "conf", 1 -> Some (Fail `Erofs)
    | _ -> None
  in
  with_chaos hook @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "x";
  expect_io "unlink on a read-only filesystem" (fun () ->
      Eio.Path.unlink (chaos / "f"));
  (match Eio.Path.mkdir ~perm:0o755 (chaos / "d") with
  | () -> Alcotest.fail "mkdir succeeded"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
  (match Eio.Path.rename (chaos / "f") (chaos / "g") with
  | () -> Alcotest.fail "rename succeeded"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
  expect_io "open for writing on a read-only filesystem" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o644) (chaos / "conf") "y");
  Alcotest.(check (list string))
    "nothing changed" [ "f" ] (Eio.Path.read_dir raw)

let suite =
  ( "chaos",
    [
      Alcotest.test_case "short read" `Quick test_short_read;
      Alcotest.test_case "short read of an empty buffer" `Quick
        test_short_read_empty;
      Alcotest.test_case "streaming read error" `Quick (test_read_error false);
      Alcotest.test_case "positional read error" `Quick (test_read_error true);
      Alcotest.test_case "short write" `Quick test_short_write;
      Alcotest.test_case "an injected error carries Injected_fault" `Quick
        test_injected_code;
      Alcotest.test_case "torn write" `Quick test_torn_write;
      Alcotest.test_case "write error" `Quick test_write_error;
      Alcotest.test_case "sync error" `Quick test_sync_error;
      Alcotest.test_case "read-only file sync" `Quick test_read_only_file_sync;
      Alcotest.test_case "open error" `Quick test_open_error;
      Alcotest.test_case "chmod and chown are changes" `Quick
        test_change_error_on_chmod_chown;
      Alcotest.test_case "opens for reading and writing" `Quick test_open_kinds;
      Alcotest.test_case "an adopted file is an open of its path" `Quick
        test_adopt;
      Alcotest.test_case "stat error" `Quick test_stat_error;
      Alcotest.test_case "real child cwd" `Quick
        (Host.posix_only "eio_windows 1.7 spawns no process" test_real_child_cwd);
      Alcotest.test_case "directory error" `Quick (test_directory_error false);
      Alcotest.test_case "directory entries error" `Quick
        (test_directory_error true);
      Alcotest.test_case "directory short-read inapplicable" `Quick
        test_directory_short_read_inapplicable;
      Alcotest.test_case "read-only descriptor stat error" `Quick
        (test_descriptor_stat_error false);
      Alcotest.test_case "read-write descriptor stat error" `Quick
        (test_descriptor_stat_error true);
      Alcotest.test_case "swap-symlink TOCTOU" `Quick test_swap_symlink;
      Alcotest.test_case "swap-symlink on open_in" `Quick
        test_swap_symlink_on_open_in;
      Alcotest.test_case "swap-symlink on open_out" `Quick
        test_swap_symlink_on_open_out;
      Alcotest.test_case "swap-symlink on open_subtree" `Quick
        test_swap_symlink_on_open_subtree;
      Alcotest.test_case "swap on a symlink subtree (real fs)" `Quick
        test_swap_on_real_symlink_subtree;
      Alcotest.test_case "swap on an empty directory subtree (real fs)" `Quick
        (Host.posix_only "Windows removes no directory a handle holds open"
           test_swap_on_real_empty_directory_subtree);
      Alcotest.test_case "swap on a populated directory subtree (real fs)"
        `Quick
        (Host.posix_only "Windows removes no directory a handle holds open"
           test_swap_on_real_populated_directory_subtree);
      Alcotest.test_case "nth operation on a path" `Quick test_nth_op_on_path;
      Alcotest.test_case "pass-through" `Quick test_pass_through;
      Alcotest.test_case "change error" `Quick test_change_error;
    ] )
