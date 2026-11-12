(** Fault-injection tests for {!Eio_fs.Chaos.Private}: each fault kind is pinned
    with a scripted schedule and asserted to occur exactly, and a seeded
    schedule is shown to reproduce deterministically. *)

module Chaos = Eio_fs.Chaos.Private
module Schedule = Chaos.Schedule
module Fault = Chaos.Fault
module Op = Chaos.Op

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* [raw] is the un-perturbed base (for setup and for reading back what actually
   committed); [chaos] wraps the same in-memory tree with [schedule]. *)
let with_chaos schedule fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  let chaos = Chaos.wrap schedule raw in
  fn ~raw ~chaos

let expect_io name fn =
  match fn () with
  | () -> Alcotest.failf "%s: expected Eio.Io, got success" name
  | exception Eio.Io _ -> ()

(* {2 Scripted, exact-scenario faults} *)

let test_short_read () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read; path = "f" };
          fault = Short_read 3;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read; path = "f" };
          fault = Short_read 3;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  let answer dir =
    Eio.Path.with_open_in (dir / "f") @@ fun file ->
    match Eio.Flow.single_read file Cstruct.empty with
    | n -> Fmt.str "%d" n
    | exception ((End_of_file | Eio.Io _) as ex) -> Printexc.to_string ex
  in
  let expected = answer raw in
  Alcotest.(check string) "as the base answers" expected (answer chaos);
  Alcotest.(check string)
    "fault consumed once" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp sched)

let test_read_error positional () =
  let observed = ref [] in
  let schedule =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read; path = "f" };
          fault = Read_error `Eio;
        };
      ]
  in
  let sched =
    Schedule.observing
      (fun op -> if op.Op.kind = `Read then observed := op :: !observed)
      schedule
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  Alcotest.(check string)
    "read fault consumed once" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp schedule);
  (match !observed with
  | [ op ] ->
      Alcotest.(check string) "read retains opened path" "f" op.Op.path;
      Alcotest.(check int)
        "requested read offset"
        (if positional then 1 else 0)
        op.Op.file_offset;
      Alcotest.(check int) "requested read length" 3 op.Op.length
  | _ -> Alcotest.fail "expected exactly one observed read");
  Alcotest.(check int) "next read reaches unchanged data" 3 (read ());
  (match !observed with
  | [ next; _ ] ->
      Alcotest.(check int)
        "failed read preserves scheduled cursor"
        (if positional then 1 else 0)
        next.Op.file_offset
  | _ -> Alcotest.fail "expected exactly two observed reads");
  Alcotest.(check string)
    "failed read preserves streaming cursor"
    (if positional then "ell" else "hel")
    (Cstruct.to_string buf)

let test_seeded_read_error positional () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Read_error ] ~seed:29 ()
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Path.with_open_in (chaos / "f") @@ fun file ->
  let buf = Cstruct.create 3 in
  expect_io "seeded read failure" (fun () ->
      ignore
        (if positional then Eio.File.pread file ~file_offset:(i63 1) [ buf ]
         else Eio.Flow.single_read file buf))

let test_short_write () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Write; path = "f" };
          fault = Short_write 2;
        };
      ]
  in
  with_chaos sched @@ fun ~raw:_ ~chaos ->
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
  let sched =
    Schedule.scripted
      [
        {
          trigger = Write_covering { path = "sb"; file_offset = 512 };
          fault = Torn_write 512;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Write; path = "f" };
          fault = Write_error `Enospc;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f") (fun file ->
      expect_io "enospc" (fun () ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "data" ]));
  Alcotest.(check string) "nothing committed" "" (Eio.Path.load (raw / "f"))

let test_sync_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Sync; path = "f" };
          fault = Sync_error `Eio;
        };
      ]
  in
  with_chaos sched @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  (* the write itself is not perturbed *)
  Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
  expect_io "fsync failure" (fun () -> Eio.File.sync file)

(* fsync(2) flushes a file through any descriptor on it, so a file opened
   read-only answers the barrier too -- a durable publish syncs a read-only
   file that way -- and the schedule fails it as it fails a read-write one. *)
let test_read_only_file_sync () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Nth_op_on_path { n = 2; kind = `Sync; path = "f" };
          fault = Sync_error `Eio;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o444) (raw / "f") "data";
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw (chaos / "f") in
  match Eio_fs_sync.file_barrier file with
  | None -> Alcotest.fail "a read-only file registers no barrier"
  | Some barrier ->
      barrier ();
      expect_io "the scheduled sync failure" barrier

(* [Fault.compare] orders faults by constructor, then by argument, and agrees
   with [Fault.equal]. *)
let test_fault_compare () =
  let faults =
    Fault.
      [
        Short_read 1;
        Short_read 2;
        Read_error `Enoent;
        Read_error `Eio;
        Short_write 3;
        Torn_write 0;
        Write_error `Enospc;
        Sync_error `Eio;
        Open_error `Eacces;
        Stat_error `Enoent;
        Swap_symlink "a";
        Swap_symlink "b";
      ]
  in
  List.iteri
    (fun i a ->
      List.iteri
        (fun j b ->
          let c = Fault.compare a b in
          Alcotest.(check int)
            (Fmt.str "%a vs %a" Fault.pp a Fault.pp b)
            (Int.compare i j)
            (if c < 0 then -1 else if c > 0 then 1 else 0);
          Alcotest.(check bool) "agrees with equal" (Fault.equal a b) (c = 0))
        faults)
    faults

let test_open_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_read; path = "secret" };
          fault = Open_error `Enoent;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "secret") "x";
  expect_io "open" (fun () -> ignore (Eio.Path.load (chaos / "secret")))

(* A chmod and a chown change an entry: a fault on [`Change] fails them
   before they are made. *)
let test_change_error_on_chmod_chown () =
  let sched =
    Schedule.hook (fun (op : Op.t) ->
        if op.kind = `Change then Some (Fault.Change_error `Eperm) else None)
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  let sched =
    Schedule.hook (fun (op : Op.t) ->
        (match op.kind with
        | (`Open_read | `Open_write) as k -> seen := (k, op.path) :: !seen
        | _ -> ());
        None)
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "d");
  Eio.Path.save ~create:(`Exclusive 0o644) (chaos / "f") "x";
  ignore (Eio.Path.load (chaos / "f"));
  Eio.Path.with_open_dir (chaos / "d") ignore;
  let name = function `Open_read -> "read" | `Open_write -> "write" in
  Alcotest.(check (list (pair string string)))
    "opens"
    [ ("write", "f"); ("read", "f"); ("read", "d") ]
    (List.rev_map (fun (k, p) -> (name k, p)) !seen)

let test_descriptor_stat_error writable () =
  let observed = ref [] in
  let schedule =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "f" };
          fault = Stat_error `Eacces;
        };
      ]
  in
  let sched =
    Schedule.observing
      (fun op -> if op.Op.kind = `Stat then observed := op.Op.path :: !observed)
      schedule
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  Alcotest.(check string)
    "one scripted metadata fault was consumed" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp schedule);
  Alcotest.(check int)
    "the unchanged descriptor metadata succeeds afterward" 5
    (Optint.Int63.to_int (Eio.File.size file))

let test_stat_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "f" };
          fault = Stat_error `Eacces;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "x";
  expect_io "stat" (fun () ->
      ignore (Eio.Path.stat ~follow:false (chaos / "f")))

let directory_names entries path =
  if entries then Eio.Path.read_dir_entries path |> List.map snd
  else Eio.Path.read_dir path

let test_directory_error entries () =
  let observed = ref [] in
  let scripted =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read_dir; path = "tree" };
          fault = Read_error `Eacces;
        };
      ]
  in
  let sched =
    Schedule.observing (fun op -> observed := op :: !observed) scripted
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
      Alcotest.(check int) "no byte offset" 0 op.file_offset;
      Alcotest.(check int) "no byte length" 0 op.length
  | _ -> Alcotest.fail "expected exactly one directory opportunity");
  Alcotest.(check string)
    "fault consumed once" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp scripted);
  Alcotest.(check (list string))
    "subsequent read recovers" [ "f" ]
    (directory_names entries (chaos / "tree"));
  Alcotest.(check string)
    "refusal preserves payload" "payload"
    (Eio.Path.load (raw / "tree" / "f"))

let test_seeded_directory_error entries () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Read_error ] ~seed:29 ()
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  expect_io "seeded directory refusal" (fun () ->
      ignore (directory_names entries (chaos / "tree")))

let test_directory_short_read_inapplicable () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Short_read ] ~seed:29 ()
  in
  let op : Op.t =
    {
      kind = `Read_dir;
      path = "tree";
      file_offset = 0;
      length = 0;
      space = false;
      dest = None;
    }
  in
  Alcotest.(check bool)
    "no byte fault selected for directory" true
    (Chaos.Internal.next sched op = None);
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Alcotest.(check (list string))
    "directory read is not a byte read" []
    (directory_names false (chaos / "tree"))

(* An operation longer than [2^30] bytes is split like any other. *)
let test_seeded_short_read_long_op () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Short_read ] ~seed:29 ()
  in
  let length = (1 lsl 30) + 1 in
  let op : Op.t =
    {
      kind = `Read;
      path = "f";
      file_offset = 0;
      length;
      space = false;
      dest = None;
    }
  in
  match Chaos.Internal.next sched op with
  | Some (Short_read n) ->
      Alcotest.(check bool)
        "split inside the operation" true
        (n >= 1 && n < length)
  | _ -> Alcotest.fail "no short read for a long operation"

(* A [Swap_symlink] fault reproduces a TOCTOU: the scripted [`Stat] on "victim"
   still returns the original regular file, then the path becomes a symlink to
   the secret, so a follow-up open on the same path -- as a builder that stats
   then opens would do -- resolves to what was swapped in. The check saw one
   thing; the use sees another. *)
let test_swap_symlink () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "victim" };
          fault = Swap_symlink "secret";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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

(* The whole of a file, read through the handle rather than through its name. *)
let contents file =
  let len = Optint.Int63.to_int (Eio.File.size file) in
  let buf = Cstruct.create len in
  Eio.File.pread_exact file ~file_offset:(i63 0) [ buf ];
  Cstruct.to_string buf

(* An open leaves the same window a stat does, and the descriptor it hands back
   is what makes it worth exploiting: the caller keeps the file the name meant at
   the open, while the name goes on to mean the target, so everything the caller
   asks the path afterwards -- how long it is, what it holds -- is answered about
   another file. Scripted on the [`Open], the fault pins that window for a
   reader. *)
let test_swap_symlink_on_open_in () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_read; path = "current" };
          fault = Swap_symlink "decoy";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_write; path = "current" };
          fault = Swap_symlink "decoy";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_read; path = "current" };
          fault = Swap_symlink "other";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
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
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. @@ fun () ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_file "eio-fs-chaos-child" "" in
  Eio.Path.unlink (fs / name);
  Eio.Path.mkdir ~perm:0o755 (fs / name);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / name))
  @@ fun () ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    (fs / name / "payload")
    "the child's exact cwd payload\n";
  let chaos = Chaos.wrap Schedule.empty (fs / name) in
  match
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      ~cwd:chaos Eio.Buf_read.take_all [ "cat"; "payload" ]
  with
  | got -> Alcotest.failf "a child ran in the wrapped directory: %S" got
  | exception Invalid_argument _ -> ()

let swap_on_open path target =
  Schedule.scripted
    [
      {
        trigger = Op_on_path { kind = `Open_read; path };
        fault = Swap_symlink target;
      };
    ]

(* The name the swap frees is a symlink: the portable case, and the guard rail
   for the fixture -- if this one fails the real-filesystem harness is broken,
   not the directory cases under it. *)
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

(* An empty true directory. The open completes, the name is a symlink to the
   target afterwards, and every path-based follow-up is answered about the
   target -- the whole point of the fault. *)
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

(* A populated true directory -- the shape a store's staging directory has, and
   the shape the in-memory subtree case above uses. Freeing this name takes the
   entries with it, so the swap must clear them rather than stop at ENOTEMPTY.
*)
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

(* {2 Schedule as an inspectable value} *)

(* A scripted rule fires the first time its trigger matches, then is spent. *)
let test_scripted_fires_once () =
  let sched =
    Schedule.scripted [ { trigger = Nth_op 2; fault = Short_read 1 } ]
  in
  let op =
    {
      Op.kind = `Read;
      path = "f";
      file_offset = 0;
      length = 10;
      space = false;
      dest = None;
    }
  in
  let check name expected =
    Alcotest.(check bool)
      name true
      (match (Chaos.Internal.next sched op, expected) with
      | None, None -> true
      | Some a, Some b -> Fault.equal a b
      | _ -> false)
  in
  check "op1 passes through" None;
  check "op2 fires" (Some (Fault.Short_read 1));
  check "op3 passes through (rule spent)" None

(* The third open of "f" fails, and no other operation: not the opens
   before it, not an open of another path between them, not a read of "f",
   and not the open after it, the rule being spent. *)
let test_nth_op_on_path () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Nth_op_on_path { n = 3; kind = `Open_read; path = "f" };
          fault = Open_error `Enoent;
        };
      ]
  in
  let op kind path =
    { Op.kind; path; file_offset = 0; length = 0; space = false; dest = None }
  in
  let faults =
    List.map
      (fun o -> Option.is_some (Chaos.Internal.next sched o))
      [
        op `Open_read "f";
        op `Open_read "g";
        op `Read "f";
        op `Open_read "f";
        op `Read "f";
        op `Open_read "f";
        op `Open_read "f";
      ]
  in
  Alcotest.(check (list bool))
    "only the third open of f fails"
    [ false; false; false; false; false; true; false ]
    faults

(* {2 Seeded determinism} *)

let ops_fixture =
  let mk kind path file_offset length : Op.t =
    { kind; path; file_offset; length; space = false; dest = None }
  in
  [
    mk `Open_write "a" 0 0;
    mk `Write "a" 0 4096;
    mk `Write "a" 4096 4096;
    mk `Sync "a" 0 0;
    mk `Read "a" 0 8192;
    mk `Stat "b" 0 0;
    mk `Open_write "b" 0 0;
    mk `Write "b" 0 512;
    mk `Read "b" 0 512;
    mk `Sync "b" 0 0;
  ]

let run_schedule seed =
  let s = Schedule.seeded ~seed ~probability:0.7 () in
  List.map (Chaos.Internal.next s) ops_fixture

let test_seed_determinism () =
  let a = run_schedule 1234 and b = run_schedule 1234 in
  Alcotest.(check int) "same length" (List.length a) (List.length b);
  List.iter2
    (fun x y ->
      Alcotest.(check bool)
        "same fault at step" true
        (match (x, y) with
        | None, None -> true
        | Some fx, Some fy -> Fault.equal fx fy
        | _ -> false))
    a b;
  (* non-vacuous: at this probability at least one op must have faulted *)
  Alcotest.(check bool)
    "faults actually occur" true
    (List.exists Option.is_some a)

(* End-to-end determinism over the composed stack: the same seed drives the
   same committed bytes and the same success/failure outcome, and some seed
   perturbs the clean result (so the wrapper really injects). *)
let run_workload schedule =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let fs = Chaos.wrap schedule env#fs in
  let outcome =
    try
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "f")
        (fun file ->
          for i = 0 to 9 do
            Eio.File.pwrite_all file
              ~file_offset:(i63 (i * 16))
              [ Fmt.kstr Cstruct.of_string "%016d" i ]
          done;
          Eio.File.sync file);
      "ok"
    with Eio.Io _ -> "io-error"
  in
  (* read back the truly-committed bytes through the un-perturbed base *)
  let committed =
    try Eio.Path.load (env#fs / "f") with Eio.Io _ -> "<none>"
  in
  (outcome, committed)

let test_seed_reproduces () =
  let a = run_workload (Schedule.seeded ~seed:99 ~probability:0.4 ()) in
  let b = run_workload (Schedule.seeded ~seed:99 ~probability:0.4 ()) in
  Alcotest.(check (pair string string)) "same outcome and bytes" a b

(* Permission faults: a directory change or an open for writing refused with
   EACCES, EPERM or EROFS, and nothing changed. *)
let test_change_error () =
  let rule kind path fault =
    { Schedule.trigger = Op_on_path { kind; path }; fault }
  in
  with_chaos
    (Schedule.scripted
       [
         rule `Change "f" (Change_error `Erofs);
         rule `Change "d" (Change_error `Eperm);
         {
           trigger = Nth_op_on_path { n = 2; kind = `Change; path = "f" };
           fault = Change_error `Eacces;
         };
         rule `Open_write "conf" (Open_error `Erofs);
       ])
  @@ fun ~raw ~chaos ->
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

let test_seeded_change_error () =
  let s = Schedule.seeded ~probability:1. ~kinds:[ `Change_error ] ~seed:3 () in
  Alcotest.(check string)
    "printed"
    "Eio_fs.Chaos.Private.Schedule.seeded ~probability:1. ~kinds:[ \
     `Change_error ] ~seed:3 ()"
    (Fmt.str "%a" Schedule.pp s);
  with_chaos s @@ fun ~raw ~chaos ->
  expect_io "a seeded mkdir" (fun () ->
      Eio.Path.mkdir ~perm:0o755 (chaos / "d"));
  Alcotest.(check (list string)) "nothing made" [] (Eio.Path.read_dir raw)

(* A seeded schedule prints as the expression that rebuilds it. *)
let test_seeded_pp_rebuilds () =
  let s =
    Schedule.seeded ~probability:0.25
      ~kinds:[ `Torn_write; `Sync_error ]
      ~seed:42 ()
  in
  Alcotest.(check string)
    "printed"
    "Eio_fs.Chaos.Private.Schedule.seeded ~probability:0.25 ~kinds:[ \
     `Torn_write; `Sync_error ] ~seed:42 ()"
    (Fmt.str "%a" Schedule.pp s);
  let rebuilt =
    Eio_fs.Chaos.Private.Schedule.seeded ~probability:0.25
      ~kinds:[ `Torn_write; `Sync_error ]
      ~seed:42 ()
  in
  let faults s = List.map (Chaos.Internal.next s) ops_fixture in
  Alcotest.(check bool)
    "same faults" true
    (List.equal (Option.equal Fault.equal) (faults s) (faults rebuilt))

(* A probability prints exactly, so the printed schedule draws the same
   faults. *)
let test_seeded_pp_floats () =
  let printed p =
    Fmt.str "%a" Schedule.pp
      (Schedule.seeded ~probability:p ~kinds:[] ~seed:1 ())
  in
  let expect p lit =
    Alcotest.(check string)
      lit
      ("Eio_fs.Chaos.Private.Schedule.seeded ~probability:" ^ lit
     ^ " ~kinds:[] ~seed:1 ()")
      (printed p)
  in
  expect (0.1 +. 0.2) "0.30000000000000004";
  expect 0.3 "0.3";
  expect 1. "1.";
  expect infinity "infinity";
  expect neg_infinity "neg_infinity";
  expect (-0.5) "(-0.5)";
  expect 1e-300 "1e-300";
  Alcotest.(check bool)
    "the literal is the float" true
    (Float.equal (0.1 +. 0.2) 0.30000000000000004)

let test_chaos_perturbs () =
  let clean = run_workload Schedule.empty in
  Alcotest.(check (pair string string))
    "clean run succeeds fully"
    ( "ok",
      "0000000000000000000000000000000100000000000000020000000000000003000000000000000400000000000000050000000000000006000000000000000700000000000000080000000000000009"
    )
    clean;
  let perturbed =
    List.init 40 (fun seed ->
        run_workload (Schedule.seeded ~seed ~probability:0.5 ()))
  in
  Alcotest.(check bool)
    "some seed perturbs the clean result" true
    (List.exists (fun r -> r <> clean) perturbed)

(* {2 Crash / durability model} *)

module Crash = Chaos.Crash

(* [raw] is the un-perturbed base; [fs] is the same in-memory tree with
   durability tracked by [ctl]. After a crash, reads go through [raw] (or the
   handle [Crash.post_crash] returns), never through the dead [fs]. *)
let with_crash ctl fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  let fs = Crash.wrap ctl raw in
  fn ~raw ~fs

let absent p = Eio.Path.kind ~follow:false p = `Not_found

(* A controller that was never wrapped has no image to rebuild, so a crash
   is refused, and the refusal leaves it uncrashed. *)
let test_crash_unwrapped_refused () =
  let ctl = Crash.v () in
  let refused f =
    match f () with _ -> false | exception Invalid_argument _ -> true
  in
  Alcotest.(check bool)
    "crash refused" true
    (refused (fun () -> Crash.crash ctl));
  Alcotest.(check bool) "not crashed" false (Crash.is_crashed ctl);
  Alcotest.(check bool)
    "post_crash refused" true
    (refused (fun () -> ignore (Crash.post_crash ctl)))

(* A write with no intervening sync is lost after a crash -- and so is the
   file itself: its creation is an uncommitted directory operation, so no
   empty ghost file lingers. *)
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
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
  let ctl = Crash.v ~policy:(Crash.Survive (fun u -> u.offset = 0)) () in
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
  Eio_fs.run @@ fun env ->
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
  Eio_fs.run @@ fun env ->
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "read-only subtree-staged file rewritten" "data"
    (Eio.Path.load (post / "stage" / "img"));
  Alcotest.(check int)
    "mode preserved" 0o444
    (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm

(* {2 Directory-operation durability}

   Directory operations -- file creation, mkdir, unlink, rmdir, symlink,
   rename -- are journaled in program order and commit on [sync_metadata] (the
   stand-in for a directory fsync), never on a file sync: fsync(2) on a
   descriptor makes that file's data durable and says nothing about the
   directory its name lives in. A crash rolls back the uncommitted suffix,
   whole. The spec is the journal model in chaos.mli. *)

(* An unlink committed by the metadata barrier stays deleted: the crash must
   not resurrect the file from its tracked durable content. *)
let test_crash_committed_unlink_stays_deleted () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio.Path.unlink (fs / "f");
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "committed unlink stays deleted" true
    (absent (post / "f"))

(* Syncing a file commits that file's data, not the directory's entries:
   fsync(2) on a descriptor says nothing about the directory a name lives in.
   So an unlink still pending when some *other* file is synced must roll back,
   exactly as it would had the machine lost power right there.

   Conflating the two hides a class of bug rather than a corner of one. A store
   that removes a journal or a log and reaches for the file barrier instead of
   the directory barrier tests green here and loses data on real hardware: that
   is how the WAL half of a durability bug in ocaml-sqlite stayed invisible
   while the journal half was caught. *)
let test_crash_file_sync_leaves_metadata_pending () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
  Eio.Path.unlink (fs / "f");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "uncommitted unlink rolled back" "data"
    (Eio.Path.load (post / "f"))

(* The tmp-file-then-rename commit protocol with no barrier after the rename:
   the swap reverts whole. The file is back at the temporary path, the final
   path is absent -- never a mixture, never both. *)
let test_crash_uncommitted_rename_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
  Eio.Path.rename (fs / "tmp") (fs / "final");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "file back at the temporary path" "NEW"
    (Eio.Path.load (post / "tmp"));
  Alcotest.(check bool)
    "final path absent after reverted swap" true
    (absent (post / "final"))

(* The same protocol with the barrier: [sync_metadata] after the rename pins
   the swap, exactly as fsyncing the parent directory would. *)
let test_crash_sync_metadata_commits_rename () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  Eio.Path.rename (fs / "tmp") (fs / "final");
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "tmp") (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
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

(* The metadata barrier commits the journal whole: the mkdir and the creation
   inside it become durable together, and the file's synced bytes come back with
   them. Syncing the file alone would leave both directory entries pending, and
   the crash would take the whole subtree with them. *)
let test_crash_sync_metadata_commits_mkdir () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
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

(* The current-pointer swap of a release store: replacing a committed symlink
   through a temporary name reverts whole when uncommitted -- the old pointer
   target is served, and the temporary never lingers. *)
let test_crash_symlink_swap_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.symlink ~link_to:"releases/a" (fs / "current");
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
  Eio.Path.rmdir (fs / "d");
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "uncommitted rmdir rolled back" true
    (Eio.Path.kind ~follow:false (post / "d") = `Directory)

(* A directory rename carries its tracked children. Uncommitted, the whole
   subtree reverts to the staging path; committed, it lives at the final
   path -- exactly the staging-directory publish of a release store. *)
let test_crash_dir_rename_uncommitted_reverts () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "stage");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "stage" / "img")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
  Eio.Switch.run (fun sw ->
      let stage = Eio.Path.open_dir ~sw (fs / "stage") in
      Eio.Path.rename (fs / "stage") (fs / "final");
      Eio.Path.mkdir ~perm:0o755 (fs / "stage");
      Crash.sync_metadata ctl;
      Eio.Path.with_open_out ~create:(`Exclusive 0o644) (stage / "img")
        (fun file ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "synced" ];
          Eio.File.sync file;
          Crash.sync_metadata ctl;
          Eio.Path.save ~create:(`Exclusive 0o644)
            (fs / "stage" / "img")
            "other";
          Eio.Path.with_open_out ~create:`Never
            (fs / "stage" / "img")
            Eio.File.sync;
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "LOST!!" ]);
      Eio.Path.mkdir ~perm:0o755 (fs / "later");
      Crash.sync_metadata ctl);
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
  Eio_fs_sync.save (fs / "state") "v1";
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
   [Eio_fs_sync.save] included. *)
let image_of_seed seed =
  let ctl = Crash.v ~policy:(Reorder_unsynced seed) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio_fs_sync.save ~durable:false (fs / "state") "v1";
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

(* Directory operations count as durability ops for the [Nth_op] trigger. *)
let test_crash_nth_op_counts_dir_ops () =
  let ctl = Crash.v ~policy:Drop_unsynced ~trigger:(Crash.Nth_op 1) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Alcotest.(check bool) "mkdir fired the trigger" true (Crash.is_crashed ctl)

(* A file whose bytes were synced and whose name a barrier on its directory
   made durable keeps its bytes, even behind an older operation in another
   directory still pending: the image a crash rebuilds from that pending
   entry holds the file, so it must hold the file's bytes too. *)
let test_crash_synced_file_behind_older_pending_entry () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d1");
  Eio.Path.mkdir ~perm:0o755 (fs / "d2");
  Crash.sync_metadata ctl;
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "d2" / "b") "b";
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "d1" / "a")
    (fun f ->
      Eio.Flow.copy_string "a" f;
      Eio.File.sync f);
  Eio_fs_sync.sync_dir (fs / "d1");
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "a's bytes" "a" (Eio.Path.load (post / "d1" / "a"));
  Alcotest.(check string) "b's bytes" "b" (Eio.Path.load (post / "d2" / "b"))

(* After the crash fires nothing reaches the store: a directory operation
   through the dead wrapper fails as a read or a write does, and the image is
   the one the crash left. A process killed at that point does nothing more
   either, so a cleanup handler the failure runs cannot undo the crash. *)
let test_crash_stops_directory_ops () =
  let ctl =
    Crash.v ~policy:(Crash.Survive (fun _ -> true)) ~trigger:(Crash.Nth_op 4) ()
  in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "x") "x";
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.mkdir ~perm:0o755 (fs / "e");
  Alcotest.(check bool)
    "the fourth op fired the trigger" true (Crash.is_crashed ctl);
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

   [Eio_fs_sync.sync_dir] through the crash wrapper is the model's metadata
   barrier, and fsync(2) fixes what one barrier covers: syncing directory D
   makes the entries OF D durable -- the journal entries whose recorded path's
   parent is D -- and says nothing about D's own entry in its parent, nor about
   a sibling of D, nor about an entry one level further down. A barrier that
   commits the whole journal cannot tell a store that barriered the right
   directory from one that barriered any directory at all, so a missing barrier
   on one level of a mkdirs reads as durable.

   The tightening leaves correct call sites alone: a store barriers the parent
   directory of the file it just published, which is exactly the directory that
   holds that file's entry, so ocaml-sqlite's crash tests stay green. It does
   not close the window that a crash trigger on a directory operation opens
   either -- there both operations are entries of the SAME directory, so the
   barrier that commits one commits the other whatever its scope. Both gaps are
   real and they need different changes. *)

(* Syncing a directory does not commit that directory's own name: [a]'s entry
   lives in the root, and only the root's barrier makes it durable. The sibling
   [b] is pending for the same reason. *)
let test_crash_dir_sync_leaves_its_own_entry_pending () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio_fs_sync.sync_dir (fs / "a");
  Crash.crash ctl;
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
  Eio_fs_sync.sync_dir fs;
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "a" / "f")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio_fs_sync.sync_dir (fs / "a");
  Crash.crash ctl;
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
  Eio_fs_sync.sync_dir fs;
  Crash.crash ctl;
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
  Eio_fs_sync.sync_dir fs;
  Eio.Path.mkdir ~perm:0o755 (fs / "a" / "sub");
  Eio.Path.with_open_out ~create:(`Exclusive 0o644)
    (fs / "a" / "sub" / "g")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
      Eio.File.sync file);
  Eio_fs_sync.sync_dir (fs / "a");
  Crash.crash ctl;
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
  Crash.sync_metadata ctl;
  streaming_writes fs;
  Alcotest.(check (list (pair int int)))
    "logged where they landed"
    [ (2, 2); (6, 2) ]
    (List.map (fun u -> (u.Crash.offset, u.Crash.length)) (Crash.unsynced ctl));
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "replayed where they landed" "abXYefgh"
    (Eio.Path.load (post / "f"))

(* The schedule is told the same offsets. *)
let test_streaming_write_offsets () =
  let offsets = ref [] in
  let schedule =
    Schedule.observing
      (fun op ->
        if op.Op.kind = `Write then offsets := op.file_offset :: !offsets)
      Schedule.empty
  in
  with_chaos schedule @@ fun ~raw ~chaos ->
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
  Crash.sync_metadata ctl;
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
  Crash.sync_metadata ctl;
  Eio.Path.rename (fs / "f") (fs / "g");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "x" / "h") "h";
  Eio_fs_sync.sync_dir (fs / "x");
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
  Crash.sync_metadata ctl;
  Eio.Path.rename (fs / "b") (fs / "a");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "x" / "h") "h";
  Eio_fs_sync.sync_dir (fs / "x");
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
  Crash.sync_metadata ctl;
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

(* Run [fn] over a crash-wrapped private temp directory on the real
   filesystem, then hand the recovered directory to [check]. *)
let with_real_crash ctl fn ~check =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-crash" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  let real = (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t) in
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () ->
  let wrapped = Crash.wrap ctl real in
  fn wrapped;
  check (Crash.post_crash ctl)

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
      Crash.sync_metadata ctl)
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
      Crash.sync_metadata ctl)
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: read-only subtree-staged file rewritten" "data"
        (Eio.Path.load (post / "stage" / "img"));
      Alcotest.(check int)
        "real fs: mode preserved" 0o444
        (Eio.Path.stat ~follow:true (post / "stage" / "img")).perm)

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
      Crash.sync_metadata ctl;
      Eio.Path.rename (fs / "tmp") (fs / "final"))
    ~check:(fun post ->
      Alcotest.(check string)
        "real fs: file back at the temporary path" "NEW"
        (Eio.Path.load (post / "tmp"));
      Alcotest.(check bool)
        "real fs: final path absent" true
        (absent (post / "final")))

(* {2 A file is the file, whatever name opened it} *)

let write_new ?(sync = false) p data =
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) p (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string data ];
      if sync then Eio.File.sync f)

let overwrite p data =
  Eio.Path.with_open_out ~create:`Never p (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string data ])

(* An unsynced write through a symbolic link is a write to its target. *)
let test_crash_write_through_a_symlink () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  write_new (raw / "real") "old";
  Eio.Path.symlink ~link_to:"real" (raw / "link");
  let fs = Crash.wrap ctl raw in
  overwrite (fs / "link") "NEW";
  Eio.Path.mkdir ~perm:0o755 (fs / "x");
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the target's write is lost" "old"
    (Eio.Path.load (post / "real"))

(* fsync(2) flushes the file whatever name opened it. *)
let test_crash_sync_through_another_spelling () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "f") "";
  Eio_fs_sync.sync_dir fs;
  overwrite (fs / "f") "data";
  Eio.Switch.run (fun sw ->
      let h = Eio.Path.open_in ~sw (fs / "./f") in
      Eio_fs_sync.Barrier.(sync_file portable) h);
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "durable" "data" (Eio.Path.load (post / "f"))

(* Opening a file under another spelling makes none of its bytes durable. *)
let test_crash_open_through_another_spelling () =
  List.iter
    (fun alias ->
      let ctl = Crash.v ~policy:Drop_unsynced () in
      with_crash ctl @@ fun ~raw:_ ~fs ->
      write_new ~sync:true (fs / "f") "";
      Eio_fs_sync.sync_dir fs;
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
  Crash.sync_metadata ctl;
  write_new ~sync:true (fs / "d" / "f") "data";
  Eio.Switch.run (fun sw ->
      let h = Eio.Path.open_in ~sw (fs / "d") in
      Eio_fs_sync.Barrier.(sync_file portable) h);
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
  Eio_fs_sync.sync_dir (fs / "a");
  write_new ~sync:true (fs / "a" / "g") "late";
  Eio_fs_sync.sync_dir fs;
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
  Eio_fs_sync.sync_dir fs;
  write_new ~sync:true (fs / "a" / "f") "data";
  Eio_fs_sync.sync_dir (fs / "a");
  Eio.Path.rename (fs / "a" / "f") (fs / "b" / "f");
  Eio_fs_sync.sync_dir (fs / "b");
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
    Eio_fs_sync.sync_dir fs;
    let sched =
      Schedule.scripted
        [
          {
            trigger = Op_on_path { kind = `Sync; path = "f" };
            fault = Sync_error `Eio;
          };
        ]
    in
    let cfs = Chaos.wrap sched fs in
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
  Eio_fs.run @@ fun env ->
  let sched =
    Schedule.scripted
      [
        {
          trigger = Nth_op_on_path { n = 2; kind = `Sync; path = "f" };
          fault = Sync_error `Eio;
        };
      ]
  in
  let fs = Crash.wrap ctl (Chaos.wrap sched env#fs) in
  write_new ~sync:true (fs / "f") "";
  Eio_fs_sync.sync_dir fs;
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "record" ];
      (try Eio.File.sync f with Eio.Io _ -> ());
      Eio.File.sync f);
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "lost" "" (Eio.Path.load (post / "f"))

(* {2 Writeback in blocks} *)

(* The images seeds 0 to 199 rebuild of one workload. *)
let images ?block_size workload =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 199 do
    let ctl = Crash.v ?block_size ~policy:(Reorder_unsynced seed) () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "";
    Eio_fs_sync.sync_dir fs;
    workload fs;
    let post = Crash.post_crash ctl in
    Hashtbl.replace seen (Eio.Path.load (post / "f")) ()
  done;
  Hashtbl.fold (fun k () acc -> k :: acc) seen []

(* A write of three blocks is not atomic: a crash keeps any of its blocks. *)
let test_crash_multi_block_write () =
  let block = 4096 in
  let data =
    String.init (3 * block) (fun i -> Char.chr (65 + Int.div i block))
  in
  let states = images (fun fs -> overwrite (fs / "f") data) in
  let partial s = s <> "" && s <> data in
  Alcotest.(check bool)
    "some state is partial" true
    (List.exists partial states);
  Alcotest.(check bool)
    "some state is a block prefix" true
    (List.mem (String.sub data 0 block) states);
  Alcotest.(check bool)
    "blocks come whole" true
    (List.for_all
       (fun s ->
         let ok = ref true in
         for b = 0 to Int.div (String.length s - 1) block do
           let blk =
             String.sub s (b * block)
               (min block (String.length s - (b * block)))
           in
           let mine = String.make (String.length blk) (Char.chr (65 + b)) in
           let zero = String.make (String.length blk) '\000' in
           if blk <> mine && blk <> zero then ok := false
         done;
         !ok)
       states)

(* Two appends in one block: the block is written back whole, so the second
   never lands without the first. *)
let appends fs =
  Eio.Path.with_open_out ~append:true ~create:`Never (fs / "f") (fun f ->
      Eio.Flow.copy_string "aaaa" f;
      Eio.Flow.copy_string "bbbb" f)

let test_crash_appends_in_one_block () =
  let states = images appends in
  Alcotest.(check bool)
    "no second append over a hole" false
    (List.mem "\000\000\000\000bbbb" states);
  Alcotest.(check bool) "both appends" true (List.mem "aaaabbbb" states);
  Alcotest.(check bool) "first append" true (List.mem "aaaa" states)

(* With blocks of four bytes the two appends are separate blocks. *)
let test_crash_block_size () =
  let states = images ~block_size:4 appends in
  Alcotest.(check bool)
    "the second append alone" true
    (List.mem "\000\000\000\000bbbb" states)

(* A rename over a file whose handle stays open: the write synced through
   the handle is durable in that file, which an uncommitted rename leaves at
   its name. *)
let test_crash_rename_over_an_open_file () =
  let ctl = Crash.v () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "b") "old";
  write_new ~sync:true (fs / "a") "other";
  Crash.sync_metadata ctl;
  Eio.Switch.run (fun sw ->
      let h = Eio.Path.open_out ~sw ~create:`Never (fs / "b") in
      Eio.File.pwrite_all h ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.Path.rename (fs / "a") (fs / "b");
      Eio.File.sync h);
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "b" "NEW" (Eio.Path.load (post / "b"));
  Alcotest.(check string) "a" "other" (Eio.Path.load (post / "a"))

(* A file created through a dangling symbolic link is a new entry at the
   link's target, journaled as any create. *)
let test_crash_create_through_a_dangling_link () =
  let ctl = Crash.v () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.symlink ~link_to:"target" (fs / "l");
  Eio.Path.with_open_out ~create:(`If_missing 0o644) (fs / "l") (fun f ->
      Eio.Flow.copy_string "data" f;
      Eio.File.sync f);
  Alcotest.(check bool)
    "the create is pending" true
    (List.exists
       (fun (u : Crash.unsynced) -> u.kind = Crash.Create)
       (Crash.unsynced ctl));
  Eio_fs_sync.sync_dir fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "target" "data" (Eio.Path.load (post / "target"))

(* The model over a user's view reads and rebuilds the tree as root: the
   user cannot read root's files, and may lock themselves out of a
   directory. *)
let alice = { Eio_fs.uid = 1000; gid = 1000; groups = []; umask = 0o022 }

let test_crash_over_a_user_view () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "shadow") "s";
  Eio.Path.mkdir ~perm:0o755 (root / "home");
  Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (root / "home");
  let a = Eio_fs.Fs.as_user alice root in
  ignore (Crash.wrap (Crash.v ()) a);
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl (a / "home") in
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  write_new ~sync:true (fs / "d/f") "x";
  Crash.sync_metadata ctl;
  Eio.Path.chmod ~follow:true ~perm:0o600 (fs / "d");
  Eio.Path.mkdir ~perm:0o755 (fs / "e");
  Eio.Path.chmod ~follow:true ~perm:0o000 (fs / "e");
  Eio.Path.mkdir ~perm:0o755 (fs / "g");
  Crash.crash ctl;
  Alcotest.(check (list string))
    "the durable tree" [ "d" ]
    (List.sort String.compare (Eio.Path.read_dir (root / "home")));
  Alcotest.(check string) "d/f" "x" (Eio.Path.load (root / "home/d/f"))

(* A crash rebuilds each entry with its owner. *)
let test_crash_restores_owners () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl root in
  write_new ~sync:true (fs / "f") "x";
  Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (fs / "f");
  Eio.Path.mkdir ~perm:0o700 (fs / "d");
  Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (fs / "d");
  Eio.Path.symlink ~link_to:"f" (fs / "l");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (fs / "l");
  Crash.sync_metadata ctl;
  let post = Crash.post_crash ctl in
  let owner p =
    let s = Eio.Path.stat ~follow:false (post / p) in
    (s.uid, s.gid)
  in
  List.iter
    (fun p -> Alcotest.(check (pair int64 int64)) p (1000L, 1000L) (owner p))
    [ "f"; "d"; "l" ];
  Eio.Path.save ~create:`Never (Eio_fs.Fs.as_user alice post / "f") "y"

(* A barrier deferred on a directory belongs to that directory: once it is
   removed or renamed, a new directory of the same name does not inherit it. *)
let test_crash_deferred_barrier_identity () =
  let replaced how =
    let ctl = Crash.v () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    write_new ~sync:true (fs / "d/f") "x";
    Eio_fs_sync.sync_dir (fs / "d");
    how fs;
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    Eio_fs_sync.sync_dir fs;
    Eio_fs_sync.sync_dir (fs / "d");
    let post = Crash.post_crash ctl in
    ( Eio.Path.read_dir (post / "d"),
      List.sort String.compare (Eio.Path.read_dir post) )
  in
  let removed, _ =
    replaced (fun fs ->
        Eio.Path.unlink (fs / "d/f");
        Eio.Path.rmdir (fs / "d"))
  in
  Alcotest.(check (list string)) "after rmdir, the new d" [] removed;
  let renamed, top =
    replaced (fun fs -> Eio.Path.rename (fs / "d") (fs / "x"))
  in
  Alcotest.(check (list string)) "after rename, the new d" [] renamed;
  Alcotest.(check (list string)) "the root" [ "d"; "x" ] top

(* A change of mode or owner is metadata: lost unless a sync of the file, or
   a barrier on the directory itself, commits it. A barrier on the parent
   does not: ext4's fsync of a directory completes only its own last
   transaction. *)
let test_crash_chmod_journaled () =
  let mode ~sync =
    let ctl = Crash.v () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "k";
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    Crash.sync_metadata ctl;
    Eio.Path.chmod ~follow:true ~perm:0o600 (fs / "f");
    Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (fs / "f");
    Eio.Path.chmod ~follow:true ~perm:0o700 (fs / "d");
    sync ctl fs;
    let post = Crash.post_crash ctl in
    let st p = Eio.Path.stat ~follow:false (post / p) in
    ((st "f").perm, (st "f").uid, (st "d").perm)
  in
  let check what expected got =
    Alcotest.(check (triple int int64 int)) what expected got
  in
  check "no sync" (0o644, 0L, 0o755) (mode ~sync:(fun _ _ -> ()));
  check "a sync of the file" (0o600, 1000L, 0o755)
    (mode ~sync:(fun _ fs ->
         Eio.Path.with_open_out ~create:`Never (fs / "f") Eio.File.sync));
  check "a barrier on the parent" (0o644, 0L, 0o755)
    (mode ~sync:(fun _ fs -> Eio_fs_sync.sync_dir fs));
  check "a barrier on the directory itself" (0o644, 0L, 0o700)
    (mode ~sync:(fun _ fs -> Eio_fs_sync.sync_dir (fs / "d")))

(* A sync makes a new file's mode durable before its name: whenever a crash
   keeps the name, the file has that mode. *)
let test_crash_synced_chmod_of_a_new_file () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun f ->
      Eio.Path.chmod ~follow:true ~perm:0o600 (fs / "f");
      Eio.File.sync f);
  let post = Crash.post_crash ctl in
  Alcotest.(check int)
    "mode" 0o600 (Eio.Path.stat ~follow:false (post / "f")).perm

(* A sync makes the file's mode durable wherever the file's name is: a
   pending rename, a rename onto itself, or a later directory of the old name
   change nothing. *)
let test_crash_synced_mode_follows_the_file () =
  let ctl = Crash.v () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "b") "x";
  Crash.sync_metadata ctl;
  Eio.Path.chmod ~follow:true ~perm:0o777 (fs / "b");
  Eio.Path.rename (fs / "b") (fs / "l");
  Eio.Path.chmod ~follow:true ~perm:0o700 (fs / "l");
  Eio.Path.rename (fs / "l") (fs / "l");
  Eio.Path.with_open_out ~create:`Never (fs / "l") Eio.File.sync;
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.chmod ~follow:true ~perm:0o711 (fs / "d");
  Eio.Path.rmdir (fs / "d");
  write_new ~sync:true (fs / "d") "y";
  let post = Crash.post_crash ctl in
  Alcotest.(check int) "b" 0o700 (Eio.Path.stat ~follow:false (post / "b")).perm;
  Alcotest.(check bool) "d" true (absent (post / "d"))

(* The model knows a file and a directory by inode, not by name: a user's
   file renamed over root's is never rewritten into root's, a file planted
   in a directory made again under a moved one's name never lands in the old
   one, and a mode synced through a read-only handle, or after a rename, is
   the file's. *)
let test_crash_inode_identity () =
  let setup () =
    let root = Eio_fs.Fs.v () in
    Eio.Path.mkdir ~perm:0o755 (root / "u");
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (root / "u");
    root
  in
  let render post p = try Eio.Path.load (post / p) with Eio.Io _ -> "-" in
  ( Eio_mock.Backend.run @@ fun () ->
    let root = setup () in
    Eio.Path.save ~create:(`Exclusive 0o644) (root / "u/passwd") "root";
    Eio.Path.save ~create:(`Exclusive 0o644) (root / "u/mine") "alice";
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (root / "u/mine");
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice root) in
    Eio.Path.rename (fs / "u/mine") (fs / "u/passwd");
    Eio.Path.with_open_out ~create:`Never (fs / "u/passwd") ignore;
    let post = Crash.post_crash ctl in
    Alcotest.(check (pair string string))
      "root's file keeps its bytes" ("root", "alice")
      (render post "u/passwd", render post "u/mine") );
  ( Eio_mock.Backend.run @@ fun () ->
    let root = setup () in
    Eio.Path.mkdir ~perm:0o755 (root / "u/d");
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice root) in
    Eio.Path.rename (fs / "u/d") (fs / "u/x");
    Eio.Path.mkdir ~perm:0o755 (fs / "u/d");
    write_new ~sync:true (fs / "u/d/planted") "evil";
    Eio_fs_sync.sync_dir (fs / "u/d");
    let post = Crash.post_crash ctl in
    Alcotest.(check (list string))
      "root's directory is untouched" []
      (Eio.Path.read_dir (post / "u/d")) );
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "f") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "g") "y";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl root in
  Eio.Path.chmod ~follow:false ~perm:0o600 (fs / "f");
  Eio.Switch.run (fun sw ->
      Eio_fs_sync.Barrier.(sync_file portable) (Eio.Path.open_in ~sw (fs / "f")));
  Eio.Path.chmod ~follow:false ~perm:0o600 (fs / "g");
  Eio.Path.rename (fs / "g") (fs / "h");
  Eio.Path.with_open_out ~create:`Never (fs / "h") Eio.File.sync;
  let post = Crash.post_crash ctl in
  let mode p = (Eio.Path.stat ~follow:false (post / p)).perm in
  Alcotest.(check (pair int int)) "modes" (0o600, 0o600) (mode "f", mode "g")

(* An entry takes effect whole, with the pending entries it needs, and a
   rename moves the image's own node: synced bytes follow their file to a
   renamed directory, a durable file survives a rename out of a directory
   later removed but is lost with a new directory whose parent was never
   synced, and a moved directory carries only what the image holds, with an
   earlier rename in the same source directory. *)
let test_crash_entries_whole () =
  let run setup ops =
    let ctl = Crash.v () in
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    setup raw;
    let fs = Crash.wrap ctl raw in
    ops fs;
    let post = Crash.post_crash ctl in
    let rec walk p acc =
      List.fold_left
        (fun acc n ->
          let q = Eio.Path.(post / p / n) in
          let key = if p = "" then n else p ^ "/" ^ n in
          match Eio.Path.kind ~follow:false q with
          | `Directory -> walk key ((key ^ "/") :: acc)
          | _ -> (key ^ "=" ^ Eio.Path.load q) :: acc)
        acc
        (List.sort String.compare (Eio.Path.read_dir Eio.Path.(post / p)))
    in
    List.rev (walk "" [])
  in
  let dirs raw l =
    List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) l
  in
  let save raw p s = Eio.Path.save ~create:(`Exclusive 0o644) (raw / p) s in
  let check what expected got =
    Alcotest.(check (list string)) what expected got
  in
  check "synced bytes in a renamed directory" [ "D/"; "D/f=synced" ]
    (run
       (fun raw -> dirs raw [ "D" ])
       (fun fs ->
         Eio.Path.rename (fs / "D") (fs / "E");
         write_new ~sync:true (fs / "E/f") "synced";
         Eio_fs_sync.sync_dir (fs / "E")));
  check "a move out of a directory then removed"
    [ "P/"; "Q/"; "Q/C/"; "Q/C/x=durable" ]
    (run
       (fun raw ->
         dirs raw [ "P"; "P/A"; "P/A/B"; "Q" ];
         save raw "P/A/B/x" "durable")
       (fun fs ->
         Eio.Path.rename (fs / "P/A/B") (fs / "Q/C");
         Eio.Path.rmdir (fs / "P/A");
         Eio_fs_sync.sync_dir (fs / "P")));
  check "a rename into a new directory, its parent never synced" [ "P/" ]
    (run
       (fun raw ->
         dirs raw [ "P" ];
         save raw "P/f" "durable")
       (fun fs ->
         Eio.Path.mkdir ~perm:0o755 (fs / "Q");
         Eio.Path.rename (fs / "P/f") (fs / "Q/f");
         Eio_fs_sync.sync_dir (fs / "P")));
  check "a moved directory carries only what the image holds"
    [ "A/"; "B/"; "B/e/"; "B/e/f=data" ]
    (run
       (fun raw ->
         dirs raw [ "A"; "A/d"; "B" ];
         save raw "A/f" "data")
       (fun fs ->
         Eio.Path.rename (fs / "A/f") (fs / "A/d/f");
         write_new ~sync:true (fs / "A/d/new") "x";
         Eio.Path.rename (fs / "A/d") (fs / "B/e");
         Eio_fs_sync.sync_dir (fs / "B")))

(* A crash gives each entry its owner before its mode: a chown clears
   setuid and setgid, which the mode then sets back. *)
let test_crash_keeps_privilege_bits () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "suid") "x";
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "suid");
  Eio.Path.chmod ~follow:false ~perm:0o6755 (raw / "suid");
  Eio.Path.mkdir ~perm:0o755 (raw / "sg");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "sg");
  Eio.Path.chmod ~follow:false ~perm:0o2775 (raw / "sg");
  let ctl = Crash.v () in
  ignore (Crash.wrap ctl raw);
  let post = Crash.post_crash ctl in
  let mode p = (Eio.Path.stat ~follow:false (post / p)).perm in
  Alcotest.(check (pair int int))
    "modes" (0o6755, 0o2775)
    (mode "suid", mode "sg")

(* A user's write clears setuid; the sync after it makes that mode durable. *)
let test_crash_sync_records_the_mode () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "suid") "x";
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "suid");
  Eio.Path.chmod ~follow:false ~perm:0o4755 (raw / "suid");
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
  Eio.Path.with_open_out ~create:`Never (fs / "suid") (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "y" ];
      Eio.File.sync f);
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair int string))
    "mode and bytes" (0o755, "y")
    ( (Eio.Path.stat ~follow:false (post / "suid")).perm,
      Eio.Path.load (post / "suid") )

(* A file truncated and rewritten, unsynced: writeback may land the new
   length over the block from before the truncation, so the bytes it cut
   come back; at the old length, the block the truncation would free holds
   its old bytes. Every outcome is one of those. *)
let test_crash_truncation_follows_writeback () =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 199 do
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "dgjutfwss";
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.save ~create:(`Or_truncate 0o644) (fs / "f") "wtw";
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  Alcotest.(check (list string))
    "outcomes"
    (List.sort String.compare [ ""; "dgj"; "dgjutfwss"; "wtw" ])
    (List.sort String.compare (List.of_seq (Hashtbl.to_seq_keys seen)))

(* A file's sync commits the file's mode, not an earlier directory operation
   on another name. *)
let test_crash_sync_commits_the_mode_alone () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "x") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "y") "y";
  let ctl = Crash.v ~policy:Drop_unsynced () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.unlink (fs / "x");
  Eio.Path.chmod ~follow:false ~perm:0o755 (fs / "y");
  Eio.Path.with_open_out ~create:`Never (fs / "y") Eio.File.sync;
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair bool int))
    "x kept, y's mode" (true, 0o755)
    ( Eio.Path.is_file (post / "x"),
      (Eio.Path.stat ~follow:false (post / "y")).perm )

(* A directory opened, renamed, and another made at its old name: syncing
   the handle commits the directory it opened, not the one at that name. *)
let test_crash_dir_handle_barrier_by_inode () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) [ "a"; "b"; "a/d" ];
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Switch.run @@ fun sw ->
  let h = Eio.Path.open_in ~sw (fs / "a/d") in
  Eio.Path.rename (fs / "a/d") (fs / "b/d");
  Eio.Path.mkdir ~perm:0o755 (fs / "a/d");
  Eio_fs_sync.sync_dir (fs / "a");
  Eio_fs_sync.sync_dir (fs / "b");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "b/d/x") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "a/d/y") "y";
  (match Eio_fs_sync.file_barrier h with
  | Some barrier -> barrier ()
  | None -> Alcotest.fail "no barrier");
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair bool bool))
    "b/d/x kept, a/d/y lost" (true, false)
    (Eio.Path.is_file (post / "b/d/x"), Eio.Path.is_file (post / "a/d/y"))

(* A barrier on a new directory, deferred until the directory is durable:
   wherever the cut falls, the directory never shows without the file the
   barrier covered. *)
let test_crash_deferred_barrier_whole () =
  for cut = 0 to 6 do
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    let keep (u : Crash.unsynced) = u.kind = Data || u.seq <= cut in
    let ctl = Crash.v ~policy:(Survive keep) () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.mkdir ~perm:0o755 (fs / "c");
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    Eio.Path.save ~create:(`Exclusive 0o644) (fs / "d/x") "x";
    Eio.Path.rename (fs / "d/x") (fs / "c/x");
    Eio_fs_sync.sync_dir (fs / "c");
    let post = Crash.post_crash ctl in
    if
      Eio.Path.is_directory (post / "c")
      && not (Eio.Path.is_file (post / "c/x"))
    then Alcotest.failf "cut %d: c without c/x" cut
  done

(* A directory moved into a directory that an earlier pending rename took
   out of it: the barrier commits that rename too, rather than dropping the
   directory into itself. *)
let test_crash_move_needs_ancestor_moves () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) [ "a"; "b"; "a/m" ];
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "a/m/f") "0";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.rename (fs / "a/m") (fs / "b/m");
  Eio.Path.rename (fs / "a") (fs / "b/m/a");
  Eio_fs_sync.sync_dir fs;
  let image =
    List.map
      (fun (p, _, k) ->
        match k with
        | `Dir -> p ^ "/"
        | `File s -> p ^ "=" ^ s
        | `Link t -> p ^ "->" ^ t)
      (Eio_fs.Chaos.Private.Internal.crash_image
         (Crash.crash ctl;
          ctl))
    |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "image"
    [ "b/"; "b/m/"; "b/m/a/"; "b/m/f=0" ]
    image

(* A synced file in a new directory, and a barrier on that directory: the
   directory itself still needs a barrier on its parent. *)
let test_crash_new_dir_needs_parent_barrier () =
  let run ~parent =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    write_new ~sync:true (fs / "d/f") "x";
    Eio_fs_sync.sync_dir (fs / "d");
    if parent then Eio_fs_sync.sync_dir fs;
    Eio.Path.is_file (Crash.post_crash ctl / "d/f")
  in
  Alcotest.(check (pair bool bool))
    "without, then with the parent's barrier" (false, true)
    (run ~parent:false, run ~parent:true)

(* A file durable before the wrapper, first opened after an uncommitted
   rename of itself or of a directory above it: what it syncs lands in the
   file the image holds under its old name. *)
let test_crash_preexisting_found_by_inode () =
  let run ~dir policy =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.mkdir ~perm:0o755 (raw / "d");
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "d/f") "OLD";
    let ctl = Crash.v ~policy () in
    let fs = Crash.wrap ctl raw in
    let now =
      if dir then (
        Eio.Path.rename (fs / "d") (fs / "e");
        "e/f")
      else (
        Eio.Path.rename (fs / "d/f") (fs / "d/g");
        "d/g")
    in
    Eio.Path.with_open_out ~create:`Never (fs / now) (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
        if policy = Crash.Drop_unsynced then Eio.File.sync f);
    Eio.Path.load (Crash.post_crash ctl / "d/f")
  in
  let keep_data = Crash.Survive (fun u -> u.kind = Data) in
  Alcotest.(check (list string))
    "renamed file, renamed directory, then a surviving write"
    [ "NEW"; "NEW"; "NEW" ]
    [
      run ~dir:false Drop_unsynced;
      run ~dir:true Drop_unsynced;
      run ~dir:false keep_data;
    ]

(* A file moved out of a directory, then another directory renamed over it:
   the barrier that commits the rename commits the move that emptied the
   directory it replaces, so the file is not lost with it. *)
let test_crash_rename_over_filled_dir () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) [ "a"; "b" ];
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "b/g") "G";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.rename (fs / "b/g") (fs / "a/g");
  Eio.Path.rename (fs / "a") (fs / "b");
  Eio_fs_sync.sync_dir fs;
  Alcotest.(check string)
    "b/g" "G"
    (Eio.Path.load (Crash.post_crash ctl / "b/g"))

(* A directory opened, emptied, removed and made again: a barrier through
   the opened directory commits the removed one's own entries, not the new
   one's. *)
let test_crash_subtree_barrier_by_inode () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o755 (raw / "a");
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "a/old") "O";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Switch.run @@ fun sw ->
  let d = Eio.Path.open_dir ~sw (fs / "a") in
  Eio.Path.unlink (fs / "a/old");
  Eio.Path.rmdir (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "a/g") "G";
  Eio_fs_sync.sync_dir d;
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair bool bool))
    "a/old and a/g" (false, false)
    (Eio.Path.is_file (post / "a/old"), Eio.Path.is_file (post / "a/g"))

(* A file truncated to nothing, then written past its end: the hole the
   write leaves can come back zeroed, as written back, and not only with the
   bytes the truncation cut. *)
let test_crash_hole_lands_zeroed () =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 199 do
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "ABCDEFGHIJ";
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) ~block_size:1 () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        Eio.File.truncate f (i63 0);
        Eio.File.pwrite_all f ~file_offset:(i63 2) [ Cstruct.of_string "x" ]);
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  Alcotest.(check (pair bool bool))
    "zeroed hole, stale bytes" (true, true)
    (Hashtbl.mem seen "\000\000x", Hashtbl.mem seen "ABx")

(* A user's write clears setgid: a crash keeps the old mode, unless
   sync_metadata committed the journal since. *)
let test_crash_sync_metadata_commits_cleared_mode () =
  let run ~commit =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "g") "KLM";
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "g");
    Eio.Path.chmod ~follow:false ~perm:0o2775 (raw / "g");
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
    Eio.Path.with_open_out ~create:`Never (fs / "g") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "x" ]);
    if commit then Crash.sync_metadata ctl;
    (Eio.Path.stat ~follow:false (Crash.post_crash ctl / "g")).perm
  in
  Alcotest.(check (pair int int))
    "without, then with sync_metadata" (0o2775, 0o775)
    (run ~commit:false, run ~commit:true)

(* A chmod of a new directory, then a barrier on it: the chmod is listed as
   unsynced until the directory is durable, and survives with it whatever a
   predicate says. *)
let test_crash_attrs_wait_for_their_node () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  let keep (u : Crash.unsynced) = u.kind = Mkdir in
  let ctl = Crash.v ~policy:(Survive keep) () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.chmod ~follow:false ~perm:0o700 (fs / "a");
  Eio_fs_sync.sync_dir (fs / "a");
  let kinds =
    List.map
      (fun (u : Crash.unsynced) -> Fmt.str "%a" Crash.pp_unsynced u)
      (Crash.unsynced ctl)
  in
  Alcotest.(check int) "mkdir and chmod unsynced" 2 (List.length kinds);
  Alcotest.(check int)
    "mode" 0o700 (Eio.Path.stat ~follow:false (Crash.post_crash ctl / "a")).perm

(* A truncation is metadata: a failed sync does not lose it, and a later
   sync makes it durable, with the bytes the page cache then holds. *)
let test_crash_failed_sync_keeps_truncation () =
  let run init work =
    let ctl = Crash.v ~policy:Drop_unsynced () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") init;
    Eio_fs_sync.sync_dir fs;
    let sched =
      Schedule.scripted
        [
          {
            trigger = Op_on_path { kind = `Sync; path = "f" };
            fault = Sync_error `Eio;
          };
        ]
    in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        Eio.File.truncate f (i63 0);
        (try Eio.File.sync f with Eio.Io _ -> ());
        work f;
        Eio.File.sync f);
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  let big = String.make 8192 'A' in
  Alcotest.(check (list string))
    "sync again, regrow, write past the cut"
    [ ""; "\000"; String.make 4096 '\000' ^ "c" ]
    [
      run "AB" ignore;
      run "AB" (fun f -> Eio.File.truncate f (i63 1));
      run big (fun f ->
          Eio.File.pwrite_all f ~file_offset:(i63 4096)
            [ Cstruct.of_string "c" ]);
    ]

(* A record lost to a failed sync, then another in the same page and a sync
   that succeeds: the page cache may have kept the lost record, which lands
   with the new one, or evicted it, so the new one lands over the old bytes. *)
let test_crash_lost_page_kept_or_evicted () =
  let seen = Hashtbl.create 4 in
  for seed = 0 to 199 do
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "";
    Eio_fs_sync.sync_dir fs;
    let sched =
      Schedule.scripted
        [
          {
            trigger = Op_on_path { kind = `Sync; path = "f" };
            fault = Sync_error `Eio;
          };
        ]
    in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "rec1" ];
        (try Eio.File.sync f with Eio.Io _ -> ());
        Eio.File.pwrite_all f ~file_offset:(i63 4) [ Cstruct.of_string "rec2" ];
        Eio.File.sync f);
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  Alcotest.(check (list string))
    "outcomes"
    [ "\000\000\000\000rec2"; "rec1rec2" ]
    (List.sort String.compare (List.of_seq (Hashtbl.to_seq_keys seen)))

(* A user's write clears setuid, which ext4 journals at once: a crash with
   no sync may keep it, and a later barrier that commits anything after it
   carries it. *)
let test_crash_clear_journaled_at_write () =
  let run ~barrier policy =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "X";
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "f");
    Eio.Path.chmod ~follow:false ~perm:0o4755 (raw / "f");
    Eio.Path.chmod ~follow:false ~perm:0o777 raw;
    let ctl = Crash.v ~policy () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "Y" ]);
    if barrier then begin
      Eio.Path.mkdir ~perm:0o755 (fs / "n");
      Eio_fs_sync.sync_dir fs
    end;
    (Eio.Path.stat ~follow:false (Crash.post_crash ctl / "f")).perm
  in
  let keep_all = Crash.Survive (fun _ -> true) in
  Alcotest.(check (list int))
    "no sync, kept; no sync, dropped; a later barrier" [ 0o755; 0o4755; 0o755 ]
    [
      run ~barrier:false keep_all;
      run ~barrier:false Drop_unsynced;
      run ~barrier:true Drop_unsynced;
    ]

(* A truncation a failed sync keeps carries its length only: the bytes in
   its block are the durable ones, not those of a write the sync lost. *)
let test_crash_kept_truncation_length_only () =
  let run policy work =
    let ctl = Crash.v ~policy () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "WXYZ";
    Eio_fs_sync.sync_dir fs;
    let sched =
      Schedule.scripted
        [
          {
            trigger = Op_on_path { kind = `Sync; path = "f" };
            fault = Sync_error `Eio;
          };
        ]
    in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        work f;
        (try Eio.File.sync f with Eio.Io _ -> ());
        Eio.File.sync f);
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  let write f off s =
    Eio.File.pwrite_all f ~file_offset:(i63 off) [ Cstruct.of_string s ]
  and trunc f n = Eio.File.truncate f (i63 n) in
  let keep_all = Crash.Survive (fun _ -> true) in
  Alcotest.(check (list string))
    "write then cut, cut then write, a cut that keeps the length"
    [ "W"; "W"; "W"; "WXYZ" ]
    [
      run Drop_unsynced (fun f ->
          write f 0 "a";
          trunc f 1);
      run keep_all (fun f ->
          write f 0 "a";
          trunc f 1);
      run Drop_unsynced (fun f ->
          trunc f 1;
          write f 0 "a";
          trunc f 1);
      run Drop_unsynced (fun f ->
          write f 0 "a";
          trunc f 4);
    ]

(* A second wrap would reset the durable image and split the files' state
   between two wrappers: it is refused. *)
let test_crash_wrap_once () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  let ctl = Crash.v () in
  ignore (Crash.wrap ctl raw);
  match Crash.wrap ctl raw with
  | _ -> Alcotest.fail "a second wrap was accepted"
  | exception Invalid_argument _ -> ()

(* A writeback that fails only in part writes the pages that succeeded:
   Reorder_unsynced keeps any subset of the blocks a failed sync lost, and
   Drop_unsynced none. *)
let test_crash_partial_writeback () =
  let run policy =
    let ctl = Crash.v ~policy ~block_size:2 () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "aaaa";
    Eio_fs_sync.sync_dir fs;
    let sched =
      Schedule.scripted
        [
          {
            trigger = Op_on_path { kind = `Sync; path = "f" };
            fault = Sync_error `Eio;
          };
        ]
    in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "BBCC" ];
        try Eio.File.sync f with Eio.Io _ -> ());
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  let seen = Hashtbl.create 4 in
  for seed = 0 to 199 do
    Hashtbl.replace seen (run (Reorder_unsynced seed)) ()
  done;
  Alcotest.(check string) "drop" "aaaa" (run Drop_unsynced);
  Alcotest.(check (list string))
    "reorder"
    [ "BBCC"; "BBaa"; "aaCC"; "aaaa" ]
    (List.sort String.compare (List.of_seq (Hashtbl.to_seq_keys seen)))

(* A truncation kept by a failed sync is in the page cache already: a
   second failure leaves the cache as it is, so a later write past its end
   dirties only its own blocks, and the bytes the first failure lost stay
   lost. *)
let test_crash_second_failure () =
  let ctl = Crash.v ~policy:Drop_unsynced ~block_size:2 () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "f") "xyz";
  Eio_fs_sync.sync_dir fs;
  let sched =
    Schedule.seeded ~probability:1.0 ~kinds:[ `Sync_error ] ~seed:0 ()
  in
  let cfs = Chaos.wrap sched fs in
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Eio.File.truncate f (i63 1);
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "bb" ]);
  for _ = 1 to 2 do
    try Eio.Path.with_open_out ~create:`Never (cfs / "f") Eio.File.sync
    with Eio.Io _ -> ()
  done;
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 3) [ Cstruct.of_string "e" ];
      Eio.File.sync f);
  Alcotest.(check string)
    "f" "x\000\000e"
    (Eio.Path.load (Crash.post_crash ctl / "f"))

(* A page a failed sync lost, then a truncation that cuts it away, made
   durable by a sync: the page never comes back, even when the cut falls on
   a block boundary. *)
let test_crash_durable_cut_frees_lost_pages () =
  let images init steps =
    let seen = Hashtbl.create 8 in
    for seed = 0 to 399 do
      let ctl = Crash.v ~policy:(Reorder_unsynced seed) ~block_size:2 () in
      with_crash ctl @@ fun ~raw:_ ~fs ->
      write_new ~sync:true (fs / "f") init;
      Eio_fs_sync.sync_dir fs;
      let sched =
        Schedule.scripted
          [
            {
              trigger = Op_on_path { kind = `Sync; path = "f" };
              fault = Sync_error `Eio;
            };
          ]
      in
      let cfs = Chaos.wrap sched fs in
      Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
          List.iter
            (function
              | `W (o, s) ->
                  Eio.File.pwrite_all f ~file_offset:(i63 o)
                    [ Cstruct.of_string s ]
              | `T n -> Eio.File.truncate f (i63 n)
              | `S -> Eio.File.sync f
              | `F -> (
                  try
                    Eio.Path.with_open_out ~create:`Never (cfs / "f")
                      Eio.File.sync
                  with Eio.Io _ -> ()))
            steps);
      Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
    done;
    List.of_seq (Hashtbl.to_seq_keys seen)
  in
  let a = images "" [ `W (0, "A"); `F; `T 0; `S; `T 1 ] in
  Alcotest.(check bool) "no A" false (List.mem "A" a);
  let b = images "pqr" [ `W (3, "A"); `F; `T 1; `S; `W (1, "BB") ] in
  Alcotest.(check bool)
    "no r at 2" false
    (List.exists (fun s -> String.length s > 2 && s.[2] = 'r') b)

(* A failed sync injected two wrappers above the model reaches it, through
   a read-write and a read-only handle: the next sync does not make the
   lost write durable. *)
let test_crash_stacked_wrappers_relay_failure () =
  let run ~ro =
    let ctl = Crash.v ~policy:Drop_unsynced () in
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "pqr";
    let mid = Chaos.wrap Schedule.empty (Crash.wrap ctl raw) in
    let rule =
      Schedule.scripted
        [
          {
            trigger = Op_on_path { kind = `Sync; path = "/f" };
            fault = Sync_error `Eio;
          };
        ]
    in
    Eio.Switch.run (fun sw ->
        let h = Eio.Path.open_out ~sw ~create:`Never (mid / "f") in
        Eio.File.pwrite_all h ~file_offset:(i63 0) [ Cstruct.of_string "A" ];
        (if ro then
           Eio.Path.with_open_in
             (Chaos.wrap rule mid / "f")
             (fun g ->
               match Eio_fs_sync.file_barrier g with
               | Some b -> (
                   match b () with
                   | () -> Alcotest.fail "no failure"
                   | exception Eio.Io _ -> ())
               | None -> Alcotest.fail "no barrier")
         else
           match
             Eio.Path.with_open_out ~create:`Never
               (Chaos.wrap rule mid / "f")
               Eio.File.sync
           with
           | () -> Alcotest.fail "no failure"
           | exception Eio.Io _ -> ());
        Eio.File.sync h);
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  Alcotest.(check (pair string string))
    "read-write, read-only" ("pqr", "pqr")
    (run ~ro:false, run ~ro:true)

(* The images seeds 0 to 399 leave of [init] after [steps], at blocks of 2
   bytes, a failed sync injected above the model. *)
let lost_images ?(init = "pqr") steps =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 399 do
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) ~block_size:2 () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") init;
    Eio_fs_sync.sync_dir fs;
    let sched =
      Schedule.seeded ~probability:1.0 ~kinds:[ `Sync_error ] ~seed:0 ()
    in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        List.iter
          (function
            | `W (o, s) ->
                Eio.File.pwrite_all f ~file_offset:(i63 o)
                  [ Cstruct.of_string s ]
            | `T n -> Eio.File.truncate f (i63 n)
            | `F -> (
                try
                  Eio.Path.with_open_out ~create:`Never (cfs / "f")
                    Eio.File.sync
                with Eio.Io _ -> ()))
          steps);
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  List.of_seq (Hashtbl.to_seq_keys seen)

(* A write to a page a truncation then cut away, and a failed sync: the
   writeback had no page to write, so with the truncation lost the block
   keeps its bytes on disk. *)
let test_crash_cut_page_not_written () =
  let i = lost_images ~init:"pqrs" [ `W (2, "A"); `T 2; `F ] in
  Alcotest.(check (list string)) "images" [ "pq"; "pqrs" ] (List.sort compare i)

(* The image [Choose] draws for [steps] over a durable [init], grown by a
   synced truncation to [grow] bytes, with each draw made by [draw]. *)
let chosen_image ?grow ~init steps draw =
  let ctl = Crash.v ~policy:(Choose draw) ~block_size:512 () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "f") init;
  Eio_fs_sync.sync_dir fs;
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Option.iter
        (fun n ->
          Eio.File.truncate f (i63 n);
          Eio.File.sync f)
        grow;
      List.iter
        (fun (o, d) ->
          Eio.File.pwrite_all f ~file_offset:(i63 o) [ Cstruct.of_string d ])
        steps);
  Eio.Path.load (Crash.post_crash ctl / "f")

(* Every image [Choose] draws for [steps], enumerated by walking the tree
   of its draws. *)
let chosen_images ?grow ~init steps =
  let seen = Hashtbl.create 8 in
  let rec go prefix =
    let trail = ref [] and pos = ref 0 in
    let draw n =
      let c = if !pos < Array.length prefix then prefix.(!pos) else 0 in
      incr pos;
      trail := (c, n) :: !trail;
      c
    in
    Hashtbl.replace seen (chosen_image ?grow ~init steps draw) ();
    let rec next = function
      | [] -> None
      | (c, n) :: rest when c + 1 < n ->
          Some (Array.of_list (List.rev_map fst rest @ [ c + 1 ]))
      | _ :: rest -> next rest
    in
    Option.iter go (next !trail)
  in
  go [||];
  List.sort compare (List.of_seq (Hashtbl.to_seq_keys seen))

(* A crash leaves a length the file had, or a page boundary a write grew it
   across, as ext4 does. A sector is written whole: in a page the disk held,
   as it was or after a write; in a page it did not, past the end or in a
   hole, also as zeros or stale data, as ext4 may commit the page before its
   bytes. *)
let test_crash_choose_as_ext4 () =
  Alcotest.(check (list string))
    "a write to an empty file"
    [ ""; "\000\000"; "77"; "AB" ]
    (chosen_images ~init:"" [ (0, "AB") ]);
  Alcotest.(check (list string))
    "an append"
    [ "pq"; "pq\000\000"; "pqAB" ]
    (chosen_images ~init:"pq" [ (2, "AB") ]);
  let rng = Random.State.make [| 7 |] in
  let lengths =
    List.init 200 (fun _ ->
        String.length
          (chosen_image ~init:""
             [ (0, String.make 4200 'A') ]
             (fun n -> Random.State.int rng n)))
    |> List.sort_uniq Int.compare
  in
  Alcotest.(check (list int))
    "the lengths of a long write" [ 0; 4096; 4200 ] lengths;
  let hole =
    chosen_images ~grow:8192 ~init:"pq" [ (4096, String.make 1024 'A') ]
  in
  let sectors a b =
    "pq" ^ String.make 4094 '\000' ^ a ^ b ^ String.make 3072 '\000'
  in
  let a = String.make 512 'A' and z = String.make 512 '\000' in
  let s = String.make 512 '7' in
  Alcotest.(check bool)
    "a write into a hole tears, and may show stale data" true
    (List.mem (sectors a z) hole
    && List.mem (sectors s a) hole
    && List.mem (sectors a a) hole);

  Alcotest.(check (list string))
    "an overwrite" [ "pXYs"; "pqrs" ]
    (chosen_images ~init:"pqrs" [ (1, "XY") ])

(* Every image of [new] a cut after [steps] leaves, over a disk that holds
   [old], enumerated by walking the tree of its draws. *)
let images_over_old steps =
  let seen = Hashtbl.create 8 in
  let rec go prefix =
    let trail = ref [] and pos = ref 0 in
    let draw n =
      let c = if !pos < Array.length prefix then prefix.(!pos) else 0 in
      incr pos;
      trail := (c, n) :: !trail;
      c
    in
    let ctl = Crash.v ~policy:(Choose draw) ~block_size:512 () in
    let image =
      Eio_mock.Backend.run @@ fun () ->
      Eio_fs.run @@ fun env ->
      write_new (env#fs / "old") (String.make 512 'o');
      let fs = Crash.wrap ctl env#fs in
      steps ctl fs;
      match Eio.Path.load (Crash.post_crash ctl / "new") with
      | s -> s
      | exception Eio.Io _ -> "-"
    in
    Hashtbl.replace seen image ();
    let rec next = function
      | [] -> None
      | (c, n) :: rest when c + 1 < n ->
          Some (Array.of_list (List.rev_map fst rest @ [ c + 1 ]))
      | _ :: rest -> next rest
    in
    Option.iter go (next !trail)
  in
  go [||];
  List.of_seq (Hashtbl.to_seq_keys seen)

(* A file the disk held when the controller was wrapped, removed, and
   committed by a metadata sync that takes a new snapshot: its blocks are
   free, and a new file's page may hold what it held there. *)
let test_crash_removed_before_snapshot () =
  let images =
    images_over_old (fun ctl fs ->
        Eio.Path.unlink (fs / "old");
        Crash.sync_metadata ctl;
        write_new (fs / "new") (String.make 512 'n'))
  in
  Alcotest.(check bool)
    "new holds what old held" true
    (List.mem (String.make 512 'o') images)

(* The images [Choose] draws at random for [steps] over a durable [init],
   each a write or a truncation, with no sync. *)
let sampled_images ~init steps =
  let rng = Random.State.make [| 42 |] in
  let seen = Hashtbl.create 64 in
  for _ = 1 to 400 do
    let ctl =
      Crash.v
        ~policy:(Choose (fun n -> Random.State.int rng n))
        ~block_size:512 ()
    in
    let image =
      with_crash ctl @@ fun ~raw:_ ~fs ->
      write_new ~sync:true (fs / "f") init;
      Eio_fs_sync.sync_dir fs;
      Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
          List.iter
            (function
              | `W (o, d) ->
                  Eio.File.pwrite_all f ~file_offset:(i63 o)
                    [ Cstruct.of_string d ]
              | `T n -> Eio.File.truncate f (i63 n))
            steps);
      Eio.Path.load (Crash.post_crash ctl / "f")
    in
    Hashtbl.replace seen image ()
  done;
  List.of_seq (Hashtbl.to_seq_keys seen)

(* A truncation frees the pages past it only once it commits: at the length
   before it, those pages hold their old bytes, whatever a later write put
   in the blocks they get. *)
let test_crash_truncation_then_reuse () =
  let i =
    sampled_images ~init:(String.make 8192 'a') [ `T 100; `W (5000, "b") ]
  in
  List.iter
    (fun s ->
      if
        String.length s = 8192 && String.sub s 4096 4096 <> String.make 4096 'a'
      then Alcotest.fail "page 1 does not hold its old bytes")
    i;
  let i =
    sampled_images ~init:(String.make 600 'a')
      [ `T 0; `W (0, String.make 100 'b') ]
  in
  List.iter
    (fun s ->
      if String.length s = 600 && s <> String.make 600 'a' then
        Alcotest.fail "the old length does not hold the old bytes")
    i

(* ext4 sets the length on disk as it writes back a page the write put bytes
   in, not the pages of the hole before it. *)
let test_crash_sparse_write_lengths () =
  let i = sampled_images ~init:(String.make 100 'a') [ `W (10000, "b") ] in
  Alcotest.(check (list int))
    "lengths" [ 100; 10001 ]
    (List.sort_uniq Int.compare (List.map String.length i))

(* A truncation zeroes the rest of its 4096-byte block, which the disk may
   write before the new length, and sector by sector. *)
let test_crash_truncation_zeroes_block () =
  let i = sampled_images ~init:(String.make 4096 'a') [ `T 100 ] in
  let zero k s = String.sub s (512 * k) 512 = String.make 512 '\000' in
  Alcotest.(check bool)
    "a sector past the cut zeroed, at the old length" true
    (List.exists
       (fun s -> String.length s = 4096 && zero 3 s && not (zero 5 s))
       i)

(* Two writes into one sector of bytes the file held: a sector is written
   whole, so the second never lands without the first. *)
let test_crash_survive_keeps_a_sector () =
  let image keep =
    let ctl = Crash.v ~policy:(Survive (fun u -> List.mem u.offset keep)) () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "hdr") (String.make 16 '.');
    Eio_fs_sync.sync_dir fs;
    Eio.Path.with_open_out ~create:`Never (fs / "hdr") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "MAGIC" ];
        Eio.File.pwrite_all f ~file_offset:(i63 8) [ Cstruct.of_string "42" ]);
    Eio.Path.load (Crash.post_crash ctl / "hdr")
  in
  Alcotest.(check (list string))
    "the first, the second, both"
    [ "MAGIC..........."; "MAGIC...42......"; "MAGIC...42......" ]
    [ image [ 0 ]; image [ 8 ]; image [ 0; 8 ] ]

(* Two failed writebacks of one block: either may have landed, the second
   also over the page re-read from disk once the first was evicted. *)
let test_crash_each_failure_may_land () =
  let i = lost_images [ `W (0, "A"); `F; `W (1, "B"); `F ] in
  Alcotest.(check (list bool))
    "first, second, second over the evicted page" [ true; true; true ]
    [ List.mem "Aqr" i; List.mem "ABr" i; List.mem "pBr" i ]

(* A lost page, then a write to it and a crash with no sync: the page
   cache may have evicted the lost page, so the write lands over the bytes
   on disk. *)
let test_crash_evicted_before_sync () =
  let i = lost_images [ `W (0, "A"); `F; `W (1, "B") ] in
  Alcotest.(check (list bool))
    "kept, evicted" [ true; true ]
    [ List.mem "ABr" i; List.mem "pBr" i ]

(* A user's write that grows a setuid file, or a truncation of it: the new
   length lands only with the setuid clear before it, as ext4 journals both
   in program order. An overwrite within the length may land without it. *)
let test_crash_length_after_clear () =
  let run op policy =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "WW";
    Eio.Path.chmod ~follow:false ~perm:0o4777 (raw / "f");
    let ctl = Crash.v ~policy ~block_size:2 () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        match op with
        | `Grow ->
            Eio.File.pwrite_all f ~file_offset:(i63 0)
              [ Cstruct.of_string "aaaa" ]
        | `Cut -> Eio.File.truncate f (i63 1)
        | `Over ->
            Eio.File.pwrite_all f ~file_offset:(i63 0)
              [ Cstruct.of_string "aa" ]);
    let post = Crash.post_crash ctl in
    (Eio.Path.load (post / "f"), (Eio.Path.stat ~follow:false (post / "f")).perm)
  in
  let policies =
    Crash.Survive (fun u -> u.kind = Data || u.kind = Truncate)
    :: List.init 100 (fun s -> Crash.Reorder_unsynced s)
  in
  List.iter
    (fun op ->
      List.iter
        (fun policy ->
          let data, perm = run op policy in
          if String.length data <> 2 && perm <> 0o777 then
            Alcotest.failf "%S with mode %o" data perm)
        policies)
    [ `Grow; `Cut ];
  Alcotest.(check bool)
    "an overwrite without its clear" true
    (List.exists (fun p -> run `Over p = ("aa", 0o4777)) policies)

(* A fault wrapper between the controller and a user's view: the crash still
   rebuilds the tree with root's rights. *)
let test_crash_over_chaos_over_user () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o755 (raw / "d");
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "d" / "f") "x";
  Eio.Path.mkdir ~perm:0o755 (raw / "u");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "u");
  let ctl = Crash.v () in
  let fs =
    Crash.wrap ctl (Chaos.wrap Schedule.empty (Eio_fs.Fs.as_user alice raw))
  in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "u" / "g") "y";
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair string bool))
    "d/f kept, u/g dropped" ("x", false)
    (Eio.Path.load (post / "d" / "f"), Eio.Path.is_file (post / "u" / "g"))

(* {2 The model's authority is the wrapper, not the descriptor}

   Over a host directory the wrapper hands eio-fs.unix, and no other caller,
   the host's descriptor beneath a file it opened, to take a record lock on.
   That descriptor names the same bytes the model is tracking, so what it is
   for has to be settled: it carries the operations the operating system
   offers on an open file, and it is not a second way into the durability
   journal. A write issued on it is not a write the model saw; a barrier issued
   on it is not the model's barrier. The two runs below take that descriptor
   -- the same open file the wrapper's own writes go through -- and put it next
   to those writes under the two policies that tell them apart. *)

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
          Crash.sync_metadata ctl;
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
          Crash.sync_metadata ctl;
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

(* The by-name half, and the sharper one: a descriptor the wrapper carried over
   at least names a file the model is tracking, but a name reaches the tree
   whether or not the model has ever heard of what it reaches. A crash rebuilds
   the whole wrapped tree from the model, so an entry made under a native name
   after the wrap -- by a child process, by a device-image builder, by a test's
   own setup -- is not in the post-crash image, whatever the policy would have
   kept. Survive-everything keeps every change the journal holds, so the file
   that is missing here is missing for having never entered it.

   The boundary is what {!Chaos.Crash.wrap} states; this pins it, because a
   workload that reaches the tree by name reads a recovered image that never
   held its bytes and calls that a durability result. A test that must plant
   something by name plants it before the wrap, where the snapshot picks it
   up. *)
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
      Crash.sync_metadata ctl;
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

(* An [After_write] trigger crashes the moment the covering write commits; the
   next op through the wrapper fails, and the unsynced block is lost. *)
let test_crash_after_write_trigger () =
  let ctl =
    Crash.v ~policy:Drop_unsynced
      ~trigger:(Crash.After_write { path = "img"; file_offset = 0 })
      ()
  in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  (try
     Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "img")
       (fun file ->
         Eio.File.pwrite_all file ~file_offset:(i63 0)
           [ Cstruct.of_string "block" ];
         (* the crash has fired; a further op must fail *)
         Eio.File.pwrite_all file ~file_offset:(i63 5)
           [ Cstruct.of_string "more" ])
   with Eio.Io _ -> ());
  Alcotest.(check bool) "crash fired on the write" true (Crash.is_crashed ctl);
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "uncommitted file gone after auto-crash" true
    (absent (post / "img"))

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

let every_op fault =
  Schedule.scripted
    (List.init 64 (fun n -> { Schedule.trigger = Nth_op (n + 1); fault }))

let test_wrapped_dir_hides_its_base () =
  with_chaos (every_op (Open_error `Eacces)) @@ fun ~raw ~chaos ->
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
  with_chaos (every_op (Write_error `Eio)) @@ fun ~raw ~chaos ->
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
  with_chaos Schedule.empty @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "a") "moved";
  Eio.Path.rename (raw / "a") (chaos / "b");
  Alcotest.(check string) "at its new name" "moved" (Eio.Path.load (raw / "b"));
  Alcotest.(check bool)
    "gone from its old name" true
    (Eio.Path.kind ~follow:false (raw / "a") = `Not_found)

let suite =
  ( "chaos",
    [
      Alcotest.test_case "rename into a wrapper" `Quick
        test_rename_into_a_wrapper;
      Alcotest.test_case "a wrapped directory hides its base" `Quick
        test_wrapped_dir_hides_its_base;
      Alcotest.test_case "a wrapped file hides its base" `Quick
        test_wrapped_file_hides_its_base;
      Alcotest.test_case "crash: a wrapped file hides its base" `Quick
        test_crash_file_hides_its_base;
      Alcotest.test_case "short read" `Quick test_short_read;
      Alcotest.test_case "short read of an empty buffer" `Quick
        test_short_read_empty;
      Alcotest.test_case "streaming read error" `Quick (test_read_error false);
      Alcotest.test_case "seeded streaming read error" `Quick
        (test_seeded_read_error false);
      Alcotest.test_case "seeded positional read error" `Quick
        (test_seeded_read_error true);
      Alcotest.test_case "positional read error" `Quick (test_read_error true);
      Alcotest.test_case "short write" `Quick test_short_write;
      Alcotest.test_case "torn write" `Quick test_torn_write;
      Alcotest.test_case "write error" `Quick test_write_error;
      Alcotest.test_case "sync error" `Quick test_sync_error;
      Alcotest.test_case "read-only file sync" `Quick test_read_only_file_sync;
      Alcotest.test_case "fault compare" `Quick test_fault_compare;
      Alcotest.test_case "open error" `Quick test_open_error;
      Alcotest.test_case "chmod and chown are changes" `Quick
        test_change_error_on_chmod_chown;
      Alcotest.test_case "opens for reading and writing" `Quick test_open_kinds;
      Alcotest.test_case "stat error" `Quick test_stat_error;
      Alcotest.test_case "real child cwd" `Quick test_real_child_cwd;
      Alcotest.test_case "directory error" `Quick (test_directory_error false);
      Alcotest.test_case "directory entries error" `Quick
        (test_directory_error true);
      Alcotest.test_case "seeded directory error" `Quick
        (test_seeded_directory_error false);
      Alcotest.test_case "seeded directory entries error" `Quick
        (test_seeded_directory_error true);
      Alcotest.test_case "directory short-read inapplicable" `Quick
        test_directory_short_read_inapplicable;
      Alcotest.test_case "seeded short read of a long operation" `Quick
        test_seeded_short_read_long_op;
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
        test_swap_on_real_empty_directory_subtree;
      Alcotest.test_case "swap on a populated directory subtree (real fs)"
        `Quick test_swap_on_real_populated_directory_subtree;
      Alcotest.test_case "scripted fires once" `Quick test_scripted_fires_once;
      Alcotest.test_case "nth operation on a path" `Quick test_nth_op_on_path;
      Alcotest.test_case "seed determinism" `Quick test_seed_determinism;
      Alcotest.test_case "seed reproduces" `Quick test_seed_reproduces;
      Alcotest.test_case "chaos perturbs" `Quick test_chaos_perturbs;
      Alcotest.test_case "change error" `Quick test_change_error;
      Alcotest.test_case "seeded change error" `Quick test_seeded_change_error;
      Alcotest.test_case "a seeded schedule prints its rebuild" `Quick
        test_seeded_pp_rebuilds;
      Alcotest.test_case "a seeded schedule prints its probability exactly"
        `Quick test_seeded_pp_floats;
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
      Alcotest.test_case "crash: a multi-block write" `Quick
        test_crash_multi_block_write;
      Alcotest.test_case "crash: two appends in one block" `Quick
        test_crash_appends_in_one_block;
      Alcotest.test_case "crash: the block size" `Quick test_crash_block_size;
      Alcotest.test_case "crash: a rename over an open file" `Quick
        test_crash_rename_over_an_open_file;
      Alcotest.test_case "crash: a create through a dangling link" `Quick
        test_crash_create_through_a_dangling_link;
      Alcotest.test_case "crash: over a user's view" `Quick
        test_crash_over_a_user_view;
      Alcotest.test_case "crash: owners are restored" `Quick
        test_crash_restores_owners;
      Alcotest.test_case
        "crash: a deferred barrier holds for its directory only" `Quick
        test_crash_deferred_barrier_identity;
      Alcotest.test_case "crash: chmod and chown are journaled" `Quick
        test_crash_chmod_journaled;
      Alcotest.test_case "crash: a synced chmod of a file whose name is pending"
        `Quick test_crash_synced_chmod_of_a_new_file;
      Alcotest.test_case "crash: a synced mode follows the file" `Quick
        test_crash_synced_mode_follows_the_file;
      Alcotest.test_case "crash: files and directories are known by inode"
        `Quick test_crash_inode_identity;
      Alcotest.test_case
        "crash: an entry lands whole, a rename moves the image's node" `Quick
        test_crash_entries_whole;
      Alcotest.test_case "crash: a crash keeps setuid and setgid" `Quick
        test_crash_keeps_privilege_bits;
      Alcotest.test_case "crash: a sync makes a cleared setuid durable" `Quick
        test_crash_sync_records_the_mode;
      Alcotest.test_case "crash: a truncation follows writeback" `Quick
        test_crash_truncation_follows_writeback;
      Alcotest.test_case "crash: a file sync commits its mode alone" `Quick
        test_crash_sync_commits_the_mode_alone;
      Alcotest.test_case
        "crash: a directory handle's barrier is on the inode it opened" `Quick
        test_crash_dir_handle_barrier_by_inode;
      Alcotest.test_case "crash: a deferred barrier lands with what it needs"
        `Quick test_crash_deferred_barrier_whole;
      Alcotest.test_case
        "crash: a rename needs the renames above where it lands" `Quick
        test_crash_move_needs_ancestor_moves;
      Alcotest.test_case
        "crash: a new directory is durable only by its parent's barrier" `Quick
        test_crash_new_dir_needs_parent_barrier;
      Alcotest.test_case
        "crash: a file met after a rename of it is found by inode" `Quick
        test_crash_preexisting_found_by_inode;
      Alcotest.test_case
        "crash: a rename over a directory needs what emptied it" `Quick
        test_crash_rename_over_filled_dir;
      Alcotest.test_case "crash: an opened directory's barrier is on its inode"
        `Quick test_crash_subtree_barrier_by_inode;
      Alcotest.test_case
        "crash: the hole a write past the end leaves can land zeroed" `Quick
        test_crash_hole_lands_zeroed;
      Alcotest.test_case "crash: sync_metadata commits a write's setgid clear"
        `Quick test_crash_sync_metadata_commits_cleared_mode;
      Alcotest.test_case
        "crash: a reached chmod stays unsynced until its node is durable" `Quick
        test_crash_attrs_wait_for_their_node;
      Alcotest.test_case "crash: a failed sync keeps a truncation" `Quick
        test_crash_failed_sync_keeps_truncation;
      Alcotest.test_case "crash: a lost page is kept or evicted" `Quick
        test_crash_lost_page_kept_or_evicted;
      Alcotest.test_case
        "crash: a write's setuid clear is journaled at the write" `Quick
        test_crash_clear_journaled_at_write;
      Alcotest.test_case "crash: a kept truncation brings no lost write back"
        `Quick test_crash_kept_truncation_length_only;
      Alcotest.test_case "crash: a controller is wrapped once" `Quick
        test_crash_wrap_once;
      Alcotest.test_case "crash: a failed sync may have written some blocks"
        `Quick test_crash_partial_writeback;
      Alcotest.test_case "crash: a second failed sync keeps the page cache"
        `Quick test_crash_second_failure;
      Alcotest.test_case "crash: a durable cut frees the pages a failure lost"
        `Quick test_crash_durable_cut_frees_lost_pages;
      Alcotest.test_case "crash: stacked wrappers relay a failed sync" `Quick
        test_crash_stacked_wrappers_relay_failure;
      Alcotest.test_case "crash: rebuilds as root beneath a fault wrapper"
        `Quick test_crash_over_chaos_over_user;
      Alcotest.test_case "crash: either failed writeback of a block may land"
        `Quick test_crash_each_failure_may_land;
      Alcotest.test_case "crash: lengths and sectors as ext4 leaves them" `Quick
        test_crash_choose_as_ext4;
      Alcotest.test_case "crash: no freed page reused before its truncation"
        `Quick test_crash_truncation_then_reuse;
      Alcotest.test_case "crash: a removal a snapshot folded frees its blocks"
        `Quick test_crash_removed_before_snapshot;
      Alcotest.test_case "crash: a sparse write's lengths" `Quick
        test_crash_sparse_write_lengths;
      Alcotest.test_case "crash: a truncation zeroes the rest of its block"
        `Quick test_crash_truncation_zeroes_block;
      Alcotest.test_case "crash: a sector's overwrites survive together" `Quick
        test_crash_survive_keeps_a_sector;
      Alcotest.test_case "crash: a page cut before a failure is not written"
        `Quick test_crash_cut_page_not_written;
      Alcotest.test_case "crash: a lost page may be evicted before any sync"
        `Quick test_crash_evicted_before_sync;
      Alcotest.test_case "crash: a length lands with the setuid clear before it"
        `Quick test_crash_length_after_clear;
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
      Alcotest.test_case "crash: after-write trigger" `Quick
        test_crash_after_write_trigger;
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
        test_crash_readonly_real;
      Alcotest.test_case "crash: closed subtree (real fs)" `Quick
        test_crash_closed_subtree_real;
      Alcotest.test_case "crash: committed unlink stays deleted" `Quick
        test_crash_committed_unlink_stays_deleted;
      Alcotest.test_case "crash: uncommitted unlink rolls back" `Quick
        test_crash_uncommitted_unlink_rolls_back;
      Alcotest.test_case "crash: a file sync leaves metadata pending" `Quick
        test_crash_file_sync_leaves_metadata_pending;
      Alcotest.test_case "crash: uncommitted rename reverts" `Quick
        test_crash_uncommitted_rename_reverts;
      Alcotest.test_case "crash: sync_metadata commits rename" `Quick
        test_crash_sync_metadata_commits_rename;
      Alcotest.test_case "crash: reverted rename restores victim" `Quick
        test_crash_reverted_rename_restores_victim;
      Alcotest.test_case "crash: uncommitted mkdir vanishes" `Quick
        test_crash_uncommitted_mkdir_vanishes;
      Alcotest.test_case "crash: sync_metadata commits mkdir" `Quick
        test_crash_sync_metadata_commits_mkdir;
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
      Alcotest.test_case "crash: nth-op counts dir ops" `Quick
        test_crash_nth_op_counts_dir_ops;
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
        test_crash_rename_revert_real;
      Alcotest.test_case "crash: a bare descriptor's write is not journalled"
        `Quick test_crash_omits_a_write_through_a_bare_descriptor;
      Alcotest.test_case "crash: a bare descriptor's barrier is not the model's"
        `Quick test_crash_ignores_a_barrier_through_a_bare_descriptor;
      Alcotest.test_case "crash: an entry planted by name is not in the image"
        `Quick test_crash_drops_an_entry_planted_by_native_name;
    ] )
