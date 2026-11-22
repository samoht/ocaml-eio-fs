(* Smoke tests for [Eio_fs_unix]: the barriers run against a real filesystem
   and surface errors as documented. Durability itself is not observable
   without cutting power; what is testable is that the barrier accepts the
   states a commit protocol hands it. *)

module Chaos = Eio_fs_chaos
module Crash = Chaos.Private.Crash
open Holder

let ( / ) = Eio.Path.( / )

(* The commit-protocol shape: write a temporary, fsync it, rename it into
   place, barrier the directory. Every step must succeed and the published
   file must read back whole. *)
let test_sync_dir_after_rename () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "f.tmp") "payload";
  Eio.Path.rename (dir / "f.tmp") (dir / "f");
  Eio_fs_unix.sync_dir dir;
  Alcotest.(check string)
    "published file whole after the barrier" "payload"
    (Eio.Path.load (dir / "f"))

(* The barrier is idempotent and accepts an empty directory. *)
let test_sync_dir_empty_and_repeated () =
  with_real_dir @@ fun dir ->
  Eio_fs_unix.sync_dir dir;
  Eio_fs_unix.sync_dir dir

(* [sync_dir] syncs the directory the capability holds, not what its name
   resolves to now: a directory renamed after it was opened is still synced. *)
let test_sync_dir_barriers_the_held_directory () =
  with_real_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o700 (root / "d");
  Eio.Path.with_open_dir (root / "d") @@ fun d ->
  Eio.Path.rename (root / "d") (root / "moved");
  Eio_fs_unix.sync_dir d

(* A link left under the directory's name is refused, not followed to a
   directory outside the tree. *)
let test_sync_dir_refuses_a_name_a_link_took () =
  with_real_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o700 (root / "tree");
  Eio.Path.mkdir ~perm:0o700 (root / "elsewhere");
  Eio.Path.with_open_dir (root / "tree") @@ fun tree ->
  Eio.Path.mkdir ~perm:0o700 (tree / "store");
  (* The adversary writes this directory: the store goes aside and a link to a
     directory of their own takes its name. *)
  Eio.Path.rename (tree / "store") (tree / "moved");
  Eio.Path.symlink
    ~link_to:(Eio.Path.native_exn (root / "elsewhere"))
    (tree / "store");
  match Eio_fs_unix.sync_dir (tree / "store") with
  | () ->
      Alcotest.fail
        "the barrier reported a directory the caller never named durable"
  | exception
      (Eio.Io (Eio.Fs.E (Eio.Fs.Symlink | Eio.Fs.Permission_denied _), _) as e)
    ->
      Alcotest.(check bool)
        "the refusal names the directory it was asked to barrier" true
        (mentions ~needle:"store" (Printexc.to_string e))

(* A link inside the directory is refused too, as its open is [~follow:false]:
   the barrier would otherwise land on the directory the link names. *)
let test_sync_dir_refuses_a_relative_link () =
  with_real_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o700 (root / "moved");
  Eio.Path.symlink ~link_to:"moved" (root / "store");
  match Eio_fs_unix.sync_dir (root / "store") with
  | () -> Alcotest.fail "the barrier followed a link"
  | exception Eio.Io _ -> ()

(* The barrier runs on a system thread: a sibling fiber gets turns while a run
   of barriers goes on. *)
let test_sync_dir_lets_a_sibling_fiber_run () =
  with_real_dir @@ fun dir ->
  (* One turn every other barrier: the barrier suspends once per call. *)
  let barriers = 10 in
  let floor = 5 in
  let started = ref false in
  let finished = ref false in
  let turns = ref 0 in
  Eio.Fiber.both
    (fun () ->
      started := true;
      for _ = 1 to barriers do
        Eio_fs_unix.sync_dir dir
      done;
      finished := true)
    (fun () ->
      while not !started do
        Eio.Fiber.yield ()
      done;
      while not !finished do
        incr turns;
        Eio.Fiber.yield ()
      done);
  if !turns < floor then
    Alcotest.failf "sibling fiber ran %d times during %d barriers, wanted %d"
      !turns barriers floor

(* A host descriptor, a read-only one included, is synced as the file
   [Eio_unix.File.import_ro] makes of it. *)
let test_sync_file_of_a_descriptor () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "f") "x";
  let path = Eio.Path.native_exn (dir / "f") in
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      Eio_fs_unix.sync_file (Eio_unix.File.import_ro ~sw ~close_unix:false fd))

(* OBuilder's runc sandbox, with fast_sync, answers every fsync(2) itself. *)
let host_drops_fsync () =
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect
    ~finally:(fun () ->
      Unix.close r;
      Unix.close w)
    (fun () ->
      match Unix.fsync r with
      | () -> true
      | exception Unix.Unix_error _ -> false)

(* The barrier raises the error of a descriptor that is no longer a file. The
   descriptor becomes a pipe with [dup2]: once closed, any open in the process
   could take its number. *)
let test_sync_file_surfaces_errors () =
  if host_drops_fsync () then Alcotest.skip ();
  let path = Filename.temp_file "eio-fs-fullfsync" ".dat" in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CLOEXEC ] 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let written = Unix.write_substring fd "durable bytes" 0 13 in
  Alcotest.(check int) "wrote the payload" 13 written;
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let file = Eio_unix.File.import_rw ~sw ~close_unix:false fd in
  Eio_fs_unix.sync_file file;
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.dup2 ~cloexec:true r fd;
  Unix.close r;
  Unix.close w;
  Sys.remove path;
  match Eio_fs_unix.sync_file file with
  | () -> Alcotest.fail "the barrier accepted a pipe"
  | exception Eio.Io _ -> ()

(* ---- the file barrier asks the registration before the descriptor ----

   A wrapper that models power loss hands out the base's own descriptor, so a
   store can take a record lock on the file it is publishing or hand it to a
   launcher. That descriptor is the operations the operating system offers on
   an open file and not a second way past the wrapper: an fsync issued on it
   is a barrier the wrapper never saw, so the model records none and drops the
   bytes at the cut while the store was told they were durable. Reaching for
   the descriptor before asking the registration is therefore not a shortcut
   to the strongest barrier, it is the defect.

   Which descriptor an fsync reached is no more observable from a program than
   which inode a directory barrier reached, so the ordering is pinned the way
   it can be: this file carries a descriptor of this process that is already
   closed, which [Eio_unix.Fd.use_exn] refuses. Reaching for it raises, and
   answering elsewhere returns, so the same file tells both halves of the
   dispatch apart depending on whether it registers [File_sync]. *)
module Registered_file = struct
  type t = {
    base : Eio.File.rw_ty Eio.Resource.t;
    fd : Eio_unix.Fd.t;
    calls : int ref;
  }

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t.base buf
  let pread t ~file_offset bufs = Eio.File.pread t.base ~file_offset bufs
  let stat t = Eio.File.stat t.base
  let seek t off cmd = Eio.File.seek t.base off cmd
  let close t = Eio.Resource.close t.base
  let single_write t bufs = Eio.Flow.single_write t.base bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let pwrite t ~file_offset bufs =
    Eio.File.pwrite_single t.base ~file_offset bufs

  let sync t = Eio.File.sync t.base
  let truncate t len = Eio.File.truncate t.base len
end

let registered_file ~sw ~register base =
  let fd =
    Eio_unix.Fd.of_unix ~sw ~close_unix:true
      (Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0)
  in
  Eio_unix.Fd.close fd;
  let state = Registered_file.{ base; fd; calls = ref 0 } in
  let registration =
    if register then
      [
        Eio.Resource.H
          ( Eio_fs.Sync.File_sync,
            fun (t : Registered_file.t) ~full:_ -> incr t.calls );
      ]
    else []
  in
  let file : Eio.File.rw_ty Eio.Resource.t =
    Eio.Resource.T
      ( state,
        Eio.Resource.handler
          (registration
          @ [
              Eio.Resource.H
                (Eio_unix.Resource.T, fun (t : Registered_file.t) -> t.fd);
            ]
          @ Eio.Resource.bindings (Eio.File.Pi.rw (module Registered_file))) )
  in
  (file, state)

let test_sync_file_asks_the_registration_before_the_descriptor () =
  with_real_dir @@ fun dir ->
  Eio.Switch.run @@ fun sw ->
  let base = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (dir / "f") in
  let file, state = registered_file ~sw ~register:true base in
  Alcotest.(check bool)
    "the file carries a descriptor for the barrier to find" true
    (Option.is_some (Eio_unix.Resource.fd_opt file));
  (* A raise here means the barrier reached past the registration for the
     descriptor. *)
  Eio_fs_unix.sync_file file;
  Alcotest.(check int)
    "the registration answered the barrier" 1
    !(state.Registered_file.calls)

(* A file with no registered barrier gets the platform's own on its
   descriptor: on a closed descriptor it raises, where the portable barrier,
   which takes the file's own sync, returns. *)
let test_sync_file_takes_the_descriptor_when_nothing_is_registered () =
  with_real_dir @@ fun dir ->
  Eio.Switch.run @@ fun sw ->
  let base = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (dir / "f") in
  let file, _ = registered_file ~sw ~register:false base in
  Eio.File.sync file;
  match Eio_fs_unix.sync_file file with
  | () ->
      Alcotest.fail
        "the barrier skipped the descriptor of a file that registers none"
  | exception Invalid_argument _ -> ()

(* Through the crash wrapper over a real directory, [sync_file] reaches the
   model's sync: of two files cut alike, the synced one survives
   [Drop_unsynced] and the other, the control, is dropped. *)
let test_sync_file_under_the_crash_model_survives_the_cut () =
  with_real_dir @@ fun dir ->
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let wdir = Crash.wrap ctl dir in
  let write ~barrier name payload =
    Eio.Switch.run @@ fun sw ->
    let file =
      Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (wdir / name)
    in
    Alcotest.(check bool)
      "the wrapped file hands out no host descriptor" true
      (Option.is_none (Eio_unix.Resource.fd_opt file));
    Eio.Flow.copy_string payload file;
    if barrier then Eio_fs_unix.sync_file file
  in
  write ~barrier:true "barriered" "payload";
  write ~barrier:false "unbarriered" "payload";
  Eio_fs_unix.sync_dir wdir;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the barriered bytes survived the cut" "payload"
    (Eio.Path.load (post / "barriered"));
  Alcotest.(check string)
    "and the unbarriered bytes did not" ""
    (Eio.Path.load (post / "unbarriered"))

(* A plain POSIX file registers nothing, so the barrier is its descriptor's
   own (F_FULLFSYNC on Darwin): it runs, and the file reads back whole. *)
let test_sync_file_on_a_plain_posix_file () =
  with_real_dir @@ fun dir ->
  Eio.Switch.run (fun sw ->
      let file =
        Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (dir / "f")
      in
      Eio.Flow.copy_string "payload" file;
      Eio_fs_unix.sync_file file);
  Alcotest.(check string)
    "the file reads back whole after its barrier" "payload"
    (Eio.Path.load (dir / "f"))

(* clonefile(2) on Darwin: the directory and everything under it, the files'
   bytes and modes, a symbolic link kept a link; the clone is a copy, so
   writing to it leaves the source alone; a destination that exists is an
   error. Elsewhere there is no clone, and nothing is written. *)
let test_clone_tree () =
  with_real_dir @@ fun dir ->
  let src = Eio.Path.(dir / "src") and dst = Eio.Path.(dir / "dst") in
  Eio.Path.mkdirs ~perm:0o755 Eio.Path.(src / "sub");
  Eio.Path.save ~create:(`Exclusive 0o755) Eio.Path.(src / "sub" / "run") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(src / "data") "bytes";
  Eio.Path.symlink ~link_to:"sub/run" Eio.Path.(src / "link");
  let darwin =
    let ic = Unix.open_process_in "uname -s" in
    let os = In_channel.input_all ic in
    ignore (Unix.close_process_in ic);
    String.trim os = "Darwin"
  in
  match Eio_fs_unix.clone_tree src dst with
  | false ->
      Alcotest.(check bool) "not on Darwin" false darwin;
      Alcotest.(check bool)
        "nothing written" true
        (Eio.Path.kind ~follow:false dst = `Not_found)
  | true ->
      Alcotest.(check string)
        "a file" "bytes"
        (Eio.Path.load Eio.Path.(dst / "data"));
      Alcotest.(check int)
        "its mode" 0o755
        ((Eio.Path.stat ~follow:false Eio.Path.(dst / "sub" / "run")).perm
       land 0o777);
      Alcotest.(check string)
        "a link" "sub/run"
        (Eio.Path.read_link Eio.Path.(dst / "link"));
      Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(dst / "data") "other";
      Alcotest.(check string)
        "the source keeps its bytes" "bytes"
        (Eio.Path.load Eio.Path.(src / "data"));
      Alcotest.(check bool)
        "a destination that exists" true
        (match Eio_fs_unix.clone_tree src dst with
        | _ -> false
        | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> true)

(* rmtree removes a whole tree: with [~grant] a directory its owner can
   neither read nor search is granted first, and a symbolic link is removed,
   never followed. Three files of five bytes, a link and three directories are
   seven entries, and 15 bytes and the link's target. A path that does not exist
   removes nothing. *)
let test_rmtree () =
  with_real_dir @@ fun dir ->
  let tree = Eio.Path.(dir / "tree") and kept = Eio.Path.(dir / "kept") in
  Eio.Path.mkdirs ~perm:0o755 Eio.Path.(tree / "a" / "b");
  Eio.Path.mkdir ~perm:0o755 kept;
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(kept / "f") "kept!";
  List.iter
    (fun p -> Eio.Path.save ~create:(`Exclusive 0o444) p "bytes")
    Eio.Path.[ tree / "f"; tree / "a" / "f"; tree / "a" / "b" / "f" ];
  Eio.Path.symlink ~link_to:(snd kept) Eio.Path.(tree / "a" / "link");
  Eio.Path.chmod ~follow:false ~perm:0o000 Eio.Path.(tree / "a" / "b");
  Eio.Path.chmod ~follow:false ~perm:0o500 Eio.Path.(tree / "a");
  let r = Eio_fs_unix.rmtree ~grant:true tree in
  Alcotest.(check (list string)) "nothing refused" [] r.refused;
  Alcotest.(check (pair int int))
    "every entry and byte"
    (7, 15 + String.length (snd kept))
    (r.entries, r.bytes);
  Alcotest.(check bool)
    "nothing left" true
    (Eio.Path.kind ~follow:false tree = `Not_found);
  Alcotest.(check string)
    "the link's target stays" "kept!"
    (Eio.Path.load Eio.Path.(kept / "f"));
  let again = Eio_fs_unix.rmtree tree in
  Alcotest.(check (pair int (list string)))
    "a path that does not exist" (0, [])
    (again.entries, again.refused)

(* A refusal names the path and stops what stands above it, with [last], and
   no other directory: a read-only directory keeps its file, the root keeps
   [last], and the sibling goes. The next call, granting, removes the rest. A
   removal that goes on past a refusal loses [last], and one that stops at the
   first leaves the sibling. *)
let test_rmtree_refused () =
  with_real_dir @@ fun dir ->
  let tree = Eio.Path.(dir / "tree") in
  let shut = Eio.Path.(tree / "shut") and open_ = Eio.Path.(tree / "open") in
  Eio.Path.mkdirs ~perm:0o755 shut;
  Eio.Path.mkdirs ~perm:0o755 open_;
  List.iter
    (fun p -> Eio.Path.save ~create:(`Exclusive 0o644) p "bytes")
    Eio.Path.[ shut / "f"; open_ / "f"; tree / "last" ];
  Eio.Path.chmod ~follow:false ~perm:0o500 shut;
  let r = Eio_fs_unix.rmtree ~last:"last" tree in
  Alcotest.(check int) "one refusal" 1 (List.length r.refused);
  Alcotest.(check bool)
    "it names the file refused" true
    (String.ends_with ~suffix:"Permission denied" (List.hd r.refused));
  Alcotest.(check (pair int int))
    "the sibling, counted" (2, 5) (r.entries, r.bytes);
  Alcotest.(check string)
    "last stays" "bytes"
    (Eio.Path.load Eio.Path.(tree / "last"));
  let rest = Eio_fs_unix.rmtree ~grant:true ~last:"last" tree in
  Alcotest.(check (pair int (list string)))
    "the rest goes" (4, [])
    (rest.entries, rest.refused);
  Alcotest.(check bool)
    "nothing left" true
    (Eio.Path.kind ~follow:false tree = `Not_found)

(* The hops a removal makes follow its directories, not its files: a directory
   of 1,000 files costs what one of 100 does. One hop per entry is the cost of
   the tree Eio.Path walks. *)
let test_rmtree_hops () =
  with_real_dir @@ fun dir ->
  let hops n =
    let tree = Eio.Path.(dir / Fmt.str "tree-%d" n) in
    Eio.Path.mkdir ~perm:0o755 tree;
    for i = 1 to n do
      Eio.Path.save ~create:(`Exclusive 0o644)
        Eio.Path.(tree / string_of_int i)
        "x"
    done;
    let before = Eio_fs_unix.Private.batch_hops () in
    let r = Eio_fs_unix.rmtree tree in
    Alcotest.(check (pair int (list string)))
      "every entry"
      (n + 1, [])
      (r.entries, r.refused);
    Eio_fs_unix.Private.batch_hops () - before
  in
  let small = hops 100 in
  Alcotest.(check int) "the hops of 1,000 files, as of 100" small (hops 1000)

(* A cancellation ends a removal at its next hop and leaves the rest: it
   lands within a directory's entries, where a removal made in one hop would
   run to the tree's end, since a system thread under way is never
   cancelled. *)
let test_rmtree_cancelled () =
  with_real_dir @@ fun dir ->
  let tree = Eio.Path.(dir / "tree") in
  Eio.Path.mkdir ~perm:0o755 tree;
  for i = 1 to 50 do
    let d = Eio.Path.(tree / string_of_int i) in
    Eio.Path.mkdir ~perm:0o755 d;
    Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(d / "f") "x"
  done;
  let before = Eio_fs_unix.Private.batch_hops () in
  let ended =
    Eio.Fiber.first
      (fun () ->
        ignore (Eio_fs_unix.rmtree tree);
        `Whole)
      (fun () ->
        while Eio_fs_unix.Private.batch_hops () = before do
          Eio.Fiber.yield ()
        done;
        `Stopped)
  in
  Alcotest.(check bool) "the stop ended it" true (ended = `Stopped);
  Alcotest.(check bool) "the rest stands" true (Eio.Path.read_dir tree <> [])

let test_statvfs_reports_raw_block_capacity () =
  with_real_dir @@ fun dir ->
  let usage = Eio_fs_unix.statvfs dir in
  Alcotest.(check bool)
    "filesystem block size is positive" true
    (Int64.compare usage.block_size 0L > 0);
  Alcotest.(check bool)
    "filesystem has blocks" true
    (Int64.compare usage.total_blocks 0L > 0);
  Alcotest.(check bool)
    "available blocks do not exceed total" true
    (Int64.compare usage.available_blocks usage.total_blocks <= 0);
  (* The order available <= free <= total says each field came from its own
     struct member. On a filesystem that keeps no reserve for the privileged
     user, free and available are equal, so this case cannot tell those two
     apart there. *)
  Alcotest.(check bool)
    "free blocks are at least the available ones" true
    (Int64.compare usage.free_blocks usage.available_blocks >= 0);
  Alcotest.(check bool)
    "free blocks do not exceed total" true
    (Int64.compare usage.free_blocks usage.total_blocks <= 0)

(* ---- positioned reads ---- *)

(* A host file of [text] opened read-write as [fd], and as a file over that
   descriptor. *)
let with_host_file dir name text fn =
  let path = Eio.Path.native_exn (dir / name) in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let n = Unix.write_substring fd text 0 (String.length text) in
  Alcotest.(check int) "the whole text was written" (String.length text) n;
  Eio.Switch.run @@ fun sw ->
  fn fd (Eio_unix.File.import_rw ~sw ~close_unix:false fd)

(* POSIX pread: the bytes at the offset asked, the count read, 0 at the end of
   the file, and the descriptor's own offset where it was. A pread written as a
   seek and a read passes the first three and fails the last. A cached read
   keeps the same contract with the runtime lock held. *)
let pread_leaves_the_offset ~cached () =
  with_real_dir @@ fun dir ->
  with_host_file dir "pread.bin" "0123456789abcdef" @@ fun fd file ->
  let pread = Eio_fs_unix.pread ~cached file in
  ignore (Unix.lseek fd 3 Unix.SEEK_SET : int);
  let buf = Bytes.make 8 '.' in
  let got = pread ~file_offset:10 buf ~pos:2 ~len:4 in
  Alcotest.(check int) "four bytes read" 4 got;
  Alcotest.(check string)
    "the bytes at offset 10, at pos 2" "..abcd.." (Bytes.to_string buf);
  Alcotest.(check int)
    "the descriptor's offset is untouched" 3
    (Unix.lseek fd 0 Unix.SEEK_CUR);
  let got = pread ~file_offset:14 buf ~pos:0 ~len:8 in
  Alcotest.(check int) "a read across the end takes what is there" 2 got;
  Alcotest.(check int)
    "a read at the end is 0" 0
    (pread ~file_offset:16 buf ~pos:0 ~len:8);
  Alcotest.check_raises "a range past the buffer is refused"
    (Invalid_argument
       "Eio_fs_unix.pread: pos is 6 and len is 4, outside the 8-byte buffer")
    (fun () -> ignore (pread ~file_offset:0 buf ~pos:6 ~len:4 : int))

(* A cached read longer than the 65536 bytes it reads with the lock held is
   an uncached one, which takes at most 65536 bytes a call: the count says
   so, and the bytes are the file's. A read that kept the lock past the bound
   would answer 70000. *)
let test_pread_cached_bound () =
  with_real_dir @@ fun dir ->
  let text = String.init 70000 (fun i -> Char.chr (i mod 251)) in
  with_host_file dir "pread_long.bin" text @@ fun _ file ->
  let buf = Bytes.make 70000 '.' in
  let got =
    Eio_fs_unix.pread ~cached:true file ~file_offset:0 buf ~pos:0 ~len:70000
  in
  Alcotest.(check int) "one call reads 65536 bytes" 65536 got;
  Alcotest.(check string)
    "they are the file's" (String.sub text 0 65536)
    (Bytes.sub_string buf 0 65536)

(* POSIX pwrite: the bytes of the span asked land at the offset asked, the
   count written, and the descriptor's own offset where it was. A pwrite
   written as a seek and a write passes the first two and fails the last. *)
let test_pwrite_leaves_the_offset () =
  with_real_dir @@ fun dir ->
  with_host_file dir "pwrite.bin" "0123456789abcdef" @@ fun fd file ->
  ignore (Unix.lseek fd 3 Unix.SEEK_SET : int);
  let got = Eio_fs_unix.pwrite file ~file_offset:10 "..WXYZ.." ~pos:2 ~len:4 in
  Alcotest.(check int) "four bytes written" 4 got;
  Alcotest.(check int)
    "the descriptor's offset is untouched" 3
    (Unix.lseek fd 0 Unix.SEEK_CUR);
  let got = Eio_fs_unix.pwrite file ~file_offset:18 "!" ~pos:0 ~len:1 in
  Alcotest.(check int) "a write past the end extends the file" 1 got;
  let buf = Bytes.make 19 '.' in
  Alcotest.(check int)
    "the file reads back" 19
    (Eio_fs_unix.pread file ~file_offset:0 buf ~pos:0 ~len:19);
  Alcotest.(check string)
    "the span at offset 10, the gap as zeros" "0123456789WXYZef\000\000!"
    (Bytes.to_string buf);
  Alcotest.check_raises "a range past the buffer is refused"
    (Invalid_argument
       "Eio_fs_unix.pwrite: pos is 2 and len is 2, outside the 3-byte buffer")
    (fun () ->
      ignore (Eio_fs_unix.pwrite file ~file_offset:0 "abc" ~pos:2 ~len:2 : int))

(* A file with no host descriptor, in memory, is read and written by Eio's
   own positioned calls, with the same counts: what lands, what is read back,
   and 0 at the end. *)
let test_positioned_in_memory () =
  with_in_memory_fs @@ fun fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) (fs / "f") @@ fun file ->
  Alcotest.(check int)
    "the span is written" 4
    (Eio_fs_unix.pwrite file ~file_offset:2 "..abcd.." ~pos:2 ~len:4);
  let buf = Bytes.make 8 '.' in
  Alcotest.(check int)
    "what is there is read" 3
    (Eio_fs_unix.pread file ~file_offset:3 buf ~pos:1 ~len:6);
  Alcotest.(check string) "at pos 1" ".bcd...." (Bytes.to_string buf);
  Alcotest.(check int)
    "a read at the end is 0" 0
    (Eio_fs_unix.pread ~cached:true file ~file_offset:6 buf ~pos:0 ~len:8)

(* [file] with a [Positioned] that counts each call, then makes it on [file]
   as it was. *)
let counted (type a) calls (t : a)
    (ops : (a, Eio.File.rw_ty) Eio.Resource.handler) :
    Eio.File.rw_ty Eio.Resource.t =
  let base t = (Eio.Resource.T (t, ops) : Eio.File.rw_ty Eio.Resource.t) in
  let module P = struct
    type t = a

    let pread t ~cached ~file_offset buf ~pos ~len =
      incr calls;
      Eio_fs_unix.pread ~cached (base t) ~file_offset buf ~pos ~len

    let pwrite t ~file_offset s ~pos ~len =
      incr calls;
      Eio_fs_unix.pwrite (base t) ~file_offset s ~pos ~len
  end in
  Eio.Resource.T
    ( t,
      Eio.Resource.handler
        (Eio.Resource.H (Eio_fs_unix.Positioned, (module P))
        :: Eio.Resource.bindings ops) )

let counting calls (Eio.Resource.T (t, ops) : Eio.File.rw_ty Eio.Resource.t) =
  counted calls t ops

(* A file that registers [Positioned] answers every positioned call itself,
   ahead of the descriptor it carries: a wrapper in front of a host file
   counts each, and its base still moves the bytes. *)
let test_positioned_wrapper () =
  with_real_dir @@ fun dir ->
  with_host_file dir "wrapped.bin" "0123" @@ fun _ file ->
  let calls = ref 0 in
  let file = counting calls (file :> Eio.File.rw_ty Eio.Resource.t) in
  Alcotest.(check int)
    "written" 2
    (Eio_fs_unix.pwrite file ~file_offset:1 "xy" ~pos:0 ~len:2);
  let buf = Bytes.make 4 '.' in
  Alcotest.(check int)
    "read" 4
    (Eio_fs_unix.pread ~cached:true file ~file_offset:0 buf ~pos:0 ~len:4);
  Alcotest.(check string) "through the base" "0xy3" (Bytes.to_string buf);
  Alcotest.(check int) "both calls went through the wrapper" 2 !calls

(* ---- the sparse map ----

   A file with a hole in it, built by writing at two offsets far enough apart
   that no filesystem allocates the span between them. Every assertion below
   is about a granularity the filesystem chooses, so none of them names a byte
   count: what is asserted is the ORDER of the answers and which of them
   exist. *)

(* Far enough apart that every filesystem this runs on leaves the span between
   the two writes unallocated, said in bytes rather than blocks since a
   filesystem may allocate a gap rather than leave a hole. Every case that
   needs the hole asserts it is there rather than assuming it. *)
let hole_span = 64 * 1024 * 1024

let with_sparse_file fn =
  with_real_dir_env @@ fun env dir ->
  let path = Eio.Path.native_exn (dir / "sparse.bin") in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let block = Bytes.make 4096 'x' in
  let write_at off =
    ignore (Unix.LargeFile.lseek fd (Int64.of_int off) Unix.SEEK_SET : int64);
    let n = Unix.write fd block 0 (Bytes.length block) in
    Alcotest.(check int) "the whole block was written" (Bytes.length block) n
  in
  write_at 0;
  write_at hole_span;
  Unix.fsync fd;
  let size = hole_span + Bytes.length block in
  fn env fd ~size

let some name = function
  | Some v -> v
  | None -> Alcotest.failf "%s answered None" name

(* The offset the file's one hole starts at, asserted to be a real hole rather
   than the end of the file: a filesystem that allocated the span would answer
   the size here, and every case that walks the hole would then be walking
   nothing. *)
let hole_start fd ~size =
  let start = some "seek_hole 0" (Eio_fs_unix.seek_hole fd ~file_offset:0) in
  Alcotest.(check bool)
    "the filesystem left a hole between the two writes" true
    (start > 0 && start < size);
  start

(* OpenBSD has no SEEK_DATA or SEEK_HOLE: the calls refuse with ENOSYS. *)
let seekable f =
  try f ()
  with Unix.Unix_error (Unix.ENOSYS, ("seek_data" | "seek_hole"), _) ->
    Alcotest.skip ()

let test_seek_data_and_hole_walk_the_file () =
  seekable @@ fun () ->
  with_sparse_file @@ fun _env fd ~size ->
  let first_data =
    some "seek_data 0" (Eio_fs_unix.seek_data fd ~file_offset:0)
  in
  Alcotest.(check int) "data starts at the first byte" 0 first_data;
  let first_hole = hole_start fd ~size in
  Alcotest.(check bool)
    "the hole starts at or before the second write" true
    (first_hole <= hole_span);
  let second_data =
    some "seek_data in the hole"
      (Eio_fs_unix.seek_data fd ~file_offset:first_hole)
  in
  Alcotest.(check bool)
    "the next data starts at or before the second write" true
    (second_data > first_hole && second_data <= hole_span);
  (* Past the last byte there is no next answer, and that is an answer rather
     than an error. *)
  Alcotest.(check bool)
    "no data past the end" true
    (Option.is_none (Eio_fs_unix.seek_data fd ~file_offset:size));
  Alcotest.(check bool)
    "no hole past the end" true
    (Option.is_none (Eio_fs_unix.seek_hole fd ~file_offset:size))

(* tmpfs and virtiofs have no FIEMAP (EOPNOTSUPP), and the BSDs no call at
   all (ENOSYS). *)
let mapped f =
  try f ()
  with Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOSYS), _, _) ->
    Alcotest.skip ()

let test_extents_report_the_physical_map () =
  mapped @@ fun () ->
  with_sparse_file @@ fun _env fd ~size:_ ->
  let es = Eio_fs_unix.extents fd ~file_offset:0 ~length:4096 ~max:16 in
  Alcotest.(check bool) "the first block is backed" true (es <> []);
  let e = List.hd es in
  Alcotest.(check int) "the first extent starts where asked" 0 e.logical;
  Alcotest.(check bool) "the extent has length" true (e.length > 0);
  Alcotest.(check bool)
    "the extent does not run past the range asked about" true
    (e.logical + e.length <= 4096);
  (* The answers are in increasing logical order and do not overlap. *)
  ignore
    (List.fold_left
       (fun previous (e : Eio_fs_unix.extent) ->
         Alcotest.(check bool)
           "extents come back in increasing logical order" true
           (e.logical >= previous);
         e.logical + e.length)
       0 es
      : int)

let test_extents_stop_at_a_hole () =
  mapped @@ fun () ->
  with_sparse_file @@ fun _env fd ~size ->
  (* Inside the hole nothing is allocated, so there is nothing to say. An empty
     answer is what ends a caller's walk, and it is the only thing that does:
     the Darwin call SUCCEEDS over a hole and reports a device offset of -1
     with the hole's own length, so a binding that reads the length alone
     counts holes as physical pieces. *)
  let start = hole_start fd ~size in
  let es = Eio_fs_unix.extents fd ~file_offset:start ~length:4096 ~max:16 in
  Alcotest.(check int) "no extents inside a hole" 0 (List.length es);
  (* And a walk that starts before the hole stops AT it rather than crossing
     it: every extent it reports lies wholly in the data before the hole. *)
  let es = Eio_fs_unix.extents fd ~file_offset:0 ~length:size ~max:64 in
  Alcotest.(check bool)
    "the walk reported the data before the hole" true (es <> []);
  List.iter
    (fun (e : Eio_fs_unix.extent) ->
      Alcotest.(check bool)
        "no extent crosses or follows the hole" true
        (e.logical + e.length <= start);
      Alcotest.(check bool)
        "every extent has a device address" true (e.physical >= 0))
    es

let test_extents_honour_max () =
  mapped @@ fun () ->
  with_sparse_file @@ fun _env fd ~size ->
  let es = Eio_fs_unix.extents fd ~file_offset:0 ~length:size ~max:1 in
  Alcotest.(check bool)
    "at most one extent when one was asked for" true
    (List.length es <= 1)

(* An empty range has no extent, on every platform. *)
let test_extents_of_an_empty_range () =
  with_sparse_file @@ fun _env fd ~size:_ ->
  Alcotest.(check int)
    "none" 0
    (List.length (Eio_fs_unix.extents fd ~file_offset:0 ~length:0 ~max:16))

let test_extents_refuse_a_batch_they_cannot_hold () =
  with_sparse_file @@ fun _env fd ~size:_ ->
  let refuses name f =
    match f () with
    | (_ : Eio_fs_unix.extent list) -> Alcotest.failf "%s was accepted" name
    | exception Invalid_argument _ -> ()
  in
  refuses "max 0" (fun () ->
      Eio_fs_unix.extents fd ~file_offset:0 ~length:4096 ~max:0);
  refuses "max above the buffer ceiling" (fun () ->
      Eio_fs_unix.extents fd ~file_offset:0 ~length:4096 ~max:65537);
  refuses "a negative offset" (fun () ->
      Eio_fs_unix.extents fd ~file_offset:(-1) ~length:4096 ~max:16);
  refuses "a negative length" (fun () ->
      Eio_fs_unix.extents fd ~file_offset:0 ~length:(-1) ~max:16);
  refuses "a range past the largest offset" (fun () ->
      Eio_fs_unix.extents fd ~file_offset:max_int ~length:1 ~max:16)

(* The whole reason these three live in this library rather than beside a
   caller: they run on a system thread with the runtime system released, so a
   walk of a fragmented file suspends only the fiber walking it. Without the
   system thread the loop below has no suspension point in it at all and the
   ticking fiber cannot run until the walk has finished, which is what this
   case refutes.

   WHAT IT DOES NOT REFUTE: releasing the runtime system around the syscalls.
   That half is load-bearing, but this case makes short calls and the fiber
   gets its turn between them either way, so it cannot see a stub that holds
   the domain for each whole call. [test_lock_wait_does_not_block_eio] binds
   that half, holding its call open while another process holds the lock. *)
let test_sparse_map_does_not_block_eio () =
  mapped @@ fun () ->
  with_sparse_file @@ fun _env fd ~size ->
  let ran = ref false in
  Eio.Switch.run @@ fun sw ->
  (* The fork runs the ticking fiber at once, and its yield puts it behind
     this one: it runs again only when the walk suspends. *)
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Fiber.yield ();
      ran := true);
  for _ = 1 to 2000 do
    ignore (Eio_fs_unix.seek_data fd ~file_offset:0 : int option);
    ignore
      (Eio_fs_unix.extents fd ~file_offset:0 ~length:size ~max:8
        : Eio_fs_unix.extent list)
  done;
  Alcotest.(check bool)
    "another fiber ran while the map was being walked" true !ran

(* The in-memory backend registers a barrier that does nothing, so
   [sync_file] runs over it too. *)
let test_sync_dir_in_memory_no_op () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env -> Eio_fs_unix.sync_dir env#fs

let test_path_usage_counts_allocated_tree_without_following_links () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "a") "a";
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "sub/b") "payload";
  Eio.Path.symlink ~link_to:"sub" (dir / "link");
  let paths = [ dir; dir / "a"; dir / "sub"; dir / "sub/b"; dir / "link" ] in
  let expected_bytes, expected_mtime =
    List.fold_left
      (fun (bytes, mtime) path ->
        let stat = Eio.Path.stat ~follow:false path in
        ( Int64.add bytes (Int64.mul stat.blocks 512L),
          Float.max mtime stat.mtime ))
      (0L, 0.) paths
  in
  let usage = Eio_fs.Tree.usage dir in
  Alcotest.(check int64)
    "allocated blocks, with the symlink counted once" expected_bytes
    usage.allocated_bytes;
  Alcotest.(check (float 0.))
    "latest tree mtime" expected_mtime usage.latest_mtime

(* [usage] counts regular files and their bytes: two files of one and seven
   bytes, beside a directory and a link to a three-character target. *)
let test_path_usage_counts_the_files_a_copy_would_move () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "a") "a";
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "sub/b") "payload";
  Eio.Path.symlink ~link_to:"sub" (dir / "link");
  let usage = Eio_fs.Tree.usage dir in
  Alcotest.(check int) "regular files, and nothing else" 2 usage.files;
  Alcotest.(check int64)
    "the length of those files, summed" 8L usage.apparent_bytes;
  Alcotest.(check bool)
    "which is no more than the tree allocates" true
    (Int64.compare usage.apparent_bytes usage.allocated_bytes <= 0)

(* The walk usage reads: each entry once, with its own stat, a symbolic link
   as itself and never what it names. *)
let test_walk_visits_each_entry_once () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "a") "a";
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "sub/b") "payload";
  Eio.Path.symlink ~link_to:"sub" (dir / "link");
  let seen = ref [] in
  Eio_fs_internal.walk ~max_fibers:2
    (fun _ path (stat : Eio.File.Stat.t) ->
      let name = if path == dir then "." else Filename.basename (snd path) in
      let kind =
        match stat.kind with
        | `Directory -> "dir"
        | `Regular_file -> "file"
        | `Symbolic_link -> "link"
        | _ -> "other"
      in
      seen := (name, kind) :: !seen)
    dir;
  Alcotest.(check (list (pair string string)))
    "every entry, once"
    [
      (".", "dir");
      ("a", "file");
      ("b", "file");
      ("link", "link");
      ("sub", "dir");
    ]
    (List.sort compare !seen)

(* An empty tree has nothing to move, and says so rather than reporting the
   directory that holds nothing as content. *)
let test_path_usage_of_an_empty_tree_moves_nothing () =
  with_real_dir @@ fun dir ->
  let usage = Eio_fs.Tree.usage dir in
  Alcotest.(check int) "no files" 0 usage.files;
  Alcotest.(check int64) "no bytes to carry" 0L usage.apparent_bytes

(* renameat2(2) and renameatx_np(2) exist only on Linux, FreeBSD and Darwin;
   elsewhere [exclusive_rename] is not available. *)
let renames f =
  try f () with Eio.Io (Eio.Exn.Not_available _, _) -> Alcotest.skip ()

(* Linux renameat2(RENAME_NOREPLACE) and Darwin renameatx_np(RENAME_EXCL)
   require refusal when the destination already exists. The source survives. *)
let test_exclusive_rename_refuses_existing_names () =
  renames @@ fun () ->
  with_real_dir @@ fun dir ->
  let source = dir / "source" and target = dir / "target" in
  Eio.Path.save ~create:(`Exclusive 0o600) source "new";
  Eio.Path.save ~create:(`Exclusive 0o600) target "old";
  let rename () =
    Eio_fs.Sync.Rename.exclusive Eio_fs_unix.exclusive_rename source target
  in
  (match rename () with
  | () -> Alcotest.fail "exclusive rename replaced a file"
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  Alcotest.(check string) "target retained" "old" (Eio.Path.load target);
  Eio.Path.unlink target;
  Eio.Path.symlink ~link_to:"missing" target;
  (match rename () with
  | () -> Alcotest.fail "exclusive rename replaced a dangling link"
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  Alcotest.(check string) "link retained" "missing" (Eio.Path.read_link target);
  Alcotest.(check string) "source retained" "new" (Eio.Path.load source)

let test_exclusive_rename_refuses_embedded_nul () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "source") "payload";
  List.iter
    (fun (source, target) ->
      (match
         Eio_fs.Sync.Rename.exclusive Eio_fs_unix.exclusive_rename
           (dir / source) (dir / target)
       with
      | () -> Alcotest.fail "an embedded NUL reached the filesystem"
      | exception Eio.Io _ -> ());
      Alcotest.(check string)
        "source unchanged" "payload"
        (Eio.Path.load (dir / "source"));
      Alcotest.(check bool)
        "truncated target absent" true
        (Eio.Path.kind ~follow:false (dir / "target") = `Not_found))
    [ ("source\000suffix", "target"); ("source", "target\000suffix") ]

let test_exclusive_publication_reaches_crash_model () =
  renames @@ fun () ->
  with_real_dir @@ fun dir ->
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl dir in
  Eio_fs.Sync.publish ~perm:0o600 ~exclusive:Eio_fs_unix.exclusive_rename
    ~max_bytes:7L (fs / "state")
    (Eio.Flow.copy_string "payload");
  Crash.cut ctl;
  Alcotest.(check string)
    "published bytes survive" "payload"
    (Eio.Path.load (Crash.post_crash ctl / "state"))

(* The platform barrier carries a publication on a real directory, a durable
   generation's read-only handles included. *)
let test_publication_with_the_platform_barrier () =
  with_real_dir @@ fun dir ->
  let barrier = Eio_fs_unix.barrier in
  Eio_fs.Sync.publish ~barrier ~max_bytes:7L (dir / "state")
    (Eio.Flow.copy_string "payload");
  Eio_fs.Sync.publish_generation ~barrier (dir / "tree") (fun d ->
      Eio.Path.save ~create:(`Exclusive 0o444) (d / "f") "frozen");
  Alcotest.(check string) "the file" "payload" (Eio.Path.load (dir / "state"));
  Alcotest.(check string)
    "the generation" "frozen"
    (Eio.Path.load (dir / "tree/f"))

let test_concurrent_exclusive_publications_have_one_winner () =
  renames @@ fun () ->
  with_real_dir @@ fun dir ->
  let save text () =
    match
      Eio_fs.Sync.publish ~perm:0o600 ~exclusive:Eio_fs_unix.exclusive_rename
        ~max_bytes:3L (dir / "winner")
        (Eio.Flow.copy_string text)
    with
    | () -> Some text
    | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> None
  in
  let first, second = Eio.Fiber.pair (save "one") (save "two") in
  match (first, second) with
  | Some expected, None | None, Some expected ->
      Alcotest.(check string)
        "the winner owns the target" expected
        (Eio.Path.load (dir / "winner"))
  | _ -> Alcotest.fail "exactly one exclusive publication must succeed"

let test_exclusive_rename_keeps_open_parent () =
  renames @@ fun () ->
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o700 (dir / "parent");
  Eio.Path.mkdir ~perm:0o700 (dir / "elsewhere");
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "parent" / "source") "payload";
  Eio.Switch.run @@ fun sw ->
  let parent = Eio.Path.open_dir ~sw (dir / "parent") in
  Eio.Path.rename (dir / "parent") (dir / "moved");
  Eio.Path.symlink ~link_to:"elsewhere" (dir / "parent");
  Eio_fs.Sync.Rename.exclusive Eio_fs_unix.exclusive_rename (parent / "source")
    (parent / "target");
  Alcotest.(check string)
    "original directory holds the move" "payload"
    (Eio.Path.load (dir / "moved" / "target"));
  Alcotest.(check (list string))
    "replacement directory untouched" []
    (Eio.Path.read_dir (dir / "elsewhere"))

(* A call that takes an [Eio.Path.t] reports a failure as [Eio.Io], as Eio's
   own backends do, never as the [Unix.Unix_error] of the call beneath. *)
let test_path_calls_raise_eio_io () =
  with_real_dir @@ fun dir ->
  let missing = dir / "missing" in
  let not_found name f =
    match f () with
    | () -> Alcotest.failf "%s of a missing path succeeded" name
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
  in
  (* A host with no xattr(7) calls refuses before it looks at the path. *)
  let xattr_not_found name f =
    try not_found name f with Eio.Io (Eio.Exn.Not_available _, _) -> ()
  in
  not_found "statvfs" (fun () -> ignore (Eio_fs_unix.statvfs missing));
  xattr_not_found "xattrs" (fun () -> ignore (Eio_fs_unix.xattrs missing));
  xattr_not_found "set_xattr" (fun () ->
      Eio_fs_unix.set_xattr missing "user.a" "v");
  Eio.Path.mkdir ~perm:0o755 (dir / "dst");
  match Eio_fs_unix.clone_tree missing (dir / "dst") with
  | false -> ()
  | true -> Alcotest.fail "cloned a missing path"
  | exception Eio.Io _ -> ()

(* A library handed only [sandbox], a directory opened beneath [dir], reaches
   nothing outside it through these calls, as it reaches nothing through
   [Eio.Path]: a [..] above the sandbox and a symbolic link leading out of it
   are refused, and a final symbolic link is never followed. The sandbox
   itself, named by the empty path, is still reachable. *)
let test_path_calls_stay_in_their_directory () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "sandbox");
  Eio.Path.mkdir ~perm:0o755 (dir / "outside");
  let secret = dir / "outside" / "secret" in
  Eio.Path.save ~create:(`Exclusive 0o644) secret "outside";
  Eio.Path.symlink ~link_to:"../outside" (dir / "sandbox" / "up");
  Eio.Path.symlink ~link_to:"../outside/secret" (dir / "sandbox" / "leaf");
  Eio.Switch.run @@ fun sw ->
  let sandbox = Eio.Path.open_dir ~sw (dir / "sandbox") in
  Eio.Path.save ~create:(`Exclusive 0o644) (sandbox / "payload") "planted";
  let refused what f =
    match f () with
    | _ -> Alcotest.failf "%s reached outside its directory" what
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ()
  in
  List.iter
    (fun out ->
      refused ("clone_tree from " ^ out) (fun () ->
          Eio_fs_unix.clone_tree (sandbox / out) (sandbox / "stolen"));
      refused ("statvfs of " ^ out) (fun () ->
          Eio_fs_unix.statvfs (sandbox / out));
      refused ("xattrs of " ^ out) (fun () ->
          Eio_fs_unix.xattrs (sandbox / out));
      refused ("set_xattr of " ^ out) (fun () ->
          Eio_fs_unix.set_xattr (sandbox / out) "user.escaped" "1"))
    [ "../outside/secret"; "up/secret" ];
  List.iter
    (fun out ->
      refused ("clone_tree to " ^ out) (fun () ->
          Eio_fs_unix.clone_tree (sandbox / "payload") (sandbox / out)))
    [ "../outside/planted"; "up/planted" ];
  Alcotest.(check bool)
    "nothing cloned in" true
    (Eio.Path.kind ~follow:false (sandbox / "stolen") = `Not_found);
  Alcotest.(check bool)
    "nothing planted outside" true
    (Eio.Path.kind ~follow:false (dir / "outside" / "planted") = `Not_found);
  (match Eio_fs_unix.set_xattr (sandbox / "leaf") "user.escaped" "1" with
  | () | (exception Eio.Io _) -> ());
  (match Eio_fs_unix.clone_tree (sandbox / "leaf") (sandbox / "copy") with
  | _ -> ()
  | exception Eio.Io _ -> ());
  Alcotest.(check bool)
    "a final link is cloned as a link, if at all" true
    (Eio.Path.kind ~follow:false (sandbox / "copy") <> `Regular_file);
  (match Eio_fs_unix.xattrs secret with
  | attributes ->
      Alcotest.(check (option string))
        "a final link is not followed" None
        (List.assoc_opt "user.escaped" attributes)
  | exception Eio.Io _ -> ());
  Alcotest.(check bool)
    "the sandbox itself" true
    (Int64.compare (Eio_fs_unix.statvfs sandbox).total_blocks 0L > 0)

(* xattr(7): a value is a byte string, and the no-follow calls act on a link
   itself. The value holds a NUL byte, which a C-string round trip would cut. *)
let user_value = "v\000alue"

let supported f =
  try f ()
  with
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EOPNOTSUPP, _, _)), _)
  | Eio.Io (Eio.Exn.Not_available _, _)
  ->
    Alcotest.skip ()

let test_xattr_round_trip () =
  with_real_dir @@ fun dir ->
  let file = dir / "f" in
  Eio.Path.save ~create:(`Exclusive 0o644) file "data";
  supported @@ fun () ->
  Eio_fs_unix.set_xattr file "user.b" "second";
  Eio_fs_unix.set_xattr file "user.a" user_value;
  let got =
    List.filter
      (fun (n, _) -> String.starts_with ~prefix:"user." n)
      (Eio_fs_unix.xattrs file)
  in
  Alcotest.(check (list (pair string string)))
    "user.* attributes, in name order"
    [ ("user.a", user_value); ("user.b", "second") ]
    got

let test_xattr_does_not_follow () =
  with_real_dir @@ fun dir ->
  let file = dir / "target" and link = dir / "link" in
  Eio.Path.save ~create:(`Exclusive 0o644) file "data";
  Eio.Path.symlink ~link_to:"target" link;
  supported @@ fun () ->
  Eio_fs_unix.set_xattr file "user.a" user_value;
  Alcotest.(check (option string))
    "the link does not report its target's attribute" None
    (List.assoc_opt "user.a" (Eio_fs_unix.xattrs link))

(* A revoked tree keeps no write bit, its links untouched, and its owner can
   be granted it again. *)
let test_revoke_writes () =
  with_real_dir @@ fun dir ->
  let tree = dir / "tree" in
  Eio.Path.mkdirs ~perm:0o755 (tree / "sub");
  Eio.Path.save ~create:(`Exclusive 0o644) (tree / "sub" / "file") "data";
  Eio.Path.save ~create:(`Exclusive 0o444) (tree / "kept") "data";
  Eio.Path.symlink ~link_to:"sub/file" (tree / "link");
  Eio_fs.Tree.revoke_writes tree;
  let perm p = (Eio.Path.stat ~follow:false p).perm in
  Alcotest.(check (list int))
    "no entry keeps a write bit"
    [ 0o555; 0o555; 0o444; 0o444 ]
    (List.map perm [ tree; tree / "sub"; tree / "sub" / "file"; tree / "kept" ]);
  Alcotest.(check string)
    "the link is left" "sub/file"
    (Eio.Path.read_link (tree / "link"));
  Eio_fs.Tree.grant_owner tree

(* POSIX truncate leaves unwritten ranges zero. The independent operation
   trace pins the data extents, so filling those gaps with zero writes fails
   even if the published bytes still look right. *)
let test_sparse_cost size =
  renames @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir =
    fs / Filename.get_temp_dir_name () / Eio_fs.Sync.fresh_name "sparse"
  in
  Eio.Path.mkdir ~perm:0o700 dir;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree dir) @@ fun () ->
  let writes = ref [] in
  let schedule (op : Eio_fs_chaos.Op.t) =
    if op.kind = `Write then writes := (op.offset, op.length) :: !writes;
    None
  in
  (* The wrapper closes what it wraps: an opened directory, which the switch
     closes, not the root of [fs]. *)
  Eio.Switch.run @@ fun sw ->
  let observed = Eio_fs_chaos.wrap schedule (Eio.Path.open_dir ~sw dir) in
  Eio_fs.Sync.publish_sparse ~max_bytes:(Int64.of_int size)
    ~exclusive:Eio_fs_unix.exclusive_rename ~size_bytes:(Int64.of_int size)
    (observed / "image")
    [
      (0L, Cstruct.of_string "HDR");
      (Int64.of_int (size - 4), Cstruct.of_string "TAIL");
    ];
  Alcotest.(check (list (pair int int)))
    "only seven supplied bytes are written, independent of logical size"
    [ (0, 3); (size - 4, 4) ]
    (List.rev !writes);
  Eio.Path.with_open_in (dir / "image") @@ fun file ->
  Alcotest.(check int64)
    "logical size" (Int64.of_int size)
    (Optint.Int63.to_int64 (Eio.File.size file));
  let read offset length =
    let data = Cstruct.create length in
    Eio.File.pread_exact file ~file_offset:(Optint.Int63.of_int offset) [ data ];
    Cstruct.to_string data
  in
  Alcotest.(check string) "header" "HDR" (read 0 3);
  Alcotest.(check string)
    "hole reads as zero" (String.make 4 '\000') (read 4096 4);
  Alcotest.(check string) "tail at the final extent" "TAIL" (read (size - 4) 4)

let suite =
  ( "eio_fs_unix",
    [
      Alcotest.test_case "exclusive rename refuses existing names" `Quick
        test_exclusive_rename_refuses_existing_names;
      Alcotest.test_case "exclusive rename refuses embedded NUL" `Quick
        test_exclusive_rename_refuses_embedded_nul;
      Alcotest.test_case "exclusive publication reaches crash model" `Quick
        test_exclusive_publication_reaches_crash_model;
      Alcotest.test_case "publication with the platform barrier" `Quick
        test_publication_with_the_platform_barrier;
      Alcotest.test_case "concurrent exclusive publications have one winner"
        `Quick test_concurrent_exclusive_publications_have_one_winner;
      Alcotest.test_case "exclusive rename keeps its open parent" `Quick
        test_exclusive_rename_keeps_open_parent;
      Alcotest.test_case "barrier after rename" `Quick
        test_sync_dir_after_rename;
      Alcotest.test_case "barrier empty and repeated" `Quick
        test_sync_dir_empty_and_repeated;
      Alcotest.test_case "barrier lets a sibling fiber run" `Quick
        test_sync_dir_lets_a_sibling_fiber_run;
      Alcotest.test_case "barrier reaches the directory held" `Quick
        test_sync_dir_barriers_the_held_directory;
      Alcotest.test_case "barrier refuses a name a link took" `Quick
        test_sync_dir_refuses_a_name_a_link_took;
      Alcotest.test_case "barrier refuses a relative link" `Quick
        test_sync_dir_refuses_a_relative_link;
      Alcotest.test_case "file barrier asks the registration first" `Quick
        test_sync_file_asks_the_registration_before_the_descriptor;
      Alcotest.test_case "file barrier takes an unregistered file's descriptor"
        `Quick test_sync_file_takes_the_descriptor_when_nothing_is_registered;
      Alcotest.test_case "file barrier survives a modelled cut" `Quick
        test_sync_file_under_the_crash_model_survives_the_cut;
      Alcotest.test_case "file barrier on a plain POSIX file" `Quick
        test_sync_file_on_a_plain_posix_file;
      Alcotest.test_case "sync_file of a host descriptor" `Quick
        test_sync_file_of_a_descriptor;
      Alcotest.test_case "sync_file surfaces errors" `Quick
        test_sync_file_surfaces_errors;
      Alcotest.test_case "statvfs reports raw block capacity" `Quick
        test_statvfs_reports_raw_block_capacity;
      Alcotest.test_case "clone_tree" `Quick test_clone_tree;
      Alcotest.test_case "rmtree" `Quick test_rmtree;
      Alcotest.test_case "rmtree stops above a refusal" `Quick
        test_rmtree_refused;
      Alcotest.test_case "rmtree's hops follow its directories" `Quick
        test_rmtree_hops;
      Alcotest.test_case "rmtree ends at a cancellation" `Quick
        test_rmtree_cancelled;
      Alcotest.test_case "calls on a path raise Eio.Io" `Quick
        test_path_calls_raise_eio_io;
      Alcotest.test_case "path calls stay in their directory" `Quick
        test_path_calls_stay_in_their_directory;
      Alcotest.test_case "pread leaves the descriptor's offset" `Quick
        (pread_leaves_the_offset ~cached:false);
      Alcotest.test_case "a cached pread leaves the descriptor's offset" `Quick
        (pread_leaves_the_offset ~cached:true);
      Alcotest.test_case "a long cached pread is an uncached one" `Quick
        test_pread_cached_bound;
      Alcotest.test_case "pwrite leaves the descriptor's offset" `Quick
        test_pwrite_leaves_the_offset;
      Alcotest.test_case "positioned calls on a file in memory" `Quick
        test_positioned_in_memory;
      Alcotest.test_case "a file that registers Positioned answers" `Quick
        test_positioned_wrapper;
      Alcotest.test_case "seek_data and seek_hole walk a sparse file" `Quick
        test_seek_data_and_hole_walk_the_file;
      Alcotest.test_case "extents report the physical map" `Quick
        test_extents_report_the_physical_map;
      Alcotest.test_case "extents stop at a hole" `Quick
        test_extents_stop_at_a_hole;
      Alcotest.test_case "extents honour max" `Quick test_extents_honour_max;
      Alcotest.test_case "extents of an empty range" `Quick
        test_extents_of_an_empty_range;
      Alcotest.test_case "extents refuse a batch they cannot hold" `Quick
        test_extents_refuse_a_batch_they_cannot_hold;
      Alcotest.test_case "the sparse map does not block Eio" `Quick
        test_sparse_map_does_not_block_eio;
      Alcotest.test_case "in-memory barrier is a no-op" `Quick
        test_sync_dir_in_memory_no_op;
      Alcotest.test_case "path usage is allocated and symlink-safe" `Quick
        test_path_usage_counts_allocated_tree_without_following_links;
      Alcotest.test_case "path usage counts what a copy would move" `Quick
        test_path_usage_counts_the_files_a_copy_would_move;
      Alcotest.test_case "the walk visits each entry once" `Quick
        test_walk_visits_each_entry_once;
      Alcotest.test_case "path usage of an empty tree moves nothing" `Quick
        test_path_usage_of_an_empty_tree_moves_nothing;
      Alcotest.test_case "xattr(7) values round-trip byte for byte" `Quick
        test_xattr_round_trip;
      Alcotest.test_case "xattr(7) calls do not follow a final link" `Quick
        test_xattr_does_not_follow;
      Alcotest.test_case "a revoked tree keeps no write bit" `Quick
        test_revoke_writes;
      Alcotest.test_case "sparse 8 MiB extents and cost" `Quick (fun () ->
          test_sparse_cost (8 * 1024 * 1024));
      Alcotest.test_case "sparse 80 MiB extents and cost" `Quick (fun () ->
          test_sparse_cost (80 * 1024 * 1024));
    ] )
