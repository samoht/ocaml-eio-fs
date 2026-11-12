(* Smoke tests for [Eio_fs_unix]: the barriers run against a real filesystem
   and surface errors as documented. Durability itself is not observable
   without cutting power; what is testable is that the barrier accepts the
   states a commit protocol hands it. *)

module Chaos = Eio_fs.Chaos.Private
module Crash = Chaos.Crash

let ( / ) = Eio.Path.( / )

let mentions ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  at 0

let with_real_dir_env fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-dir" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () -> fn env (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t)

let with_real_dir fn = with_real_dir_env (fun _env dir -> fn dir)

let with_in_memory_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn env#fs

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

(* ---- the barrier lands on the directory the caller holds ----

   [sync_dir] is handed an [Eio.Path.t]: a directory capability and a name
   resolved beneath it. Flattening that pair into a native string and opening
   the string from [/] hands the barrier to whatever the name resolves to at
   that instant. Eio says as much of the flattening itself -- [Path.native]
   "does not check for confinement (the resulting path might not be accessible
   via [t] itself)", and "if a directory was opened with [open_subtree] and
   later renamed, this might use the old name" -- and the last step of a commit
   protocol is exactly where that matters, since what a barrier reports durable
   has to be the directory whose entry was just renamed into place.

   Which inode an [fsync] reached is not observable from a program: nothing
   short of cutting power tells a barrier on one directory from a barrier on
   another. The resolution is observable, and these two cases pin it from both
   sides.

   Here the capability outlives the name. The directory is opened, then its
   name is taken away; the caller still holds the directory, so the barrier it
   asks for is the one that directory can give and there is nothing to fail. A
   barrier that re-resolves the name instead finds nothing under it and fails a
   publication that is already complete. *)
let test_sync_dir_barriers_the_held_directory () =
  with_real_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o700 (root / "d");
  Eio.Path.with_open_dir (root / "d") @@ fun d ->
  Eio.Path.rename (root / "d") (root / "moved");
  Eio_fs_unix.sync_dir d

(* And here the name outlives the directory, which is the shape that costs a
   publication rather than merely failing one. The store is moved aside and a
   link to a directory of the adversary's own left under its name: re-resolving
   the name follows the link, fsyncs a directory the caller never named, and
   returns, so the caller is told its rename reached stable storage while
   nothing of its own was flushed at all. Resolving through the capability
   refuses the link, its target being outside the tree the capability names, so
   the caller learns rather than being handed a barrier it did not get. *)
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

(* The barrier runs on a fiber that shares its domain with every other fiber of
   the program: a server publishes while it serves. So the open and the fsync
   belong on a system thread, not on the fiber's own thread -- an inline pair
   holds the domain's scheduler inside the C call for the whole barrier, which
   on Darwin is F_FULLFSYNC, through to the platter and the slowest call the
   process makes. Here one fiber issues a run of barriers while a sibling
   counts its own turns; the sibling gets none at all while the pair runs
   inline. The sibling spins for as long as the barriers take, so the run is
   short: a directory fsync can take 100 ms (an ext4 on a loop device), and
   the count separates the two shapes at any length. *)
let test_sync_dir_lets_a_sibling_fiber_run () =
  with_real_dir @@ fun dir ->
  (* One turn every other barrier is a floor the fixed shape clears with room
     to spare -- it suspends once per barrier -- and the inline pair cannot
     reach at all, having no suspension point between the first barrier and
     the last. ([/] is [Eio.Path.( / )] here, so the floor is spelt out.) *)
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

(* full_fsync accepts a regular file descriptor, including a read-only one. *)
let test_full_fsync_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "f") "x";
  let path = Eio.Path.native_exn (dir / "f") in
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Eio_fs_unix.full_fsync fd)

(* full_fsync pushes a real file's data to stable storage and surfaces the OS
   error on a dead descriptor -- on Darwin this exercises the F_FULLFSYNC path
   (and its fsync fallback), elsewhere plain fsync. Power-cut durability
   itself is not unit-testable; what is pinned here is that the barrier is
   issued on the right descriptor and failures are not swallowed. *)
let test_full_fsync_surfaces_errors () =
  let path = Filename.temp_file "eio-fs-fullfsync" ".dat" in
  let fd = Unix.openfile path [ Unix.O_WRONLY ] 0o644 in
  let written = Unix.write_substring fd "durable bytes" 0 13 in
  Alcotest.(check int) "wrote the payload" 13 written;
  Eio_fs_unix.full_fsync fd;
  Unix.close fd;
  Sys.remove path;
  match Eio_fs_unix.full_fsync fd with
  | () -> Alcotest.fail "full_fsync accepted a closed descriptor"
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> ()

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
          (Eio_fs_sync.File_sync, fun (t : Registered_file.t) -> incr t.calls);
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

(* The other half, and the reason this entry point exists at all rather than
   deferring to the portable one. A file that registers no barrier is where the
   platform's own belongs: on Darwin F_FULLFSYNC, through to the platter, where
   the file's own sync stops at the drive's cache. That the descriptor is
   reached for is what the same closed descriptor says here, from the other
   side -- the barrier must raise. The portable barrier over the same file is
   what says the raise can only be the descriptor: it takes the file's own
   sync, which succeeds. So an implementation that quietly deferred to the
   portable route -- the durability guarantee traded away for a greener suite
   -- returns here where this one raises. *)
let test_sync_file_takes_the_descriptor_when_nothing_is_registered () =
  with_real_dir @@ fun dir ->
  Eio.Switch.run @@ fun sw ->
  let base = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (dir / "f") in
  let file, _ = registered_file ~sw ~register:false base in
  Eio_fs_sync.sync_file file;
  match Eio_fs_unix.sync_file file with
  | () ->
      Alcotest.fail
        "the barrier skipped the descriptor of a file that registers none"
  | exception Invalid_argument _ -> ()

(* A file imported over a host descriptor reads and writes the descriptor's
   file at the offsets asked, and is that descriptor: [fd_opt] reaches it, and
   a switch that ends leaves open what [close_unix:false] says it does not
   own. The bytes written are not a period of the offsets read. *)
let test_import_file () =
  with_real_dir @@ fun dir ->
  let name = Option.get (Eio.Path.native (dir / "f")) in
  let raw = Unix.openfile name [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close raw) @@ fun () ->
  let at = Optint.Int63.of_int in
  Eio.Switch.run (fun sw ->
      let file = Eio_fs_unix.import_file ~sw ~close_unix:false raw in
      Eio.File.pwrite_all file ~file_offset:(at 3)
        [ Cstruct.of_string "abcdefg" ];
      let buf = Cstruct.create 4 in
      Eio.File.pread_exact file ~file_offset:(at 5) [ buf ];
      Alcotest.(check string)
        "read at its offset" "cdef" (Cstruct.to_string buf);
      Alcotest.(check int)
        "stat reads the file's size" 10
        (Optint.Int63.to_int (Eio.File.stat file).size);
      Eio.File.truncate file (at 6);
      Alcotest.(check string)
        "the host reads what the file wrote" "\000\000\000abc"
        (In_channel.with_open_bin name In_channel.input_all);
      Alcotest.(check bool)
        "the resource is the descriptor" true
        (match Eio_unix.Resource.fd_opt file with
        | None -> false
        | Some fd -> Eio_unix.Fd.use_exn "import" fd (fun fd -> fd = raw)));
  Alcotest.(check int)
    "the descriptor outlives the switch" 6 (Unix.fstat raw).st_size

(* The same ordering in the shape a store publishes in, over the real
   filesystem it runs on. The crash wrapper's file hands its caller no host
   descriptor, so the barrier reaches the model's own sync and not a real
   F_FULLFSYNC the model never recorded -- after which [Drop_unsynced] would
   drop the image bytes the store was told had reached the platter.

   Two files take the same cut, and only one is barriered. The unbarriered one
   is the control: it says the model still drops what no barrier covered, so
   the barriered one surviving is the barrier being seen and not the model
   having gone soft. Both entries are committed, the names being the metadata
   journal's business, so what differs between the two is the data barrier
   alone. *)
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
   own -- F_FULLFSYNC on Darwin. Power-cut durability is not unit-testable;
   what is pinned here is that the barrier is issued and the file reads back
   whole after it, the file half of [test_sync_dir_after_rename]. *)
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

(* POSIX pread: the bytes at the offset asked, the count read, 0 at the end of
   the file, and the descriptor's own offset where it was. A pread written as a
   seek and a read passes the first three and fails the last. *)
let test_pread_leaves_the_offset () =
  with_real_dir_env @@ fun _env dir ->
  let path = Eio.Path.native_exn (dir / "pread.bin") in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let text = "0123456789abcdef" in
  let n = Unix.write_substring fd text 0 (String.length text) in
  Alcotest.(check int) "the whole text was written" (String.length text) n;
  ignore (Unix.lseek fd 3 Unix.SEEK_SET : int);
  let buf = Bytes.make 8 '.' in
  let got = Eio_fs_unix.pread fd ~file_offset:10 buf ~pos:2 ~len:4 in
  Alcotest.(check int) "four bytes read" 4 got;
  Alcotest.(check string)
    "the bytes at offset 10, at pos 2" "..abcd.." (Bytes.to_string buf);
  Alcotest.(check int)
    "the descriptor's offset is untouched" 3
    (Unix.lseek fd 0 Unix.SEEK_CUR);
  let got = Eio_fs_unix.pread fd ~file_offset:14 buf ~pos:0 ~len:8 in
  Alcotest.(check int) "a read across the end takes what is there" 2 got;
  Alcotest.(check int)
    "a read at the end is 0" 0
    (Eio_fs_unix.pread fd ~file_offset:16 buf ~pos:0 ~len:8);
  Alcotest.check_raises "a range past the buffer is refused"
    (Invalid_argument "Eio_fs_unix.pread") (fun () ->
      ignore (Eio_fs_unix.pread fd ~file_offset:0 buf ~pos:6 ~len:4 : int))

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

let test_seek_data_and_hole_walk_the_file () =
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

(* tmpfs and virtiofs have no FIEMAP: EOPNOTSUPP. *)
let mapped f =
  try f () with Unix.Unix_error (Unix.EOPNOTSUPP, _, _) -> Alcotest.skip ()

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

(* The in-memory backend registers its barrier as a no-op -- volatile storage
   has nothing to sync -- so the platter-strength entry point runs unchanged
   over it too. *)
let test_sync_dir_in_memory_no_op () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> Eio_fs_unix.sync_dir env#fs

let await_holder child =
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "lock holder exited %d" n
  | `Signaled n -> Alcotest.failf "lock holder took signal %d" n

let with_held_lock ~sw ~env ~clock ~lock fn =
  let manager = Eio.Stdenv.process_mgr env in
  let input, release = Eio.Process.pipe ~sw manager in
  let ready, output = Eio.Process.pipe ~sw manager in
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw manager ~stdin:input ~stdout:output ~executable:self
      [ self; "--hold-eio-fs-lock-until-release"; Eio.Path.native_exn lock ]
  in
  Eio.Resource.close input;
  Eio.Resource.close output;
  let announced = Cstruct.create 1 in
  Eio.Time.with_timeout_exn clock 10.0 (fun () ->
      Eio.Flow.read_exact ready announced);
  Alcotest.(check char)
    "holder acquired the native lock" 'R'
    (Cstruct.get_char announced 0);
  Fun.protect
    ~finally:(fun () ->
      Eio.Flow.copy_string "X" release;
      Eio.Resource.close release;
      Eio.Time.with_timeout_exn clock 10.0 (fun () -> await_holder child))
    fn

(* A process holding [lock] by [kind] ([test.ml], [hold_until_release]), and
   [release], which tells it to let go and returns how it ended: [`Exited 0]
   when it let go at [release], [`Exited 3] when it let go at its own
   five-second bound. *)
let holding_process ~sw ~env ~clock ~kind ~lock =
  let manager = Eio.Stdenv.process_mgr env in
  let input, release = Eio.Process.pipe ~sw manager in
  let ready, output = Eio.Process.pipe ~sw manager in
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw manager ~stdin:input ~stdout:output ~executable:self
      [ self; "--hold-until-release"; kind; Eio.Path.native_exn lock ]
  in
  Eio.Resource.close input;
  Eio.Resource.close output;
  let announced = Cstruct.create 1 in
  Eio.Time.with_timeout_exn clock 5.0 (fun () ->
      Eio.Flow.read_exact ready announced);
  Alcotest.(check char)
    "the other process holds the lock" 'R'
    (Cstruct.get_char announced 0);
  ( child,
    fun () ->
      (try Eio.Flow.copy_string "X" release with Eio.Io _ -> ());
      Eio.Resource.close release;
      Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Process.await child) )

let status =
  Alcotest.testable
    (fun ppf -> function
      | `Exited n -> Fmt.pf ppf "exited %d" n
      | `Signaled n -> Fmt.pf ppf "signalled %d" n)
    ( = )

(* A wait for a lock another process holds is the kernel's blocking request
   (POSIX fcntl F_SETLKW, "fcntl() shall wait until the request can be
   satisfied"; flock(2) without LOCK_NB), which answers when the holder lets
   go: the waiter asks once, says it waits once, and is granted at the
   release, with no further ask. [take ~on_wait lock fn] is the acquisition
   under test. The waiter's grant is read after the holder was told to let
   go, and never before. *)
let a_wait_answers_at_the_release ~kind take () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~clock ~kind ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let waits = ref 0 in
  let granted, now_granted = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      take
        ~on_wait:(fun () ->
          incr waits;
          ignore (Eio.Promise.try_resolve now_waiting ()))
        lock
        (fun () -> Eio.Promise.resolve now_granted ()));
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await waiting);
  Alcotest.(check bool)
    "not granted while held" false
    (Eio.Promise.is_resolved granted);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ());
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await granted);
  Alcotest.(check int) "said it waits once" 1 !waits

(* The same wait is one a caller can end: cancelled once the waiter is in the
   kernel's wait, it answers at the cancel while the holder still holds (the
   holder lets go at the test's word, after the cancel, so a wait that
   answered only at a release finds the holder gone at its own bound), and it
   leaves no hold behind: once the holder lets go, [free lock] finds the lock
   free. *)
let a_wait_is_cancellable ~kind take ~free () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~clock ~kind ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let answer =
    Eio.Fiber.first
      (fun () ->
        take
          ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
          lock
          (fun () -> `Granted))
      (fun () ->
        Eio.Promise.await waiting;
        `Cancelled)
  in
  (match answer with
  | `Granted -> Alcotest.fail "a lock another process holds was granted"
  | `Cancelled -> ());
  Alcotest.check status "the holder let go when told, after the cancel"
    (`Exited 0) (release ());
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> free ~env lock)

(* A lease a fiber of this process holds is waited for under [`Fibers], and
   the waiter says it waits, once, before it queues: a caller that waits on
   its own run's fibers can tell it from one that ran at once. The waiter is
   granted once the holder lets go, and not before. *)
let test_lease_wait_for_a_fiber_says_so () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held, now_held = Eio.Promise.create () in
  let go, now_go = Eio.Promise.create () in
  let waiting, now_waiting = Eio.Promise.create () in
  let waits = ref 0 in
  let granted, now_granted = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.Lease.with_exclusive ~block:`Fibers lock (fun _ ->
          Eio.Promise.resolve now_held ();
          Eio.Promise.await go));
  Eio.Promise.await held;
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.Lease.with_exclusive ~block:`Fibers
        ~on_wait:(fun () ->
          incr waits;
          ignore (Eio.Promise.try_resolve now_waiting ()))
        lock
        (fun _ -> Eio.Promise.resolve now_granted ()));
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await waiting);
  Alcotest.(check bool)
    "not granted while held" false
    (Eio.Promise.is_resolved granted);
  Eio.Promise.resolve now_go ();
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await granted);
  Alcotest.(check int) "said it waits once" 1 !waits

(* A monotonic clock that counts the waits begun on it and never moves, so a
   bound set on it never lapses and a wait that paces itself on it never asks
   again. *)
module Still_clock = struct
  type t = int ref
  type time = Mtime.t

  let now _ = Mtime.min_stamp

  let sleep_until begun _ =
    incr begun;
    Eio.Fiber.await_cancel ()
end

(* [on_mono clock f] is [f ()] with every [Eio_unix.sleep] of the fibers [f]
   starts on [clock]. *)
let rec on_mono : type r. Eio.Time.Mono.ty Eio.Resource.t -> (unit -> r) -> r =
 fun clock f ->
  let effc : type a. a Effect.t -> ((a, r) Effect.Deep.continuation -> r) option
      = function
    | Eio_unix.Private.Get_monotonic_clock ->
        Some (fun k -> Effect.Deep.continue k clock)
    | Eio.Private.Effects.Fork (ctx, child) ->
        Some
          (fun k ->
            Effect.perform
              (Eio.Private.Effects.Fork (ctx, fun () -> on_mono clock child));
            Effect.Deep.continue k ())
    | _ -> None
  in
  Effect.Deep.try_with f () { effc }

(* [counting_waits f] is [f ()] on a {!Still_clock}, and the number of waits
   begun on it. *)
let counting_waits f =
  let begun = ref 0 in
  let clock = Eio.Resource.T (begun, Eio.Time.Pi.clock (module Still_clock)) in
  let value = on_mono clock f in
  (value, !begun)

(* A wait bounded by [`For] is the same kernel request, raced against its
   bound: the waiter asks once, says it waits once, and is granted at the
   release, having begun one wait on the monotonic clock, its bound's. The
   clock never moves, so a wait that asked again on a cadence would begin a
   second wait and never ask again. *)
let a_bounded_wait_answers_at_the_release ~kind take () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~clock ~kind ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let waits = ref 0 in
  let granted, now_granted = Eio.Promise.create () in
  let begun, now_begun = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let (), begun =
        counting_waits (fun () ->
            take
              ~on_wait:(fun () ->
                incr waits;
                ignore (Eio.Promise.try_resolve now_waiting ()))
              lock
              (fun () -> Eio.Promise.resolve now_granted ()))
      in
      Eio.Promise.resolve now_begun begun);
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await waiting);
  Alcotest.(check bool)
    "not granted while held" false
    (Eio.Promise.is_resolved granted);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ());
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await granted);
  Alcotest.(check int) "said it waits once" 1 !waits;
  Alcotest.(check int)
    "began one wait on the clock, its bound's" 1
    (Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await begun))

let take_record ~on_wait lock fn = Eio_fs_unix.with_lock ~on_wait lock fn

let take_record_for ~on_wait lock fn =
  Eio_fs_unix.with_lock ~block:(`For 60.) ~on_wait lock fn

let take_lease_for ~on_wait lock fn =
  Eio_fs_unix.Lease.with_exclusive ~block:(`For 60.) ~on_wait lock (fun _ ->
      fn ())

let take_lease ~on_wait lock fn =
  Eio_fs_unix.Lease.with_exclusive ~on_wait lock (fun _ -> fn ())

(* Another process is granted the record lock: this process's own asks are
   granted whatever it holds. *)
let record_free ~env lock =
  let manager = Eio.Stdenv.process_mgr env in
  let self = Sys.executable_name in
  Alcotest.check status "another process is granted the lock" (`Exited 0)
    ( Eio.Switch.run @@ fun sw ->
      Eio.Process.await
        (Eio.Process.spawn ~sw manager ~executable:self
           [ self; "--try-eio-fs-lock"; Eio.Path.native_exn lock ]) )

(* A byte-range lock is placed from the start of the file and leaves the
   descriptor's offset where it was (POSIX fcntl F_SETLK with SEEK_SET), so a
   lock taken through a descriptor another reader seeks moves none of its
   reads. Another process meets the lock, and the lock is gone once let go;
   [range_locked] answers about another process's lock and never this
   one's. *)
let test_lock_range () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "range" in
  Eio.Path.save ~create:(`Or_truncate 0o600) lock (String.make 64 'r');
  let fd = Unix.openfile (Eio.Path.native_exn lock) [ Unix.O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  ignore (Unix.lseek fd 7 Unix.SEEK_SET : int);
  Eio_fs_unix.lock_range fd `Write ~file_offset:10 ~length:5;
  Alcotest.(check int)
    "the lock leaves the offset" 7
    (Unix.lseek fd 0 Unix.SEEK_CUR);
  Alcotest.(check bool)
    "this process's own lock is not another's" false
    (Eio_fs_unix.range_locked fd ~file_offset:10 ~length:5);
  Alcotest.(check bool)
    "nor does it name a holder" true
    (Eio_fs_unix.range_holder fd ~file_offset:10 ~length:5 = `Free);
  let manager = Eio.Stdenv.process_mgr env in
  let self = Sys.executable_name in
  let try_lock () =
    Eio.Switch.run @@ fun sw ->
    Eio.Process.await
      (Eio.Process.spawn ~sw manager ~executable:self
         [ self; "--try-eio-fs-lock"; Eio.Path.native_exn lock ])
  in
  Alcotest.check status "another process meets the lock" (`Exited 3)
    (try_lock ());
  Eio_fs_unix.lock_range fd `Unlock ~file_offset:10 ~length:5;
  Alcotest.check status "and is granted the file once it is let go" (`Exited 0)
    (try_lock ());
  Eio.Switch.run @@ fun sw ->
  with_held_lock ~sw ~env ~clock ~lock @@ fun () ->
  Alcotest.(check bool)
    "another process's lock is seen" true
    (Eio_fs_unix.range_locked fd ~file_offset:10 ~length:5);
  (match Eio_fs_unix.range_holder fd ~file_offset:10 ~length:5 with
  | `Held (Some pid) ->
      Alcotest.(check bool)
        "the holder is another process" true
        (pid <> Unix.getpid ())
  | `Held None | `Free -> Alcotest.fail "the holding process is not named");
  match Eio_fs_unix.lock_range fd `Read ~file_offset:10 ~length:5 with
  | () -> Alcotest.fail "a read lock was granted under another's write lock"
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> ()

(* A lease belongs to its open file description, so a lease of this process
   excludes another of its own: a blocking one is granted once nothing holds
   it. *)
let lease_free ~env:_ lock =
  Eio_fs_unix.Lease.with_exclusive ~block:`Yes lock (fun _ -> ())

let test_lease_survives_owner_close_until_inherited_descriptor_closes () =
  with_real_dir_env @@ fun _env dir ->
  let lock = dir / "lease" in
  let wait_child pid =
    let rec loop () =
      match Unix.waitpid [] pid with
      | answer -> answer
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    in
    loop ()
  in
  let child =
    Eio_fs_unix.Lease.with_exclusive lock @@ fun lease ->
    let fd =
      Eio_unix.Fd.use_exn "test inherited lease"
        (Eio_fs_unix.Lease.fd lease)
        Fun.id
    in
    Unix.clear_close_on_exec fd;
    (* The child says it runs, holding the inherited descriptor, with one
       byte on [announced]. *)
    let announced, announce = Unix.pipe ~cloexec:true () in
    Fun.protect ~finally:(fun () -> Unix.close announced) @@ fun () ->
    let pid =
      Fun.protect
        ~finally:(fun () ->
          Unix.close announce;
          Unix.set_close_on_exec fd)
        (fun () ->
          Unix.create_process Sys.executable_name
            [| Sys.executable_name; "--hold-eio-fs-lease" |]
            Unix.stdin announce Unix.stderr)
    in
    Eio_unix.await_readable announced;
    let said = Bytes.create 1 in
    if Unix.read announced said 0 1 <> 1 || Bytes.get said 0 <> 'R' then begin
      (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
      ignore (wait_child pid);
      Alcotest.fail "lease child ended before it said it runs"
    end;
    pid
  in
  let reaped = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !reaped then begin
        (try Unix.kill child Sys.sigterm with Unix.Unix_error _ -> ());
        ignore (wait_child child)
      end)
    (fun () ->
      (match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
      | () -> Alcotest.fail "inherited descriptor did not retain the lease"
      | exception Eio_fs_unix.Lock_unavailable _ -> ());
      Unix.kill child Sys.sigterm;
      let _, status = wait_child child in
      reaped := true;
      (match status with
      | Unix.WSIGNALED n when Int.equal n Sys.sigterm -> ()
      | Unix.WEXITED n -> Alcotest.failf "lease child exited %d" n
      | Unix.WSIGNALED n -> Alcotest.failf "lease child took signal %d" n
      | Unix.WSTOPPED n -> Alcotest.failf "lease child stopped by %d" n);
      Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()))

(* An unlocked lease holds nothing through any descriptor that shares its
   open file description: a process that inherited it lives on, and the lease
   is free for the next taker all the same. *)
let test_lease_unlock_frees_inherited_descriptor () =
  with_real_dir_env @@ fun _env dir ->
  let lock = dir / "lease" in
  let wait_child pid =
    let rec loop () =
      match Unix.waitpid [] pid with
      | answer -> answer
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    in
    loop ()
  in
  let child =
    Eio_fs_unix.Lease.with_exclusive lock @@ fun lease ->
    let fd =
      Eio_unix.Fd.use_exn "test unlocked lease"
        (Eio_fs_unix.Lease.fd lease)
        Fun.id
    in
    Unix.clear_close_on_exec fd;
    let announced, announce = Unix.pipe ~cloexec:true () in
    Fun.protect ~finally:(fun () -> Unix.close announced) @@ fun () ->
    let pid =
      Fun.protect
        ~finally:(fun () ->
          Unix.close announce;
          Unix.set_close_on_exec fd)
        (fun () ->
          Unix.create_process Sys.executable_name
            [| Sys.executable_name; "--hold-eio-fs-lease" |]
            Unix.stdin announce Unix.stderr)
    in
    Eio_unix.await_readable announced;
    let said = Bytes.create 1 in
    if Unix.read announced said 0 1 <> 1 || Bytes.get said 0 <> 'R' then begin
      (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
      ignore (wait_child pid);
      Alcotest.fail "lease child ended before it said it runs"
    end;
    (try Eio_fs_unix.Lease.unlock lease
     with e ->
       (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
       ignore (wait_child pid);
       raise e);
    pid
  in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.kill child Sys.sigterm with Unix.Unix_error _ -> ());
      ignore (wait_child child))
    (fun () ->
      (match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
      | () -> ()
      | exception Eio_fs_unix.Lock_unavailable _ ->
          Alcotest.fail "an unlocked lease was still held by its inheritor");
      match Unix.waitpid [ Unix.WNOHANG ] child with
      | 0, _ -> ()
      | _ -> Alcotest.fail "lease child ended before the lease was taken")

(* The holder lets go only when a sibling fiber tells it to, and the sibling
   runs only once the wait has begun: a wait that held the domain would never
   be answered, and the holder would let go at its own bound, exiting 3. *)
let test_lock_wait_does_not_block_eio () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let progressed = ref false and ended = ref (`Exited (-1)) in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock
        ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
        lock
        (fun () -> ()))
    (fun () ->
      Eio.Promise.await waiting;
      progressed := true;
      ended := release ());
  Alcotest.(check bool)
    "another fiber progressed while waiting" true !progressed;
  Alcotest.check status "the holder let go when told" (`Exited 0) !ended

let test_lock_serializes_fibers () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* ---- the exclusion is the file's, not the name's ----

   POSIX record locks do not exclude within a process: a second [lockf] over
   one file, taken through a second descriptor, is granted to the process that
   already holds it, and closing any one of those descriptors drops the lock
   for all of them. Between two fibers of this process the mutex is therefore
   the whole of the exclusion, and it excludes exactly what its key identifies.

   A name does not identify a file. Here one file is reached by two of them --
   a hard link, which is what a snapshot, a stale deployment, or a party who
   writes the rendezvous directory leaves behind -- and canonicalising does not
   merge them, [Unix.realpath] resolving symbolic links and a hard link being
   none of those. Two keys are two mutexes; the operating system grants the
   second acquisition; and both callers run their critical section over one
   file at once, with nothing in either run to say the exclusion was absent. *)
let test_lock_excludes_another_name_for_the_locked_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (dir / "lock") "";
  Unix.link
    (Eio.Path.native_exn (dir / "lock"))
    (Eio.Path.native_exn (dir / "alias"));
  let held, hold = Eio.Promise.create () in
  let checked, check = Eio.Promise.create () in
  let granted = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock (dir / "lock") @@ fun () ->
      Eio.Promise.resolve hold ();
      Eio.Promise.await checked)
    (fun () ->
      Eio.Promise.await held;
      (match
         Eio_fs_unix.with_lock ~block:`No (dir / "alias") (fun () -> ())
       with
      | () -> granted := true
      | exception Eio_fs_unix.Lock_unavailable _ -> ());
      Eio.Promise.resolve check ());
  Alcotest.(check bool)
    "a second name for the held file was not granted the lock" false !granted

(* The key is resolved unconfined as well. [Unix.realpath] walks the name from
   [/], through the directories a party other than the caller writes, where the
   open that follows it resolves every component beneath the capability the
   path carries. Where the two disagree, realpath answers first and answers for
   the lock: a rendezvous under a parent link that points nowhere ends the
   acquisition in [Unix.Unix_error (ENOENT, "realpath", ...)] before the lock's
   own resolution has said anything -- a failure of the key, and none of the
   answers this interface gives, which are Eio's own for a path it declines and
   [Not_found] for a rendezvous that is not there. *)
let test_lock_answers_a_parent_link_that_leads_nowhere () =
  with_real_dir @@ fun dir ->
  Eio.Path.symlink ~link_to:"gone" (dir / "parent");
  match Eio_fs_unix.with_lock (dir / "parent" / "lock") (fun () -> ()) with
  | () ->
      Alcotest.fail "a rendezvous under a link that points nowhere was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let test_lock_updates_contents_in_place () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock ~contents:"first\n" lock (fun () -> ());
  let before = Eio.Path.stat ~follow:false lock in
  Eio_fs_unix.with_lock ~contents:"second\n" lock (fun () ->
      let while_locked = Eio.Path.stat ~follow:false lock in
      Alcotest.(check int64) "same locked inode" before.ino while_locked.ino;
      Alcotest.(check string)
        "current diagnostic" "second\n" (Eio.Path.load lock));
  let after = Eio.Path.stat ~follow:false lock in
  Alcotest.(check int64) "inode survives acquisition" before.ino after.ino

let test_lock_non_blocking_unavailable () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  (match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
  | () -> Alcotest.fail "non-blocking lock unexpectedly succeeded"
  | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- a refusal names who holds the lock ----

   A path says which resource is busy, not who has it, and the reader of a
   refusal is left to find the holder with [lsof]. The kernel knows the
   process holding a record lock ([fcntl F_GETLK], POSIX.1-2017 fcntl, "the
   l_pid ... of the lock that would prevent this lock"), so the printed
   refusal names it. A holder's pid differs from this process's, so the case
   reds on a refusal that names nobody and on one that names the asker. *)

(* [printed] names process [pid] as a whole number, not a prefix of one. *)
let names_process printed pid =
  let needle = Fmt.str "process %d" pid in
  let n = String.length needle and len = String.length printed in
  let rec from i =
    i + n <= len
    && (String.sub printed i n = needle
        && (i + n = len
           || not (match printed.[i + n] with '0' .. '9' -> true | _ -> false))
       || from (i + 1))
  in
  from 0

let test_lock_refusal_names_the_holding_process () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  let holder = Eio.Process.pid child in
  (match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
  | () -> Alcotest.fail "a lock another process holds was granted"
  | exception (Eio_fs_unix.Lock_unavailable refused as refusal) ->
      let printed = Printexc.to_string refusal in
      if not (names_process printed holder) then
        Alcotest.failf "the refusal %S does not name the holder, process %d"
          printed holder;
      Alcotest.(check (option int))
        "the holder the refusal carries" (Some holder) refused.holder);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* The mutex half of the exclusion is held by a fiber of this process, so the
   holder a refusal carries there is this process, which it prints as another
   fiber of it, never as a pid that reads as a foreign holder, and the record
   it carries is what that fiber wrote through [contents]. *)
let test_lock_refusal_names_this_process_for_a_fiber () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let record = "the holding fiber's record\n" in
  Eio_fs_unix.with_lock ~contents:record lock @@ fun () ->
  match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
  | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
  | exception (Eio_fs_unix.Lock_unavailable refused as refusal) ->
      let printed = Printexc.to_string refusal in
      let self = Unix.getpid () in
      if names_process printed self then
        Alcotest.failf "the refusal %S names this process, %d, as a holder"
          printed self;
      Alcotest.(check (option int))
        "the holder the refusal carries" (Some self) refused.holder;
      Alcotest.(check string) "the holder's record" record refused.record

(* ---- asking from the holding process keeps the hold ----

   POSIX drops every record lock a process holds on a file when that process
   closes any descriptor for it (POSIX.1-2017 fcntl, "all locks associated with
   a file for a given process shall be removed when a file descriptor for that
   file is closed by that process"). A second acquisition from the holding
   process opens the rendezvous to find its key, so the descriptor it closes on
   the way out of a refusal is the holder's release. Only another process can
   tell: in this one the fiber mutex still answers. *)

let another_process_is_granted ~env lock =
  let self = Sys.executable_name in
  Eio.Switch.run @@ fun sw ->
  let child =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      ~executable:self
      [ self; "--try-eio-fs-lock"; Eio.Path.native_exn lock ]
  in
  match Eio.Process.await child with
  | `Exited 0 -> true
  | `Exited 3 -> false
  | `Exited n -> Alcotest.failf "lock asker exited %d" n
  | `Signaled n -> Alcotest.failf "lock asker took signal %d" n

let asked_from_the_holder ~block () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock lock @@ fun () ->
  (match Eio_fs_unix.with_lock ~block lock (fun () -> ()) with
  | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
  | exception Eio_fs_unix.Lock_unavailable _ -> ());
  if another_process_is_granted ~env lock then
    Alcotest.fail
      "another process was granted the lock after a refusal in the holder"

let test_lock_refusal_in_the_holder_keeps_its_hold () =
  asked_from_the_holder ~block:`No ()

let test_lock_bounded_wait_in_the_holder_keeps_its_hold () =
  asked_from_the_holder ~block:(`For 0.05) ()

(* A descriptor handed to the holder is closed when the hold ends, not kept:
   each refused ask from the holder would otherwise cost the process one
   descriptor for as long as it runs. *)
let open_descriptors () = Array.length (Sys.readdir "/dev/fd")

let test_lock_asks_from_the_holder_leave_no_descriptor () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let before = open_descriptors () in
  ( Eio_fs_unix.with_lock lock @@ fun () ->
    for _ = 1 to 3 do
      match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
      | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
      | exception Eio_fs_unix.Lock_unavailable _ -> ()
    done );
  Alcotest.(check int)
    "descriptors open once the hold ends" before (open_descriptors ())

(* The descriptors this process holds open on [lock]'s file, each of [/dev/fd]
   matched by inode: macOS's [/dev/fd] reports its own device for each. *)
let descriptors_on lock =
  let ino = (Unix.stat (Eio.Path.native_exn lock)).st_ino in
  Sys.readdir "/dev/fd"
  |> Array.fold_left
       (fun n fd ->
         match Unix.stat (Filename.concat "/dev/fd" fd) with
         | { st_kind = Unix.S_REG; st_ino; _ } when st_ino = ino -> n + 1
         | _ | (exception Unix.Unix_error _) -> n)
       0

(* A holder that holds for the life of its process, as a server holds its
   serve lock, is asked again and again from that process: every ask leaves
   the process with the holder's one descriptor on the file, never one more,
   so the asks of a long life cannot reach the descriptor limit. *)
let test_lock_asks_during_a_hold_keep_one_descriptor () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock ~contents:"held for life\n" lock @@ fun () ->
  for round = 1 to 5 do
    (match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
    | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
    | exception Eio_fs_unix.Lock_unavailable _ -> ());
    (match Eio_fs_unix.with_lock ~block:(`For 0.01) lock (fun () -> ()) with
    | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
    | exception Eio_fs_unix.Lock_unavailable _ -> ());
    (match Eio_fs_unix.probe lock with
    | `Free _ ->
        Alcotest.fail "a lock a fiber of this process holds probed free"
    | `Held refused ->
        Alcotest.(check string)
          "the holder's record" "held for life\n" refused.record);
    Alcotest.(check int)
      (Fmt.str "descriptors on the lock file after %d rounds of asks" round)
      1 (descriptors_on lock)
  done

(* An ask from the holding process reads the holder's record through the
   holder's descriptor, and the holder may let go of the lock, closing that
   descriptor, at any point of the ask: every ask still answers, the lock held
   or free, and never reads a closed descriptor. The holder releases with a
   last record, whose write suspends it while it still holds, and the ask
   starts after 0 to 19 yields of its own, so the close lands at each
   suspension point of the ask in turn. *)
let test_lock_asks_while_the_holder_lets_go () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let ask name f =
    for yields = 0 to 19 do
      Eio.Switch.run @@ fun sw ->
      let held =
        Eio_fs_unix.Held.take ~sw ~contents:"held for the ask\n" lock
      in
      Eio.Fiber.both
        (fun () -> Eio_fs_unix.Held.release ~contents:"let go\n" held)
        (fun () ->
          for _ = 1 to yields do
            Eio.Fiber.yield ()
          done;
          match f () with
          | () -> ()
          | exception Invalid_argument message ->
              Alcotest.failf "%s after %d yields: %s" name yields message)
    done
  in
  ask "a probe" (fun () -> ignore (Eio_fs_unix.probe lock));
  ask "a refused ask" (fun () ->
      try Eio_fs_unix.with_lock ~block:`No lock ignore
      with Eio_fs_unix.Lock_unavailable _ -> ())

(* ---- a probe says who holds a lock without taking it ----

   Asking by acquiring answers "free" only by holding the lock for a moment,
   and a real acquirer in another process that asks in that moment is refused
   by a caller that wanted nothing. A probe asks the kernel ([fcntl F_GETLK])
   and the fiber half, and takes nothing. *)

let check_free_probe what lock ~record =
  match Eio_fs_unix.probe lock with
  | `Free left ->
      Alcotest.(check string) (what ^ ": the record left") record left
  | `Held (r : Eio_fs_unix.refusal) ->
      Alcotest.failf "%s: a free lock probed as held by %a" what
        Fmt.(option ~none:(any "nobody") int)
        r.holder

let test_probe_names_the_holding_process () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock ~contents:"the last holder\n" lock (fun () -> ());
  check_free_probe "before the child" lock ~record:"the last holder\n";
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  (match Eio_fs_unix.probe lock with
  | `Free _ -> Alcotest.fail "a lock another process holds probed as free"
  | `Held refused ->
      Alcotest.(check (option int))
        "the holder the probe names"
        (Some (Eio.Process.pid child))
        refused.holder);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ());
  check_free_probe "after the child" lock ~record:"the last holder\n"

(* A probe from the holding process names it, and like any ask from there it
   keeps the hold: only another process can tell. *)
let test_probe_from_the_holder_keeps_its_hold () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  let record = "the holding fiber's record\n" in
  Eio_fs_unix.with_lock ~contents:record lock @@ fun () ->
  (match Eio_fs_unix.probe lock with
  | `Free _ -> Alcotest.fail "a lock a fiber of this process holds probed free"
  | `Held refused ->
      Alcotest.(check (option int))
        "the holder the probe names"
        (Some (Unix.getpid ()))
        refused.holder;
      Alcotest.(check string) "the holder's record" record refused.record);
  if another_process_is_granted ~env lock then
    Alcotest.fail "another process was granted the lock after a probe"

(* A probe takes nothing, so it needs no more than to read the lock file: a
   lock file this user may read and not write is still answered. *)
let test_probe_answers_a_read_only_lock_file () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Eio.Path.save ~create:(`Exclusive 0o400) lock "written under other rights\n";
  check_free_probe "the probe" lock ~record:"written under other rights\n"

(* A lock file can go while a probe reads it: a build slot's ticket is
   removed by its holder once the slot is its own, while a watcher probes
   every ticket. The probe opens the file and then weighs the name against
   what it opened; a name that holds nothing by then is a lock that is
   absent, as one absent when the probe began, and never a name that is
   not a lock file. The file goes at the first stat after the open, the
   moment between the two, through the file system the probe reads. *)
let test_probe_of_a_lock_file_that_goes () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "ticket") "a waiting check\n";
  let native = Eio.Path.native_exn (dir / "ticket") in
  Eio.Path.with_open_dir dir @@ fun base ->
  let opened = ref false and gone = ref false in
  let schedule =
    Chaos.Schedule.observing
      (fun (operation : Chaos.Op.t) ->
        match operation.kind with
        | `Open_read | `Open_write -> opened := true
        | `Stat when !opened && not !gone ->
            Unix.unlink native;
            gone := true
        | _ -> ())
      Chaos.Schedule.empty
  in
  let wrapped = Chaos.wrap schedule base in
  (match Eio_fs_unix.probe (wrapped / "ticket") with
  | `Free _ | `Held _ -> Alcotest.fail "a lock file that went was answered"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
      Alcotest.fail "a lock file that went was refused as no lock file");
  Alcotest.(check bool) "the file went between the open and the stat" true !gone

(* A lock file's name can be given to a new file while a probe reads the
   old one: a build slot numbers a new ticket after the highest left, so
   the first ticket's name comes back once every ticket went. The probe
   answers for the file the name holds when it answers, the new one, and
   never refuses the name as not a lock file. The old file goes and the new
   one takes its name at the first stat after the open. *)
let test_probe_of_a_lock_file_replaced () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "ticket") "the old ticket\n";
  let native = Eio.Path.native_exn (dir / "ticket") in
  Eio.Path.with_open_dir dir @@ fun base ->
  let opened = ref false and replaced = ref false in
  let schedule =
    Chaos.Schedule.observing
      (fun (operation : Chaos.Op.t) ->
        match operation.kind with
        | `Open_read | `Open_write -> opened := true
        | `Stat when !opened && not !replaced ->
            Unix.unlink native;
            Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_excl ]
              0o600 native (fun oc -> output_string oc "the new ticket\n");
            replaced := true
        | _ -> ())
      Chaos.Schedule.empty
  in
  let wrapped = Chaos.wrap schedule base in
  (match Eio_fs_unix.probe (wrapped / "ticket") with
  | `Free record ->
      Alcotest.(check string) "the new file's record" "the new ticket\n" record
  | `Held _ -> Alcotest.fail "a free lock file was answered as held"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
      Alcotest.fail "a lock file replaced was refused as no lock file");
  Alcotest.(check bool)
    "the file was replaced between the open and the stat" true !replaced

(* A held lock not released lasts the rest of its switch: taken when [take]
   returns, still held while the switch runs, and let go when it finishes. *)
let test_hold_lasts_as_long_as_its_switch () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let record = "held for the switch\n" in
  Eio.Switch.run (fun sw ->
      ignore
        (Eio_fs_unix.Held.take ~sw ~contents:record lock : Eio_fs_unix.Held.t);
      match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
      | () -> Alcotest.fail "a lock held for the switch was granted"
      | exception Eio_fs_unix.Lock_unavailable refused ->
          Alcotest.(check (option int))
            "held by this process"
            (Some (Unix.getpid ()))
            refused.holder;
          Alcotest.(check string) "the hold's record" record refused.record);
  let granted = ref false in
  Eio_fs_unix.with_lock ~block:`No lock (fun () -> granted := true);
  Alcotest.(check bool) "the lock is free once the switch ends" true !granted

(* A hold refused by another process raises to its caller, naming the holder,
   and leaves nothing held. *)
let test_hold_refusal_names_the_holding_process () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  ( Eio.Switch.run @@ fun hold_sw ->
    match Eio_fs_unix.Held.take ~sw:hold_sw ~block:`No lock with
    | (_ : Eio_fs_unix.Held.t) ->
        Alcotest.fail "a hold was granted over another process's lock"
    | exception Eio_fs_unix.Lock_unavailable refused ->
        Alcotest.(check (option int))
          "the holder the refusal carries"
          (Some (Eio.Process.pid child))
          refused.holder );
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- a held lock is restated and let go on demand ----

   A process that claims a file for as long as it runs states what it is
   doing in the lock file as it goes, and lets the claim go before it exits,
   leaving a last record for the reader who comes after it. Both writes go
   through the held descriptor: opening the file again to write it would be
   a close on the held file, and POSIX would release the lock with it. *)

let held_record what lock ~record =
  match Eio_fs_unix.probe lock with
  | `Free _ -> Alcotest.failf "%s: the lock probed as free" what
  | `Held (r : Eio_fs_unix.refusal) ->
      Alcotest.(check (option int))
        (what ^ ": held by this process")
        (Some (Unix.getpid ()))
        r.holder;
      Alcotest.(check string) (what ^ ": the record") record r.record

let test_held_restates_its_record () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held = Eio_fs_unix.Held.take ~sw ~contents:"booting\n" lock in
  held_record "as taken" lock ~record:"booting\n";
  Eio_fs_unix.Held.restate held "running\n";
  held_record "restated" lock ~record:"running\n";
  if another_process_is_granted ~env lock then
    Alcotest.fail "another process was granted a lock that was restated"

let test_held_release_lets_the_lock_go_and_leaves_a_record () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held = Eio_fs_unix.Held.take ~sw ~contents:"running\n" lock in
  Eio_fs_unix.Held.release ~contents:"ended\n" held;
  (match Eio_fs_unix.probe lock with
  | `Free record -> Alcotest.(check string) "the record left" "ended\n" record
  | `Held _ -> Alcotest.fail "a released lock probed as held");
  if not (another_process_is_granted ~env lock) then
    Alcotest.fail "another process was refused a released lock";
  Eio_fs_unix.Held.release held;
  match Eio_fs_unix.Held.restate held "again\n" with
  | () -> Alcotest.fail "a released lock was restated"
  | exception Invalid_argument _ -> ()

let test_held_ends_with_its_switch () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  let held =
    Eio.Switch.run @@ fun sw -> Eio_fs_unix.Held.take ~sw ~contents:"" lock
  in
  if not (another_process_is_granted ~env lock) then
    Alcotest.fail "another process was refused a lock whose switch ended";
  Eio_fs_unix.Held.release held

(* A holder that lets go when its switch ends does so from a release hook of
   that switch, where it knows its last record: the lock is still held there,
   so the record goes through the held descriptor and stays. *)
let test_held_released_from_its_switch_hook_leaves_a_record () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  ( Eio.Switch.run @@ fun sw ->
    let held = Eio_fs_unix.Held.take ~sw ~contents:"running\n" lock in
    Eio.Switch.on_release sw (fun () ->
        Eio_fs_unix.Held.release ~contents:"ended\n" held) );
  match Eio_fs_unix.probe lock with
  | `Free record -> Alcotest.(check string) "the record left" "ended\n" record
  | `Held _ -> Alcotest.fail "a lock whose switch ended probed as held"

(* A close of the held file anywhere in this process -- a reader that opened
   it by name and let it go -- drops the record lock under the holder, and
   nothing tells it. A restate takes it again on the held descriptor before it
   writes, so the claim a holder goes on stating is one it still has. *)
let test_held_restate_takes_back_a_lock_a_stray_close_dropped () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held = Eio_fs_unix.Held.take ~sw ~contents:"running\n" lock in
  Unix.close (Unix.openfile (Eio.Path.native_exn lock) [ Unix.O_RDONLY ] 0);
  Eio_fs_unix.Held.restate held "still running\n";
  if another_process_is_granted ~env lock then
    Alcotest.fail "another process was granted a lock its holder restated"

(* ---- waiting for this process, answering about every other one ----

   The two halves of the exclusion are two kinds of contention, and a caller
   can want a different answer to each. The fibers of one program are its own
   cooperating writers: one that finds another inside wants its turn, because
   refusing it fails work the program itself asked for and could have waited
   out. Another process is an independent actor, and how long it holds the
   rendezvous is not this program's to bound: a command-line tool would rather
   tell its operator which resource is busy than wait for a holder that may be
   wedged, with nothing printed and no deadline.

   [`Fibers] is that pair: entering queues behind a fiber of this process, and
   a hold taken by any other process is answered rather than waited out. *)
let test_lock_fibers_mode_queues_behind_a_fiber () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false and overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock ~block:`Fibers lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock ~block:`Fibers lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the second fiber took its turn rather than being refused" true !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* Refusing rather than waiting is what the answer proves: a caller that waited
   out the holder would be granted the lock once the child exits and return
   unit, so [Lock_unavailable] can only come from an acquisition that did not
   wait. *)
let test_lock_fibers_mode_refuses_another_process () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  (match Eio_fs_unix.with_lock ~block:`Fibers lock (fun () -> ()) with
  | () ->
      Alcotest.fail "another process's hold was waited out rather than answered"
  | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- a bound is a wait a caller can plan against ----

   [`Yes] and [`No] are the two answers a second writer of a shared store can
   be given, and neither suits several checkouts publishing into one cache.
   [`For seconds] waits and answers: a caller that knows the bound can name it
   before it waits and act when the wait is over. Nothing orders the processes
   contending for a record lock, so the answer comes no sooner than the bound
   and does not wait out a holder that is still there. *)
let test_lock_bounded_wait_answers_at_the_bound () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  let mono = Eio_mock.Clock.Mono.make () in
  let waiting, now_waiting = Eio.Promise.create () in
  let answer, answered = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve answered
        (on_mono
           (mono :> Eio.Time.Mono.ty Eio.Resource.t)
           (fun () ->
             match
               Eio_fs_unix.with_lock ~block:(`For 0.2)
                 ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
                 lock
                 (fun () -> `Granted)
             with
             | answer -> answer
             | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
                 `Refused path)));
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await waiting);
  Alcotest.(check bool)
    "no answer before the bound" false
    (Eio.Promise.is_resolved answer);
  Eio_mock.Clock.Mono.advance mono;
  (match
     Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await answer)
   with
  | `Granted -> Alcotest.fail "a lock held for the whole bound was granted"
  | `Refused path ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Alcotest.check status "the holder still held it, and let go when told"
    (`Exited 0) (release ())

(* The fibers of this process are the other holder a bound has to cover. Here
   the record lock is free and a fiber of this program is inside, so what the
   bound answers about is the mutex alone: without it, a caller told "up to a
   fifth of a second" would wait a fifth for the fiber and a fifth more for the
   record lock, which is not the number it was told. The bound is on a clock
   the case moves once the waiter says it waits, and the fiber lets go only
   after the answer, so the answer can come from nothing but the bound. *)
let test_lock_bounded_wait_answers_a_fiber_that_holds_it () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  let mono = Eio_mock.Clock.Mono.make () in
  let entered, set_entered = Eio.Promise.create () in
  let release, let_go = Eio.Promise.create () in
  let waiting, now_waiting = Eio.Promise.create () in
  let answer, answered = Eio.Promise.create () in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      Eio.Promise.resolve set_entered ();
      Eio.Promise.await release);
  Eio.Promise.await entered;
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve answered
        (on_mono
           (mono :> Eio.Time.Mono.ty Eio.Resource.t)
           (fun () ->
             match
               Eio_fs_unix.with_lock ~block:(`For 0.2)
                 ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
                 lock
                 (fun () -> `Granted)
             with
             | answer -> answer
             | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
                 `Refused path)));
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await waiting);
  Alcotest.(check bool)
    "no answer before the bound" false
    (Eio.Promise.is_resolved answer);
  Eio_mock.Clock.Mono.advance mono;
  (match
     Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await answer)
   with
  | `Granted ->
      Alcotest.fail "a lock a fiber held for the whole bound was granted"
  | `Refused path ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Eio.Promise.resolve let_go ()

(* A wait with no bound behind a fiber of this process says so too, once, and
   is granted when the fiber lets go. *)
let test_lock_wait_behind_a_fiber_says_so () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  let entered, set_entered = Eio.Promise.create () in
  let release, let_go = Eio.Promise.create () in
  let waiting, now_waiting = Eio.Promise.create () in
  let granted, now_granted = Eio.Promise.create () in
  let waits = ref 0 in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      Eio.Promise.resolve set_entered ();
      Eio.Promise.await release);
  Eio.Promise.await entered;
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.with_lock ~block:`Yes
        ~on_wait:(fun () ->
          incr waits;
          ignore (Eio.Promise.try_resolve now_waiting ()))
        lock
      @@ fun () -> Eio.Promise.resolve now_granted ());
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await waiting);
  Alcotest.(check bool)
    "not granted while the fiber holds it" false
    (Eio.Promise.is_resolved granted);
  Eio.Promise.resolve let_go ();
  Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Promise.await granted);
  Alcotest.(check int) "said it waits once" 1 !waits

(* A holder that dies does not cost the waiter its bound. The record lock is
   the kernel's, held on an open descriptor, so it goes when the last
   descriptor to the file closes -- which a killed process does as surely as an
   exiting one. A waiter that read the rendezvous rather than asking the kernel
   would sit out the whole bound behind a process that is gone. Here the bound
   is on a clock that never moves and the holder never lets go on its own, so
   the kill is all that can end the wait. *)
let test_lock_bounded_wait_does_not_sit_out_a_killed_holder () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, _ = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let granted = ref false in
  Eio.Time.with_timeout_exn clock 5.0 (fun () ->
      Eio.Fiber.both
        (fun () ->
          Eio.Promise.await waiting;
          Eio.Process.signal child Sys.sigkill)
        (fun () ->
          let (), _ =
            counting_waits (fun () ->
                Eio_fs_unix.with_lock ~block:(`For 30.)
                  ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
                  lock
                  (fun () -> granted := true))
          in
          ()));
  Alcotest.(check bool) "the lock was taken once its holder died" true !granted;
  match Eio.Process.await child with
  | `Signaled n when Int.equal n Sys.sigkill -> ()
  | `Exited n ->
      Alcotest.failf "the holder exited %d rather than being killed" n
  | `Signaled n -> Alcotest.failf "the holder took signal %d" n

let test_lock_existing_does_not_create () =
  with_real_dir @@ fun dir ->
  let lock = dir / "absent" in
  (match Eio_fs_unix.with_lock ~create:`Never lock (fun () -> ()) with
  | () -> Alcotest.fail "an absent rendezvous was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ());
  Alcotest.(check bool)
    "probing does not create the rendezvous" false (Eio.Path.is_file lock)

(* ---- the lock path is the lock's own file ----

   A rendezvous lives in a directory another party writes, which is exactly
   where an entry gets planted under the name a locker is about to use. If the
   lock follows what it finds there, the exclusive lock lands on the entry's
   target: two processes then hold two locks over one resource, both read the
   same state and both publish, with nothing in either run to say the exclusion
   was absent. So the lock path must be its own regular file, and anything else
   under that name is refused.

   The refusal is Eio's own vocabulary for a path it declines to resolve --
   [Eio.Io (Eio.Fs.E (Permission_denied _))], as Eio raises for a name outside a
   sandbox -- and it names the lock path, because an operator reading the failure
   needs to know which entry is wrong. *)

let check_refusal what path fn =
  let native = Eio.Path.native_exn path in
  match fn () with
  | () -> Alcotest.failf "%s at the lock path was locked" what
  | exception (Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) as e) ->
      Alcotest.(check bool)
        (what ^ ": the refusal names the lock path")
        true
        (mentions ~needle:native (Printexc.to_string e))

(* A link to a sibling regular file: the shape an adversary plants, and the
   shape a stale deployment leaves behind. The sibling must come back neither
   locked nor written. *)
let test_lock_refuses_a_symlink_at_the_lock_path () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (dir / "target") "target\n";
  Eio.Path.symlink ~link_to:"target" (dir / "lock");
  check_refusal "a symbolic link" (dir / "lock") (fun () ->
      Eio_fs_unix.with_lock ~contents:"holder\n" (dir / "lock") (fun () -> ()));
  Alcotest.(check string)
    "the link's target was not written through the link" "target\n"
    (Eio.Path.load (dir / "target"));
  (* The target's own path still locks, so the refusal took nothing with it. *)
  Eio_fs_unix.with_lock (dir / "target") (fun () -> ())

(* A dangling link is the sharper case: creating the rendezvous through it
   creates a file wherever the link points, so the refusal has to come before
   the open, not after it. *)
let test_lock_refuses_a_dangling_symlink_without_creating_its_target () =
  with_real_dir @@ fun dir ->
  Eio.Path.symlink ~link_to:"elsewhere" (dir / "lock");
  check_refusal "a dangling symbolic link" (dir / "lock") (fun () ->
      Eio_fs_unix.with_lock (dir / "lock") (fun () -> ()));
  Alcotest.(check bool)
    "the link's target was not created" false
    (Eio.Path.is_file (dir / "elsewhere"))

(* ---- the window between the name check and the open ----

   The name is weighed before the file is opened and the descriptor after the
   lock is held, and in between the open resolves that same name a second time.
   An entry planted in that window is what the second resolution follows. It is
   a real window rather than a theoretical one: eio runs [stat] and [openat] on
   a system thread, so the fiber suspends at each and another fiber of the same
   domain -- let alone another process sharing the rendezvous directory -- runs
   between them.

   A test cannot hook between two calls it does not make, so the directory
   itself does the planting: [Plant_after_stat] answers a stat about the base
   directory as it stands, then takes the name it was asked about with a
   dangling symbolic link before returning. Everything else, including the
   descriptor an open returns, is the base directory's own. *)
module Plant_after_stat = struct
  type t = {
    base : Eio.Fs.dir_ty Eio.Resource.t;
    link_to : string;
    planted : bool ref;
  }

  let at t path = (t.base, path)

  let plant t path =
    if not !(t.planted) then begin
      t.planted := true;
      Eio.Path.symlink ~link_to:t.link_to (at t path)
    end

  let stat t ~follow path =
    Fun.protect
      ~finally:(fun () -> plant t path)
      (fun () -> Eio.Path.stat ~follow (at t path))

  let open_in t ~sw ~follow path = Eio.Path.open_in ~sw ~follow (at t path)

  let open_out t ~sw ~follow ~append ~create path =
    Eio.Path.open_out ~sw ~follow ~append ~create (at t path)

  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  (* TODO: drop the attribute when Eio 1.7, whose [Pi.DIR] has [sync_dir], is
     released, and call [Eio.Path.sync_dir]. *)
  let sync_dir t path = Eio_fs_sync.sync_dir (at t path) [@@warning "-32"]
  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "plant-after-stat"

  let open_subtree _ ~sw:_ _ =
    failwith "Plant_after_stat: the lock opens no subtree"

  include Eio_utils.Posix_path
end

let plant_after_stat ~link_to ((base, _) : Eio.Fs.dir_ty Eio.Path.t) =
  let t = { Plant_after_stat.base; link_to; planted = ref false } in
  (Eio.Resource.T
     ( t,
       Eio.Resource.handler
         [ Eio.Resource.H (Eio.Fs.Pi.Dir, (module Plant_after_stat)) ] )
    : Eio.Fs.dir_ty Eio.Resource.t)

(* A link planted in that window is followed by the open, and a creating open
   that follows one creates the file at the far end: an empty, owner-writable
   file wherever the party that planted the link chose, left behind after the
   lock itself was refused. Refusing is only half the guarantee; the other half
   is that the attempt created nothing. *)
let test_lock_creates_nothing_through_a_link_planted_after_the_check () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let racing =
    (plant_after_stat ~link_to:"victim" dir, Eio.Path.native_exn lock)
  in
  check_refusal "a link planted after the name check" lock (fun () ->
      Eio_fs_unix.with_lock racing (fun () -> ()));
  Alcotest.(check bool)
    "the planted link took the name" true
    ((Eio.Path.stat ~follow:false lock).kind = `Symbolic_link);
  Alcotest.(check bool)
    "the link's target was not created" false
    (Eio.Path.is_file (dir / "victim"))

(* Neither a directory nor a FIFO carries the record lock the interface
   promises, so both are refused in the same vocabulary rather than surfacing
   as whichever [open] error the platform happens to pick. *)
let test_lock_refuses_a_directory_or_a_fifo () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "as-a-directory");
  Unix.mkfifo (Eio.Path.native_exn (dir / "as-a-fifo")) 0o600;
  check_refusal "a directory" (dir / "as-a-directory") (fun () ->
      Eio_fs_unix.with_lock (dir / "as-a-directory") (fun () -> ()));
  check_refusal "a FIFO" (dir / "as-a-fifo") (fun () ->
      Eio_fs_unix.with_lock (dir / "as-a-fifo") (fun () -> ()))

(* An absent rendezvous is still created, as its own owner-only regular file:
   refusing what is not a regular file must not refuse the ordinary first
   acquisition. *)
let test_lock_creates_a_missing_rendezvous_as_a_regular_file () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Alcotest.(check bool)
    "no rendezvous before the first acquisition" false (Eio.Path.is_file lock);
  Eio_fs_unix.with_lock lock (fun () ->
      let held = Eio.Path.stat ~follow:false lock in
      Alcotest.(check bool)
        "the rendezvous is a regular file while held" true
        (held.kind = `Regular_file);
      Alcotest.(check int) "created owner-only" 0o600 held.perm);
  let after = Eio.Path.stat ~follow:false lock in
  Alcotest.(check bool)
    "the rendezvous survives as a regular file" true
    (after.kind = `Regular_file)

(* ---- a rendezvous that carries no descriptor ----

   The record lock is taken on a Unix descriptor, and a backend that is not the
   host filesystem hands out files carrying none. Two such backends exist here
   and one answer cannot serve both.

   This package's in-memory filesystem holds its files in this process's heap.
   No other process can open them: there is no name to open them by --
   [Eio.Path.native] answers [None] over it -- and no descriptor to inherit. So
   no opener outside this process exists to be excluded, and the in-process
   mutex is not a weaker lock there, it is the whole exclusion the backend
   admits. A unikernel filesystem is the same case.

   A wrapper over a real host directory is the other case and is nothing like
   it: the file it hands back is an ordinary host file, which any process opens
   by name and locks. Running under the mutex alone there puts two processes
   inside one critical section with nothing in either run to say the exclusion
   was absent -- the failure this interface exists to prevent. So a wrapper
   answers for the base it was given: over a host directory it passes that
   directory's own descriptors through, and the record lock taken under it is
   the host's; over a backend that admits no outside opener it forwards what
   the backend declares, and the mutex is the whole exclusion there as it is
   without the wrapper.

   So the missing descriptor is not the question. Whether the backend admits an
   opener outside this process is, and only the backend knows: one that admits
   none says so, and its rendezvous is held under the mutex alone. Every other
   rendezvous with no descriptor is refused -- loudly, as the barrier next door
   refuses a backend that registers none -- rather than locked in name only,
   silence being what turns a missing exclusion into a bug nobody sees. *)

let test_lock_over_a_process_local_backend_serializes_fibers () =
  with_in_memory_fs @@ fun fs ->
  let lock = fs / "lock" in
  Alcotest.(check bool)
    "no name outside this process reaches the rendezvous" true
    (Option.is_none (Eio.Path.native lock));
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the critical section ran over a backend with no descriptors" true
    !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped;
  Alcotest.(check bool)
    "the rendezvous is the backend's own regular file" true
    ((Eio.Path.stat ~follow:false lock).kind = `Regular_file)

(* A bound is a wait's: an acquisition nobody holds against waits for nothing,
   so it answers without the system clock the bound would run on. The mock
   backend handles no system clock, so a bound started anyway raises there. *)
let test_lock_bounded_and_uncontended_needs_no_clock () =
  with_in_memory_fs @@ fun fs ->
  let ran = ref 0 in
  Eio_fs_unix.with_lock ~block:(`For 30.) (fs / "lock") (fun () -> incr ran);
  Eio_fs_unix.with_lock ~block:(`For 30.) (fs / "lock") (fun () -> incr ran);
  Alcotest.(check int) "both critical sections ran" 2 !ran

(* Holding under the mutex alone is not a licence to skip what the name is
   weighed for: the in-memory backend has links too, and a link at the lock
   name is not the lock's own file whatever the exclusion behind it is. *)
let test_lock_over_a_process_local_backend_refuses_a_symlink () =
  with_in_memory_fs @@ fun fs ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (fs / "target") "target\n";
  Eio.Path.symlink ~link_to:"target" (fs / "lock");
  (match
     Eio_fs_unix.with_lock ~contents:"holder\n" (fs / "lock") (fun () -> ())
   with
  | () -> Alcotest.fail "a symbolic link at the lock path was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
  Alcotest.(check string)
    "the link's target was not written through the link" "target\n"
    (Eio.Path.load (fs / "target"))

(* A wrapper over that backend is the same case, and it is the case a store
   under test is in: [Eio_fs.Chaos.Private] perturbs or crash-models the tree beneath
   it, and over the in-memory backend the tree it perturbs is still this
   process's heap. Nothing the wrapper adds gives those files a name to open
   them by or a descriptor to inherit, so the exclusion they admit is the one
   the base declares, and the wrapper's business is to say so rather than to
   answer for a base it did not look at. A wrapper that declares nothing turns
   a rendezvous this interface holds under the mutex into one it refuses, which
   is the whole of a fault-injected store's locking gone. *)
let test_lock_over_a_wrapped_process_local_backend_serializes_fibers () =
  with_in_memory_fs @@ fun fs ->
  let wrapped = Chaos.wrap Chaos.Schedule.empty fs in
  let lock = wrapped / "lock" in
  Alcotest.(check bool)
    "the wrapper forwards what its base declares about outside openers" true
    (Eio_fs_sync.is_process_local (fst lock));
  Alcotest.(check bool)
    "no name outside this process reaches the rendezvous" true
    (Option.is_none (Eio.Path.native lock));
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the critical section ran through the wrapper" true !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* ---- the lock is granted in the order callers entered ----

   An acquisition suspends before it takes anything: the rendezvous is opened
   first, the key the fiber mutex is found by being the opened file's own
   device and inode, and a backend runs that open off the calling fiber. So the
   order two fibers reach the mutex in is the order their opens came back in,
   which is not the order they entered, and a caller cannot repair it from
   outside: between "I entered" and "I hold" it has no point to stand. What
   that costs is a queue that reorders its own writers -- a spool whose
   sequence numbers are issued inside the lock publishes them out of the order
   its callers were served in.

   The suspension belongs to the backend, so a test that wants an exact
   interleaving brings its own: [Slow_open] delegates every operation to the
   in-memory backend and yields before an open, as many scheduler turns as the
   entering fiber asked for. Under [Eio_mock.Backend] the run queue is
   deterministic, so the fiber that enters first can be made to reach its own
   open last -- the reversal a real filesystem produces by itself, where the
   open is a worker thread's round trip and no order is promised at all. *)
module Slow_open = struct
  type t = { base : Eio.Fs.dir_ty Eio.Resource.t }

  (* How many scheduler turns the entering fiber spends inside each open. *)
  let turns : int Eio.Fiber.key = Eio.Fiber.create_key ()

  let dawdle () =
    match Eio.Fiber.get turns with
    | None | Some 0 -> ()
    | Some n ->
        for _ = 1 to n do
          Eio.Fiber.yield ()
        done

  let at t path = (t.base, path)

  let open_out t ~sw ~follow ~append ~create path =
    dawdle ();
    Eio.Path.open_out ~sw ~follow ~append ~create (at t path)

  let open_in t ~sw ~follow path =
    dawdle ();
    Eio.Path.open_in ~sw ~follow (at t path)

  let stat t ~follow path = Eio.Path.stat ~follow (at t path)
  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  (* TODO: drop the attribute when Eio 1.7, whose [Pi.DIR] has [sync_dir], is
     released, and call [Eio.Path.sync_dir]. *)
  let sync_dir t path = Eio_fs_sync.sync_dir (at t path) [@@warning "-32"]
  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "slow-open"
  let open_subtree _ ~sw:_ _ = failwith "Slow_open: the lock opens no subtree"

  include Eio_utils.Posix_path
end

let slow_open ((base, path) : Eio.Fs.dir_ty Eio.Path.t) =
  let dir =
    Eio.Resource.T
      ( { Slow_open.base },
        Eio.Resource.handler
          [
            Eio.Resource.H (Eio.Fs.Pi.Dir, (module Slow_open));
            (* The wrapper answers for the base it was given, as every other
               wrapper here does: over the in-memory backend the files it hands
               out are still this process's heap. *)
            Eio.Resource.H
              ( Eio_fs_sync.Process_local,
                fun (t : Slow_open.t) -> Eio_fs_sync.is_process_local t.base );
          ] )
  in
  ((dir :> Eio.Fs.dir_ty Eio.Resource.t), path)

let test_lock_is_granted_in_the_order_fibers_entered () =
  with_in_memory_fs @@ fun fs ->
  let lock = slow_open fs / "lock" in
  let entered = ref [] and held = ref [] in
  (* Each fiber dawdles for a different number of turns inside its opens, the
     first to enter for the longest, so entry order and open order are exact
     reverses of one another. *)
  let contend name turns () =
    Eio.Fiber.with_binding Slow_open.turns turns @@ fun () ->
    entered := name :: !entered;
    Eio_fs_unix.with_lock lock @@ fun () -> held := name :: !held
  in
  Eio.Fiber.all [ contend "a" 4; contend "b" 2; contend "c" 0 ];
  Alcotest.(check (list string))
    "the fibers entered in this order" [ "a"; "b"; "c" ] (List.rev !entered);
  Alcotest.(check (list string))
    "the lock was granted in the order the fibers entered" [ "a"; "b"; "c" ]
    (List.rev !held)

(* The other side of "a wrapper answers for its base": the crash model wraps a
   real host directory to put a store's durability under test, and the file it
   hands back is that directory's own file. Its exclusion is the host's record
   lock -- any process opens the file by name and takes it -- and the wrapper
   has none of its own to offer instead, so it hands the descriptor beneath to
   this library, and to no caller, and the lock a store takes under the model
   is the lock it takes in production.

   Another process holds the record lock on that same host file throughout, so
   a non-blocking acquisition through the wrapper has one answer, and it names
   the rendezvous. Being granted the lock would mean the wrapper handed out an
   exclusion the operating system never gave; being refused for want of a
   descriptor would mean no crash-modelled store can lock at all. *)
let test_lock_through_a_crash_wrapper_takes_the_host_lock () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  with_held_lock ~sw ~env ~clock ~lock:(dir / "lock") @@ fun () ->
  let wrapped = Crash.wrap (Crash.v ~policy:Drop_unsynced ()) dir in
  let file =
    Eio.Path.open_out ~sw ~create:(`If_missing 0o600) (wrapped / "probe")
  in
  Alcotest.(check bool)
    "a file opened through the wrapper hands its caller no descriptor" true
    (Option.is_none (Eio_unix.Resource.fd_opt file));
  match Eio_fs_unix.with_lock ~block:`No (wrapped / "lock") (fun () -> ()) with
  | () ->
      Alcotest.fail
        "a wrapped rendezvous was locked while another process held its file"
  | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
      Alcotest.(check string)
        "the refusal names the host file the lock is held on"
        (Eio.Path.native_exn (dir / "lock"))
        path

(* What a lock writes is a write like any other: [~contents] goes through the
   wrapper's own file, so the crash model sees it and the policy decides
   whether it survives the cut. The rendezvous' own entry is committed in both
   runs below, so what differs is only the policy's answer about an un-synced
   write. *)
let crash_after_a_lock_published_its_contents ~policy =
  with_real_dir_env @@ fun _env dir ->
  let ctl = Crash.v ~policy () in
  let wrapped = Crash.wrap ctl dir in
  Eio_fs_unix.with_lock ~contents:"held\n" (wrapped / "lock") (fun () -> ());
  Alcotest.(check string)
    "the live rendezvous carries what the holder published" "held\n"
    (Eio.Path.load (dir / "lock"));
  Crash.sync_metadata ctl;
  Eio.Path.load (Crash.post_crash ctl / "lock")

let test_lock_contents_are_lost_by_an_unsynced_crash () =
  Alcotest.(check string)
    "the rendezvous keeps its committed entry and loses the un-synced write" ""
    (crash_after_a_lock_published_its_contents ~policy:Crash.Drop_unsynced)

let test_lock_contents_survive_a_policy_that_keeps_them () =
  Alcotest.(check string)
    "the write went through the wrapper, so the journal holds it" "held\n"
    (crash_after_a_lock_published_its_contents
       ~policy:(Crash.Survive (fun _ -> true)))

(* ---- a backend that hides descriptors and declares nothing ----

   Between the two answers above sits the wiring error they exist to tell
   apart: a directory whose files carry no descriptor and whose handler says
   nothing about who else can open them. It is built here rather than borrowed
   from a wrapper this package ships, because a wrapper that hides descriptors
   today passes them through tomorrow, and the case has to stay the case.

   Every operation is the host directory's own, the native name included, so
   the rendezvous is a real host file; only the files handed back are re-boxed
   behind the read and write operations alone, which is what an adapter written
   without a thought for descriptors produces. *)

module Hidden_ro = struct
  type t = Eio.File.ro_ty Eio.Resource.t

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t buf
  let pread t ~file_offset bufs = Eio.File.pread t ~file_offset bufs
  let stat t = Eio.File.stat t
  let seek t off cmd = Eio.File.seek t off cmd
  let close t = Eio.Resource.close t
end

module Hidden_rw = struct
  type t = Eio.File.rw_ty Eio.Resource.t

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t buf
  let pread t ~file_offset bufs = Eio.File.pread t ~file_offset bufs
  let stat t = Eio.File.stat t
  let seek t off cmd = Eio.File.seek t off cmd
  let close t = Eio.Resource.close t
  let single_write t bufs = Eio.Flow.single_write t bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let pwrite t ~file_offset bufs = Eio.File.pwrite_single t ~file_offset bufs
  let sync t = Eio.File.sync t
  let truncate t len = Eio.File.truncate t len
end

let hidden_ro base : Eio.File.ro_ty Eio.Resource.t =
  Eio.Resource.T (base, Eio.File.Pi.ro (module Hidden_ro))

let hidden_rw base : Eio.File.rw_ty Eio.Resource.t =
  Eio.Resource.T (base, Eio.File.Pi.rw (module Hidden_rw))

module Hide_descriptors = struct
  type t = Eio.Fs.dir_ty Eio.Resource.t

  let at t path = (t, path)

  let open_in t ~sw ~follow path =
    hidden_ro (Eio.Path.open_in ~sw ~follow (at t path))

  let open_out t ~sw ~follow ~append ~create path =
    hidden_rw (Eio.Path.open_out ~sw ~follow ~append ~create (at t path))

  let stat t ~follow path = Eio.Path.stat ~follow (at t path)
  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  (* TODO: drop the attribute when Eio 1.7, whose [Pi.DIR] has [sync_dir], is
     released, and call [Eio.Path.sync_dir]. *)
  let sync_dir t path = Eio_fs_sync.sync_dir (at t path) [@@warning "-32"]
  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "hide-descriptors"

  let open_subtree _ ~sw:_ _ =
    failwith "Hide_descriptors: the lock opens no subtree"

  include Eio_utils.Posix_path
end

let hide_descriptors ((base, path) : Eio.Fs.dir_ty Eio.Path.t) :
    Eio.Fs.dir_ty Eio.Path.t =
  ( (Eio.Resource.T
       ( base,
         Eio.Resource.handler
           [ Eio.Resource.H (Eio.Fs.Pi.Dir, (module Hide_descriptors)) ] )
      : Eio.Fs.dir_ty Eio.Resource.t),
    path )

(* The rendezvous is a host file another process is holding the record lock on
   right here, so the mutex in this process excludes nobody, and a caller let
   into its critical section would be the second writer of a resource one
   process already holds. It is refused instead, and the refusal names both the
   interface that made it and the rendezvous it was asked for, so a wiring
   error reads as one. *)
let test_lock_refuses_a_wrapper_that_hides_a_host_descriptor () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let _, release =
    holding_process ~sw ~env ~clock ~kind:"record" ~lock:(dir / "lock")
  in
  let hidden = hide_descriptors dir in
  let ran = ref false in
  (match Eio_fs_unix.with_lock (hidden / "lock") (fun () -> ran := true) with
  | () ->
      Alcotest.fail
        "a rendezvous with no descriptor was locked while another process held \
         it"
  | exception Invalid_argument message ->
      Alcotest.(check bool)
        "the refusal names the interface that refused" true
        (mentions ~needle:"Eio_fs_unix.with_lock" message);
      Alcotest.(check bool)
        "the refusal names the rendezvous" true
        (mentions ~needle:(Eio.Path.native_exn (dir / "lock")) message));
  Alcotest.(check bool)
    "the critical section did not run over the held file" false !ran;
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- the rendezvous as a section a backend-free component runs inside ----

   [lock_section] hands the lock over as an [Eio_fs_sync.Lock.t], which is the
   argument a component takes when what exclusion its files admit is the
   deployment's to say. What it owes that component is the lock's exclusion
   unchanged, and a refusal in a vocabulary the component can name. *)
let test_section_excludes_fibers_over_the_rendezvous () =
  with_real_dir @@ fun dir ->
  let section = Eio_fs_unix.lock_section ~block:`Fibers (dir / "lock") in
  let inside = ref 0 and most = ref 0 in
  let enter () =
    Eio_fs_sync.Lock.use section (fun () ->
        incr inside;
        most := max !most !inside;
        Eio.Fiber.yield ();
        decr inside)
  in
  Eio.Fiber.both enter enter;
  Alcotest.(check int) "never two fibers in one section" 1 !most;
  (* One section value serves sections at every result type, the packed lock
     inheriting the caller's polymorphism rather than fixing it at the first
     use. *)
  Alcotest.(check string)
    "and answers at another result type" "answer"
    (Eio_fs_sync.Lock.use section (fun () -> "answer"))

(* A hold another process took is what a component cannot see and cannot name.
   It reaches the component as [Unavailable], carrying the rendezvous and its
   holder, so an interface whose type promises a result has something to put in
   it. *)
let test_section_refusal_reaches_the_component_as_unavailable () =
  with_real_dir_env @@ fun env dir ->
  let clock = Eio.Stdenv.clock env in
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~clock ~kind:"record" ~lock in
  let section = Eio_fs_unix.lock_section ~block:`Fibers lock in
  let ran = ref false in
  (match Eio_fs_sync.Lock.use section (fun () -> ran := true) with
  | () ->
      Alcotest.fail "another process's hold was waited out rather than answered"
  | exception Eio_fs_sync.Lock.Unavailable what ->
      Alcotest.(check bool)
        "the refusal names the rendezvous" true
        (mentions ~needle:(Eio.Path.native_exn lock) what);
      Alcotest.(check bool)
        "and names the process that holds it" true
        (names_process what (Eio.Process.pid child)));
  Alcotest.(check bool) "the section did not run" false !ran;
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* A name that is not the lock's own regular file is the other refusal, and it
   is not the ordinary busy one: a component told "held by another process"
   would wait and retry for ever over a planted directory. *)
let test_section_refuses_a_name_that_is_not_the_lock_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "lock");
  let section = Eio_fs_unix.lock_section (dir / "lock") in
  match Eio_fs_sync.Lock.use section (fun () -> ()) with
  | () -> Alcotest.fail "a directory was locked as a rendezvous"
  | exception Eio_fs_sync.Lock.Unavailable what ->
      Alcotest.(check bool)
        "the refusal names the rendezvous" true
        (mentions ~needle:(Eio.Path.native_exn (dir / "lock")) what);
      Alcotest.(check bool)
        "and says it is not the lock's own file" true
        (mentions ~needle:"not the lock's own regular file" what)

(* Only a refusal to enter is translated. [Invalid_argument] is in the set the
   acquisition raises, so a body raising it is the case that separates the two:
   it must travel out as the body raised it, or a component would report a
   failure of its own as the section being unavailable. *)
let test_section_lets_the_body_s_own_raise_out () =
  with_real_dir @@ fun dir ->
  let section = Eio_fs_unix.lock_section (dir / "lock") in
  (match
     Eio_fs_sync.Lock.use section (fun () -> invalid_arg "the body's own")
   with
  | () -> Alcotest.fail "the section swallowed a raise"
  | exception Eio_fs_sync.Lock.Unavailable what ->
      Alcotest.failf "the body's raise was reported as a refusal: %s" what
  | exception Invalid_argument m ->
      Alcotest.(check string) "raised as thrown" "the body's own" m);
  Alcotest.(check int)
    "and the section is still there" 7
    (Eio_fs_sync.Lock.use section (fun () -> 7))

(* A rendezvous carrying no descriptor over a backend that does not declare
   itself process-local offers no exclusion between processes at all, and
   [with_lock] refuses it with [Invalid_argument]. That refusal is an
   acquisition's, so it reaches the component as [Unavailable] like any other:
   an exception the component cannot catch would leave the wiring error to
   surface as a crash somewhere past the interface that promised a result. *)
let test_section_refuses_a_wrapper_that_hides_a_host_descriptor () =
  with_real_dir @@ fun dir ->
  let hidden = hide_descriptors dir in
  let section = Eio_fs_unix.lock_section (hidden / "lock") in
  let ran = ref false in
  (match Eio_fs_sync.Lock.use section (fun () -> ran := true) with
  | () -> Alcotest.fail "a rendezvous with no descriptor was locked"
  | exception Eio_fs_sync.Lock.Unavailable what ->
      Alcotest.(check bool)
        "the refusal names the interface that refused" true
        (mentions ~needle:"Eio_fs_unix.with_lock" what));
  Alcotest.(check bool) "the section did not run" false !ran

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
  let usage = Eio_fs.Path.usage dir in
  Alcotest.(check int64)
    "allocated blocks, with the symlink counted once" expected_bytes
    usage.allocated_bytes;
  Alcotest.(check (float 0.))
    "latest tree mtime" expected_mtime usage.latest_mtime

(* What a copy of a tree will move, asked of the tree before the copy starts.

   The figures are the fixture's own, not a second reading of the same stats:
   two regular files of one and seven bytes, under a directory, beside a
   symbolic link whose target is three characters. A walk that counted the link
   would report three files and eleven bytes, and one that counted the directory
   would report whatever the filesystem gives a directory entry -- so the tree
   separates content from the things that carry it. *)
let test_path_usage_counts_the_files_a_copy_would_move () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "a") "a";
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "sub/b") "payload";
  Eio.Path.symlink ~link_to:"sub" (dir / "link");
  let usage = Eio_fs.Path.usage dir in
  Alcotest.(check int) "regular files, and nothing else" 2 usage.files;
  Alcotest.(check int64)
    "the length of those files, summed" 8L usage.apparent_bytes;
  Alcotest.(check bool)
    "which is no more than the tree allocates" true
    (Int64.compare usage.apparent_bytes usage.allocated_bytes <= 0)

(* The walk usage reads, given to a caller: each entry once, with its own
   stat, a symbolic link as itself and never what it names. *)
let test_path_iter_visits_each_entry_once () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "a") "a";
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "sub/b") "payload";
  Eio.Path.symlink ~link_to:"sub" (dir / "link");
  let seen = ref [] in
  Eio_fs.Path.iter ~max_fibers:2
    (fun path (stat : Eio.File.Stat.t) ->
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
  let usage = Eio_fs.Path.usage dir in
  Alcotest.(check int) "no files" 0 usage.files;
  Alcotest.(check int64) "no bytes to carry" 0L usage.apparent_bytes

(* A path reached straight through the unconfined filesystem is named by the
   spelling the host knows it under, so a message carrying that name hands its
   reader something to open, stat or remove rather than a label private to the
   program that printed it. *)
let test_name_is_the_host_spelling () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "image.raw") "payload";
  let name = Eio_fs.Path.name (dir / "image.raw") in
  Alcotest.(check string)
    "a path on the host filesystem is named by its native spelling"
    (Eio.Path.native_exn (dir / "image.raw"))
    name;
  Alcotest.(check bool)
    "the name picks the file out for anything outside the program" true
    (Sys.file_exists name)

(* A file reached through an opened directory has a second rendering, and it is
   the wrong one to name it by: a directory capability is labelled with the
   basename it was opened under, so Eio prints a member of it as
   "<label:member>", which points at no file on the host and matches no path the
   caller asked for. The name stays the full native path, so one file answers to
   one name whichever capability reached it. *)
let test_name_of_a_member_of_an_opened_subtree () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "shadow") "root:x:0:0:::\n";
  Eio.Switch.run @@ fun sw ->
  let member = Eio.Path.open_subtree ~sw dir / "shadow" in
  let name = Eio_fs.Path.name member in
  Alcotest.(check string)
    "a member of an opened subtree is named by its full native path"
    (Eio.Path.native_exn (dir / "shadow"))
    name;
  Alcotest.(check bool)
    "the name picks the file out for anything outside the program" true
    (Sys.file_exists name);
  Alcotest.(check string)
    "one file answers to one name, whichever capability reached it"
    (Eio_fs.Path.name (dir / "shadow"))
    name

(* renameat2(2) and renameatx_np(2) exist only on Linux and Darwin; elsewhere
   [exclusive_rename] answers ENOSYS. *)
let renames f =
  try f ()
  with Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENOSYS, _, _)), _) ->
    Alcotest.skip ()

(* Linux renameat2(RENAME_NOREPLACE) and Darwin renameatx_np(RENAME_EXCL)
   require refusal when the destination already exists. The source survives. *)
let test_exclusive_rename_refuses_existing_names () =
  renames @@ fun () ->
  with_real_dir @@ fun dir ->
  let source = dir / "source" and target = dir / "target" in
  Eio.Path.save ~create:(`Exclusive 0o600) source "new";
  Eio.Path.save ~create:(`Exclusive 0o600) target "old";
  let rename () =
    Eio_fs_sync.Rename.exclusive Eio_fs_unix.exclusive_rename source target
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
         Eio_fs_sync.Rename.exclusive Eio_fs_unix.exclusive_rename
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
  let config = Eio_fs_sync.config ~perm:0o600 ~max_bytes:7L () in
  Eio_fs_sync.publish ~config ~mode:`Exclusive
    ~rename:Eio_fs_unix.exclusive_rename (fs / "state")
    (Eio.Flow.copy_string "payload");
  Crash.crash ctl;
  Alcotest.(check string)
    "published bytes survive" "payload"
    (Eio.Path.load (Crash.post_crash ctl / "state"))

(* The platform barrier carries a publication on a real directory, a durable
   generation's read-only handles included. *)
let test_publication_with_the_platform_barrier () =
  with_real_dir @@ fun dir ->
  let barrier = Eio_fs_unix.barrier in
  Eio_fs_sync.publish
    ~config:(Eio_fs_sync.config ~barrier ~max_bytes:7L ())
    (dir / "state")
    (Eio.Flow.copy_string "payload");
  Eio_fs_sync.publish_generation ~barrier (dir / "tree") (fun d ->
      Eio.Path.save ~create:(`Exclusive 0o444) (d / "f") "frozen");
  Alcotest.(check string) "the file" "payload" (Eio.Path.load (dir / "state"));
  Alcotest.(check string)
    "the generation" "frozen"
    (Eio.Path.load (dir / "tree/f"))

let test_concurrent_exclusive_publications_have_one_winner () =
  renames @@ fun () ->
  with_real_dir @@ fun dir ->
  let config = Eio_fs_sync.config ~perm:0o600 ~max_bytes:3L () in
  let save text () =
    match
      Eio_fs_sync.publish ~config ~mode:`Exclusive
        ~rename:Eio_fs_unix.exclusive_rename (dir / "winner")
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
  Eio_fs_sync.Rename.exclusive Eio_fs_unix.exclusive_rename (parent / "source")
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
  not_found "statvfs" (fun () -> ignore (Eio_fs_unix.statvfs missing));
  not_found "xattrs" (fun () -> ignore (Eio_fs_unix.xattrs missing));
  not_found "set_xattr" (fun () -> Eio_fs_unix.set_xattr missing "user.a" "v");
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

(* The lease is a kernel flock, which dies with its holder: a process that
   exits holding it leaves no stale lock, and the next taker is granted it at
   once, with no takeover. *)
let test_lease_dies_with_its_holder () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lease" in
  let manager = Eio.Stdenv.process_mgr env in
  let self = Sys.executable_name in
  Alcotest.check status "the holder exited holding the lease" (`Exited 0)
    ( Eio.Switch.run @@ fun sw ->
      Eio.Process.await
        (Eio.Process.spawn ~sw manager ~executable:self
           [ self; "--exit-holding-eio-fs-lease"; Eio.Path.native_exn lock ]) );
  Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())

(* A lease a fiber of this process holds is refused to another fiber as held by
   this process, which a reader must not take for another process: the
   refusal says so, and names no pid that reads as a foreign holder. *)
let test_lease_refusal_names_a_fiber () =
  with_real_dir_env @@ fun _env dir ->
  let lock = dir / "lease" in
  Eio_fs_unix.Lease.with_exclusive ~contents:"preparing lib\n" lock @@ fun _ ->
  match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
  | () -> Alcotest.fail "a lease a fiber holds was granted"
  | exception Eio_fs_unix.Lock_unavailable refusal ->
      let said = Fmt.str "%a" Eio_fs_unix.pp_refusal refusal in
      let has sub =
        let n = String.length sub and m = String.length said in
        let rec at i =
          i + n <= m && (String.sub said i n = sub || at (i + 1))
        in
        at 0
      in
      Alcotest.(check bool)
        ("another fiber of this process: " ^ said)
        true
        (has "held by another fiber of this process");
      Alcotest.(check bool)
        ("what the holder is doing: " ^ said)
        true (has "preparing lib");
      Alcotest.(check bool)
        ("no pid as a foreign holder: " ^ said)
        false
        (Fmt.kstr has "process %d" (Unix.getpid ()))

let test_switch_lease_release () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  Eio.Switch.run @@ fun sw ->
  let lease = Eio_fs_unix.Lease.take ~sw lock in
  (match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
  | () -> Alcotest.fail "a contender acquired the switch's lease"
  | exception Eio_fs_unix.Lock_unavailable _ -> ());
  Eio_fs_unix.Lease.release lease;
  Eio_fs_unix.Lease.release lease;
  Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())

let test_switch_lease_exception () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  let exception End in
  (match
     Eio.Switch.run (fun sw ->
         ignore (Eio_fs_unix.Lease.take ~sw lock);
         raise End)
   with
  | () -> Alcotest.fail "the holder exception disappeared"
  | exception End -> ());
  Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())

let test_switch_lease_cancel () =
  let take ~on_wait lock fn =
    Eio.Switch.run @@ fun sw ->
    ignore (Eio_fs_unix.Lease.take ~sw ~on_wait lock);
    fn ()
  in
  a_wait_is_cancellable ~kind:"lease" take ~free:lease_free ()

(* A reader's look at a lease, a shared lease taken and let go at once, is
   waited for by an exclusive take with [`Readers], which takes the lease
   once the look lets go; an exclusive holder refuses it. A take that read
   the look as a holder, as [`No] does, refuses a lease nobody keeps: the
   failure mq's lease takers, build slots and run lock met under readers. *)
let test_lease_waits_out_readers () =
  with_real_dir_env @@ fun _env dir ->
  let lock = dir / "readers" in
  let looking, looked = Eio.Promise.create () in
  let waiting, waited = Eio.Promise.create () in
  let entered = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.Lease.with_shared lock (fun _ ->
          Eio.Promise.resolve looked ();
          Eio.Promise.await waiting))
    (fun () ->
      Eio.Promise.await looking;
      Eio_fs_unix.Lease.with_exclusive ~block:`Readers
        ~on_wait:(fun () -> Eio.Promise.resolve waited ())
        lock
        (fun _ -> entered := true));
  Alcotest.(check bool) "taken once the look let go" true !entered;
  Alcotest.(check bool)
    "it said it waits" true
    (Option.is_some (Eio.Promise.peek waiting));
  Eio_fs_unix.Lease.with_exclusive lock (fun _ ->
      match
        Eio_fs_unix.Lease.with_exclusive ~block:`Readers lock (fun _ -> ())
      with
      | () -> Alcotest.fail "an exclusive holder did not refuse the take"
      | exception Eio_fs_unix.Lock_unavailable _ -> ())

let lease_test_case =
  Alcotest.test_case "a lease follows its inherited descriptor" `Quick
    test_lease_survives_owner_close_until_inherited_descriptor_closes

let lease_unlock_test_case =
  Alcotest.test_case "an unlocked lease frees its inheritor" `Quick
    test_lease_unlock_frees_inherited_descriptor

let lease_readers_test_case =
  Alcotest.test_case "a lease taker waits out a reader's look" `Quick
    test_lease_waits_out_readers

let lease_suite =
  ("lease", [ lease_test_case; lease_unlock_test_case; lease_readers_test_case ])

let suite =
  ( "eio_fs_unix",
    [
      Alcotest.test_case "a switch lease releases once" `Quick
        test_switch_lease_release;
      Alcotest.test_case "a switch lease ends on exception" `Quick
        test_switch_lease_exception;
      Alcotest.test_case "a switch lease acquisition is cancellable" `Quick
        test_switch_lease_cancel;
      Alcotest.test_case "a byte-range lock leaves the offset" `Quick
        test_lock_range;
      Alcotest.test_case "exclusive rename refuses existing names" `Quick
        test_exclusive_rename_refuses_existing_names;
      Alcotest.test_case "a lease dies with its holder" `Quick
        test_lease_dies_with_its_holder;
      Alcotest.test_case "a lease refusal names a fiber" `Quick
        test_lease_refusal_names_a_fiber;
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
      Alcotest.test_case "name is the host's spelling of the path" `Quick
        test_name_is_the_host_spelling;
      Alcotest.test_case "name of a member of an opened subtree" `Quick
        test_name_of_a_member_of_an_opened_subtree;
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
      Alcotest.test_case "an imported descriptor is a file" `Quick
        test_import_file;
      Alcotest.test_case "file barrier asks the registration first" `Quick
        test_sync_file_asks_the_registration_before_the_descriptor;
      Alcotest.test_case "file barrier takes an unregistered file's descriptor"
        `Quick test_sync_file_takes_the_descriptor_when_nothing_is_registered;
      Alcotest.test_case "file barrier survives a modelled cut" `Quick
        test_sync_file_under_the_crash_model_survives_the_cut;
      Alcotest.test_case "file barrier on a plain POSIX file" `Quick
        test_sync_file_on_a_plain_posix_file;
      Alcotest.test_case "full_fsync on a file" `Quick test_full_fsync_file;
      Alcotest.test_case "full_fsync surfaces errors" `Quick
        test_full_fsync_surfaces_errors;
      Alcotest.test_case "statvfs reports raw block capacity" `Quick
        test_statvfs_reports_raw_block_capacity;
      Alcotest.test_case "clone_tree" `Quick test_clone_tree;
      Alcotest.test_case "calls on a path raise Eio.Io" `Quick
        test_path_calls_raise_eio_io;
      Alcotest.test_case "path calls stay in their directory" `Quick
        test_path_calls_stay_in_their_directory;
      Alcotest.test_case "pread leaves the descriptor's offset" `Quick
        test_pread_leaves_the_offset;
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
      Alcotest.test_case "lock wait does not block Eio" `Quick
        test_lock_wait_does_not_block_eio;
      lease_test_case;
      lease_unlock_test_case;
      lease_readers_test_case;
      Alcotest.test_case "a record lock wait answers at the release" `Quick
        (a_wait_answers_at_the_release ~kind:"record" take_record);
      Alcotest.test_case "a record lock wait answers at a cancel" `Quick
        (a_wait_is_cancellable ~kind:"record" take_record ~free:record_free);
      Alcotest.test_case "a lease wait answers at the release" `Quick
        (a_wait_answers_at_the_release ~kind:"lease" take_lease);
      Alcotest.test_case "a lease wait answers at a cancel" `Quick
        (a_wait_is_cancellable ~kind:"lease" take_lease ~free:lease_free);
      Alcotest.test_case "a bounded record lock wait answers at the release"
        `Quick
        (a_bounded_wait_answers_at_the_release ~kind:"record" take_record_for);
      Alcotest.test_case "a bounded lease wait answers at the release" `Quick
        (a_bounded_wait_answers_at_the_release ~kind:"lease" take_lease_for);
      Alcotest.test_case "a lease wait for a fiber says so" `Quick
        test_lease_wait_for_a_fiber_says_so;
      Alcotest.test_case "lock serializes fibers" `Quick
        test_lock_serializes_fibers;
      Alcotest.test_case "lock excludes another name for its file" `Quick
        test_lock_excludes_another_name_for_the_locked_file;
      Alcotest.test_case "lock answers a parent link that leads nowhere" `Quick
        test_lock_answers_a_parent_link_that_leads_nowhere;
      Alcotest.test_case "lock contents stay on the locked inode" `Quick
        test_lock_updates_contents_in_place;
      Alcotest.test_case "non-blocking lock reports unavailable" `Quick
        test_lock_non_blocking_unavailable;
      Alcotest.test_case "a refusal names the holding process" `Quick
        test_lock_refusal_names_the_holding_process;
      Alcotest.test_case "a refusal names a fiber of this process" `Quick
        test_lock_refusal_names_this_process_for_a_fiber;
      Alcotest.test_case "a refusal in the holder keeps its hold" `Quick
        test_lock_refusal_in_the_holder_keeps_its_hold;
      Alcotest.test_case "a bounded wait in the holder keeps its hold" `Quick
        test_lock_bounded_wait_in_the_holder_keeps_its_hold;
      Alcotest.test_case "asks from the holder leave no descriptor" `Quick
        test_lock_asks_from_the_holder_leave_no_descriptor;
      Alcotest.test_case "asks during a hold keep one descriptor" `Quick
        test_lock_asks_during_a_hold_keep_one_descriptor;
      Alcotest.test_case "asks while the holder lets go answer" `Quick
        test_lock_asks_while_the_holder_lets_go;
      Alcotest.test_case "a probe names the holding process" `Quick
        test_probe_names_the_holding_process;
      Alcotest.test_case "a probe from the holder keeps its hold" `Quick
        test_probe_from_the_holder_keeps_its_hold;
      Alcotest.test_case "a probe answers a read-only lock file" `Quick
        test_probe_answers_a_read_only_lock_file;
      Alcotest.test_case "a probe of a lock file that goes" `Quick
        test_probe_of_a_lock_file_that_goes;
      Alcotest.test_case "a probe of a lock file replaced" `Quick
        test_probe_of_a_lock_file_replaced;
      Alcotest.test_case "a hold lasts as long as its switch" `Quick
        test_hold_lasts_as_long_as_its_switch;
      Alcotest.test_case "a hold's refusal names the holding process" `Quick
        test_hold_refusal_names_the_holding_process;
      Alcotest.test_case "a held lock restates its record" `Quick
        test_held_restates_its_record;
      Alcotest.test_case "a held lock's release leaves a record" `Quick
        test_held_release_lets_the_lock_go_and_leaves_a_record;
      Alcotest.test_case "a held lock ends with its switch" `Quick
        test_held_ends_with_its_switch;
      Alcotest.test_case "a restate takes back a lock a stray close dropped"
        `Quick test_held_restate_takes_back_a_lock_a_stray_close_dropped;
      Alcotest.test_case "a release from its switch's hook leaves a record"
        `Quick test_held_released_from_its_switch_hook_leaves_a_record;
      Alcotest.test_case "a fibers-only lock queues behind a fiber" `Quick
        test_lock_fibers_mode_queues_behind_a_fiber;
      Alcotest.test_case "a fibers-only lock answers another process" `Quick
        test_lock_fibers_mode_refuses_another_process;
      Alcotest.test_case "a bounded wait answers at the bound" `Quick
        test_lock_bounded_wait_answers_at_the_bound;
      Alcotest.test_case "a bounded wait answers a fiber that holds it" `Quick
        test_lock_bounded_wait_answers_a_fiber_that_holds_it;
      Alcotest.test_case "a wait behind a fiber says so" `Quick
        test_lock_wait_behind_a_fiber_says_so;
      Alcotest.test_case "a bounded wait does not sit out a killed holder"
        `Quick test_lock_bounded_wait_does_not_sit_out_a_killed_holder;
      Alcotest.test_case "existing-only lock does not create" `Quick
        test_lock_existing_does_not_create;
      Alcotest.test_case "lock refuses a symlink at the lock path" `Quick
        test_lock_refuses_a_symlink_at_the_lock_path;
      Alcotest.test_case "lock refuses a dangling symlink" `Quick
        test_lock_refuses_a_dangling_symlink_without_creating_its_target;
      Alcotest.test_case
        "lock creates nothing through a link planted after the check" `Quick
        test_lock_creates_nothing_through_a_link_planted_after_the_check;
      Alcotest.test_case "lock refuses a directory or a FIFO" `Quick
        test_lock_refuses_a_directory_or_a_fifo;
      Alcotest.test_case "missing rendezvous is created as a regular file"
        `Quick test_lock_creates_a_missing_rendezvous_as_a_regular_file;
      Alcotest.test_case "lock serializes fibers over a process-local backend"
        `Quick test_lock_over_a_process_local_backend_serializes_fibers;
      Alcotest.test_case "a process-local backend still refuses a symlink"
        `Quick test_lock_over_a_process_local_backend_refuses_a_symlink;
      Alcotest.test_case "an uncontended bounded lock needs no clock" `Quick
        test_lock_bounded_and_uncontended_needs_no_clock;
      Alcotest.test_case
        "lock serializes fibers over a wrapped process-local backend" `Quick
        test_lock_over_a_wrapped_process_local_backend_serializes_fibers;
      Alcotest.test_case "lock is granted in the order fibers entered" `Quick
        test_lock_is_granted_in_the_order_fibers_entered;
      Alcotest.test_case "lock through a crash wrapper takes the host lock"
        `Quick test_lock_through_a_crash_wrapper_takes_the_host_lock;
      Alcotest.test_case "an un-synced crash loses a lock's contents" `Quick
        test_lock_contents_are_lost_by_an_unsynced_crash;
      Alcotest.test_case "a lock's contents survive a policy that keeps them"
        `Quick test_lock_contents_survive_a_policy_that_keeps_them;
      Alcotest.test_case "lock refuses a wrapper that hides a host descriptor"
        `Quick test_lock_refuses_a_wrapper_that_hides_a_host_descriptor;
      Alcotest.test_case "a lock section excludes fibers over the rendezvous"
        `Quick test_section_excludes_fibers_over_the_rendezvous;
      Alcotest.test_case "a lock section's refusal is Unavailable" `Quick
        test_section_refusal_reaches_the_component_as_unavailable;
      Alcotest.test_case
        "a lock section refuses a name that is not the lock file" `Quick
        test_section_refuses_a_name_that_is_not_the_lock_file;
      Alcotest.test_case "a lock section lets the body's own raise out" `Quick
        test_section_lets_the_body_s_own_raise_out;
      Alcotest.test_case
        "a lock section refuses a wrapper that hides a descriptor" `Quick
        test_section_refuses_a_wrapper_that_hides_a_host_descriptor;
      Alcotest.test_case "path usage is allocated and symlink-safe" `Quick
        test_path_usage_counts_allocated_tree_without_following_links;
      Alcotest.test_case "path usage counts what a copy would move" `Quick
        test_path_usage_counts_the_files_a_copy_would_move;
      Alcotest.test_case "path iter visits each entry once" `Quick
        test_path_iter_visits_each_entry_once;
      Alcotest.test_case "path usage of an empty tree moves nothing" `Quick
        test_path_usage_of_an_empty_tree_moves_nothing;
      Alcotest.test_case "xattr(7) values round-trip byte for byte" `Quick
        test_xattr_round_trip;
      Alcotest.test_case "xattr(7) calls do not follow a final link" `Quick
        test_xattr_does_not_follow;
    ] )
