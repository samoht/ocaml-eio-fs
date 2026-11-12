let ( / ) = Eio.Path.( / )

let expect_io_error expected fn =
  match fn () with
  | () -> Alcotest.failf "expected %s" expected
  | exception Eio.Io (Eio.Fs.E error, _) -> (
      match (expected, error) with
      | "not found", Eio.Fs.Not_found _ -> ()
      | "already exists", Eio.Fs.Already_exists _ -> ()
      | "permission denied", Eio.Fs.Permission_denied _ -> ()
      | "too many symbolic links", Eio.Fs.Symlink -> ()
      | _ -> Alcotest.failf "expected %s, got another filesystem error" expected
      )

let with_env ?cwd ?user fn =
  Eio_mock.Backend.run @@ fun () -> Eio_fs.run ?cwd ?user fn

(* A user who is not root, whom file modes bind. *)
let alice = { Eio_fs.uid = 1000; gid = 1000; groups = []; umask = 0o022 }

let test_missing_file () =
  with_env @@ fun env ->
  expect_io_error "not found" (fun () ->
      ignore (Eio.Path.load Eio.Path.(env#fs / "missing")))

let test_exclusive_create () =
  with_env @@ fun env ->
  let path = env#fs / "exists" in
  Eio.Path.save ~create:(`Or_truncate 0o600) path "first";
  expect_io_error "already exists" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o600) path "second");
  Alcotest.(check string) "original contents" "first" (Eio.Path.load path)

let test_cwd_parent_escape () =
  with_env ~cwd:"/sandbox" @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(env#fs / "/outside")
    "secret";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load Eio.Path.(env#cwd / "../outside")))

let test_cwd_symlink_escape () =
  with_env ~cwd:"/sandbox" @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(env#fs / "/outside")
    "secret";
  Eio.Path.symlink ~link_to:"../outside" Eio.Path.(env#fs / "/sandbox/link");
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load Eio.Path.(env#cwd / "link")))

(* {2 POSIX permission semantics}

   The in-memory backend must model open(2) faithfully enough for
   fault-injection tests to be meaningful: a created file records the mode the
   caller asked for, an existing file without the owner-write bit cannot be
   reopened for writing (EACCES), and the open that creates a file is never
   restricted by the mode it sets. *)

(* open(2): "the mode argument specifies the file mode bits to be applied when
   a new file is created". The recorded mode is what the caller passed, not the
   open flags. *)
let test_create_mode_recorded () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o444) (env#fs / "ro") "x";
  let stat = Eio.Path.stat ~follow:true (env#fs / "ro") in
  Alcotest.(check int) "created file records its mode" 0o444 stat.perm

(* open(2): EACCES when "the file exists and the permissions specified by
   oflag are denied". Reopening an existing 0o444 file for writing must fail,
   even for the owner, unless root. *)
let test_readonly_reopen_denied () =
  with_env ~user:alice @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o444) (env#fs / "ro") "x";
  expect_io_error "permission denied" (fun () ->
      Eio.Path.with_open_out ~create:(`Or_truncate 0o444) (env#fs / "ro")
        (fun _ -> ()))

(* open(2): the mode of a freshly created file does not restrict the creating
   open itself -- O_CREAT|O_RDWR with mode 0444 yields a writable fd. *)
let test_readonly_fresh_create_writable () =
  with_env @@ fun env ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o444) (env#fs / "ro") (fun f ->
      Eio.Flow.copy_string "data" f);
  Alcotest.(check string)
    "fresh read-only file accepted writes" "data"
    (Eio.Path.load (env#fs / "ro"))

(* chmod(2) restores writability: after adding the owner-write bit an existing
   read-only file reopens for writing. *)
let test_chmod_restores_writability () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o444) (env#fs / "ro") "x";
  Eio.Path.chmod ~follow:true ~perm:0o644 (env#fs / "ro");
  Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "ro") "y";
  Alcotest.(check string) "rewritten" "y" (Eio.Path.load (env#fs / "ro"))

(* open(2): EACCES also when read permission is missing. A write-only file
   can be created -- the creating open is unrestricted -- but not reopened:
   this backend opens files O_RDONLY or O_RDWR, and both need the read bit. *)
let test_unreadable_reopen_denied () =
  with_env ~user:alice @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o200) (env#fs / "wo") "x";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load (env#fs / "wo")));
  expect_io_error "permission denied" (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o200) (env#fs / "wo") "y")

(* A directory capability opened for a switch dies with the switch, as a real
   O_DIRECTORY fd does: using it afterwards raises [Invalid_argument], as
   eio_posix does, not silent success. *)
let test_subtree_fd_dies_with_switch () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "d/f") "x";
  let leaked = ref None in
  Eio.Switch.run (fun sw ->
      leaked := Some (Eio.Path.open_dir ~sw (env#fs / "d")));
  let stale = Option.get !leaked in
  match Eio.Path.load Eio.Path.(stale / "f") with
  | (_ : string) ->
      Alcotest.fail "stale directory fd still resolves after its switch exited"
  | exception Invalid_argument _ -> ()

(* close(2): "the file descriptor is deallocated" and the next open may be
   handed the same number. A handle answers on its own state: closing it a
   second time does nothing (as [Eio.Resource.close] specifies), and using it
   raises [Invalid_argument], as eio_posix does, whatever file has taken its
   number since. Each case opens [a],
   closes it, then opens [b], which takes the number [a] had. *)
let with_reused_number env fn =
  let fs = env#fs in
  Eio.Switch.run @@ fun sw ->
  let a = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (fs / "a") in
  Eio.Resource.close a;
  let b = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (fs / "b") in
  fn a b

let test_stale_close_spares_reused_fd () =
  with_env @@ fun env ->
  with_reused_number env (fun a b ->
      Eio.Resource.close a;
      Eio.Flow.copy_string "hello" b);
  Alcotest.(check string)
    "the open handle kept its file" "hello"
    (Eio.Path.load (env#fs / "b"))

let test_closed_handle_write_refused () =
  with_env @@ fun env ->
  with_reused_number env (fun a _ ->
      (match Eio.Flow.copy_string "stale" a with
      | () -> Alcotest.fail "a write through a closed handle succeeded"
      | exception Invalid_argument _ -> ());
      match Eio.File.stat a with
      | _ -> Alcotest.fail "a stat through a closed handle succeeded"
      | exception Invalid_argument _ -> ());
  Alcotest.(check string)
    "the file holding the number is untouched" ""
    (Eio.Path.load (env#fs / "b"))

(* {2 Descriptors that are not regular files}

   Opening a directory as a flow is a supported operation: eio-fs.sync reaches a
   directory's write pi that way, and both real Eio backends allow the O_RDONLY
   open. The descriptor that comes back still answers the whole file interface,
   so every operation on it has to fail the way a real backend fails one.
   read(2) and write(2) on a directory descriptor are EISDIR, which Eio raises
   as an [Eio.Io]; a bare [Failure] would leave a caller that handles Eio's
   error model correctly crashing on this backend alone. *)

let expect_eio_io what fn =
  match fn () with
  | () -> Alcotest.failf "expected %s to fail" what
  | exception Eio.Io _ -> ()

let test_read_directory_fd () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  Eio.Path.with_open_in (env#fs / "d") @@ fun flow ->
  let buf = Cstruct.create 8 in
  expect_eio_io "reading a directory descriptor" (fun () ->
      ignore (Eio.Flow.single_read flow buf : int));
  expect_eio_io "positioned read of a directory descriptor" (fun () ->
      ignore (Eio.File.pread flow ~file_offset:Optint.Int63.zero [ buf ] : int))

(* open(2) for writing refuses a directory with EISDIR, at the open. *)
let test_write_directory_fd () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  List.iter
    (fun create ->
      expect_eio_io "opening a directory for writing" (fun () ->
          Eio.Path.with_open_out ~create (env#fs / "d") ignore))
    [ `Never; `If_missing 0o644; `Or_truncate 0o644 ]

(* open(2) with O_CREAT answers ENOTDIR when a component of the path prefix is
   not a directory. *)
let test_create_under_regular_file () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (env#fs / "reg") "x";
  expect_eio_io "creating a file under a regular file" (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o600) (env#fs / "reg/child") "y")

(* open(2) with O_CREAT on a missing path in directory syntax ("x/", "x/.")
   creates nothing: a trailing separator names a directory, which O_CREAT
   never makes. Linux answers EISDIR and macOS ENOENT, so the case pins the
   [Eio.Io] and that no entry appears, not which of the two. *)
let test_create_directory_syntax () =
  with_env @@ fun env ->
  List.iter
    (fun spelling ->
      expect_eio_io (Fmt.str "creating %S" spelling) (fun () ->
          Eio.Path.save ~create:(`Exclusive 0o600) (env#fs / spelling) "y");
      Alcotest.(check bool)
        (Fmt.str "creating %S made no entry" spelling)
        true
        (Eio.Path.kind ~follow:false (env#fs / "x") = `Not_found))
    [ "x/"; "x/." ]

(* ENOTDIR, which Eio.Fs has no constructor for: an [Eio.Io] that is not
   [Not_found], so a [~missing_ok] does not swallow it, as on eio_posix. *)
let expect_enotdir what fn =
  match fn () with
  | _ -> Alcotest.failf "%s: expected ENOTDIR" what
  | exception Eio.Io (Eio.Exn.X _, _) -> ()
  | exception Eio.Io (Eio.Fs.E e, _) ->
      Alcotest.failf "%s: expected ENOTDIR, got %a" what Eio.Exn.pp_err
        (Eio.Fs.E e)

let test_dir_ops_under_regular_file () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Or_truncate 0o600) (fs / "reg") "x";
  let under = fs / "reg" / "sub" in
  expect_enotdir "mkdir" (fun () -> Eio.Path.mkdir ~perm:0o755 under);
  expect_enotdir "unlink" (fun () -> Eio.Path.unlink under);
  expect_enotdir "unlink ~missing_ok" (fun () ->
      Eio.Path.unlink ~missing_ok:true under);
  expect_enotdir "rmtree ~missing_ok" (fun () ->
      Eio.Path.rmtree ~missing_ok:true under);
  expect_enotdir "symlink" (fun () -> Eio.Path.symlink ~link_to:"t" under);
  expect_enotdir "rmdir" (fun () -> Eio.Path.rmdir under);
  expect_enotdir "read_dir" (fun () -> Eio.Path.read_dir under);
  expect_enotdir "kind" (fun () -> Eio.Path.kind ~follow:true under);
  expect_enotdir "stat" (fun () -> Eio.Path.stat ~follow:false under);
  expect_enotdir "load" (fun () -> Eio.Path.load under);
  expect_enotdir "read_link" (fun () -> Eio.Path.read_link under);
  expect_enotdir "mkdirs ~exists_ok" (fun () ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (under / "deeper"))

(* A trailing slash names a directory: on anything else it is ENOTDIR, and a
   non-directory is never created under such a name. *)
let test_trailing_slash () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "f") "x";
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.symlink ~link_to:"d" (fs / "l");
  expect_enotdir "unlink f/" (fun () -> Eio.Path.unlink (fs / "f/"));
  expect_enotdir "unlink l/" (fun () -> Eio.Path.unlink (fs / "l/"));
  expect_enotdir "rmdir l/" (fun () -> Eio.Path.rmdir (fs / "l/"));
  expect_enotdir "rename f g/" (fun () ->
      Eio.Path.rename (fs / "f") (fs / "g/"));
  expect_enotdir "rename f/ g" (fun () ->
      Eio.Path.rename (fs / "f/") (fs / "g"));
  expect_enotdir "rename l/ m" (fun () ->
      Eio.Path.rename (fs / "l/") (fs / "m"));
  expect_io_error "not found" (fun () ->
      Eio.Path.symlink ~link_to:"t" (fs / "s/"));
  Alcotest.(check (list string))
    "nothing moved" [ "d"; "f"; "l" ]
    (List.sort String.compare (Eio.Path.read_dir fs));
  Eio.Path.rename (fs / "d/") (fs / "e/");
  Eio.Path.rmdir (fs / "e/");
  Alcotest.(check (list string))
    "a directory takes the slash" [ "f"; "l" ]
    (List.sort String.compare (Eio.Path.read_dir fs))

(* A file grows by doubling its capacity, so many small writes cost linear
   time: 16384 appends of 1 KiB copy 16 MiB, not 128 GiB. *)
let test_many_small_writes () =
  with_env @@ fun env ->
  let chunk = String.make 1024 'x' in
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (env#fs / "big") (fun f ->
      for _ = 1 to 16384 do
        Eio.Flow.copy_string chunk f
      done);
  Alcotest.(check int)
    "size" (16384 * 1024)
    (Optint.Int63.to_int (Eio.Path.stat ~follow:false (env#fs / "big")).size)

(* EFBIG past the size cap, never an allocation failure or an overflow. *)
let test_file_too_large () =
  with_env @@ fun env ->
  let too_large what fn =
    match fn () with
    | _ -> Alcotest.failf "%s: expected File_too_large" what
    | exception Eio.Io (Eio.Fs.E Eio.Fs.File_too_large, _) -> ()
  in
  let huge = Optint.Int63.of_int (1 lsl 47) in
  let x = [ Cstruct.of_string "x" ] in
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (env#fs / "f") @@ fun f ->
  too_large "pwrite at 2^47" (fun () ->
      Eio.File.pwrite_single f ~file_offset:huge x);
  too_large "pwrite at max_int - 1" (fun () ->
      Eio.File.pwrite_single f
        ~file_offset:(Optint.Int63.of_int (max_int - 1))
        [ Cstruct.of_string "abcd" ]);
  too_large "truncate to 2^47" (fun () -> Eio.File.truncate f huge);
  ignore (Eio.File.seek f huge `Set : Optint.Int63.t);
  too_large "write after a seek to 2^47" (fun () -> Eio.Flow.single_write f x);
  Alcotest.(check int)
    "the file is empty" 0
    (Optint.Int63.to_int (Eio.File.size f))

(* An unlinked or replaced file is freed once no name and no descriptor
   holds it. An embedded file aliases its string, which so becomes
   collectable. *)
let embedded names =
  let weak = Weak.create (List.length names) in
  let entries =
    List.mapi
      (fun i name ->
        let contents =
          Sys.opaque_identity (String.make 64 (Char.chr (97 + i)))
        in
        Weak.set weak i (Some contents);
        Eio_fs.Crunch.File { path = "/" ^ name; perm = 0o644; contents })
      names
  in
  ( weak,
    Eio_fs.Crunch.load
      (Eio_fs.Crunch.Dir { path = "/"; perm = 0o755 } :: entries) )

let collected weak i =
  Gc.full_major ();
  Option.is_none (Weak.get weak i)

let test_unlinked_file_freed () =
  Eio_mock.Backend.run @@ fun () ->
  let weak, fs = embedded [ "a"; "b"; "c" ] in
  Eio.Path.unlink (fs / "a");
  Eio.Path.rename (fs / "b") (fs / "c");
  Alcotest.(check bool) "the unlinked file" true (collected weak 0);
  Alcotest.(check bool) "the replaced file" true (collected weak 2);
  Alcotest.(check bool) "the renamed file" false (collected weak 1);
  Alcotest.(check string)
    "still read" (String.make 64 'b')
    (Eio.Path.load (fs / "c"))

(* A descriptor keeps an unlinked file alive. *)
let test_unlinked_open_file_kept () =
  Eio_mock.Backend.run @@ fun () ->
  let weak, fs = embedded [ "a" ] in
  Eio.Switch.run (fun sw ->
      let f = Eio.Path.open_in ~sw (fs / "a") in
      Eio.Path.unlink (fs / "a");
      Alcotest.(check bool) "held" false (collected weak 0);
      let buf = Cstruct.create 64 in
      Eio.Flow.read_exact f buf;
      Alcotest.(check string)
        "read" (String.make 64 'a') (Cstruct.to_string buf));
  Alcotest.(check bool) "closed" true (collected weak 0)

(* A removed directory takes no new entry, even through an open handle. *)
let test_removed_directory () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "f") "x";
  Eio.Switch.run @@ fun sw ->
  let a = Eio.Path.open_dir ~sw (fs / "a") in
  Eio.Path.rmdir (fs / "a");
  expect_io_error "not found" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o644) (a / "f") "y");
  expect_io_error "not found" (fun () -> Eio.Path.mkdir ~perm:0o755 (a / "d"));
  expect_io_error "not found" (fun () ->
      Eio.Path.symlink ~link_to:"t" (a / "l"));
  expect_io_error "not found" (fun () -> Eio.Path.rename (fs / "f") (a / "f"));
  Alcotest.(check (list string)) "empty" [] (Eio.Path.read_dir a);
  Alcotest.(check string) "f stayed" "x" (Eio.Path.load (fs / "f"))

(* The current directory is a directory, not a path: it follows a rename of
   its directory, and a link planted at its old path changes nothing. *)
let test_cwd_follows_its_directory () =
  with_env ~cwd:"/a" @@ fun env ->
  let fs = env#fs and cwd = env#cwd in
  Eio.Path.save ~create:(`Exclusive 0o644) (cwd / "f") "mine";
  Eio.Path.rename (fs / "/a") (fs / "/b");
  Alcotest.(check string) "after the rename" "mine" (Eio.Path.load (cwd / "f"));
  Eio.Path.mkdir ~perm:0o755 (fs / "/c");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/c/f") "planted";
  Eio.Path.symlink ~link_to:"/c" (fs / "/a");
  Alcotest.(check string)
    "past a planted link" "mine"
    (Eio.Path.load (cwd / "f"));
  Alcotest.(check string)
    "relative through fs" "mine"
    (Eio.Path.load (fs / "f"))

(* lstat(2) gives a symbolic link the length of its target as its size. *)
let test_symlink_size () =
  with_env @@ fun env ->
  Eio.Path.symlink ~link_to:"hello" (env#fs / "l");
  Alcotest.(check int)
    "size" 5
    (Optint.Int63.to_int (Eio.Path.stat ~follow:false (env#fs / "l")).size)

(* lseek(2) on a directory descriptor answers 0, as eio_posix does. *)
let test_seek_directory_fd () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  Eio.Path.with_open_in (env#fs / "d") @@ fun f ->
  Alcotest.(check int)
    "offset" 0
    (Optint.Int63.to_int (Eio.File.seek f Optint.Int63.zero `Set))

(* A name holds at most 255 bytes (ENAMETOOLONG) and no NUL (ENOENT). *)
let test_name_limits () =
  with_env @@ fun env ->
  let fs = env#fs in
  let long = String.make 256 'n' in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / String.make 255 'n') "ok";
  expect_eio_io "a 256-byte name" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o644) (fs / long) "x");
  expect_eio_io "a 256-byte directory" (fun () ->
      Eio.Path.mkdir ~perm:0o755 (fs / long));
  expect_eio_io "a 256-byte component" (fun () ->
      ignore (Eio.Path.load (fs / long / "f") : string));
  expect_io_error "not found" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o644) (fs / "a\000b") "x");
  expect_io_error "not found" (fun () ->
      Eio.Path.mkdir ~perm:0o755 (fs / "a\000b"));
  Alcotest.(check int) "one entry" 1 (List.length (Eio.Path.read_dir fs))

(* A last [..] at a confined directory's base names its parent, outside it. *)
let test_dotdot_at_base () =
  with_env ~cwd:"/d" @@ fun env ->
  let cwd = env#cwd in
  Eio.Path.mkdir ~perm:0o755 (cwd / "a");
  expect_io_error "permission denied" (fun () -> Eio.Path.rmdir (cwd / ".."));
  expect_io_error "permission denied" (fun () ->
      Eio.Path.rename (cwd / "a") (cwd / ".."));
  expect_io_error "permission denied" (fun () ->
      Eio.Path.rename (cwd / "..") (cwd / "a/a"))

(* pwrite(2) on an O_APPEND descriptor appends on Linux, whatever the
   offset. *)
let test_pwrite_appends () =
  with_env @@ fun env ->
  let p = env#fs / "f" in
  Eio.Path.save ~create:(`Exclusive 0o644) p "abc";
  Eio.Path.with_open_out ~append:true ~create:`Never p (fun f ->
      Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero
        [ Cstruct.of_string "Z" ]);
  Alcotest.(check string) "appended" "abcZ" (Eio.Path.load p)

(* So does the crash model. *)
let test_crash_pwrite_appends () =
  with_env @@ fun env ->
  let module Crash = Eio_fs.Chaos.Private.Crash in
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  let fs = Crash.wrap ctl env#fs in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "f") "abc";
  Crash.sync_metadata ctl;
  Eio.Path.with_open_out ~append:true ~create:`Never (fs / "f") (fun f ->
      Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero
        [ Cstruct.of_string "Z" ]);
  Alcotest.(check string)
    "replayed at the end" "abcZ"
    (Eio.Path.load (Crash.post_crash ctl / "f"))

(* Linux refuses chmod(2) through AT_SYMLINK_NOFOLLOW on a link
   (EOPNOTSUPP) and symlink(2) to the empty path (ENOENT). *)
let test_linux_link_refusals () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "f") "x";
  Eio.Path.symlink ~link_to:"f" (fs / "l");
  expect_eio_io "chmod a link" (fun () ->
      Eio.Path.chmod ~follow:false ~perm:0o600 (fs / "l"));
  Alcotest.(check int)
    "the target kept its mode" 0o644
    (Eio.Path.stat ~follow:true (fs / "l")).perm;
  expect_io_error "not found" (fun () ->
      Eio.Path.symlink ~link_to:"" (fs / "e"));
  Alcotest.(check bool)
    "no link made" true
    (Eio.Path.kind ~follow:false (fs / "e") = `Not_found)

(* {2 Permissions} *)

let bob = { alice with uid = 1001; gid = 1001 }

let denied what fn =
  expect_io_error "permission denied" fn |> fun () -> ignore what

let perm p = (Eio.Path.stat ~follow:false p).perm

(* Every directory a path walks needs its search bit; root needs none. *)
let test_perm_search () =
  with_env ~user:alice ~cwd:"/home/alice" @@ fun env ->
  let d = env#cwd / "d" in
  Eio.Path.mkdir ~perm:0o755 d;
  Eio.Path.save ~create:(`Exclusive 0o644) (d / "f") "x";
  Eio.Path.chmod ~follow:false ~perm:0o600 d;
  denied "load" (fun () -> ignore (Eio.Path.load (d / "f") : string));
  denied "stat" (fun () -> ignore (Eio.Path.stat ~follow:false (d / "f")));
  Eio.Path.chmod ~follow:false ~perm:0o000 d;
  let as_root = Eio_fs.Fs.as_user Eio_fs.root env#cwd in
  Alcotest.(check string) "root" "x" (Eio.Path.load (as_root / "d" / "f"))

(* Making, removing or renaming an entry needs write and search on its
   directory. *)
let test_perm_dir_write () =
  with_env ~user:alice ~cwd:"/home/alice" @@ fun env ->
  let d = env#cwd / "d" in
  Eio.Path.mkdir ~perm:0o755 d;
  Eio.Path.save ~create:(`Exclusive 0o644) (d / "f") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (env#cwd / "g") "y";
  Eio.Path.chmod ~follow:false ~perm:0o500 d;
  denied "create" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o644) (d / "new") "z");
  denied "mkdir" (fun () -> Eio.Path.mkdir ~perm:0o755 (d / "e"));
  denied "symlink" (fun () -> Eio.Path.symlink ~link_to:"f" (d / "l"));
  denied "unlink" (fun () -> Eio.Path.unlink (d / "f"));
  denied "rename out" (fun () -> Eio.Path.rename (d / "f") (env#cwd / "h"));
  denied "rename in" (fun () -> Eio.Path.rename (env#cwd / "g") (d / "g"));
  Eio.Path.save ~create:`Never (d / "f") "w";
  Alcotest.(check string)
    "a file in it is still writable" "w"
    (Eio.Path.load (d / "f"))

(* In a sticky directory a user removes or renames only their own entries. *)
let test_perm_sticky () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o777 (fs / "tmp");
  Eio.Path.chmod ~follow:false ~perm:0o1777 (fs / "tmp");
  let as_alice = Eio_fs.Fs.as_user alice fs in
  let as_bob = Eio_fs.Fs.as_user bob fs in
  Eio.Path.save ~create:(`Exclusive 0o666) (as_bob / "tmp" / "b") "bob's";
  Eio.Path.save ~create:(`Exclusive 0o666) (as_alice / "tmp" / "a") "alice's";
  denied "unlink" (fun () -> Eio.Path.unlink (as_alice / "tmp" / "b"));
  denied "rename" (fun () ->
      Eio.Path.rename (as_alice / "tmp" / "b") (as_alice / "tmp" / "c"));
  denied "replace" (fun () ->
      Eio.Path.rename (as_alice / "tmp" / "a") (as_alice / "tmp" / "b"));
  Eio.Path.unlink (as_alice / "tmp" / "a");
  Eio.Path.unlink (fs / "tmp" / "b");
  Alcotest.(check (list string)) "empty" [] (Eio.Path.read_dir (fs / "tmp"))

(* Listing a directory needs its read bit; looking a name up does not. *)
let test_perm_read_dir () =
  with_env ~user:alice ~cwd:"/home/alice" @@ fun env ->
  let d = env#cwd / "d" in
  Eio.Path.mkdir ~perm:0o755 d;
  Eio.Path.save ~create:(`Exclusive 0o644) (d / "f") "x";
  Eio.Path.chmod ~follow:false ~perm:0o300 d;
  denied "read_dir" (fun () -> ignore (Eio.Path.read_dir d : string list));
  Alcotest.(check string) "lookup" "x" (Eio.Path.load (d / "f"))

(* Opening a file reads the owner's, the group's or the others' bits. *)
let test_perm_open_classes () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o640) (fs / "f") "x";
  let carol = { bob with uid = 1002; groups = [ 0 ] } in
  let as_carol = Eio_fs.Fs.as_user carol fs in
  Alcotest.(check string) "group reads" "x" (Eio.Path.load (as_carol / "f"));
  denied "group writes" (fun () ->
      Eio.Path.save ~create:`Never (as_carol / "f") "y");
  denied "others read" (fun () ->
      ignore (Eio.Path.load (Eio_fs.Fs.as_user alice fs / "f") : string))

(* A new entry loses the umask's bits and belongs to its maker. *)
let test_perm_umask_and_owner () =
  with_env ~user:alice ~cwd:"/home/alice" @@ fun env ->
  Eio.Path.mkdir ~perm:0o777 (env#cwd / "d");
  Eio.Path.save ~create:(`Exclusive 0o666) (env#cwd / "f") "x";
  Alcotest.(check int) "directory" 0o755 (perm (env#cwd / "d"));
  Alcotest.(check int) "file" 0o644 (perm (env#cwd / "f"));
  let stat = Eio.Path.stat ~follow:false (env#cwd / "f") in
  Alcotest.(check (pair int64 int64)) "owner" (1000L, 1000L) (stat.uid, stat.gid);
  let strict = Eio_fs.Fs.as_user { alice with umask = 0o077 } env#cwd in
  Eio.Path.save ~create:(`Exclusive 0o666) (strict / "g") "y";
  Alcotest.(check int) "umask 0o077" 0o600 (perm (env#cwd / "g"))

(* Only a file's owner changes its mode; only root gives it away. *)
let test_perm_chmod_chown () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "root's") "x";
  let as_alice = Eio_fs.Fs.as_user alice fs in
  denied "chmod" (fun () ->
      Eio.Path.chmod ~follow:false ~perm:0o666 (as_alice / "root's"));
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (fs / "root's");
  Eio.Path.chmod ~follow:false ~perm:0o600 (as_alice / "root's");
  denied "chown" (fun () ->
      Eio.Path.chown ~follow:false ~uid:1001L (as_alice / "root's"));
  Alcotest.(check int) "mode" 0o600 (perm (fs / "root's"))

(* The same workload as root and as a user: a config directory under a tree
   the user does not own. *)
let test_perm_root_and_user_differ () =
  let workload user =
    with_env ~user ~cwd:"/home/alice" @@ fun env ->
    match
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (env#fs / "/etc/app");
      Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "/etc/app/conf") "x"
    with
    | () -> "saved"
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> "denied"
  in
  Alcotest.(check string) "root" "saved" (workload Eio_fs.root);
  Alcotest.(check string) "a user" "denied" (workload alice)

(* A copied tree keeps its modes, whatever the umask. *)
let test_perm_load_keeps_modes () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o777 (env#fs / "d");
  Eio.Path.chmod ~follow:false ~perm:0o777 (env#fs / "d");
  Eio.Path.save ~create:(`Exclusive 0o666) (env#fs / "d/f") "x";
  Eio.Path.chmod ~follow:false ~perm:0o666 (env#fs / "d/f");
  let copy = Eio_fs.Fs.load (env#fs / "d") in
  Alcotest.(check int) "directory" 0o777 (perm (copy / "d"));
  Alcotest.(check int) "file" 0o666 (perm (copy / "d/f"))

(* chmod(2) keeps the permission bits of the mode only. *)
let test_chmod_masks_the_mode () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "f") "x";
  Eio.Path.chmod ~follow:true ~perm:0o100644 (env#fs / "f");
  Alcotest.(check int) "mode" 0o644 (perm (env#fs / "f"))

(* A symbolic link's mode is 0o777, as Linux lstat(2) reports it. *)
let test_symlink_mode () =
  with_env @@ fun env ->
  Eio.Path.symlink ~link_to:"f" (env#fs / "l");
  Alcotest.(check int) "mode" 0o777 (perm (env#fs / "l"))

(* Moving a directory to another parent rewrites its [..] entry, so a user
   needs write permission on the directory itself. *)
let test_perm_move_directory () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o777 (fs / "pub");
  Eio.Path.chmod ~follow:false ~perm:0o777 (fs / "pub");
  Eio.Path.mkdir ~perm:0o755 (fs / "pub/rootdir");
  let a = Eio_fs.Fs.as_user alice fs in
  Eio.Path.mkdir ~perm:0o777 (a / "pub/other");
  denied "move to another parent" (fun () ->
      Eio.Path.rename (a / "pub/rootdir") (a / "pub/other/x"));
  Eio.Path.rename (a / "pub/rootdir") (a / "pub/same");
  Alcotest.(check bool)
    "renamed in place" true
    (Eio.Path.is_directory (fs / "pub/same"))

(* Opening a directory to read it needs its read bit; open_dir does not, as
   eio_posix opens it O_PATH. *)
let test_perm_open_directory () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o711 (fs / "d");
  let a = Eio_fs.Fs.as_user alice fs in
  denied "open_in" (fun () ->
      Eio.Switch.run (fun sw -> ignore (Eio.Path.open_in ~sw (a / "d"))));
  Eio.Switch.run (fun sw -> ignore (Eio.Path.open_dir ~sw (a / "d")))

(* The errno Linux answers, by the exception eio_posix raises for it. *)
let errno f =
  let contains s sub =
    let n = String.length s and m = String.length sub in
    let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
    go 0
  in
  match f () with
  | _ -> "ok"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> "EACCES"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> "ENOENT"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> "EEXIST"
  | exception (Eio.Io _ as e) ->
      let s = Printexc.to_string e in
      List.fold_left
        (fun acc (sub, code) ->
          if acc = "?" && contains s sub then code else acc)
        "?"
        [
          ("directory not empty", "ENOTEMPTY");
          ("is a directory", "EISDIR");
          ("not a directory", "ENOTDIR");
          ("invalid argument", "EINVAL");
          ("resource busy", "EBUSY");
        ]

(* A tree [w] alice owns, with [w/p/f] and [w/q/f], and [w/nx] she cannot
   search. *)
let linux_tree () =
  let fs = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o777 (fs / "w");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (fs / "w");
  let a = Eio_fs.Fs.as_user alice fs in
  Eio.Path.mkdir ~perm:0o755 (a / "w/nx");
  Eio.Path.chmod ~follow:false ~perm:0o600 (a / "w/nx");
  Eio.Path.mkdir ~perm:0o755 (a / "w/p");
  Eio.Path.save ~create:(`Exclusive 0o644) (a / "w/p/f") "x";
  Eio.Path.mkdir ~perm:0o755 (a / "w/q");
  Eio.Path.save ~create:(`Exclusive 0o644) (a / "w/q/f") "x";
  (fs, a)

(* As Linux answers, checked against eio_posix: mkdir keeps the permission
   and sticky bits of its mode only, and a new directory is setgid iff its
   parent is. *)
let test_linux_mkdir_modes () =
  Eio_mock.Backend.run @@ fun () ->
  let _, a = linux_tree () in
  let mode p = Fmt.str "%o" (perm (a / p)) in
  let mkdir p m =
    Eio.Path.mkdir ~perm:m (a / p);
    mode p
  in
  Alcotest.(check (list string))
    "mkdir in a plain parent" [ "755"; "755"; "1755" ]
    [ mkdir "w/g" 0o2755; mkdir "w/u" 0o4755; mkdir "w/t" 0o1777 ];
  Eio.Path.chmod ~follow:false ~perm:0o2755 (a / "w/g");
  Alcotest.(check (list string))
    "mkdir in a setgid parent"
    [ "2755"; "2755"; "2755"; "2700" ]
    [
      mkdir "w/g/a" 0o755;
      mkdir "w/g/b" 0o2755;
      mkdir "w/g/c" 0o6777;
      mkdir "w/g/d" 0o4700;
    ]

(* rename needs search permission on both parents before its source must
   exist. *)
let test_linux_rename_search_first () =
  Eio_mock.Backend.run @@ fun () ->
  let _, a = linux_tree () in
  Alcotest.(check string)
    "rename" "EACCES"
    (errno (fun () -> Eio.Path.rename (a / "w/missing") (a / "w/nx/y")))

(* rename refuses a target above its source with ENOTEMPTY, before it
   compares kinds. *)
let test_linux_rename_above () =
  Eio_mock.Backend.run @@ fun () ->
  let _, a = linux_tree () in
  Alcotest.(check string)
    "rename" "ENOTEMPTY"
    (errno (fun () -> Eio.Path.rename (a / "w/p/f") (a / "w/p")))

(* readlink of anything but a link is EINVAL. *)
let test_linux_readlink () =
  Eio_mock.Backend.run @@ fun () ->
  let _, a = linux_tree () in
  Alcotest.(check (list string))
    "readlink" [ "EINVAL"; "EINVAL" ]
    [
      errno (fun () -> Eio.Path.read_link (a / "w/p/f"));
      errno (fun () -> Eio.Path.read_link (a / "w/p"));
    ]

(* A read of a directory is EISDIR, and so is a file over a directory. *)
let test_linux_eisdir () =
  Eio_mock.Backend.run @@ fun () ->
  let _, a = linux_tree () in
  Alcotest.(check (list string))
    "read, rename" [ "EISDIR"; "EISDIR" ]
    [
      errno (fun () -> Eio.Path.load (a / "w/p"));
      errno (fun () -> Eio.Path.rename (a / "w/p/f") (a / "w/q"));
    ]

(* rename answers EBUSY for a last [.] or [..], or EEXIST for the target's
   when it must not replace it. *)
let test_linux_rename_dots () =
  Eio_mock.Backend.run @@ fun () ->
  let _, a = linux_tree () in
  let rename x y () = Eio.Path.rename (a / x) (a / y) in
  let exclusive x y () =
    Eio_fs_sync.Rename.exclusive Eio_fs_sync.Rename.capability (a / x) (a / y)
  in
  Alcotest.(check (list string))
    "rename"
    [ "EBUSY"; "EBUSY"; "EBUSY"; "EBUSY"; "EEXIST"; "EBUSY" ]
    (List.map errno
       [
         rename "w/p/f" "w/.";
         rename "w/." "w/z";
         rename "w/p/f" "w/q/..";
         rename "w/p/." "w/z";
         exclusive "w/p/f" "w/.";
         exclusive "w/p/." "w/z";
       ])

(* In a directory the user cannot write, Linux checks that permission
   before the kinds: EACCES. *)
let test_linux_permission_first () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o755 (fs / "ro");
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (fs / "ro" / d)) [ "d"; "e" ];
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "ro/e/x") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "ro/f") "f";
  Eio.Path.symlink ~link_to:"d" (fs / "ro/l");
  let a = Eio_fs.Fs.as_user alice fs in
  Alcotest.(check (list string))
    "errors"
    [ "EACCES"; "EACCES"; "EACCES"; "EACCES"; "EACCES"; "EACCES" ]
    (List.map errno
       [
         (fun () -> Eio.Path.rmdir (a / "ro/f"));
         (fun () -> Eio.Path.rmdir (a / "ro/l"));
         (fun () -> Eio.Path.rename (a / "ro/d") (a / "ro/e"));
         (fun () -> Eio.Path.rename (a / "ro/f") (a / "ro/d"));
         (fun () -> Eio.Path.rename (a / "ro/d") (a / "ro/f"));
         (fun () -> Eio.Path.unlink (a / "ro/d"));
       ])

(* Linux: a setgid directory gives new entries its group, and new
   subdirectories its setgid bit; a chown, or a write by a user, clears the
   setuid and setgid bits of a file; a chmod by a user outside the file's
   group drops setgid. *)
let test_perm_setgid_and_privileges () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  let carol = { alice with groups = [ 50 ] } in
  let a = Eio_fs.Fs.as_user carol fs in
  let owner p =
    let s = Eio.Path.stat ~follow:false (fs / p) in
    Fmt.str "%Ld:%Ld %o" s.uid s.gid s.perm
  in
  let check what expected p = Alcotest.(check string) what expected (owner p) in
  Eio.Path.mkdir ~perm:0o755 (fs / "g");
  Eio.Path.chown ~follow:false ~gid:50L (fs / "g");
  Eio.Path.chmod ~follow:false ~perm:0o2777 (fs / "g");
  Eio.Path.save ~create:(`Exclusive 0o644) (a / "g/f") "";
  Eio.Path.mkdir ~perm:0o755 (a / "g/d");
  check "a file in a setgid directory" "1000:50 644" "g/f";
  check "a directory in a setgid directory" "1000:50 2755" "g/d";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "sx") "";
  Eio.Path.chmod ~follow:false ~perm:0o6755 (fs / "sx");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (fs / "sx");
  check "chown clears setuid and setgid" "1000:1000 755" "sx";
  Eio.Path.chmod ~follow:false ~perm:0o6755 (a / "sx");
  Eio.Path.with_open_out ~append:true ~create:`Never (a / "sx") (fun f ->
      Eio.Flow.copy_string "x" f);
  check "a user's write clears them" "1000:1000 755" "sx";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "sg") "";
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:60L (fs / "sg");
  Eio.Path.chmod ~follow:false ~perm:0o2755 (a / "sg");
  check "chmod outside the group drops setgid" "1000:60 755" "sg"

(* A directory barrier opens the directory read-only first, as fsync(2) on
   Linux needs: a missing one is Not_found, an unreadable one
   Permission_denied, through the crash model too. *)
let test_sync_dir_checks () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o311 (fs / "x");
  let a = Eio_fs.Fs.as_user alice fs in
  let crash =
    Eio_fs.Chaos.Private.Crash.wrap (Eio_fs.Chaos.Private.Crash.v ()) a
  in
  List.iter
    (fun (what, dir) ->
      expect_io_error "not found" (fun () ->
          Eio_fs_sync.sync_dir (dir / "missing"));
      denied what (fun () -> Eio_fs_sync.sync_dir (dir / "x")))
    [ ("in memory", a); ("through the crash model", crash) ]

(* A file a user creates in a setgid directory of a group the user is not
   in loses setgid, as Linux strips it at create; a member keeps it. *)
let test_perm_setgid_stripped_outside_group () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o777 (fs / "g");
  Eio.Path.chown ~follow:false ~uid:0L ~gid:2000L (fs / "g");
  Eio.Path.chmod ~follow:false ~perm:0o2777 (fs / "g");
  let make user name =
    let d = Eio_fs.Fs.as_user user fs in
    Eio.Path.with_open_out ~create:(`Exclusive 0o2755) (d / "g" / name) ignore;
    let st = Eio.Path.stat ~follow:false (fs / "g" / name) in
    (st.perm, st.gid)
  in
  let member = { alice with groups = [ 2000 ] } in
  Alcotest.(check (pair (pair int int64) (pair int int64)))
    "outsider, member"
    ((0o755, 2000L), (0o2755, 2000L))
    (make { alice with umask = 0 } "a", make { member with umask = 0 } "b")

(* {2 The file offset belongs to the open file description}

   open(2) makes a new open file description, with its own offset and its own
   O_APPEND flag: opening a file a second time moves no other descriptor's
   offset, and every write through an O_APPEND descriptor lands at the end of
   the file, wherever its offset was. *)

let read_exactly flow n =
  let buf = Cstruct.create n in
  Eio.Flow.read_exact flow buf;
  Cstruct.to_string buf

let test_second_open_keeps_offset () =
  with_env @@ fun env ->
  let path = env#fs / "f" in
  Eio.Path.save ~create:(`Exclusive 0o644) path "abcdef";
  Eio.Switch.run @@ fun sw ->
  let w = Eio.Path.open_out ~sw ~create:`Never path in
  Eio.Flow.copy_string "AB" w;
  let r = Eio.Path.open_in ~sw path in
  Alcotest.(check string) "first read" "AB" (read_exactly r 2);
  let _other = Eio.Path.open_in ~sw path in
  Eio.Flow.copy_string "CD" w;
  Alcotest.(check string) "the reader kept its offset" "CD" (read_exactly r 2);
  Alcotest.(check string)
    "the writer kept its offset" "ABCDef" (Eio.Path.load path)

let test_append_writes_at_end () =
  with_env @@ fun env ->
  let path = env#fs / "log" in
  Eio.Path.save ~create:(`Exclusive 0o644) path "abc";
  Eio.Switch.run @@ fun sw ->
  let a = Eio.Path.open_out ~sw ~append:true ~create:`Never path in
  let _other = Eio.Path.open_in ~sw path in
  Eio.Flow.copy_string "def" a;
  ignore (Eio.File.seek a (Optint.Int63.of_int 0) `Set : Optint.Int63.t);
  Eio.Flow.copy_string "ghi" a;
  let w = Eio.Path.open_out ~sw ~create:`Never path in
  Eio.Flow.copy_string "X" w;
  Eio.Flow.copy_string "jkl" a;
  Alcotest.(check string)
    "every append landed at the end" "Xbcdefghijkl" (Eio.Path.load path)

(* writev(2) writes its buffers in order as one contiguous run, at the end of
   the file as well as inside it, and answers the bytes written. *)
let test_vectored_write () =
  with_env @@ fun env ->
  let path = env#fs / "v" in
  let bufs = List.map Cstruct.of_string [ "hello"; " "; "world" ] in
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) path (fun f ->
      let n = Eio.Flow.single_write f bufs in
      Alcotest.(check int) "bytes written" 11 n);
  Alcotest.(check string) "every buffer" "hello world" (Eio.Path.load path);
  Eio.Path.with_open_out ~create:`Never path (fun f ->
      let n =
        Eio.Flow.single_write f (List.map Cstruct.of_string [ "HE"; "LL" ])
      in
      Alcotest.(check int) "bytes overwritten" 4 n);
  Alcotest.(check string) "inside the file" "HELLo world" (Eio.Path.load path)

(* A descriptor opened by one fiber while another is in the middle of a read
   stays open: a read never writes back a descriptor table it read before
   letting the other fiber run. *)
let test_open_during_read () =
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "src") "data";
  Eio.Switch.run @@ fun sw ->
  let src = Eio.Path.open_in ~sw (env#fs / "src") in
  Eio.Fiber.both
    (fun () -> ignore (read_exactly src 4 : string))
    (fun () ->
      let dst =
        Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (env#fs / "dst")
      in
      Eio.Fiber.yield ();
      Eio.Flow.copy_string "x" dst);
  Alcotest.(check string)
    "the write went through" "x"
    (Eio.Path.load (env#fs / "dst"))

(* One file system may be shared by fibers of several domains: entries two
   domains create at once in one directory are all there afterwards. *)
let test_shared_across_domains () =
  with_env @@ fun env ->
  let n = 300 in
  let create prefix () =
    Eio_mock.Backend.run @@ fun () ->
    for i = 1 to n do
      Eio.Path.mkdir ~perm:0o755 (env#fs / Fmt.str "%s%d" prefix i)
    done
  in
  let other = Domain.spawn (create "a") in
  create "b" ();
  Domain.join other;
  Alcotest.(check int)
    "every entry" (2 * n)
    (List.length (Eio.Path.read_dir env#fs))

(* path_resolution(7): resolution follows at most 40 symbolic links, as
   eio_posix does, and then fails with ELOOP, which Eio raises as
   [Eio.Fs.Symlink]. *)
let link_chain fs n =
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "target") "end";
  for i = 1 to n do
    let link_to = if i = 1 then "target" else Fmt.str "l%d" (i - 1) in
    Eio.Path.symlink ~link_to (fs / Fmt.str "l%d" i)
  done;
  fs / Fmt.str "l%d" n

let test_symlink_chain_limit () =
  with_env @@ fun env ->
  let last = link_chain env#fs 41 in
  Alcotest.(check string)
    "forty links resolve" "end"
    (Eio.Path.load (env#fs / "l40"));
  expect_io_error "too many symbolic links" (fun () ->
      ignore (Eio.Path.load last : string))

(* A loop never resolves: a link to itself and two links to each other fail
   with ELOOP, as the last component and as a directory on the way. *)
let test_symlink_loop () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.symlink ~link_to:"self" (fs / "self");
  Eio.Path.symlink ~link_to:"b" (fs / "a");
  Eio.Path.symlink ~link_to:"a" (fs / "b");
  List.iter
    (fun path ->
      expect_io_error "too many symbolic links" (fun () ->
          ignore (Eio.Path.load (fs / path) : string)))
    [ "self"; "a"; "self/x"; "a/x" ]

(* {2 Creating over a dangling symbolic link}

   A symbolic link whose target is missing still occupies its name. open(2)
   with O_CREAT | O_EXCL, mkdir(2) and symlink(2) fail with EEXIST on it;
   O_CREAT alone follows it and creates the target; O_NOFOLLOW refuses it with
   ELOOP. None of them adds a second entry of that name. *)

let names dir = List.sort String.compare (Eio.Path.read_dir dir)

let test_create_over_dangling_link () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.symlink ~link_to:"missing" (fs / "l");
  expect_io_error "already exists" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o644) (fs / "l") "x");
  expect_io_error "already exists" (fun () ->
      Eio.Path.mkdir ~perm:0o755 (fs / "l"));
  expect_io_error "already exists" (fun () ->
      Eio.Path.symlink ~link_to:"other" (fs / "l"));
  expect_io_error "too many symbolic links" (fun () ->
      Eio.Path.with_open_out ~follow:false ~create:(`If_missing 0o644)
        (fs / "l") ignore);
  Alcotest.(check (list string)) "only the link" [ "l" ] (names fs);
  Eio.Path.save ~create:(`If_missing 0o644) (fs / "l") "x";
  Alcotest.(check (list string))
    "the target was created" [ "l"; "missing" ] (names fs);
  Alcotest.(check string) "through the link" "x" (Eio.Path.load (fs / "l"));
  Alcotest.(check string)
    "the link is still a link" "missing"
    (Eio.Path.read_link (fs / "l"))

(* {2 rename(2) between kinds}

   rename(2) refuses to move a directory into its own subtree (EINVAL), a
   directory over a non-empty directory (ENOTEMPTY), a non-directory over a
   directory (EISDIR) and a directory over a non-directory (ENOTDIR), and
   leaves both names as they were. A directory over an empty directory
   replaces it. *)

let kind fs path = Eio.Path.kind ~follow:false (fs / path)

let test_rename_refusals () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdirs ~perm:0o755 (fs / "a/b");
  Eio.Path.mkdirs ~perm:0o755 (fs / "full/z");
  Eio.Path.mkdir ~perm:0o755 (fs / "empty");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "f") "f";
  let refused what src dst =
    expect_eio_io what (fun () -> Eio.Path.rename (fs / src) (fs / dst));
    Alcotest.(check bool)
      (what ^ ": the source stays")
      true
      (kind fs src <> `Not_found)
  in
  refused "into its own subtree" "a" "a/b/c";
  refused "onto itself below" "a" "a/b";
  refused "over a non-empty directory" "a" "full";
  refused "a file over a directory" "f" "empty";
  refused "a directory over a file" "a" "f";
  Alcotest.(check bool) "the subtree is intact" true (kind fs "a/b" = `Directory);
  Alcotest.(check string) "the file is intact" "f" (Eio.Path.load (fs / "f"));
  Eio.Path.rename (fs / "a") (fs / "empty");
  Alcotest.(check bool)
    "a directory replaces an empty one" true
    (kind fs "empty/b" = `Directory && kind fs "a" = `Not_found)

(* A moved directory's parent is its new parent: a path resolved inside it
   from a capability confined to the new parent stays inside that parent. *)
let test_rename_moves_parent_link () =
  with_env ~cwd:"/q" @@ fun env ->
  Eio.Path.mkdirs ~perm:0o755 (env#fs / "/p/m");
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (env#fs / "/q");
  Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "/p/m/f") "x";
  Eio.Path.rename (env#fs / "/p/m") (env#fs / "/q/m");
  Alcotest.(check string)
    "reached from the new parent" "x"
    (Eio.Path.load (env#cwd / "m/f"))

(* unlink(2) of a missing name is ENOENT, and of a directory EISDIR on Linux
   (EPERM on macOS): the directory stays. A symbolic link to a directory on
   the way is followed, as for any parent. *)
let test_unlink () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "d/f") "f";
  Eio.Path.symlink ~link_to:"d" (fs / "ld");
  expect_io_error "not found" (fun () -> Eio.Path.unlink (fs / "missing"));
  expect_eio_io "unlinking a directory" (fun () -> Eio.Path.unlink (fs / "d"));
  Alcotest.(check bool)
    "the directory stays" true
    (Eio.Path.kind ~follow:false (fs / "d") = `Directory);
  Eio.Path.unlink (fs / "ld/f");
  Alcotest.(check (list string)) "through a linked parent" [] (names (fs / "d"))

(* rmdir(2) does not follow a final symbolic link: on a link to a directory
   it fails with ENOTDIR and removes neither. *)
let test_rmdir_symlink () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.symlink ~link_to:"d" (fs / "ld");
  expect_eio_io "rmdir on a symbolic link" (fun () ->
      Eio.Path.rmdir (fs / "ld"));
  Alcotest.(check (list string)) "both stay" [ "d"; "ld" ] (names fs)

(* An absolute symbolic link resolves from the root of the file system, as
   the last component and as a directory on the way, not from the directory
   holding the link. *)
let test_absolute_symlink () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdirs ~perm:0o755 (fs / "/d/e");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/target") "root";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/d/target") "d";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/d/e/f") "e";
  Eio.Path.symlink ~link_to:"/target" (fs / "/d/l");
  Eio.Path.symlink ~link_to:"/d/e" (fs / "/d/e/le");
  Alcotest.(check string)
    "the last component" "root"
    (Eio.Path.load (fs / "/d/l"));
  Alcotest.(check string) "on the way" "e" (Eio.Path.load (fs / "/d/e/le/f"))

(* open(2) with O_DIRECTORY fails with ENOTDIR on anything but a directory,
   which [Eio.Path.open_dir] asks for; a symbolic link to a directory is
   followed. *)
let test_open_dir_not_a_directory () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "f") "f";
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.symlink ~link_to:"d" (fs / "ld");
  expect_eio_io "opening a file as a directory" (fun () ->
      Eio.Switch.run @@ fun sw -> ignore (Eio.Path.open_dir ~sw (fs / "f")));
  Eio.Switch.run @@ fun sw ->
  let d = Eio.Path.open_dir ~sw (fs / "ld") in
  Alcotest.(check (list string)) "through a link" [] (Eio.Path.read_dir d)

(* lseek(2) to a negative offset fails with EINVAL and leaves the offset;
   one past the end is allowed, and a read there is at end of file. *)
let test_seek_bounds () =
  with_env @@ fun env ->
  let path = env#fs / "f" in
  Eio.Path.save ~create:(`Exclusive 0o644) path "abc";
  Eio.Path.with_open_in path @@ fun f ->
  ignore (Eio.File.seek f (Optint.Int63.of_int 1) `Set : Optint.Int63.t);
  expect_eio_io "a negative offset" (fun () ->
      ignore (Eio.File.seek f (Optint.Int63.of_int (-1)) `Set : Optint.Int63.t));
  expect_eio_io "before the start" (fun () ->
      ignore (Eio.File.seek f (Optint.Int63.of_int (-2)) `Cur : Optint.Int63.t));
  Alcotest.(check string) "the offset stayed" "bc" (read_exactly f 2);
  ignore (Eio.File.seek f (Optint.Int63.of_int 10) `Set : Optint.Int63.t);
  match Eio.Flow.single_read f (Cstruct.create 4) with
  | n -> Alcotest.failf "read %d bytes past the end" n
  | exception End_of_file -> ()

(* A confined capability, the current directory or an opened directory,
   refuses an absolute path as eio_posix does, rather than resolving it below
   itself. *)
let test_confined_absolute_path () =
  with_env ~cwd:"/d" @@ fun env ->
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (env#fs / "/d");
  Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "/d/f") "inner";
  Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "/f") "outer";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load (env#cwd / "/f") : string));
  Eio.Switch.run @@ fun sw ->
  let d = Eio.Path.open_dir ~sw (env#fs / "/d") in
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load Eio.Path.(d / "/f") : string))

(* A directory opened below the root reaches nothing above it, whatever links
   it holds. A link to an absolute path is refused, as eio_posix refuses one
   through a confined capability, and so is a [..] that climbs past the
   directory from inside a link's target, on the way or as the last
   component. *)
let test_confined_link_climb () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdirs ~perm:0o755 (fs / "/a/b/sub");
  Eio.Path.mkdirs ~perm:0o755 (fs / "/a/c");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/a/secret") "secret";
  Eio.Switch.run @@ fun sw ->
  let b = Eio.Path.open_dir ~sw (fs / "/a/b") in
  Eio.Path.symlink ~link_to:"/a/b/.." (b / "M");
  Eio.Path.symlink ~link_to:"/a/b/M" (b / "L");
  Eio.Path.symlink ~link_to:"../.." (b / "sub/U");
  Eio.Path.symlink ~link_to:"sub/U" (b / "V");
  List.iter
    (fun path ->
      match Eio.Path.read_dir (b / path) with
      | names ->
          Alcotest.failf "read_dir %S answered [%s]" path
            (String.concat "; " names)
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ())
    [ "M"; "L"; "L/c"; "sub/U"; "V"; "V/c" ];
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load (b / "L/secret") : string));
  expect_io_error "permission denied" (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o644) (b / "V/secret") "x");
  expect_io_error "permission denied" (fun () ->
      Eio.Switch.run @@ fun sw -> ignore (Eio.Path.open_dir ~sw (b / "L")));
  expect_io_error "permission denied" (fun () ->
      Eio.Switch.run @@ fun sw -> ignore (Eio.Path.open_dir ~sw (b / "V")));
  Alcotest.(check string)
    "the file above is untouched" "secret"
    (Eio.Path.load (fs / "/a/secret"))

(* A refusal names what the caller asked for, never where the capability sits
   in the file system: a holder of a directory learns nothing of the path
   above it from an error. *)
let test_refusal_hides_location () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdirs ~perm:0o755 (fs / "/hidden/place/d");
  Eio.Switch.run @@ fun sw ->
  let d = Eio.Path.open_dir ~sw (fs / "/hidden/place/d") in
  Eio.Path.symlink ~link_to:"/hidden" (d / "abs");
  List.iter
    (fun path ->
      match Eio.Path.stat ~follow:true (d / path) with
      | _ -> Alcotest.failf "%S was not refused" path
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied e), _) ->
          let message = Fmt.str "%a" Eio.Exn.Backend.pp e in
          let mentions word =
            let n = String.length word in
            let rec at i =
              i + n <= String.length message
              && (String.equal (String.sub message i n) word || at (i + 1))
            in
            at 0
          in
          Alcotest.(check bool)
            (Fmt.str "%S: %s names no directory above" path message)
            false
            (mentions "hidden" || mentions "place"))
    [ ".."; "../d"; "abs"; "abs/place" ]

(* path_resolution(7): a [..] after a symbolic link climbs from the link's
   target, not from the directory holding the link, through full access and
   through a confined directory alike. *)
let test_dotdot_after_link () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.mkdirs ~perm:0o755 (fs / "/d/e/f");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/x") "/x";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/d/x") "/d/x";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "/d/e/x") "/d/e/x";
  Eio.Path.symlink ~link_to:"d/e" (fs / "/s");
  Alcotest.(check string) "full access" "/d/x" (Eio.Path.load (fs / "/s/../x"));
  Eio.Switch.run @@ fun sw ->
  let d = Eio.Path.open_dir ~sw (fs / "/d") in
  Eio.Path.symlink ~link_to:"e/f" (d / "t");
  Alcotest.(check string) "confined" "/d/e/x" (Eio.Path.load (d / "t/../x"));
  Eio.Path.save ~create:(`Exclusive 0o644) (d / "t/../y") "made";
  Alcotest.(check string)
    "created beside the target" "made"
    (Eio.Path.load (fs / "/d/e/y"))

let suite =
  ( "low_level",
    [
      Alcotest.test_case "confined link climb" `Quick test_confined_link_climb;
      Alcotest.test_case "refusal hides the location" `Quick
        test_refusal_hides_location;
      Alcotest.test_case "dot-dot after a link" `Quick test_dotdot_after_link;
      Alcotest.test_case "confined absolute path" `Quick
        test_confined_absolute_path;
      Alcotest.test_case "seek bounds" `Quick test_seek_bounds;
      Alcotest.test_case "open_dir on a file" `Quick
        test_open_dir_not_a_directory;
      Alcotest.test_case "absolute symbolic link" `Quick test_absolute_symlink;
      Alcotest.test_case "rmdir on a symbolic link" `Quick test_rmdir_symlink;
      Alcotest.test_case "unlink" `Quick test_unlink;
      Alcotest.test_case "rename refusals" `Quick test_rename_refusals;
      Alcotest.test_case "rename moves the parent link" `Quick
        test_rename_moves_parent_link;
      Alcotest.test_case "create over a dangling link" `Quick
        test_create_over_dangling_link;
      Alcotest.test_case "symbolic link loop" `Quick test_symlink_loop;
      Alcotest.test_case "symbolic link chain limit" `Quick
        test_symlink_chain_limit;
      Alcotest.test_case "shared across domains" `Quick
        test_shared_across_domains;
      Alcotest.test_case "open during a read" `Quick test_open_during_read;
      Alcotest.test_case "vectored write" `Quick test_vectored_write;
      Alcotest.test_case "a second open keeps the offset" `Quick
        test_second_open_keeps_offset;
      Alcotest.test_case "append writes at the end" `Quick
        test_append_writes_at_end;
      Alcotest.test_case "missing file" `Quick test_missing_file;
      Alcotest.test_case "exclusive create" `Quick test_exclusive_create;
      Alcotest.test_case "cwd parent escape" `Quick test_cwd_parent_escape;
      Alcotest.test_case "cwd symlink escape" `Quick test_cwd_symlink_escape;
      Alcotest.test_case "create records mode" `Quick test_create_mode_recorded;
      Alcotest.test_case "read-only reopen denied" `Quick
        test_readonly_reopen_denied;
      Alcotest.test_case "unreadable reopen denied" `Quick
        test_unreadable_reopen_denied;
      Alcotest.test_case "fresh read-only create writable" `Quick
        test_readonly_fresh_create_writable;
      Alcotest.test_case "chmod restores writability" `Quick
        test_chmod_restores_writability;
      Alcotest.test_case "subtree fd dies with switch" `Quick
        test_subtree_fd_dies_with_switch;
      Alcotest.test_case "stale close spares a reused fd" `Quick
        test_stale_close_spares_reused_fd;
      Alcotest.test_case "closed handle write refused" `Quick
        test_closed_handle_write_refused;
      Alcotest.test_case "read directory fd" `Quick test_read_directory_fd;
      Alcotest.test_case "write directory fd" `Quick test_write_directory_fd;
      Alcotest.test_case "create under regular file" `Quick
        test_create_under_regular_file;
      Alcotest.test_case "create in directory syntax" `Quick
        test_create_directory_syntax;
      Alcotest.test_case "directory ops under a regular file" `Quick
        test_dir_ops_under_regular_file;
      Alcotest.test_case "trailing slash" `Quick test_trailing_slash;
      Alcotest.test_case "many small writes" `Quick test_many_small_writes;
      Alcotest.test_case "file too large" `Quick test_file_too_large;
      Alcotest.test_case "an unlinked file is freed" `Quick
        test_unlinked_file_freed;
      Alcotest.test_case "an open unlinked file is kept" `Quick
        test_unlinked_open_file_kept;
      Alcotest.test_case "a removed directory" `Quick test_removed_directory;
      Alcotest.test_case "cwd follows its directory" `Quick
        test_cwd_follows_its_directory;
      Alcotest.test_case "symbolic link size" `Quick test_symlink_size;
      Alcotest.test_case "seek a directory descriptor" `Quick
        test_seek_directory_fd;
      Alcotest.test_case "name limits" `Quick test_name_limits;
      Alcotest.test_case "dot-dot at the base" `Quick test_dotdot_at_base;
      Alcotest.test_case "pwrite appends" `Quick test_pwrite_appends;
      Alcotest.test_case "crash: pwrite appends" `Quick
        test_crash_pwrite_appends;
      Alcotest.test_case "Linux link refusals" `Quick test_linux_link_refusals;
      Alcotest.test_case "permissions: search" `Quick test_perm_search;
      Alcotest.test_case "permissions: directory write" `Quick
        test_perm_dir_write;
      Alcotest.test_case "permissions: sticky bit" `Quick test_perm_sticky;
      Alcotest.test_case "permissions: read a directory" `Quick
        test_perm_read_dir;
      Alcotest.test_case "permissions: owner, group, others" `Quick
        test_perm_open_classes;
      Alcotest.test_case "permissions: umask and owner" `Quick
        test_perm_umask_and_owner;
      Alcotest.test_case "permissions: chmod and chown" `Quick
        test_perm_chmod_chown;
      Alcotest.test_case "permissions: root and a user differ" `Quick
        test_perm_root_and_user_differ;
      Alcotest.test_case "permissions: setgid stripped outside its group" `Quick
        test_perm_setgid_stripped_outside_group;
      Alcotest.test_case "permissions: load keeps modes" `Quick
        test_perm_load_keeps_modes;
      Alcotest.test_case "chmod masks the mode" `Quick test_chmod_masks_the_mode;
      Alcotest.test_case "symbolic link mode" `Quick test_symlink_mode;
      Alcotest.test_case "permissions: move a directory" `Quick
        test_perm_move_directory;
      Alcotest.test_case "permissions: open a directory" `Quick
        test_perm_open_directory;
      Alcotest.test_case "Linux: mkdir keeps the permission and sticky bits"
        `Quick test_linux_mkdir_modes;
      Alcotest.test_case "Linux: rename searches before the source must exist"
        `Quick test_linux_rename_search_first;
      Alcotest.test_case "Linux: rename over a directory above is ENOTEMPTY"
        `Quick test_linux_rename_above;
      Alcotest.test_case "Linux: readlink of a non-link is EINVAL" `Quick
        test_linux_readlink;
      Alcotest.test_case "Linux: a read of a directory is EISDIR" `Quick
        test_linux_eisdir;
      Alcotest.test_case "Linux: rename of a last . or .. is EBUSY" `Quick
        test_linux_rename_dots;
      Alcotest.test_case "Linux: permission before kind" `Quick
        test_linux_permission_first;
      Alcotest.test_case "permissions: setgid directories and privilege bits"
        `Quick test_perm_setgid_and_privileges;
      Alcotest.test_case "a directory barrier opens its directory" `Quick
        test_sync_dir_checks;
    ] )
