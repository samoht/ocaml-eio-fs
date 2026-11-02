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

let with_env ?cwd fn = Eio_mock.Backend.run @@ fun () -> Eio_fs.run ?cwd fn

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
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / "/sandbox");
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(env#fs / "/outside")
    "secret";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load Eio.Path.(env#cwd / "../outside")))

let test_cwd_symlink_escape () =
  with_env ~cwd:"/sandbox" @@ fun env ->
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / "/sandbox");
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
   even for the owner. *)
let test_readonly_reopen_denied () =
  with_env @@ fun env ->
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
  with_env @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o200) (env#fs / "wo") "x";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load (env#fs / "wo")));
  expect_io_error "permission denied" (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o200) (env#fs / "wo") "y")

(* A directory capability opened for a switch dies with the switch, as a real
   O_DIRECTORY fd does: using it afterwards is EBADF, not silent success. *)
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
  | exception Eio.Io _ -> ()

(* close(2): "the file descriptor is deallocated" and the next open may be
   handed the same number. A handle answers on its own state: closing it a
   second time does nothing (as [Eio.Resource.close] specifies), and using it
   is EBADF, whatever file has taken its number since. Each case opens [a],
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
      match Eio.Flow.copy_string "stale" a with
      | () -> Alcotest.fail "a write through a closed handle succeeded"
      | exception Eio.Io _ -> ());
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

let test_write_directory_fd () =
  with_env @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "d");
  Eio.Path.with_open_out ~create:`Never (env#fs / "d") @@ fun flow ->
  let buf = Cstruct.of_string "x" in
  expect_eio_io "writing to a directory descriptor" (fun () ->
      ignore (Eio.Flow.single_write flow [ buf ] : int));
  expect_eio_io "positioned write to a directory descriptor" (fun () ->
      ignore
        (Eio.File.pwrite_single flow ~file_offset:Optint.Int63.zero [ buf ]
          : int))

(* open(2) with O_CREAT answers ENOTDIR when a component of the path prefix is
   not a directory. This backend already reports a walk through a regular file
   as a path that does not resolve; creating under one must agree instead of
   escaping the error model. *)
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

(* ENOTDIR is an error of the filesystem's own model, not a caller mistake:
   with a regular file in the tree, every directory operation that walks
   through it must answer through [Eio.Io], not an [Invalid_argument] this
   backend alone raises. The five below are one operation set and must agree,
   so [Not_found] is pinned for all five. *)
let test_dir_ops_under_regular_file () =
  with_env @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Or_truncate 0o600) (fs / "reg") "x";
  expect_io_error "not found" (fun () ->
      Eio.Path.mkdir ~perm:0o755 (fs / "reg" / "sub"));
  expect_io_error "not found" (fun () -> Eio.Path.unlink (fs / "reg" / "child"));
  expect_io_error "not found" (fun () ->
      Eio.Path.symlink ~link_to:"t" (fs / "reg" / "l"));
  expect_io_error "not found" (fun () -> Eio.Path.rmdir (fs / "reg" / "sub"));
  expect_io_error "not found" (fun () ->
      ignore (Eio.Path.read_dir (fs / "reg" / "sub") : string list))

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
  Eio.Path.mkdir ~perm:0o755 (env#fs / "/q");
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
  Eio.Path.mkdir ~perm:0o755 (env#fs / "/d");
  Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "/d/f") "inner";
  Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "/f") "outer";
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load (env#cwd / "/f") : string));
  Eio.Switch.run @@ fun sw ->
  let d = Eio.Path.open_dir ~sw (env#fs / "/d") in
  expect_io_error "permission denied" (fun () ->
      ignore (Eio.Path.load Eio.Path.(d / "/f") : string))

let suite =
  ( "low_level",
    [
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
    ] )
