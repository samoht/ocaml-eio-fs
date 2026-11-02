let ( / ) = Eio.Path.( / )

(* [Eio_fs.run] installs no scheduler, so drive it under the mock backend. *)
let with_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn env#fs

let write fs name contents =
  Eio.Path.save ~create:(`Or_truncate 0o644) (fs / name) contents

let test_write_read () =
  with_fs @@ fun fs ->
  write fs "hello.txt" "world";
  Alcotest.(check string) "roundtrip" "world" (Eio.Path.load (fs / "hello.txt"))

(* [Fs.v ()] is the root of a fresh filesystem, so two roots are two
   filesystems: a file written under one does not exist under the other, even
   when both were made before the write. *)
let test_v_roots_share_no_file () =
  Eio_mock.Backend.run @@ fun () ->
  let first = Eio_fs.Fs.v () in
  let second = Eio_fs.Fs.v () in
  write first "first.txt" "one";
  Alcotest.(check bool)
    "the second root does not see the first root's file" true
    (Eio.Path.kind ~follow:false (second / "first.txt") = `Not_found)

(* [Fs.v ()] is the root of an empty filesystem whatever another root holds. *)
let test_v_is_empty () =
  Eio_mock.Backend.run @@ fun () ->
  write (Eio_fs.Fs.v ()) "earlier.txt" "stale";
  Alcotest.(check (list string))
    "a fresh root has no entry" []
    (Eio.Path.read_dir (Eio_fs.Fs.v ()))

(* rename(2) fails with EXDEV when its two paths are on different file systems
   (POSIX.1-2017, rename, ERRORS), and two [Fs.v ()] roots are two: the entry
   stays where it was and the target gains nothing. *)
let test_rename_across_roots_is_refused () =
  Eio_mock.Backend.run @@ fun () ->
  let first = Eio_fs.Fs.v () in
  let second = Eio_fs.Fs.v () in
  write first "moved.txt" "one";
  (match Eio.Path.rename (first / "moved.txt") (second / "moved.txt") with
  | () -> Alcotest.fail "a rename moved an entry between two filesystems"
  | exception Eio.Io _ -> ());
  Alcotest.(check string)
    "the source keeps its file" "one"
    (Eio.Path.load (first / "moved.txt"));
  Alcotest.(check bool)
    "the target gained nothing" true
    (Eio.Path.kind ~follow:false (second / "moved.txt") = `Not_found)

let test_mkdir_read_dir () =
  with_fs @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  write fs "d/a" "x";
  write fs "d/b" "y";
  let names = List.sort compare (Eio.Path.read_dir (fs / "d")) in
  Alcotest.(check (list string)) "entries" [ "a"; "b" ] names

let show_entry (kind, name) =
  let k =
    match kind with
    | `Directory -> "dir"
    | `Regular_file -> "file"
    | _ -> "special"
  in
  Fmt.str "%s:%s" name k

(* Drives the DIR provider's [with_dir_entries]. *)
let test_read_dir_entries () =
  with_fs @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "sub");
  write fs "f" "z";
  let entries =
    Eio.Path.read_dir_entries fs |> List.map show_entry |> List.sort compare
  in
  Alcotest.(check (list string)) "kinds" [ "f:file"; "sub:dir" ] entries

(* Drives the DIR provider's [chmod]. *)
let test_chmod () =
  with_fs @@ fun fs ->
  write fs "c" "x";
  Eio.Path.chmod ~follow:false ~perm:0o600 (fs / "c");
  let s = Eio.Path.stat ~follow:false (fs / "c") in
  Alcotest.(check int) "perm updated" 0o600 s.perm

(* Drives the DIR provider's [chown]. *)
let test_chown () =
  with_fs @@ fun fs ->
  write fs "o" "x";
  Eio.Path.chown ~follow:false ~uid:42L ~gid:7L (fs / "o");
  let s = Eio.Path.stat ~follow:false (fs / "o") in
  Alcotest.(check int64) "uid updated" 42L s.uid;
  Alcotest.(check int64) "gid updated" 7L s.gid

let test_rename () =
  with_fs @@ fun fs ->
  write fs "old" "data";
  Eio.Path.rename (fs / "old") (fs / "new");
  Alcotest.(check string) "moved" "data" (Eio.Path.load (fs / "new"))

(* POSIX [rename(2)] MOVES: after the rename the source path no longer exists
   and the target is an independent file, not a hardlink sharing the source's
   storage. *)
let test_rename_moves () =
  with_fs @@ fun fs ->
  write fs "a" "data";
  Eio.Path.rename (fs / "a") (fs / "b");
  (* (1) the source path no longer resolves. *)
  (match Eio.Path.load (fs / "a") with
  | (_ : string) -> Alcotest.fail "source still resolves after rename"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ());
  (* (2) the target carries the moved content. *)
  Alcotest.(check string) "target content" "data" (Eio.Path.load (fs / "b"));
  (* (3) a fresh file created at the source shares no storage with the target:
     writing it must not change the target, and vice versa. *)
  write fs "a" "fresh";
  Alcotest.(check string) "target unchanged" "data" (Eio.Path.load (fs / "b"));
  Alcotest.(check string) "new source" "fresh" (Eio.Path.load (fs / "a"))

(* The atomic-save idiom (write a temporary, then rename it over the target)
   behaves like a real filesystem: after the rename the temporary is gone, so a
   later [`Or_truncate] save through the same temporary name starts from a fresh
   file and never truncates the committed target's shared storage. *)
let test_rename_atomic_save () =
  with_fs @@ fun fs ->
  write fs "db.tmp" "v1";
  Eio.Path.rename (fs / "db.tmp") (fs / "db");
  (* A second atomic save reuses the temporary name. *)
  write fs "db.tmp" "v2";
  (* The committed target from the first round is untouched by the second save. *)
  Alcotest.(check string)
    "committed target intact" "v1"
    (Eio.Path.load (fs / "db"));
  Eio.Path.rename (fs / "db.tmp") (fs / "db");
  Alcotest.(check string)
    "second commit visible" "v2"
    (Eio.Path.load (fs / "db"))

let test_unlink () =
  with_fs @@ fun fs ->
  write fs "gone" "x";
  Eio.Path.unlink (fs / "gone");
  let names = Eio.Path.read_dir fs in
  Alcotest.(check bool) "removed" false (List.mem "gone" names)

let test_stat_kind () =
  with_fs @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "adir");
  write fs "afile" "x";
  let dir = Eio.Path.stat ~follow:false (fs / "adir") in
  let file = Eio.Path.stat ~follow:false (fs / "afile") in
  Alcotest.(check bool) "dir kind" true (dir.kind = `Directory);
  Alcotest.(check bool) "file kind" true (file.kind = `Regular_file)

let test_symlink () =
  with_fs @@ fun fs ->
  write fs "target" "payload";
  Eio.Path.symlink ~link_to:"target" (fs / "link");
  Alcotest.(check string)
    "read_link" "target"
    (Eio.Path.read_link (fs / "link"));
  Alcotest.(check string) "follows" "payload" (Eio.Path.load (fs / "link"))

let i63 = Optint.Int63.of_int

(* Positioned I/O: [pwrite] addresses an explicit offset without moving the
   cursor and grows the file, leaving a zero-filled gap. *)
let test_pwrite_pread () =
  with_fs @@ fun fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "f") @@ fun file ->
  Eio.File.pwrite_all file ~file_offset:(i63 5) [ Cstruct.of_string "world" ];
  Alcotest.(check int) "size" 10 (Optint.Int63.to_int (Eio.File.size file));
  let buf = Cstruct.create 5 in
  Eio.File.pread_exact file ~file_offset:(i63 5) [ buf ];
  Alcotest.(check string) "pread" "world" (Cstruct.to_string buf);
  let gap = Cstruct.create 5 in
  Eio.File.pread_exact file ~file_offset:(i63 0) [ gap ];
  Alcotest.(check string)
    "gap zero-filled" (String.make 5 '\000') (Cstruct.to_string gap)

(* [pread] past end-of-file raises rather than looping forever. *)
let test_pread_eof () =
  with_fs @@ fun fs ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (fs / "f") "abc";
  Eio.Path.with_open_in (fs / "f") @@ fun file ->
  Alcotest.check_raises "eof" End_of_file (fun () ->
      Eio.File.pread_exact file ~file_offset:(i63 3) [ Cstruct.create 1 ])

let test_seek () =
  with_fs @@ fun fs ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (fs / "f") "0123456789";
  Eio.Path.with_open_in (fs / "f") @@ fun file ->
  let pos = Eio.File.seek file (i63 4) `Set in
  Alcotest.(check int) "seek pos" 4 (Optint.Int63.to_int pos);
  let buf = Cstruct.create 3 in
  let got = Eio.Flow.single_read file buf in
  Alcotest.(check string)
    "read from cursor" "456"
    (Cstruct.to_string (Cstruct.sub buf 0 got))

let test_truncate () =
  with_fs @@ fun fs ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (fs / "f") "0123456789";
  Eio.Path.with_open_out ~create:`Never (fs / "f") @@ fun file ->
  Eio.File.truncate file (i63 4);
  Alcotest.(check int) "shrunk" 4 (Optint.Int63.to_int (Eio.File.size file));
  Eio.File.truncate file (i63 6);
  let buf = Cstruct.create 6 in
  Eio.File.pread_exact file ~file_offset:(i63 0) [ buf ];
  Alcotest.(check string)
    "extend zero-fills" "0123\000\000" (Cstruct.to_string buf)

(* The in-memory store is already durable, so [sync] is a no-op that must not
   raise. *)
let test_sync_noop () =
  with_fs @@ fun fs ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "f") @@ fun file ->
  Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "x" ];
  Eio.File.sync file

module Crunch = struct
  let ( / ) = Eio.Path.( / )

  (* [Eio_fs.run] installs no scheduler, so drive it under the mock backend. *)
  let with_fs fn =
    Eio_mock.Backend.run @@ fun () ->
    Eio_fs.run @@ fun env -> fn env#fs

  let write ?(perm = 0o644) fs name contents =
    Eio.Path.save ~create:(`Or_truncate perm) (fs / name) contents

  (* Scanning a tree and loading the entries rebuilds it exactly: contents,
   permissions, symbolic links and empty directories all survive. *)
  let test_roundtrip () =
    with_fs @@ fun fs ->
    Eio.Path.mkdir ~perm:0o750 (fs / "src");
    write fs "src/hello.txt" "world";
    write ~perm:0o755 fs "src/run.sh" "#!/bin/sh\n";
    Eio.Path.mkdir ~perm:0o755 (fs / "src/sub");
    write fs "src/sub/nested.txt" "deep";
    Eio.Path.mkdir ~perm:0o700 (fs / "src/empty");
    Eio.Path.symlink ~link_to:"hello.txt" (fs / "src/link");
    let entries = Eio_fs.Crunch.scan (fs / "src") in
    let fs = Eio_fs.Crunch.load entries in
    Alcotest.(check string)
      "contents" "world"
      (Eio.Path.load (fs / "hello.txt"));
    Alcotest.(check string)
      "nested contents" "deep"
      (Eio.Path.load (fs / "sub/nested.txt"));
    let stat name = (Eio.Path.stat ~follow:false (fs / name)).perm in
    Alcotest.(check int) "root perm" 0o750 (stat "/");
    Alcotest.(check int) "file perm" 0o644 (stat "hello.txt");
    Alcotest.(check int) "exec perm" 0o755 (stat "run.sh");
    Alcotest.(check int) "empty dir perm" 0o700 (stat "empty");
    Alcotest.(check string)
      "symlink target" "hello.txt"
      (Eio.Path.read_link (fs / "link"));
    Alcotest.(check string)
      "read through symlink" "world"
      (Eio.Path.load (fs / "link"))

  (* [load] builds a filesystem of its own: nothing written elsewhere is in it. *)
  let test_load_resets () =
    with_fs @@ fun fs ->
    write fs "old.txt" "stale";
    let fs = Eio_fs.Crunch.(load [ Dir { path = "/"; perm = 0o755 } ]) in
    Alcotest.(check bool)
      "old file gone" true
      (Eio.Path.kind ~follow:false (fs / "old.txt") = `Not_found)

  (* Embedded files alias the entry strings until first written to; mutating a
   loaded file must copy first, leave the result readable, and never disturb
   its neighbours. *)
  let test_copy_on_write () =
    with_fs @@ fun _env_fs ->
    let load () =
      Eio_fs.Crunch.(
        load
          [
            Dir { path = "/"; perm = 0o755 };
            File { path = "/note.txt"; perm = 0o644; contents = "abc" };
            File { path = "/other.txt"; perm = 0o644; contents = "keep" };
          ])
    in
    let fs = load () in
    Eio.Path.save ~append:true ~create:`Never (fs / "note.txt") "def";
    Alcotest.(check string)
      "append after embed" "abcdef"
      (Eio.Path.load (fs / "note.txt"));
    Eio.Path.save ~create:(`Or_truncate 0o644) (fs / "note.txt") "xyz";
    Alcotest.(check string)
      "truncate rewrite" "xyz"
      (Eio.Path.load (fs / "note.txt"));
    Alcotest.(check string)
      "neighbour untouched" "keep"
      (Eio.Path.load (fs / "other.txt"));
    let fs = load () in
    Alcotest.(check string)
      "reload restores embedded contents" "abc"
      (Eio.Path.load (fs / "note.txt"))

  (* Positioned reads address the embedded string directly. *)
  let test_embedded_pread () =
    with_fs @@ fun _env_fs ->
    let fs =
      Eio_fs.Crunch.(
        load
          [
            Dir { path = "/"; perm = 0o755 };
            File { path = "/note.txt"; perm = 0o644; contents = "abcdef" };
          ])
    in
    Eio.Path.with_open_in (fs / "note.txt") @@ fun file ->
    let buf = Cstruct.create 3 in
    let got =
      Eio.File.pread file ~file_offset:(Optint.Int63.of_int 2) [ buf ]
    in
    Alcotest.(check int) "pread length" 3 got;
    Alcotest.(check string) "pread contents" "cde" (Cstruct.to_string buf)

  let test_scan_rejects_file () =
    with_fs @@ fun fs ->
    write fs "plain.txt" "x";
    match Eio_fs.Crunch.scan (fs / "plain.txt") with
    | _ -> Alcotest.fail "scan accepted a non-directory"
    | exception Invalid_argument _ -> ()

  (* [find] answers a file's contents by its image path, and nothing for a
   directory, a symbolic link or a path not in the image: an implementation that
   matched on the name alone would answer "/a"'s contents for "/sub/a". *)
  let test_find () =
    let entries =
      Eio_fs.Crunch.
        [
          Dir { path = "/"; perm = 0o755 };
          File { path = "/a"; perm = 0o644; contents = "top" };
          Dir { path = "/sub"; perm = 0o755 };
          File { path = "/sub/a"; perm = 0o644; contents = "nested" };
          Symlink { path = "/l"; target = "a" };
        ]
    in
    let find = Eio_fs.Crunch.find entries in
    Alcotest.(check (option string)) "/a" (Some "top") (find "/a");
    Alcotest.(check (option string)) "/sub/a" (Some "nested") (find "/sub/a");
    Alcotest.(check (option string)) "directory" None (find "/sub");
    Alcotest.(check (option string)) "symbolic link" None (find "/l");
    Alcotest.(check (option string)) "absent" None (find "/b")

  (* The generated source is byte-exact, including OCaml string escaping of
   quotes, newlines and non-printable bytes. *)
  let test_source () =
    let entries =
      Eio_fs.Crunch.
        [
          Dir { path = "/"; perm = 0o755 };
          File { path = "/a b"; perm = 0o600; contents = "x\"y\nz\000" };
          Symlink { path = "/l"; target = "a b" };
        ]
    in
    let expected =
      "(* Generated by eio-fs-crunch. Do not edit. *)\n\n\
       let entries : Eio_fs.Crunch.t =\n\
      \  Eio_fs.Crunch.\n\
      \    [\n\
      \      Dir { path = \"/\"; perm = 0o755 };\n\
      \      File { path = \"/a b\"; perm = 0o600; contents = \
       \"x\\\"y\\nz\\000\" };\n\
      \      Symlink { path = \"/l\"; target = \"a b\" };\n\
      \    ]\n\n\
       let load () = Eio_fs.Crunch.load entries\n"
    in
    Alcotest.(check string)
      "source" expected
      (Fmt.str "%a" Eio_fs.Crunch.source entries)

  let test_interface () =
    let expected =
      "(* Generated by eio-fs-crunch. Do not edit. *)\n\n\
       val entries : Eio_fs.Crunch.t\n\
       (** The embedded file tree. *)\n\n\
       val load : unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t\n\
       (** [load ()] is the root of a new in-memory filesystem holding\n\
      \    {!entries}. *)\n"
    in
    Alcotest.(check string) "interface" expected Eio_fs.Crunch.interface

  let suite =
    ( "crunch",
      [
        Alcotest.test_case "roundtrip" `Quick test_roundtrip;
        Alcotest.test_case "load resets" `Quick test_load_resets;
        Alcotest.test_case "copy on write" `Quick test_copy_on_write;
        Alcotest.test_case "embedded pread" `Quick test_embedded_pread;
        Alcotest.test_case "scan rejects file" `Quick test_scan_rejects_file;
        Alcotest.test_case "find" `Quick test_find;
        Alcotest.test_case "source" `Quick test_source;
        Alcotest.test_case "interface" `Quick test_interface;
      ] )
end

(* A path that traverses *through* a regular file resolves to nothing: POSIX
   answers ENOTDIR, and every real Eio backend surfaces that as an [Eio.Io].
   The in-memory backend must too, or it is not substitutable for one -- code
   written correctly against Eio's error model still crashes on it, and only on
   this backend. Serving static files is the everyday way to hit it: a server
   that falls back from "page.txt" to "page.txt/index.html" walks straight into
   this path on any request for a file that exists. *)
let test_path_through_file_is_io_error () =
  with_fs @@ fun fs ->
  write fs "page.txt" "hello";
  let expect_io what f =
    match f () with
    | _ -> Alcotest.failf "%s: expected an Eio.Io error" what
    | exception Eio.Io _ -> ()
  in
  expect_io "load through a file" (fun () ->
      Eio.Path.load (fs / "page.txt" / "index.html"));
  expect_io "read_dir through a file" (fun () ->
      Eio.Path.read_dir (fs / "page.txt"))

(* Closing a file that was opened with [~sw] must not leave the switch a second
   close to make: [Eio.Resource.close] is the documented way to release early,
   and the switch releases whatever is still open when it finishes. The POSIX
   backends drop the hook on close, so a store that opens a scratch file, closes
   it and carries on -- writing a journal, fsyncing it, closing it, then removing
   it, as a rollback-journal commit does -- runs fine there and died here on the
   switch's second close. That is a substitutability bug: the in-memory backend
   exists so storage tests can run against it instead of a real directory. *)
let test_close_then_switch_release () =
  with_fs @@ fun fs ->
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (fs / "j") in
  Eio.Resource.close file

(* The same for a read handle, which takes the other open path. *)
let test_close_in_then_switch_release () =
  with_fs @@ fun fs ->
  write fs "r" "x";
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw (fs / "r") in
  Eio.Resource.close file

(* And the same again for the second close a caller makes itself.
   [Eio.Resource.close] is specified to be idempotent -- "If [t] is already
   closed then this does nothing (it does not raise an exception)" (eio 1.4,
   [lib_eio/resource.mli]) -- and eio's own posix backend keeps that promise,
   its close answering a descriptor that has gone with [false] rather than
   raising. A double close is the ordinary shape of one handle with two owners:
   a session closing both views of a file it opened twice, a caller's own close
   in front of a hook that closes what it registered. Over a real directory
   that works, so it has to work here, or a store's tests pass against this
   backend and its production run does something else.

   The file is read back afterwards because "does nothing" is the whole
   specification: the second close must not raise, and must not take the file
   with it either. *)
let test_close_twice_is_not_an_error () =
  with_fs @@ fun fs ->
  Eio.Switch.run @@ fun sw ->
  let out = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (fs / "w") in
  Eio.Flow.copy_string "payload" out;
  Eio.Resource.close out;
  Eio.Resource.close out;
  Alcotest.(check string)
    "a closed file is closed, not lost" "payload"
    (Eio.Path.load (fs / "w"));
  let inp = Eio.Path.open_in ~sw (fs / "w") in
  Eio.Resource.close inp;
  Eio.Resource.close inp;
  Alcotest.(check string)
    "and a read handle closed twice takes nothing with it either" "payload"
    (Eio.Path.load (fs / "w"))

(* Not every path has a spelling the host would recognise: nothing outside the
   process backs an in-memory tree, so a native name for it would point at an
   unrelated real location. A path still has to be nameable when it is refused,
   so the name falls back to the rendering Eio gives the path itself -- the
   capability's label and the member inside it -- which at least says which
   path was meant. *)
let test_name_falls_back_to_the_eio_rendering () =
  with_fs @@ fun fs ->
  write fs "hello.txt" "world";
  let path = fs / "hello.txt" in
  Alcotest.(check (option string))
    "an in-memory path has no native spelling to be named by" None
    (Eio.Path.native path);
  Alcotest.(check string)
    "the name is what Eio.Path.pp renders the path as" "<fs:hello.txt>"
    (Eio_fs.Path.name path)

(* [Fs.tree] labels every node by the last component of its own path, a
   symbolic link by that component and its target, and the root by the path the
   caller handed in -- for the root of this backend, the directory itself. The
   labels are path arithmetic on the path each node was reached by, and nothing
   else in the suite renders a tree, so the shapes that arithmetic has to
   answer for are pinned here: a directory below the root, a name reached
   through a trailing slash, and the root of the capability, which has no last
   component to be named by. *)
(* The glyphs the tree is drawn with, named so the renderings below read as
   trees: a branch with siblings after it, and the last branch of a level. *)
let branch = "\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 "
let last_branch = "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 "

let test_tree_labels_each_node_by_its_last_component () =
  with_fs @@ fun fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  write fs "d/a" "x";
  Eio.Path.symlink ~link_to:"a" (fs / "d/l");
  let render ~depth path = Fmt.str "%a" (Eio_fs.Fs.tree ~depth) path in
  Alcotest.(check string)
    "the root of the capability is named as the backend spells it"
    ("." ^ "\n" ^ last_branch ^ "d")
    (render ~depth:2 fs);
  Alcotest.(check string)
    "a directory is named by its own last component, a link by its target"
    ("d" ^ "\n" ^ branch ^ "a" ^ "\n" ^ last_branch ^ "l -> a")
    (render ~depth:2 (fs / "d"));
  Alcotest.(check string)
    "a trailing slash names the same directory"
    (render ~depth:2 (fs / "d"))
    (render ~depth:2 (fs / "d/"));
  Alcotest.(check string)
    "depth 1 is the path alone" "d"
    (render ~depth:1 (fs / "d"));
  Alcotest.(check string) "depth 0 prints nothing" "" (render ~depth:0 fs)

(* Eio 1.4 Resource.close: closing prevents new operations, repeated close
   succeeds, and a switch may still release a handle closed by its caller. *)
let expect_closed what fn =
  match fn () with
  | _ -> Alcotest.failf "%s succeeded through a closed directory" what
  | exception Eio.Io _ -> ()

let test_root_close_invalidates_aliases () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o700 (root / "child");
  write (root / "child") "payload" "the child keeps its bytes";
  Eio.Switch.run @@ fun sw ->
  let child = Eio.Path.open_dir ~sw (root / "child") in
  let alias = root in
  Alcotest.(check (list string))
    "the root worked before close" [ "child" ] (Eio.Path.read_dir root);
  Eio.Resource.close (fst root);
  Eio.Resource.close (fst alias);
  expect_closed "read_dir" (fun () -> Eio.Path.read_dir alias);
  expect_closed "mkdir" (fun () -> Eio.Path.mkdir ~perm:0o700 (alias / "new"));
  Alcotest.(check string)
    "closing a root does not close an existing child"
    "the child keeps its bytes"
    (Eio.Path.load (child / "payload"))

let test_directory_close_then_switch_release () =
  with_fs @@ fun fs ->
  Eio.Path.mkdir ~perm:0o700 (fs / "child");
  write (fs / "child") "payload" "the original bytes";
  Eio.Switch.run @@ fun sw ->
  let child = Eio.Path.open_dir ~sw (fs / "child") in
  Alcotest.(check string)
    "the open directory worked before close" "the original bytes"
    (Eio.Path.load (child / "payload"));
  Eio.Resource.close (fst child);
  Eio.Resource.close (fst child);
  expect_closed "load" (fun () -> Eio.Path.load (child / "payload"));
  expect_closed "rename target" (fun () ->
      Eio.Path.rename (fs / "child" / "payload") (child / "moved"));
  Alcotest.(check string)
    "closed target refusal preserves the source" "the original bytes"
    (Eio.Path.load (fs / "child" / "payload"))

let test_directory_close_after_switch_release () =
  with_fs @@ fun fs ->
  Eio.Path.mkdir ~perm:0o700 (fs / "child");
  let child = ref None in
  Eio.Switch.run (fun sw ->
      child := Some (Eio.Path.open_dir ~sw (fs / "child")));
  let child = Option.get !child in
  Eio.Resource.close (fst child);
  Eio.Resource.close (fst child);
  expect_closed "read_dir after switch release" (fun () ->
      Eio.Path.read_dir child)

(* [load] copies a tree from any filesystem, an in-memory one included, to
   the same path in a new one; a directory its filter refuses is not copied,
   and nothing under it is read. *)
let test_load_filter () =
  with_fs @@ fun src ->
  Eio.Path.mkdirs ~perm:0o755 (src / "tree/keep");
  Eio.Path.mkdirs ~perm:0o755 (src / "tree/skip");
  write src "tree/keep/a" "kept";
  write src "tree/skip/b" "skipped";
  let filter (_, path) = Filename.basename path <> "skip" in
  let dst = Eio_fs.Fs.load ~filter (src / "tree") in
  Alcotest.(check string)
    "a kept file is copied" "kept"
    (Eio.Path.load (dst / "tree/keep/a"));
  Alcotest.(check bool)
    "a refused directory is not copied" true
    (Eio.Path.kind ~follow:false (dst / "tree/skip") = `Not_found)

let suite =
  ( "eio_fs",
    [
      Alcotest.test_case "root close invalidates aliases" `Quick
        test_root_close_invalidates_aliases;
      Alcotest.test_case "directory close then switch release" `Quick
        test_directory_close_then_switch_release;
      Alcotest.test_case "directory close after switch release" `Quick
        test_directory_close_after_switch_release;
      Alcotest.test_case "write/read" `Quick test_write_read;
      Alcotest.test_case "load copies what its filter accepts" `Quick
        test_load_filter;
      Alcotest.test_case "v roots share no file" `Quick
        test_v_roots_share_no_file;
      Alcotest.test_case "v is empty" `Quick test_v_is_empty;
      Alcotest.test_case "rename across roots is refused" `Quick
        test_rename_across_roots_is_refused;
      Alcotest.test_case "tree labels each node by its last component" `Quick
        test_tree_labels_each_node_by_its_last_component;
      Alcotest.test_case "name falls back to the Eio rendering" `Quick
        test_name_falls_back_to_the_eio_rendering;
      Alcotest.test_case "close then switch release" `Quick
        test_close_then_switch_release;
      Alcotest.test_case "close read handle then switch release" `Quick
        test_close_in_then_switch_release;
      Alcotest.test_case "close twice is not an error" `Quick
        test_close_twice_is_not_an_error;
      Alcotest.test_case "mkdir/read_dir" `Quick test_mkdir_read_dir;
      Alcotest.test_case "read_dir_entries" `Quick test_read_dir_entries;
      Alcotest.test_case "chmod" `Quick test_chmod;
      Alcotest.test_case "chown" `Quick test_chown;
      Alcotest.test_case "rename" `Quick test_rename;
      Alcotest.test_case "rename moves" `Quick test_rename_moves;
      Alcotest.test_case "rename atomic save" `Quick test_rename_atomic_save;
      Alcotest.test_case "unlink" `Quick test_unlink;
      Alcotest.test_case "stat kind" `Quick test_stat_kind;
      Alcotest.test_case "symlink" `Quick test_symlink;
      Alcotest.test_case "pwrite/pread" `Quick test_pwrite_pread;
      Alcotest.test_case "pread eof" `Quick test_pread_eof;
      Alcotest.test_case "seek" `Quick test_seek;
      Alcotest.test_case "truncate" `Quick test_truncate;
      Alcotest.test_case "sync no-op" `Quick test_sync_noop;
      Alcotest.test_case "path through a file is an Eio.Io error" `Quick
        test_path_through_file_is_io_error;
    ]
    @ snd Crunch.suite )
