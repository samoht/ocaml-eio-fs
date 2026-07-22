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

  (* [load] resets the filesystem: nothing outlives it. *)
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
       (** [load ()] resets the in-memory filesystem, populates it with\n\
      \    {!entries} and returns its root. *)\n"
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
        Alcotest.test_case "source" `Quick test_source;
        Alcotest.test_case "interface" `Quick test_interface;
      ] )
end

let suite =
  ( "eio_fs",
    [
      Alcotest.test_case "write/read" `Quick test_write_read;
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
    ]
    @ snd Crunch.suite )
