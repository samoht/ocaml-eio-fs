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

let suite =
  ( "eio_fs",
    [
      Alcotest.test_case "write/read" `Quick test_write_read;
      Alcotest.test_case "mkdir/read_dir" `Quick test_mkdir_read_dir;
      Alcotest.test_case "read_dir_entries" `Quick test_read_dir_entries;
      Alcotest.test_case "chmod" `Quick test_chmod;
      Alcotest.test_case "chown" `Quick test_chown;
      Alcotest.test_case "rename" `Quick test_rename;
      Alcotest.test_case "unlink" `Quick test_unlink;
      Alcotest.test_case "stat kind" `Quick test_stat_kind;
      Alcotest.test_case "symlink" `Quick test_symlink;
    ] )
