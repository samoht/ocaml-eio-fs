(* A batch is confined to its directory as openat(2) with O_NOFOLLOW resolves
   it one component at a time: no [..], no symbolic link before the last
   component, and every operation answers for itself. *)
module Batch = Eio_fs_unix.Batch

let outside_contents = "outside, must not change"

(* [with_tree fn] makes [base/inside], the batch root, beside [base/outside], a
   file, and [base/outside_dir], an empty directory, and calls [fn] with the
   native base and the open batch. *)
let with_tree fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let base = Filename.temp_dir "eio-fs-batch" "" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / base))
    (fun () ->
      Unix.mkdir (Filename.concat base "inside") 0o755;
      Unix.mkdir (Filename.concat base "outside_dir") 0o755;
      Out_channel.with_open_bin (Filename.concat base "outside") (fun oc ->
          output_string oc outside_contents);
      Eio.Switch.run @@ fun sw ->
      let batch = Batch.v ~sw Eio.Path.(fs / Filename.concat base "inside") in
      fn base batch)

let read path = In_channel.with_open_bin path In_channel.input_all

(* The identity of what must not change: the outside file's bytes catch a
   write through an escape, and the outside directory's listing catches a
   creation, a rename or a symbolic link made there. *)
let check_outside base =
  Alcotest.(check string)
    "outside file untouched" outside_contents
    (read (Filename.concat base "outside"));
  Alcotest.(check (array string))
    "outside directory untouched" [||]
    (Sys.readdir (Filename.concat base "outside_dir"));
  Alcotest.(check (list string))
    "nothing new beside the root"
    [ "inside"; "outside"; "outside_dir" ]
    (List.sort compare (Array.to_list (Sys.readdir base)))

let refused message = function
  | Error (`Refused _) -> ()
  | Error (#Batch.error as e) ->
      Alcotest.failf "%s: expected a refusal, got %a" message Batch.pp_error e
  | Ok _ -> Alcotest.failf "%s: expected a refusal, got success" message

let escaping path = Batch.Create { path; perm = 0o644; contents = "escaped" }

let test_dotdot () =
  with_tree @@ fun base batch ->
  let ops =
    [
      escaping "../outside";
      escaping "../escaped";
      escaping "sub/../../escaped";
      Batch.Mkdir { path = "../outside_dir/x"; perm = 0o755 };
      Batch.Symlink { path = "../outside_dir/l"; target = "x" };
      Batch.Rename { src = "../outside"; dst = "stolen" };
      Batch.Link { src = "../outside"; dst = "stolen" };
      Batch.Link { src = "f"; dst = "../outside_dir/aliased" };
      Batch.Unlink { path = "../outside"; dir = false };
      escaping "/tmp/escaped";
      escaping "./a";
      escaping "a//b";
      escaping "";
    ]
  in
  let results = Batch.run batch ops in
  Alcotest.(check int)
    "one result per op" (List.length ops) (List.length results);
  List.iteri (fun i r -> refused (Fmt.str "op %d" i) r) results;
  check_outside base;
  Alcotest.(check (array string))
    "root still empty" [||]
    (Sys.readdir (Filename.concat base "inside"))

let test_symlink_component () =
  with_tree @@ fun base batch ->
  let inside = Filename.concat base "inside" in
  Unix.symlink "../outside_dir" (Filename.concat inside "link");
  Unix.symlink "../outside" (Filename.concat inside "leaf");
  Out_channel.with_open_bin (Filename.concat inside "f") (fun oc ->
      output_string oc "f");
  let results =
    Batch.run batch
      [
        escaping "link/x";
        Batch.Mkdir { path = "link/sub"; perm = 0o755 };
        Batch.Symlink { path = "link/s"; target = "x" };
        Batch.Rename { src = "f"; dst = "link/f" };
        Batch.Link { src = "f"; dst = "link/aliased" };
        Batch.Lstat "link/x";
      ]
  in
  List.iteri (fun i r -> refused (Fmt.str "op %d" i) r) results;
  (* The last component is never followed: creating over a planted link to
     the outside file fails with EEXIST and writes nothing through it. *)
  (match Batch.run batch [ escaping "leaf" ] with
  | [ Error (`Unix (Unix.EEXIST, _, _)) ] -> ()
  | [ Error e ] ->
      Alcotest.failf "leaf: expected EEXIST, got %a" Batch.pp_error e
  | _ -> Alcotest.fail "leaf: expected EEXIST");
  check_outside base

(* [linkat(2)] with no flags links the symbolic link at [src] itself (IEEE Std
   1003.1-2017, linkat), so a planted link to the outside file gains a second
   name while the outside file keeps its one. A regular file beneath the root
   gains a second name sharing its inode. *)
let test_link () =
  with_tree @@ fun base batch ->
  let inside = Filename.concat base "inside" in
  Unix.symlink "../outside" (Filename.concat inside "leaf");
  Out_channel.with_open_bin (Filename.concat inside "f") (fun oc ->
      output_string oc "f");
  (match
     Batch.run batch
       [
         Batch.Link { src = "f"; dst = "g" };
         Batch.Link { src = "leaf"; dst = "leaf2" };
       ]
   with
  | [ Ok Batch.Done; Ok Batch.Done ] -> ()
  | _ -> Alcotest.fail "expected both links to be made");
  let f = Unix.lstat (Filename.concat inside "f")
  and g = Unix.lstat (Filename.concat inside "g") in
  Alcotest.(check int) "g shares f's inode" f.Unix.st_ino g.Unix.st_ino;
  Alcotest.(check int) "f has two names" 2 f.Unix.st_nlink;
  Alcotest.(check bool)
    "leaf2 is the link itself" true
    ((Unix.lstat (Filename.concat inside "leaf2")).Unix.st_kind = Unix.S_LNK);
  Alcotest.(check int)
    "outside file keeps one name" 1
    (Unix.lstat (Filename.concat base "outside")).Unix.st_nlink;
  check_outside base

let kind = function
  | Ok (Batch.Stat st) ->
      Fmt.str "%a" Eio.File.Stat.pp_kind st.Eio.File.Stat.kind
  | Ok Batch.Done -> "done"
  | Error (`Refused _) -> "refused"
  | Error (`Unix (code, _, _)) -> Unix.error_message code

let test_per_op_errors () =
  with_tree @@ fun base batch ->
  let inside = Filename.concat base "inside" in
  let results =
    Batch.run batch
      [
        Batch.Create { path = "a"; perm = 0o640; contents = "abc" };
        Batch.Create { path = "a"; perm = 0o640; contents = "zzzz" };
        Batch.Mkdir { path = "d"; perm = 0o755 };
        Batch.Create { path = "d/b"; perm = 0o751; contents = "hello" };
        Batch.Lstat "missing";
        Batch.Rename { src = "a"; dst = "d/c" };
        Batch.Symlink { path = "d/l"; target = "c" };
        Batch.Lstat "d/l";
        Batch.Unlink { path = "d"; dir = true };
        Batch.Lstat "d/b";
      ]
  in
  let ok = "done" in
  Alcotest.(check (list string))
    "each op answers for itself"
    [
      ok;
      Unix.error_message Unix.EEXIST;
      ok;
      ok;
      Unix.error_message Unix.ENOENT;
      ok;
      ok;
      "symbolic link";
      Unix.error_message Unix.ENOTEMPTY;
      "regular file";
    ]
    (List.map kind results);
  (match List.nth results 9 with
  | Ok (Batch.Stat st) ->
      Alcotest.(check int) "size of d/b" 5 (Optint.Int63.to_int st.size);
      Alcotest.(check int) "perm of d/b, umask not applied" 0o751 st.perm
  | _ -> Alcotest.fail "d/b: expected a stat");
  Alcotest.(check string)
    "the failed create left the first one's bytes" "abc"
    (read (Filename.concat inside "d/c"));
  Alcotest.(check string) "d/b" "hello" (read (Filename.concat inside "d/b"));
  Alcotest.(check string)
    "d/l" "c"
    (Unix.readlink (Filename.concat inside "d/l"));
  check_outside base

let test_one_hop () =
  with_tree @@ fun base batch ->
  let n = 256 in
  let ops =
    Batch.Mkdir { path = "d"; perm = 0o755 }
    :: List.init n (fun i ->
        Batch.Create
          {
            path = Fmt.str "d/f%03d" i;
            perm = 0o644;
            contents = string_of_int i;
          })
  in
  let before = Eio_fs_unix.Private.batch_hops () in
  let results = Batch.run batch ops in
  let after = Eio_fs_unix.Private.batch_hops () in
  Alcotest.(check int) "N + 1 operations, one hop" 1 (after - before);
  Alcotest.(check int)
    "every op succeeded" (n + 1)
    (List.length (List.filter Result.is_ok results));
  Alcotest.(check int)
    "every file written" n
    (Array.length (Sys.readdir (Filename.concat base "inside/d")))

let suite =
  ( "batch",
    [
      Alcotest.test_case "dot-dot is refused" `Quick test_dotdot;
      Alcotest.test_case "symlink component is refused" `Quick
        test_symlink_component;
      Alcotest.test_case "per-op errors" `Quick test_per_op_errors;
      Alcotest.test_case "link" `Quick test_link;
      Alcotest.test_case "one hop per batch" `Quick test_one_hop;
    ] )
