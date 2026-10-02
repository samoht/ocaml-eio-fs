(* POSIX openat(2), O_NOFOLLOW and symlinkat(2): authority is the opened
   directory, and an existing destination must never be overwritten. *)
module Link = Eio_fs_unix.Link

let ( / ) = Eio.Path.( / )

let with_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_dir "eio-fs-link" "" in
  let root = fs / name in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () -> fn root)

let get = function Ok value -> value | Error message -> Alcotest.fail message

let exists path =
  match Eio.Path.stat ~follow:false path with
  | _ -> true
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

let publish ?mkdir root path target =
  Link.with_parent ?mkdir ~root ~path (fun parent ->
      Link.publish parent ~target)

let publication message expected actual =
  let show = function Link.Created -> "created" | Present -> "present" in
  Alcotest.(check string) message (show expected) (show (get actual))

let contains needle text =
  let n = String.length needle and len = String.length text in
  let rec loop i =
    i + n <= len && (String.equal needle (String.sub text i n) || loop (i + 1))
  in
  loop 0

let refusal ?kind root path result =
  match result with
  | Ok _ -> Alcotest.fail "unexpected publication"
  | Error message ->
      List.iter
        (fun expected ->
          Alcotest.(check bool) expected true (contains expected message))
        ([ Eio_fs.Path.name (root / path); "repair" ] @ Option.to_list kind)

let test_missing_parents () =
  with_dir @@ fun root ->
  publication "new link" Link.Created
    (publish ~mkdir:0o755 root "packages/engine/library" "/selected");
  Alcotest.(check string)
    "exact target" "/selected"
    (Eio.Path.read_link (root / "packages/engine/library"))

let test_existing_parent () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "packages");
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "packages/kept") "untouched";
  publication "new leaf" Link.Created
    (publish root "packages/library" "/selected");
  Alcotest.(check string)
    "unrelated bytes" "untouched"
    (Eio.Path.load (root / "packages/kept"))

let test_missing_refusal () =
  with_dir @@ fun root ->
  refusal root "missing/library" (publish root "missing/library" "/selected");
  Alcotest.(check bool) "no parent created" false (exists (root / "missing"))

let test_ancestor_link () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "outside");
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "outside/kept")
    "outside bytes";
  Eio.Path.symlink ~link_to:"outside" (root / "packages");
  refusal root "packages/library"
    (publish ~mkdir:0o755 root "packages/library" "/selected");
  Alcotest.(check string)
    "outside untouched" "outside bytes"
    (Eio.Path.load (root / "outside/kept"));
  Alcotest.(check bool)
    "no outside portal" false
    (exists (root / "outside/library"))

let test_foreign_leaf () =
  with_dir @@ fun root ->
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "library") "foreign bytes";
  refusal ~kind:"regular file" root "library"
    (publish root "library" "/selected");
  Alcotest.(check string)
    "foreign bytes" "foreign bytes"
    (Eio.Path.load (root / "library"))

let test_wrong_link () =
  with_dir @@ fun root ->
  Eio.Path.symlink ~link_to:"/other" (root / "library");
  refusal ~kind:"symbolic link" root "library"
    (publish root "library" "/selected");
  Alcotest.(check string)
    "foreign link" "/other"
    (Eio.Path.read_link (root / "library"))

let test_existing_exact () =
  with_dir @@ fun root ->
  Eio.Path.symlink ~link_to:"/selected" (root / "library");
  publication "observation" Link.Present (publish root "library" "/selected");
  Alcotest.(check string)
    "link text" "/selected"
    (Eio.Path.read_link (root / "library"))

let test_parent_file () =
  with_dir @@ fun root ->
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "parent") "parent bytes";
  let called = ref false in
  refusal root "parent/library"
    (Link.with_parent ~mkdir:0o755 ~root ~path:"parent/library" (fun _ ->
         called := true;
         Ok Link.Created));
  Alcotest.(check bool) "no callback for a non-directory" false !called;
  Alcotest.(check string)
    "parent bytes preserved" "parent bytes"
    (Eio.Path.load (root / "parent"))

let test_nested_link () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "packages");
  Eio.Path.mkdir ~perm:0o755 (root / "packages/outside");
  Eio.Path.symlink ~link_to:"outside" (root / "packages/nested");
  refusal root "packages/nested/library"
    (publish ~mkdir:0o755 root "packages/nested/library" "/selected");
  Alcotest.(check (list string))
    "outside stays empty" []
    (Eio.Path.read_dir (root / "packages/outside"))

let test_directory_leaf () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "library");
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "library/kept")
    "nested bytes";
  refusal ~kind:"directory" root "library" (publish root "library" "/selected");
  Alcotest.(check string)
    "directory content preserved" "nested bytes"
    (Eio.Path.load (root / "library/kept"))

let test_fifo_leaf () =
  with_dir @@ fun root ->
  let path = Eio.Path.native_exn (root / "library") in
  Unix.mkfifo path 0o600;
  refusal ~kind:"foreign filesystem entry" root "library"
    (publish root "library" "/selected");
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_FIFO -> ()
  | _ -> Alcotest.fail "the FIFO was replaced"

let test_parent_race () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "packages");
  Eio.Path.mkdir ~perm:0o755 (root / "outside");
  publication "held descriptor" Link.Created
    (Link.with_parent ~root ~path:"packages/library" (fun parent ->
         Eio.Path.rename (root / "packages") (root / "held");
         Eio.Path.symlink ~link_to:"outside" (root / "packages");
         Link.publish parent ~target:"/selected"));
  Alcotest.(check string)
    "held directory receives link" "/selected"
    (Eio.Path.read_link (root / "held/library"));
  Alcotest.(check (list string))
    "outside unchanged" []
    (Eio.Path.read_dir (root / "outside"))

let test_leaf_race () =
  with_dir @@ fun root ->
  refusal root "library"
    (Link.with_parent ~root ~path:"library" (fun parent ->
         Eio.Path.save ~create:(`Exclusive 0o600) (root / "library")
           "racing bytes";
         Link.publish parent ~target:"/selected"));
  Alcotest.(check string)
    "racing writer preserved" "racing bytes"
    (Eio.Path.load (root / "library"))

let test_admission () =
  with_dir @@ fun root ->
  List.iter
    (fun path ->
      match publish ~mkdir:0o755 root path "/selected" with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "accepted %S" path)
    [ ""; "/absolute"; "."; ".."; "a//b"; "a/./b"; "a/../b"; "a/"; "a\000b" ];
  Alcotest.(check (list string))
    "no admission side effects" [] (Eio.Path.read_dir root)

let suite =
  ( "link",
    List.map
      (fun (name, fn) -> Alcotest.test_case name `Quick fn)
      [
        ("missing parents", test_missing_parents);
        ("existing parent", test_existing_parent);
        ("non-directory parent refuses before callback", test_parent_file);
        ("nested symlink ancestor refuses", test_nested_link);
        ("missing parent refuses", test_missing_refusal);
        ("symlink ancestor refuses", test_ancestor_link);
        ("foreign leaf preserves bytes", test_foreign_leaf);
        ("wrong symlink refuses", test_wrong_link);
        ("directory leaf preserves contents", test_directory_leaf);
        ("FIFO leaf refuses", test_fifo_leaf);
        ("exact symlink no-op", test_existing_exact);
        ("parent replacement race", test_parent_race);
        ("foreign leaf race", test_leaf_race);
        ("path admission", test_admission);
      ] )
