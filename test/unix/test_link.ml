(* POSIX openat(2), O_NOFOLLOW and symlinkat(2): authority is the opened
   directory, and an existing destination must never be overwritten. *)
module Link = Eio_fs_unix.Link

let ( / ) = Eio.Path.( / )

(* [fn] gets the test's directory opened: a path beneath it is a name
   [with_parent] resolves one component at a time. *)
let with_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_dir "eio-fs-link" "" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / name))
    (fun () -> Eio.Path.with_open_dir (fs / name) fn)

let get = function Ok value -> value | Error message -> Alcotest.fail message

let exists path =
  match Eio.Path.stat ~follow:false path with
  | _ -> true
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

let publish ?create root path target =
  Link.with_parent ?create (root / path) (fun parent ->
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

(* A parent refused raises before the callback, naming the path. *)
let refused_parent root path fn =
  match fn () with
  | _ -> Alcotest.failf "%s was not refused" path
  | exception (Eio.Io _ as e) ->
      Alcotest.(check bool)
        "names the path" true
        (contains (Eio_fs.Path.name (root / path)) (Printexc.to_string e))

let mkdir = `If_missing 0o755

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
    (publish ~create:mkdir root "packages/engine/library" "/selected");
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
  refused_parent root "missing/library" (fun () ->
      publish root "missing/library" "/selected");
  Alcotest.(check bool) "no parent created" false (exists (root / "missing"))

let test_ancestor_link () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "outside");
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "outside/kept")
    "outside bytes";
  Eio.Path.symlink ~link_to:"outside" (root / "packages");
  refused_parent root "packages/library" (fun () ->
      publish ~create:mkdir root "packages/library" "/selected");
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
  refused_parent root "parent/library" (fun () ->
      Link.with_parent ~create:mkdir (root / "parent/library") (fun _ ->
          called := true));
  Alcotest.(check bool) "no callback for a non-directory" false !called;
  Alcotest.(check string)
    "parent bytes preserved" "parent bytes"
    (Eio.Path.load (root / "parent"))

let test_nested_link () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "packages");
  Eio.Path.mkdir ~perm:0o755 (root / "packages/outside");
  Eio.Path.symlink ~link_to:"outside" (root / "packages/nested");
  refused_parent root "packages/nested/library" (fun () ->
      publish ~create:mkdir root "packages/nested/library" "/selected");
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
    (Link.with_parent (root / "packages/library") (fun parent ->
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
    (Link.with_parent (root / "library") (fun parent ->
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
      match publish ~create:mkdir root path "/selected" with
      | Error _ | (exception Eio.Io _) -> ()
      | Ok _ -> Alcotest.failf "accepted %S" path)
    [ ""; "/absolute"; "."; ".."; "a//b"; "a/./b"; "a/../b"; "a/"; "a\000b" ];
  Alcotest.(check (list string))
    "no admission side effects" [] (Eio.Path.read_dir root)

let read ?(max_bytes = 1024) root path =
  Link.with_parent (root / path) (Link.read_regular ~max_bytes)

let read_refused ~kind root path =
  match read root path with
  | Ok _ -> Alcotest.failf "read %S" path
  | Error message ->
      Alcotest.(check bool) kind true (contains kind message);
      Alcotest.(check bool)
        "names the path" true
        (contains (Eio_fs.Path.name (root / path)) message)

let test_read_regular () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "packages");
  Eio.Path.save ~create:(`Exclusive 0o640) (root / "packages/library") "bytes";
  (match get (read root "packages/library") with
  | Some { Link.content; perm } ->
      Alcotest.(check string) "the file's bytes" "bytes" content;
      Alcotest.(check int) "the descriptor's permission bits" 0o640 perm
  | None -> Alcotest.fail "a present regular file read as absent");
  Alcotest.(check bool)
    "an absent entry" true
    (Option.is_none (get (read root "packages/absent")))

let test_read_symlink_leaf () =
  with_dir @@ fun root ->
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "target") "bytes";
  Eio.Path.symlink ~link_to:"target" (root / "library");
  match read root "library" with
  | Ok _ -> Alcotest.fail "read through a symbolic link"
  | Error message ->
      Alcotest.(check bool)
        "names the path" true
        (contains (Eio_fs.Path.name (root / "library")) message)

let test_read_directory_leaf () =
  with_dir @@ fun root ->
  Eio.Path.mkdir ~perm:0o755 (root / "library");
  read_refused ~kind:"not a regular file" root "library"

(* A FIFO with no writer is refused at once: the open does not wait for one. *)
let test_read_fifo_leaf () =
  with_dir @@ fun root ->
  Unix.mkfifo (Eio.Path.native_exn (root / "library")) 0o600;
  read_refused ~kind:"not a regular file" root "library"

let test_read_too_large () =
  with_dir @@ fun root ->
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "library") "12345";
  match read ~max_bytes:4 root "library" with
  | Ok _ -> Alcotest.fail "read past the bound"
  | Error message ->
      Alcotest.(check bool) "too large" true (contains "too large" message);
      Alcotest.(check (option string))
        "at the bound" (Some "12345")
        (Option.map
           (fun (r : Link.regular) -> r.content)
           (get (read ~max_bytes:5 root "library")))

(* The bound is Nofollow's: a negative one or max_int is refused. *)
let test_read_bound () =
  with_dir @@ fun root ->
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "library") "bytes";
  List.iter
    (fun max_bytes ->
      match read ~max_bytes root "library" with
      | _ -> Alcotest.failf "accepted a bound of %d" max_bytes
      | exception Invalid_argument _ -> ())
    [ -1; max_int ]

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
        ("read a regular file", test_read_regular);
        ("read refuses a symlink leaf", test_read_symlink_leaf);
        ("read refuses a directory leaf", test_read_directory_leaf);
        ("read refuses a FIFO leaf without blocking", test_read_fifo_leaf);
        ("read refuses a file over the bound", test_read_too_large);
        ("read refuses an invalid bound", test_read_bound);
      ] )
