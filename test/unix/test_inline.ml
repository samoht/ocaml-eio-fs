(* Each call through [Eio_fs_unix.inline_fs env] against the same call
   through [Eio.Stdenv.fs env]: the same answer, or the same error as eio-fs
   promises one, which says the operation that failed, on which path, and
   why. *)

let err fmt = Fmt.kstr (fun s -> Error s) fmt

(* [e] as eio-fs promises it: the operation and the path, which are the
   context Eio.Path adds, the kind of error and its errno. The system call a
   backend names and its argument are not part of the promise: eio_linux
   names statx and openat2, with an empty argument, where eio_posix and
   inline_fs name fstatat, openat and opendir, with the path. A comparison
   of the printed exception would catch inline_fs reporting another errno
   or another operation, and not a backend's choice of call. *)
let promised = function
  | Eio.Io (err, context) ->
      let errno = function
        | Eio_unix.Unix_error (code, _, _) -> Eio_unix.Unix_error (code, "", "")
        | b -> b
      in
      let err : Eio.Exn.err =
        match err with
        | Eio.Fs.E (Already_exists b) -> Eio.Fs.E (Already_exists (errno b))
        | Eio.Fs.E (Not_found b) -> Eio.Fs.E (Not_found (errno b))
        | Eio.Fs.E (Permission_denied b) ->
            Eio.Fs.E (Permission_denied (errno b))
        | Eio.Exn.X b -> Eio.Exn.X (errno b)
        | err -> err
      in
      Eio.Io (err, context)
  | e -> e

let outcome f =
  match f () with
  | v -> Ok v
  | exception (Eio.Io _ as e) -> err "%a" Eio.Exn.pp (promised e)

let pp_stat ppf (s : Eio.File.Stat.t) = Eio.File.Stat.pp ppf s
let stat_t = Alcotest.(result (testable pp_stat ( = )) string)
let string_r = Alcotest.(result string string)
let unit_r = Alcotest.(result unit string)

let with_tree fn =
  Eio_main.run @@ fun env ->
  let base = Filename.temp_dir "eio-fs-inline" "" in
  let fs = Eio.Stdenv.fs env and inline = Eio_fs_unix.inline_fs env in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / base))
    (fun () ->
      let path name = Filename.concat base name in
      Out_channel.with_open_bin (path "file") (fun oc ->
          output_string oc "data");
      Unix.mkdir (path "dir") 0o755;
      Unix.symlink "file" (path "link");
      Unix.symlink "missing" (path "dangling");
      fn ~fs ~inline path)

let names = [ "file"; "dir"; "dir/"; "link"; "dangling"; "missing"; "file/" ]

let test_stat () =
  with_tree @@ fun ~fs ~inline path ->
  List.iter
    (fun name ->
      List.iter
        (fun follow ->
          let stat root () =
            Eio.Path.stat ~follow Eio.Path.(root / path name)
          in
          Alcotest.check stat_t
            (Fmt.str "stat ~follow:%b %s" follow name)
            (outcome (stat fs))
            (outcome (stat inline)))
        [ true; false ])
    names

let test_open_in () =
  with_tree @@ fun ~fs ~inline path ->
  List.iter
    (fun name ->
      List.iter
        (fun follow ->
          let load root () =
            Eio.Path.with_open_in ~follow
              Eio.Path.(root / path name)
              (fun f ->
                let buf = Cstruct.create 16 in
                match Eio.Flow.single_read f buf with
                | n -> Cstruct.to_string ~len:n buf
                | exception End_of_file -> "")
          in
          Alcotest.check string_r
            (Fmt.str "open_in ~follow:%b %s" follow name)
            (outcome (load fs))
            (outcome (load inline)))
        [ true; false ])
    [ "file"; "link"; "dangling"; "missing"; "file/" ]

let creates : (string * Eio.Fs.create) list =
  [
    ("never", `Never);
    ("if_missing", `If_missing 0o600);
    ("or_truncate", `Or_truncate 0o640);
    ("exclusive", `Exclusive 0o644);
  ]

(* Each create mode and append, on a file that exists and one that does not,
   each in a tree of its own: the bytes left and the mode a new file gets. *)
let test_open_out () =
  List.iter
    (fun (mode, create) ->
      List.iter
        (fun append ->
          List.iter
            (fun name ->
              let write root path =
                outcome (fun () ->
                    Eio.Path.with_open_out ~append ~create
                      Eio.Path.(root / path name)
                      (fun f -> Eio.Flow.copy_string "new" f);
                    let st = Unix.stat (path name) in
                    Fmt.str "%s %o"
                      (In_channel.with_open_bin (path name) In_channel.input_all)
                      (st.st_perm land 0o777))
              in
              let posix = with_tree (fun ~fs ~inline:_ path -> write fs path) in
              let ours =
                with_tree (fun ~fs:_ ~inline path -> write inline path)
              in
              let strip =
                Result.map_error (fun e ->
                    (* The two trees are two temporary directories. *)
                    match String.index_opt e '(' with
                    | Some i -> String.sub e 0 i
                    | None -> e)
              in
              Alcotest.check string_r
                (Fmt.str "open_out %s ~append:%b %s" mode append name)
                (strip posix) (strip ours))
            [ "file"; "new"; "link"; "dir" ])
        [ false; true ])
    creates

let strings_r = Alcotest.(result (list string) string)

let pp_entry ppf ((kind : Eio.File.Stat.kind), name) =
  Fmt.pf ppf "%s %a" name Eio.File.Stat.pp_kind kind

let entries_r = Alcotest.(result (list (testable pp_entry ( = ))) string)

(* The listing of a directory, a file, a link to one, a dangling link and a
   missing name, names alone and with their kinds: the same entries, or the
   same error. *)
let test_read_dir () =
  with_tree @@ fun ~fs ~inline path ->
  List.iter
    (fun name ->
      let listing root () = Eio.Path.read_dir Eio.Path.(root / path name) in
      Alcotest.check strings_r
        (Fmt.str "read_dir %s" name)
        (outcome (listing fs))
        (outcome (listing inline));
      let entries root () =
        Eio.Path.read_dir_entries Eio.Path.(root / path name)
      in
      Alcotest.check entries_r
        (Fmt.str "read_dir_entries %s" name)
        (outcome (entries fs))
        (outcome (entries inline)))
    (names @ [ "" ])

(* Everything else is the root's own: a directory opened from it, a rename
   on it. *)
let test_rest () =
  with_tree @@ fun ~fs:_ ~inline path ->
  Eio.Switch.run @@ fun sw ->
  let dir = Eio.Path.open_dir ~sw Eio.Path.(inline / path "dir") in
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(dir / "inner") "x";
  Alcotest.(check string)
    "a file in the opened directory" "x"
    (Eio.Path.load Eio.Path.(inline / path "dir/inner"));
  Alcotest.check unit_r "the opened directory stays confined" (Error "")
    (Result.map_error
       (fun _ -> "")
       (outcome (fun () -> ignore (Eio.Path.load Eio.Path.(dir / "../file")))));
  Eio.Path.rename
    Eio.Path.(inline / path "dir/inner")
    Eio.Path.(inline / path "renamed");
  Alcotest.(check (list string))
    "the listing after a rename"
    [ "dangling"; "dir"; "file"; "link"; "renamed" ]
    (List.sort String.compare (Eio.Path.read_dir Eio.Path.(inline / path "")))

(* A call that hands off suspends its fiber, so another runs before it
   returns. *)
let test_no_hop () =
  with_tree @@ fun ~fs ~inline path ->
  let hops root =
    let ran = ref false in
    fst
      (Eio.Fiber.pair
         (fun () ->
           ignore (Eio.Path.stat ~follow:true Eio.Path.(root / path "file"));
           Eio.Path.with_open_in Eio.Path.(root / path "file") ignore;
           ignore (Eio.Path.read_dir Eio.Path.(root / path "dir"));
           ignore (Eio.Path.read_dir_entries Eio.Path.(root / path "dir"));
           !ran)
         (fun () -> ran := true))
  in
  Alcotest.(check bool) "the root hands off (control)" true (hops fs);
  Alcotest.(check bool) "inline_fs does not" false (hops inline)

let suite =
  ( "inline",
    [
      Alcotest.test_case "stat" `Quick test_stat;
      Alcotest.test_case "open_in" `Quick test_open_in;
      Alcotest.test_case "open_out" `Quick test_open_out;
      Alcotest.test_case "read_dir" `Quick test_read_dir;
      Alcotest.test_case "the root's other calls" `Quick test_rest;
      Alcotest.test_case "no system-thread hop" `Quick test_no_hop;
    ] )
