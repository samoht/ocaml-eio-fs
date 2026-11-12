(* The in-memory filesystem against Linux and eio_posix. A stat answers
   times, link counts and block counts as Linux gives them: the expected
   answers were measured with eio_posix on tmpfs and on ext4 (Linux 7.2,
   relatime), which agree on every line. *)

let ( / ) = Eio.Path.( / )

(* A clock that moves only when the test moves it. *)
module Manual = struct
  type t = float ref
  type time = float

  let now t = !t
  let sleep_until _ _ = invalid_arg "Manual.sleep_until"
end

let manual () =
  let t = ref 0. in
  (t, Eio.Resource.T (t, Eio.Time.Pi.clock (module Manual)))

let with_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  let t, clock = manual () in
  fn t (Eio_fs.Fs.v ~clock ())

let stat p = Eio.Path.stat ~follow:false p

(* Which of atime, mtime and ctime moved, and how the link count did. *)
let moved (b : Eio.File.Stat.t) (a : Eio.File.Stat.t) =
  (if a.atime <> b.atime then "a" else "")
  ^ (if a.mtime <> b.mtime then "m" else "")
  ^ (if a.ctime <> b.ctime then "c" else "")
  ^ if a.nlink <> b.nlink then Fmt.str " nlink %Ld->%Ld" b.nlink a.nlink else ""

(* Each operation runs a second after the last one; the answer is what moved
   on each watched path that exists before and after it. *)
let script t (watched : (string * _ Eio.Path.t) list) ops =
  let snap () =
    List.map
      (fun (n, p) -> (n, try Some (stat p) with Eio.Io _ -> None))
      watched
  in
  List.map
    (fun (label, op) ->
      t := !t +. 1.;
      let before = snap () in
      (try op () with End_of_file -> ());
      let changes =
        List.filter_map
          (fun ((n, b), (_, a)) ->
            match (b, a) with
            | Some b, Some a when moved b a <> "" -> Some (n ^ ":" ^ moved b a)
            | _ -> None)
          (List.combine before (snap ()))
      in
      String.concat " " (label :: changes))
    ops

let linux_times =
  [
    "create f d:mc";
    "write 3 f:mc";
    "write 0";
    "pwrite 1 f:mc";
    "truncate to the same size f:mc";
    "truncate 10 f:mc";
    "fsync";
    "fstat";
    "read 1 f:a";
    "read 1 again";
    "pread at the end";
    "open to truncate f:mc";
    "open to truncate, empty f:mc";
    "open to read";
    "open to write";
    "chmod to the same mode f:c";
    "chmod 600 f:c";
    "chown to the same owner f:c";
    "chown nothing f:c";
    "stat";
    "mkdir sub d:mc nlink 2->3";
    "symlink l d:mc";
    "readlink l l:a";
    "readlink l again";
    "read_dir d d:a";
    "read_dir d again";
    "rename f g d:mc";
    "rename g f d:mc";
    "rename d/sub e/sub d:mc nlink 3->2 e:mc nlink 2->3";
    "rename e/sub d/sub d:mc nlink 2->3 e:mc nlink 3->2";
    "rename d/sub d/sub";
    "rmdir sub d:mc nlink 3->2";
    "unlink l d:mc";
  ]

let test_times () =
  with_fs @@ fun t root ->
  let d = root / "d" and e = root / "e" in
  Eio.Path.mkdir ~perm:0o755 d;
  Eio.Path.mkdir ~perm:0o755 e;
  let f = d / "f" and g = d / "g" and l = d / "l" and sub = d / "sub" in
  let int = Optint.Int63.of_int in
  Eio.Switch.run @@ fun sw ->
  let wf = ref None and rf = ref None in
  let w () = Option.get !wf and r () = Option.get !rf in
  let open_w () = wf := Some (Eio.Path.open_out ~sw ~create:`Never f) in
  let open_r () = rf := Some (Eio.Path.open_in ~sw f) in
  let got =
    script t
      [ ("d", d); ("e", e); ("f", f); ("l", l) ]
      [
        ("create f", fun () -> Eio.Path.save ~create:(`Exclusive 0o644) f "");
        ( "write 3",
          fun () ->
            open_w ();
            Eio.Flow.copy_string "abc" (w ()) );
        ("write 0", fun () -> ignore (Eio.Flow.single_write (w ()) []));
        ( "pwrite 1",
          fun () ->
            Eio.File.pwrite_all (w ()) ~file_offset:(int 1)
              [ Cstruct.of_string "x" ] );
        ("truncate to the same size", fun () -> Eio.File.truncate (w ()) (int 3));
        ("truncate 10", fun () -> Eio.File.truncate (w ()) (int 10));
        ("fsync", fun () -> Eio.File.sync (w ()));
        ("fstat", fun () -> ignore (Eio.File.stat (w ())));
        ( "read 1",
          fun () ->
            open_r ();
            ignore (Eio.Flow.single_read (r ()) (Cstruct.create 1)) );
        ( "read 1 again",
          fun () -> ignore (Eio.Flow.single_read (r ()) (Cstruct.create 1)) );
        ( "pread at the end",
          fun () ->
            ignore
              (Eio.File.pread (r ()) ~file_offset:(int 100)
                 [ Cstruct.create 1 ]) );
        ( "open to truncate",
          fun () ->
            Eio.Switch.run @@ fun sw ->
            ignore (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) f) );
        ( "open to truncate, empty",
          fun () ->
            Eio.Switch.run @@ fun sw ->
            ignore (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) f) );
        ( "open to read",
          fun () -> Eio.Switch.run @@ fun sw -> ignore (Eio.Path.open_in ~sw f)
        );
        ( "open to write",
          fun () ->
            Eio.Switch.run @@ fun sw ->
            ignore (Eio.Path.open_out ~sw ~create:`Never f) );
        ( "chmod to the same mode",
          fun () -> Eio.Path.chmod ~follow:false ~perm:0o644 f );
        ("chmod 600", fun () -> Eio.Path.chmod ~follow:false ~perm:0o600 f);
        ( "chown to the same owner",
          fun () -> Eio.Path.chown ~follow:false ~uid:0L ~gid:0L f );
        ("chown nothing", fun () -> Eio.Path.chown ~follow:false f);
        ("stat", fun () -> ignore (Eio.Path.stat ~follow:true f));
        ("mkdir sub", fun () -> Eio.Path.mkdir ~perm:0o755 sub);
        ("symlink l", fun () -> Eio.Path.symlink ~link_to:"f" l);
        ("readlink l", fun () -> ignore (Eio.Path.read_link l));
        ("readlink l again", fun () -> ignore (Eio.Path.read_link l));
        ("read_dir d", fun () -> ignore (Eio.Path.read_dir d));
        ("read_dir d again", fun () -> ignore (Eio.Path.read_dir d));
        ("rename f g", fun () -> Eio.Path.rename f g);
        ("rename g f", fun () -> Eio.Path.rename g f);
        ("rename d/sub e/sub", fun () -> Eio.Path.rename sub (e / "sub"));
        ("rename e/sub d/sub", fun () -> Eio.Path.rename (e / "sub") sub);
        ("rename d/sub d/sub", fun () -> Eio.Path.rename sub sub);
        ("rmdir sub", fun () -> Eio.Path.rmdir sub);
        ("unlink l", fun () -> Eio.Path.unlink l);
      ]
  in
  Alcotest.(check (list string)) "as Linux" linux_times got

(* The entry a rename moves, the one it replaces and the one an unlink
   removes, each stat'ed through a descriptor open on it. *)
let test_inode_times () =
  with_fs @@ fun t root ->
  let f = root / "f" and g = root / "g" and h = root / "h" in
  List.iter
    (fun p -> Eio.Path.save ~create:(`Exclusive 0o644) p "x")
    [ f; g; h ];
  Eio.Switch.run @@ fun sw ->
  let fd p = Eio.Path.open_in ~sw p in
  let ff = fd f and gf = fd g and hf = fd h in
  let before = List.map Eio.File.stat [ ff; gf; hf ] in
  t := 1.;
  Eio.Path.rename f g;
  Eio.Path.unlink h;
  let after = List.map Eio.File.stat [ ff; gf; hf ] in
  Alcotest.(check (list string))
    "moved, replaced, unlinked"
    [ "c"; "c nlink 1->0"; "c nlink 1->0" ]
    (List.map2 moved before after)

(* atime moves once a day even past mtime and ctime. *)
let test_relatime_day () =
  with_fs @@ fun t root ->
  let f = root / "f" in
  Eio.Path.save ~create:(`Exclusive 0o644) f "x";
  t := 1.;
  ignore (Eio.Path.load f);
  t := 2.;
  ignore (Eio.Path.load f);
  Alcotest.(check (float 0.)) "not again" 1. (stat f).atime;
  t := 86401.;
  ignore (Eio.Path.load f);
  Alcotest.(check (float 0.)) "a day later" 86401. (stat f).atime

let test_nlink_blocks () =
  with_fs @@ fun _ root ->
  let d = root / "d" in
  Eio.Path.mkdir ~perm:0o755 d;
  let p = ( / ) d in
  Eio.Path.mkdir ~perm:0o755 (p "s1");
  Eio.Path.mkdir ~perm:0o755 (p "s2");
  Eio.Path.save ~create:(`Exclusive 0o644) (p "one") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (p "empty") "";
  Eio.Path.save ~create:(`Exclusive 0o644) (p "big") (String.make 5000 'x');
  ( Eio.Switch.run @@ fun sw ->
    let int = Optint.Int63.of_int in
    let file n = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (p n) in
    Eio.File.pwrite_all (file "sparse") ~file_offset:(int 1_000_000)
      [ Cstruct.of_string "y" ];
    Eio.File.truncate (file "trunc") (int 100_000);
    let cut = file "cut" in
    Eio.File.pwrite_all cut ~file_offset:Optint.Int63.zero
      [ Cstruct.of_string (String.make 9000 'z') ];
    Eio.File.truncate cut (int 4097) );
  Eio.Path.symlink ~link_to:"one" (p "ln");
  let row n =
    let s = stat (p n) in
    Fmt.str "%s nlink=%Ld blocks=%Ld" n s.nlink s.blocks
  in
  Alcotest.(check (list string))
    "as tmpfs"
    [
      "one nlink=1 blocks=8";
      "empty nlink=1 blocks=0";
      "big nlink=1 blocks=16";
      "sparse nlink=1 blocks=8";
      "trunc nlink=1 blocks=0";
      "cut nlink=1 blocks=16";
      "ln nlink=1 blocks=0";
      "s1 nlink=2 blocks=0";
    ]
    (List.map row
       [ "one"; "empty"; "big"; "sparse"; "trunc"; "cut"; "ln"; "s1" ]);
  Alcotest.(check int64) "a directory" 4L (stat d).nlink;
  Alcotest.(check int64) "the root" 3L (stat root).nlink;
  Alcotest.(check int64) "usage" 24576L (Eio_fs.Path.usage d).allocated_bytes

(* Without a clock, time stands at 0. *)
let test_no_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "f") "x";
  Eio.Path.chmod ~follow:false ~perm:0o600 (root / "f");
  let s = stat (root / "f") in
  Alcotest.(check (list (float 0.)))
    "times" [ 0.; 0.; 0. ]
    [ s.atime; s.mtime; s.ctime ]

(* make(1)'s rule: a target older than its source is rebuilt. *)
let build fs =
  let src = fs / "src" and out = fs / "out" in
  let fresh =
    match stat out with
    | o -> (stat src).mtime <= o.mtime
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false
  in
  if not fresh then
    Eio.Path.save ~create:(`Or_truncate 0o644) out
      (String.uppercase_ascii (Eio.Path.load src))

(* A source edited after its target is built is seen newer, under faults:
   an error ends the run, which then checks nothing. *)
let test_freshness_under_check () =
  Eio_fs.Chaos.check
    ~setup:(fun env ->
      Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "src") "a")
    (fun env ->
      match
        build env#fs;
        Eio.Time.sleep env#clock 1.;
        Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "src") "b";
        build env#fs;
        Eio.Path.load (env#fs / "out")
      with
      | "B" -> ()
      | got -> Fmt.failwith "out holds %S, built from a stale source" got
      | exception Eio.Io _ -> ())

let suite =
  ( "memfs",
    [
      Alcotest.test_case "times move as on Linux" `Quick test_times;
      Alcotest.test_case "times of moved and removed inodes" `Quick
        test_inode_times;
      Alcotest.test_case "relatime moves atime once a day" `Quick
        test_relatime_day;
      Alcotest.test_case "link and block counts" `Quick test_nlink_blocks;
      Alcotest.test_case "without a clock, time stands at 0" `Quick
        test_no_clock;
      Alcotest.test_case "a freshness check under Chaos.check" `Quick
        test_freshness_under_check;
    ] )
