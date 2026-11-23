(* The in-memory filesystem against Linux and eio_posix. The stat answers
   Linux records are replayed in interop/stat. *)

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
  fn t (Eio_fs_mem.v ~clock ())

let stat p = Eio.Path.stat ~follow:false p

(* atime moves once a day even past mtime and ctime; not recorded, as it
   takes a day. *)
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

(* Without a clock, time stands at 0. *)
let test_no_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs_mem.v () in
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
  Eio_fs_chaos.run
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

(* A power cut leaves each inode's times as its version on the disk holds
   them: a file's at its last sync, a directory's at the last change to it
   the disk kept. *)
let test_crash_times () =
  with_fs @@ fun t root ->
  let module Crash = Eio_fs_chaos.Power in
  t := 1.;
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "f") "a";
  let c = Crash.v Lose_unsynced in
  let d = Crash.wrap c root in
  let times p =
    let s = stat p in
    [ s.mtime; s.ctime ]
  in
  Eio.Path.with_open_out ~create:`Never (d / "f") (fun h ->
      t := 2.;
      Eio.Flow.copy_string "b" h;
      Eio.File.sync h;
      t := 3.;
      Eio.Flow.copy_string "c" h);
  t := 4.;
  Eio.Path.mkdir ~perm:0o755 (d / "sub");
  Alcotest.(check (list (float 0.))) "f, live" [ 3.; 3. ] (times (root / "f"));
  Alcotest.(check (list (float 0.))) "the root, live" [ 4.; 4. ] (times root);
  let disk = Crash.cut c in
  Alcotest.(check string) "f, after" "b" (Eio.Path.load (disk / "f"));
  Alcotest.(check (list (float 0.))) "f, after" [ 2.; 2. ] (times (disk / "f"));
  Alcotest.(check (list (float 0.))) "the root, after" [ 1.; 1. ] (times disk)

(* Absolute symbolic links, in memory and through eio_posix: the root
   [Eio_fs_mem.v] returns is [Eio.Stdenv.fs], which follows one from the
   root, and a directory opened from it is confined, refusing to follow one
   with [Permission_denied] in every operation that would. *)

let outcome f =
  match f () with
  | v -> "ok " ^ v
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
      "permission denied"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> "not found"
  | exception Eio.Io _ -> "error"

let kind p =
  Fmt.str "%a" Eio.File.Stat.pp_kind (Eio.Path.stat ~follow:true p).kind

let unit f () =
  f ();
  ""

(* [dir] holds [abs], a link to the file [top ^ "/s/target"], and [absdir],
   one to the directory [top ^ "/s/tdir"]. *)
let ops (dir : _ Eio.Path.t) =
  let p x = dir / x in
  [
    ("load", fun () -> Eio.Path.load (p "abs"));
    ("stat", fun () -> kind (p "abs"));
    ( "stat, not followed",
      fun () ->
        Fmt.str "%a" Eio.File.Stat.pp_kind
          (Eio.Path.stat ~follow:false (p "abs")).kind );
    ("read_link", fun () -> Filename.basename (Eio.Path.read_link (p "abs")));
    ("save", unit (fun () -> Eio.Path.save ~create:`Never (p "abs") "T"));
    ("chmod", unit (fun () -> Eio.Path.chmod ~follow:true ~perm:0o644 (p "abs")));
    ("read_dir", fun () -> String.concat "," (Eio.Path.read_dir (p "absdir")));
    ( "create below",
      unit (fun () ->
          Eio.Path.save ~create:(`Exclusive 0o644) (p "absdir/x") "x") );
    ("stat below", fun () -> kind (p "absdir/x"));
    ("unlink below", unit (fun () -> Eio.Path.unlink (p "absdir/x")));
    ("mkdir below", unit (fun () -> Eio.Path.mkdir ~perm:0o755 (p "absdir/m")));
    ("rmdir below", unit (fun () -> Eio.Path.rmdir (p "absdir/m")));
    ( "symlink below",
      unit (fun () -> Eio.Path.symlink ~link_to:"z" (p "absdir/y")) );
    ( "rename below",
      unit (fun () -> Eio.Path.rename (p "absdir/y") (p "absdir/y2")) );
    ("unlink renamed", unit (fun () -> Eio.Path.unlink (p "absdir/y2")));
    ( "open_dir",
      unit (fun () ->
          Eio.Switch.run @@ fun sw ->
          ignore (Eio.Path.open_dir ~sw (p "absdir"))) );
  ]

let run_ops dir = List.map (fun (n, f) -> n ^ ": " ^ outcome f) (ops dir)

(* What [fs] at [top], and a directory opened there, answer. *)
let answers fs top =
  let s = fs / (top ^ "/s") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 s;
  Eio.Path.save ~create:(`Exclusive 0o644) (s / "target") "T";
  Eio.Path.mkdir ~perm:0o755 (s / "tdir");
  Eio.Path.symlink ~link_to:(top ^ "/s/target") (s / "abs");
  Eio.Path.symlink ~link_to:(top ^ "/s/tdir") (s / "absdir");
  let unconfined = run_ops s in
  let confined =
    Eio.Switch.run @@ fun sw -> run_ops (Eio.Path.open_dir ~sw s)
  in
  (unconfined, confined)

(* eio_posix, which Windows lacks: there the test is skipped. *)
let eio_posix f =
  match Posix.run with Some run -> run f | None -> Alcotest.skip ()

let test_as_eio_posix () =
  eio_posix @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let top = Filename.temp_file "eio-fs-absolute-link" "" in
  Sys.remove top;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / top))
  @@ fun () ->
  let posix = answers fs top in
  let mem = answers (Eio_fs_mem.v ()) top in
  let check what (p, m) = Alcotest.(check (list string)) what p m in
  check "Eio.Stdenv.fs and the root" (fst posix, fst mem);
  check "an opened directory" (snd posix, snd mem);
  let ok s =
    match String.index_opt s ':' with
    | Some i ->
        String.starts_with ~prefix:": ok" (String.sub s i (String.length s - i))
    | None -> false
  in
  Alcotest.(check (list string))
    "the root follows" []
    (List.filter (fun s -> not (ok s)) (fst mem));
  Alcotest.(check (list string))
    "an opened directory refuses to follow"
    [ "stat, not followed"; "read_link" ]
    (List.filter_map
       (fun s ->
         if ok s then Some (List.hd (String.split_on_char ':' s)) else None)
       (snd mem))

(* A file is no socket: eio_posix's handle binds no shutdown, and neither
   does the in-memory backend's. *)
let test_shutdown_as_eio_posix () =
  (* Files are no flows of type [two_way_ty]: their shutdown is reached
     through the handler. *)
  let shutdown (type t) f (ops : t Eio.Resource.binding list) =
    match
      List.find_map
        (fun (Eio.Resource.H (pi, impl) : t Eio.Resource.binding) ->
          match pi with
          | Eio.Flow.Pi.Shutdown ->
              Some (impl : (module Eio.Flow.Pi.SHUTDOWN with type t = t))
          | _ -> None)
        ops
    with
    | None -> "no shutdown"
    | Some (module S) -> (
        match S.shutdown f `All with
        | () -> "ok"
        | exception Eio.Io (Eio.Exn.X _, _) -> "Eio.Io")
  in
  let outcome dir =
    Eio.Path.save ~create:(`Or_truncate 0o644) (dir / "f") "x";
    Eio.Path.with_open_out ~create:`Never (dir / "f")
    @@ fun (Eio.Resource.T (f, ops)) -> shutdown f (Eio.Resource.bindings ops)
  in
  eio_posix @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let top = Filename.temp_file "eio-fs-shutdown" "" in
  Sys.remove top;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / top))
  @@ fun () ->
  Eio.Path.mkdir ~perm:0o755 (fs / top);
  Alcotest.(check string) "eio_posix" "no shutdown" (outcome (fs / top));
  Alcotest.(check string)
    "the in-memory backend" "no shutdown"
    (outcome (Eio_fs_mem.v () / "."))

(* An error Eio does not classify carries the code Eio_fs_mem exports, and
   one it classifies carries it too, so that a handler can tell the
   in-memory filesystem's errors apart. *)
let test_error_code () =
  with_fs @@ fun _ fs ->
  Eio.Path.mkdirs ~perm:0o755 (fs / "d/e");
  (match Eio.Path.rmdir (fs / "d") with
  | () -> Alcotest.fail "removed a directory that is not empty"
  | exception Eio.Io (Eio.Exn.X (Eio_fs_mem.Eio_fs what), _) ->
      Alcotest.(check string) "what" "rmdir: directory not empty: /d" what);
  match Eio.Path.load (fs / "missing") with
  | _ -> Alcotest.fail "loaded a missing file"
  | exception Eio.Io (Eio.Fs.E (Not_found (Eio_fs_mem.Eio_fs _)), _) -> ()

(* Under Windows the root is the first drive's, spelt as eio_windows spells
   paths, each drive given is one, by any case, and only Windows has
   drives. *)
let test_windows_drives () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs_mem.v ~os:`Windows ~drives:[ 'C'; 'D' ] () in
  Alcotest.(check string) "root" {|C:\|} (snd root);
  Alcotest.(check (option string))
    "native" (Some {|C:\a|})
    (Eio.Path.native (root / "a"));
  Eio.Path.save ~create:(`Exclusive 0o644) (root / {|D:\x|}) "d";
  Alcotest.(check string) "on D:" "d" (Eio.Path.load (fst root, {|d:\X|}));
  (match Eio.Path.load (fst root, {|E:\x|}) with
  | _ -> Alcotest.fail "a drive not given"
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ());
  match Eio_fs_mem.v ~os:`Linux ~drives:[ 'C' ] () with
  | _ -> Alcotest.fail "a drive under Linux"
  | exception Invalid_argument _ -> ()

(* A program that saves, renames and reads a file meets no refusal under
   Windows, a power cut included, and one that holds a file open while it
   renames it is refused. *)
let test_windows_run () =
  Eio_fs_chaos.run ~os:`Windows (fun env ->
      let cwd = Eio.Stdenv.cwd env in
      Eio.Path.save ~create:(`Or_truncate 0o644) (cwd / "tmp") "a";
      Eio.Path.rename (cwd / "tmp") (cwd / "final");
      ignore (Eio.Path.load (cwd / "FINAL")));
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run ~os:`Windows @@ fun env ->
  let f = env#cwd / "f" in
  Eio.Path.save ~create:(`Or_truncate 0o644) f "a";
  Eio.Switch.run @@ fun sw ->
  ignore (Eio.Path.open_in ~sw f);
  match Eio.Path.rename f (env#cwd / "g") with
  | () -> Alcotest.fail "renamed a file held open"
  | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let test_stdio () =
  Eio_mock.Backend.run @@ fun () ->
  let stdin =
    Eio.Flow.cstruct_source Cstruct.[ of_string "hello"; of_string " world" ]
  in
  let buf = Buffer.create 16 in
  let stdout = Eio.Flow.buffer_sink buf in
  Eio_fs_mem.run ~stdin ~stdout @@ fun env ->
  Eio.Flow.copy env#stdin env#stdout;
  Alcotest.(check string) "same output" "hello world" (Buffer.contents buf)

let test_pipe () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio.Process.pipe ~sw env#process_mgr in
  Eio.Flow.copy_string "hello, world!" w;
  Eio.Flow.close w;
  let buf = Buffer.create 16 in
  Eio.Flow.copy r (Eio.Flow.buffer_sink buf);
  Alcotest.(check string) "same output" "hello, world!" (Buffer.contents buf)

let test_fd_resource () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (env#fs / "hello.txt")
  @@ fun sink ->
  match Eio_fs_mem.Resource.fd_opt sink with
  | Some _ -> ()
  | None -> Alcotest.fail "Expected an FD on open file"

let suite =
  ( "memfs",
    [
      Alcotest.test_case "errors carry Eio_fs_mem.Eio_fs" `Quick test_error_code;
      Alcotest.test_case "relatime moves atime once a day" `Quick
        test_relatime_day;
      Alcotest.test_case "a file binds no shutdown, as on eio_posix" `Quick
        test_shutdown_as_eio_posix;
      Alcotest.test_case "without a clock, time stands at 0" `Quick
        test_no_clock;
      Alcotest.test_case "a freshness check under Chaos.run" `Quick
        test_freshness_under_check;
      Alcotest.test_case "a power cut keeps the times the disk kept" `Quick
        test_crash_times;
      Alcotest.test_case "absolute links as eio_posix" `Quick test_as_eio_posix;
      Alcotest.test_case "Windows: drives" `Quick test_windows_drives;
      Alcotest.test_case "Windows: a run and a file held open" `Quick
        test_windows_run;
      Alcotest.test_case "standard IO behaves normally" `Quick test_stdio;
      Alcotest.test_case "pipes work" `Quick test_pipe;
      Alcotest.test_case "accessing underlying FDs works" `Quick
        test_fd_resource;
    ] )
