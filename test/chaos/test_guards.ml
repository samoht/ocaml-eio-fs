(* The guards of a check, its determinism, its budget and its replay. *)

module P = Eio.Path

let save d v = App.save_config d v

let log_mutant ?budget ?replay () =
  After.test_b ?budget ?replay
    ~append:(App.Log.append ~bug:`No_sync)
    ~recover:(fun d -> App.Log.recover d)
    ()

let cache_mutant ?replay () =
  After.test_c ?replay ~get:(App.Cache.get ~bug:`Only_eacces) ()

let replay_of report =
  Scanf.sscanf (Expect.field "replay" report) "~replay:%S" Fun.id

let hash replay = List.nth (String.split_on_char '/' replay) 1

let same_failure a b =
  List.iter
    (fun f -> Alcotest.(check string) f (Expect.field f a) (Expect.field f b))
    [ "cut"; "faults"; "tier"; "error"; "replay" ]

let test_determinism () =
  same_failure
    (Expect.report (fun () -> log_mutant ()))
    (Expect.report (fun () -> log_mutant ()));
  same_failure
    (Expect.report (fun () -> cache_mutant ()))
    (Expect.report (fun () -> cache_mutant ()))

(* A replay runs the scenario a failure named, and fails the same way; once
   the bug is fixed, it passes. *)
let test_replay () =
  let report = Expect.report (fun () -> log_mutant ()) in
  let replay = replay_of report in
  same_failure report (Expect.report (fun () -> log_mutant ~replay ()));
  After.test_b ~replay
    ~append:(fun f r -> App.Log.append f r)
    ~recover:(fun d -> App.Log.recover d)
    ();
  let report = Expect.report (fun () -> cache_mutant ()) in
  let replay = replay_of report in
  same_failure report (Expect.report (fun () -> cache_mutant ~replay ()))

(* A replay of a state a cut allows runs that state. *)
let test_replay_state () =
  let marker () =
    After.test_b
      ~append:(fun f r -> App.Log.append f r)
      ~recover:(App.Log.recover ~bug:`Marker_only)
      ()
  in
  let report = Expect.report marker in
  let replay = replay_of report in
  same_failure report
    (Expect.report (fun () ->
         After.test_b ~replay
           ~append:(fun f r -> App.Log.append f r)
           ~recover:(App.Log.recover ~bug:`Marker_only)
           ()))

(* A save of [n] bytes that does not sync: the fix changes the size of what
   it writes, and so the workload's first run. *)
let config ?replay ~value ~sync () =
  Eio_fs.Chaos.Crash.check ?replay
    ~setup:(fun env -> App.save_config env#fs "old")
    (fun ~ack env ->
      match
        App.save_config
          ?bug:(if sync then None else Some `No_dir_sync)
          env#fs value
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      match (App.load_config env#fs, acked) with
      | v, _ when v = value -> ()
      | "old", [] -> ()
      | v, _ -> Fmt.failwith "config is %S" v)

(* A replay whose workload changed, but which still has the scenario, runs
   it and says so. *)
let test_rematched_replay () =
  let report = Expect.report (config ~value:"new" ~sync:false) in
  let replay = replay_of report in
  config ~replay ~value:"a longer new value" ~sync:true ();
  let again =
    Expect.report (config ~replay ~value:"a longer new value" ~sync:false)
  in
  Alcotest.(check string)
    "cut"
    (Expect.field "cut" report)
    (Expect.field "cut" again);
  Alcotest.(check string)
    "matched"
    (Fmt.str "the replay named first run %s, and the workload's is now %s"
       (hash replay)
       (hash (replay_of again)))
    (Expect.field "matched" again)

(* A replay whose scenario the workload no longer has is refused. *)
let test_stale_replay () =
  let replay = replay_of (Expect.report (fun () -> cache_mutant ())) in
  let key =
    String.concat "/" (List.tl (List.tl (String.split_on_char '/' replay)))
  in
  Expect.refused
    (Fmt.str
       "Eio_fs.Chaos.check: the replay names %s, a scenario this check no \
        longer has: its first run was %s and is now"
       key (hash replay))
    (fun () ->
      Eio_fs.Chaos.check ~replay (fun env ->
          try P.save ~create:(`Or_truncate 0o644) P.(env#fs / "other") "x"
          with Eio.Io _ -> ()))
    ()

let test_unknown_replay () =
  let replay = replay_of (Expect.report (fun () -> log_mutant ())) in
  Expect.refused
    "Eio_fs.Chaos.Crash.check: the replay names \
     none|before:sync:/nowhere#9|drop, a scenario this check no longer has"
    (fun () ->
      log_mutant
        ~replay:(Fmt.str "fs/%s/none|before:sync:/nowhere#9|drop" (hash replay))
        ())
    ();
  Expect.refused
    "Eio_fs.Chaos.Crash.check: ~replay:\"net/x/y\" is not a replay of eio-fs"
    (fun () -> log_mutant ~replay:"net/x/y" ())
    ()

let test_budget () =
  List.iter
    (fun budget ->
      match log_mutant ~budget () with
      | exception Invalid_argument _ -> ()
      | () | (exception Failure _) ->
          Alcotest.failf "the budget %d was accepted" budget)
    [ 0; -1 ];
  Expect.refused "Eio_fs.Chaos.Crash.check: the linear tier needs"
    (fun () -> log_mutant ~budget:5 ())
    ()

let test_nothing () =
  Expect.refused
    "Eio_fs.Chaos.check: the workload makes no filesystem operation"
    (fun () -> Eio_fs.Chaos.check ignore)
    ();
  Expect.refused
    "Eio_fs.Chaos.Crash.check: the workload makes no filesystem operation"
    (fun () ->
      Eio_fs.Chaos.Crash.check
        ~setup:(fun env ->
          P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "x")
        (fun ~ack:_ _ -> ())
        ~recover:(fun ~acked:_ env -> ignore (P.load P.(env#fs / "f"))))
    ()

let test_nondeterministic () =
  let calls = ref 0 in
  Expect.refused
    "Eio_fs.Chaos.check: two runs with no fault made different operations"
    (fun () ->
      Eio_fs.Chaos.check (fun env ->
          incr calls;
          for i = 1 to 1 + (!calls mod 2) do
            P.save ~create:(`Or_truncate 0o644)
              P.(env#fs / "f")
              (string_of_int i)
          done))
    ()

let test_first_run_fails () =
  Expect.refused "Eio_fs.Chaos.check: the run with no fault failed: Eio.Io"
    (fun () -> Eio_fs.Chaos.check (fun env -> ignore (P.load P.(env#fs / "f"))))
    ()

let test_nothing_acked () =
  Expect.refused
    "Eio_fs.Chaos.Crash.check: the first run acknowledges nothing and setup \
     makes nothing"
    (fun () ->
      Eio_fs.Chaos.Crash.check
        (fun ~ack:_ env ->
          P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "x")
        ~recover:(fun ~acked:_ _ -> ()))
    ()

let test_empty_accepted () =
  Expect.refused
    "Eio_fs.Chaos.Crash.check: recover accepts an empty directory, though \
     setup made files"
    (fun () ->
      Eio_fs.Chaos.Crash.check
        ~setup:(fun env ->
          P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "x")
        (fun ~ack:_ env ->
          try P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "y"
          with Eio.Io _ -> ())
        ~recover:(fun ~acked:_ _ -> ()))
    ()

(* A program that waits for good on an error: every fiber waits, and no
   timer is due. *)
let test_hang () =
  Alcotest.(check string)
    "error" "the run hangs: every fiber waits and no timer is due"
    (Expect.field "error"
       (Expect.report (fun () ->
            Eio_fs.Chaos.check
              ~setup:(fun env ->
                P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "x")
              (fun env ->
                match P.load P.(env#fs / "f") with
                | _ -> ()
                | exception Eio.Io _ -> Eio.Fiber.await_cancel ()))))

(* A program that retries a failed operation at once spins for ever on a
   read-only filesystem. *)
let test_livelock () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            let rec save () =
              match
                P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x"
              with
              | () -> ()
              | exception Eio.Io _ -> save ()
            in
            save ()))
  in
  Alcotest.(check string)
    "error"
    "the run hangs: 10000 filesystem operations failed in a row while its \
     clock stood still"
    (Expect.field "error" report)

(* A workload that catches the cut's cancellation keeps going. *)
let test_kept_going () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.Crash.check
          (fun ~ack env ->
            let n = ref 0 in
            while !n < 3 do
              try
                P.save ~create:(`Or_truncate 0o644)
                  P.(env#fs / "f")
                  (string_of_int !n);
                Eio_fs_sync.sync_dir env#fs;
                ack !n;
                incr n
              with _ -> ()
            done)
          ~recover:(fun ~acked env ->
            if acked <> [] then ignore (P.load P.(env#fs / "f"))))
  in
  Alcotest.(check string)
    "error"
    "the run hangs: it kept going for 1000 operations after the power cut, so \
     it catches Eio.Cancel.Cancelled"
    (Expect.field "error" report)

(* A recovery that does not sync what it repairs: a second cut during it
   loses the repair. *)
let test_cut_in_recover () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.Crash.check
          ~setup:(fun env ->
            P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "a")
          (fun ~ack env ->
            match
              P.with_open_out ~create:(`Or_truncate 0o644)
                P.(env#fs / "f.new")
                (fun f ->
                  Eio.Flow.copy_string "b" f;
                  Eio.File.sync f);
              Eio_fs_sync.sync_dir env#fs
            with
            | () -> ack ()
            | exception Eio.Io _ -> ())
          ~recover:(fun ~acked env ->
            (* A repair: the new file replaces the old one, with no sync of
               its data, then the old name is checked. *)
            (match P.load P.(env#fs / "f.new") with
            | "b" as v ->
                P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") v;
                Eio_fs_sync.sync_dir env#fs;
                P.unlink P.(env#fs / "f.new");
                Eio_fs_sync.sync_dir env#fs
            | _ | (exception Eio.Io _) -> ());
            match (P.load P.(env#fs / "f"), acked) with
            | "b", _ | "a", [] -> ()
            | v, _ -> Fmt.failwith "f is %S" v))
  in
  if Expect.field "recovery" report = "" then
    Alcotest.failf "the failure is not in recovery: %s" report

(* A check runs inside another event loop as it does outside one. *)
let test_nested () =
  Eio_mock.Backend.run @@ fun () ->
  After.test_a ~save ();
  ignore (Expect.report (fun () -> log_mutant ()))

let suite =
  ( "guards",
    [
      Alcotest.test_case "determinism" `Quick test_determinism;
      Alcotest.test_case "replay" `Quick test_replay;
      Alcotest.test_case "a replay of a state a cut allows" `Quick
        test_replay_state;
      Alcotest.test_case "a replay after a fix is re-matched" `Quick
        test_rematched_replay;
      Alcotest.test_case "a stale replay is refused" `Quick test_stale_replay;
      Alcotest.test_case "an unknown replay is refused" `Quick
        test_unknown_replay;
      Alcotest.test_case "the budget" `Quick test_budget;
      Alcotest.test_case "a workload with no operation is refused" `Quick
        test_nothing;
      Alcotest.test_case "a nondeterministic workload is refused" `Quick
        test_nondeterministic;
      Alcotest.test_case "a first run that fails is refused" `Quick
        test_first_run_fails;
      Alcotest.test_case "nothing acked and nothing set up is refused" `Quick
        test_nothing_acked;
      Alcotest.test_case "a recover that accepts nothing is refused" `Quick
        test_empty_accepted;
      Alcotest.test_case "a run every fiber waits in hangs" `Quick test_hang;
      Alcotest.test_case "a run that retries at once hangs" `Quick test_livelock;
      Alcotest.test_case "a run that catches the cut hangs" `Quick
        test_kept_going;
      Alcotest.test_case "a cut during recover" `Quick test_cut_in_recover;
      Alcotest.test_case "a check inside another loop" `Quick test_nested;
    ] )
