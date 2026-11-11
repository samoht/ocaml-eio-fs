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

(* A cache that evicts the entry setup made before it inserts another: a cut
   may leave the directory empty, so recover must accept that. *)
let test_empty_reachable () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> P.save ~create:(`Exclusive 0o644) P.(env#fs / "k1") "v$")
    (fun ~ack:_ env ->
      try
        P.unlink P.(env#fs / "k1");
        P.with_open_out ~create:(`Exclusive 0o644)
          P.(env#fs / "k2.tmp")
          (fun f ->
            Eio.Flow.copy_string "w$" f;
            Eio.File.sync f);
        P.rename P.(env#fs / "k2.tmp") P.(env#fs / "k2")
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      List.iter
        (fun n ->
          if not (Filename.check_suffix n ".tmp") then
            let v = P.load P.(env#fs / n) in
            if not (String.ends_with ~suffix:"$" v) then
              Fmt.failwith "entry %s is %S" n v)
        (P.read_dir env#fs))

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
   read-only filesystem. Its own bound, well past the guard's, fails the
   run instead of spinning for ever when the guard does not stop it. *)
let test_livelock () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            let rec save n =
              if n > 50_000 then failwith "the spin was not stopped";
              match
                P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x"
              with
              | () -> ()
              | exception Eio.Io _ -> save (n + 1)
            in
            save 0))
  in
  Alcotest.(check string)
    "error"
    "the run hangs: the same operation on /f failed 10003 times while its \
     clock stood still"
    (Expect.field "error" report)

(* A batch job that logs a line per item and drops the lines a full disk
   refuses: each line is a new write, not a retry. The replay runs the
   scenario where the disk is full from the first line on. *)
let test_busy_is_not_a_spin () =
  Eio_fs.Chaos.check ~replay:"fs/00000000/from:write:/log#1.full" (fun env ->
      let processed = ref 0 in
      (try
         P.with_open_out ~append:true ~create:(`If_missing 0o644)
           P.(env#fs / "log")
           (fun log ->
             for i = 1 to 12_000 do
               incr processed;
               try Eio.Flow.copy_string (Fmt.str "item %d done\n" i) log
               with Eio.Io _ -> ()
             done)
       with Eio.Io _ -> processed := 12_000);
      if !processed <> 12_000 then failwith "the batch is incomplete")

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

(* A supervisor that runs each save in a new fiber and retries it with
   backoff until it succeeds: a cut stops it, as nothing runs after a power
   cut. *)
let test_cut_stops_supervisor () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> save env#fs "0")
    (fun ~ack env ->
      Eio.Switch.run @@ fun sw ->
      for i = 1 to 3 do
        let v = string_of_int i in
        let rec go backoff =
          match
            Eio.Promise.await
              (Eio.Fiber.fork_promise ~sw (fun () -> save env#fs v))
          with
          | Ok () -> ack v
          | Error _ ->
              Eio.Time.sleep env#clock backoff;
              go (Float.min 60. (backoff *. 2.))
        in
        go 0.1
      done)
    ~recover:(fun ~acked env ->
      match (App.load_config env#fs, List.rev acked) with
      | v, a :: _ when v < a -> Fmt.failwith "config is %s, acked %s" v a
      | _ -> ())

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
      Alcotest.test_case "a recover that accepts a reachable empty directory"
        `Quick test_empty_reachable;
      Alcotest.test_case "a run every fiber waits in hangs" `Quick test_hang;
      Alcotest.test_case "a run that retries at once hangs" `Quick test_livelock;
      Alcotest.test_case "a batch that drops what fails is not a spin" `Quick
        test_busy_is_not_a_spin;
      Alcotest.test_case "a run that catches the cut hangs" `Quick
        test_kept_going;
      Alcotest.test_case "a cut stops a supervisor that retries" `Quick
        test_cut_stops_supervisor;
      Alcotest.test_case "a cut during recover" `Quick test_cut_in_recover;
      Alcotest.test_case "a check inside another loop" `Quick test_nested;
    ] )
