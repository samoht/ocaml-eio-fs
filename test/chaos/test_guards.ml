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

(* A replay of a state the cut does not allow is refused: a choice past
   those of its draw, or a draw the cut does not make. *)
let test_replay_unknown_state () =
  let marker replay =
    After.test_b ?replay
      ~append:(fun f r -> App.Log.append f r)
      ~recover:(App.Log.recover ~bug:`Marker_only)
      ()
  in
  let replay = replay_of (Expect.report (fun () -> marker None)) in
  let cut = String.sub replay 0 (String.rindex replay '|') in
  let state =
    String.sub replay
      (String.length cut + 1)
      (String.length replay - String.length cut - 1)
  in
  List.iter
    (fun st ->
      let key =
        String.concat "/" (List.tl (List.tl (String.split_on_char '/' cut)))
      in
      Expect.refused
        (Fmt.str
           "Eio_fs.Chaos.Crash.check: the replay names %s|%s, a scenario this \
            check no longer has"
           key st)
        (fun () -> marker (Some (cut ^ "|" ^ st)))
        ())
    [
      "e999";
      state
      ^ ".0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.1";
    ]

(* A cut that allows two states, which the check tries one by one: a
   replay of a state drawn at random there, or of a malformed seed, is
   refused. *)
let test_replay_drawn_state () =
  let mkdir ?replay () =
    Eio_fs.Chaos.Crash.check ?replay
      (fun ~ack env ->
        (try P.mkdir ~perm:0o755 P.(env#fs / "d") with Eio.Io _ -> ());
        ack ())
      ~recover:(fun ~acked env ->
        if acked <> [] && not (P.is_directory P.(env#fs / "d")) then
          failwith "d missing")
  in
  let replay = replay_of (Expect.report (fun () -> mkdir ())) in
  let h = hash replay in
  List.iter
    (fun st ->
      Expect.refused
        (Fmt.str
           "Eio_fs.Chaos.Crash.check: the replay names none|end|%s, a scenario \
            this check no longer has"
           st)
        (fun () -> mkdir ~replay:(Fmt.str "fs/%s/none|end|%s" h st) ())
        ())
    [ "s0"; "s1"; "s5"; "s-3"; "p1" ]

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

let reads_nothing =
  "Eio_fs.Chaos.Crash.check: recover makes no filesystem operation"

let test_reads_nothing () =
  Expect.refused reads_nothing
    (fun () ->
      Eio_fs.Chaos.Crash.check
        (fun ~ack env ->
          P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "x";
          ack ())
        ~recover:(fun ~acked:_ _ -> ()))
    ()

(* With no ack and no setup, recover still checks what the workload made is
   whole. *)
let test_nothing_acked () =
  Eio_fs.Chaos.Crash.check
    (fun ~ack:_ env ->
      try Eio_fs_sync.save P.(env#fs / "f") "x" with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      match P.load P.(env#fs / "f") with
      | "x" | (exception Eio.Io _) -> ()
      | v -> Fmt.failwith "f is %S" v)

(* A setup that makes only a directory: recover checks it. *)
let test_setup_directory () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env ->
      P.mkdir ~perm:0o755 P.(env#fs / "objects");
      Eio_fs_sync.sync_dir env#fs)
    (fun ~ack:_ env ->
      try P.save ~create:(`Exclusive 0o644) P.(env#fs / "objects" / "x") "x"
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      if P.kind ~follow:false P.(env#fs / "objects") <> `Directory then
        failwith "objects lost")

(* A log setup made, which recover reads as empty when it is missing: a
   correct recover that accepts an empty directory. *)
let lenient_log ?(sync = true) () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "log") "")
    (fun ~ack env ->
      try
        P.with_open_out ~append:true ~create:`Never P.(env#fs / "log")
        @@ fun f ->
        Eio.Flow.copy_string "r1\n" f;
        if sync then Eio.File.sync f;
        ack "r1"
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let log = try P.load P.(env#fs / "log") with Eio.Io _ -> "" in
      let lines = String.split_on_char '\n' log in
      List.iter
        (fun r -> if not (List.mem r lines) then Fmt.failwith "%s lost" r)
        acked)

let test_empty_accepted () = lenient_log ()

let test_guard_hides_nothing () =
  let m = Expect.report (fun () -> lenient_log ~sync:false ()) in
  Alcotest.(check string)
    "error" "Failure(\"r1 lost\")" (Expect.field "error" m)

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

let busy =
  "the run woke 10,000,000 times without waiting 1 ms or more: a busy loop"

let too_many_ops =
  "the run made 100,000 filesystem operations, more than a check plans from: a \
   busy loop, or a workload too large for a check"

(* A fiber that polls with a yield for a file another fiber failed to
   write: each turn of the loop takes time, and the run is a busy loop. Its
   own bound, past that, fails the run when nothing stops it. *)
let test_yield_poll_is_busy () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            let ready = ref false in
            Eio.Fiber.both
              (fun () ->
                try
                  P.save ~create:(`Or_truncate 0o644) P.(env#fs / "ready") "go";
                  ready := true
                with Eio.Io _ -> ())
              (fun () ->
                let polls = ref 0 in
                while not !ready do
                  incr polls;
                  if !polls > 20_000_000 then
                    failwith "the poll was not stopped";
                  Eio.Fiber.yield ()
                done)))
  in
  Alcotest.(check string) "error" busy (Expect.field "error" report)

(* The same with a sleep of 1 us, or 100 us, for each turn. *)
let test_tiny_sleeps_are_busy () =
  List.iter
    (fun d ->
      let report =
        Expect.report (fun () ->
            Eio_fs.Chaos.check (fun env ->
                let ready = ref false in
                (try
                   P.save ~create:(`Or_truncate 0o644) P.(env#fs / "ready") "go";
                   ready := true
                 with Eio.Io _ -> ());
                let sleeps = ref 0 in
                while not !ready do
                  incr sleeps;
                  if !sleeps > 20_000_000 then
                    failwith "the loop was not stopped";
                  Eio.Time.sleep env#clock d
                done))
      in
      Alcotest.(check string) "error" busy (Expect.field "error" report))
    [ 1e-6; 1e-4 ]

(* A loop of 9.9 million yields then a sleep of 1 ms: each sleep ends a run
   of turns in a row, and the run stops at 10 million turns in all. Its own
   bound, past that, fails the run when nothing stops it. *)
let test_too_many_turns () =
  Expect.refused
    "Eio_fs.Chaos.check: the run with no fault failed: the run took 10,000,000 \
     turns, more than a check can repeat: a busy loop, or a workload too large \
     for a check"
    (fun () ->
      Eio_fs.Chaos.check (fun env ->
          (try P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x"
           with Eio.Io _ -> ());
          for _ = 1 to 3 do
            for _ = 1 to 9_900_000 do
              Eio.Fiber.yield ()
            done;
            Eio.Time.Mono.sleep env#mono_clock 0.001
          done;
          failwith "the loop was not stopped"))
    ()

(* A thousand fibers on a heartbeat of 100 ms and three polling every 1 ms,
   for 1000 s of clock: far more than 10 million turns in all, each fiber
   waiting on its timer between two, and no busy loop. *)
let test_timers_are_not_busy () =
  Eio_fs.Chaos.check (fun env ->
      match P.mkdir ~perm:0o755 P.(env#fs / "d") with
      | exception Eio.Io _ -> ()
      | () ->
          let ticker period () =
            while Eio.Time.now env#clock < 1000. do
              Eio.Time.sleep env#clock period
            done
          in
          Eio.Fiber.all
            (List.init 1000 (fun _ -> ticker 0.1)
            @ List.init 3 (fun _ -> ticker 0.001)))

(* A computation that yields 10000 times ends well within a deadline of
   100 ms. *)
let test_yields_meet_a_deadline () =
  Eio_fs.Chaos.check (fun env ->
      (try P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x"
       with Eio.Io _ -> ());
      Eio.Time.with_timeout_exn env#clock 0.1 (fun () ->
          for _ = 1 to 10_000 do
            Eio.Fiber.yield ()
          done))

(* Two fibers that pass a token back and forth until a timer of 10 ms stops
   them: time passes as they do, and the timer ends the run. *)
let test_ping_pong_until_a_timer () =
  Eio_fs.Chaos.check (fun env ->
      (try P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x"
       with Eio.Io _ -> ());
      let stop = ref false in
      let ping = Eio.Stream.create 1 and pong = Eio.Stream.create 1 in
      let rec player turns inbox outbox =
        if turns > 20_000_000 then failwith "the timer never fired";
        if not !stop then begin
          Eio.Stream.take inbox;
          Eio.Stream.add outbox ();
          player (turns + 1) inbox outbox
        end
      in
      Eio.Stream.add ping ();
      Eio.Fiber.all
        [
          (fun () -> player 0 ping pong);
          (fun () -> player 0 pong ping);
          (fun () ->
            Eio.Time.sleep env#clock 0.01;
            stop := true);
        ])

(* Three tries get past two faults, not a lasting condition: a run that
   demands success after them fails there. *)
let test_three_tries_and_a_lasting_condition () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            let rec save n =
              match
                P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x"
              with
              | () -> ()
              | exception Eio.Io _ when n < 3 -> save (n + 1)
            in
            save 1))
  in
  let faults = Expect.field "faults" report in
  if not (String.starts_with ~prefix:"the filesystem is" faults) then
    Alcotest.failf "the failure is not a lasting condition: %s" faults

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

(* The same with a fresh temporary name for each try: each name fails
   once, and the directory they are in spins. *)
let test_livelock_fresh_names () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            let rec save n =
              if n > 50_000 then failwith "the spin was not stopped";
              match
                P.save ~create:(`Exclusive 0o644)
                  P.(env#fs / Fmt.str "a.tmp.%d" n)
                  "x"
              with
              | () -> ()
              | exception Eio.Io _ -> save (n + 1)
            in
            save 0))
  in
  Alcotest.(check string)
    "error"
    "the run hangs: one kind of operation in / failed 11000 times while its \
     clock stood still"
    (Expect.field "error" report)

(* A log of 300 records, each retried at once until it lands: under a
   read-only filesystem the first record spins. The check stops it within
   a few times what the first run made of that write, not 10000 times
   that. *)
let test_hot_spin_stops_early () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            let tries = ref 0 in
            try
              P.with_open_out ~create:(`Or_truncate 0o644) P.(env#fs / "log")
              @@ fun f ->
              for i = 0 to 299 do
                let rec write () =
                  incr tries;
                  if !tries > 50_000 then failwith "the spin was not stopped";
                  try
                    Eio.File.pwrite_all f
                      ~file_offset:(Optint.Int63.of_int (i * 8))
                      [ Cstruct.of_string "record.." ]
                  with Eio.Io _ -> write ()
                in
                write ()
              done
            with Eio.Io _ -> ()))
  in
  Alcotest.(check string)
    "error"
    "the run hangs: the same operation on /log failed 10900 times while its \
     clock stood still"
    (Expect.field "error" report)

(* The report of a spin shows the loop once, not each of its turns. *)
let test_spin_report_is_short () =
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
  let lines = List.length (String.split_on_char '\n' report) in
  if lines > 100 then Alcotest.failf "the report has %d lines" lines

(* A pool of 50 slots, each trying 300 names once and moving on after any
   error: under a lasting condition it fails 15000 times with no time
   passing, and ends. *)
let test_wide_pool_is_not_a_spin () =
  Eio_fs.Chaos.check (fun env ->
      for i = 0 to 49 do
        let rec take j =
          if j < 300 then
            match
              P.save ~create:(`Exclusive 0o644)
                P.(env#fs / Fmt.str "s%d.%d" i j)
                "x"
            with
            | () -> ()
            | exception Eio.Io _ -> take (j + 1)
        in
        take 0
      done)

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

(* A pool of 10 slots, each trying 1100 names once and moving on after any
   error: under a lasting condition it fails 11000 times with no time passing,
   and ends. *)
let test_pool_is_not_a_spin () =
  Eio_fs.Chaos.check (fun env ->
      for i = 0 to 9 do
        let rec take j =
          if j < 1100 then
            match
              P.save ~create:(`Exclusive 0o644)
                P.(env#fs / Fmt.str "s%d.%d" i j)
                "x"
            with
            | () -> ()
            | exception Eio.Io _ -> take (j + 1)
        in
        take 0
      done)

(* Linux needs no room to remove an entry, change a mode, or open a file
   that exists, and checks access at the open, not at each write: a program
   that frees space on a full disk, or writes to a file it opened, never
   meets those errors there. *)
let test_errors_linux_returns () =
  let contains s sub =
    let n = String.length s and m = String.length sub in
    let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
    go 0
  in
  let never codes f =
    try f ()
    with Eio.Io _ as e ->
      let s = Printexc.to_string e in
      List.iter
        (fun c -> if contains s c then Fmt.failwith "%s from %s" c s)
        codes
  in
  Eio_fs.Chaos.check
    ~setup:(fun env ->
      P.save ~create:(`Exclusive 0o644) P.(env#fs / "old") "o";
      P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "f")
    (fun env ->
      never [ "ENOSPC" ] (fun () ->
          P.chmod ~follow:false ~perm:0o600 P.(env#fs / "f"));
      never [ "ENOSPC" ] (fun () -> P.unlink P.(env#fs / "old"));
      never [ "ENOSPC" ] (fun () ->
          P.with_open_out ~create:`Never P.(env#fs / "f") @@ fun f ->
          never [ "EACCES" ] (fun () -> Eio.Flow.copy_string "x" f)))

(* A check keeps no run's operations once it has drawn from them: the heap
   its runs see stays flat as the linear tier goes. *)
let test_heap_stays_flat () =
  Gc.compact ();
  let base = (Gc.quick_stat ()).heap_words and top = ref 0 in
  Eio_fs.Chaos.check (fun env ->
      top := max !top (Gc.quick_stat ()).heap_words;
      for i = 1 to 30 do
        try Eio_fs_sync.save P.(env#fs / "k") (string_of_int i)
        with Eio.Io _ -> ()
      done);
  let grown = !top - base in
  if grown > 1_000_000 then Alcotest.failf "the heap grew by %d words" grown

(* A save retried after a yield on a read-only filesystem: the run stops at
   100,000 operations. The check keeps only what the report shows, so the
   heap stays flat as the run goes, and the report is short. *)
let test_busy_heap_stays_flat () =
  Gc.compact ();
  let base = (Gc.quick_stat ()).heap_words and top = ref 0 in
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            let rec save n =
              if n mod 100_000 = 0 then
                top := max !top (Gc.quick_stat ()).heap_words;
              if n > 20_000_000 then failwith "the poll was not stopped";
              match
                P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x"
              with
              | () -> ()
              | exception Eio.Io _ ->
                  Eio.Fiber.yield ();
                  save (n + 1)
            in
            save 0))
  in
  Alcotest.(check string) "error" too_many_ops (Expect.field "error" report);
  let lines = List.length (String.split_on_char '\n' report) in
  if lines > 20 then Alcotest.failf "the report has %d lines" lines;
  let grown = !top - base in
  if grown > 1_000_000 then Alcotest.failf "the heap grew by %d words" grown

(* Three fibers that stat a file every 1 ms for 100 s of clock: each stat
   is structural, so the linear tier needs more runs than the budget long
   before the first run ends. The first run stops there, and the heap stays
   flat. *)
let test_first_run_stops_at_the_budget () =
  Gc.compact ();
  let base = (Gc.quick_stat ()).heap_words and top = ref 0 in
  let stats = ref 0 in
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.check (fun env ->
            Eio.Fiber.all
              (List.init 3 (fun _ () ->
                   while Eio.Time.now env#clock < 100. do
                     incr stats;
                     if !stats mod 1000 = 0 then
                       top := max !top (Gc.quick_stat ()).heap_words;
                     (try ignore (P.kind ~follow:true P.(env#fs / "x"))
                      with Eio.Io _ -> ());
                     Eio.Time.sleep env#clock 0.001
                   done))))
  in
  if
    not
      (String.starts_with ~prefix:"Eio_fs.Chaos.check: the linear tier needs"
         report)
  then Alcotest.failf "not refused by the budget: %s" report;
  let grown = !top - base in
  if grown > 1_000_000 then Alcotest.failf "the heap grew by %d words" grown

(* A poll that reads an open file until it changes, with no yield: no turn
   passes and nothing fails, so only the run's operations stop it. Its own
   bound, past that, fails the run when nothing stops it. *)
let test_read_poll_stops () =
  Expect.refused
    ("Eio_fs.Chaos.check: the run with no fault failed: " ^ too_many_ops)
    (fun () ->
      Eio_fs.Chaos.check
        ~setup:(fun env ->
          P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "x")
        (fun env ->
          try
            P.with_open_in P.(env#fs / "f") @@ fun f ->
            let buf = Cstruct.create 1 and reads = ref 0 in
            while
              incr reads;
              if !reads > 1_000_000 then failwith "the poll was not stopped";
              Eio.File.pread_exact f ~file_offset:Optint.Int63.zero [ buf ];
              Cstruct.to_string buf = "x"
            do
              ()
            done
          with Eio.Io _ -> ()))
    ()

(* Three fibers that stat a file every 1 ms for 300 s of clock, under power
   cuts: a stat changes nothing, so the linear tier stays small, and the
   run stops at its operations. The heap stays flat. Its own bound, past
   that, fails the check when nothing stops the runs. *)
let test_crash_stat_poll_stops () =
  Gc.compact ();
  let base = (Gc.quick_stat ()).heap_words and top = ref 0 in
  let stats = ref 0 in
  Expect.refused
    ("Eio_fs.Chaos.Crash.check: the run with no cut failed: " ^ too_many_ops)
    (fun () ->
      Eio_fs.Chaos.Crash.check
        (fun ~ack:_ env ->
          Eio.Fiber.all
            (List.init 3 (fun _ () ->
                 while Eio.Time.now env#clock < 300. do
                   incr stats;
                   if !stats > 3_000_000 then
                     failwith "the runs were not stopped";
                   if !stats mod 1000 = 0 then
                     top := max !top (Gc.quick_stat ()).heap_words;
                   (try ignore (P.kind ~follow:true P.(env#fs / "x"))
                    with Eio.Io _ -> ());
                   Eio.Time.sleep env#clock 0.001
                 done)))
        ~recover:(fun ~acked:_ env -> ignore (P.read_dir env#fs)))
    ();
  let grown = !top - base in
  if grown > 4_000_000 then Alcotest.failf "the heap grew by %d words" grown

(* The same for power cuts: a check keeps no run's operations once it has
   drawn from them, so the heap stays flat as runs go, and does not grow
   with the number of runs, nor, as it draws states, with the number of cuts
   times the draws each makes. *)
let test_crash_heap_stays_flat () =
  Gc.compact ();
  let base = (Gc.quick_stat ()).heap_words and top = ref 0 in
  let n = 200 in
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      top := max !top (Gc.quick_stat ()).heap_words;
      for i = 1 to n do
        try
          Eio_fs_sync.save P.(env#fs / Fmt.str "f%d" i) "x";
          ack i
        with Eio.Io _ -> ()
      done)
    ~recover:(fun ~acked env ->
      top := max !top (Gc.quick_stat ()).heap_words;
      List.iter
        (fun i ->
          if P.load P.(env#fs / Fmt.str "f%d" i) <> "x" then
            Fmt.failwith "f%d lost" i)
        acked);
  let grown = !top - base in
  if grown > 2_000_000 then Alcotest.failf "the heap grew by %d words" grown

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

(* A failure where every change was kept, as after a process kill: its
   report names no change as lost. *)
let test_kept_loses_nothing () =
  let report =
    Expect.report (fun () ->
        Eio_fs.Chaos.Crash.check
          (fun ~ack env ->
            P.with_open_out ~create:(`Exclusive 0o644)
              P.(env#fs / "f")
              (fun f ->
                Eio.Flow.copy_string "half" f;
                Eio.Flow.copy_string "-whole" f;
                Eio.File.sync f);
            Eio_fs_sync.sync_dir env#fs;
            ack ())
          ~recover:(fun ~acked env ->
            match P.load P.(env#fs / "f") with
            | "" | "half-whole" -> ()
            | v -> Fmt.failwith "f is %S" v
            | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
                if acked <> [] then failwith "f is lost"))
  in
  Alcotest.(check string)
    "cut" "before #3 write /f@4+6, every change kept, as after a process kill"
    (Expect.field "cut" report);
  Alcotest.(check string) "lost" "" (Expect.field "lost" report)

(* A report numbers each operation as its trace does: the changes a cut
   lost, a lasting condition, and what a truncation left. *)
let test_one_numbering () =
  let has m line =
    let n = String.length m and k = String.length line in
    let rec go i = i + k <= n && (String.sub m i k = line || go (i + 1)) in
    if not (go 0) then Alcotest.failf "no %S in %s" line m
  in
  let lost =
    Expect.report (fun () ->
        Eio_fs.Chaos.Crash.check
          ~setup:(fun env ->
            Eio_fs_sync.save P.(env#fs / "old") "o";
            Eio_fs_sync.save P.(env#fs / "x") "x")
          (fun ~ack env ->
            try
              P.save ~create:(`Exclusive 0o644) P.(env#fs / "a") "data";
              P.unlink P.(env#fs / "old");
              P.rename P.(env#fs / "x") P.(env#fs / "y");
              ack ()
            with Eio.Io _ -> ())
          ~recover:(fun ~acked env ->
            if acked <> [] && P.kind ~follow:false P.(env#fs / "y") = `Not_found
            then failwith "y lost"))
  in
  Alcotest.(check string)
    "lost" "#1 create /a; #2 write /a@0+4; #3 unlink /old; #4 rename /x to /y"
    (Expect.field "lost" lost);
  has lost "\n    #4 rename /x to /y\n";
  let left =
    Expect.report (fun () ->
        Eio_fs.Chaos.Crash.check
          ~setup:(fun env ->
            Eio_fs_sync.save P.(env#fs / "f") (String.make 9000 'A'))
          (fun ~ack env ->
            try
              P.with_open_out ~create:`Never P.(env#fs / "f") @@ fun f ->
              Eio.File.truncate f (Optint.Int63.of_int 1000);
              Eio.File.pwrite_all f ~file_offset:(Optint.Int63.of_int 2000)
                [ Cstruct.of_string "BB" ];
              ack ()
            with Eio.Io _ -> ())
          ~recover:(fun ~acked:_ env ->
            let s = P.load P.(env#fs / "f") in
            if String.length s = 2002 && s.[2000] <> 'B' then failwith "no BB"))
  in
  Alcotest.(check string)
    "unsynced" "#2 truncate /f to 1000; #3 write /f@2000+2"
    (Expect.field "unsynced" left);
  Alcotest.(check string)
    "left"
    "/f, 2002 bytes: @512+512 as the truncation to 1000 left it, zeros from \
     1000; @1536+466 zeros, from the truncation to 1000"
    (Expect.field "left" left);
  let spin =
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
    "faults" "the filesystem is read-only from #1 open-write /f on"
    (Expect.field "faults" spin)

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
      Alcotest.test_case "a replay of a state the cut does not allow" `Quick
        test_replay_unknown_state;
      Alcotest.test_case "a replay of a drawn state where none is drawn" `Quick
        test_replay_drawn_state;
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
      Alcotest.test_case "a recover that reads nothing is refused" `Quick
        test_reads_nothing;
      Alcotest.test_case "nothing acked and nothing set up" `Quick
        test_nothing_acked;
      Alcotest.test_case "a setup that makes a directory makes something" `Quick
        test_setup_directory;
      Alcotest.test_case "a recover that reads a missing log as empty" `Quick
        test_empty_accepted;
      Alcotest.test_case "a failure comes before a guard" `Quick
        test_guard_hides_nothing;
      Alcotest.test_case "a recover that accepts a reachable empty directory"
        `Quick test_empty_reachable;
      Alcotest.test_case "a run every fiber waits in hangs" `Quick test_hang;
      Alcotest.test_case "a yield poll is a busy loop" `Quick
        test_yield_poll_is_busy;
      Alcotest.test_case "a loop of tiny sleeps is a busy loop" `Quick
        test_tiny_sleeps_are_busy;
      Alcotest.test_case "a run stops at 10 million turns" `Quick
        test_too_many_turns;
      Alcotest.test_case "many fibers on timers are not busy" `Quick
        test_timers_are_not_busy;
      Alcotest.test_case "10000 yields meet a deadline of 100 ms" `Quick
        test_yields_meet_a_deadline;
      Alcotest.test_case "a ping-pong ends with its timer" `Quick
        test_ping_pong_until_a_timer;
      Alcotest.test_case "three tries do not outlast a lasting condition" `Quick
        test_three_tries_and_a_lasting_condition;
      Alcotest.test_case "a run that retries at once hangs" `Quick test_livelock;
      Alcotest.test_case "a run that retries at once under fresh names hangs"
        `Quick test_livelock_fresh_names;
      Alcotest.test_case "a spin on a hot path stops early" `Quick
        test_hot_spin_stops_early;
      Alcotest.test_case "the report of a spin is short" `Quick
        test_spin_report_is_short;
      Alcotest.test_case "a wide pool is not a spin" `Quick
        test_wide_pool_is_not_a_spin;
      Alcotest.test_case "a batch that drops what fails is not a spin" `Quick
        test_busy_is_not_a_spin;
      Alcotest.test_case "a pool that tries each name once is not a spin" `Quick
        test_pool_is_not_a_spin;
      Alcotest.test_case "only the errors Linux returns" `Quick
        test_errors_linux_returns;
      Alcotest.test_case "the heap stays flat" `Quick test_heap_stays_flat;
      Alcotest.test_case "the heap stays flat in a busy loop" `Quick
        test_busy_heap_stays_flat;
      Alcotest.test_case "a first run stops at the budget" `Quick
        test_first_run_stops_at_the_budget;
      Alcotest.test_case "a read poll with no yield stops" `Quick
        test_read_poll_stops;
      Alcotest.test_case "a stat poll under power cuts stops" `Quick
        test_crash_stat_poll_stops;
      Alcotest.test_case "the heap stays flat under power cuts" `Quick
        test_crash_heap_stays_flat;
      Alcotest.test_case "a run that catches the cut hangs" `Quick
        test_kept_going;
      Alcotest.test_case "a cut stops a supervisor that retries" `Quick
        test_cut_stops_supervisor;
      Alcotest.test_case "a kept state loses nothing" `Quick
        test_kept_loses_nothing;
      Alcotest.test_case "one numbering in a report" `Quick test_one_numbering;
      Alcotest.test_case "a cut during recover" `Quick test_cut_in_recover;
      Alcotest.test_case "a check inside another loop" `Quick test_nested;
    ] )
