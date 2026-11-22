(* Locks over a lock file: [with_lock], probes, held locks, waits
   and their bounds, the lock file's checks, and sections. *)

module Chaos = Eio_fs_chaos
module Crash = Chaos.Private.Crash
open Holder

let ( / ) = Eio.Path.( / )

let await_holder child =
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "lock holder exited %d" n
  | `Signaled n -> Alcotest.failf "lock holder took signal %d" n

let with_held_lock ~sw ~env ~lock fn =
  let manager = Eio.Stdenv.process_mgr env in
  let input, release = Eio.Process.pipe ~sw manager in
  let ready, output = Eio.Process.pipe ~sw manager in
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw manager ~stdin:input ~stdout:output ~executable:self
      [ self; "--hold-eio-fs-lock-until-release"; Eio.Path.native_exn lock ]
  in
  Eio.Resource.close input;
  Eio.Resource.close output;
  let announced = Cstruct.create 1 in
  Eio.Flow.read_exact ready announced;
  Alcotest.(check char)
    "holder acquired the native lock" 'R'
    (Cstruct.get_char announced 0);
  Fun.protect
    ~finally:(fun () ->
      Eio.Flow.copy_string "X" release;
      Eio.Resource.close release;
      await_holder child)
    fn

(* A wait for a lock another process holds says it waits once and is granted
   once the holder lets go, and never before. [take ~on_wait lock fn] is the
   acquisition under test. *)
let a_wait_answers_at_the_release ~kind take () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let waits = ref 0 in
  let granted, now_granted = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      take
        ~on_wait:(fun () ->
          incr waits;
          ignore (Eio.Promise.try_resolve now_waiting ()))
        lock
        (fun () -> Eio.Promise.resolve now_granted ()));
  Eio.Promise.await waiting;
  Alcotest.(check bool)
    "not granted while held" false
    (Eio.Promise.is_resolved granted);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ());
  Eio.Promise.await granted;
  Alcotest.(check int) "said it waits once" 1 !waits

(* The same wait is one a caller can end: cancelled once the waiter is in the
   kernel's wait, it answers at the cancel while the holder still holds (the
   holder lets go at the test's word, after the cancel, so a wait that
   answered only at a release finds the holder gone at its own bound), and it
   leaves no hold behind: once the holder lets go, [free lock] finds the lock
   free. *)
let a_wait_is_cancellable ~kind take ~free () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let answer =
    Eio.Fiber.first
      (fun () ->
        take
          ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
          lock
          (fun () -> `Granted))
      (fun () ->
        Eio.Promise.await waiting;
        `Cancelled)
  in
  (match answer with
  | `Granted -> Alcotest.fail "a lock another process holds was granted"
  | `Cancelled -> ());
  Alcotest.check status "the holder let go when told, after the cancel"
    (`Exited 0) (release ());
  free ~env lock

(* A lease a fiber of this process holds is waited for under [`Fibers], and
   the waiter says it waits, once, before it queues: a caller that waits on
   its own run's fibers can tell it from one that ran at once. The waiter is
   granted once the holder lets go, and not before. *)
let test_lease_wait_for_a_fiber_says_so () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held, now_held = Eio.Promise.create () in
  let go, now_go = Eio.Promise.create () in
  let waiting, now_waiting = Eio.Promise.create () in
  let waits = ref 0 in
  let granted, now_granted = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.Lease.with_exclusive ~block:`Fibers lock (fun _ ->
          Eio.Promise.resolve now_held ();
          Eio.Promise.await go));
  Eio.Promise.await held;
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.Lease.with_exclusive ~block:`Fibers
        ~on_wait:(fun () ->
          incr waits;
          ignore (Eio.Promise.try_resolve now_waiting ()))
        lock
        (fun _ -> Eio.Promise.resolve now_granted ()));
  Eio.Promise.await waiting;
  Alcotest.(check bool)
    "not granted while held" false
    (Eio.Promise.is_resolved granted);
  Eio.Promise.resolve now_go ();
  Eio.Promise.await granted;
  Alcotest.(check int) "said it waits once" 1 !waits

(* A monotonic clock that counts the waits begun on it and never moves, so a
   bound set on it never lapses and a wait that paces itself on it never asks
   again. *)
module Still_clock = struct
  type t = int ref
  type time = Mtime.t

  let now _ = Mtime.min_stamp

  let sleep_until begun _ =
    incr begun;
    Eio.Fiber.await_cancel ()
end

(* [on_mono clock f] is [f ()] with every [Eio_unix.sleep] of the fibers [f]
   starts on [clock]. *)
let rec on_mono : type r. Eio.Time.Mono.ty Eio.Resource.t -> (unit -> r) -> r =
 fun clock f ->
  let effc : type a. a Effect.t -> ((a, r) Effect.Deep.continuation -> r) option
      = function
    | Eio_unix.Private.Get_monotonic_clock ->
        Some (fun k -> Effect.Deep.continue k clock)
    | Eio.Private.Effects.Fork (ctx, child) ->
        Some
          (fun k ->
            Effect.perform
              (Eio.Private.Effects.Fork (ctx, fun () -> on_mono clock child));
            Effect.Deep.continue k ())
    | _ -> None
  in
  Effect.Deep.try_with f () { effc }

(* [counting_waits f] is [f ()] on a {!Still_clock}, and the number of waits
   begun on it. *)
let counting_waits f =
  let begun = ref 0 in
  let clock = Eio.Resource.T (begun, Eio.Time.Pi.clock (module Still_clock)) in
  let value = on_mono clock f in
  (value, !begun)

(* A record lock wait bounded by [`For] is the kernel's blocking request
   (POSIX fcntl F_SETLKW), raced against its bound: the waiter asks once, says
   it waits once, and is granted at the release, having begun one wait on the
   monotonic clock, its bound's. The clock never moves, so a wait that asked
   again on a cadence would begin a second wait and never ask again. *)
let a_bounded_wait_answers_at_the_release ~kind take () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let waits = ref 0 in
  let granted, now_granted = Eio.Promise.create () in
  let begun, now_begun = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let (), begun =
        counting_waits (fun () ->
            take
              ~on_wait:(fun () ->
                incr waits;
                ignore (Eio.Promise.try_resolve now_waiting ()))
              lock
              (fun () -> Eio.Promise.resolve now_granted ()))
      in
      Eio.Promise.resolve now_begun begun);
  Eio.Promise.await waiting;
  Alcotest.(check bool)
    "not granted while held" false
    (Eio.Promise.is_resolved granted);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ());
  Eio.Promise.await granted;
  Alcotest.(check int) "said it waits once" 1 !waits;
  Alcotest.(check int)
    "began one wait on the clock, its bound's" 1 (Eio.Promise.await begun)

let take_record ~on_wait lock fn = Eio_fs_unix.with_lock ~on_wait lock fn

let take_record_for ~on_wait lock fn =
  Eio_fs_unix.with_lock ~block:(`For 60.) ~on_wait lock fn

let take_lease_for ~on_wait lock fn =
  Eio_fs_unix.Lease.with_exclusive ~block:(`For 60.) ~on_wait lock (fun _ ->
      fn ())

let take_lease ~on_wait lock fn =
  Eio_fs_unix.Lease.with_exclusive ~on_wait lock (fun _ -> fn ())

(* Another process is granted the record lock: this process's own asks are
   granted whatever it holds. *)
let record_free ~env lock =
  let manager = Eio.Stdenv.process_mgr env in
  let self = Sys.executable_name in
  Alcotest.check status "another process is granted the lock" (`Exited 0)
    ( Eio.Switch.run @@ fun sw ->
      Eio.Process.await
        (Eio.Process.spawn ~sw manager ~executable:self
           [ self; "--try-eio-fs-lock"; Eio.Path.native_exn lock ]) )

(* A byte-range lock is placed from the start of the file and leaves the
   descriptor's offset where it was (POSIX fcntl F_SETLK with SEEK_SET), so a
   lock taken through a descriptor another reader seeks moves none of its
   reads. Another process meets the lock, and the lock is gone once let go;
   [range_holder] answers about another process's lock and never this
   one's. *)
let test_lock_range () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "range" in
  Eio.Path.save ~create:(`Or_truncate 0o600) lock (String.make 64 'r');
  let fd = Unix.openfile (Eio.Path.native_exn lock) [ Unix.O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  ignore (Unix.lseek fd 7 Unix.SEEK_SET : int);
  Alcotest.(check bool)
    "the lock is set" true
    (Eio_fs_unix.lock_range fd `Write ~file_offset:10 ~length:5);
  Alcotest.(check int)
    "the lock leaves the offset" 7
    (Unix.lseek fd 0 Unix.SEEK_CUR);
  Alcotest.(check bool)
    "this process's own lock is not another's" true
    (Eio_fs_unix.range_holder fd ~file_offset:10 ~length:5 = `Free);
  let manager = Eio.Stdenv.process_mgr env in
  let self = Sys.executable_name in
  let try_lock () =
    Eio.Switch.run @@ fun sw ->
    Eio.Process.await
      (Eio.Process.spawn ~sw manager ~executable:self
         [ self; "--try-eio-fs-lock"; Eio.Path.native_exn lock ])
  in
  Alcotest.check status "another process meets the lock" (`Exited 3)
    (try_lock ());
  Alcotest.(check bool)
    "an unlock is always set" true
    (Eio_fs_unix.lock_range fd `Unlock ~file_offset:10 ~length:5);
  Alcotest.check status "and is granted the file once it is let go" (`Exited 0)
    (try_lock ());
  Eio.Switch.run @@ fun sw ->
  with_held_lock ~sw ~env ~lock @@ fun () ->
  (match Eio_fs_unix.range_holder fd ~file_offset:10 ~length:5 with
  | `Held (Some pid) ->
      Alcotest.(check bool)
        "the holder is another process" true
        (pid <> Unix.getpid ())
  | `Held None | `Free -> Alcotest.fail "the holding process is not named");
  Alcotest.(check bool)
    "a read lock is refused under another's write lock" false
    (Eio_fs_unix.lock_range fd `Read ~file_offset:10 ~length:5)

(* A lease belongs to its open file description, so a lease of this process
   excludes another of its own: a blocking one is granted once nothing holds
   it. *)
let lease_free ~env:_ lock =
  Eio_fs_unix.Lease.with_exclusive ~block:`Yes lock (fun _ -> ())

(* A lease wait cancelled in a domain of its own lets that domain end while
   the holder still holds, and the holder lets go when told. A wait
   that held the domain would end only once the holder let go at its own
   bound, which it answers as [`Exited 3]. *)
let test_cancelled_lease_wait_ends_its_domain () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind:"lease" ~lock in
  Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
      let waiting, now_waiting = Eio.Promise.create () in
      Eio.Fiber.first
        (fun () ->
          take_lease
            ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
            lock
            (fun () -> Alcotest.fail "a held lease was granted"))
        (fun () -> Eio.Promise.await waiting));
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* The holder lets go only when a sibling fiber tells it to, and the sibling
   runs only once the wait has begun: a wait that held the domain would never
   be answered, and the holder would let go at its own bound, exiting 3. *)
let test_lock_wait_does_not_block_eio () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind:"record" ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let progressed = ref false and ended = ref (`Exited (-1)) in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock
        ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
        lock
        (fun () -> ()))
    (fun () ->
      Eio.Promise.await waiting;
      progressed := true;
      ended := release ());
  Alcotest.(check bool)
    "another fiber progressed while waiting" true !progressed;
  Alcotest.check status "the holder let go when told" (`Exited 0) !ended

let test_lock_serialises_fibers () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* Two names of one file, here a hard link, share one fiber mutex: the
   exclusion is the file's, not the name's. *)
let test_lock_excludes_another_name_for_the_locked_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (dir / "lock") "";
  Unix.link
    (Eio.Path.native_exn (dir / "lock"))
    (Eio.Path.native_exn (dir / "alias"));
  let held, hold = Eio.Promise.create () in
  let checked, check = Eio.Promise.create () in
  let granted = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock (dir / "lock") @@ fun () ->
      Eio.Promise.resolve hold ();
      Eio.Promise.await checked)
    (fun () ->
      Eio.Promise.await held;
      (match
         Eio_fs_unix.with_lock ~block:`No (dir / "alias") (fun () -> ())
       with
      | () -> granted := true
      | exception Eio_fs_unix.Lock_unavailable _ -> ());
      Eio.Promise.resolve check ());
  Alcotest.(check bool)
    "a second name for the held file was not granted the lock" false !granted

(* A lock file under a dangling parent link raises [Not_found] through the
   capability, not [Unix_error] from an unconfined [realpath]. *)
let test_lock_answers_a_parent_link_that_leads_nowhere () =
  with_real_dir @@ fun dir ->
  Eio.Path.symlink ~link_to:"gone" (dir / "parent");
  match Eio_fs_unix.with_lock (dir / "parent" / "lock") (fun () -> ()) with
  | () ->
      Alcotest.fail "a lock file under a link that points nowhere was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let test_lock_updates_contents_in_place () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock ~contents:"first\n" lock (fun () -> ());
  let before = Eio.Path.stat ~follow:false lock in
  Eio_fs_unix.with_lock ~contents:"second\n" lock (fun () ->
      let while_locked = Eio.Path.stat ~follow:false lock in
      Alcotest.(check int64) "same locked inode" before.ino while_locked.ino;
      Alcotest.(check string)
        "current diagnostic" "second\n" (Eio.Path.load lock));
  let after = Eio.Path.stat ~follow:false lock in
  Alcotest.(check int64) "inode survives acquisition" before.ino after.ino

let test_lock_non_blocking_unavailable () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind:"record" ~lock in
  (match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
  | () -> Alcotest.fail "non-blocking lock unexpectedly succeeded"
  | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- a refusal names who holds the lock ----

   A path says which resource is busy, not who has it, and the reader of a
   refusal is left to find the holder with [lsof]. The kernel knows the
   process holding a record lock ([fcntl F_GETLK], POSIX.1-2017 fcntl, "the
   l_pid ... of the lock that would prevent this lock"), so the printed
   refusal names it. A holder's pid differs from this process's, so the case
   reds on a refusal that names nobody and on one that names the asker. *)

(* [printed] names process [pid] as a whole number, not a prefix of one. *)
let names_process printed pid =
  let needle = Fmt.str "process %d" pid in
  let n = String.length needle and len = String.length printed in
  let rec from i =
    i + n <= len
    && (String.sub printed i n = needle
        && (i + n = len
           || not (match printed.[i + n] with '0' .. '9' -> true | _ -> false))
       || from (i + 1))
  in
  from 0

let test_lock_refusal_names_the_holding_process () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~kind:"record" ~lock in
  let holder = Eio.Process.pid child in
  (match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
  | () -> Alcotest.fail "a lock another process holds was granted"
  | exception (Eio_fs_unix.Lock_unavailable refused as refusal) ->
      let printed = Printexc.to_string refusal in
      if not (names_process printed holder) then
        Alcotest.failf "the refusal %S does not name the holder, process %d"
          printed holder;
      Alcotest.(check (option int))
        "the holder the refusal carries" (Some holder) refused.holder);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* The mutex half of the exclusion is held by a fiber of this process, so the
   holder a refusal carries there is this process, which it prints as another
   fiber of it, never as a pid that reads as a foreign holder, and the record
   it carries is what that fiber wrote through [contents]. *)
let test_lock_refusal_names_this_process_for_a_fiber () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let record = "the holding fiber's record\n" in
  Eio_fs_unix.with_lock ~contents:record lock @@ fun () ->
  match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
  | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
  | exception (Eio_fs_unix.Lock_unavailable refused as refusal) ->
      let printed = Printexc.to_string refusal in
      let self = Unix.getpid () in
      if names_process printed self then
        Alcotest.failf "the refusal %S names this process, %d, as a holder"
          printed self;
      Alcotest.(check (option int))
        "the holder the refusal carries" (Some self) refused.holder;
      Alcotest.(check string) "the holder's record" record refused.contents

(* ---- asking from the holding process keeps the hold ----

   POSIX drops every record lock a process holds on a file when that process
   closes any descriptor for it (POSIX.1-2017 fcntl, "all locks associated with
   a file for a given process shall be removed when a file descriptor for that
   file is closed by that process"). A second acquisition from the holding
   process opens the lock file to find its key, so the descriptor it closes on
   the way out of a refusal is the holder's release. Only another process can
   tell: in this one the fiber mutex still answers. *)

let another_process_is_granted ~env lock =
  let self = Sys.executable_name in
  Eio.Switch.run @@ fun sw ->
  let child =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      ~executable:self
      [ self; "--try-eio-fs-lock"; Eio.Path.native_exn lock ]
  in
  match Eio.Process.await child with
  | `Exited 0 -> true
  | `Exited 3 -> false
  | `Exited n -> Alcotest.failf "lock asker exited %d" n
  | `Signaled n -> Alcotest.failf "lock asker took signal %d" n

let asked_from_the_holder ~block () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock lock @@ fun () ->
  (match Eio_fs_unix.with_lock ~block lock (fun () -> ()) with
  | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
  | exception Eio_fs_unix.Lock_unavailable _ -> ());
  if another_process_is_granted ~env lock then
    Alcotest.fail
      "another process was granted the lock after a refusal in the holder"

let test_lock_refusal_in_the_holder_keeps_its_hold () =
  asked_from_the_holder ~block:`No ()

let test_lock_bounded_wait_in_the_holder_keeps_its_hold () =
  asked_from_the_holder ~block:(`For 0.05) ()

(* A descriptor handed to the holder is closed when the hold ends, not kept:
   each refused ask from the holder would otherwise cost the process one
   descriptor for as long as it runs. *)
let open_descriptors () = Array.to_list (Sys.readdir "/dev/fd")

(* The descriptors open now that were not in [before]. A count would also see
   an unrelated descriptor of the runtime closed in between. *)
let opened_since before =
  List.filter (fun fd -> not (List.mem fd before)) (open_descriptors ())

let test_lock_asks_from_the_holder_leave_no_descriptor () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let before = open_descriptors () in
  ( Eio_fs_unix.with_lock lock @@ fun () ->
    for _ = 1 to 3 do
      match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
      | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
      | exception Eio_fs_unix.Lock_unavailable _ -> ()
    done );
  Alcotest.(check (list string))
    "descriptors opened once the hold ends" [] (opened_since before)

(* Waits against another process's hold, ended by their [`For] bound or by a
   cancel, leave no descriptor behind while the holder still holds; a waiter
   is then granted once the holder lets go. [take_for seconds lock fn] is the
   bounded acquisition, [take] the unbounded one. *)
let ended_waits_leave_no_descriptor ~kind ~take_for take () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind ~lock in
  let bounded () =
    match take_for 0.01 lock ignore with
    | () -> Alcotest.fail "a lock another process holds was granted"
    | exception Eio_fs_unix.Lock_unavailable _ -> ()
  in
  let cancelled () =
    let waiting, now_waiting = Eio.Promise.create () in
    Eio.Fiber.first
      (fun () ->
        take
          ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
          lock
          (fun () -> Alcotest.fail "a lock another process holds was granted"))
      (fun () -> Eio.Promise.await waiting)
  in
  bounded ();
  let before = open_descriptors () in
  for _ = 1 to 6 do
    bounded ();
    cancelled ()
  done;
  Alcotest.(check (list string))
    "descriptors opened by the ended waits, the holder holding" []
    (opened_since before);
  let waiting, now_waiting = Eio.Promise.create () in
  let granted, now_granted = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      take
        ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
        lock
        (fun () -> Eio.Promise.resolve now_granted ()));
  Eio.Promise.await waiting;
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ());
  Eio.Promise.await granted

(* The descriptors this process holds open on [lock]'s file, each of [/dev/fd]
   matched by inode: macOS's [/dev/fd] reports its own device for each. *)
let descriptors_on lock =
  let ino = (Unix.stat (Eio.Path.native_exn lock)).st_ino in
  Sys.readdir "/dev/fd"
  |> Array.fold_left
       (fun n fd ->
         match Unix.stat (Filename.concat "/dev/fd" fd) with
         | { st_kind = Unix.S_REG; st_ino; _ } when st_ino = ino -> n + 1
         | _ | (exception Unix.Unix_error _) -> n)
       0

(* A holder that holds for the life of its process, as a server holds its
   serve lock, is asked again and again from that process: every ask leaves
   the process with the holder's one descriptor on the file, never one more,
   so the asks of a long life cannot reach the descriptor limit. *)
let test_lock_asks_during_a_hold_keep_one_descriptor () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  (* FreeBSD's [/dev/fd] lists only 0, 1 and 2 unless fdescfs is mounted. *)
  Eio.Path.save ~create:(`Exclusive 0o644) lock "";
  let seen =
    let fd = Unix.openfile (Eio.Path.native_exn lock) [ O_RDONLY ] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> descriptors_on lock = 1)
  in
  Eio.Path.unlink lock;
  if not seen then Alcotest.skip ();
  Eio_fs_unix.with_lock ~contents:"held for life\n" lock @@ fun () ->
  for round = 1 to 5 do
    (match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
    | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
    | exception Eio_fs_unix.Lock_unavailable _ -> ());
    (match Eio_fs_unix.with_lock ~block:(`For 0.01) lock (fun () -> ()) with
    | () -> Alcotest.fail "a lock a fiber of this process holds was granted"
    | exception Eio_fs_unix.Lock_unavailable _ -> ());
    (match Eio_fs_unix.probe lock with
    | `Free _ ->
        Alcotest.fail "a lock a fiber of this process holds probed free"
    | `Held refused ->
        Alcotest.(check string)
          "the holder's record" "held for life\n" refused.contents);
    Alcotest.(check int)
      (Fmt.str "descriptors on the lock file after %d rounds of asks" round)
      1 (descriptors_on lock)
  done

(* An ask from the holding process reads the holder's record through the
   holder's descriptor, and the holder may let go of the lock, closing that
   descriptor, at any point of the ask: every ask still answers, the lock held
   or free, and never reads a closed descriptor. The holder releases with a
   last record, whose write suspends it while it still holds, and the ask
   starts after 0 to 19 yields of its own, so the close lands at each
   suspension point of the ask in turn. *)
let test_lock_asks_while_the_holder_lets_go () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let ask name f =
    for yields = 0 to 19 do
      Eio.Switch.run @@ fun sw ->
      let held =
        Eio_fs_unix.Held.take ~sw ~contents:"held for the ask\n" lock
      in
      Eio.Fiber.both
        (fun () -> Eio_fs_unix.Held.release ~contents:"let go\n" held)
        (fun () ->
          for _ = 1 to yields do
            Eio.Fiber.yield ()
          done;
          match f () with
          | () -> ()
          | exception Invalid_argument message ->
              Alcotest.failf "%s after %d yields: %s" name yields message)
    done
  in
  ask "a probe" (fun () -> ignore (Eio_fs_unix.probe lock));
  ask "a refused ask" (fun () ->
      try Eio_fs_unix.with_lock ~block:`No lock ignore
      with Eio_fs_unix.Lock_unavailable _ -> ())

(* ---- a probe says who holds a lock without taking it ----

   Asking by acquiring answers "free" only by holding the lock for a moment,
   and a real acquirer in another process that asks in that moment is refused
   by a caller that wanted nothing. A probe asks the kernel ([fcntl F_GETLK])
   and the fiber half, and takes nothing. *)

let check_free_probe what lock ~record =
  match Eio_fs_unix.probe lock with
  | `Free left ->
      Alcotest.(check string) (what ^ ": the record left") record left
  | `Held (r : Eio_fs_unix.refusal) ->
      Alcotest.failf "%s: a free lock probed as held by %a" what
        Fmt.(option ~none:(any "nobody") int)
        r.holder

let test_probe_names_the_holding_process () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock ~contents:"the last holder\n" lock (fun () -> ());
  check_free_probe "before the child" lock ~record:"the last holder\n";
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~kind:"record" ~lock in
  (match Eio_fs_unix.probe lock with
  | `Free _ -> Alcotest.fail "a lock another process holds probed as free"
  | `Held refused ->
      Alcotest.(check (option int))
        "the holder the probe names"
        (Some (Eio.Process.pid child))
        refused.holder);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ());
  check_free_probe "after the child" lock ~record:"the last holder\n"

(* Through a wrapper with no fault, a probe answers as it does on the host
   directory beneath: a file the wrapper opened for reading hands eio-fs.unix
   the host descriptor the kernel is asked on. *)
let test_probe_through_a_wrapper_names_the_holding_process () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio_fs_unix.with_lock ~contents:"the last holder\n" lock (fun () -> ());
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~kind:"record" ~lock in
  let wrapped = Chaos.wrap (fun _ -> None) dir in
  (match Eio_fs_unix.probe (wrapped / "lock") with
  | `Free _ -> Alcotest.fail "a lock another process holds probed as free"
  | `Held refused ->
      Alcotest.(check (option int))
        "the holder the probe names"
        (Some (Eio.Process.pid child))
        refused.holder);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* A probe from the holding process names it, and like any ask from there it
   keeps the hold: only another process can tell. *)
let test_probe_from_the_holder_keeps_its_hold () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  let record = "the holding fiber's record\n" in
  Eio_fs_unix.with_lock ~contents:record lock @@ fun () ->
  (match Eio_fs_unix.probe lock with
  | `Free _ -> Alcotest.fail "a lock a fiber of this process holds probed free"
  | `Held refused ->
      Alcotest.(check (option int))
        "the holder the probe names"
        (Some (Unix.getpid ()))
        refused.holder;
      Alcotest.(check string) "the holder's record" record refused.contents);
  if another_process_is_granted ~env lock then
    Alcotest.fail "another process was granted the lock after a probe"

(* A probe takes nothing, so it needs no more than to read the lock file: a
   lock file this user may read and not write is still answered. *)
let test_probe_answers_a_read_only_lock_file () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Eio.Path.save ~create:(`Exclusive 0o400) lock "written under other rights\n";
  check_free_probe "the probe" lock ~record:"written under other rights\n"

(* A lock file can go while a probe reads it: a build slot's ticket is
   removed by its holder once the slot is its own, while a watcher probes
   every ticket. The probe opens the file and then weighs the name against
   what it opened; a name that holds nothing by then is a lock that is
   absent, as one absent when the probe began, and never a name that is
   not a lock file. The file goes at the first stat after the open, the
   moment between the two, through the file system the probe reads. *)
let test_probe_of_a_lock_file_that_goes () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "ticket") "a waiting check\n";
  let native = Eio.Path.native_exn (dir / "ticket") in
  Eio.Path.with_open_dir dir @@ fun base ->
  let opened = ref false and gone = ref false in
  let schedule (operation : Chaos.Op.t) =
    (match operation.kind with
    | `Open_read | `Open_write -> opened := true
    | `Stat when !opened && not !gone ->
        Unix.unlink native;
        gone := true
    | _ -> ());
    None
  in
  let wrapped = Chaos.wrap schedule base in
  (match Eio_fs_unix.probe (wrapped / "ticket") with
  | `Free _ | `Held _ -> Alcotest.fail "a lock file that went was answered"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
      Alcotest.fail "a lock file that went was refused as no lock file");
  Alcotest.(check bool) "the file went between the open and the stat" true !gone

(* A lock file's name can be given to a new file while a probe reads the
   old one: a build slot numbers a new ticket after the highest left, so
   the first ticket's name comes back once every ticket went. The probe
   answers for the file the name holds when it answers, the new one, and
   never refuses the name as not a lock file. The old file goes and the new
   one takes its name at the first stat after the open. *)
let test_probe_of_a_lock_file_replaced () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "ticket") "the old ticket\n";
  let native = Eio.Path.native_exn (dir / "ticket") in
  Eio.Path.with_open_dir dir @@ fun base ->
  let opened = ref false and replaced = ref false in
  let schedule (operation : Chaos.Op.t) =
    (match operation.kind with
    | `Open_read | `Open_write -> opened := true
    | `Stat when !opened && not !replaced ->
        Unix.unlink native;
        Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_excl ]
          0o600 native (fun oc -> output_string oc "the new ticket\n");
        replaced := true
    | _ -> ());
    None
  in
  let wrapped = Chaos.wrap schedule base in
  (match Eio_fs_unix.probe (wrapped / "ticket") with
  | `Free record ->
      Alcotest.(check string) "the new file's record" "the new ticket\n" record
  | `Held _ -> Alcotest.fail "a free lock file was answered as held"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
      Alcotest.fail "a lock file replaced was refused as no lock file");
  Alcotest.(check bool)
    "the file was replaced between the open and the stat" true !replaced

(* A held lock not released lasts the rest of its switch: taken when [take]
   returns, still held while the switch runs, and let go when it finishes. *)
let test_hold_lasts_as_long_as_its_switch () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let record = "held for the switch\n" in
  Eio.Switch.run (fun sw ->
      ignore
        (Eio_fs_unix.Held.take ~sw ~contents:record lock : Eio_fs_unix.Held.t);
      match Eio_fs_unix.with_lock ~block:`No lock (fun () -> ()) with
      | () -> Alcotest.fail "a lock held for the switch was granted"
      | exception Eio_fs_unix.Lock_unavailable refused ->
          Alcotest.(check (option int))
            "held by this process"
            (Some (Unix.getpid ()))
            refused.holder;
          Alcotest.(check string) "the hold's record" record refused.contents);
  let granted = ref false in
  Eio_fs_unix.with_lock ~block:`No lock (fun () -> granted := true);
  Alcotest.(check bool) "the lock is free once the switch ends" true !granted

(* A hold refused by another process raises to its caller, naming the holder,
   and leaves nothing held. *)
let test_hold_refusal_names_the_holding_process () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~kind:"record" ~lock in
  ( Eio.Switch.run @@ fun hold_sw ->
    match Eio_fs_unix.Held.take ~sw:hold_sw ~block:`No lock with
    | (_ : Eio_fs_unix.Held.t) ->
        Alcotest.fail "a hold was granted over another process's lock"
    | exception Eio_fs_unix.Lock_unavailable refused ->
        Alcotest.(check (option int))
          "the holder the refusal carries"
          (Some (Eio.Process.pid child))
          refused.holder );
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- a held lock is restated and let go on demand ----

   A process that claims a file for as long as it runs states what it is
   doing in the lock file as it goes, and lets the claim go before it exits,
   leaving a last record for the reader who comes after it. Both writes go
   through the held descriptor: opening the file again to write it would be
   a close on the held file, and POSIX would release the lock with it. *)

let held_record what lock ~record =
  match Eio_fs_unix.probe lock with
  | `Free _ -> Alcotest.failf "%s: the lock probed as free" what
  | `Held (r : Eio_fs_unix.refusal) ->
      Alcotest.(check (option int))
        (what ^ ": held by this process")
        (Some (Unix.getpid ()))
        r.holder;
      Alcotest.(check string) (what ^ ": the record") record r.contents

let test_held_restates_its_record () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held = Eio_fs_unix.Held.take ~sw ~contents:"booting\n" lock in
  held_record "as taken" lock ~record:"booting\n";
  Eio_fs_unix.Held.restate held "running\n";
  held_record "restated" lock ~record:"running\n";
  if another_process_is_granted ~env lock then
    Alcotest.fail "another process was granted a lock that was restated"

let test_held_release_lets_the_lock_go_and_leaves_a_record () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held = Eio_fs_unix.Held.take ~sw ~contents:"running\n" lock in
  Eio_fs_unix.Held.release ~contents:"ended\n" held;
  (match Eio_fs_unix.probe lock with
  | `Free record -> Alcotest.(check string) "the record left" "ended\n" record
  | `Held _ -> Alcotest.fail "a released lock probed as held");
  if not (another_process_is_granted ~env lock) then
    Alcotest.fail "another process was refused a released lock";
  Eio_fs_unix.Held.release held;
  match Eio_fs_unix.Held.restate held "again\n" with
  | () -> Alcotest.fail "a released lock was restated"
  | exception Invalid_argument _ -> ()

let test_held_ends_with_its_switch () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  let held =
    Eio.Switch.run @@ fun sw -> Eio_fs_unix.Held.take ~sw ~contents:"" lock
  in
  if not (another_process_is_granted ~env lock) then
    Alcotest.fail "another process was refused a lock whose switch ended";
  Eio_fs_unix.Held.release held

(* A holder that lets go when its switch ends does so from a release hook of
   that switch, where it knows its last record: the lock is still held there,
   so the record goes through the held descriptor and stays. *)
let test_held_released_from_its_switch_hook_leaves_a_record () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  ( Eio.Switch.run @@ fun sw ->
    let held = Eio_fs_unix.Held.take ~sw ~contents:"running\n" lock in
    Eio.Switch.on_release sw (fun () ->
        Eio_fs_unix.Held.release ~contents:"ended\n" held) );
  match Eio_fs_unix.probe lock with
  | `Free record -> Alcotest.(check string) "the record left" "ended\n" record
  | `Held _ -> Alcotest.fail "a lock whose switch ended probed as held"

(* A close of the held file elsewhere in this process drops the record lock
   under the holder; [restate] takes it again before it writes. *)
let test_held_restate_takes_back_a_lock_a_stray_close_dropped () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let held = Eio_fs_unix.Held.take ~sw ~contents:"running\n" lock in
  Unix.close (Unix.openfile (Eio.Path.native_exn lock) [ Unix.O_RDONLY ] 0);
  Eio_fs_unix.Held.restate held "still running\n";
  if another_process_is_granted ~env lock then
    Alcotest.fail "another process was granted a lock its holder restated"

(* ---- waiting for this process, answering about every other one ----

   The two halves of the exclusion are two kinds of contention, and a caller
   can want a different answer to each. The fibers of one program are its own
   cooperating writers: one that finds another inside wants its turn, because
   refusing it fails work the program itself asked for and could have waited
   out. Another process is an independent actor, and how long it holds the
   lock file is not this program's to bound: a command-line tool would rather
   tell its operator which resource is busy than wait for a holder that may be
   wedged, with nothing printed and no deadline.

   [`Fibers] is that pair: entering queues behind a fiber of this process, and
   a hold taken by any other process is answered rather than waited out. *)
let test_lock_fibers_mode_queues_behind_a_fiber () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false and overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock ~block:`Fibers lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock ~block:`Fibers lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the second fiber took its turn rather than being refused" true !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* Refusing rather than waiting is what the answer proves: a caller that waited
   out the holder would be granted the lock once the child exits and return
   unit, so [Lock_unavailable] can only come from an acquisition that did not
   wait. *)
let test_lock_fibers_mode_refuses_another_process () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind:"record" ~lock in
  (match Eio_fs_unix.with_lock ~block:`Fibers lock (fun () -> ()) with
  | () ->
      Alcotest.fail "another process's hold was waited out rather than answered"
  | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- a bound is a wait a caller can plan against ----

   [`Yes] and [`No] are the two answers a second writer of a shared store can
   be given, and neither suits several checkouts publishing into one cache.
   [`For seconds] waits and answers: a caller that knows the bound can name it
   before it waits and act when the wait is over. Nothing orders the processes
   contending for a record lock, so the answer comes no sooner than the bound
   and does not wait out a holder that is still there. *)
let test_lock_bounded_wait_answers_at_the_bound () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind:"record" ~lock in
  let mono = Eio_mock.Clock.Mono.make () in
  let waiting, now_waiting = Eio.Promise.create () in
  let answer, answered = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve answered
        (on_mono
           (mono :> Eio.Time.Mono.ty Eio.Resource.t)
           (fun () ->
             match
               Eio_fs_unix.with_lock ~block:(`For 0.2)
                 ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
                 lock
                 (fun () -> `Granted)
             with
             | answer -> answer
             | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
                 `Refused path)));
  Eio.Promise.await waiting;
  Alcotest.(check bool)
    "no answer before the bound" false
    (Eio.Promise.is_resolved answer);
  Eio_mock.Clock.Mono.advance mono;
  (match Eio.Promise.await answer with
  | `Granted -> Alcotest.fail "a lock held for the whole bound was granted"
  | `Refused path ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Alcotest.check status "the holder still held it, and let go when told"
    (`Exited 0) (release ())

(* The fibers of this process are the other holder a bound has to cover. Here
   the record lock is free and a fiber of this program is inside, so what the
   bound answers about is the mutex alone: without it, a caller told "up to a
   fifth of a second" would wait a fifth for the fiber and a fifth more for the
   record lock, which is not the number it was told. The bound is on a clock
   the case moves once the waiter says it waits, and the fiber lets go only
   after the answer, so the answer can come from nothing but the bound. *)
let test_lock_bounded_wait_answers_a_fiber_that_holds_it () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let mono = Eio_mock.Clock.Mono.make () in
  let entered, set_entered = Eio.Promise.create () in
  let release, let_go = Eio.Promise.create () in
  let waiting, now_waiting = Eio.Promise.create () in
  let answer, answered = Eio.Promise.create () in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      Eio.Promise.resolve set_entered ();
      Eio.Promise.await release);
  Eio.Promise.await entered;
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve answered
        (on_mono
           (mono :> Eio.Time.Mono.ty Eio.Resource.t)
           (fun () ->
             match
               Eio_fs_unix.with_lock ~block:(`For 0.2)
                 ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
                 lock
                 (fun () -> `Granted)
             with
             | answer -> answer
             | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
                 `Refused path)));
  Eio.Promise.await waiting;
  Alcotest.(check bool)
    "no answer before the bound" false
    (Eio.Promise.is_resolved answer);
  Eio_mock.Clock.Mono.advance mono;
  (match Eio.Promise.await answer with
  | `Granted ->
      Alcotest.fail "a lock a fiber held for the whole bound was granted"
  | `Refused path ->
      Alcotest.(check string) "unavailable path" (Eio.Path.native_exn lock) path);
  Eio.Promise.resolve let_go ()

(* A wait with no bound behind a fiber of this process says so too, once, and
   is granted when the fiber lets go. *)
let test_lock_wait_behind_a_fiber_says_so () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let entered, set_entered = Eio.Promise.create () in
  let release, let_go = Eio.Promise.create () in
  let waiting, now_waiting = Eio.Promise.create () in
  let granted, now_granted = Eio.Promise.create () in
  let waits = ref 0 in
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      Eio.Promise.resolve set_entered ();
      Eio.Promise.await release);
  Eio.Promise.await entered;
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.with_lock ~block:`Yes
        ~on_wait:(fun () ->
          incr waits;
          ignore (Eio.Promise.try_resolve now_waiting ()))
        lock
      @@ fun () -> Eio.Promise.resolve now_granted ());
  Eio.Promise.await waiting;
  Alcotest.(check bool)
    "not granted while the fiber holds it" false
    (Eio.Promise.is_resolved granted);
  Eio.Promise.resolve let_go ();
  Eio.Promise.await granted;
  Alcotest.(check int) "said it waits once" 1 !waits

(* A killed holder frees the waiter at once: the kernel drops the record
   lock with the last descriptor, on a clock that never moves. *)
let test_lock_bounded_wait_does_not_sit_out_a_killed_holder () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, _ = holding_process ~sw ~env ~kind:"record" ~lock in
  let waiting, now_waiting = Eio.Promise.create () in
  let granted = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio.Promise.await waiting;
      Eio.Process.signal child Sys.sigkill)
    (fun () ->
      let (), _ =
        counting_waits (fun () ->
            Eio_fs_unix.with_lock ~block:(`For 30.)
              ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
              lock
              (fun () -> granted := true))
      in
      ());
  Alcotest.(check bool) "the lock was taken once its holder died" true !granted;
  match Eio.Process.await child with
  | `Signaled n when Int.equal n Sys.sigkill -> ()
  | `Exited n ->
      Alcotest.failf "the holder exited %d rather than being killed" n
  | `Signaled n -> Alcotest.failf "the holder took signal %d" n

let test_lock_existing_does_not_create () =
  with_real_dir @@ fun dir ->
  let lock = dir / "absent" in
  (match Eio_fs_unix.with_lock ~create:`Never lock (fun () -> ()) with
  | () -> Alcotest.fail "an absent lock file was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ());
  Alcotest.(check bool)
    "probing does not create the lock file" false (Eio.Path.is_file lock)

(* The lock path is its own regular file: anything else under that name is
   refused with [Permission_denied], naming the lock path. *)

let check_refusal what path fn =
  let native = Eio.Path.native_exn path in
  match fn () with
  | () -> Alcotest.failf "%s at the lock path was locked" what
  | exception (Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) as e) ->
      Alcotest.(check bool)
        (what ^ ": the refusal names the lock path")
        true
        (mentions ~needle:native (Printexc.to_string e))

(* A link to a sibling regular file: the shape an adversary plants, and the
   shape a stale deployment leaves behind. The sibling must come back neither
   locked nor written. *)
let test_lock_refuses_a_symlink_at_the_lock_path () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (dir / "target") "target\n";
  Eio.Path.symlink ~link_to:"target" (dir / "lock");
  check_refusal "a symbolic link" (dir / "lock") (fun () ->
      Eio_fs_unix.with_lock ~contents:"holder\n" (dir / "lock") (fun () -> ()));
  Alcotest.(check string)
    "the link's target was not written through the link" "target\n"
    (Eio.Path.load (dir / "target"));
  (* The target's own path still locks, so the refusal took nothing with it. *)
  Eio_fs_unix.with_lock (dir / "target") (fun () -> ())

(* A dangling link is the sharper case: creating the lock file through it
   creates a file wherever the link points, so the refusal has to come before
   the open, not after it. *)
let test_lock_refuses_a_dangling_symlink_without_creating_its_target () =
  with_real_dir @@ fun dir ->
  Eio.Path.symlink ~link_to:"elsewhere" (dir / "lock");
  check_refusal "a dangling symbolic link" (dir / "lock") (fun () ->
      Eio_fs_unix.with_lock (dir / "lock") (fun () -> ()));
  Alcotest.(check bool)
    "the link's target was not created" false
    (Eio.Path.is_file (dir / "elsewhere"))

(* An entry planted between the stat of the lock name and its open is
   refused. [Plant_after_stat] answers a stat, then takes the name with a
   dangling link. *)
module Plant_after_stat = struct
  type t = {
    base : Eio.Fs.dir_ty Eio.Resource.t;
    link_to : string;
    planted : bool ref;
  }

  let at t path = (t.base, path)

  let plant t path =
    if not !(t.planted) then begin
      t.planted := true;
      Eio.Path.symlink ~link_to:t.link_to (at t path)
    end

  let stat t ~follow path =
    Fun.protect
      ~finally:(fun () -> plant t path)
      (fun () -> Eio.Path.stat ~follow (at t path))

  let open_in t ~sw ~follow path = Eio.Path.open_in ~sw ~follow (at t path)

  let open_out t ~sw ~follow ~append ~create path =
    Eio.Path.open_out ~sw ~follow ~append ~create (at t path)

  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  let sync_dir t path = Eio.Path.sync_dir (at t path)
  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "plant-after-stat"

  let open_subtree _ ~sw:_ _ =
    failwith "Plant_after_stat: the lock opens no subtree"

  include Eio_utils.Posix_path
end

let plant_after_stat ~link_to ((base, _) : Eio.Fs.dir_ty Eio.Path.t) =
  let t = { Plant_after_stat.base; link_to; planted = ref false } in
  (Eio.Resource.T
     ( t,
       Eio.Resource.handler
         [ Eio.Resource.H (Eio.Fs.Pi.Dir, (module Plant_after_stat)) ] )
    : Eio.Fs.dir_ty Eio.Resource.t)

(* A link planted in that window is followed by the open, and a creating open
   that follows one creates the file at the far end: an empty, owner-writable
   file wherever the party that planted the link chose, left behind after the
   lock itself was refused. Refusing is only half the guarantee; the other half
   is that the attempt created nothing. *)
let test_lock_creates_nothing_through_a_link_planted_after_the_check () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  let racing =
    (plant_after_stat ~link_to:"victim" dir, Eio.Path.native_exn lock)
  in
  check_refusal "a link planted after the name check" lock (fun () ->
      Eio_fs_unix.with_lock racing (fun () -> ()));
  Alcotest.(check bool)
    "the planted link took the name" true
    ((Eio.Path.stat ~follow:false lock).kind = `Symbolic_link);
  Alcotest.(check bool)
    "the link's target was not created" false
    (Eio.Path.is_file (dir / "victim"))

(* Neither a directory nor a FIFO carries the record lock the interface
   promises, so both are refused in the same vocabulary rather than surfacing
   as whichever [open] error the platform happens to pick. *)
let test_lock_refuses_a_directory_or_a_fifo () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "as-a-directory");
  Unix.mkfifo (Eio.Path.native_exn (dir / "as-a-fifo")) 0o600;
  check_refusal "a directory" (dir / "as-a-directory") (fun () ->
      Eio_fs_unix.with_lock (dir / "as-a-directory") (fun () -> ()));
  check_refusal "a FIFO" (dir / "as-a-fifo") (fun () ->
      Eio_fs_unix.with_lock (dir / "as-a-fifo") (fun () -> ()))

(* An absent lock file is still created, as its own owner-only regular file:
   refusing what is not a regular file must not refuse the ordinary first
   acquisition. *)
let test_lock_creates_a_missing_lock_file_as_a_regular_file () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lock" in
  Alcotest.(check bool)
    "no lock file before the first acquisition" false (Eio.Path.is_file lock);
  Eio_fs_unix.with_lock lock (fun () ->
      let held = Eio.Path.stat ~follow:false lock in
      Alcotest.(check bool)
        "the lock file is a regular file while held" true
        (held.kind = `Regular_file);
      Alcotest.(check int) "created owner-only" 0o600 held.perm);
  let after = Eio.Path.stat ~follow:false lock in
  Alcotest.(check bool)
    "the lock file survives as a regular file" true
    (after.kind = `Regular_file)

(* A lock file with no descriptor. A backend that declares no opener outside
   this process, as the in-memory one does, holds it under the in-process
   mutex alone. Any other lock file with no descriptor is refused. *)

let test_lock_over_a_process_local_backend_serialises_fibers () =
  with_in_memory_fs @@ fun fs ->
  let lock = fs / "lock" in
  Alcotest.(check bool)
    "no name outside this process reaches the lock file" true
    (Option.is_none (Eio.Path.native lock));
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the critical section ran over a backend with no descriptors" true
    !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped;
  Alcotest.(check bool)
    "the lock file is the backend's own regular file" true
    ((Eio.Path.stat ~follow:false lock).kind = `Regular_file)

(* A bound is a wait's: an acquisition nobody holds against waits for nothing,
   so it answers without the system clock the bound would run on. The mock
   backend handles no system clock, so a bound started anyway raises there. *)
let test_lock_bounded_and_uncontended_needs_no_clock () =
  with_in_memory_fs @@ fun fs ->
  let ran = ref 0 in
  Eio_fs_unix.with_lock ~block:(`For 30.) (fs / "lock") (fun () -> incr ran);
  Eio_fs_unix.with_lock ~block:(`For 30.) (fs / "lock") (fun () -> incr ran);
  Alcotest.(check int) "both critical sections ran" 2 !ran

(* Holding under the mutex alone is not a licence to skip what the name is
   weighed for: the in-memory backend has links too, and a link at the lock
   name is not the lock's own file whatever the exclusion behind it is. *)
let test_lock_over_a_process_local_backend_refuses_a_symlink () =
  with_in_memory_fs @@ fun fs ->
  Eio.Path.save ~create:(`Or_truncate 0o600) (fs / "target") "target\n";
  Eio.Path.symlink ~link_to:"target" (fs / "lock");
  (match
     Eio_fs_unix.with_lock ~contents:"holder\n" (fs / "lock") (fun () -> ())
   with
  | () -> Alcotest.fail "a symbolic link at the lock path was locked"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
  Alcotest.(check string)
    "the link's target was not written through the link" "target\n"
    (Eio.Path.load (fs / "target"))

(* A wrapper over that backend is the same case, and it is the case a store
   under test is in: [Eio_fs_chaos.Private] perturbs or crash-models the tree beneath
   it, and over the in-memory backend the tree it perturbs is still this
   process's heap. Nothing the wrapper adds gives those files a name to open
   them by or a descriptor to inherit, so the exclusion they admit is the one
   the base declares, and the wrapper's business is to say so rather than to
   answer for a base it did not look at. A wrapper that declares nothing turns
   a lock file this interface holds under the mutex into one it refuses, which
   is the whole of a fault-injected store's locking gone. *)
let test_lock_over_a_wrapped_process_local_backend_serialises_fibers () =
  with_in_memory_fs @@ fun fs ->
  let wrapped = Chaos.wrap (fun _ -> None) fs in
  let lock = wrapped / "lock" in
  Alcotest.(check bool)
    "the wrapper forwards what its base declares about outside openers" true
    (Eio_fs.Sync.is_process_local (fst lock));
  Alcotest.(check bool)
    "no name outside this process reaches the lock file" true
    (Option.is_none (Eio.Path.native lock));
  let entered, set_entered = Eio.Promise.create () in
  let first_inside = ref false in
  let second_ran = ref false in
  let overlapped = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.with_lock lock @@ fun () ->
      first_inside := true;
      Eio.Promise.resolve set_entered ();
      Eio.Fiber.yield ();
      Eio.Fiber.yield ();
      first_inside := false)
    (fun () ->
      Eio.Promise.await entered;
      Eio_fs_unix.with_lock lock @@ fun () ->
      second_ran := true;
      if !first_inside then overlapped := true);
  Alcotest.(check bool)
    "the critical section ran through the wrapper" true !second_ran;
  Alcotest.(check bool)
    "same-process critical sections did not overlap" false !overlapped

(* The lock is granted in the order callers entered, even when their opens
   come back in the other order. [Slow_open] yields before an open as many
   turns as the entering fiber asks. *)
module Slow_open = struct
  type t = { base : Eio.Fs.dir_ty Eio.Resource.t }

  (* How many scheduler turns the entering fiber spends inside each open. *)
  let turns : int Eio.Fiber.key = Eio.Fiber.create_key ()

  let dawdle () =
    match Eio.Fiber.get turns with
    | None | Some 0 -> ()
    | Some n ->
        for _ = 1 to n do
          Eio.Fiber.yield ()
        done

  let at t path = (t.base, path)

  let open_out t ~sw ~follow ~append ~create path =
    dawdle ();
    Eio.Path.open_out ~sw ~follow ~append ~create (at t path)

  let open_in t ~sw ~follow path =
    dawdle ();
    Eio.Path.open_in ~sw ~follow (at t path)

  let stat t ~follow path = Eio.Path.stat ~follow (at t path)
  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  let sync_dir t path = Eio.Path.sync_dir (at t path)
  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "slow-open"
  let open_subtree _ ~sw:_ _ = failwith "Slow_open: the lock opens no subtree"

  include Eio_utils.Posix_path
end

let slow_open ((base, path) : Eio.Fs.dir_ty Eio.Path.t) =
  let dir =
    Eio.Resource.T
      ( { Slow_open.base },
        Eio.Resource.handler
          [
            Eio.Resource.H (Eio.Fs.Pi.Dir, (module Slow_open));
            (* The wrapper answers for the base it was given, as every other
               wrapper here does: over the in-memory backend the files it hands
               out are still this process's heap. *)
            Eio.Resource.H
              ( Eio_fs.Sync.Process_local,
                fun (t : Slow_open.t) -> Eio_fs.Sync.is_process_local t.base );
          ] )
  in
  ((dir :> Eio.Fs.dir_ty Eio.Resource.t), path)

let test_lock_is_granted_in_the_order_fibers_entered () =
  with_in_memory_fs @@ fun fs ->
  let lock = slow_open fs / "lock" in
  let entered = ref [] and held = ref [] in
  (* Each fiber dawdles for a different number of turns inside its opens, the
     first to enter for the longest, so entry order and open order are exact
     reverses of one another. *)
  let contend name turns () =
    Eio.Fiber.with_binding Slow_open.turns turns @@ fun () ->
    entered := name :: !entered;
    Eio_fs_unix.with_lock lock @@ fun () -> held := name :: !held
  in
  Eio.Fiber.all [ contend "a" 4; contend "b" 2; contend "c" 0 ];
  Alcotest.(check (list string))
    "the fibers entered in this order" [ "a"; "b"; "c" ] (List.rev !entered);
  Alcotest.(check (list string))
    "the lock was granted in the order the fibers entered" [ "a"; "b"; "c" ]
    (List.rev !held)

(* Through the crash wrapper over a host directory, the lock is the host's
   record lock: a non-blocking take refused by another process names the lock
   file. *)
let test_lock_through_a_crash_wrapper_takes_the_host_lock () =
  with_real_dir_env @@ fun env dir ->
  Eio.Switch.run @@ fun sw ->
  with_held_lock ~sw ~env ~lock:(dir / "lock") @@ fun () ->
  let wrapped = Crash.wrap (Crash.v ~policy:Drop_unsynced ()) dir in
  let file =
    Eio.Path.open_out ~sw ~create:(`If_missing 0o600) (wrapped / "probe")
  in
  Alcotest.(check bool)
    "a file opened through the wrapper hands its caller no descriptor" true
    (Option.is_none (Eio_unix.Resource.fd_opt file));
  match Eio_fs_unix.with_lock ~block:`No (wrapped / "lock") (fun () -> ()) with
  | () ->
      Alcotest.fail
        "a wrapped lock file was locked while another process held its file"
  | exception Eio_fs_unix.Lock_unavailable { path; _ } ->
      Alcotest.(check string)
        "the refusal names the host file the lock is held on"
        (Eio.Path.native_exn (dir / "lock"))
        path

(* What a lock writes is a write like any other: [~contents] goes through the
   wrapper's own file, so the crash model sees it and the policy decides
   whether it survives the cut. The lock file's own entry is committed in both
   runs below, so what differs is only the policy's answer about an un-synced
   write. *)
let crash_after_a_lock_published_its_contents ~policy =
  with_real_dir @@ fun dir ->
  let ctl = Crash.v ~policy () in
  let wrapped = Crash.wrap ctl dir in
  Eio_fs_unix.with_lock ~contents:"held\n" (wrapped / "lock") (fun () -> ());
  Alcotest.(check string)
    "the live lock file carries what the holder published" "held\n"
    (Eio.Path.load (dir / "lock"));
  Eio.Path.sync_dir wrapped;
  Eio.Path.load (Crash.post_crash ctl / "lock")

(* The barrier commits the lock file's entry; the bytes the lock wrote and
   the length its truncation set are unsynced, so the file comes back
   empty. *)
let test_lock_contents_are_lost_by_an_unsynced_crash () =
  Alcotest.(check string)
    "the lock file keeps its committed entry, not the un-synced bytes" ""
    (crash_after_a_lock_published_its_contents ~policy:Crash.Drop_unsynced)

let test_lock_contents_survive_a_policy_that_keeps_them () =
  Alcotest.(check string)
    "the write went through the wrapper, so the journal holds it" "held\n"
    (crash_after_a_lock_published_its_contents
       ~policy:(Crash.Survive (fun _ -> true)))

(* ---- a backend that hides descriptors and declares nothing ----

   Between the two answers above sits the wiring error they exist to tell
   apart: a directory whose files carry no descriptor and whose handler says
   nothing about who else can open them. It is built here rather than borrowed
   from a wrapper this package ships, because a wrapper that hides descriptors
   today passes them through tomorrow, and the case has to stay the case.

   Every operation is the host directory's own, the native name included, so
   the lock file is a real host file; only the files handed back are re-boxed
   behind the read and write operations alone, which is what an adapter written
   without a thought for descriptors produces. *)

module Hidden_ro = struct
  type t = Eio.File.ro_ty Eio.Resource.t

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t buf
  let pread t ~file_offset bufs = Eio.File.pread t ~file_offset bufs
  let stat t = Eio.File.stat t
  let seek t off cmd = Eio.File.seek t off cmd
  let close t = Eio.Resource.close t
end

module Hidden_rw = struct
  type t = Eio.File.rw_ty Eio.Resource.t

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t buf
  let pread t ~file_offset bufs = Eio.File.pread t ~file_offset bufs
  let stat t = Eio.File.stat t
  let seek t off cmd = Eio.File.seek t off cmd
  let close t = Eio.Resource.close t
  let single_write t bufs = Eio.Flow.single_write t bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let pwrite t ~file_offset bufs = Eio.File.pwrite_single t ~file_offset bufs
  let sync t = Eio.File.sync t
  let truncate t len = Eio.File.truncate t len
end

let hidden_ro base : Eio.File.ro_ty Eio.Resource.t =
  Eio.Resource.T (base, Eio.File.Pi.ro (module Hidden_ro))

let hidden_rw base : Eio.File.rw_ty Eio.Resource.t =
  Eio.Resource.T (base, Eio.File.Pi.rw (module Hidden_rw))

module Hide_descriptors = struct
  type t = Eio.Fs.dir_ty Eio.Resource.t

  let at t path = (t, path)

  let open_in t ~sw ~follow path =
    hidden_ro (Eio.Path.open_in ~sw ~follow (at t path))

  let open_out t ~sw ~follow ~append ~create path =
    hidden_rw (Eio.Path.open_out ~sw ~follow ~append ~create (at t path))

  let stat t ~follow path = Eio.Path.stat ~follow (at t path)
  let mkdir t ~perm path = Eio.Path.mkdir ~perm (at t path)
  let read_dir t path = Eio.Path.read_dir (at t path)
  let with_dir_entries t path fn = Eio.Path.with_dir_entries (at t path) fn
  let unlink t path = Eio.Path.unlink (at t path)
  let rmdir t path = Eio.Path.rmdir (at t path)
  let rename t path dir dst = Eio.Path.rename (at t path) (dir, dst)
  let read_link t path = Eio.Path.read_link (at t path)
  let symlink ~link_to t path = Eio.Path.symlink ~link_to (at t path)
  let chmod t ~follow ~perm path = Eio.Path.chmod ~follow ~perm (at t path)

  let chown ~follow ?uid ?gid t path =
    Eio.Path.chown ~follow ?uid ?gid (at t path)

  let sync_dir t path = Eio.Path.sync_dir (at t path)
  let native t path = Eio.Path.native (at t path)
  let pp f _ = Format.pp_print_string f "hide-descriptors"

  let open_subtree _ ~sw:_ _ =
    failwith "Hide_descriptors: the lock opens no subtree"

  include Eio_utils.Posix_path
end

let hide_descriptors ((base, path) : Eio.Fs.dir_ty Eio.Path.t) :
    Eio.Fs.dir_ty Eio.Path.t =
  ( (Eio.Resource.T
       ( base,
         Eio.Resource.handler
           [ Eio.Resource.H (Eio.Fs.Pi.Dir, (module Hide_descriptors)) ] )
      : Eio.Fs.dir_ty Eio.Resource.t),
    path )

(* The lock file is a host file another process is holding the record lock on
   right here, so the mutex in this process excludes nobody, and a caller let
   into its critical section would be the second writer of a resource one
   process already holds. It is refused instead, and the refusal names both the
   interface that made it and the lock file it was asked for, so a wiring
   error reads as one. *)
let test_lock_refuses_a_wrapper_that_hides_a_host_descriptor () =
  with_real_dir_env @@ fun env dir ->
  Eio.Switch.run @@ fun sw ->
  let _, release =
    holding_process ~sw ~env ~kind:"record" ~lock:(dir / "lock")
  in
  let hidden = hide_descriptors dir in
  let ran = ref false in
  (match Eio_fs_unix.with_lock (hidden / "lock") (fun () -> ran := true) with
  | () ->
      Alcotest.fail
        "a lock file with no descriptor was locked while another process held \
         it"
  | exception Invalid_argument message ->
      Alcotest.(check bool)
        "the refusal names the interface that refused" true
        (mentions ~needle:"Eio_fs_unix.with_lock" message);
      Alcotest.(check bool)
        "the refusal names the lock file" true
        (mentions ~needle:(Eio.Path.native_exn (dir / "lock")) message));
  Alcotest.(check bool)
    "the critical section did not run over the held file" false !ran;
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* ---- the lock file as a section a backend-free component runs inside ----

   [lock_section] hands the lock over as an [Eio_fs.Sync.Lock.t], which is the
   argument a component takes when what exclusion its files admit is the
   deployment's to say. What it owes that component is the lock's exclusion
   unchanged, and a refusal in a vocabulary the component can name. *)
let test_section_excludes_fibers_over_the_lock_file () =
  with_real_dir @@ fun dir ->
  let section = Eio_fs_unix.lock_section ~block:`Fibers (dir / "lock") in
  let inside = ref 0 and most = ref 0 in
  let enter () =
    Eio_fs.Sync.Lock.use section (fun () ->
        incr inside;
        most := max !most !inside;
        Eio.Fiber.yield ();
        decr inside)
  in
  Eio.Fiber.both enter enter;
  Alcotest.(check int) "never two fibers in one section" 1 !most;
  (* One section value serves sections at every result type, the packed lock
     inheriting the caller's polymorphism rather than fixing it at the first
     use. *)
  Alcotest.(check string)
    "and answers at another result type" "answer"
    (Eio_fs.Sync.Lock.use section (fun () -> "answer"))

(* A hold another process took is what a component cannot see and cannot name.
   It reaches the component as [Unavailable], carrying the lock file and its
   holder, so an interface whose type promises a result has something to put in
   it. *)
let test_section_refusal_reaches_the_component_as_unavailable () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let child, release = holding_process ~sw ~env ~kind:"record" ~lock in
  let section = Eio_fs_unix.lock_section ~block:`Fibers lock in
  let ran = ref false in
  (match Eio_fs.Sync.Lock.use section (fun () -> ran := true) with
  | () ->
      Alcotest.fail "another process's hold was waited out rather than answered"
  | exception Eio_fs.Sync.Lock.Unavailable what ->
      Alcotest.(check bool)
        "the refusal names the lock file" true
        (mentions ~needle:(Eio.Path.native_exn lock) what);
      Alcotest.(check bool)
        "and names the process that holds it" true
        (names_process what (Eio.Process.pid child)));
  Alcotest.(check bool) "the section did not run" false !ran;
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

(* A name that is not the lock's own regular file is the other refusal, and it
   is not the ordinary busy one: a component told "held by another process"
   would wait and retry for ever over a planted directory. *)
let test_section_refuses_a_name_that_is_not_the_lock_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.mkdir ~perm:0o755 (dir / "lock");
  let section = Eio_fs_unix.lock_section (dir / "lock") in
  match Eio_fs.Sync.Lock.use section (fun () -> ()) with
  | () -> Alcotest.fail "a directory was locked as a lock file"
  | exception Eio_fs.Sync.Lock.Unavailable what ->
      Alcotest.(check bool)
        "the refusal names the lock file" true
        (mentions ~needle:(Eio.Path.native_exn (dir / "lock")) what);
      Alcotest.(check bool)
        "and says it is not the lock's own file" true
        (mentions ~needle:"not the lock's own regular file" what)

(* Only a refusal to enter is translated. [Invalid_argument] is in the set the
   acquisition raises, so a body raising it is the case that separates the two:
   it must travel out as the body raised it, or a component would report a
   failure of its own as the section being unavailable. *)
let test_section_lets_the_body_s_own_raise_out () =
  with_real_dir @@ fun dir ->
  let section = Eio_fs_unix.lock_section (dir / "lock") in
  (match
     Eio_fs.Sync.Lock.use section (fun () -> invalid_arg "the body's own")
   with
  | () -> Alcotest.fail "the section swallowed a raise"
  | exception Eio_fs.Sync.Lock.Unavailable what ->
      Alcotest.failf "the body's raise was reported as a refusal: %s" what
  | exception Invalid_argument m ->
      Alcotest.(check string) "raised as thrown" "the body's own" m);
  Alcotest.(check int)
    "and the section is still there" 7
    (Eio_fs.Sync.Lock.use section (fun () -> 7))

(* A lock file carrying no descriptor over a backend that does not declare
   itself process-local offers no exclusion between processes at all, and
   [with_lock] refuses it with [Invalid_argument]. That refusal is an
   acquisition's, so it reaches the component as [Unavailable] like any other:
   an exception the component cannot catch would leave the wiring error to
   surface as a crash somewhere past the interface that promised a result. *)
let test_section_refuses_a_wrapper_that_hides_a_host_descriptor () =
  with_real_dir @@ fun dir ->
  let hidden = hide_descriptors dir in
  let section = Eio_fs_unix.lock_section (hidden / "lock") in
  let ran = ref false in
  (match Eio_fs.Sync.Lock.use section (fun () -> ran := true) with
  | () -> Alcotest.fail "a lock file with no descriptor was locked"
  | exception Eio_fs.Sync.Lock.Unavailable what ->
      Alcotest.(check bool)
        "the refusal names the interface that refused" true
        (mentions ~needle:"Eio_fs_unix.with_lock" what));
  Alcotest.(check bool) "the section did not run" false !ran

let test_switch_lease_cancel () =
  let take ~on_wait lock fn =
    Eio.Switch.run @@ fun sw ->
    ignore (Eio_fs_unix.Lease.take ~sw ~on_wait lock);
    fn ()
  in
  a_wait_is_cancellable ~kind:"lease" take ~free:lease_free ()

let suite =
  ( "lock",
    [
      Alcotest.test_case "a switch lease acquisition is cancellable" `Quick
        test_switch_lease_cancel;
      Alcotest.test_case "a byte-range lock leaves the offset" `Quick
        test_lock_range;
      Alcotest.test_case "lock wait does not block Eio" `Quick
        test_lock_wait_does_not_block_eio;
      Alcotest.test_case "a record lock wait answers at the release" `Quick
        (a_wait_answers_at_the_release ~kind:"record" take_record);
      Alcotest.test_case "a record lock wait answers at a cancel" `Quick
        (a_wait_is_cancellable ~kind:"record" take_record ~free:record_free);
      Alcotest.test_case "a lease wait answers at the release" `Quick
        (a_wait_answers_at_the_release ~kind:"lease" take_lease);
      Alcotest.test_case "a cancelled lease wait ends its domain" `Quick
        test_cancelled_lease_wait_ends_its_domain;
      Alcotest.test_case "ended lease waits leave no descriptor" `Quick
        (ended_waits_leave_no_descriptor ~kind:"lease"
           ~take_for:(fun seconds lock fn ->
             Eio_fs_unix.Lease.with_exclusive ~block:(`For seconds) lock
               (fun _ -> fn ()))
           take_lease);
      Alcotest.test_case "ended record lock waits leave no descriptor" `Quick
        (ended_waits_leave_no_descriptor ~kind:"record"
           ~take_for:(fun seconds lock fn ->
             Eio_fs_unix.with_lock ~block:(`For seconds) lock fn)
           take_record);
      Alcotest.test_case "a lease wait answers at a cancel" `Quick
        (a_wait_is_cancellable ~kind:"lease" take_lease ~free:lease_free);
      Alcotest.test_case "a bounded record lock wait answers at the release"
        `Quick
        (a_bounded_wait_answers_at_the_release ~kind:"record" take_record_for);
      Alcotest.test_case "a bounded lease wait answers at the release" `Quick
        (a_wait_answers_at_the_release ~kind:"lease" take_lease_for);
      Alcotest.test_case "a lease wait for a fiber says so" `Quick
        test_lease_wait_for_a_fiber_says_so;
      Alcotest.test_case "lock serialises fibers" `Quick
        test_lock_serialises_fibers;
      Alcotest.test_case "lock excludes another name for its file" `Quick
        test_lock_excludes_another_name_for_the_locked_file;
      Alcotest.test_case "lock answers a parent link that leads nowhere" `Quick
        test_lock_answers_a_parent_link_that_leads_nowhere;
      Alcotest.test_case "lock contents stay on the locked inode" `Quick
        test_lock_updates_contents_in_place;
      Alcotest.test_case "non-blocking lock reports unavailable" `Quick
        test_lock_non_blocking_unavailable;
      Alcotest.test_case "a refusal names the holding process" `Quick
        test_lock_refusal_names_the_holding_process;
      Alcotest.test_case "a refusal names a fiber of this process" `Quick
        test_lock_refusal_names_this_process_for_a_fiber;
      Alcotest.test_case "a refusal in the holder keeps its hold" `Quick
        test_lock_refusal_in_the_holder_keeps_its_hold;
      Alcotest.test_case "a bounded wait in the holder keeps its hold" `Quick
        test_lock_bounded_wait_in_the_holder_keeps_its_hold;
      Alcotest.test_case "asks from the holder leave no descriptor" `Quick
        test_lock_asks_from_the_holder_leave_no_descriptor;
      Alcotest.test_case "asks during a hold keep one descriptor" `Quick
        test_lock_asks_during_a_hold_keep_one_descriptor;
      Alcotest.test_case "asks while the holder lets go answer" `Quick
        test_lock_asks_while_the_holder_lets_go;
      Alcotest.test_case "a probe names the holding process" `Quick
        test_probe_names_the_holding_process;
      Alcotest.test_case "probe through a wrapper names the holding process"
        `Quick test_probe_through_a_wrapper_names_the_holding_process;
      Alcotest.test_case "a probe from the holder keeps its hold" `Quick
        test_probe_from_the_holder_keeps_its_hold;
      Alcotest.test_case "a probe answers a read-only lock file" `Quick
        test_probe_answers_a_read_only_lock_file;
      Alcotest.test_case "a probe of a lock file that goes" `Quick
        test_probe_of_a_lock_file_that_goes;
      Alcotest.test_case "a probe of a lock file replaced" `Quick
        test_probe_of_a_lock_file_replaced;
      Alcotest.test_case "a hold lasts as long as its switch" `Quick
        test_hold_lasts_as_long_as_its_switch;
      Alcotest.test_case "a hold's refusal names the holding process" `Quick
        test_hold_refusal_names_the_holding_process;
      Alcotest.test_case "a held lock restates its record" `Quick
        test_held_restates_its_record;
      Alcotest.test_case "a held lock's release leaves a record" `Quick
        test_held_release_lets_the_lock_go_and_leaves_a_record;
      Alcotest.test_case "a held lock ends with its switch" `Quick
        test_held_ends_with_its_switch;
      Alcotest.test_case "a restate takes back a lock a stray close dropped"
        `Quick test_held_restate_takes_back_a_lock_a_stray_close_dropped;
      Alcotest.test_case "a release from its switch's hook leaves a record"
        `Quick test_held_released_from_its_switch_hook_leaves_a_record;
      Alcotest.test_case "a fibers-only lock queues behind a fiber" `Quick
        test_lock_fibers_mode_queues_behind_a_fiber;
      Alcotest.test_case "a fibers-only lock answers another process" `Quick
        test_lock_fibers_mode_refuses_another_process;
      Alcotest.test_case "a bounded wait answers at the bound" `Quick
        test_lock_bounded_wait_answers_at_the_bound;
      Alcotest.test_case "a bounded wait answers a fiber that holds it" `Quick
        test_lock_bounded_wait_answers_a_fiber_that_holds_it;
      Alcotest.test_case "a wait behind a fiber says so" `Quick
        test_lock_wait_behind_a_fiber_says_so;
      Alcotest.test_case "a bounded wait does not sit out a killed holder"
        `Quick test_lock_bounded_wait_does_not_sit_out_a_killed_holder;
      Alcotest.test_case "existing-only lock does not create" `Quick
        test_lock_existing_does_not_create;
      Alcotest.test_case "lock refuses a symlink at the lock path" `Quick
        test_lock_refuses_a_symlink_at_the_lock_path;
      Alcotest.test_case "lock refuses a dangling symlink" `Quick
        test_lock_refuses_a_dangling_symlink_without_creating_its_target;
      Alcotest.test_case
        "lock creates nothing through a link planted after the check" `Quick
        test_lock_creates_nothing_through_a_link_planted_after_the_check;
      Alcotest.test_case "lock refuses a directory or a FIFO" `Quick
        test_lock_refuses_a_directory_or_a_fifo;
      Alcotest.test_case "missing lock file is created as a regular file" `Quick
        test_lock_creates_a_missing_lock_file_as_a_regular_file;
      Alcotest.test_case "lock serialises fibers over a process-local backend"
        `Quick test_lock_over_a_process_local_backend_serialises_fibers;
      Alcotest.test_case "a process-local backend still refuses a symlink"
        `Quick test_lock_over_a_process_local_backend_refuses_a_symlink;
      Alcotest.test_case "an uncontended bounded lock needs no clock" `Quick
        test_lock_bounded_and_uncontended_needs_no_clock;
      Alcotest.test_case
        "lock serialises fibers over a wrapped process-local backend" `Quick
        test_lock_over_a_wrapped_process_local_backend_serialises_fibers;
      Alcotest.test_case "lock is granted in the order fibers entered" `Quick
        test_lock_is_granted_in_the_order_fibers_entered;
      Alcotest.test_case "lock through a crash wrapper takes the host lock"
        `Quick test_lock_through_a_crash_wrapper_takes_the_host_lock;
      Alcotest.test_case "an un-synced crash loses a lock's contents" `Quick
        test_lock_contents_are_lost_by_an_unsynced_crash;
      Alcotest.test_case "a lock's contents survive a policy that keeps them"
        `Quick test_lock_contents_survive_a_policy_that_keeps_them;
      Alcotest.test_case "lock refuses a wrapper that hides a host descriptor"
        `Quick test_lock_refuses_a_wrapper_that_hides_a_host_descriptor;
      Alcotest.test_case "a lock section excludes fibers over the lock file"
        `Quick test_section_excludes_fibers_over_the_lock_file;
      Alcotest.test_case "a lock section's refusal is Unavailable" `Quick
        test_section_refusal_reaches_the_component_as_unavailable;
      Alcotest.test_case
        "a lock section refuses a name that is not the lock file" `Quick
        test_section_refuses_a_name_that_is_not_the_lock_file;
      Alcotest.test_case "a lock section lets the body's own raise out" `Quick
        test_section_lets_the_body_s_own_raise_out;
      Alcotest.test_case
        "a lock section refuses a wrapper that hides a descriptor" `Quick
        test_section_refuses_a_wrapper_that_hides_a_host_descriptor;
    ] )
