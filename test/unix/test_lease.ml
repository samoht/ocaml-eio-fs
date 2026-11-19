(* Leases: BSD [flock] holds over a lock file, inherited by a child
   and released at the last close, taken for a switch or after readers. *)

open Holder

let ( / ) = Eio.Path.( / )

let test_lease_survives_owner_close_until_inherited_descriptor_closes () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  let wait_child pid =
    let rec loop () =
      match Unix.waitpid [] pid with
      | answer -> answer
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    in
    loop ()
  in
  let child =
    Eio_fs_unix.Lease.with_exclusive lock @@ fun lease ->
    let fd =
      Eio_unix.Fd.use_exn "test inherited lease"
        (Eio_fs_unix.Lease.fd lease)
        Fun.id
    in
    Unix.clear_close_on_exec fd;
    (* The child says it runs, holding the inherited descriptor, with one
       byte on [announced]. *)
    let announced, announce = Unix.pipe ~cloexec:true () in
    Fun.protect ~finally:(fun () -> Unix.close announced) @@ fun () ->
    let pid =
      Fun.protect
        ~finally:(fun () ->
          Unix.close announce;
          Unix.set_close_on_exec fd)
        (fun () ->
          Unix.create_process Sys.executable_name
            [| Sys.executable_name; "--hold-eio-fs-lease" |]
            Unix.stdin announce Unix.stderr)
    in
    Eio_unix.await_readable announced;
    let said = Bytes.create 1 in
    if Unix.read announced said 0 1 <> 1 || Bytes.get said 0 <> 'R' then begin
      (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
      ignore (wait_child pid);
      Alcotest.fail "lease child ended before it said it runs"
    end;
    pid
  in
  let reaped = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !reaped then begin
        (try Unix.kill child Sys.sigterm with Unix.Unix_error _ -> ());
        ignore (wait_child child)
      end)
    (fun () ->
      (match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
      | () -> Alcotest.fail "inherited descriptor did not retain the lease"
      | exception Eio_fs_unix.Lock_unavailable _ -> ());
      Unix.kill child Sys.sigterm;
      let _, status = wait_child child in
      reaped := true;
      (match status with
      | Unix.WSIGNALED n when Int.equal n Sys.sigterm -> ()
      | Unix.WEXITED n -> Alcotest.failf "lease child exited %d" n
      | Unix.WSIGNALED n -> Alcotest.failf "lease child took signal %d" n
      | Unix.WSTOPPED n -> Alcotest.failf "lease child stopped by %d" n);
      Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()))

(* An unlocked lease holds nothing through any descriptor that shares its
   open file description: a process that inherited it lives on, and the lease
   is free for the next taker all the same. *)
let test_lease_unlock_frees_inherited_descriptor () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  let wait_child pid =
    let rec loop () =
      match Unix.waitpid [] pid with
      | answer -> answer
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    in
    loop ()
  in
  let child =
    Eio_fs_unix.Lease.with_exclusive lock @@ fun lease ->
    let fd =
      Eio_unix.Fd.use_exn "test unlocked lease"
        (Eio_fs_unix.Lease.fd lease)
        Fun.id
    in
    Unix.clear_close_on_exec fd;
    let announced, announce = Unix.pipe ~cloexec:true () in
    Fun.protect ~finally:(fun () -> Unix.close announced) @@ fun () ->
    let pid =
      Fun.protect
        ~finally:(fun () ->
          Unix.close announce;
          Unix.set_close_on_exec fd)
        (fun () ->
          Unix.create_process Sys.executable_name
            [| Sys.executable_name; "--hold-eio-fs-lease" |]
            Unix.stdin announce Unix.stderr)
    in
    Eio_unix.await_readable announced;
    let said = Bytes.create 1 in
    if Unix.read announced said 0 1 <> 1 || Bytes.get said 0 <> 'R' then begin
      (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
      ignore (wait_child pid);
      Alcotest.fail "lease child ended before it said it runs"
    end;
    (try Eio_fs_unix.Lease.unlock lease
     with e ->
       (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
       ignore (wait_child pid);
       raise e);
    pid
  in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.kill child Sys.sigterm with Unix.Unix_error _ -> ());
      ignore (wait_child child))
    (fun () ->
      (match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
      | () -> ()
      | exception Eio_fs_unix.Lock_unavailable _ ->
          Alcotest.fail "an unlocked lease was still held by its inheritor");
      match Unix.waitpid [ Unix.WNOHANG ] child with
      | 0, _ -> ()
      | _ -> Alcotest.fail "lease child ended before the lease was taken")

(* The lease is a kernel flock, which dies with its holder: a process that
   exits holding it leaves no stale lock, and the next taker is granted it at
   once, with no takeover. *)
let test_lease_dies_with_its_holder () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lease" in
  let manager = Eio.Stdenv.process_mgr env in
  let self = Sys.executable_name in
  Alcotest.check status "the holder exited holding the lease" (`Exited 0)
    ( Eio.Switch.run @@ fun sw ->
      Eio.Process.await
        (Eio.Process.spawn ~sw manager ~executable:self
           [ self; "--exit-holding-eio-fs-lease"; Eio.Path.native_exn lock ]) );
  Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())

(* A lease a fiber of this process holds is refused to another fiber as held by
   this process, which a reader must not take for another process: the
   refusal says so, and names no pid that reads as a foreign holder. *)
let test_lease_refusal_names_a_fiber () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  Eio_fs_unix.Lease.with_exclusive ~contents:"preparing lib\n" lock @@ fun _ ->
  match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
  | () -> Alcotest.fail "a lease a fiber holds was granted"
  | exception Eio_fs_unix.Lock_unavailable refusal ->
      let said = Fmt.str "%a" Eio_fs_unix.pp_refusal refusal in
      let has sub =
        let n = String.length sub and m = String.length said in
        let rec at i =
          i + n <= m && (String.sub said i n = sub || at (i + 1))
        in
        at 0
      in
      Alcotest.(check bool)
        ("another fiber of this process: " ^ said)
        true
        (has "held by another fiber of this process");
      Alcotest.(check bool)
        ("what the holder is doing: " ^ said)
        true (has "preparing lib");
      Alcotest.(check bool)
        ("no pid as a foreign holder: " ^ said)
        false
        (Fmt.kstr has "process %d" (Unix.getpid ()))

let test_switch_lease_release () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  Eio.Switch.run @@ fun sw ->
  let lease = Eio_fs_unix.Lease.take ~sw lock in
  (match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
  | () -> Alcotest.fail "a contender acquired the switch's lease"
  | exception Eio_fs_unix.Lock_unavailable _ -> ());
  Eio_fs_unix.Lease.release lease;
  Eio_fs_unix.Lease.release lease;
  Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())

let test_switch_lease_exception () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  let exception End in
  (match
     Eio.Switch.run (fun sw ->
         ignore (Eio_fs_unix.Lease.take ~sw lock);
         raise End)
   with
  | () -> Alcotest.fail "the holder exception disappeared"
  | exception End -> ());
  Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())

(* [Lease.take ~block:`Readers], held for the call of [fn]. *)
let after_readers ?on_wait ~gate lock fn =
  Eio.Switch.run @@ fun sw ->
  let lease = Eio_fs_unix.Lease.take ~sw ~gate ~block:`Readers ?on_wait lock in
  Fun.protect
    ~finally:(fun () -> Eio_fs_unix.Lease.release lease)
    (fun () -> fn lease)

(* A reader's look at a lease, a shared lease taken and let go at once, is
   waited for by a take after readers, which takes the lease once the look
   lets go; an exclusive holder refuses it. A take that read the look as a
   holder, as [`No] does, refuses a lease nobody keeps. *)
let test_lease_waits_out_readers () =
  with_real_dir @@ fun dir ->
  let lock = dir / "readers" and gate = dir / "readers.gate" in
  let looking, looked = Eio.Promise.create () in
  let waiting, waited = Eio.Promise.create () in
  let entered = ref false in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.Lease.with_shared lock (fun _ ->
          Eio.Promise.resolve looked ();
          Eio.Promise.await waiting))
    (fun () ->
      Eio.Promise.await looking;
      after_readers ~gate
        ~on_wait:(fun () -> Eio.Promise.resolve waited ())
        lock
        (fun _ -> entered := true));
  Alcotest.(check bool) "taken once the look let go" true !entered;
  Alcotest.(check bool)
    "it said it waits" true
    (Option.is_some (Eio.Promise.peek waiting));
  Eio_fs_unix.Lease.with_exclusive lock (fun _ ->
      match after_readers ~gate lock ignore with
      | () -> Alcotest.fail "an exclusive holder did not refuse the take"
      | exception Eio_fs_unix.Lock_unavailable _ -> ());
  match Eio_fs_unix.Lease.with_exclusive ~block:`Readers lock ignore with
  | () -> Alcotest.fail "a wait for readers was taken with no gate"
  | exception Invalid_argument _ -> ()

(* A take that waits for readers waits for nothing else, whichever instant
   another take comes in: a reader holds the lease shared, so a take after
   readers is refused at once, finds only the reader, and says it waits
   (on_wait); there the reader lets go and another process takes the lease
   exclusively through the same gate, before this take asks the kernel
   again, the interleaving forced by reading that process's answer with the
   domain blocked, so no fiber runs in between. That process is refused, and
   this take is granted as the reader let go, while it still runs. A take
   that read the holders once, before its blocking take, let that process in
   and waited behind it for its whole hold. *)
let test_lease_waits_for_readers_alone () =
  with_real_dir_env @@ fun _env dir ->
  let lock = dir / "readers" and gate = dir / "readers.gate" in
  Eio.Path.save ~create:(`Or_truncate 0o600) lock "";
  let to_child, child_in = Unix.pipe ~cloexec:true () in
  let child_out, from_child = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process Sys.executable_name
      [|
        Sys.executable_name;
        "--take-gated-eio-fs-lease";
        Eio.Path.native_exn gate;
        Eio.Path.native_exn lock;
      |]
      to_child from_child Unix.stderr
  in
  Unix.close to_child;
  Unix.close from_child;
  let ended = ref false in
  let ends () =
    if not !ended then begin
      ended := true;
      Unix.close child_in;
      let rec reap () =
        match Unix.waitpid [] pid with
        | _ -> ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
      in
      reap ()
    end
  in
  Fun.protect ~finally:(fun () ->
      ends ();
      Unix.close child_out)
  @@ fun () ->
  (* The child exits once it answers, and its SIGCHLD may interrupt the
     read. *)
  let rec no_eintr f =
    try f () with Unix.Unix_error (Unix.EINTR, _, _) -> no_eintr f
  in
  let take () =
    ignore
      (no_eintr (fun () -> Unix.write_substring child_in "take\n" 0 5) : int);
    let b = Bytes.create 16 in
    let n = no_eintr (fun () -> Unix.read child_out b 0 16) in
    String.trim (Bytes.sub_string b 0 n)
  in
  Eio.Switch.run @@ fun sw ->
  let reader, set_reader = Eio.Promise.create () in
  let read, set_read = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio_fs_unix.Lease.with_shared ~block:`No ~create:`Never lock (fun lease ->
          Eio.Promise.resolve set_reader lease;
          Eio.Promise.await read));
  let reader = Eio.Promise.await reader in
  let answer, set_answer = Eio.Promise.create () in
  let on_wait () =
    if not (Eio.Promise.is_resolved answer) then begin
      Eio_fs_unix.Lease.unlock reader;
      Eio.Promise.resolve set_answer (take ())
    end
  in
  let took =
    Eio.Fiber.fork_promise ~sw (fun () ->
        after_readers ~gate ~on_wait lock ignore)
  in
  let answer = Eio.Promise.await answer in
  Eio.Promise.resolve set_read ();
  Alcotest.(check string) "the other process is refused" "refused" answer;
  match Eio.Promise.await took with
  | Ok () -> ends ()
  | Error e -> Alcotest.failf "the take was not granted: %a" Fmt.exn e

(* A remover of a lease removes its gate holding it, and a take that waited
   on that gate takes the file its name holds next, which the name then
   shows held: a take that kept the removed file excludes no other. *)
let test_lease_gate_removed_under_a_waiter () =
  with_real_dir_env @@ fun _env dir ->
  let lock = dir / "removed" and gate = dir / "removed.gate" in
  let at_gate = ref `Not_taken in
  ( Eio.Switch.run @@ fun sw ->
    Eio_fs_unix.Lease.with_exclusive ~gate lock @@ fun _ ->
    Eio.Fiber.fork ~sw (fun () ->
        Eio_fs_unix.Lease.with_exclusive ~gate ~block:`Yes lock (fun _ ->
            at_gate :=
              if not (Eio.Path.is_file gate) then `Gone
              else
                match
                  Eio_fs_unix.Lease.with_exclusive ~block:`No gate ignore
                with
                | () -> `Free
                | exception Eio_fs_unix.Lock_unavailable _ -> `Held));
    Eio.Path.unlink gate;
    Eio.Path.unlink lock );
  match !at_gate with
  | `Held -> ()
  | `Free -> Alcotest.fail "the waiter holds no lock on the gate at its name"
  | `Gone -> Alcotest.fail "the waiter holds a gate no name holds"
  | `Not_taken -> Alcotest.fail "the waiter never took the lease"

(* Another process, this executable run with [args], with a pipe on its
   standard input and one on its standard output. *)
let spawned args =
  let exe = Sys.executable_name in
  let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process exe
      (Array.of_list (exe :: args))
      stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  (pid, stdin_w, stdout_r)

(* One byte read from [fd], waiting as long as it takes, past signals. *)
let rec read_byte fd =
  let b = Bytes.create 1 in
  match Unix.read fd b 0 1 with
  | 1 -> Some (Bytes.get b 0)
  | _ -> None
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_byte fd

let ended = function
  | Unix.WEXITED n -> `Exited n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> `Signaled n

(* A wait for a lease's holders to let go holds nothing whenever its caller's
   domain runs: the waiting domain is held in a blocking read, the holder lets
   go, and another process's exclusive take of the lease is granted, which
   ends that read. The wait returns only once the holder has let go, and holds
   nothing once it has returned: this process's own exclusive take is granted
   without waiting. A wait that kept a shared grant until its fiber ran would
   hold the take off for as long as the domain does not run, as a reader's
   busy domain did to a check's take of its lease. *)
let test_await_free_holds_nothing () =
  with_real_dir_env @@ fun _env dir ->
  let lock = dir / "lock" in
  let path = Eio.Path.native_exn lock in
  let holder, release, said =
    spawned [ "--hold-until-release"; "lease"; path ]
  in
  Alcotest.(check (option char))
    "the holder holds the lease" (Some 'R') (read_byte said);
  let resume_r, resume_w = Unix.pipe ~cloexec:true () in
  let let_go = Atomic.make false in
  let taken = ref None in
  let waited = ref None in
  let waiting, now_waiting = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () ->
      Eio_fs_unix.Lease.await_free
        ~on_wait:(fun () -> Eio.Promise.resolve now_waiting ())
        lock;
      waited := Some (Atomic.get let_go))
    (fun () ->
      Eio.Promise.await waiting;
      let take =
        Thread.create
          (fun () ->
            Atomic.set let_go true;
            ignore (Unix.write_substring release "X" 0 1 : int);
            ignore (reap holder : int * Unix.process_status);
            let taker, input, output =
              spawned [ "--take-eio-fs-lease"; path ]
            in
            let status = ended (snd (reap taker)) in
            Unix.close input;
            Unix.close output;
            taken := Some status;
            ignore (Unix.write_substring resume_w "T" 0 1 : int))
          ()
      in
      Alcotest.(check (option char))
        "the domain resumes once the take is granted" (Some 'T')
        (read_byte resume_r);
      Thread.join take);
  List.iter Unix.close [ release; said; resume_r; resume_w ];
  Alcotest.(check (option status))
    "another process takes the lease while the waiter's domain does not run"
    (Some (`Exited 0))
    !taken;
  Alcotest.(check (option bool))
    "the wait returns once the holder has let go" (Some true) !waited;
  Alcotest.(check bool)
    "the wait holds nothing once it returns" false
    (refused (fun () -> Eio_fs_unix.Lease.with_exclusive ~block:`No lock ignore))

(* {1 Signatures}

   [with_shared] is the reader operation, on the same lock file as
   [with_exclusive]; [Lease.t], [fd] and [unlock] serve both modes. A take after
   readers needs its gate: without one it does not type. *)

let _ :
    ?block:[ `Yes | `No | `Fibers | `For of float ] ->
    ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    (Eio_fs_unix.Lease.t -> 'a) ->
    'a =
  Eio_fs_unix.Lease.with_shared

let _ : Eio_fs_unix.Lease.t -> Eio_unix.Fd.t = Eio_fs_unix.Lease.fd
let _ : Eio_fs_unix.Lease.t -> unit = Eio_fs_unix.Lease.unlock

let _ :
    sw:Eio.Switch.t -> gate:_ Eio.Path.t -> _ Eio.Path.t -> Eio_fs_unix.Lease.t
    =
 fun ~sw ~gate path -> Eio_fs_unix.Lease.take ~sw ~gate ~block:`Readers path

(* A lease may belong to a switch while its caller goes on; [release] closes
   the owner's descriptor, which an inherited one outlives. *)
let _ :
    sw:Eio.Switch.t ->
    ?gate:_ Eio.Path.t ->
    ?block:[ Eio_fs_unix.block | `Readers ] ->
    ?create:Eio_fs_unix.create ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    Eio_fs_unix.Lease.t =
  Eio_fs_unix.Lease.take

let _ : Eio_fs_unix.Lease.t -> unit = Eio_fs_unix.Lease.release

(* {1 Shared leases} *)

let require what b = Alcotest.(check bool) what true b

(* Readers overlap, in one process too, and exclude an exclusive take and a
   POSIX record lock, at the path and through a hard link. An unlocked
   reader's callback close does not drop a later POSIX holder of the inode. A
   cancelled reader never enters its callback, and a reader refuses a link, a
   directory and a FIFO with [Not_a_lock_file]. *)
let test_shared_lease () =
  with_real_dir @@ fun dir ->
  let lock = dir / "lease" in
  let exclusive () =
    Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())
  in
  Eio_fs_unix.with_lock lock (fun () -> ());
  let alias = dir / "alias" in
  Unix.link (Eio.Path.native_exn lock) (Eio.Path.native_exn alias);
  Eio_fs_unix.with_lock lock (fun () ->
      List.iter
        (fun path ->
          require
            "shared refusal respects an existing POSIX holder and inode alias"
            (refused (fun () ->
                 Eio_fs_unix.Lease.with_shared ~block:`No path (fun _ -> ())));
          probe "--try-eio-fs-lock" lock 3)
        [ lock; alias ]);
  probe "--try-eio-fs-lock" lock 0;
  List.iter
    (fun reader_path ->
      let unlocked, announce_unlock = Eio.Promise.create () in
      let writer_started, announce_writer = Eio.Promise.create () in
      let reader_closed, announce_close = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () ->
          Eio_fs_unix.Lease.with_shared reader_path (fun lease ->
              Eio_fs_unix.Lease.unlock lease;
              Eio.Promise.resolve announce_unlock ();
              Eio.Promise.await writer_started);
          Eio.Promise.resolve announce_close ())
        (fun () ->
          Eio.Promise.await unlocked;
          Eio_fs_unix.with_lock ~block:`No lock (fun () ->
              Eio.Promise.resolve announce_writer ();
              Eio.Promise.await reader_closed;
              probe "--try-eio-fs-lock" lock 3)))
    [ lock; alias ];
  let entered = ref false in
  Eio_fs_unix.Lease.with_shared ~block:`No lock (fun first ->
      require "writer excludes a single reader" (refused exclusive);
      List.iter
        (fun path ->
          require
            "canonical POSIX writer refuses live shared reader and inode alias"
            (refused (fun () ->
                 Eio_fs_unix.with_lock ~block:`No path (fun () -> ()))))
        [ lock; alias ];
      Eio_fs_unix.Lease.with_shared ~block:`No lock (fun second ->
          entered := true;
          require "writer excludes simultaneous independent readers"
            (refused exclusive);
          Eio_fs_unix.Lease.unlock second;
          require "unlock of one description preserves another reader"
            (refused exclusive));
      require "readers actually overlapped" !entered;
      Eio_fs_unix.Lease.unlock first;
      exclusive ());
  exclusive ();
  let inherited =
    Eio_fs_unix.Lease.with_shared lock (fun lease ->
        Eio_unix.Fd.use_exn "shared lease callback-close spec"
          (Eio_fs_unix.Lease.fd lease)
          (Unix.dup ~cloexec:true))
  in
  Fun.protect
    ~finally:(fun () -> Unix.close inherited)
    (fun () ->
      require "callback close retains duplicated reader description"
        (refused exclusive);
      probe "--try-eio-fs-with-lock" lock 3);
  exclusive ();
  let unlocked =
    Eio_fs_unix.Lease.with_shared lock (fun lease ->
        let copy =
          Eio_unix.Fd.use_exn "shared unlock duplicate spec"
            (Eio_fs_unix.Lease.fd lease)
            (Unix.dup ~cloexec:true)
        in
        Eio_fs_unix.Lease.unlock lease;
        exclusive ();
        copy)
  in
  Fun.protect ~finally:(fun () -> Unix.close unlocked) exclusive;
  Eio_fs_unix.Lease.with_exclusive lock (fun _ ->
      require "a reader refuses an exclusive writer"
        (refused (fun () ->
             Eio_fs_unix.Lease.with_shared ~block:`No lock (fun _ -> ())));
      let waited, wake = Eio.Promise.create () in
      let called = ref false in
      Eio.Fiber.first
        (fun () ->
          Eio_fs_unix.Lease.with_shared ~block:`Yes
            ~on_wait:(fun () -> Eio.Promise.resolve wake ())
            lock
            (fun _ -> called := true))
        (fun () -> Eio.Promise.await waited);
      require "cancelled reader never enters callback" (not !called));
  Eio_fs_unix.with_lock lock (fun () ->
      let waited, wake = Eio.Promise.create () in
      let entered = ref false in
      Eio.Fiber.first
        (fun () ->
          Eio_fs_unix.Lease.with_shared ~block:`Fibers
            ~on_wait:(fun () -> Eio.Promise.resolve wake ())
            alias
            (fun _ -> entered := true))
        (fun () -> Eio.Promise.await waited);
      require "Fibers reader waits for canonical POSIX holder without entering"
        (not !entered);
      probe "--try-eio-fs-lock" lock 3);
  Eio_fs_unix.Lease.with_shared ~block:(`For 0.5) lock (fun _ -> ());
  exclusive ();
  require "Never refuses an absent reader lock file"
    (match
       Eio_fs_unix.Lease.with_shared ~create:`Never (dir / "absent") (fun _ ->
           ())
     with
    | () -> false
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true);
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "target") "unchanged";
  Eio.Path.symlink ~link_to:"target" (dir / "link");
  Eio.Path.mkdir ~perm:0o700 (dir / "directory");
  Unix.mkfifo (Eio.Path.native_exn (dir / "fifo")) 0o600;
  List.iter
    (fun name ->
      require
        ("shared reader refuses nonregular lock file: " ^ name)
        (match
           Eio_fs_unix.Lease.with_shared ~block:`No (dir / name) (fun _ -> ())
         with
        | () -> false
        | exception
            Eio.Io
              ( Eio.Fs.E
                  (Eio.Fs.Permission_denied (Eio_fs_unix.Not_a_lock_file _)),
                _ ) ->
            true))
    [ "link"; "directory"; "fifo" ];
  Alcotest.(check string)
    "reader refusal leaves symlink target unchanged" "unchanged"
    (Eio.Path.load (dir / "target"))

let lease_test_case =
  Alcotest.test_case "a lease follows its inherited descriptor" `Quick
    test_lease_survives_owner_close_until_inherited_descriptor_closes

let lease_unlock_test_case =
  Alcotest.test_case "an unlocked lease frees its inheritor" `Quick
    test_lease_unlock_frees_inherited_descriptor

let lease_readers_test_case =
  Alcotest.test_case "a lease taker waits out a reader's look" `Quick
    test_lease_waits_out_readers

let lease_readers_alone_test_case =
  Alcotest.test_case "a lease taker waits for readers alone" `Quick
    test_lease_waits_for_readers_alone

let lease_gate_removed_test_case =
  Alcotest.test_case "a removed gate is taken again by its waiter" `Quick
    test_lease_gate_removed_under_a_waiter

let lease_suite =
  ( "lease",
    [
      lease_test_case;
      lease_unlock_test_case;
      lease_readers_test_case;
      lease_readers_alone_test_case;
      lease_gate_removed_test_case;
    ] )

let test_shared_lease_outlives_its_owner () =
  let self = Sys.executable_name in
  let pid =
    Unix.create_process self [| self; owner_arg |] Unix.stdin Unix.stdout
      Unix.stderr
  in
  let _, status = reap pid in
  require "the shared lease outlived its owner" (exited status 0)

(* A reader's look tried without waiting, as at a ledger's slot, and refused by
   another process's lease, leaves no descriptor behind: a process that looks
   at every move of a ledger would otherwise reach the descriptor limit. *)
let test_refused_looks_leave_no_descriptor () =
  with_real_dir_env @@ fun env dir ->
  let lock = dir / "lock" in
  Eio.Switch.run @@ fun sw ->
  let _, release = holding_process ~sw ~env ~kind:"lease" ~lock in
  let descriptors () = Array.length (Sys.readdir "/dev/fd") in
  let look () =
    match Eio_fs_unix.Lease.with_shared ~block:`No lock ignore with
    | () -> Alcotest.fail "a lease another process holds was shared"
    | exception Eio_fs_unix.Lock_unavailable _ -> ()
  in
  look ();
  let before = descriptors () in
  for _ = 1 to 10 do
    look ()
  done;
  Alcotest.(check int)
    "descriptors open after the refused looks" before (descriptors ());
  Alcotest.check status "the holder let go when told" (`Exited 0) (release ())

let suite =
  ( "lease",
    [
      Alcotest.test_case "a switch lease releases once" `Quick
        test_switch_lease_release;
      Alcotest.test_case "a switch lease ends on exception" `Quick
        test_switch_lease_exception;
      Alcotest.test_case "a lease dies with its holder" `Quick
        test_lease_dies_with_its_holder;
      Alcotest.test_case "a lease refusal names a fiber" `Quick
        test_lease_refusal_names_a_fiber;
      lease_test_case;
      lease_unlock_test_case;
      lease_readers_test_case;
      lease_readers_alone_test_case;
      lease_gate_removed_test_case;
      Alcotest.test_case "a wait for a lease's release holds nothing" `Quick
        test_await_free_holds_nothing;
      Alcotest.test_case "shared leases" `Quick test_shared_lease;
      Alcotest.test_case "refused reader looks leave no descriptor" `Quick
        test_refused_looks_leave_no_descriptor;
      Alcotest.test_case "a shared lease outlives its owner" `Quick
        test_shared_lease_outlives_its_owner;
    ] )
