# Shared lease runtime contract

Each block executes its assertions when the public shared operation exists.
The first case checks independent open file descriptions in one process;
the second uses real fork inheritance and owner `_exit`, with pipe barriers.
There is no fixture implementation of locking. BSD `flock(2)` shared locks
exclude exclusive locks; close releases an inherited open-file-description
lock only at the final descriptor, while `LOCK_UN` releases it explicitly.

```ocaml
# let () =
    Eio_main.run @@ fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. @@ fun () ->
    let fs = Eio.Stdenv.fs env in
    let rendezvous_name = Filename.temp_file "shared-lease-spec" "" in
    Sys.remove rendezvous_name;
    let dir = Eio.Path.(fs / rendezvous_name) in
    Eio.Path.mkdir ~perm:0o700 dir;
    Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir) @@ fun () ->
    let lock = Eio.Path.(dir / "lease") in
    let require what b = if not b then failwith what in
    let refused f = match f () with
      | () -> false
      | exception Eio_fs_unix.Lock_unavailable _ -> true in
    let exclusive () = Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) in
    let probe mode path expected =
      let pid = Unix.create_process "../unix/test.exe"
        [| "../unix/test.exe"; mode; Eio.Path.native_exn path |]
        Unix.stdin Unix.stdout Unix.stderr in
      let _, status = Unix.waitpid [] pid in
      require ("native cross-API probe " ^ mode) (status = Unix.WEXITED expected) in
    Eio_fs_unix.with_lock lock (fun () -> ());
    let alias = Eio.Path.(dir / "alias") in
    Unix.link (Eio.Path.native_exn lock) (Eio.Path.native_exn alias);
    Eio_fs_unix.with_lock lock (fun () ->
      List.iter (fun path ->
        require "shared refusal respects an existing POSIX holder and inode alias"
          (refused (fun () -> Eio_fs_unix.Lease.with_shared ~block:`No path (fun _ -> ())));
        probe "--try-eio-fs-lock" lock 3) [ lock; alias ]);
    probe "--try-eio-fs-lock" lock 0;
    let entered = ref false in
    Eio_fs_unix.Lease.with_shared ~block:`No lock (fun first ->
      require "writer excludes a single reader" (refused exclusive);
      List.iter (fun path ->
        require "canonical POSIX writer refuses live shared reader and inode alias"
          (refused (fun () -> Eio_fs_unix.with_lock ~block:`No path (fun () -> ())))) [ lock; alias ];
      Eio_fs_unix.Lease.with_shared ~block:`No lock (fun second ->
        entered := true;
        require "writer excludes simultaneous independent readers" (refused exclusive);
        Eio_fs_unix.Lease.unlock second;
        require "unlock of one description preserves another reader" (refused exclusive));
      require "readers actually overlapped" !entered;
      Eio_fs_unix.Lease.unlock first;
      exclusive ());
    exclusive ();
    let inherited = Eio_fs_unix.Lease.with_shared lock (fun lease ->
      Eio_unix.Fd.use_exn "shared lease callback-close spec"
        (Eio_fs_unix.Lease.fd lease) (Unix.dup ~cloexec:true)) in
    Fun.protect ~finally:(fun () -> Unix.close inherited) (fun () ->
      require "callback close retains duplicated reader description" (refused exclusive);
      probe "--try-eio-fs-with-lock" lock 3);
    exclusive ();
    let unlocked = Eio_fs_unix.Lease.with_shared lock (fun lease ->
      let copy = Eio_unix.Fd.use_exn "shared unlock duplicate spec"
        (Eio_fs_unix.Lease.fd lease) (Unix.dup ~cloexec:true) in
      Eio_fs_unix.Lease.unlock lease;
      exclusive ();
      copy) in
    Fun.protect ~finally:(fun () -> Unix.close unlocked) exclusive;
    Eio_fs_unix.Lease.with_exclusive lock (fun _ ->
      require "a reader refuses an exclusive writer"
        (refused (fun () -> Eio_fs_unix.Lease.with_shared ~block:`No lock (fun _ -> ())));
      let waited, wake = Eio.Promise.create () in
      let called = ref false in
      Eio.Fiber.first
        (fun () -> Eio_fs_unix.Lease.with_shared ~block:`Yes
          ~on_wait:(fun () -> Eio.Promise.resolve wake ()) lock
          (fun _ -> called := true))
        (fun () -> Eio.Promise.await waited);
      require "cancelled reader never enters callback" (not !called));
    Eio_fs_unix.with_lock lock (fun () ->
      let waited, wake = Eio.Promise.create () in
      let entered = ref false in
      Eio.Fiber.first
        (fun () -> Eio_fs_unix.Lease.with_shared ~block:`Fibers
          ~on_wait:(fun () -> Eio.Promise.resolve wake ()) alias (fun _ -> entered := true))
        (fun () -> Eio.Promise.await waited);
      require "Fibers reader waits for canonical POSIX holder without entering" (not !entered);
      probe "--try-eio-fs-lock" lock 3);
    Eio_fs_unix.Lease.with_shared ~block:(`For 0.5) lock (fun _ -> ());
    exclusive ();
    require "Never refuses an absent reader rendezvous"
      (match Eio_fs_unix.Lease.with_shared ~create:`Never Eio.Path.(dir / "absent") (fun _ -> ()) with
       | () -> false
       | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true);
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(dir / "target") "unchanged";
    Eio.Path.symlink ~link_to:"target" Eio.Path.(dir / "link");
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(dir / "directory");
    Unix.mkfifo (Eio.Path.native_exn Eio.Path.(dir / "fifo")) 0o600;
    List.iter (fun name ->
      require ("shared reader refuses nonregular rendezvous: " ^ name)
        (match Eio_fs_unix.Lease.with_shared ~block:`No Eio.Path.(dir / name) (fun _ -> ()) with
         | () -> false
         | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied (Eio_fs_unix.Not_a_lock_file _)), _) -> true))
      [ "link"; "directory"; "fifo" ];
    require "reader refusal leaves symlink target unchanged"
      (Eio.Path.load Eio.Path.(dir / "target") = "unchanged");;
- : unit = ()
```

The owner takes a real shared lease, forks a reader which inherits its
open file description, announces the reader and exits inside the callback.
The observer reaps that owner before trying an exclusive hold. The reader
waits at a pipe until explicitly released; it then closes the lease before
reporting completion. Both child waits have a two-second bound.

```ocaml
# let () =
    let rendezvous_name = Filename.temp_file "shared-owner-death-spec" "" in
    let announced, announce = Unix.pipe ~cloexec:true () in
    let gate, release = Unix.pipe ~cloexec:true () in
    let finished, finish = Unix.pipe ~cloexec:true () in
    let byte fd expected =
      if Unix.select [ fd ] [] [] 2. = ([], [], []) then failwith "shared inherited pipe bound";
      let b = Bytes.create 1 in
      if Unix.read fd b 0 1 <> 1 || Bytes.get b 0 <> expected then failwith "shared inherited pipe evidence" in
    let owner = Unix.fork () in
    if owner = 0 then begin
      Unix.close announced; Unix.close release; Unix.close finished;
      Eio_main.run (fun env ->
        Eio_fs_unix.Lease.with_shared ~block:`No Eio.Path.(Eio.Stdenv.fs env / rendezvous_name) (fun lease ->
          let raw = Eio_unix.Fd.use_exn "shared owner death spec" (Eio_fs_unix.Lease.fd lease) Fun.id in
          let child = Unix.fork () in
          if child = 0 then begin
            Unix.close announce;
            (try byte gate 'X'; Unix.close raw; ignore (Unix.write_substring finish "D" 0 1); Unix.close finish; Unix._exit 0
             with _ -> Unix._exit 3)
          end;
          Unix.close gate; Unix.close finish;
          ignore (Unix.write_substring announce "R" 0 1);
          Unix.close announce;
          Unix._exit 0));
      Unix._exit 4
    end;
    Unix.close announce; Unix.close gate; Unix.close finish;
    let released = ref false and reaped = ref false in
    Fun.protect ~finally:(fun () ->
      if not !released then (try ignore (Unix.write_substring release "X" 0 1) with Unix.Unix_error _ -> ());
      Unix.close release; Unix.close announced; Unix.close finished;
      if not !reaped then ignore (Unix.waitpid [] owner);
      Sys.remove rendezvous_name) @@ fun () ->
    byte announced 'R';
    let _, status = Unix.waitpid [] owner in reaped := true;
    if status <> Unix.WEXITED 0 then failwith "shared lease owner did not exit inside callback";
    Eio_main.run (fun env ->
      let lock = Eio.Path.(Eio.Stdenv.fs env / rendezvous_name) in
      (match Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()) with
       | () -> failwith "owner death released the inherited shared reader"
       | exception Eio_fs_unix.Lock_unavailable _ -> ());
      let probe = Unix.create_process "../unix/test.exe"
        [| "../unix/test.exe"; "--try-eio-fs-with-lock"; rendezvous_name |]
        Unix.stdin Unix.stdout Unix.stderr in
      let _, status = Unix.waitpid [] probe in
      if status <> Unix.WEXITED 3 then failwith "canonical POSIX writer bypasses inherited shared reader";
      ignore (Unix.write_substring release "X" 0 1); released := true;
      byte finished 'D';
      let eof = Bytes.create 1 in
      if Unix.select [ finished ] [] [] 2. = ([], [], []) || Unix.read finished eof 0 1 <> 0
      then failwith "inherited reader did not end after final descriptor close";
      Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ()));
    ;;
- : unit = ()
```
