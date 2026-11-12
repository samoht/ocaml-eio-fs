(* What the cases of this executable share: a fresh directory of the real
   file system, and another process holding a lock in it. *)

let with_real_dir_env fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-dir" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () -> fn env (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t)

(* A process holding [lock] by [kind] ([test.ml], [hold_until_release]), and
   [release], which tells it to let go and returns how it ended: [`Exited 0]
   when it let go at [release], [`Exited 3] when it let go at its own
   five-second bound. *)
let holding_process ~sw ~env ~clock ~kind ~lock =
  let manager = Eio.Stdenv.process_mgr env in
  let input, release = Eio.Process.pipe ~sw manager in
  let ready, output = Eio.Process.pipe ~sw manager in
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw manager ~stdin:input ~stdout:output ~executable:self
      [ self; "--hold-until-release"; kind; Eio.Path.native_exn lock ]
  in
  Eio.Resource.close input;
  Eio.Resource.close output;
  let announced = Cstruct.create 1 in
  Eio.Time.with_timeout_exn clock 5.0 (fun () ->
      Eio.Flow.read_exact ready announced);
  Alcotest.(check char)
    "the other process holds the lock" 'R'
    (Cstruct.get_char announced 0);
  ( child,
    fun () ->
      (try Eio.Flow.copy_string "X" release with Eio.Io _ -> ());
      Eio.Resource.close release;
      Eio.Time.with_timeout_exn clock 5.0 (fun () -> Eio.Process.await child) )

let status =
  Alcotest.testable
    (fun ppf -> function
      | `Exited n -> Fmt.pf ppf "exited %d" n
      | `Signaled n -> Fmt.pf ppf "signalled %d" n)
    ( = )
