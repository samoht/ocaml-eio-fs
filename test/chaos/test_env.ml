(* A workload's env mirrors Eio_main's: [fs] reaches the whole tree, [cwd] is
   confined to its directory, as eio_posix confines it. *)

module P = Eio.Path

(* An absolute link, and a directory it names. *)
let setup env =
  P.mkdir ~perm:0o755 P.(env#fs / "x");
  P.symlink ~link_to:"/x" P.(env#fs / "abs")

(* Through [cwd], an absolute link, an absolute path and a [..] above it are
   refused; through [fs] they resolve. *)
let confined env =
  let refused p =
    match P.stat ~follow:true p with
    | _ -> Fmt.failwith "%a resolved" P.pp p
    | exception Eio.Io _ -> ()
  in
  refused P.(env#cwd / "abs");
  refused P.(env#cwd / "/x");
  refused P.(env#cwd / "..")

let check () = Eio_fs.Chaos.check ~setup (fun env -> confined env)

let crash () =
  Eio_fs.Chaos.Crash.check ~setup
    (fun ~ack env ->
      match Eio_fs_sync.save P.(env#cwd / "f") "v" with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env -> confined env)

let suite =
  ( "env",
    [
      Alcotest.test_case "check: cwd is confined" `Quick (Expect.passes check);
      Alcotest.test_case "crash: cwd is confined" `Quick (Expect.passes crash);
    ] )
