(** What the cases of this executable share: a fresh directory, another process
    holding a lock, and how a process ended. *)

val with_real_dir_env :
  (Eio_unix.Stdenv.base -> Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_real_dir_env fn] is [fn env dir], [dir] a fresh directory of the real
    file system, removed once [fn] returns. *)

val holding_process :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  clock:_ Eio.Time.clock ->
  kind:string ->
  lock:_ Eio.Path.t ->
  Eio_unix.Process.ty Eio.Resource.t
  * (unit -> [ `Exited of int | `Signaled of int ])
(** [holding_process ~sw ~env ~clock ~kind ~lock] is another process holding
    [lock] by [kind], ["record"] or ["lease"] ([hold_until_release] in
    [test.ml]), once it said it holds it, and the function that tells it to let
    go and answers how it ended: [`Exited 0] when it let go then, [`Exited 3]
    when it let go at its own five-second bound. *)

val status : [ `Exited of int | `Signaled of int ] Alcotest.testable
(** [status] compares how a process ended. *)
