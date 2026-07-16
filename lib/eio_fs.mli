(** {1 In-memory {! Eio} Resources}

    {! Eio_fs} provides in-memory resources for {! Eio}. *)

(** An in-memory filesystem for {! Eio}. *)
module Fs : sig
  val v : unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** Construct a fresh, empty in-memory file-system. *)

  val load : ?filter:('a Eio.Path.t -> bool) -> 'a Eio.Path.t -> unit
  (** [load path] will reset the in-memory filesystem and fill it with the
      directory tree at [path].

      @param filter
        Called on every path, if it returns [true] then this file is kept. *)

  val tree : depth:int -> _ Eio.Path.t Fmt.t
  (** A [tree]-like pretty printer. *)
end

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >

val run : ?cwd:string -> (stdenv -> 'a) -> 'a
(** [run fn] runs [fn] which provides in-memory resources to [fn]. Note that
    this does not install an effect handler and you should use something like
    [Eio_mock.Backend.run] around this call.

    @param cwd
      Set the current working directory, otherwise it is assumed to be "/". *)
