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

(** Compile-time file embedding for the in-memory filesystem, in the style of
    ocaml-crunch. The [eio-fs-crunch] binary serializes a directory tree into an
    OCaml module of {!Crunch.entry} values; at runtime the generated module's
    [load ()] rebuilds the tree in {!Fs}. *)
module Crunch : sig
  (** One node of an embedded file tree. Paths are absolute within the image and
      use ["/"] as separator. *)
  type entry =
    | Dir of { path : string; perm : int }
    | File of { path : string; perm : int; contents : string }
    | Symlink of { path : string; target : string }

  type t = entry list
  (** An embedded file tree: every directory precedes its children. *)

  val scan : _ Eio.Path.t -> t
  (** [scan dir] serializes the directory tree rooted at [dir]. The root is
      recorded as [Dir { path = "/"; _ }], entries follow in depth-first sorted
      order, and symbolic links are recorded without being followed.

      @raise Invalid_argument if [dir] is not a directory. *)

  val load : t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [load entries] resets the in-memory filesystem, populates it with
      [entries] -- preserving permissions and symbolic links -- and returns its
      root. *)

  val source : t Fmt.t
  (** [source] prints a standalone OCaml module embedding the entries. The
      generated module exposes [entries : Eio_fs.Crunch.t] and
      [load : unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t]. *)

  val interface : string
  (** The interface matching {!source} output, for the generated module's [.mli]
      file. *)
end

module Chaos = Chaos
(** An adversarial filesystem wrapper for fault-injection testing. *)

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >

val run : ?cwd:string -> (stdenv -> 'a) -> 'a
(** [run fn] runs [fn] which provides in-memory resources to [fn]. Note that
    this does not install an effect handler and you should use something like
    [Eio_mock.Backend.run] around this call.

    @param cwd
      Set the current working directory, otherwise it is assumed to be "/". *)
