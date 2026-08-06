(** In-memory {!Eio} resources.

    [Eio_fs] provides in-memory resources for {!Eio}: a filesystem ({!Fs}) that
    lives entirely in the process, a way to embed a directory tree into an OCaml
    module at compile time ({!Crunch}), and a fault-injecting wrapper
    ({!Chaos}). There is a single in-memory filesystem per process; {!Fs.v},
    {!Fs.load} and {!Crunch.load} all reset it. *)

(** Helpers over standard {!Eio.Path} values. *)
module Path : sig
  type usage = { allocated_bytes : int64; latest_mtime : float }
  (** The allocated size and newest modification time of a tree. *)

  val usage : ?max_fibers:int -> _ Eio.Path.t -> usage
  (** [usage path] measures [path] and all descendants without following
      symbolic links. {!field-allocated_bytes} is based on allocated 512-byte
      blocks, not apparent file length, and saturates at [Int64.max_int]. The
      immediate children are measured concurrently, bounded by [max_fibers]
      (default [32]); each child tree is walked serially so concurrency stays
      bounded.

      Filesystem errors are propagated. This makes a caller deciding whether to
      remove a tree fail closed instead of acting on a partial measurement.

      @raise Invalid_argument if [max_fibers] is not positive. *)
end

(** An in-memory filesystem for {!Eio}. *)
module Fs : sig
  val v : unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [v ()] is the root of a fresh, empty in-memory filesystem. *)

  val load : ?filter:('a Eio.Path.t -> bool) -> 'a Eio.Path.t -> unit
  (** [load ~filter path] resets the in-memory filesystem and fills it with the
      directory tree at [path]. [filter] is called on every path and the path is
      copied only if it returns [true] (defaults to [fun _ -> true]). *)

  val tree : depth:int -> _ Eio.Path.t Fmt.t
  (** [tree ~depth] formats a path as a [tree]-like listing [depth] levels deep:
      [depth = 1] is the path alone, and [depth <= 0] prints nothing. Symbolic
      links are shown as [name -> target] and are not followed. *)
end

(** Compile-time file embedding for the in-memory filesystem, in the style of
    ocaml-crunch. The [eio-fs-crunch] binary serializes a directory tree into an
    OCaml module of {!Crunch.entry} values; at runtime the generated module's
    [load ()] rebuilds the tree in {!Fs}. *)
module Crunch : sig
  (** The type for nodes of an embedded file tree. Paths are absolute within the
      image and use ["/"] as separator. *)
  type entry =
    | Dir of { path : string; perm : int }
    | File of { path : string; perm : int; contents : string }
    | Symlink of { path : string; target : string }

  type t = entry list
  (** The type for embedded file trees. Every directory precedes its children.
  *)

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
  (** [interface] is the [.mli] file matching what {!source} prints: it declares
      the generated module's entry list and its loading function. *)
end

module Chaos = Chaos
(** An adversarial filesystem wrapper for fault-injection testing. *)

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >
(** The type for the standard environment {!run} passes on: an in-memory
    filesystem and current directory. *)

val run : ?cwd:string -> (stdenv -> 'a) -> 'a
(** [run ~cwd fn] resets the in-memory filesystem and is [fn env], with [env]
    holding in-memory resources and the current directory bound to [cwd]
    (defaults to ["/"]).

    {b Note.} This does not install an effect handler; wrap the call in
    something like [Eio_mock.Backend.run]. *)
