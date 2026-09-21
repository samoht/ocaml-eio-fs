(** In-memory {!Eio} resources.

    [Eio_fs] provides in-memory resources for {!Eio}: a filesystem ({!Fs}) that
    lives entirely in the process, a way to embed a directory tree into an OCaml
    module at compile time ({!Crunch}), and a fault-injecting wrapper
    ({!Chaos}). There is a single in-memory filesystem per process; {!Fs.v},
    {!Fs.load} and {!Crunch.load} all reset it. *)

(** Backend support for descriptor-bound, bounded reads that refuse a symbolic
    link at the final path component. The user-facing POSIX operation is
    {!Eio_fs_unix.read_file_nofollow}; this module lets another directory
    backend provide the same operation without exposing its descriptor. *)
module Nofollow : sig
  type error = [ `Invalid of string | `Too_large ]
  type result = (Cstruct.t, error) Stdlib.result

  type reader =
    sw:Eio.Switch.t ->
    path:string ->
    max_bytes:int ->
    validate:(Eio.File.Stat.t -> (unit, string) Stdlib.result) ->
    result

  val find : _ Eio.Resource.t -> reader option
  (** [find dir] is the no-follow reader registered by [dir]'s backend. *)

  val run :
    max_bytes:int ->
    validate:(Eio.File.Stat.t -> (unit, string) Stdlib.result) ->
    stat:Eio.File.Stat.t ->
    read:(Cstruct.t -> int) ->
    result
  (** [run] validates [stat] before asking [read] for bytes and reads through
      end-of-file with one byte of lookahead past [max_bytes]. A rejected or
      partially read buffer is overwritten before the function returns or
      raises. This is the common implementation for directory backends. *)
end

(** Helpers over standard {!Eio.Path} values. The lexical arithmetic on the
    strings those values carry -- what a path's components are, and which
    spellings name one object -- is {!Eio_fs_path}, in [eio-fs.path]. *)
module Path : sig
  type usage = {
    allocated_bytes : int64;
    apparent_bytes : int64;
    files : int;
    latest_mtime : float;
  }
  (** What a tree costs in {!field-allocated_bytes} and {!field-apparent_bytes},
      how many {!field-files} carry it, and when one last changed. All come off
      one walk. *)

  val usage : ?max_fibers:int -> _ Eio.Path.t -> usage
  (** [usage path] measures [path] and all descendants without following
      symbolic links. {!field-allocated_bytes} is based on allocated 512-byte
      blocks, not apparent length, and both byte counts saturate at
      [Int64.max_int]; {!field-apparent_bytes} and {!field-files} count the
      regular files alone. The walk is breadth-first, at most [max_fibers]
      workers per level (default [32]).

      Filesystem errors are propagated, so a caller deciding whether to remove a
      tree fails closed.

      @raise Invalid_argument if [max_fibers] is not positive. *)

  val name : _ Eio.Path.t -> string
  (** [name path] is what [path] is called outside the program: its native
      spelling when the capability has one, and the rendering {!Eio.Path.pp}
      gives it when not. *)
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
