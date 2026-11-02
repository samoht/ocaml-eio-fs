(** In-memory filesystems and filesystem helpers for {!Eio}.

    [Eio_fs] provides a filesystem ({!Fs}) that lives entirely in the process, a
    way to embed a directory tree into an OCaml module at compile time
    ({!Crunch}), and a fault-injecting wrapper ({!Chaos}). Each call to {!Fs.v},
    {!Fs.load}, {!Crunch.load} or {!run} makes a filesystem of its own: two of
    them share no file.

    {!Nofollow}, {!Path} and {!Chaos} work on any {!Eio} backend, the in-memory
    one included. *)

(** Bounded reads that refuse a symbolic link at the final path component. *)
module Nofollow : sig
  type error =
    [ `Invalid of string
      (** The [validate] function refused the file's stat, for the reason given.
      *)
    | `Too_large  (** The file holds more than [max_bytes] bytes. *) ]
  (** The type for read refusals. *)

  type result = (Cstruct.t, error) Result.t
  (** The type for read results: the file's bytes, or why they were refused. *)

  val read_file :
    max_bytes:int ->
    validate:(Eio.File.Stat.t -> (unit, string) Result.t) ->
    _ Eio.Path.t ->
    result
  (** [read_file ~max_bytes ~validate path] opens [path] as
      [Eio.Path.open_in ~follow:false] does, validates the opened file's
      {!Eio.File.stat} with [validate], and reads through end-of-file with one
      byte of lookahead past [max_bytes]. Validation happens before any byte is
      read. The result is [Ok data] with the file's bytes, [Error (`Invalid r)]
      if [validate] answered [Error r], and [Error `Too_large] if the file holds
      more than [max_bytes] bytes. The returned [Cstruct.t] belongs to the
      caller and can be overwritten after use.

      Prefix components keep ordinary {!Eio.Path} resolution and confinement;
      only the final component is refused when it is a symbolic link. Every
      backend that honours [~follow:false] gets the same refusal, and a wrapper
      such as {!Chaos} observes the one open.

      @raise Invalid_argument if [max_bytes] is negative or [max_int].
      @raise Eio.Io
        when the open or read fails, including when the final component is a
        symbolic link. *)

  val run :
    max_bytes:int ->
    validate:(Eio.File.Stat.t -> (unit, string) Result.t) ->
    stat:Eio.File.Stat.t ->
    read:(Cstruct.t -> int) ->
    result
  (** [run ~max_bytes ~validate ~stat ~read] validates [stat] before asking
      [read] for bytes and reads through end-of-file with one byte of lookahead
      past [max_bytes]. The buffer offered to [read] starts at [stat]'s size
      plus one byte and doubles toward [max_bytes + 1] only while the file keeps
      answering, so a read costs the file and not the bound. A rejected or
      partially read buffer, and each one outgrown, is overwritten before the
      function returns or raises. The result is {!read_file}'s, for a caller
      that opened the file another way; [read] answering [0] or raising
      [End_of_file] ends the file, and what [validate] or [read] raises
      otherwise is propagated.

      @raise Invalid_argument if [max_bytes] is negative or [max_int]. *)
end

(** Helpers over standard {!Eio.Path} values. The lexical arithmetic on the
    strings those values carry (what a path's components are, and which
    spellings name one object) is {!Eio_fs_path}, in [eio-fs.path]. *)
module Path : sig
  type usage = {
    allocated_bytes : int64;
        (** The bytes every entry's allocated 512-byte blocks hold, whatever its
            apparent length. *)
    apparent_bytes : int64;  (** The lengths of the regular files. *)
    files : int;  (** The number of regular files. *)
    latest_mtime : float;
        (** The latest modification time of any entry, in seconds since the
            epoch. *)
  }
  (** The type for what a tree costs. Both byte counts saturate at
      [Int64.max_int]. *)

  val usage : ?max_fibers:int -> _ Eio.Path.t -> usage
  (** [usage ~max_fibers path] measures [path] and all descendants in one walk,
      without following symbolic links. The walk is breadth-first, at most
      [max_fibers] workers per level (defaults to [32]).

      Filesystem errors are propagated, so a caller deciding whether to remove a
      tree fails closed.

      @raise Invalid_argument if [max_fibers] is not positive. *)

  val iter :
    ?max_fibers:int ->
    ('a Eio.Path.t -> Eio.File.Stat.t -> unit) ->
    'a Eio.Path.t ->
    unit
  (** [iter ~max_fibers f path] is [f p stat] for [path] and each of its
      descendants [p], with [stat] its one [Eio.Path.stat ~follow:false]:
      symbolic links are not followed. A directory's [f] runs before its entries
      are read. The walk is {!val-usage}'s: breadth-first, at most [max_fibers]
      workers per level (defaults to [32]).

      Filesystem errors and those [f] raises are propagated, the walk's other
      workers cancelled.

      @raise Invalid_argument if [max_fibers] is not positive. *)

  val name : _ Eio.Path.t -> string
  (** [name path] is what [path] is called outside the program: its native
      spelling when the capability has one, and the rendering {!Eio.Path.pp}
      gives it when not. *)

  val pp_tree : depth:int -> _ Eio.Path.t Fmt.t
  (** [pp_tree ~depth] formats a path and what is under it [depth] levels deep,
      as tree(1) lists it: each entry by its last component, a symbolic link as
      [name -> target], not followed. [depth = 1] is the path alone, and
      [depth <= 0] prints nothing. *)
end

(** An in-memory filesystem for {!Eio}. *)
module Fs : sig
  val v : unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [v ()] is the root of a fresh, empty in-memory filesystem. Closing this
      capability prevents further operations through its aliases. Directories
      already opened from it keep their own lifetimes. *)

  val load :
    ?filter:('a Eio.Path.t -> bool) ->
    'a Eio.Path.t ->
    [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [load ~filter path] is the root of a new in-memory filesystem holding a
      copy of the directory tree at [path], at the same path from its root:
      [load (dir / "a/b")] holds [/a/b]. [filter] (defaults to [fun _ -> true])
      is called on [path] and every path below it before it is copied: a path
      for which it is [false] is not copied, nor anything under it. [path] can
      be on any filesystem, an in-memory one included.

      An entry that is neither a regular file, a directory nor a symbolic link
      is not copied; it is traced with [Eio.traceln].

      @raise Eio.Io if [path] or an entry under it cannot be read. *)
end

(** Compile-time file embedding for the in-memory filesystem, in the style of
    ocaml-crunch. The [eio-fs-crunch] binary serializes a directory tree into an
    OCaml module of {!Crunch.entry} values; at runtime the generated module's
    [load ()] rebuilds the tree in a new in-memory filesystem. *)
module Crunch : sig
  (** The type for nodes of an embedded file tree. Paths are absolute within the
      image and use ["/"] as separator. *)
  type entry =
    | Dir of { path : string; perm : int }
        (** A directory at [path] with mode [perm]. *)
    | File of { path : string; perm : int; contents : string }
        (** A regular file at [path] with mode [perm], holding [contents]. *)
    | Symlink of { path : string; target : string }
        (** A symbolic link at [path] holding [target], never resolved. *)

  type t = entry list
  (** The type for embedded file trees. Every directory precedes its children.
  *)

  val scan : _ Eio.Path.t -> t
  (** [scan dir] serializes the directory tree rooted at [dir]. The root is
      recorded as [Dir { path = "/"; _ }], entries follow in depth-first sorted
      order, and symbolic links are recorded without being followed. An entry
      that is neither a regular file, a directory nor a symbolic link is not
      recorded; it is traced with [Eio.traceln].

      @raise Invalid_argument if [dir] is not a directory. *)

  val find : t -> string -> string option
  (** [find entries path] is [Some contents] for the first {!File} at [path] in
      [entries], and [None] if there is none. [path] is absolute within the
      image, as {!scan} records it, and compared as a string; a symbolic link is
      not followed. A program that embeds fixtures reads them with it and needs
      no filesystem. *)

  val load : t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [load entries] is the root of a new in-memory filesystem holding
      [entries], with their permissions and symbolic links. A {!Dir} creates its
      missing ancestors; a {!File} or {!Symlink} needs its parent directory to
      exist already, as the root or from an earlier entry.

      @raise Eio.Io
        with {!Eio.Fs.Not_found} if the parent of a {!File} or {!Symlink} is
        missing, and with {!Eio.Fs.Already_exists} if its path is already taken.
  *)

  val source : t Fmt.t
  (** [source] formats a standalone OCaml module embedding the entries. The
      generated module exposes [entries : Eio_fs.Crunch.t] and
      [load : unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t], which is {!load}
      of [entries]. *)

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
(** [run ~cwd fn] is [fn env], with [env] holding the root and the current
    directory of a new in-memory filesystem; the current directory is the
    absolute path [cwd] (defaults to ["/"]).

    {b Note.} This does not install an effect handler; wrap the call in
    something like [Eio_mock.Backend.run]. *)
