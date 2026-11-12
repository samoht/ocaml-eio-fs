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

  val read_file :
    max_bytes:int ->
    validate:(Eio.File.Stat.t -> (unit, string) result) ->
    _ Eio.Path.t ->
    (Cstruct.t, error) result
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

      @raise Stdlib.Invalid_argument if [max_bytes] is negative or [max_int].
      @raise Eio.Io
        when the open or read fails, including when the final component is a
        symbolic link. *)

  val run :
    max_bytes:int ->
    validate:(Eio.File.Stat.t -> (unit, string) result) ->
    stat:Eio.File.Stat.t ->
    read:(Cstruct.t -> int) ->
    (Cstruct.t, error) result
  (** [run ~max_bytes ~validate ~stat ~read] is the bounded read of {!read_file}
      over a file the caller has already opened. It opens nothing and refuses no
      symbolic link: that check belongs to the open. It validates [stat] before
      asking [read] for bytes and reads through end-of-file with one byte of
      lookahead past [max_bytes]. The buffer offered to [read] starts at
      [stat]'s size plus one byte and doubles toward [max_bytes + 1] only while
      the file keeps answering, so a read costs the file and not the bound. A
      rejected or partially read buffer, and each one outgrown, is overwritten
      before the function returns or raises. The result is {!read_file}'s, for a
      caller that opened the file another way; [read] answering [0] or raising
      [End_of_file] ends the file, and what [validate] or [read] raises
      otherwise is propagated.

      @raise Stdlib.Invalid_argument if [max_bytes] is negative or [max_int]. *)
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

      @raise Stdlib.Invalid_argument if [max_fibers] is not positive. *)

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

      @raise Stdlib.Invalid_argument if [max_fibers] is not positive. *)

  val grant_owner : _ Eio.Path.t -> unit
  (** [grant_owner p] grants its owner read, write and search on the directory
      [p] and on each directory under it, so that every entry of the tree can be
      unlinked: a tree that a program made read-only, then left when it was
      killed, refuses any removal. Symbolic links are not followed, so the walk
      stays in [p]'s tree. A directory that cannot be granted or read is left as
      it is, for the removal to name; a [p] that is missing or not a directory
      is left alone. It costs a stat per entry and a listing per directory. *)

  val revoke_writes : _ Eio.Path.t -> unit
  (** [revoke_writes p] removes every write bit from [p] and from each entry
      under it but symbolic links, which are neither followed nor changed. An
      entry that has none is left as it is, since a chmod moves its change time
      even to the same mode. It costs a stat per entry and a listing per
      directory.

      @raise Eio.Io when an entry cannot be read or changed. *)

  val name : _ Eio.Path.t -> string
  (** [name path] is what [path] is called outside the program: its native
      spelling when the capability has one, and the rendering {!Eio.Path.pp}
      gives it when not. *)

  val pp_tree : depth:int -> _ Eio.Path.t Fmt.t
  (** [pp_tree ~depth] formats a path and what is under it [depth] levels deep,
      as tree(1) lists it: each entry by its last component, a symbolic link as
      [name -> target], not followed. [depth = 1] is the path alone, and
      [depth <= 0] prints nothing. A missing path prints nothing, and an entry
      that is neither a regular file, a directory nor a symbolic link (a FIFO, a
      socket, a device) is left out with what is under it.

      @raise Eio.Io
        if a directory cannot be listed or a link read; what was formatted
        before stays formatted. *)
end

type user = {
  uid : int;  (** The user id. *)
  gid : int;  (** The primary group id. *)
  groups : int list;  (** The supplementary group ids. *)
  umask : int;  (** The mode bits a new file or directory loses. *)
}
(** The type for the user an in-memory filesystem's operations run as. *)

val root : user
(** [root] is uid [0], gid [0], no supplementary group and umask [0o022]. *)

(** An in-memory filesystem for {!Eio}.

    An entry's stat carries its kind, mode, size and inode, and the owner
    {!Eio.Path.chown} gives it. It keeps no times, no block count and no link
    count: [atime], [mtime] and [ctime] are [0.], [blocks] is [0], so
    {!Path.val-usage} counts no allocated bytes, and [nlink] is [0].

    A file's bytes live in memory, holes included, and a file holds at most 4
    GiB: a write or a truncation past that raises {!Eio.Fs.File_too_large}.

    Where POSIX systems differ, it answers as Linux does: {!Eio.Path.unlink} of
    a directory raises the error EISDIR names, not [Permission_denied], a
    positioned write to a file opened to append lands at its end,
    [Eio.Path.chmod ~follow:false] on a symbolic link raises the error
    EOPNOTSUPP names, and a symbolic link to the empty path is refused with
    [Not_found]. Its paths have no native spelling ({!Eio.Path.native} is
    [None]), and an absolute symbolic link resolves from the filesystem's root,
    so through the root that {!Fs.v} returns it is followed where eio_posix
    would refuse it. One filesystem may be used from several domains at once.

    A path resolves one component at a time, as path_resolution(7) describes: a
    [".."] climbs from the directory the walk has reached, a link's target
    included, never from a lexical rewrite of the path. A directory opened from
    the filesystem is confined as eio_posix confines one: an absolute path, an
    absolute link target and a [".."] above that directory are refused with
    [Eio.Fs.Permission_denied], whose message names the path asked for and not
    where the directory sits.

    {b Permissions.} Each operation runs as a {!type-user} and meets the POSIX
    rules: search ([x]) on every directory a path walks; write and search on the
    directory where an entry is made, removed or renamed, with the sticky bit
    keeping removal and renaming to the entry's or the directory's owner; read
    on a directory to list it; read, or read and write, on a file to open it. In
    a sticky directory that others may write, a link is followed as the last
    component of a path only by its owner, or if the directory's owner owns it,
    root included: Linux does so with [fs.protected_symlinks] set, as most
    distributions set it. A new file or directory loses the user's umask bits
    and belongs to the user's uid and gid, or to the group of a setgid directory
    it is made in. A new directory keeps only the permission and sticky bits of
    its mode, and is setgid iff its parent is. A file made in a setgid directory
    by a user outside that group loses setgid, as Linux strips it at create.
    Only a file's owner changes its mode, and a user outside the file's group
    cannot set its setgid bit. Only root gives a file away; its owner may move
    it to one of their own groups. A chown of a regular file, or a write to one
    by a user other than root, clears its setuid bit, and its setgid bit when
    its group may execute it. Root needs no bit to read, write or search, as on
    Linux, so a test that runs as {!root}, the default, misses the bugs a user
    meets: a directory it cannot create in, a file it cannot open. *)
module Fs : sig
  val v : ?user:user -> unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [v ~user ()] is the root of a fresh, empty in-memory filesystem whose
      operations run as [user] (defaults to {!root}), who owns its root
      directory. Closing this capability prevents further operations through its
      aliases. Directories already opened from it keep their own lifetimes. *)

  val as_user : user -> 'a Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [as_user user path] is [path], an in-memory directory, with its operations
      run as [user]. A test sets a tree up as {!root} and runs the code under
      test as a user. A directory opened from the result shares its descriptor
      with [path]'s.

      @raise Stdlib.Invalid_argument if [path] is not an in-memory directory. *)

  val load :
    ?filter:('a Eio.Path.t -> bool) ->
    'a Eio.Path.t ->
    [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [load ~filter path] is the root of a new in-memory filesystem holding a
      copy of the directory tree at [path], at the same path from its root:
      [load (dir / "a/b")] holds [/a/b]. [path] is followed if it is a symbolic
      link; the entries below it are copied as they are, a link kept a link.
      [filter] (defaults to [fun _ -> true]) is called on [path], then on every
      path below it, before anything of it is copied: a path for which it is
      [false] is not copied, nor anything under it, so a refused [path] is an
      empty filesystem. [path] can be on any filesystem, an in-memory one
      included.

      An entry that is neither a regular file, a directory nor a symbolic link
      is not copied; it is traced with [Eio.traceln].

      @raise Stdlib.Invalid_argument if [path] is not a directory.
      @raise Eio.Io
        if [path] or an entry under it cannot be read, or a regular file is
        replaced between the stat that found it and the open that copies it:
        that open does not follow a link. *)
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

      @raise Stdlib.Invalid_argument if [dir] is not a directory.
      @raise Eio.Io if [dir] or an entry under it cannot be read. *)

  val find : string -> t -> string option
  (** [find path entries] is [Some contents] for the first {!File} at [path] in
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

val run : ?cwd:string -> ?user:user -> (stdenv -> 'a) -> 'a
(** [run ~cwd ~user fn] calls [fn] with a new in-memory filesystem in place of
    the host's. Unlike [Eio_main.run], it starts no event loop and installs no
    effect handler: call it inside one, such as [Eio_main.run] or
    [Eio_mock.Backend.run]. [env] holds the root and the current directory of
    the new filesystem. The current directory is the directory at the absolute
    path [cwd] (defaults to ["/"]), made with its missing ancestors, mode
    [0o755], before [fn] runs. Like a process's, it follows that directory
    through a rename. Both run as [user] (defaults to {!root}). Root owns the
    tree and gives [user] the current directory.

    @raise Stdlib.Invalid_argument if [cwd] is not an absolute path. *)
