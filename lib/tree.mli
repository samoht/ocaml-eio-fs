(** Trees and directories on any {!Eio} backend:

    - walks over the tree under a path, which measure it ({!val-usage}), read
      its links ({!links}), change its modes ({!grant_owner}, {!revoke_writes}),
      copy its files with other links apart ({!unshare}) or format it
      ({!pp_tree});
    - the removal of a chain of empty directories ({!remove_empty});
    - whether two paths name one file ({!same_file}). *)

type usage = {
  allocated_bytes : int64;
      (** The bytes every entry's allocated 512-byte blocks hold, whatever its
          apparent length. *)
  apparent_bytes : int64;  (** The lengths of the regular files. *)
  files : int;  (** The number of regular files. *)
  latest_mtime : float;
      (** The latest modification time of any entry, in seconds since the epoch.
      *)
}
(** The type for what a tree costs. Both byte counts saturate at
    [Int64.max_int]. *)

val usage : ?max_fibers:int -> _ Eio.Path.t -> usage
(** [usage ~max_fibers path] measures [path] and all descendants in one walk,
    without following symbolic links. The walk is breadth-first, at most
    [max_fibers] workers per level (defaults to [32]).

    @raise Invalid_argument if [max_fibers] is not positive.
    @raise Eio.Io as a stat or a listing of the walk raises. *)

val allocated : _ Eio.Path.t -> int64
(** [allocated path] is the bytes [path] allocates itself, without following a
    symbolic link and without its descendants: a directory's own blocks, which
    {!val-usage} adds to its entries'. It is {!val-usage}'s
    {!field-allocated_bytes} reading of one entry.

    @raise Eio.Io when the stat fails. *)

val grant_owner : _ Eio.Path.t -> unit
(** [grant_owner p] grants its owner read, write and search on the directory [p]
    and on each directory under it, so that every entry of the tree can be
    unlinked: a tree that a program made read-only, then left when it was
    killed, refuses any removal. Symbolic links are not followed, so the walk
    stays in [p]'s tree. A directory that cannot be granted or read is left as
    it is, for the removal to name; a [p] that is missing or not a directory is
    left alone. It costs a stat per entry and a listing per directory. *)

val revoke_writes : _ Eio.Path.t -> unit
(** [revoke_writes p] removes every write bit from [p] and from each entry under
    it but symbolic links, which are neither followed nor changed. An entry that
    has none is left as it is, since a chmod moves its change time even to the
    same mode. It costs a stat per entry and a listing per directory.

    @raise Eio.Io when an entry cannot be read or changed. *)

val remove_empty : 'a Eio.Path.t list -> (unit, 'a Eio.Path.t * exn) result
(** [remove_empty dirs] removes each directory of [dirs] in turn while it is
    empty, as Git removes the directories a deleted ref leaves: the first that
    holds anything, or is gone, ends it with [Ok ()], and stays with those after
    it. It is [Error (d, e)] for the first [d] whose removal failed for another
    reason, [e] the {!Eio.Io} it raised. *)

val links : _ Eio.Path.t -> string -> (string * string) list option
(** [links dir rel] is each symbolic link at or below [rel] in [dir] with its
    target, read and not followed, when [rel] is a link, or a directory whose
    entries are each a link or such a directory. A link is named by its path
    from [dir]: an entry [n] of a directory [rel] is [rel/n], a trailing slash
    of [rel] dropped. It is [None] when [rel], or an entry below it, is anything
    else: a file, an empty directory, or nothing. The walk stops at the first
    such entry.

    @raise Eio.Io when a directory or a link of the walk cannot be read. *)

val same_file : _ Eio.Path.t -> _ Eio.Path.t -> bool
(** [same_file a b] is [true] iff [a] and [b] both name a file and their stats
    give the same device and inode: the same file, by any of its names. Symbolic
    links are followed, and a path that names nothing matches no path, itself
    included.

    @raise Eio.Io as a stat fails for another reason than a missing file. *)

val unshare : spare:'a Eio.Path.t -> 'a Eio.Path.t -> (unit, string) result
(** [unshare ~spare path] replaces each regular file at or under [path] that has
    more than one link by a copy of itself, so a write to it reaches no other
    link. A copy is made beside [spare], named [spare] with a count appended,
    with the file's permission bits, and renamed over the file. Symbolic links
    are not followed, as {!val-usage} does not follow them. It is [Error why]
    for the first file that cannot be copied apart, [why] naming it and the
    reason; the files copied before it stay apart.

    @raise Eio.Io as a stat or a listing of the walk raises. *)

val pp_tree : depth:int -> _ Eio.Path.t Fmt.t
(** [pp_tree ~depth] formats a path and what is under it [depth] levels deep, as
    tree(1) lists it: each entry by its last component, a symbolic link as
    [name -> target], not followed. [depth = 1] is the path alone, and
    [depth <= 0] prints nothing. A missing path prints nothing, and an entry
    that is neither a regular file, a directory nor a symbolic link (a FIFO, a
    socket, a device) is left out with what is under it.

    @raise Eio.Io
      if a directory cannot be listed or a link read; what was formatted before
      stays formatted. *)
