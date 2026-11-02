(** Lexical arithmetic on the paths an {!Eio} filesystem is addressed by: Eio
    paths, whose separator is always ["/"] even on Windows, so the host's own
    path modules answer about a different language. Everything here is lexical;
    nothing opens a file or asks the filesystem anything. {!normalize} rewrites
    a [".."], sound only where the component before it is not a link;
    {!canonical} refuses one instead. *)

val is_abs : string -> bool
(** [is_abs path] is [true] iff [path] starts at the root, that is iff it opens
    with a separator. *)

val is_rel : string -> bool
(** [is_rel path] is [not (is_abs path)]. *)

val segs : string -> string list
(** [segs path] is [path] cut at every separator, empty segments kept: an
    absolute path opens with one, a directory path ends with one, and a doubled
    separator is one of its own. It is the inverse of joining with ["/"], so
    nothing about the spelling is lost: [segs ""] is [[""]]. *)

val normalize : string -> string
(** [normalize path] is [path] with its empty and ["."] segments dropped and its
    [".."] segments resolved against the segment before them, so the spellings
    of one path (["/a//b"], ["/a/./b"], ["/a/x/../b"]) share a name. A [".."]
    that would climb above an absolute root is dropped; one at the head of a
    relative path is kept, there being nothing to resolve it against. Directory
    syntax survives: a [path] ending in a separator, a ["."] or a [".."] is
    answered ending in a separator. A path left with no segment is ["/"] when
    absolute and ["."] otherwise, so [normalize ""] is ["."] and [normalize "."]
    is ["./"]. *)

val append : string -> string -> string
(** [append base path] is [path] resolved against [base]: [path] itself when it
    is absolute, and otherwise [base] and [path] joined by a separator, added
    only when [base] is not empty and does not already end in one. *)

val to_dir_path : string -> string
(** [to_dir_path path] is [path] in directory syntax: [path] ending in a
    separator, and unchanged when it already does. The empty path is the current
    directory, as Eio reads it, so [to_dir_path ""] is ["./"]. *)

val basename : string -> string
(** [basename path] is [path]'s last component, ignoring trailing separators,
    and [""] for a path that has none: the empty path, a root, ["."] or [".."].
*)

val parent : string -> string
(** [parent path] is the directory holding [path], in directory syntax and
    without resolving it: ["/"] for a path directly under the root, ["./"] for a
    relative path of one component, and the root itself for the root. The empty
    path is the current directory, as Eio reads it, so its parent is ["../"]; a
    final ["."] is the directory before it, and the parent of a final [".."] is
    one more [".."] up, except at the root. *)

val components : string -> string list
(** [components path] are the segments {!normalize} leaves, its empty and ["."]
    segments dropped. Two spellings of one path have equal components. An
    absolute and a relative path can too ([components "/a"] and [components "a"]
    are both [["a"]]), so a comparison checks {!is_abs} as well, as {!is_rooted}
    does. *)

val is_rooted : root:string -> string -> bool
(** [is_rooted ~root path] is [true] iff [root] and [path] are both absolute or
    both relative, the {!components} of [root] are a prefix of those of [path],
    and the components of [path] past that prefix hold no [".."]: [path] names
    [root] or something under it, whatever the spelling of either. A relative
    [path] that still climbs out of a relative [root] once normalised is not
    under it, so [is_rooted ~root:"." "../x"] is [false]. An absolute [root] and
    a relative [path] are never rooted in one another: they are named from
    different places and nothing here knows where. The answer is lexical, so it
    holds of the objects named only where no component on the way is a symbolic
    link. *)

(** The type for the reasons {!canonical} refuses a path. *)
type refusal =
  | Not_a_path  (** Empty, or holding a NUL byte. *)
  | Relative  (** Does not start at the root. *)
  | Root  (** Is the root, which has no components. *)
  | Not_canonical  (** Names something, by a spelling that is not its own. *)

val canonical : string -> (string list, refusal) result
(** [canonical path] is [Ok cs], [cs] the components of [path], when [path] is
    absolute and already canonical, and otherwise [Error r] with [r] the first
    of these that holds:
    - {!Not_a_path} if [path] is empty or holds a NUL byte.
    - {!Relative} if [path] does not start with a separator.
    - {!Root} if [path] has no segment but empty ones (["/"], ["//"]).
    - {!Not_canonical} if a segment is ["."] or [".."], or if an empty segment
      follows the leading one (a trailing or doubled separator).

    It refuses rather than rewrites: rewriting [a/b/..] to [a] names a different
    directory wherever [b] is a symbolic link, so a caller resolving components
    under [O_NOFOLLOW] needs the refusal. *)
