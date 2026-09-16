(** Lexical arithmetic on the paths an {!Eio} filesystem is addressed by: Eio
    paths, whose separator is always ["/"] even on Windows, so the host's own
    path modules answer about a different language. Everything here is lexical;
    nothing opens a file or asks the filesystem anything. {!normalize} rewrites
    a [".."], sound only where the component before it is not a link;
    {!canonical} refuses one instead. *)

val is_abs : string -> bool
(** [is_abs path] is whether [path] starts at the root, which is whether it
    opens with a separator. *)

val is_rel : string -> bool
(** [is_rel path] is [not (is_abs path)]. *)

val segs : string -> string list
(** [segs path] is [path] cut at every separator, empty segments kept: an
    absolute path opens with one, a directory path ends with one, and a doubled
    separator is one of its own. It is the inverse of joining with ["/"], so
    nothing about the spelling is lost. *)

val normalize : string -> string
(** [normalize path] is [path] with its empty and ["."] segments dropped and its
    [".."] segments resolved against the segment before them, so the spellings
    of one path (["/a//b"], ["/a/./b"], ["/a/x/../b"]) share a name. A [".."]
    that would climb above an absolute root is dropped; one at the head of a
    relative path is kept, there being nothing to resolve it against. Directory
    syntax survives: a [path] ending in a separator, a ["."] or a [".."] is
    answered ending in a separator. *)

val append : string -> string -> string
(** [append base path] is [path] resolved against [base]: [path] itself when it
    is absolute, and [base] and [path] joined by a single separator otherwise.
*)

val to_dir_path : string -> string
(** [to_dir_path path] is [path] in directory syntax: ending in a separator, and
    unchanged when it already does. *)

val basename : string -> string
(** [basename path] is [path]'s last component, ignoring trailing separators,
    and [""] for a path that has none: a root, ["."] or [".."]. *)

val parent : string -> string
(** [parent path] is the directory holding [path], in directory syntax: ["/"]
    for a path directly under the root, ["./"] for a relative path of one
    component, and the root itself for the root. *)

val components : string -> string list
(** [components path] are the segments {!normalize} leaves, its empty and ["."]
    segments dropped. Two spellings of one path have one answer here, which is
    what makes it the thing to compare two paths by. *)

val is_rooted : root:string -> string -> bool
(** [is_rooted ~root path] is whether [path] names [root] or something under it,
    compared by {!components}, so neither spelling matters. An absolute [root]
    and a relative [path] are never rooted in one another: they are named from
    different places and nothing here knows where. *)

type refusal =
  | Not_a_path  (** Empty, or holding a NUL byte. *)
  | Relative  (** Does not start at the root. *)
  | Root  (** Is the root, which has no components. *)
  | Not_canonical  (** Names something, by a spelling that is not its own. *)

val canonical : string -> (string list, refusal) result
(** [canonical path] are the components of [path] when it is absolute and
    already canonical, and why it is not otherwise. It refuses rather than
    rewrites: rewriting [a/b/..] to [a] names a different directory wherever
    [b] is a symbolic link, so a caller resolving components under [O_NOFOLLOW]
    needs the refusal. A ["."], [".."] or any spelling whose components do not
    rejoin to exactly the string given is {!Not_canonical}; a NUL byte is
    {!Not_a_path}. *)
