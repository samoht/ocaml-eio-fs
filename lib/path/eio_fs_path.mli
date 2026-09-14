(** Lexical arithmetic on the paths an {!Eio} filesystem is addressed by.

    They are Eio paths, whose "directory separator is always ["/"], even on
    Windows" ({!Eio.Path}), so the host's own path modules answer about a
    different language: to them a ["\\"] may be a separator and a leading ["c:"]
    a volume, and here both are ordinary characters in an ordinary component.
    This is where the rules for these paths live, so that what a path resolves
    to has one producer.

    Everything here is lexical: nothing opens a file, follows a link or asks the
    filesystem anything. {!normalize} and {!components} therefore rewrite a
    [".."], which is sound only where the component before it is not a symbolic
    link; {!canonical} refuses one instead, for a caller that cannot assume
    that. *)

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
(** [canonical path] are the components of [path] when [path] is an absolute
    path already written in canonical form, and why it is not otherwise.

    It refuses rather than rewrites, which is what a caller resolving the
    components one at a time under [O_NOFOLLOW] needs: rewriting [a/b/..] to [a]
    names a different directory wherever [b] is a symbolic link, and such a
    caller exists precisely because it cannot assume [b] is not. So a [path]
    carrying a ["."] or a [".."] is {!Not_canonical} here rather than resolved,
    and so is every spelling whose components do not rejoin to exactly the
    string given -- a trailing separator, a doubled separator, a leading double
    separator. What comes back is the components of a path that already was
    canonical, so the caller's own name for it and the filesystem's are the same
    string.

    A NUL byte is {!Not_a_path} rather than a component boundary. Every layer
    below this one takes it for the end of the string, so a path carrying one is
    refused where the refusal can still name the whole of it. *)
