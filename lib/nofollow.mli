(** Reads and walks that follow no symbolic link: bounded reads that refuse one
    at the final path component, and the parents of a path opened one component
    at a time ({!with_parent}). *)

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
    read. The result is [Ok data] with the file's bytes, [Error (`Invalid r)] if
    [validate] answered [Error r], and [Error `Too_large] if the file holds more
    than [max_bytes] bytes. The returned [Cstruct.t] belongs to the caller and
    can be overwritten after use.

    Prefix components keep ordinary {!Eio.Path} resolution and confinement; only
    the final component is refused when it is a symbolic link. Every backend
    that honours [~follow:false] gets the same refusal, and a wrapper of
    [Eio_fs_chaos] observes the one open.

    @raise Invalid_argument if [max_bytes] is negative or [max_int].
    @raise Eio.Io
      when the open or read fails, including when the final component is a
      symbolic link. *)

val run :
  max_bytes:int ->
  validate:(Eio.File.Stat.t -> (unit, string) result) ->
  stat:Eio.File.Stat.t ->
  read:(Cstruct.t -> int) ->
  (Cstruct.t, error) result
(** [run ~max_bytes ~validate ~stat ~read] is {!read_file}'s read, for a caller
    that opened the file another way: it validates [stat] before asking [read]
    for bytes and reads through end-of-file with one byte of lookahead past
    [max_bytes]. The buffer offered to [read] starts at [stat]'s size plus one
    byte and doubles toward [max_bytes + 1] only while the file keeps answering,
    so a read costs the file and not the bound. A rejected or partially read
    buffer, and each one outgrown, is overwritten before the function returns or
    raises.

    @raise Invalid_argument if [max_bytes] is negative or [max_int]. *)

val with_regular :
  _ Eio.Path.t ->
  (Eio.File.ro_ty Eio.Resource.t -> Eio.File.Stat.t -> 'a) ->
  ('a, [ `Invalid of string ]) result
(** [with_regular path f] is [Ok (f file stat)], [file] the file at [path]
    opened as [Eio.Path.open_in ~follow:false] opens it and [stat] its stat once
    opened, when [path] is a regular file of one link. It is
    [Error (`Invalid why)] otherwise, [why] saying what [path] is, such as
    ["a symbolic link, not a regular file"] or ["a file of 2 links"], and [f] is
    not called. [path] is stated before the open, so a FIFO, which an open or a
    read waits on, and a device, which a read never ends, are refused unopened.
    The opened file is checked again: a rename over [path] once it is opened
    leaves it with no name, and it is read as it was opened.

    @raise Eio.Io when the stat or the open fails, or as [f] raises. *)

(** {1:parents Parents} *)

type Eio.Exn.Backend.t +=
  | Unsafe_parent of string
        (** Why {!with_parent} refused a path: it names nothing beneath its
            directory, or a parent on the way is not a directory, or was
            replaced while it was opened. *)

val with_parent :
  ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
  _ Eio.Path.t ->
  (Eio.Fs.dir_ty Eio.Path.t -> 'a) ->
  'a
(** [with_parent ~create path fn] is [fn leaf], [leaf] the last component of
    [path] in its parent, which is opened one component at a time from [path]'s
    directory, following no symbolic link. [path]'s name must be relative, with
    no empty, ["."] or [".."] component and no NUL byte. Each parent must be a
    directory, and the one opened must be the one its stat found just before, so
    a parent replaced meanwhile, by a symbolic link or anything else, is
    refused. [create] (defaults to [`Never]) says what a missing parent does:
    [`If_missing perm] makes it with mode [perm], less the umask, and [`Never]
    refuses it; a parent made stays when a later one is refused.

    A parent renamed while it is open still holds [leaf]. The parents are closed
    when [fn] returns or raises, and [leaf] must not be used after that;
    [path]'s directory stays open. [with_parent] does not follow a last
    component that is a symbolic link; what [fn] does with [leaf] resolves it as
    {!Eio.Path} does. Any backend serves, a directory with no host descriptor
    included.

    @raise Eio.Io
      with {!Eio.Fs.Permission_denied} carrying {!Unsafe_parent} for a refused
      name, a parent that is not a directory, or one replaced while it was
      opened; with {!Eio.Fs.Not_found} for a missing parent under [`Never]; and
      as the stat, open or mkdir of a parent raises otherwise. [fn] is then not
      called. *)
