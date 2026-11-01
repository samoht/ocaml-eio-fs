(** The directory-durability barrier and the atomic publication built on it,
    portable across backends.

    {!sync_dir} issues the barrier a write-temporary-then-rename protocol needs;
    a backend with its own semantics registers {!Dir_sync}, and a plain POSIX
    directory is opened read-only and fsynced. {!sync_file} is the same for the
    bytes an entry names: a backend registers {!File_sync}, otherwise the
    barrier is {!Eio.File.sync}. {!publish} is the protocol itself, under a
    mandatory byte ceiling ({!type-config}, {!Limit_exceeded}).

    {!Process_local} declares whether any opener of a backend's files can exist
    outside this process, and {!Lock} is the exclusion a caller supplies. This
    library depends on nothing but eio. *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync : ('t, 't -> path:string -> unit, [> `Dir_sync ]) Eio.Resource.pi
        (** The operation a directory-capable backend registers in its resource
            handler: make the entries of the directory at [path] (relative to
            the resource, as for every DIR operation) durable. *)

val dir_barrier : 'a Eio.Resource.t -> (path:string -> unit) option
(** [dir_barrier res] is the directory barrier [res] registers, if any. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place. A backend that registers
    {!Dir_sync} answers through it; otherwise [dir] is opened read-only and its
    handle fsynced. [Eio_fs_unix.sync_dir] upgrades that to platter strength for
    programs that link unix.

    @raise Eio.Io if the directory cannot be opened or the sync fails.
    @raise Failure
      if the backend registers no barrier and its handles expose no sync
      operation. *)

val sync_parent : _ Eio.Path.t -> unit
(** [sync_parent p] makes the directory holding [p]'s entry durable. The parent
    comes from {!Eio.Path.split}, so it keeps [p]'s directory capability; a path
    with no parent is the root of that capability.

    @raise Eio.Io as {!sync_dir}, on the directory the barrier lands on. *)

val mkdirs :
  ?exists_ok:bool -> perm:Eio.File.Unix_perm.t -> _ Eio.Path.t -> unit
(** [mkdirs ~exists_ok ~perm p] creates [p] and its missing ancestors,
    establishing the durability of every traversed directory entry, parent
    first, with {!sync_parent}. Existing ancestors are barriered too: they may
    be visible after a previous writer's barrier failed. An accepted existing
    target is also barriered, so retry can establish its durability.

    The directory capability root must itself already be durable; this operation
    never acquires or barriers its parent outside the capability. [exists_ok]
    defaults to [false]; with [true], [p] may already be a directory. Existing
    permissions are unchanged. A file is never accepted as a directory.

    The number of directory barriers is linear in the traversed path depth. A
    failure can leave created directories visible with uncertain durability; a
    successful retry with [exists_ok] reissues the required barriers.

    @raise Eio.Io as {!Eio.Path.mkdir} and {!sync_dir} raise. *)

type (_, _, _) Eio.Resource.pi +=
  | File_sync : ('t, 't -> unit, [> `File_sync ]) Eio.Resource.pi
        (** The operation a file-capable backend registers in its resource
            handler: make this file's data durable. A file is itself the
            resource, so the operation carries no name, where {!Dir_sync} takes
            the [~path] a directory resource is joined with. *)

val file_barrier : 'a Eio.Resource.t -> (unit -> unit) option
(** [file_barrier res] is the file barrier [res] registers, if any. *)

val sync_file : _ Eio.File.rw -> unit
(** [sync_file file] makes [file]'s data durable: the barrier a commit protocol
    issues over the bytes it wrote. A backend that registers {!File_sync}
    answers through it, whole and never alongside the file's own sync; otherwise
    the barrier is {!Eio.File.sync}. [Eio_fs_unix.sync_file] upgrades that to
    platter strength for programs that link unix.

    @raise Eio.Io as {!Eio.File.sync}, or as the registered barrier raises. *)

type (_, _, _) Eio.Resource.pi +=
  | Process_local : ('t, 't -> bool, [> `Process_local ]) Eio.Resource.pi
        (** What a backend declares about its files' openers: [true] when no
            opener outside this process can reach them, so an in-process
            exclusion is the whole exclusion. A wrapper answers for its base. *)

val is_process_local : _ Eio.Resource.t -> bool
(** [is_process_local res] is what [res] declares through {!Process_local}, and
    [false] when it declares nothing: silence must read as "an outside opener
    exists". *)

type (_, _, _) Eio.Resource.pi +=
  | Modeless : ('t, 't -> bool, [> `Modeless ]) Eio.Resource.pi
        (** What a directory backend declares about Unix modes: [true] when it
            stores none, so a file's mode is neither kept nor changeable and its
            stat reports a fixed one. *)

val is_modeless : _ Eio.Resource.t -> bool
(** [is_modeless res] is what [res] declares through {!Modeless}, and [false]
    when it declares nothing. *)

type config = {
  durable : bool;
      (** Push the temporary and target directory to stable storage. *)
  perm : Eio.File.Unix_perm.t;
      (** The mode of the temporary, and therefore of the published file:
          exactly this, whatever the process umask. A {!Modeless} backend keeps
          no mode, so there the file is published without one. *)
  max_bytes : int64;  (** Hard ceiling on bytes accepted from the producer. *)
}
(** Publication policy. A finite {!type-config.field-max_bytes} is mandatory:
    streaming avoids buffering a whole value, but it must not turn an allocation
    failure into unbounded disk consumption. *)

exception Limit_exceeded of { max_bytes : int64; attempted : int64 }
(** Raised before a write that would exceed {!type-config.field-max_bytes}. *)

val fresh_name : string -> string
(** [fresh_name stem] is [stem] extended with a per-process salt and a
    fiber-atomic counter, so no other writer produces the same string. It is
    exported for a store whose target name is unknown until its bytes are read.
    The name is not itself the exclusion: open it with [`Exclusive]. *)

module Rename = Rename
(** Capability-dispatched exclusive rename and backend construction. *)

val publish :
  config:config ->
  ?mode:[ `Replace | `Exclusive ] ->
  ?rename:Rename.t ->
  ?tmp:'a Eio.Path.t ->
  'a Eio.Path.t ->
  (Eio.Flow.sink_ty Eio.Resource.t -> 'b) ->
  'b
(** [publish ~config ~mode ~rename ~tmp path write] publishes [path]'s contents
    atomically and durably: [write] streams into a temporary, which is fsynced,
    moved to [path], and whose directory is barriered with {!sync_parent}.

    [mode] defaults to [`Replace]. [`Exclusive] refuses an existing target
    atomically, using [rename] for directory backends without a registered
    operation. [rename] defaults to {!Rename.capability}; POSIX callers use the
    backend provided by [Eio_fs_unix.exclusive_rename].

    An omitted [tmp] is a fresh sibling name, opened exclusively and drawn
    again, at most 127 times, while the name is taken. An explicit one is also
    exclusive, must not exist, must differ from [path] and live on the same
    filesystem, and is never drawn again: a taken name raises [Already_exists].
    A process killed between the create and the rename leaves the temporary in
    place, so a caller naming [tmp] owns every way that name can already be
    taken. On a failure of [write], the sync or the rename, the temporary is
    removed and [path] stays intact; a barrier failure is reported after the
    rename, the new file visible with uncertain durability.
    {!type-config.field-durable} [false] keeps only the atomicity. *)

val publish_sparse :
  config:config ->
  ?mode:[ `Replace | `Exclusive ] ->
  ?rename:Rename.t ->
  ?tmp:'a Eio.Path.t ->
  size_bytes:int64 ->
  'a Eio.Path.t ->
  (int64 * Cstruct.t) list ->
  unit
(** [publish_sparse ~config ~size_bytes path spans] publishes a file with
    logical size [size_bytes] and ordered, disjoint [(offset, data)] extents.
    Unwritten gaps read as zeros; only the supplied data is written. The caller
    must retain the buffers without mutating them until the call returns.

    The temporary, modes, barriers, exclusive rename and failure cleanup are
    those of {!publish}. The logical size must fit the file-offset type and
    {!type-config.field-max_bytes}; every extent must lie within that size. All
    bounds and ordering are checked before any filesystem operation.

    @raise Limit_exceeded if [size_bytes] exceeds the byte ceiling.
    @raise Invalid_argument
      for a negative or unrepresentable size, negative offsets, unordered or
      overlapping extents, or data outside the logical file. A negative
      {!type-config.field-max_bytes} is refused as by {!publish}. *)

val save :
  ?durable:bool -> ?perm:Eio.File.Unix_perm.t -> _ Eio.Path.t -> string -> unit
(** [save ~durable ~perm path s] is {!publish} of the string [s] at [path]:
    [path] holds [s] whole or what it held before, never part of [s]. The
    ceiling is [s]'s length, [perm] defaults to [0o644] and [durable] to [true].
*)

val append_lines :
  ?durable:bool ->
  ?perm:Eio.File.Unix_perm.t ->
  _ Eio.Path.t ->
  string list ->
  unit
(** [append_lines ~durable ~perm path lines] appends each of [lines] to the file
    at [path], each ended by LF, in one write, as a log of JSON Lines or any
    other line-per-record file is appended to. A missing file is created with
    mode [perm] (defaults to [0o644]).

    A file whose last byte is not LF ends in a line torn by a crash part way
    through an append. That line is ended first, so the first of [lines] starts
    a line of its own: the torn bytes stay, as a line a reader refuses as it
    refuses any malformed one, and no line appended after them is read as their
    continuation. A file that is empty or ends in LF is appended to as it is.

    If [durable] is [true] (the default), the file is fsynced after the write,
    and the directory holding it too when the call created it. Appends from two
    writers are not ordered with each other: a caller with several takes a
    {!Lock} around each.

    @raise Invalid_argument if a line holds LF. *)

val publish_generation :
  durable:bool ->
  ?perm:Eio.File.Unix_perm.t ->
  'a Eio.Path.t ->
  ('a Eio.Path.t -> 'b) ->
  'b
(** [publish_generation ~durable path populate] publishes a directory tree at
    [path] whole: [populate] fills a fresh sibling generation directory (mode
    [perm], default [0o755]), and a symbolic link to it is renamed over [path].
    rename(2) keeps [path] naming the old link or the new one throughout, so a
    reader resolving [path] sees one complete generation or the other and never
    a mixture.

    With [durable], every file and directory in the generation is synced before
    the switch and [path]'s directory is barriered after it, so after a crash
    [path] resolves to the previous generation or the new one, never to neither;
    the previous generation is removed only once the switch is committed.
    [durable] [false] keeps only the atomicity.

    Reclaiming an owned old or failed staged generation grants its directories
    owner read, write and search permission when necessary. Files and symbolic
    link targets are not changed. A failure before the switch never changes the
    previous generation's permissions.

    [path] must be absent or a symbolic link: anything else raises
    [Invalid_argument] before [populate] runs, and is left as it was. A
    generation is a hidden sibling, a dot, [path]'s basename, [".gen."] and a
    fresh suffix ({!is_generation}); the one the old link named is reclaimed
    only when it is a generation of the same basename. One writer publishes a
    given [path] at a time; the exclusion is the caller's ({!Lock}).

    On a failure of [populate], a sync or the switch, the staged generation is
    removed and [path] is unchanged. A barrier failure is reported after the
    switch, the new generation visible with uncertain durability and the
    previous one retained. *)

val is_generation : string -> bool
(** [is_generation name] is whether [name] is a generation {!publish_generation}
    stages beside its pointer, so a reader listing that directory can tell the
    pointers from their generations. *)

module Lock = Lock
(** The critical section a caller supplies: what keeps two writers out of one
    another's read-modify-write, named by the deployment rather than the store.
*)

val publish_link :
  durable:bool ->
  lock:Lock.t ->
  expected:string option ->
  link_to:string ->
  _ Eio.Path.t ->
  (unit, [ `Changed of string option | `Invalid of string ]) result
(** [publish_link ~durable ~lock ~expected ~link_to path] replaces the entry at
    [path] with a symbolic link holding exactly [link_to], under [lock]. The
    entry must hold what [expected] says, read without following it: [None] is
    no entry, [Some raw] a symbolic link whose target is the bytes [raw].
    Anything else is [`Changed actual], and an entry that is not a symbolic link
    is [`Invalid]; neither is replaced. A link that already holds [link_to] is
    left as it is.

    The link is made under a fresh sibling name ({!fresh_name}), the entry at
    [path] is read again, and the link is renamed over it; rename(2) keeps
    [path] naming the old entry or the new link throughout. Targets are raw
    bytes, written and compared as given and never resolved or normalised; an
    empty target or one holding a NUL byte is [`Invalid].

    With [durable], the parent directory is barriered before the rename and
    after it, so after a crash [path] holds the old entry or the new link. A
    barrier that fails after the rename raises, the new link visible with
    uncertain durability; a later call with the same [link_to] issues the
    barrier again. [durable] [false] keeps only the atomicity.

    [lock] is the exclusion against other writers of [path], which a writer that
    ignores it can still race between the last read and the rename.

    @raise Lock.Unavailable when [lock] refuses, before anything is written. *)
