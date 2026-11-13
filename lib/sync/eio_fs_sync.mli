(** The directory-durability barrier and the atomic publication built on it,
    portable across backends.

    {!sync_dir} issues the barrier a write-temporary-then-rename protocol needs;
    a backend with its own semantics registers {!Dir_sync}, and a plain POSIX
    directory is opened read-only and fsynced. {!sync_file} is the same for the
    bytes an entry names: a backend registers {!File_sync}, otherwise the
    barrier is {!Eio.File.sync}. {!publish} is the protocol itself, under a
    mandatory byte ceiling ({!type-config}, {!Limit_exceeded}), and issues the
    barriers a {!Barrier.t} names: the portable ones above by default,
    [Eio_fs_unix.barrier] for the platform's strongest.

    {!Process_local} declares whether any opener of a backend's files can exist
    outside this process, and {!Lock} is the exclusion a caller supplies. This
    library does not use [Unix]: the POSIX barrier is in [eio-fs.unix]. *)

(** {1:sync Directory and file barriers} *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync :
      ( 't,
        't -> path:string -> full:bool -> unit,
        [> `Dir_sync ] )
      Eio.Resource.pi
        (** The operation a directory-capable backend registers in its resource
            handler: make the entries of the directory at [path] (relative to
            the resource, as for every DIR operation) durable. [full] asks for
            them to go through the drive's cache too, as [F_FULLFSYNC] does on
            Darwin; {!sync_dir} asks [false], [Eio_fs_unix.sync_dir] [true]. *)

val dir_barrier : 'a Eio.Resource.t -> (path:string -> full:bool -> unit) option
(** [dir_barrier res] is [Some b] with [b] the directory barrier [res] registers
    through {!Dir_sync}, and [None] if it registers none. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place. A backend that registers
    {!Dir_sync} answers through it; otherwise [dir] is opened read-only and its
    handle fsynced. [Eio_fs_unix.sync_dir] upgrades that to platter strength for
    programs that link unix.

    A backend that registers no barrier and whose directory handle has no sync
    operation is a wiring error, which [sync_dir] reports rather than skip the
    barrier.

    @raise Stdlib.Invalid_argument
      if [dir]'s backend registers no {!Dir_sync} and its directory handle
      cannot sync.
    @raise Eio.Io
      if the directory cannot be opened, or as the barrier or the sync raises.
*)

val sync_parent : _ Eio.Path.t -> unit
(** [sync_parent p] makes the directory holding [p]'s entry durable. The parent
    comes from {!Eio.Path.split}, so it keeps [p]'s directory capability; a path
    with no parent is the root of that capability. It raises as {!sync_dir}
    does, on the directory the barrier lands on. *)

type (_, _, _) Eio.Resource.pi +=
  | File_sync : ('t, 't -> full:bool -> unit, [> `File_sync ]) Eio.Resource.pi
        (** The operation a file-capable backend registers in its resource
            handler: make this file's data durable, through the drive's cache
            too when [full], as {!Dir_sync}. A file is itself the resource, so
            the operation carries no name, where {!Dir_sync} takes the [~path] a
            directory resource is joined with. *)

val file_barrier : 'a Eio.Resource.t -> (full:bool -> unit) option
(** [file_barrier res] is [Some b] with [b] the file barrier [res] registers
    through {!File_sync}, and [None] if it registers none. *)

val sync_file : _ Eio.File.rw -> unit
(** [sync_file file] makes [file]'s data durable: the barrier a commit protocol
    issues over the bytes it wrote. A backend that registers {!File_sync}
    answers through it, whole and never alongside the file's own sync; otherwise
    the barrier is {!Eio.File.sync}. [Eio_fs_unix.sync_file] upgrades that to
    platter strength for programs that link unix.

    @raise Eio.Io as {!Eio.File.sync}, or as the registered barrier raises. *)

(** {1:declarations Backend declarations} *)

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

(** {1:barriers Barrier pairs} *)

(** The barriers a publication issues: one over a file's bytes, one over a
    directory's entries. *)
module Barrier : sig
  type t
  (** The type for barrier pairs. *)

  (** Barrier implementations. *)
  module type S = sig
    type t
    (** The type for an implementation's state. *)

    val sync_file : t -> _ Eio.File.ro -> unit
    (** [sync_file t file] makes [file]'s data durable. [file] may be open for
        reading only. *)

    val sync_dir : t -> _ Eio.Path.t -> unit
    (** [sync_dir t dir] makes [dir]'s entries durable. *)
  end

  (** [Make (B)] packs a [B.t] into a {!t}. *)
  module Make (B : S) : sig
    val v : B.t -> t
    (** [v b] is the pair [B.sync_file b] and [B.sync_dir b]. *)
  end

  val portable : t
  (** [portable] is {!sync_file}, on a file open for reading through the write
      side the POSIX backends give every handle, and {!sync_dir}. On Darwin both
      are [fsync(2)], which reaches the drive but not through its cache. *)

  val sync_file : t -> _ Eio.File.ro -> unit
  (** [sync_file t file] is [t]'s barrier over [file]'s data. *)

  val sync_dir : t -> _ Eio.Path.t -> unit
  (** [sync_dir t dir] is [t]'s barrier over [dir]'s entries. *)
end

val mkdirs :
  ?exists_ok:bool ->
  ?barrier:Barrier.t ->
  perm:Eio.File.Unix_perm.t ->
  _ Eio.Path.t ->
  unit
(** [mkdirs ~exists_ok ~barrier ~perm p] creates [p] and its missing ancestors
    with mode [perm], establishing the durability of every traversed directory
    entry, parent first, with [barrier]'s directory barrier (defaults to
    {!Barrier.portable}, whose barrier is {!sync_parent}). With [exists_ok]
    (defaults to [false]) [p] may already be a directory; an ancestor always
    may. Existing ancestors are barriered too: they may be visible after a
    previous writer's barrier failed. An accepted existing target is also
    barriered, so retry can establish its durability.

    The directory capability root must itself already be durable; this operation
    never acquires or barriers its parent outside the capability. Existing
    permissions are unchanged. A file is never accepted as a directory.

    The number of directory barriers is linear in the traversed path depth. A
    failure can leave created directories visible with uncertain durability; a
    successful retry with [exists_ok] reissues the required barriers.

    @raise Eio.Io
      with {!Eio.Fs.Already_exists} if [p] exists and [exists_ok] is [false], or
      if [p] or an ancestor is not a directory; otherwise as {!Eio.Path.mkdir}
      and the barrier raise. *)

(** {1:publication Publication} *)

type config
(** The type for publication policies. A finite byte ceiling is mandatory:
    streaming avoids buffering a whole value, but it must not turn an allocation
    failure into unbounded disk consumption. *)

val config :
  ?durable:bool ->
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  max_bytes:int64 ->
  unit ->
  config
(** [config ~durable ~barrier ~perm ~max_bytes ()] is the policy such that:
    - [durable] (defaults to [true]) pushes the temporary and its directory to
      stable storage with [barrier]'s barriers (defaults to
      {!Barrier.portable}); [false] keeps only the atomicity, and [barrier] is
      not used.
    - [perm] (defaults to [0o644]) is the mode of the temporary, and so of the
      published file: exactly this, whatever the process umask. A {!Modeless}
      backend keeps no mode, so there the file is published without one.
    - [max_bytes] is the ceiling on the bytes the producer writes, or on the
      logical size {!publish_sparse} publishes.

    @raise Stdlib.Invalid_argument if [max_bytes] is negative. *)

exception Limit_exceeded of { max_bytes : int64; attempted : int64 }
(** Raised before a write that would exceed a {!type-config}'s [max_bytes], with
    that ceiling and the byte count the write would have reached (saturating at
    [Int64.max_int]); nothing of that write reaches the file. *)

val fresh_name : string -> string
(** [fresh_name stem] is [stem ^ ".tmp." ^ salt ^ "." ^ n], with [salt] a random
    number drawn once per process and [n] a counter shared by every fiber and
    domain, so no two calls in one process answer the same string, and two
    processes do only if they draw the same salt. It is exported for a store
    whose target name is unknown until its bytes are read. The name is not
    itself the exclusion: open it with [`Exclusive]. *)

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
    atomically and durably, and is what [write] returns: [write] streams into a
    temporary, which is fsynced, moved to [path], and whose directory is
    barriered as {!sync_parent} barriers it. The sink [write] is given refuses,
    with {!Limit_exceeded}, a write that would take it past [config]'s
    [max_bytes].

    With [mode] [`Exclusive] (it defaults to [`Replace]), an existing target is
    refused atomically, using [rename] (defaults to {!Rename.capability}) for
    directory backends without a registered operation; POSIX callers use the
    backend provided by [Eio_fs_unix.exclusive_rename].

    An omitted [tmp] is a sibling name, opened exclusively: [path]'s name with
    [".tmp.0"] appended, then [".tmp.1"] and so on while the name is taken, so
    one workload names the same temporaries in every run. Past 64 taken names
    the rest come from {!fresh_name}, up to 128 names in all. An explicit one is
    also exclusive, must not exist, must differ from [path] and live on the same
    filesystem, and is never drawn again: a taken name raises [Already_exists].
    A process killed between the create and the rename leaves the temporary in
    place, so a caller naming [tmp] owns every way that name can already be
    taken. On a failure of [write], the sync or the rename, the temporary is
    removed and [path] stays intact; a barrier failure is reported after the
    rename, the new file visible with uncertain durability. A [config] that is
    not durable keeps only the atomicity.

    {b Note.} The barriers are [config]'s. {!Barrier.portable}'s are [fsync(2)],
    which on Darwin stops at the drive's cache: a power cut can lose a
    publication it reported durable. A program that links unix passes
    [Eio_fs_unix.barrier], whose file barrier is [F_FULLFSYNC] there.

    @raise Stdlib.Invalid_argument
      if [tmp] is [path] (the same directory and name, or the same file once
      created), or, in [`Exclusive] mode, if [path]'s directory has no exclusive
      rename of its own and [rename] is {!Rename.capability}.
    @raise Limit_exceeded if [write] lets the exception from its sink escape.
    @raise Eio.Io
      with {!Eio.Fs.Already_exists} if an explicit [tmp] exists, if 128 fresh
      names in a row are taken, or, in [`Exclusive] mode, if [path] exists;
      otherwise as the filesystem operations raise. What [write] raises is
      propagated. *)

val publish_sparse :
  config:config ->
  ?mode:[ `Replace | `Exclusive ] ->
  ?rename:Rename.t ->
  ?tmp:'a Eio.Path.t ->
  size_bytes:int64 ->
  'a Eio.Path.t ->
  (int64 * Cstruct.t) list ->
  unit
(** [publish_sparse ~config ~mode ~rename ~tmp ~size_bytes path spans] publishes
    a file with logical size [size_bytes] and ordered, disjoint [(offset, data)]
    extents. Unwritten gaps read as zeros; only the supplied data is written.
    The caller must retain the buffers without mutating them until the call
    returns.

    The temporary, modes, barriers, exclusive rename and failure cleanup are
    those of {!publish}. The logical size must fit the file-offset type and
    [config]'s [max_bytes]; every extent must lie within that size. All bounds
    and ordering are checked before any filesystem operation.

    @raise Limit_exceeded if [size_bytes] exceeds [config]'s [max_bytes].
    @raise Stdlib.Invalid_argument
      for a negative size or one past the largest file offset, negative offsets,
      unordered or overlapping extents, or data outside the logical file; and as
      {!publish} raises it for [tmp] and [rename].
    @raise Eio.Io as {!publish} raises it. *)

val save :
  ?durable:bool ->
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  _ Eio.Path.t ->
  string ->
  unit
(** [save ~durable ~barrier ~perm path s] is {!publish} of the string [s] at
    [path] in [`Replace] mode, with
    [config ~durable ~barrier ~perm ~max_bytes ()] and [max_bytes] the length of
    [s]: [path] holds [s] whole or what it held before, never part of [s].

    @raise Eio.Io as {!publish} raises it. *)

val append_lines :
  ?durable:bool ->
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  _ Eio.Path.t ->
  string list ->
  unit
(** [append_lines ~durable ~barrier ~perm path lines] appends each of [lines] to
    the file at [path], each ended by LF, in one write, as a log of JSON Lines
    or any other line-per-record file is appended to. A missing file is created
    with mode [perm] (defaults to [0o644]). With [lines] empty, a missing file
    is not created and a file's only change is the LF that ends a torn last
    line.

    A file whose last byte is not LF ends in a line torn by a crash part way
    through an append. That line is ended first, so the first of [lines] starts
    a line of its own: the torn bytes stay, as a line a reader refuses as it
    refuses any malformed one, and no line appended after them is read as their
    continuation. A file that is empty or ends in LF is appended to as it is.

    With [durable] (defaults to [true]), the file is barriered after the write
    with [barrier] (defaults to {!Barrier.portable}), then the directory holding
    it, on every call: a directory barrier that failed on the call that created
    the file is issued again by the next one. Appends from two writers are not
    ordered with each other: a caller with several takes a {!Lock} around each.

    @raise Stdlib.Invalid_argument
      if a line holds LF, before anything is written.
    @raise Eio.Io as the open, the write and the barriers raise. *)

val publish_generation :
  ?durable:bool ->
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  'a Eio.Path.t ->
  ('a Eio.Path.t -> 'b) ->
  'b
(** [publish_generation ~durable ~barrier path populate] publishes a directory
    tree at [path] whole, and is what [populate] returns: [populate] fills a
    fresh sibling generation directory (mode [perm], defaults to [0o755]), and a
    symbolic link to it is renamed over [path]. rename(2) keeps [path] naming
    the old link or the new one throughout, so a reader resolving [path] sees
    one complete generation or the other and never a mixture.

    With [durable] (defaults to [true]), every file and directory in the
    generation is barriered with [barrier] (defaults to {!Barrier.portable})
    before the switch and [path]'s directory after it, so after a crash [path]
    resolves to the previous generation or the new one, never to neither; the
    previous generation is removed only once the switch is committed. [durable]
    [false] keeps only the atomicity.

    Reclaiming an owned old or failed staged generation grants its directories
    owner read, write and search permission when necessary. Files and symbolic
    link targets are not changed. A failure before the switch never changes the
    previous generation's permissions.

    [path] must be absent or a symbolic link, and must have a parent in its
    capability: anything else raises [Invalid_argument] before [populate] runs,
    and is left as it was. A generation is a hidden sibling, a dot, [path]'s
    basename, [".gen."] and a fresh suffix ({!is_generation}); the one the old
    link named is reclaimed only when it is a generation of the same basename.
    One writer publishes a given [path] at a time; the exclusion is the caller's
    ({!Lock}).

    On a failure of [populate], a sync or the switch, the staged generation is
    removed and [path] is unchanged. A barrier failure is reported after the
    switch, the new generation visible with uncertain durability and the
    previous one retained. A failure to reclaim the previous generation is
    raised after the switch is committed.

    @raise Eio.Io
      as the filesystem operations raise. What [populate] raises is propagated.
*)

val is_generation : string -> bool
(** [is_generation name] is [true] iff [name] has the form of a generation
    {!publish_generation} stages beside its pointer: a dot, a non-empty
    basename, [".gen."], then two dot-separated runs of decimal digits. A reader
    listing that directory tells the pointers from their generations by it. *)

module Lock = Lock
(** The critical section a caller supplies: what keeps two writers out of one
    another's read-modify-write, named by the deployment rather than the store.
*)

val publish_link :
  ?durable:bool ->
  ?barrier:Barrier.t ->
  lock:Lock.t ->
  expected:string option ->
  link_to:string ->
  _ Eio.Path.t ->
  (unit, [ `Changed of string option | `Invalid of string ]) result
(** [publish_link ~durable ~barrier ~lock ~expected ~link_to path] replaces the
    entry at [path] with a symbolic link holding exactly [link_to], under
    [lock]. The entry must hold what [expected] says, read without following it:
    [None] is no entry, [Some raw] a symbolic link whose target is the bytes
    [raw]. Anything else is [`Changed actual], and an entry that is not a
    symbolic link is [`Invalid]; neither is replaced. A link that already holds
    [link_to] is left as it is.

    The link is made under a fresh sibling name ({!fresh_name}), the entry at
    [path] is read again, and the link is renamed over it; rename(2) keeps
    [path] naming the old entry or the new link throughout. Targets are raw
    bytes, written and compared as given and never resolved or normalised; an
    empty target or one holding a NUL byte is [`Invalid].

    With [durable] (defaults to [true]), the parent directory is barriered with
    [barrier] (defaults to {!Barrier.portable}) before the rename and after it,
    so after a crash [path] holds the old entry or the new link. A barrier that
    fails after the rename raises, the new link visible with uncertain
    durability; a later call with the same [link_to] issues the barrier again.
    [durable] [false] keeps only the atomicity.

    [lock] is the exclusion against other writers of [path], which a writer that
    ignores it can still race between the last read and the rename.

    @raise Lock.Unavailable when [lock] refuses, before anything is written.
    @raise Eio.Io as the filesystem operations and barriers raise. *)
