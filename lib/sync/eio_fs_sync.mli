(** The barriers and the atomic publications built on them, portable across
    backends.

    A barrier is a sync that returns once what it covers is durable, so that a
    power cut after it loses none of it: a file barrier covers a file's data,
    and a directory barrier the entries of a directory. [Eio.Path.sync_dir]
    issues the barrier a write-temporary-then-rename protocol needs. A backend
    registers {!Dir_sync} for a directory barrier that may also go through the
    drive's cache, and {!File_sync} for a file barrier. A {!Barrier.t} is the
    pair of barriers a protocol issues: {!Barrier.portable} by default,
    [Eio_fs_unix.barrier] for the platform's strongest, {!Barrier.none} for
    none. By hand, {!Eio.File.sync} and {!Eio.Path.sync_dir} issue the portable
    barriers, and [Eio_fs_unix.sync_file] and [Eio_fs_unix.sync_dir] the
    platform's strongest. {!publish} is the protocol itself, under a mandatory
    byte ceiling ({!Limit_exceeded}). A publication is atomic on any backend,
    and durable with the barriers it is given: on macOS the portable ones stop
    in the drive's cache, and only [Eio_fs_unix.barrier] goes through it.

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
            Darwin; [Eio_fs_unix.sync_dir] asks [true], and the backend's own
            [Eio.Path.sync_dir] is the barrier with [false]. *)

val dir_barrier : 'a Eio.Resource.t -> (path:string -> full:bool -> unit) option
(** [dir_barrier res] is [Some b] with [b] the directory barrier [res] registers
    through {!Dir_sync}, and [None] if it registers none. *)

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
(** [is_modeless res] is what [res] declares through {!Modeless}, and when it
    declares nothing, whether the host is Windows: there the native backend,
    eio_windows, keeps no Unix mode (a file's reads [0o666] and a chmod below an
    opened directory is refused), and it has no way to say so. *)

type (_, _, _) Eio.Resource.pi +=
  | Adopt :
      ( 't,
        't ->
        sw:Eio.Switch.t ->
        path:string ->
        Eio.File.rw_ty Eio.Resource.t ->
        Eio.File.rw_ty Eio.Resource.t,
        [> `Adopt ] )
      Eio.Resource.pi
        (** The operation a directory wrapper registers: hand out a file of its
            base at [path] (relative to the resource, as for every directory
            operation), wrapped as an open of [path] through the wrapper wraps
            the file it opens, its release tied to [sw], and refused where that
            open would be. A wrapper adopts through its base first, so stacked
            wrappers each apply. *)

val adopt :
  sw:Eio.Switch.t ->
  _ Eio.Path.t ->
  Eio.File.rw_ty Eio.Resource.t ->
  Eio.File.rw_ty Eio.Resource.t
(** [adopt ~sw (dir, path) file] is [file], a descriptor the caller holds of the
    backend under [dir] on the file at [path], with the wrappers [dir] stacks
    over its backend applied, as an open of [path] through [dir] under [sw]
    would apply them, without opening the file again: a store that keeps one
    descriptor per file and process reads and writes it through each caller's
    directory. A directory that registers no {!Adopt} is the backend's own, and
    [file] is handed out as it is. Closing the result closes [file].

    @raise Eio.Io where a wrapper of [dir] refuses the open of [path]. *)

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
  (** [portable] is the barrier a backend registers through {!File_sync},
      otherwise the file's own {!Eio.File.sync}, reached on a file open for
      reading through the write side the POSIX backends give every handle; and
      [Eio.Path.sync_dir]. On Darwin both are [fsync(2)], which reaches the
      drive but not through its cache: [Eio_fs_unix.barrier] goes through it. *)

  val none : t
  (** [none] issues no barrier: a publication keeps only its atomicity. *)
end

(** {1:dirs Directories} *)

val mkdirs :
  ?exists_ok:bool ->
  ?barrier:Barrier.t ->
  perm:Eio.File.Unix_perm.t ->
  _ Eio.Path.t ->
  unit
(** [mkdirs ~exists_ok ~barrier ~perm p] is like {!Eio.Path.mkdirs} but makes
    each directory durable: it creates [p] and its missing ancestors with mode
    [perm] and issues [barrier]'s directory barrier (defaults to
    {!Barrier.portable}) on the parent of each, existing ones included, parent
    first: a writer whose barrier failed may have left an existing directory
    visible but not durable. With [exists_ok] (defaults to [false]) [p] may
    already be a directory; an ancestor always may.

    The root of [p]'s capability must already be durable: [mkdirs] issues no
    barrier outside it. Existing permissions are unchanged. A file is never
    accepted as a directory.

    A failure can leave created directories visible but not durable; a retry
    with [exists_ok] issues the barriers again.

    @raise Eio.Io
      with {!Eio.Fs.Already_exists} if [p] exists and [exists_ok] is [false], or
      if [p] or an ancestor is not a directory; otherwise as {!Eio.Path.mkdir}
      and the barrier raise. *)

(** {1:publication Publication} *)

exception Limit_exceeded of { max_bytes : int64; attempted : int64 }
(** Raised before a write that would exceed a publication's [max_bytes], with
    that ceiling and the byte count the write would have reached (saturating at
    [Int64.max_int]); nothing of that write reaches the file. *)

val fresh_name : string -> string
(** [fresh_name name] is [stem ^ ".tmp." ^ salt ^ "." ^ n], with [salt] a random
    number drawn once per process and [n] a counter shared by every fiber and
    domain, so no two calls in one process answer the same string, and two
    processes do only if they draw the same salt. [stem] is [name] when it holds
    at most 200 bytes; a longer one is cut at a character boundary and given a
    digest of the whole, so the result is a valid file name whenever [name] is.
    Of a relative path, only the last component is cut, so the result stays in
    that path's directory. The name is not itself the exclusion: open it with
    [`Exclusive]. *)

module Rename = Rename
(** Capability-dispatched exclusive rename and backend construction. *)

val publish :
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  ?exclusive:Rename.t ->
  ?tmp:'a Eio.Path.t ->
  max_bytes:int64 ->
  'a Eio.Path.t ->
  (Eio.Flow.sink_ty Eio.Resource.t -> 'b) ->
  'b
(** [publish ~barrier ~perm ~exclusive ~tmp ~max_bytes path write] publishes
    [path]'s contents atomically, and durably as far as [barrier] goes (see the
    preamble for macOS), and is what [write] returns: [write] streams into a
    temporary, which is synced, moved to [path], and whose directory is synced.
    The sink [write] is given refuses, with {!Limit_exceeded}, a write that
    would take it past [max_bytes]: a finite ceiling is mandatory, so that
    streaming cannot turn an allocation failure into unbounded disk consumption.

    [barrier] (defaults to {!Barrier.portable}) issues both barriers;
    {!Barrier.none} keeps only the atomicity. [perm] (defaults to [0o644]) is
    the mode of the temporary, and so of the published file: exactly this,
    whatever the process umask. A backend that keeps no mode ({!is_modeless}),
    Windows' among them, publishes the file without one.

    Without [exclusive], [path] is replaced. With it, an existing target is
    refused atomically, through the {!Rename.Exclusive} operation [path]'s
    directory registers, or through the backend [exclusive] when it registers
    none: {!Rename.capability} for this package's in-memory filesystem and its
    wrappers, [Eio_fs_unix.exclusive_rename] on POSIX.

    An omitted [tmp] is a sibling name, opened exclusively: [path]'s name, cut
    as {!fresh_name} cuts a long one, with [".tmp.0"] appended, then [".tmp.1"]
    and so on while the name is taken, so one program names the same temporaries
    in every run. Past 64 taken names the rest come from {!fresh_name}, up to
    128 names in all. An explicit one is also exclusive, must not exist, must
    differ from [path] and live on the same filesystem, and is never drawn
    again: a taken name raises [Already_exists]. A process killed between the
    create and the rename leaves the temporary in place, so a caller naming
    [tmp] owns every way that name can already be taken. On a failure of
    [write], the sync or the rename, the temporary is removed and [path] stays
    intact; a barrier failure is reported after the rename, the new file visible
    with uncertain durability.

    {b Note.} {!Barrier.portable}'s barriers are [fsync(2)], which on Darwin
    stops at the drive's cache: a power cut can lose a publication it reported
    durable. A program that links unix passes [Eio_fs_unix.barrier], whose file
    barrier is [F_FULLFSYNC] there.

    @raise Invalid_argument
      if [max_bytes] is negative, if [tmp] is [path] (the same directory and
      name, or the same file once created), or if [path]'s directory has no
      exclusive rename of its own and [exclusive] is {!Rename.capability}.
    @raise Limit_exceeded if [write] lets the exception from its sink escape.
    @raise Eio.Io
      with {!Eio.Fs.Already_exists} if an explicit [tmp] exists, if 128 fresh
      names in a row are taken, or, with [exclusive], if [path] exists;
      otherwise as the filesystem operations raise. What [write] raises is
      propagated. *)

val publish_sparse :
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  ?exclusive:Rename.t ->
  ?tmp:'a Eio.Path.t ->
  max_bytes:int64 ->
  size_bytes:int64 ->
  'a Eio.Path.t ->
  (int64 * Cstruct.t) list ->
  unit
(** [publish_sparse ~barrier ~perm ~exclusive ~tmp ~max_bytes ~size_bytes path
     spans] publishes a file with logical size [size_bytes] and ordered,
    disjoint [(offset, data)] extents. Unwritten gaps read as zeros; only the
    supplied data is written. The caller must retain the buffers without
    mutating them until the call returns.

    The temporary, modes, barriers, exclusive rename and failure cleanup are
    those of {!publish}. The logical size must fit the file-offset type and
    [max_bytes]; every extent must lie within that size. All bounds and ordering
    are checked before any filesystem operation.

    @raise Limit_exceeded if [size_bytes] exceeds [max_bytes].
    @raise Invalid_argument
      for a negative size or one past the largest file offset, negative offsets,
      unordered or overlapping extents, or data outside the logical file; and as
      {!publish} raises it for [max_bytes], [tmp] and [exclusive].
    @raise Eio.Io as {!publish} raises it. *)

val save :
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  _ Eio.Path.t ->
  string ->
  unit
(** [save ~barrier ~perm path s] is {!publish} of the string [s] at [path],
    replacing it, with [max_bytes] the length of [s]: [path] holds [s] whole or
    what it held before, never part of [s].

    @raise Eio.Io as {!publish} raises it. *)

val append_lines :
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  ?tail:[ `Preserve | `Discard ] ->
  _ Eio.Path.t ->
  string list ->
  unit
(** [append_lines ~barrier ~perm ~tail path lines] appends each of [lines] to
    the file at [path], each ended by LF, in one write. A missing file is
    created with mode [perm] (defaults to [0o644]). With [lines] empty, a
    missing file is not created and a file's only change is to its torn last
    line.

    A file whose last byte is not LF ends in a line torn by a crash part way
    through an append. With [tail] [`Preserve] (the default), that line is ended
    first, so the first of [lines] starts a line of its own: the torn bytes
    stay, as a line a reader refuses as it refuses any malformed one, and no
    line appended after them is read as their continuation. With [`Discard], the
    file is truncated just past its last LF, to empty when it has none, before
    [lines] are appended: the policy of a journal whose LF commits a record.
    Complete lines stay as they are, malformed ones included, and the tail is
    found by reading the file back from its end, 4096 bytes at a time. A file
    that is empty or ends in LF is appended to as it is.

    The file is synced after the write with [barrier] (defaults to
    {!Barrier.portable}), then the directory holding it, on every call, one with
    nothing to write included: a barrier that failed on the call that created
    the file is issued again by the next one, [append_lines path []] being the
    retry that adds no line. A call with [lines] empty on a missing file issues
    no barrier, and with {!Barrier.none} a call with nothing to write or discard
    opens nothing. Appends from two writers are not ordered with each other: a
    caller with several wraps each call in {!Lock.use}.

    @raise Invalid_argument if a line holds LF, before anything is written.
    @raise Eio.Io as the open, the write and the barriers raise. *)

val publish_generation :
  ?barrier:Barrier.t ->
  ?perm:Eio.File.Unix_perm.t ->
  'a Eio.Path.t ->
  ('a Eio.Path.t -> 'b) ->
  'b
(** [publish_generation ~barrier ~perm path populate] publishes a directory tree
    at [path] whole, and is what [populate] returns: [populate] fills a fresh
    sibling generation directory (mode [perm], defaults to [0o755]), and a
    symbolic link to it is renamed over [path]. rename(2) keeps [path] naming
    the old link or the new one throughout, so a reader resolving [path] sees
    one complete generation or the other and never a mixture.

    A reader that resolved [path] sees that generation complete until the second
    publication after it: a publication keeps the generation it replaces and
    reclaims the ones older than that. A reader that needs its generation longer
    holds the {!Lock} the publishers of [path] take.

    Every file and directory in the generation is synced with [barrier]
    (defaults to {!Barrier.portable}) before the switch and [path]'s directory
    after it, so after a crash [path] resolves to the previous generation or the
    new one, never to neither; older generations are removed only once the
    switch is committed. {!Barrier.none} keeps only the atomicity.

    Reclaiming an owned old or failed staged generation grants its directories
    owner read, write and search permission when necessary. Files and symbolic
    link targets are not changed. A failure before the switch never changes the
    previous generation's permissions.

    [path] must be absent or a symbolic link, and must have a parent in its
    capability; otherwise [publish_generation] raises before [populate] runs,
    and [path] is left as it was. A generation is a hidden sibling, a dot,
    [path]'s basename (cut as {!fresh_name} cuts a long one), [".gen."] and a
    fresh suffix ({!is_generation}); only generations of the same basename are
    reclaimed, never a tree a link made by hand names. One writer publishes a
    given [path] at a time: a caller with several wraps each call in
    {!Lock.use}.

    On a failure of [populate], a sync or the switch, the staged generation is
    removed and [path] is unchanged. A barrier failure is reported after the
    switch, the new generation visible with uncertain durability and the
    previous one retained. A failure to reclaim an older generation is raised
    after the switch is committed.

    @raise Invalid_argument
      if [path] is the root of its capability, which has no parent.
    @raise Eio.Io
      with {!Eio.Fs.Already_exists} if [path] is neither absent nor a symbolic
      link; otherwise as the filesystem operations raise. What [populate] raises
      is propagated. *)

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
  ?barrier:Barrier.t ->
  ?replace:string ->
  target:string ->
  _ Eio.Path.t ->
  (unit, [ `Changed of string option | `Invalid of string ]) result
(** [publish_link ~barrier ~replace ~target path] makes the entry at [path] a
    symbolic link holding exactly [target]. Without [replace] there must be no
    entry at [path]; with [replace raw] it must be a symbolic link whose target
    is the bytes [raw], which is replaced. The entry is read without following
    it. Anything else is [`Changed actual], [actual] the target found or [None]
    for no entry, and an entry that is not a symbolic link is [`Invalid];
    neither is replaced. Without [replace], a link that already holds [target]
    is left as it is; with [replace raw], only when [raw] is [target].

    Without [replace], the link is made at [path] by symlink(2), which refuses a
    name taken meanwhile. With it, the link is made under a fresh sibling name
    ({!fresh_name}), the entry at [path] is read again, and the link is renamed
    over it; rename(2) keeps [path] naming the old entry or the new link
    throughout. Targets are raw bytes, written and compared as given and never
    resolved or normalised; an empty target or one holding a NUL byte is
    [`Invalid].

    The parent directory is synced with [barrier] (defaults to
    {!Barrier.portable}) after the link is made, and before a rename too, so
    after a crash [path] holds the old entry or the new link. A barrier that
    fails after the link is visible raises, with uncertain durability; a later
    call with [replace] and [target] both the new target issues the barrier
    again. {!Barrier.none} keeps only the atomicity.

    One writer replaces a given [path] at a time: a caller with several wraps
    each call in {!Lock.use}, and a writer outside it can still race between the
    last read and the rename. Under [Eio_fs.Nofollow.with_parent], no symbolic
    link on the way to [path] redirects the publication.

    @raise Eio.Io as the filesystem operations and barriers raise. *)
