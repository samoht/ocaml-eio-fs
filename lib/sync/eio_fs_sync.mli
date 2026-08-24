(** The directory-durability barrier and the atomic publication built on it,
    portable across backends.

    A write-temporary-then-rename commit protocol is durable only once the
    directory entry itself reaches stable storage. {!sync_dir} issues that
    barrier over any backend. A backend with its own barrier semantics registers
    {!Dir_sync} in its resource handler: the in-memory backend a no-op (volatile
    storage has nothing to sync), a crash-model wrapper its journal commit, a
    block-device filesystem its flush. A plain POSIX directory needs no
    registration: it is opened read-only and its handle fsynced.

    The bytes that entry names have a barrier of their own, {!sync_file}, and it
    dispatches the same way: a backend registers {!File_sync}, a crash-model
    wrapper committing that file's data journal so a model of power loss sees
    the barrier the protocol issued. A plain POSIX file needs no registration
    either, and neither does the in-memory backend, for a reason the directory
    half did not have: a file's own {!Eio.File.sync} already is its barrier,
    where a directory's had to be reached by opening the directory read-only and
    fsyncing that handle. The registration is there so a wrapper can answer for
    what its base would do, not because the fallback is wrong.

    {!publish} is that protocol itself, written once so every store commits the
    same way: contents streamed into a temporary, the temporary synced, renamed
    over the target, and the directory holding the target's entry barriered --
    under a mandatory ceiling on the bytes it will accept ({!type-config},
    {!Limit_exceeded}), so a producer that never ends fills no disk.

    {!Process_local} is the other thing a backend declares here: whether any
    opener of its files can exist outside this process, which is what tells an
    in-process exclusion apart from a stand-in for a real one. This library
    depends on nothing but eio, so pure Eio adapters that cross-compile to
    unikernels link it freely. *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync : ('t, 't -> path:string -> unit, [> `Dir_sync ]) Eio.Resource.pi
        (** The operation a directory-capable backend registers in its resource
            handler: make the entries of the directory at [path] (relative to
            the resource, as for every DIR operation) durable. *)

val dir_barrier : 'a Eio.Resource.t -> (path:string -> unit) option
(** [dir_barrier res] is the directory barrier [res] registers, if any. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place, without which a power cut can
    revert the rename to the prior entry. A backend that registers {!Dir_sync}
    answers through it; otherwise [dir] is opened read-only and its handle
    fsynced, which is the POSIX directory barrier. On Darwin that fsync reaches
    the drive's cache, the same strength Eio gives file data;
    [Eio_fs_unix.sync_dir] upgrades the barrier to platter strength for programs
    that can link unix.

    @raise Eio.Io
      if the directory cannot be opened for its barrier or the sync itself
      fails; the error carries context naming the missing {!Dir_sync}
      registration.
    @raise Failure
      if the backend registers no barrier and its directory handles expose no
      sync operation -- a wiring error, since silence would silently lose
      durability. *)

val sync_parent : _ Eio.Path.t -> unit
(** [sync_parent p] makes the directory that holds [p]'s entry durable: the
    typed answer to "barrier the directory this name lives in", the barrier a
    commit protocol issues after creating or renaming [p]. The parent comes from
    {!Eio.Path.split}, so it keeps the directory capability [p] was expressed
    against, where taking the parent through the path's string half
    ([Filename.dirname]) drops that confinement. A path with no parent is the
    root of the capability itself, and its barrier is {!sync_dir} of that path:
    there is nothing above it a confined path may reach for.

    @raise Eio.Io as {!sync_dir}, on the directory the barrier lands on. *)

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
    issues over the bytes it wrote, before the rename that publishes them.
    Without it a power cut can leave the published entry naming bytes that never
    reached stable storage. A backend that registers {!File_sync} answers
    through it; otherwise the barrier is {!Eio.File.sync}, which is what a plain
    POSIX file's barrier already is. [Eio_fs_unix.sync_file] upgrades that
    fallback to platter strength for programs that can link unix.

    A declared barrier is answered whole, never alongside the file's own sync: a
    backend that registers one owns its semantics, and syncing behind it would
    hand a wrapper that models power loss a barrier it never recorded.

    @raise Eio.Io as {!Eio.File.sync}, or as the registered barrier raises. *)

type (_, _, _) Eio.Resource.pi +=
  | Process_local : ('t, 't -> bool, [> `Process_local ]) Eio.Resource.pi
        (** What a backend declares about the openers of its files: [true] when
            no opener outside this process can reach them, so an exclusion taken
            between the fibers of this process is the whole exclusion those
            files admit. The in-memory backend registers it, its files living in
            the heap with no name to open them by and no descriptor to inherit,
            as does a unikernel filesystem. A wrapper answers for the base it
            was given, which is why the answer is a function of the resource. *)

val is_process_local : _ Eio.Resource.t -> bool
(** [is_process_local res] is what [res] declares through {!Process_local}, and
    [false] for a resource that declares nothing: a caller weighing whether an
    in-process exclusion is the whole exclusion must read silence as "an outside
    opener exists", since the other reading turns a forgotten registration into
    two processes inside one critical section. *)

type config = {
  durable : bool;
      (** Push the temporary and target directory to stable storage. *)
  perm : Eio.File.Unix_perm.t;
      (** Permissions used when creating the temporary, and therefore the
          published file. *)
  max_bytes : int64;  (** Hard ceiling on bytes accepted from the producer. *)
}
(** Publication policy. A finite {!type-config.field-max_bytes} is mandatory:
    streaming avoids buffering a whole value, but it must not turn an allocation
    failure into unbounded disk consumption. *)

exception Limit_exceeded of { max_bytes : int64; attempted : int64 }
(** Raised before a write that would exceed {!type-config.field-max_bytes}. *)

val publish :
  config:config ->
  ?tmp:'a Eio.Path.t ->
  'a Eio.Path.t ->
  (Eio.Flow.sink_ty Eio.Resource.t -> 'b) ->
  'b
(** [publish ~config path write] replaces [path]'s contents atomically and
    durably. [write file] streams the new contents into a temporary Eio file;
    after it returns, the temporary is fsynced, renamed over [path] -- a reader
    or a crash sees the old file or the whole new one, never a torn or empty
    file -- and the directory holding [path]'s entry is barriered with
    {!sync_parent}. The callback's result is returned only after publication
    completes.

    When [tmp] is omitted, a fresh sibling name is opened exclusively. An
    explicit [tmp] is also opened exclusively, must not exist, must be distinct
    from [path], and must live on the same filesystem (rename cannot cross
    devices), but need not share its directory. Only [path]'s directory is
    barriered; a stale temporary resurrected by a power cut is garbage for the
    store's usual cleanup, never served state.

    {!type-config.field-max_bytes} is enforced by the sink before each write. If
    [write], the temporary sync, or the rename fails, the owned temporary is
    removed and the old [path] remains intact. A directory-barrier failure is
    reported after the rename: the new file is visible but its power-cut
    durability is uncertain.

    Setting {!type-config.field-durable} to [false] keeps only the atomicity --
    no fsync, no barrier -- for a store that deliberately trades the power-cut
    window for write speed, as git does for loose objects; say why at the call
    site. *)
