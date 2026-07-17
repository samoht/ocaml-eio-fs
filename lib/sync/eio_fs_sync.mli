(** The directory-durability barrier, portable across backends.

    A write-temporary-then-rename commit protocol is durable only once the
    directory entry itself reaches stable storage. {!sync_dir} issues that
    barrier over any backend. A backend with its own barrier semantics registers
    {!Dir_sync} in its resource handler: the in-memory backend a no-op (volatile
    storage has nothing to sync), a crash-model wrapper its journal commit, a
    block-device filesystem its flush. A plain POSIX directory needs no
    registration: it is opened read-only and its handle fsynced. This library
    depends on nothing but eio, so pure Eio adapters that cross-compile to
    unikernels link it freely. *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync : ('t, 't -> path:string -> unit, [> `Dir_sync ]) Eio.Resource.pi
        (** The operation a directory-capable backend registers in its resource
            handler: make the entries of the directory at [path] (relative to
            the resource, as for every DIR operation) durable. *)

val find : 'a Eio.Resource.t -> (path:string -> unit) option
(** [find res] is the barrier [res] registers, if any. *)

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

val publish :
  ?durable:bool ->
  tmp:'a Eio.Path.t ->
  create:Eio.Fs.create ->
  'a Eio.Path.t ->
  string ->
  unit
(** [publish ~tmp ~create path content] replaces [path]'s contents with
    [content] atomically and durably through the caller-chosen temporary [tmp]:
    the temporary is written and fsynced, renamed over [path] -- a reader or a
    crash sees the old file or the whole new one, never a torn or empty file --
    and [path]'s directory is barriered with {!sync_dir} so a power cut cannot
    revert the publication. The temporary is removed if the write or the rename
    fails.

    [tmp] must live on the same filesystem as [path] (rename cannot cross
    devices) but not necessarily in the same directory: a store watched by
    filesystem monitors points it at a quiet directory, and a store with
    concurrent writers to one target gives each writer a unique name. Only
    [path]'s directory is barriered; a stale temporary resurrected by a power
    cut is garbage for the store's usual cleanup, never served state.

    [~durable:false] keeps only the atomicity -- no fsync, no barrier -- for a
    store that deliberately trades the power-cut window for write speed, as git
    does for loose objects; say why at the call site. *)

val save_durable : create:Eio.Fs.create -> _ Eio.Path.t -> string -> unit
(** [save_durable ~create path content] is {!publish} with the sibling temporary
    [<path>.tmp]: the shape for a store whose writers are serialised by its own
    lock, where the deterministic name lets path-triggered crash tests address
    the temporary.

    [create] is applied to the temporary (its permission becomes the target's);
    [`Or_truncate] is the usual choice. *)

val save_durable_with :
  sync_dir:('a Eio.Path.t -> unit) ->
  create:Eio.Fs.create ->
  'a Eio.Path.t ->
  string ->
  unit
(** [save_durable_with ~sync_dir ~create path content] is
    [save_durable ~create path content] with the directory barrier resolved by
    [sync_dir]: the plumbing a platform facade such as [Eio_fs_unix] shares, not
    an entry point for stores. *)
