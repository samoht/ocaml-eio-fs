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

val save_durable : create:Eio.Fs.create -> _ Eio.Path.t -> string -> unit
(** [save_durable ~create path content] replaces [path]'s contents with
    [content] atomically and durably: the sibling temporary [<path>.tmp] is
    written and fsynced, renamed over [path] -- a reader or a crash sees the old
    file or the whole new one, never a torn or empty file -- and the directory
    is barriered with {!sync_dir} so a power cut cannot revert the publication.
    The temporary is removed if the write fails.

    [create] is applied to the temporary (its permission becomes the target's);
    [`Or_truncate] is the usual choice. The temporary's name is deterministic,
    so concurrent writers to one target must be serialised by the caller. *)

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
