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

    @raise Eio.Io
      if the directory cannot be opened or the sync fails.
    @raise Failure
      if the backend registers no barrier and its handles expose no sync
      operation. *)

val sync_parent : _ Eio.Path.t -> unit
(** [sync_parent p] makes the directory holding [p]'s entry durable. The parent
    comes from {!Eio.Path.split}, so it keeps [p]'s directory capability; a path
    with no parent is the root of that capability.

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

val fresh_name : string -> string
(** [fresh_name stem] is [stem] extended with a per-process salt and a
    fiber-atomic counter, so no other writer produces the same string. It is
    exported for a store whose target name is unknown until its bytes are read.
    The name is not itself the exclusion: open it with [`Exclusive]. *)

val publish :
  config:config ->
  ?tmp:'a Eio.Path.t ->
  'a Eio.Path.t ->
  (Eio.Flow.sink_ty Eio.Resource.t -> 'b) ->
  'b
(** [publish ~config path write] replaces [path]'s contents atomically and
    durably: [write] streams into a temporary, which is fsynced, renamed over
    [path], and whose directory is barriered with {!sync_parent}.

    An omitted [tmp] is a fresh sibling name, opened exclusively; an explicit
    one is also exclusive, must not exist, must differ from [path] and live on
    the same filesystem. On a failure of [write], the sync or the rename, the
    temporary is removed and [path] stays intact; a barrier failure is reported
    after the rename, the new file visible with uncertain durability.
    {!type-config.field-durable} [false] keeps only the atomicity. *)

module Lock = Lock
(** The critical section a caller supplies: what keeps two writers out of one
    another's read-modify-write, named by the deployment rather than the store. *)
