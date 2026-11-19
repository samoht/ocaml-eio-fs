(** The fault wrapper. {!wrap} wraps a directory, in memory or on disk, and asks
    a {!type-hook} about each read, write, sync, open, stat, listing and
    directory change. A fault is raised as an ordinary {!Eio.Io}, so the code
    under test cannot tell it from a failure of the base. *)

type Eio.Exn.Backend.t +=
  | Injected_fault of string
        (** The code of an injected error: its errno's name and what it names,
            as ["EIO /f"]. *)

type hook = space:bool -> Op.t -> Fault.t option
(** The type for what a wrapper asks about each operation. [space] is whether
    the operation may fail for want of room: a write, an open that makes its
    file, a [mkdir], a [symlink] or a [rename]. *)

type sched
(** The type for schedules: a hook and the numbering of the operations it is
    asked about, which every wrapper given the schedule shares. *)

val schedule : hook -> sched
(** [schedule f] asks [f] about each operation, numbered from [1]. *)

val applies : space:bool -> Op.t -> Fault.t -> bool
(** [applies ~space op f] is [true] iff [f] does something to [op]: a fault of
    another kind of operation does nothing, and neither does an [ENOSPC] on an
    open or a change that makes nothing, as Linux never answers it there. *)

val wrap : sched -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [wrap sched base] is [base] with each operation meeting the fault [sched]'s
    hook answers, when it {!applies}; opened files and subdirectories stay
    wrapped and share [sched].

    The wrapper and every file and subdirectory it opens hold [base] out of
    reach: each is a value of its own bound to the wrapper's interfaces only, so
    code handed the wrapper cannot rebuild a handler that reaches [base] and
    steps around [sched]. It answers {!Eio_fs_sync.Process_local} and
    {!Eio_fs_sync.Modeless} as [base] does, and every opened file, a read-only
    one included, registers {!Eio_fs_sync.File_sync} so a barrier through it
    reaches the wrapper's own sync. It registers {!Eio_fs_sync.Adopt}: a file of
    [base] the caller holds, given to {!Eio_fs_sync.adopt}, is wrapped as one
    opened through it, the hook asked as for that open. A backend's own
    interfaces are not carried over: a wrapper over a host directory is no
    native working directory for a child process, and its files hand out no host
    descriptor, except to [eio-fs.unix], which takes a lock on the descriptor
    beneath. A rename from an in-memory directory of {!Eio_fs_mem} into a
    wrapper over the same tree is the in-memory rename, outside [sched]; one
    from a host directory of another backend into a wrapper is refused by that
    backend, which finds no directory of its own there. *)

(** {1:shared What the crash model shares} *)

(** The type for a directory's interface and value, unpacked once. *)
type base_dir =
  | Base_dir : (module Eio.Fs.Pi.DIR with type t = 'a) * 'a -> base_dir

val raise_io : Fault.code -> string -> 'a
(** [raise_io code ctx] raises the {!Eio.Io} error of [code] about [ctx]. *)

val remove_at : base_dir -> string -> unit
(** [remove_at dir path] frees the name [path] of [dir], whatever holds it: a
    directory is emptied and removed, anything else unlinked. *)

val close_resource : _ Eio.Resource.t -> unit
(** [close_resource r] closes [r], if it can be closed. *)

val write_offset : append:bool -> pos:int ref -> _ Eio.File.ro -> int
(** [write_offset ~append ~pos file] is where a streaming write to [file] lands:
    at [!pos], or at its end if [append]. *)

val do_seek :
  pos:int ref ->
  _ Eio.File.ro ->
  Optint.Int63.t ->
  [ `Set | `Cur | `End ] ->
  Optint.Int63.t
(** [do_seek ~pos file off cmd] seeks [file] and moves [pos] where it lands. *)

val sync_base : full:bool -> _ Eio.File.ro -> unit
(** [sync_base ~full file] is a barrier on [file], through its own barrier or
    the sync its handle exposes, whichever descriptor names it.

    @raise Invalid_argument if [file] can do neither. *)

val rw_boxed :
  ?extra:'s Eio.Resource.binding list ->
  (module Eio.File.Pi.WRITE with type t = 's) ->
  barrier:('s -> full:bool -> unit) ->
  's ->
  base:Eio.File.rw_ty Eio.Std.r ->
  Eio.File.rw_ty Eio.Std.r
(** [rw_boxed (module X) ~barrier state ~base] is a read-write file of its own,
    [state] bound to [X], with [barrier] as its {!Eio_fs_sync.File_sync} and
    [base] reachable through [Eio_fs_internal.Base_file] alone. [extra] are its
    other bindings. *)
