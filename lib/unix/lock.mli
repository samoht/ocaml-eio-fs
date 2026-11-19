(** Record locks, lock files, held locks, leases and lock sections, which
    {!Eio_fs_unix} includes and documents, and the calls the rest of the library
    shares with them. *)

val is_darwin : unit -> bool
(** [is_darwin ()] is [true] iff the host is macOS. *)

val eio_unix_errors : (unit -> 'a) -> 'a
(** [eio_unix_errors fn] is [fn ()], with a [Unix.Unix_error] it raises reported
    as [Eio.Io], as Eio's own backends report it. *)

val retry_eintr : ('a -> unit) -> 'a -> unit
(** [retry_eintr f fd] is [f fd], made again while it fails with [EINTR]. *)

val file_fd : _ Eio.Resource.t -> Eio_unix.Fd.t option
(** [file_fd file] is the host descriptor beneath [file], through any wrapper of
    this package in front of it. *)

val lock_range :
  Unix.file_descr ->
  [ `Read | `Write | `Unlock ] ->
  file_offset:int ->
  length:int ->
  bool
(** See {!Eio_fs_unix.lock_range}. *)

val range_holder :
  Unix.file_descr ->
  file_offset:int ->
  length:int ->
  [ `Free | `Held of int option ]
(** See {!Eio_fs_unix.range_holder}. *)

val await_record_lock :
  ?mode:[ `Shared | `Exclusive ] -> Unix.file_descr -> unit
(** See {!Eio_fs_unix.await_record_lock}. *)

type block = [ `Yes | `No | `Fibers | `For of float ]
(** See {!Eio_fs_unix.block}. *)

type create = [ `Never | `If_missing of Eio.File.Unix_perm.t ]
(** See {!Eio_fs_unix.create}. *)

type refusal = { path : string; holder : int option; contents : string }
(** See {!Eio_fs_unix.refusal}. *)

exception Lock_unavailable of refusal
(** See {!Eio_fs_unix.Lock_unavailable}. *)

val pp_refusal : refusal Fmt.t
(** See {!Eio_fs_unix.pp_refusal}. *)

type Eio.Exn.Backend.t +=
  | Not_a_lock_file of string  (** See {!Eio_fs_unix.Not_a_lock_file}. *)

val with_lock :
  ?block:block ->
  ?create:create ->
  ?contents:string ->
  ?on_wait:(unit -> unit) ->
  _ Eio.Path.t ->
  (unit -> 'a) ->
  'a
(** See {!Eio_fs_unix.with_lock}. *)

val probe : _ Eio.Path.t -> [ `Free of string | `Held of refusal ]
(** See {!Eio_fs_unix.probe}. *)

module Held : sig
  type t
  (** See {!Eio_fs_unix.Held.t}. *)

  val take :
    sw:Eio.Switch.t ->
    ?block:block ->
    ?create:create ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    t
  (** See {!Eio_fs_unix.Held.take}. *)

  val restate : t -> string -> unit
  (** See {!Eio_fs_unix.Held.restate}. *)

  val release : ?contents:string -> t -> unit
  (** See {!Eio_fs_unix.Held.release}. *)
end

module Lease : sig
  type t
  (** See {!Eio_fs_unix.Lease.t}. *)

  val fd : t -> Eio_unix.Fd.t
  (** See {!Eio_fs_unix.Lease.fd}. *)

  val unlock : t -> unit
  (** See {!Eio_fs_unix.Lease.unlock}. *)

  val take :
    sw:Eio.Switch.t ->
    ?gate:_ Eio.Path.t ->
    ?block:[ block | `Readers ] ->
    ?create:create ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    t
  (** See {!Eio_fs_unix.Lease.take}. *)

  val release : t -> unit
  (** See {!Eio_fs_unix.Lease.release}. *)

  val with_exclusive :
    ?gate:_ Eio.Path.t ->
    ?block:[ block | `Readers ] ->
    ?create:create ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    (t -> 'a) ->
    'a
  (** See {!Eio_fs_unix.Lease.with_exclusive}. *)

  val with_shared :
    ?block:block ->
    ?create:create ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    (t -> 'a) ->
    'a
  (** See {!Eio_fs_unix.Lease.with_shared}. *)

  val await_free : ?on_wait:(unit -> unit) -> _ Eio.Path.t -> unit
  (** See {!Eio_fs_unix.Lease.await_free}. *)
end

val lock_section :
  ?block:block ->
  ?create:create ->
  ?contents:string ->
  _ Eio.Path.t ->
  Eio_fs_sync.Lock.t
(** See {!Eio_fs_unix.lock_section}. *)
