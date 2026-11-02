(** Atomic exclusive rename, dispatched through directory capabilities. *)

type path = Eio.Fs.dir_ty Eio.Path.t
(** The type for paths, carrying the directory capability they are resolved
    through. *)

type t
(** The type for exclusive-rename backends, which move an entry of a directory
    that registers no {!Exclusive} operation. *)

(** Exclusive-rename implementations. *)
module type S = sig
  type t
  (** The type for an implementation's state. *)

  val rename : t -> path -> path -> unit
  (** [rename t source target] atomically moves [source] to an absent [target].
      An existing target, including a symbolic link, must be refused. *)
end

(** [Make (B)] packs a [B.t] into a {!t}. *)
module Make (B : S) : sig
  val v : B.t -> t
  (** [v b] is the backend that moves through [B.rename b]. *)
end

val capability : t
(** [capability] is the backend that refuses: {!exclusive} with it moves only
    through a directory's registered {!Exclusive} operation, and raises
    [Invalid_argument] for a directory that registers none. *)

val exclusive : t -> _ Eio.Path.t -> _ Eio.Path.t -> unit
(** [exclusive backend source target] moves [source] atomically without
    replacing [target]: through the {!Exclusive} operation [source]'s directory
    registers, and through [backend] when it registers none. Directory wrappers
    register one and are consulted before [backend], so the move is visible to
    their operation and durability models.

    @raise Eio.Io
      as the operation or [backend] refuses an existing [target]: with
      {!Eio.Fs.Already_exists} for this package's in-memory filesystem and
      [Eio_fs_unix.exclusive_rename]. *)

type (_, _, _) Eio.Resource.pi +=
  | Exclusive :
      ( 'a,
        'a -> t -> source:string -> target:path -> unit,
        [> `Exclusive_rename ] )
      Eio.Resource.pi
        (** A directory's exclusive-rename operation. A wrapper delegates
            through {!exclusive} with the supplied backend and its underlying
            paths. *)
