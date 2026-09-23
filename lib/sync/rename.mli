(** Atomic exclusive rename, dispatched through directory capabilities. *)

type path = Eio.Fs.dir_ty Eio.Path.t
(** A path carrying the directory authority used for resolution. *)

type t
(** An exclusive-rename backend for directories without a registered operation.
*)

module type S = sig
  type t

  val rename : t -> path -> path -> unit
  (** [rename t source target] atomically moves [source] to an absent [target].
      An existing target, including a symbolic link, must be refused. *)
end

module Make (B : S) : sig
  val v : B.t -> t
  (** [v backend] dispatches unregistered directories through [backend]. *)
end

val capability : t
(** [capability] dispatches only through registered directory operations and
    refuses an unregistered backend. *)

val exclusive : t -> _ Eio.Path.t -> _ Eio.Path.t -> unit
(** [exclusive backend source target] moves [source] atomically without
    replacing [target]. Directory wrappers are consulted before [backend], so
    the move is visible to their operation and durability models. *)

type (_, _, _) Eio.Resource.pi +=
  | Exclusive :
      ( 'a,
        'a -> t -> source:string -> target:path -> unit,
        [> `Exclusive_rename ] )
      Eio.Resource.pi
        (** A directory's exclusive-rename operation. A wrapper delegates
            through {!exclusive} with the supplied backend and its underlying
            paths. *)
