(** The critical section a component runs its file operations inside, supplied
    by whoever deploys it. What exclusion a directory admits is the host's to
    say: on a POSIX host another process can open the same files, so the section
    is a record lock ([Eio_fs_unix.with_lock]); where the backend declares
    [Eio_fs_sync.Process_local], the fiber exclusion is the whole of it. *)

type t
(** A critical-section runner. *)

exception Unavailable of string

val use : t -> (unit -> 'a) -> 'a
(** [use t f] runs [f] inside [t]'s section and is what [f] returns. One [t]
    serves sections at every result type.

    @raise Unavailable
      when the section could not be entered: the runner refused rather than
      waited, so [f] never ran. The string says what refused it. What [f] itself
      raises travels out as [f] raised it. *)

val none : t
(** [none] runs a section directly, adding no exclusion. It is the whole
    exclusion only where the files have one writer; in a directory another
    process can reach, two writers both read the same state and the later write
    is the only one that survives. *)

(** The caller's lock, as the section it runs. *)
module type S = sig
  type t
  (** The lock handle. *)

  val use : t -> (unit -> 'a) -> 'a
  (** [use t f] runs [f] under [t], holding [t] for the whole call. A component
      enters the section once per operation and takes none of its own inside it,
      so a non-re-entrant lock is enough.

      @raise Unavailable
        rather than an exception of the lock's own when [t] cannot be taken,
        raised before [f] runs and never after. *)
end

(** [Make (L)] packs an [L.t] into a {!t}. *)
module Make (L : S) : sig
  val v : L.t -> t
  (** [v l] is the section run under [l]. *)
end
