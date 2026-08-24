(** The critical section a component runs its file operations inside, supplied
    by whoever deploys it.

    A file-backed read-modify-write is critical over the {e directory} it reads
    and writes, and what exclusion a directory admits is the host's to say
    rather than the component's. On a POSIX host another process can open the
    same files, so the section has to be a record lock over a rendezvous beside
    them ([Eio_fs_unix.with_lock]). Where no process outside this one can open
    them -- a filesystem whose resource declares [Eio_fs_sync.Process_local], a
    unikernel's -- the exclusion between this program's fibers is the whole of
    it, not a weaker stand-in for one.

    This library depends on nothing but eio, so a component that takes its
    section from here cross-compiles to a unikernel and cannot take a record
    lock itself. The section is therefore the caller's to supply, and saying
    which one is saying what the component's files admit. *)

type t
(** A critical-section runner. *)

exception Unavailable of string

val use : t -> (unit -> 'a) -> 'a
(** [use t f] runs [f] inside [t]'s section and is what [f] returns. The section
    is exclusion, not a channel for one kind of answer: one [t] serves sections
    at every result type.

    @raise Unavailable
      when the section could not be entered: the runner refused rather than
      waited, and [f] never ran. The string says what refused it, a rendezvous
      another process holds or a name that is not the lock's own file, and is
      what a component whose own interface promises a result puts in that
      result. A component that cannot name the runner's exceptions -- one
      backend-free enough to cross-compile -- has this one to catch, which is
      why the section rather than the caller's lock declares it. What [f] itself
      raises is not this: it travels out as [f] raised it. *)

val none : t
(** [none] runs a section directly, adding no exclusion of its own.

    It is the whole exclusion only where the files it covers have one writer:
    one handle, in one process, over files no other process can open. Given to a
    component in a directory another process can reach, it leaves that component
    with no exclusion over it -- two writers both read the current state, both
    derive the same next one, and the later write is the only one that survives.
*)

(** The caller's lock, as the section it runs. *)
module type S = sig
  type t
  (** The lock handle. *)

  val use : t -> (unit -> 'a) -> 'a
  (** [use t f] runs [f] under [t] and is what [f] returns, holding [t] for the
      whole call -- [Eio_fs_unix.lock_section] on a host, [Eio.Mutex.use_ro]
      over a mutex the writers share where the fibers of one process are all
      there is.

      A component enters the section once per operation and takes none of its
      own inside it, so an ordinary non-re-entrant lock is enough.

      @raise Unavailable
        rather than any exception of the lock's own when [t] cannot be taken.
        The component on the other side is backend-free by design and has no way
        to name a lock library's exceptions, so a runner over one translates
        them here, at the one place that knows both. Raise it before [f] runs
        and never after: a component reads it as the section refusing, and what
        [f] raised is the component's own to answer. *)
end

(** [Make (L)] packs an [L.t] into a {!t}. *)
module Make (L : S) : sig
  val v : L.t -> t
  (** [v l] is the section run under [l]. *)
end
