(** Sequences of filesystem operations and their outcomes, run through
    {!Eio.Path} on any backend: eio_posix to record a kernel, the in-memory
    filesystem to replay it. *)

type ids = { gid : int; other_gid : int }
(** The type for the groups an outcome names: the user's primary group, and a
    supplementary group the user is in. *)

val scenarios : (string * string list) list
(** [scenarios] is each scenario's name and operations. *)

val run : ids -> Eio.Fs.dir_ty Eio.Path.t -> string list -> string list
(** [run ids dir ops] runs [ops] in the empty directory [dir], and is each
    operation's outcome. *)

val key : string list -> string
(** [key ops] is how a trace spells [ops]: escaped, separated by [";"]. *)

val run_unix : ids -> string -> string list -> string list
(** [run_unix ids dir ops] is {!run} through [Unix], straight to the kernel,
    with [dir] the directory's path. An operation [Unix] cannot make has the
    outcome ["-"]. *)
