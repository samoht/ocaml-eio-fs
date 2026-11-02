(** What a wrapper of this package was built over, for this package's own
    libraries and tests only.

    A wrapper binds its own interfaces and nothing of its base's, so a caller
    holding it cannot step around it. Two of this package's libraries still need
    the base: {!Eio_fs.Chaos} itself, to hand a rename target that is a wrapper
    to the base backend, and [eio-fs.unix], to reach the host descriptor a lock
    or a native batch is taken on. They reach it through the interfaces below,
    which no library outside the package can name. *)

type (_, _, _) Eio.Resource.pi +=
  | Base_dir :
      ('t, 't -> Eio.Fs.dir_ty Eio.Resource.t, [> `Base_dir ]) Eio.Resource.pi
        (** The directory a directory wrapper wraps. *)
  | Base_file :
      ('t, 't -> Eio.File.ro_ty Eio.Resource.t, [> `Base_file ]) Eio.Resource.pi
        (** The file a file wrapper wraps. *)

val base_dir : _ Eio.Resource.t -> Eio.Fs.dir_ty Eio.Resource.t option
(** [base_dir r] is the directory [r] wraps, or [None] when [r] binds no
    {!Base_dir}. *)

val base_file : _ Eio.Resource.t -> Eio.File.ro_ty Eio.Resource.t option
(** [base_file r] is the file [r] wraps, or [None] when [r] binds no
    {!Base_file}. *)
