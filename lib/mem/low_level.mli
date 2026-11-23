(** Filesystem and file-descriptor operations of the in-memory backend.

    This is an internal interface: the functions mirror the POSIX [*at] calls,
    each resolving against the file system its directory belongs to. *)

type Eio.Exn.Backend.t +=
  | Eio_fs of string
        (** The code of the filesystem's errors: the operation, the error and
            the path, as ["rmdir: directory not empty: /d"]. *)

module Error : sig
  val badf : unit -> 'a
  (** [badf ()] raises [Invalid_argument] for a closed descriptor or directory
      capability, as eio_posix does. *)

  val err : string -> 'a
  (** [err msg] raises the backend I/O error [msg], for a failure Eio.Fs has no
      constructor for. *)
end

val name_key : [ `Linux | `Macos | `Windows ] -> string -> string
(** [name_key os name] is what [os] compares [name] by: [name] itself on Linux,
    its canonical caseless form on macOS, its characters' simple uppercase on
    Windows. Two names with one key name one entry. *)

(** Flags for {!openat}. *)
module Open_flags : sig
  val o_rdonly : int
  (** [o_rdonly] opens for reading. *)

  val o_rdwr : int
  (** [o_rdwr] opens for reading and writing. *)

  val o_append : int
  (** [o_append] positions writes at the end of the file. *)

  val o_creat : int
  (** [o_creat] creates the file when it is missing. *)

  val o_directory : int
  (** [o_directory] requires the opened resource to be a directory. *)

  val o_excl : int
  (** [o_excl] fails creation when the path already exists. *)

  val o_nofollow : int
  (** [o_nofollow] refuses a symbolic link at the final path component. *)

  val o_trunc : int
  (** [o_trunc] truncates an existing file. *)

  val empty : int
  (** [empty] is the empty flag set. *)

  val ( + ) : int -> int -> int
  (** [f0 + f1] is the union of the flag sets [f0] and [f1]. *)
end

(** Who runs an operation. *)
module User : sig
  type t = { uid : int64; gid : int64; groups : int64 list; umask : int }
  (** The type for users: their user and group ids, their supplementary groups
      and the umask their new files and directories lose. *)

  val root : t
  (** [root] is uid and gid [0], no supplementary group, umask [0o022]. *)
end

type t
(** The type for in-memory file systems: a tree and the descriptors open on it.
    Two file systems share neither. Each operation below runs whole under a lock
    of its file system, so fibers of several domains may share one. *)

val v :
  ?clock:_ Eio.Time.clock ->
  ?cwd:string ->
  ?owner:User.t ->
  ?os:[< `Linux | `Macos | `Windows ] ->
  ?drives:char list ->
  ?stdin:_ Eio.Flow.source ->
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  unit ->
  t
(** [v ~clock ~cwd ~owner ~os ~drives ()] is a new file system holding only an
    empty root directory, whose current directory is the directory at the
    absolute path [cwd] (defaults to ["/"], or the first drive's root on
    Windows) when first used: it then follows that directory, as a descriptor
    does. Its root directory, mode [0o755], belongs to [owner] (defaults to
    {!User.root}). Its times are read from [clock]; without one, they stay at
    [0.]. It answers as [os], [`Linux] by default. On Windows its root holds a
    directory for each of [drives] (defaults to [['C']]), named ["C:"].

    Pass in a [stdin], [stdout] and [stderr] to connect them to the standard IO.

    @raise Invalid_argument if [drives] is given for another [os]. *)

(** File descriptors. *)
module Fd : sig
  type t
  (** The type for file descriptors, each open on one file system. *)

  val closed : t -> bool
  (** [closed fd] is [true] iff [fd] was closed, by {!close} or by its switch.
  *)
end

(** File descriptor-backed resources. *)
module Resource : sig
  type 'a t = ([> `Mem_fd ] as 'a) Eio.Resource.t
  (** Resources that have FDs are tagged with [`Mem_fd]. *)

  type ('t, _, _) Eio.Resource.pi +=
    | T : ('t, 't -> Fd.t, [> `Mem_fd ]) Eio.Resource.pi

  val fd : _ t -> Fd.t
  (** [fd t] returns the FD being wrapped by a resource. *)

  val fd_opt : _ Eio.Resource.t -> Fd.t option
  (** [fd_opt t] returns the FD being wrapped by a generic resource, if any.

      This just probes [t] using {!extension-T}. *)
end

(** Directory descriptors. *)
module Dir : sig
  (** The type for the directory a relative path resolves against: a file
      system's root, its current directory, or an open descriptor. The root has
      full access. The other two confine a resolution below themselves: an
      absolute path or link target, and a [..] above them, are refused with
      [Eio.Fs.Permission_denied]. *)
  type at = Fs of t | Cwd of t | Dir_fd of Fd.t

  type fd = { at : at; user : User.t; admin : bool }
  (** A directory, the user each operation through it runs as, and whether it is
      the administrator's, whose files take [pread] and [pwrite] even on
      Windows. *)
end

val windows : Dir.fd -> bool
(** [windows dir] is [true] iff the file system of [dir] answers as Windows
    does. *)

val root_path : t -> string
(** [root_path t] is the path a full-access directory of [t] stands at: ["/"],
    or the root of the drive of [t]'s current directory on Windows. *)

val win_path : Fd.t -> string
(** [win_path fd] is the Win32 path of the directory [fd] is open on, as
    ["C:\\a"]. *)

val close : Fd.t -> unit
(** [close fd] releases [fd]. Closing an already closed descriptor does nothing.
*)

val descriptors : Dir.fd -> int
(** [descriptors dir] is how many descriptors are open on the file system of
    [dir]. *)

val openat : mode:int -> sw:Eio.Switch.t -> Dir.fd -> string -> int -> Fd.t
(** [openat ~mode ~sw dir path flags] is a descriptor on [path] relative to
    [dir], opened with the {!Open_flags} [flags]. [mode] is the permissions of a
    file it creates. It is closed when [sw] finishes. *)

val mkdirat : Dir.fd -> perm:int -> string -> unit
(** [mkdirat dir ~perm path] creates the directory [path] relative to [dir]. *)

val embed : Dir.fd -> perm:int -> string -> contents:string -> unit
(** [embed dir ~perm path ~contents] creates the regular file [path] whose
    contents alias the immutable [contents] string: nothing is copied until the
    file is first written to. The parent directory must exist. *)

val unlink : Dir.fd -> string -> unit
(** [unlink dir path] removes the non-directory entry [path]. *)

val rmdir : Dir.fd -> string -> unit
(** [rmdir dir path] removes the empty directory [path]. *)

val statat : follow:bool -> Dir.fd -> string -> Eio.File.Stat.t
(** [statat ~follow dir path] is the metadata of [path], following a final
    symbolic link iff [follow] is [true]. *)

val held : Dir.fd -> string -> Extents.t option
(** [held dir path] is the byte ranges writes covered in the regular file
    [path], following links: the rest of it is a hole. It is [None] if [path] is
    not a regular file. *)

val set_times :
  Dir.fd -> string -> atime:float -> mtime:float -> ctime:float -> unit
(** [set_times dir path ~atime ~mtime ~ctime] sets the times of [path], not
    followed. *)

val read_dir : Dir.fd -> string -> string list
(** [read_dir dir path] is the names in the directory [path], without [.] and
    [..]. *)

val read_dir_entries : Dir.fd -> string -> (Eio.File.Stat.kind * string) list
(** [read_dir_entries dir path] is the kind and name of each entry of the
    directory [path]. *)

val read_link : Dir.fd -> string -> string
(** [read_link dir path] is the target of the symbolic link [path], not
    followed. *)

val chmod : follow:bool -> Dir.fd -> perm:int -> string -> unit
(** [chmod ~follow dir ~perm path] sets the permissions of [path] to [perm],
    following a final symbolic link iff [follow] is [true]. *)

val chown : follow:bool -> ?uid:int64 -> ?gid:int64 -> Dir.fd -> string -> unit
(** [chown ~follow ~uid ~gid dir path] sets the owner and group of [path],
    following a final symbolic link iff [follow] is [true]. An absent [uid] or
    [gid] is left unchanged. *)

val rename : ?replace:bool -> Dir.fd -> string -> Dir.fd -> string -> unit
(** [rename ~replace dir0 path0 dir1 path1] renames [path0] under [dir0] to
    [path1] under [dir1], replacing an existing [path1] iff [replace] is [true]
    (defaults to [true]).

    @raise Eio.Io if [dir0] and [dir1] belong to two file systems. *)

val symlinkat : link_to:string -> Dir.fd -> string -> unit
(** [symlinkat ~link_to dir path] creates the symbolic link [path] pointing to
    [link_to]. *)

val pipe : sw:Eio.Switch.t -> User.t -> t -> Fd.t * Fd.t
(** [pipe ~sw user t] is a new connected pipe, returning the reading and the
    writing end. Both ends of the pipe are closed when the switch finishes. *)

val stdin : User.t -> t -> Fd.t
(** [stdin user t] is the standard input. *)

val stdout : User.t -> t -> Fd.t
(** [stdout user t] is the standard output. *)

val stderr : User.t -> t -> Fd.t
(** [stderr user t] is the standard error. *)

module Flow : sig
  val of_fd : Fd.t -> Eio.File.rw_ty Eio.Resource.t
  (** [of_fd fd] is [fd] as an Eio file resource. *)
end
