(** Filesystem and file-descriptor operations of the in-memory backend.

    This is an internal interface: the functions mirror the POSIX [*at] calls
    over a global descriptor table. *)

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

(** File descriptors. *)
module Fd : sig
  type t
  (** The type for file descriptors in the global table. *)
end

(** Directory descriptors. *)
module Dir : sig
  (** The type for the directory a relative path resolves against: the
      filesystem root, the fiber's current directory, or an open descriptor. *)
  type fd = Fs | Cwd | Dir_fd of Fd.t
end

(** The current directory. *)
module Directory : sig
  val cwd : string Eio.Fiber.key
  (** [cwd] is the fiber-local absolute path that {!Dir.Cwd} resolves to. *)
end

val openat : mode:int -> sw:Eio.Switch.t -> Dir.fd -> string -> int -> Fd.t
(** [openat ~mode ~sw dir path flags] opens [path] relative to [dir] with the
    {!Open_flags} [flags], creating it with permissions [mode]. The descriptor
    is closed when [sw] finishes. *)

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
    (defaults to [true]). *)

val symlinkat : link_to:string -> Dir.fd -> string -> unit
(** [symlinkat ~link_to dir path] creates the symbolic link [path] pointing to
    [link_to]. *)

module Flow : sig
  val of_fd :
    Fd.t ->
    [ [ `Generic | `Unix ] Eio.Net.stream_socket_ty | Eio.File.rw_ty ]
    Eio.Resource.t
  (** [of_fd fd] is [fd] as an Eio flow and file resource. *)
end

val reset : unit -> unit
(** [reset ()] empties the global descriptor table and the filesystem. *)
