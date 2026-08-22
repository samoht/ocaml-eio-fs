(** Internal filesystem and file-descriptor operations. *)

(** Lexical arithmetic on the paths this backend is addressed by. They are Eio
    paths, whose "directory separator is always "/", even on Windows"
    ({!Eio.Path}), so the host's own path modules answer about a different
    language: this is where the rules for these paths live. *)
module Path : sig
  val basename : string -> string
  (** [basename path] is [path]'s last component, ignoring trailing separators,
      and [""] for a path that has none: a root, [.] or [..]. *)

  val append : string -> string -> string
  (** [append base path] is [path] resolved against [base]: [path] itself when
      it is absolute, and [base] and [path] joined by a single separator
      otherwise. *)
end

module Open_flags : sig
  val o_rdonly : int
  (** Open for reading. *)

  val o_rdwr : int
  (** Open for reading and writing. *)

  val o_append : int
  (** Position writes at the end of the file. *)

  val o_creat : int
  (** Create the file when it is missing. *)

  val o_directory : int
  (** Require the opened resource to be a directory. *)

  val o_excl : int
  (** Fail creation when the path already exists. *)

  val o_trunc : int
  (** Truncate an existing file. *)

  val empty : int
  (** An empty flag set. *)

  val ( + ) : int -> int -> int
  (** Combine two flag sets. *)
end

module Fd : sig
  type t
end

module Dir : sig
  type fd = Fs | Cwd | Dir_fd of Fd.t
end

module Directory : sig
  val cwd : string Eio.Fiber.key
  (** Fiber-local absolute path used to resolve the current-directory
      descriptor. *)
end

val openat : mode:int -> sw:Eio.Switch.t -> Dir.fd -> string -> int -> Fd.t
(** Open a path relative to a directory descriptor. *)

val mkdirat : Dir.fd -> perm:int -> string -> unit
(** Create a directory relative to a directory descriptor. *)

val embed : Dir.fd -> perm:int -> string -> contents:string -> unit
(** [embed dir ~perm path ~contents] creates the regular file [path] whose
    contents alias the immutable [contents] string: nothing is copied until the
    file is first written to. The parent directory must exist. *)

val unlink : Dir.fd -> string -> unit
(** Remove a non-directory entry. *)

val rmdir : Dir.fd -> string -> unit
(** Remove an empty directory. *)

val statat : follow:bool -> Dir.fd -> string -> Eio.File.Stat.t
(** Read path metadata, optionally following a symbolic link. *)

val read_dir : Dir.fd -> string -> string list
(** List the names in a directory, excluding [.] and [..]. *)

val read_dir_entries : Dir.fd -> string -> (Eio.File.Stat.kind * string) list
(** List the kind and name of each directory entry. *)

val read_link : Dir.fd -> string -> string
(** Read a symbolic link target without following it. *)

val chmod : follow:bool -> Dir.fd -> perm:int -> string -> unit
(** Change path permissions, optionally following a symbolic link. *)

val chown : follow:bool -> ?uid:int64 -> ?gid:int64 -> Dir.fd -> string -> unit
(** Change path ownership, optionally following a symbolic link. *)

val rename : Dir.fd -> string -> Dir.fd -> string -> unit
(** Rename a path between two directory descriptors. *)

val symlinkat : link_to:string -> Dir.fd -> string -> unit
(** Create a symbolic link relative to a directory descriptor. *)

module Flow : sig
  val of_fd :
    Fd.t ->
    [ [ `Generic | `Unix ] Eio.Net.stream_socket_ty | Eio.File.rw_ty ]
    Eio.Resource.t
  (** Wrap a file descriptor as an Eio flow and file resource. *)
end

val reset : unit -> unit
(** Reset the global descriptor table and filesystem contents. *)
