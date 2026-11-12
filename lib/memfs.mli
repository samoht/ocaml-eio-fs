(** The in-memory filesystem's directories, behind [Eio_fs.Fs]. *)

val fs :
  ?user:Low_level.User.t ->
  Low_level.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t
(** [fs ~user t] is full access to the filesystem [t], as [user] (defaults to
    root). *)

val cwd :
  ?user:Low_level.User.t ->
  Low_level.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t
(** [cwd ~user t] is access to [t] confined to its current directory. *)

val v :
  ?clock:_ Eio.Time.clock ->
  ?user:Low_level.User.t ->
  unit ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [v ~clock ~user ()] is the root of a fresh, empty filesystem whose times
    [clock] gives. *)

val as_user :
  Low_level.User.t -> 'a Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [as_user user path] is [path], an in-memory directory, as [user].

    @raise Invalid_argument if [path] is not an in-memory directory. *)

val held : _ Eio.Path.t -> (int * int) list option
(** [held path] is the byte ranges writes covered in the regular file [path] of
    an in-memory filesystem, in order: the rest of it is a hole. It is [None] if
    [path] is not one. *)

val set_times :
  _ Eio.Path.t -> atime:float -> mtime:float -> ctime:float -> unit
(** [set_times path ~atime ~mtime ~ctime] sets the times of [path], not
    followed, as root, if it is in an in-memory filesystem, and does nothing
    otherwise. *)

val load :
  ?clock:_ Eio.Time.clock ->
  ?filter:('a Eio.Path.t -> bool) ->
  'a Eio.Path.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [load ~filter path] is a new filesystem holding a copy of the tree at
    [path]. *)
