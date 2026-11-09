(* The fake file-descriptor table and filesystem. *)
open! Hermetic

module Open_flags = struct
  let o_rdonly = 0x0
  let o_rdwr = 0x2
  let o_append = 0x400
  let o_creat = 0x40
  let o_directory = 0x10000
  let o_excl = 0x80
  let o_nofollow = 0x20000
  let o_trunc = 0x200
  let empty = 0
  let ( + ) = ( lor )
  let mem a b = a land b = a
end

type Eio.Exn.Backend.t += Eio_fs of string

(* Why a confined capability refused a path, named as eio_posix names it: by
   what the caller asked for, never by where the capability sits. *)
type Eio.Exn.Backend.t += Outside_sandbox of string | Absolute_path

let () =
  let pp_eio_fs ppf = function
    | Eio_fs "" ->
        Fmt.pf ppf "eio-fs";
        true
    | Eio_fs s ->
        Fmt.pf ppf "eio-fs:%s" s;
        true
    | Outside_sandbox path ->
        Fmt.pf ppf "Outside_sandbox (%S)" path;
        true
    | Absolute_path ->
        Fmt.pf ppf "Absolute_path";
        true
    | _ -> false
  in
  Eio.Exn.Backend.register_pp pp_eio_fs

module Error = struct
  let not_found path =
    let exn = Eio.Exn.create Eio.Fs.(E (Not_found (Eio_fs path))) in
    raise exn

  let already_exists path =
    let exn = Eio.Exn.create Eio.Fs.(E (Already_exists (Eio_fs path))) in
    raise exn

  let permission_denied path =
    let exn = Eio.Exn.create Eio.Fs.(E (Permission_denied (Eio_fs path))) in
    raise exn

  let outside_sandbox path =
    raise (Eio.Fs.err (Permission_denied (Outside_sandbox path)))

  let absolute_path () = raise (Eio.Fs.err (Permission_denied Absolute_path))

  (* ELOOP, which Eio raises as [Symlink]. *)
  let symlink_loop () = raise (Eio.Fs.err Eio.Fs.Symlink)

  (* A caller's mistake, as eio_posix reports it. *)
  let badf () = invalid_arg "file descriptor used after calling close!"

  let err ctx =
    let exn = Eio.Exn.create (Eio.Exn.X (Eio_fs ctx)) in
    raise exn

  (* ENOTDIR, which Eio.Fs has no constructor for and eio_posix raises as a
     bare [Eio.Io]: never [Not_found], which a [~missing_ok] would swallow. *)
  let not_a_directory path = err ("not a directory: " ^ path)
end

(* The number a descriptor table hands out. *)
module Num : sig
  type t = private int

  val compare : t -> t -> int
  val of_int : int -> t
  val incr : t -> t
end = struct
  type t = int

  let compare (f1 : t) (f2 : t) = Int.compare (f1 :> int) (f2 :> int)
  let of_int i = i
  let incr t = t + 1
end

module Inode = struct
  type t = int64

  let compare = Int64.compare
  let equal = Int64.equal
end

module Dir_node = struct
  (* A node holds no name of its own: its names are the entries of the
     directories that hold it. *)
  type 'a t = { stat : Eio.File.Stat.t; entries : 'a list }

  let v ~parent stat = { stat; entries = [ (stat.ino, "."); (parent, "..") ] }
  let is_empty dir = List.length dir.entries = 2

  let parent dir =
    List.find (function _, ".." -> true | _ -> false) dir.entries
end

module Symlink = struct
  type t = { stat : Eio.File.Stat.t; link_to : string }

  let v ~link_to stat = { stat; link_to }
end

module File = struct
  (* [Literal] aliases an immutable string (an embedded asset) and is turned
     into an owned [Buffer] by the first write, so embedding a file costs no
     copy until someone mutates it. A [Buffer] is at least as long as the file,
     whose length is the stat's size, and grows by doubling, so a run of writes
     costs linear time. *)
  type contents = Literal of string | Buffer of Cstruct.t
  type t = { stat : Eio.File.Stat.t; contents : contents }

  let v ~stat = { stat; contents = Buffer Cstruct.empty }
  let literal ~stat contents = { stat; contents = Literal contents }
  let size f = Optint.Int63.to_int f.stat.size

  let blit t ~src_off dst ~dst_off ~len =
    match t with
    | Literal s -> Cstruct.blit_from_string s src_off dst dst_off len
    | Buffer b -> Cstruct.blit b src_off dst dst_off len

  (* A buffer holding [f]'s bytes with room for [need], and every byte from
     [f]'s size to [need] zero: a truncation leaves stale bytes past the end. *)
  let reserve f need =
    let size = size f in
    let buf =
      match f.contents with
      | Buffer b when Cstruct.length b >= need -> b
      | contents ->
          let cap =
            match contents with Buffer b -> Cstruct.length b | Literal _ -> 0
          in
          let b = Cstruct.create (max need (2 * cap)) in
          blit contents ~src_off:0 b ~dst_off:0 ~len:size;
          b
    in
    if need > size then Cstruct.memset (Cstruct.sub buf size (need - size)) 0;
    buf

  (* The largest file, 4 GiB: the bytes live in memory, holes included. *)
  let max_size = 1 lsl 32

  (* EFBIG past [max_size], checked without overflowing. *)
  let check_size ~off ~len =
    if off > max_size || len > max_size - off then
      raise (Eio.Fs.err Eio.Fs.File_too_large)

  (* [f] with [src] written at [off]. *)
  let write f ~off src =
    let len = Cstruct.length src in
    check_size ~off ~len;
    let buf = reserve f (off + len) in
    Cstruct.blit src 0 buf off len;
    let size = Optint.Int63.of_int (max (size f) (off + len)) in
    { stat = { f.stat with size }; contents = Buffer buf }

  (* [f] cut or zero-extended to [n] bytes. *)
  let truncate f n =
    check_size ~off:n ~len:0;
    let contents = if n <= size f then f.contents else Buffer (reserve f n) in
    { stat = { f.stat with size = Optint.Int63.of_int n }; contents }
end

module Fd_table = Map.Make (Num)

(* Only what the tree itself holds. This backend has no console, so it has no
   standard streams to open: descriptors 0 to 2 are reserved by {!first_fd} and
   never resolve to anything. *)
module Resource = struct
  type t =
    | File of File.t
    | Dir of (Inode.t * string) Dir_node.t
    | Symlink of Symlink.t

  let stat = function
    | File f -> f.stat
    | Dir dir -> dir.stat
    | Symlink s -> s.stat

  let set_stat r stat =
    match r with
    | File f -> File { f with stat }
    | Dir d -> Dir { d with stat }
    | Symlink s -> Symlink { s with stat }
end

type resource = Resource.t

(* A component that had to be a directory and is not: ENOTDIR. *)
let as_dir ~path = function
  | Resource.Dir dir -> dir
  | _ -> Error.not_a_directory path

let truncate_file r =
  match !r with
  | Resource.File f ->
      let stat = { f.stat with size = Optint.Int63.zero } in
      r := File { contents = File.Buffer Cstruct.empty; stat }
  | _ -> ()

(* Descriptors 0 to 2 stay reserved for the standard streams, so a number this
   backend hands out is never one a caller reads as stdin, stdout or stderr. *)
let first_fd = Num.of_int 3

let next_fd_of_fds fds =
  match Fd_table.max_binding_opt fds with
  | None -> first_fd
  | Some (fd, _) -> Num.incr fd

module Entries = Map.Make (Inode)

(* One file system: its tree by inode, the descriptors open on it, the last
   inode it numbered, and the directory a relative path resolves against.
   Nothing is shared between two of them. [lock] serialises the operations on
   it, so fibers of several domains may share one. *)
type fs = {
  mutable tree : resource ref Entries.t;
  mutable fds : resource ref Fd_table.t;
  mutable last_ino : Inode.t;
  cwd : string;
  mutable cwd_dir : resource ref option;
      (* The directory [cwd] named when first used: it follows that
         directory, as a descriptor does, whatever later takes the path. *)
  lock : Mutex.t;
}

type t = fs

(* A descriptor is one open file description: the offset and the O_APPEND
   flag are its own, never shared with another open of the same file. *)
module Fd = struct
  type t = {
    fs : fs;
    num : Num.t;
    append : bool;
    mutable pos : Optint.Int63.t;
    mutable closed : bool;
  }

  let v ?(append = false) fs num =
    { fs; num; append; pos = Optint.Int63.zero; closed = false }

  let closed t = t.closed

  (* A closed handle answers on its own state, never by its number, which the
     next open may have been handed. *)
  let num t = if t.closed then Error.badf () else t.num
end

module Dir = struct
  type fd = Fs of fs | Cwd of fs | Dir_fd of Fd.t

  let fs = function Fs fs | Cwd fs -> fs | Dir_fd fd -> fd.fs
end

(* Each operation runs whole under its file system's lock and never yields
   inside it: no other fiber, in this domain or another, sees the tree or the
   descriptor table half updated, nor writes back a table read before it. *)
let locked fs fn = Mutex.protect fs.lock fn

(* The resource the live descriptor [t] is open on. *)
let resource_of (t : Fd.t) =
  match Fd_table.find_opt (Fd.num t) t.fs.fds with
  | Some r -> r
  | None -> Error.badf ()

let default_stat =
  Eio.File.Stat.
    {
      dev = 0L;
      ino = 0L;
      kind = `Regular_file;
      perm = 0;
      nlink = 0L;
      uid = 0L;
      gid = 0L;
      rdev = 0L;
      size = Optint.Int63.zero;
      atime = 0.;
      mtime = 0.;
      ctime = 0.;
      blksize = 512L;
      blocks = 0L;
    }

let new_stat fs ?kind ?perm () =
  fs.last_ino <- Int64.succ fs.last_ino;
  {
    default_stat with
    ino = fs.last_ino;
    kind = Option.value ~default:default_stat.kind kind;
    perm = Option.value ~default:default_stat.perm perm;
  }

module Filesystem = struct
  type t = resource ref Entries.t

  (* A function, not a value: the root is a [ref] mutated in place as entries
     are added, so each empty filesystem needs its own fresh root ref. *)
  let empty () : t =
    let root =
      Resource.Dir
        {
          stat =
            {
              default_stat with
              size = Optint.Int63.of_int 4096;
              ino = 0L;
              kind = `Directory;
            };
          entries = [ (0L, "."); (0L, "..") ];
        }
    in
    Entries.empty |> Entries.add 0L (ref root)

  let add_file fs ino r = fs.tree <- Entries.add ino r fs.tree
  let root (tree : t) = Entries.find 0L tree

  (* A removed directory's parent may be gone too: ENOENT. *)
  let parent_dir (tree : t) r =
    match
      Entries.find_opt (fst (Dir_node.parent (as_dir ~path:".." !r))) tree
    with
    | Some parent -> parent
    | None -> Error.not_found ".."

  (* Whether the directory [r] is the directory [ino] or lies below it, by the
     parent links of the tree itself. *)
  let rec within (tree : t) r ino =
    match !r with
    | Resource.Dir d -> (
        Inode.equal d.stat.ino ino
        || (not (Inode.equal d.stat.ino 0L))
           &&
           match Entries.find_opt (fst (Dir_node.parent d)) tree with
           | Some parent -> within tree parent ino
           | None -> false)
    | _ -> false

  (* path_resolution(7): a resolution follows at most [max_follows] symbolic
     links, as Linux and eio_posix do, and then fails with ELOOP. *)
  let max_follows = 40

  let follow_link follows =
    if !follows <= 0 then Error.symlink_loop ();
    decr follows

  (* A resolution in progress, one component at a time as path_resolution(7)
     describes it. [dirs] holds the directories entered, innermost first. A
     confined walk keeps all of them back to its [base] and climbs a [..] by
     dropping one, so it never leaves the base: a [..] at the base, or an
     absolute path or link target, is refused, as eio_posix refuses them. A
     walk with full access keeps only the directory it is in and climbs to
     its parent, the root being its own. *)
  type walk = {
    tree : t;
    base : Inode.t option;
    follows : int ref;
    dirs : resource ref list;
  }

  let here w = match w.dirs with r :: _ -> r | [] -> assert false

  let push w r =
    match w.base with
    | Some _ -> { w with dirs = r :: w.dirs }
    | None -> { w with dirs = [ r ] }

  (* [up w rest] is [w] one directory up, [rest] the components after the
     [..], which a refusal names as eio_posix names them. *)
  let up w rest =
    match (w.base, w.dirs) with
    | Some _, [ _ ] -> Error.outside_sandbox (String.concat "/" (".." :: rest))
    | Some _, _ :: dirs -> { w with dirs }
    | Some _, [] -> assert false
    | None, _ -> { w with dirs = [ parent_dir w.tree (here w) ] }

  let names path =
    List.filter (fun name -> not (String.equal name "")) (Eio_fs_path.segs path)

  (* Where [path] starts: the root for an absolute path, which a confined walk
     refuses, and the directory [w] is in otherwise. *)
  let enter w path =
    if not (Eio_fs_path.is_abs path) then w
    else
      match w.base with
      | Some _ -> Error.absolute_path ()
      | None -> { w with dirs = [ root w.tree ] }

  (* The entry [name] names in the directory [w] is in, [.] and [..] included,
     or [None]. *)
  let child w name =
    match name with
    | "." -> Some (here w)
    | ".." -> Some (here (up w []))
    | name -> (
        let d = as_dir ~path:name !(here w) in
        match List.find_opt (fun (_, n) -> String.equal n name) d.entries with
        | None -> None
        | Some (ino, _) -> Entries.find_opt ino w.tree)

  (* A link to the empty path names nothing: ENOENT. *)
  let link_target name (s : Symlink.t) =
    if String.equal s.link_to "" then Error.not_found name;
    s.link_to

  (* [walk ~after w names] enters each directory of [names] in turn,
     following every link on the way; [after] are the components left once
     [names] are, which a refusal names. *)
  let rec walk ?(after = []) w = function
    | [] -> w
    | "." :: rest -> walk ~after w rest
    | ".." :: rest -> walk ~after (up w (rest @ after)) rest
    | name :: rest -> walk ~after (into w name) rest

  and into w name =
    match child w name with
    | None -> Error.not_found name
    | Some r -> (
        match !r with
        | Resource.Dir _ -> push w r
        | Symlink s ->
            follow_link w.follows;
            let target = link_target name s in
            walk (enter w target) (names target)
        | File _ -> Error.not_a_directory name)

  (* [locate w path] is the walk into the directory holding [path]'s last
     component, that component (["."] when [path] has none) and whether
     [path] ends in a separator. A confined walk checks by the tree's own
     parent links that the directory it reached is below its base. *)
  let locate w path =
    (* NUL ends a C path: ENOENT. A name holds at most NAME_MAX bytes. *)
    if String.exists (Char.equal '\000') path then Error.not_found path;
    if List.exists (fun n -> String.length n > 255) (names path) then
      Error.err ("file name too long: " ^ path);
    let w = enter w path in
    let w, name =
      match List.rev (names path) with
      | [] -> (w, ".")
      | last :: rest -> (walk ~after:[ last ] w (List.rev rest), last)
    in
    (match w.base with
    | Some base when not (within w.tree (here w) base) ->
        Error.outside_sandbox path
    | _ -> ());
    let len = String.length path in
    (w, name, len > 0 && path.[len - 1] = '/')

  (* [final ~follow w name ~slash] is the resource [name] names in [w], a link
     followed when [follow] or [slash] says so. A name in directory syntax
     must be a directory. *)
  let rec final ~follow w name ~slash =
    let r =
      match child w name with Some r -> r | None -> Error.not_found name
    in
    match !r with
    | Resource.Symlink s when follow || slash ->
        follow_link w.follows;
        let w, name, slash' = locate w (link_target name s) in
        final ~follow:true w name ~slash:(slash || slash')
    | Resource.Dir _ -> r
    | _ when slash -> Error.not_a_directory name
    | _ -> r

  (* Where [path] given to [dir_fd] starts. Full access resolves a relative
     path against the current directory, which an absolute path need not
     reach; the current directory and an open directory confine the walk to
     themselves. *)
  let start dir_fd path =
    let fs = Dir.fs dir_fd in
    let tree = fs.tree in
    let at_root =
      { tree; base = None; follows = ref max_follows; dirs = [ root tree ] }
    in
    let cwd () =
      let r =
        match fs.cwd_dir with
        | Some r -> r
        | None ->
            let r = here (walk at_root (names fs.cwd)) in
            fs.cwd_dir <- Some r;
            r
      in
      { at_root with dirs = [ r ] }
    in
    match dir_fd with
    | Dir.Fs _ when Eio_fs_path.is_abs path -> at_root
    | Dir.Fs _ -> cwd ()
    | Dir.Cwd _ ->
        let w = cwd () in
        { w with base = Some (Resource.stat !(here w)).ino }
    | Dir.Dir_fd fd ->
        let r = resource_of fd in
        { at_root with base = Some (Resource.stat !r).ino; dirs = [ r ] }

  let locate_from dir_fd path = locate (start dir_fd path) path

  let lookup ~follow dir_fd path =
    let w, name, slash = locate_from dir_fd path in
    final ~follow w name ~slash
end

let v ?(cwd = "/") () =
  {
    tree = Filesystem.empty ();
    fds = Fd_table.empty;
    last_ino = 0L;
    cwd;
    cwd_dir = None;
    lock = Mutex.create ();
  }

(* Closing an already-closed handle does nothing, as [Eio.Resource.close]
   specifies and eio's posix backend does. A handle with two owners, a caller
   closing early in front of its switch, is the ordinary reason. *)
let close (fd : Fd.t) =
  locked fd.fs @@ fun () ->
  if not fd.closed then begin
    fd.closed <- true;
    fd.fs.fds <- Fd_table.remove fd.num fd.fs.fds
  end

(* open(2): opening an existing file requires the permission bits the access
   mode asks for; the mode given at creation does not restrict the creating
   open itself. This backend opens files O_RDONLY or O_RDWR, so the read bit
   is always required and the write bit with o_rdwr. The model is
   single-user, so the owner bits are the ones consulted. Directories are
   exempt: fixtures predate recorded directory modes. *)
let check_access path flags r =
  let need = if Open_flags.(mem o_rdwr flags) then 0o600 else 0o400 in
  match !r with
  | Resource.File f when f.stat.perm land need <> need ->
      Error.permission_denied path
  | _ -> ()

(* A directory removed while a handle held it takes no new entry: ENOENT. *)
let check_live fs rdir path =
  if not (Entries.mem (Resource.stat !rdir).ino fs.tree) then
    Error.not_found path

(* [.] and [..] name a directory already there, never an entry to make,
   remove or move. *)
let is_dot name = String.equal name "." || String.equal name ".."

(* Create the entry [path] with [add parent name] unless its name is taken,
   by a dangling symbolic link included: EEXIST, as mkdir(2) and symlink(2)
   answer. Only a directory is made under a name with a trailing slash: ENOENT
   otherwise. *)
let new_entry ?(dir = false) dir_fd path add =
  let w, name, slash = Filesystem.locate_from dir_fd path in
  match Filesystem.child w name with
  | Some _ -> Error.already_exists path
  | None when slash && not dir -> Error.not_found path
  | None ->
      check_live (Dir.fs dir_fd) (Filesystem.here w) path;
      add (Filesystem.here w) name

(* Create [name] in the directory [w] is in, register the new file in the
   filesystem, and return its resource. *)
let new_file ~mode fs (w : Filesystem.walk) name ~slash =
  (* A path in directory syntax names a directory, which O_CREAT never makes:
     macOS answers ENOENT, Linux EISDIR. *)
  if slash then Error.not_found name;
  let rdir = Filesystem.here w in
  check_live fs rdir name;
  let dir = as_dir ~path:name !rdir in
  let stat = new_stat fs ~perm:(mode land 0o7777) () in
  let f = File.v ~stat in
  let dir = { dir with entries = (f.stat.ino, name) :: dir.entries } in
  rdir := Resource.Dir dir;
  let file = ref (Resource.File f) in
  Filesystem.add_file fs f.stat.ino file;
  file

(* The descriptor is registered with [sw] once the lock is released: a
   finished switch runs [close] at once, which takes the lock. *)
let open_locked ~mode dir_fd path flags =
  let is_create = Open_flags.(mem o_creat flags) in
  let is_excl = Open_flags.(mem o_excl flags) in
  let append = Open_flags.(mem o_append flags) in
  let truncate = Open_flags.(mem o_trunc flags) in
  let nofollow = Open_flags.(mem o_nofollow flags) in
  let directory = Open_flags.(mem o_directory flags) in
  let fs = Dir.fs dir_fd in
  let opened r =
    (match !r with
    (* Write access to a directory: EISDIR. *)
    | Resource.Dir _ when Open_flags.(mem o_rdwr flags) ->
        Error.err ("open: is a directory: " ^ path)
    | Resource.Dir _ -> ()
    (* O_DIRECTORY on anything else: ENOTDIR. *)
    | _ when directory -> Error.err ("open: not a directory: " ^ path)
    | _ -> ());
    check_access path flags r;
    if truncate then truncate_file r;
    r
  in
  let r =
    match is_create || is_excl with
    | false -> (
        let r = Filesystem.lookup ~follow:(not nofollow) dir_fd path in
        match !r with
        (* O_NOFOLLOW on a symbolic link: ELOOP. *)
        | Resource.Symlink _ -> Error.symlink_loop ()
        | _ -> opened r)
    | true ->
        (* O_CREAT looks at the last component without following it. A
           symbolic link there, dangling or not, makes O_EXCL fail with EEXIST
           and O_NOFOLLOW with ELOOP; otherwise the open follows it and, when
           its target is missing, creates that target, resolved from the
           directory holding the link. *)
        let rec create (w : Filesystem.walk) name ~slash =
          match Filesystem.child w name with
          | None -> new_file ~mode fs w name ~slash
          | Some _ when is_excl -> Error.already_exists path
          | Some r -> (
              match !r with
              | Resource.Symlink _ when nofollow -> Error.symlink_loop ()
              | Resource.Symlink s -> (
                  match Filesystem.final ~follow:true w name ~slash with
                  | r -> opened r
                  | exception Eio.Io (Eio.Fs.E (Not_found _), _) ->
                      Filesystem.follow_link w.follows;
                      let w, name, slash =
                        Filesystem.locate w (Filesystem.link_target name s)
                      in
                      create w name ~slash)
              | _ -> opened r)
        in
        let w, name, slash = Filesystem.locate_from dir_fd path in
        create w name ~slash
  in
  let num = next_fd_of_fds fs.fds in
  fs.fds <- Fd_table.add num r fs.fds;
  Fd.v ~append fs num

let openat ~mode ~sw dir_fd path flags =
  let fd =
    locked (Dir.fs dir_fd) @@ fun () -> open_locked ~mode dir_fd path flags
  in
  Eio.Switch.on_release sw (fun () -> close fd);
  fd

let statat ~follow dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  Resource.stat !(Filesystem.lookup ~follow dir_fd path)

let chmod ~follow dir_fd ~perm path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let r = Filesystem.lookup ~follow dir_fd path in
  r := Resource.set_stat !r { (Resource.stat !r) with perm }

let chown ~follow ?uid ?gid dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let r = Filesystem.lookup ~follow dir_fd path in
  let s = Resource.stat !r in
  let uid = Option.value ~default:s.uid uid in
  let gid = Option.value ~default:s.gid gid in
  r := Resource.set_stat !r { s with uid; gid }

(* The directory [w] is in, without its entry [name]. The entry's inode leaves
   the tree with its only name, and is freed once no descriptor holds it. *)
let remove_entry fs (w : Filesystem.walk) name =
  let r = Filesystem.here w in
  let d = as_dir ~path:name !r in
  let gone, entries =
    List.partition (fun (_, n) -> String.equal name n) d.entries
  in
  r := Resource.Dir { d with entries };
  List.iter (fun (ino, _) -> fs.tree <- Entries.remove ino fs.tree) gone

let unlink dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let w, name, slash = Filesystem.locate_from dir_fd path in
  (* EISDIR, as Linux answers; macOS answers EPERM. *)
  let is_a_directory () = Error.err ("unlink: is a directory: " ^ path) in
  if is_dot name then is_a_directory ();
  match Filesystem.child w name with
  | None -> Error.not_found path
  | Some { contents = Resource.Dir _ } -> is_a_directory ()
  (* A trailing slash on anything else, a link to a directory included. *)
  | Some _ when slash -> Error.not_a_directory path
  | Some _ -> remove_entry (Dir.fs dir_fd) w name

let mkdirat dir_fd ~perm path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let fs = Dir.fs dir_fd in
  let mkdir r name =
    let new_stat = new_stat fs ~kind:`Directory ~perm () in
    let d = as_dir ~path !r in
    let dir = Dir_node.v ~parent:d.stat.ino new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file fs ino (ref (Resource.Dir dir));
    let entries = (ino, name) :: d.entries in
    r := Dir { d with entries }
  in
  new_entry ~dir:true dir_fd path mkdir

let embed dir_fd ~perm path ~contents =
  locked (Dir.fs dir_fd) @@ fun () ->
  let fs = Dir.fs dir_fd in
  let add r name =
    let stat =
      {
        (new_stat fs ~perm:(perm land 0o7777) ()) with
        size = Optint.Int63.of_int (String.length contents);
      }
    in
    let d = as_dir ~path !r in
    let f = File.literal ~stat contents in
    Filesystem.add_file fs stat.ino (ref (Resource.File f));
    r := Dir { d with entries = (stat.ino, name) :: d.entries }
  in
  new_entry dir_fd path add

let symlinkat ~link_to dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let fs = Dir.fs dir_fd in
  let symlink r name =
    let new_stat =
      {
        (new_stat fs ~kind:`Symbolic_link ~perm:0o644 ()) with
        size = Optint.Int63.of_int (String.length link_to);
      }
    in
    let d = as_dir ~path !r in
    let dir = Symlink.v ~link_to new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file fs ino (ref (Resource.Symlink dir));
    let entries = (ino, name) :: d.entries in
    r := Dir { d with entries }
  in
  new_entry dir_fd path symlink

let read_link dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  match !(Filesystem.lookup ~follow:false dir_fd path) with
  | Symlink s -> s.link_to
  | _ -> Error.err ("not a symbolic link: " ^ path)

let read_dir dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  match !(Filesystem.lookup ~follow:true dir_fd path) with
  | Resource.Dir dir ->
      List.map snd dir.entries
      |> List.filter (function "." | ".." -> false | _ -> true)
  | _ -> Error.err "Not a directory"

let read_dir_entries dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  match !(Filesystem.lookup ~follow:true dir_fd path) with
  | Resource.Dir dir ->
      dir.entries
      |> List.filter (fun (_, name) -> name <> "." && name <> "..")
      |> List.map (fun (ino, name) ->
          let kind =
            match Entries.find_opt ino (Dir.fs dir_fd).tree with
            | Some res -> (Resource.stat !res).kind
            | None -> `Unknown
          in
          (kind, name))
  | _ -> Error.err "Not a directory"

let rmdir dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let w, name, _ = Filesystem.locate_from dir_fd path in
  (* rmdir(2) refuses a last [.] with EINVAL and a last [..] with ENOTEMPTY. *)
  if String.equal name "." then Error.err ("rmdir: invalid argument: " ^ path);
  if String.equal name ".." then
    Error.err ("rmdir: directory not empty: " ^ path);
  (* A final symbolic link is not followed: ENOTDIR. *)
  match Filesystem.child w name with
  | None -> Error.not_found path
  | Some { contents = Resource.Dir d } ->
      if not (Dir_node.is_empty d) then Error.err "Directory is not empty"
      else remove_entry (Dir.fs dir_fd) w name
  | Some _ -> Error.err ("rmdir: not a directory: " ^ path)

(* rename(2) refuses to move a directory below itself (EINVAL), a directory
   over a non-empty directory (ENOTEMPTY) or over a non-directory (ENOTDIR),
   and a non-directory over a directory (EISDIR). Eio.Fs has no constructor
   for these, which eio_posix raises as a bare [Eio.Io]. *)
let check_rename tree ~src ~new_parent ~dst new_path =
  let ino = (Resource.stat !src).ino in
  match (!src, Option.map ( ! ) dst) with
  | Resource.Dir _, _ when Filesystem.within tree new_parent ino ->
      Error.err ("rename: invalid argument, into its own subtree: " ^ new_path)
  | Resource.Dir _, Some (Resource.Dir d) when not (Dir_node.is_empty d) ->
      Error.err ("rename: directory not empty: " ^ new_path)
  | Resource.Dir _, Some (File _ | Symlink _) ->
      Error.err ("rename: not a directory: " ^ new_path)
  | (File _ | Symlink _), Some (Resource.Dir _) ->
      Error.err ("rename: is a directory: " ^ new_path)
  | _ -> ()

(* [entries] without the entry [name]. *)
let without name entries =
  List.filter (fun (_, n) -> not (String.equal n name)) entries

(* A rename MOVES: the source loses [old_name] and the target gains [new_name]
   pointing at [ino]. Same-parent edits go to one entries list, so the move is
   atomic. *)
let move_entry ino ~old_parent ~old_name old_path ~new_parent ~new_name new_path
    =
  if old_parent == new_parent then begin
    let d = as_dir ~path:new_path !new_parent in
    let entries =
      (ino, new_name) :: without new_name (without old_name d.entries)
    in
    new_parent := Resource.Dir { d with entries }
  end
  else begin
    let dst = as_dir ~path:new_path !new_parent in
    let entries = (ino, new_name) :: without new_name dst.entries in
    new_parent := Resource.Dir { dst with entries };
    let src = as_dir ~path:old_path !old_parent in
    old_parent :=
      Resource.Dir { src with entries = without old_name src.entries }
  end

(* A directory's [..] names its new parent. *)
let reparent res ~new_parent =
  match !res with
  | Resource.Dir d ->
      let parent = (Resource.stat !new_parent).ino in
      let entries =
        List.map
          (fun ((_, name) as e) ->
            if String.equal name ".." then (parent, "..") else e)
          d.entries
      in
      res := Resource.Dir { d with entries }
  | File _ | Symlink _ -> ()

let rename ?(replace = true) dir_fd old_path new_dir new_path =
  locked (Dir.fs dir_fd) @@ fun () ->
  (* rename(2) does not move an entry between two file systems: EXDEV, which
     Eio.Fs has no constructor for. *)
  if Dir.fs dir_fd != Dir.fs new_dir then
    Error.err ("rename: invalid cross-device link: " ^ new_path);
  let tree = (Dir.fs dir_fd).tree in
  let old_w, old_name, old_slash = Filesystem.locate_from dir_fd old_path in
  let new_w, new_name, new_slash = Filesystem.locate_from new_dir new_path in
  (* rename(2) answers EINVAL or EBUSY for a last [.] or [..]. *)
  if is_dot old_name || is_dot new_name then
    Error.err ("rename: invalid argument: " ^ new_path);
  (* Rename the directory entry itself. In particular, a symlink's target need
     not exist and must not be followed while moving the link. *)
  let old_res =
    match Filesystem.child old_w old_name with
    | Some r -> r
    | None -> Error.not_found old_path
  in
  (* A trailing slash on either name requires a directory, never followed. *)
  (match !old_res with
  | Resource.Dir _ -> ()
  | _ when old_slash || new_slash -> Error.not_a_directory old_path
  | _ -> ());
  let ino = Resource.(stat !old_res).ino in
  let old_parent = Filesystem.here old_w in
  let new_parent = Filesystem.here new_w in
  check_live (Dir.fs new_dir) new_parent new_path;
  let existing =
    List.find_opt
      (fun (_, name) -> String.equal name new_name)
      (as_dir ~path:new_path !new_parent).entries
  in
  match existing with
  (* Two names of one entry: rename(2) does nothing. *)
  | Some (dst_ino, _) when Inode.equal dst_ino ino -> ()
  | _ ->
      if (not replace) && Option.is_some existing then
        Error.already_exists new_path;
      let dst =
        Option.bind existing (fun (dst_ino, _) -> Entries.find_opt dst_ino tree)
      in
      check_rename tree ~src:old_res ~new_parent ~dst new_path;
      move_entry ino ~old_parent ~old_name old_path ~new_parent ~new_name
        new_path;
      reparent old_res ~new_parent;
      (* The replaced entry lost its only name. *)
      Option.iter
        (fun (dst_ino, _) ->
          let fs = Dir.fs dir_fd in
          fs.tree <- Entries.remove dst_ino fs.tree)
        existing

let stat t = locked t.Fd.fs @@ fun () -> Resource.stat !(resource_of t)

(* An offset before the start of a file: EINVAL, which eio_posix raises as a
   bare [Eio.Io]. *)
let check_offset op off =
  if Optint.Int63.compare off Optint.Int63.zero < 0 then
    Fmt.kstr Error.err "%s: invalid offset %a" op Optint.Int63.pp off

(* TODO: update access times *)

(* EISDIR, naming the operation and never the contents. *)
let not_a_file op r =
  Error.err
    (op ^ ": not a file: "
    ^ match r with Resource.Dir _ -> "a directory" | _ -> "not a regular file")

(* A descriptor open on a non-regular file still answers the file interface --
   [Eio.Path.open_in] on a directory is legal and the portable directory
   barrier depends on it -- so the operations below refuse one with EISDIR, as
   read(2) and write(2) do. *)
let writev t bufs =
  locked t.Fd.fs @@ fun () ->
  let r = resource_of t in
  match !r with
  | File e ->
      (* The buffers are one contiguous run, written at the offset or, for an
         append descriptor, at the end. *)
      let src = Cstruct.concat bufs in
      if t.append then t.pos <- e.stat.size;
      r := File (File.write e ~off:(Optint.Int63.to_int t.pos) src);
      t.pos <- Optint.Int63.add t.pos (Optint.Int63.of_int (Cstruct.length src));
      Cstruct.length src
  | r -> not_a_file "writev" r

let readv t bufs =
  locked t.Fd.fs @@ fun () ->
  match !(resource_of t) with
  | File e ->
      let read = ref 0 in
      let pos = ref t.pos in
      let size = Optint.Int63.to_int e.stat.size in
      List.iter
        (fun buf ->
          let len = Cstruct.length buf in
          let posi = Optint.Int63.to_int !pos in
          let actual_len = max 0 (min len (size - posi)) in
          (* At or past the end nothing is read: end of file. *)
          if actual_len > 0 then
            File.blit e.contents ~src_off:posi buf ~dst_off:0 ~len:actual_len;
          pos := Optint.Int63.add !pos (Optint.Int63.of_int actual_len);
          read := !read + actual_len)
        bufs;
      t.pos <- !pos;
      !read
  | r -> not_a_file "readv" r

(* Positioned reads and writes address an explicit [file_offset] and never move
   the cursor, unlike [readv]/[writev]. This mirrors [Eio.File.pread]/[pwrite]. *)
let preadv t ~file_offset bufs =
  locked t.Fd.fs @@ fun () ->
  check_offset "pread" file_offset;
  match !(resource_of t) with
  | Resource.File e ->
      let size = File.size e in
      let start = Optint.Int63.to_int file_offset in
      if start >= size then raise End_of_file
      else begin
        let off = ref start in
        let read = ref 0 in
        List.iter
          (fun buf ->
            let len = Cstruct.length buf in
            let actual = max 0 (min len (size - !off)) in
            File.blit e.contents ~src_off:!off buf ~dst_off:0 ~len:actual;
            off := !off + actual;
            read := !read + actual)
          bufs;
        if !read = 0 then raise End_of_file else !read
      end
  | r -> not_a_file "pread" r

let pwritev t ~file_offset bufs =
  locked t.Fd.fs @@ fun () ->
  check_offset "pwrite" file_offset;
  let r = resource_of t in
  match !r with
  | Resource.File e ->
      let src = Cstruct.concat bufs in
      let off = Optint.Int63.to_int file_offset in
      r := Resource.File (File.write e ~off src);
      Cstruct.length src
  | r -> not_a_file "pwrite" r

let seek_fd t off cmd =
  locked t.Fd.fs @@ fun () ->
  match !(resource_of t) with
  | Resource.File e ->
      let base =
        match cmd with
        | `Set -> Optint.Int63.zero
        | `Cur -> t.pos
        | `End -> e.stat.size
      in
      let newpos = Optint.Int63.add base off in
      check_offset "seek" newpos;
      t.pos <- newpos;
      newpos
  (* lseek(2) on a directory succeeds; its offset is no byte count. *)
  | Resource.Dir _ -> Optint.Int63.zero
  | r -> not_a_file "seek" r

let truncate_fd t len =
  locked t.Fd.fs @@ fun () ->
  check_offset "truncate" len;
  let r = resource_of t in
  match !r with
  | Resource.File e ->
      r := Resource.File (File.truncate e (Optint.Int63.to_int len))
  | r -> not_a_file "truncate" r

module Flow = struct
  module Impl = struct
    type tag = [ `Generic | `Unix ]
    type t = Fd.t

    let stat = stat
    let single_write t bufs = writev t bufs

    (* Copy using the [Read_source_buffer] optimisation.
       Avoids a copy if the source already has the data. *)
    let copy_with_rsb rsb dst =
      try
        while true do
          rsb (single_write dst)
        done
      with End_of_file -> ()

    let copy t ~src =
      let (Eio.Resource.T (src_t, ops)) = src in
      let module Src = (val Eio.Resource.get ops Eio.Flow.Pi.Source) in
      let rec aux = function
        | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb (rsb src_t) t
        | _ :: xs -> aux xs
        | [] -> Eio.Flow.Pi.simple_copy ~single_write t ~src
      in
      aux Src.read_methods

    let single_read t buf =
      match readv t [ buf ] with 0 -> raise End_of_file | got -> got

    let shutdown _ _ = Fmt.failwith "TODO: shutdown"
    let getsockopt _ _ = Fmt.failwith "in-memory flow has no socket options"
    let setsockopt _ _ _ = Fmt.failwith "in-memory flow has no socket options"
    let read_methods = []
    let pread t ~file_offset bufs = preadv t ~file_offset bufs
    let pwrite t ~file_offset bufs = pwritev t ~file_offset bufs
    let seek t off cmd = seek_fd t off cmd

    (* The in-memory store is already durable, so [sync] has nothing to flush. *)
    let sync _ = ()
    let truncate t len = truncate_fd t len
    let close = close
  end

  module type FLOW = sig
    include Eio.File.Pi.WRITE
    include Eio.Net.Pi.STREAM_SOCKET with type t := t
  end

  let flow_handler (type t tag)
      (module X : FLOW with type t = t and type tag = tag) :
      ( t,
        [ tag Eio.Net.stream_socket_ty | Eio.File.rw_ty ] )
      Eio.Resource.handler =
    Eio.Resource.handler
    @@ Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module X))
    @ Eio.Resource.bindings (Eio.File.Pi.rw (module X))

  let handler = flow_handler (module Impl)
  let of_fd fd = Eio.Resource.T (fd, handler)
end
