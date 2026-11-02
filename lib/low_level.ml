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

(* Eio makes use of the empty string for paths so we can start
   with "no path". Fpath does not allow that. *)
let check_path p = if String.equal p "" then "." else Eio_fs_path.normalize p

let () =
  let pp_eio_fs ppf = function
    | Eio_fs "" ->
        Fmt.pf ppf "eio-fs";
        true
    | Eio_fs s ->
        Fmt.pf ppf "eio-fs:%s" s;
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

  (* ELOOP, which Eio raises as [Symlink]. *)
  let symlink_loop () = raise (Eio.Fs.err Eio.Fs.Symlink)

  let badf () =
    let exn = Eio.Exn.create (Eio.Exn.X (Eio_fs "Bad file descriptor")) in
    raise exn

  let err ctx =
    let exn = Eio.Exn.create (Eio.Exn.X (Eio_fs ctx)) in
    raise exn
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
  type 'a t = { name : string; stat : Eio.File.Stat.t; entries : 'a list }

  let v ~parent name stat =
    { name; stat; entries = [ (stat.ino, "."); (parent, "..") ] }

  let is_empty dir = List.length dir.entries = 2

  let parent dir =
    List.find (function _, ".." -> true | _ -> false) dir.entries
end

module Symlink = struct
  type t = { name : string; stat : Eio.File.Stat.t; link_to : string }

  let v ~link_to name stat = { name; stat; link_to }
end

module File = struct
  (* [Literal] aliases an immutable string (an embedded asset) and is turned
     into an owned [Buffer] by the first write, so embedding a file costs no
     copy until someone mutates it. *)
  type contents = Literal of string | Buffer of Cstruct.t
  type t = { stat : Eio.File.Stat.t; name : string; contents : contents }

  let v ~stat name = { stat; name; contents = Buffer Cstruct.empty }
  let literal ~stat name contents = { stat; name; contents = Literal contents }

  let length = function
    | Literal s -> String.length s
    | Buffer b -> Cstruct.length b

  let buffer = function Literal s -> Cstruct.of_string s | Buffer b -> b

  let blit t ~src_off dst ~dst_off ~len =
    match t with
    | Literal s -> Cstruct.blit_from_string s src_off dst dst_off len
    | Buffer b -> Cstruct.blit b src_off dst dst_off len

  let to_string = function Literal s -> s | Buffer b -> Cstruct.to_string b
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

  let pp ppf = function
    | File f ->
        Fmt.pf ppf "file:%s:[%a]" f.name
          Fmt.(truncated ~max:12)
          (File.to_string f.contents)
    | Dir dir ->
        Fmt.pf ppf "dir:%s:%a" dir.name
          Fmt.(
            brackets
            @@ list ~sep:comma
                 (Fmt.parens @@ pair ~sep:comma int64 (quote string)))
          dir.entries
    | Symlink s -> Fmt.pf ppf "sym:%s -> %s" s.name s.link_to

  let name = function File f -> f.name | Dir d -> d.name | Symlink s -> s.name

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

(* A component that had to be a directory and is not: POSIX answers ENOTDIR,
   which every real Eio backend surfaces as an [Eio.Io]. Eio.Fs has no ENOTDIR
   constructor, so report the path does not resolve. *)
let as_dir ~label = function
  | Resource.Dir dir -> dir
  | r ->
      Fmt.kstr Error.not_found "as_dir(%s): not a directory: %a" label
        Resource.pp r

let truncate_file r =
  match !r with
  | Resource.File f ->
      let stat = { f.stat with size = Optint.Int63.zero } in
      r := File { f with contents = File.Buffer Cstruct.empty; stat }
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
          name = "/";
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

  let absolute_path (tree : t) (dir : _ Dir_node.t) =
    if dir.stat.ino = 0L then "/"
    else
      let rec loop (dir : _ Dir_node.t) acc =
        if Int64.equal dir.stat.ino 0L then "/" ^ acc
        else
          let parent = Entries.find (Dir_node.parent dir |> fst) tree in
          loop (as_dir ~label:"absolute" !parent) (dir.name ^ "/" ^ acc)
      in
      loop dir ""

  let is_confined ~root p =
    if Eio_fs_path.is_abs p then Eio_fs_path.is_rooted ~root p
    else
      let full_path = Eio_fs_path.append root p in
      Eio_fs_path.is_rooted ~root full_path

  let check_confinement ~root p =
    if is_confined ~root p then () else Error.permission_denied p

  (* path_resolution(7): a resolution follows at most [max_follows] symbolic
     links, as Linux and eio_posix do, and then fails with ELOOP. *)
  let max_follows = 40

  let follow_link follows =
    if !follows <= 0 then Error.symlink_loop ();
    decr follows

  (* The directory a symbolic link's target resolves from: the root for an
     absolute target, the directory holding the link otherwise. *)
  let link_base fs ~parent link_to =
    if Eio_fs_path.is_abs link_to then Entries.find 0L fs else parent

  let rec find_entry_relative_to_dir ~follows ~check ~follow fs dir
      (path : string) =
    let confinement_dir = as_dir ~label:"confinement-dir" !dir in
    let confinement_path = absolute_path fs confinement_dir in
    let check_confinement parent resource_name =
      let parent_path =
        absolute_path fs (as_dir ~label:"confinement" !parent)
      in
      let path_of_next_item =
        if resource_name = "/" then resource_name
        else begin
          Eio_fs_path.append parent_path resource_name
        end
      in
      check_confinement ~root:confinement_path
        (Eio_fs_path.to_dir_path path_of_next_item)
    in
    let rec loop ~parent r = function
      | [] -> (
          match !r with
          | Resource.Symlink s when follow ->
              follow_link follows;
              if check then check_confinement dir s.link_to;
              find_entry_relative_to_dir ~follows ~check ~follow fs
                (link_base fs ~parent s.link_to)
                s.link_to
          | _ ->
              let parent_path =
                absolute_path fs (as_dir ~label:"find-loop" !parent)
              in
              let resource_name = Resource.name !r in
              if resource_name = "/" then (resource_name, r)
              else begin
                (Eio_fs_path.append parent_path resource_name, r)
              end)
      | "" :: segs -> loop ~parent r segs
      | seg :: segs -> (
          if check then check_confinement dir seg;
          let entries, resolved_dir =
            entries_exn ~follows
              ~check:(if check then Some (check_confinement dir) else None)
              ~follow ~parent fs r
          in
          match
            List.find_opt (fun (_, name) -> String.equal seg name) entries
          with
          | None -> Error.not_found (String.concat "/" segs)
          | Some (ino, _) -> (
              match Entries.find_opt ino fs with
              | None -> Error.not_found (String.concat "/" segs)
              | Some n ->
                  if check then check_confinement dir (Resource.name !n);
                  loop ~parent:resolved_dir n segs))
    in
    let parent =
      match !dir with
      | Dir d ->
          let ino, _ = Dir_node.parent d in
          Entries.find ino fs
      | _ -> assert false
    in
    loop ~parent dir (Eio_fs_path.segs path)

  and entries_exn ~follows ~check ~follow ~parent fs :
      resource ref -> (Inode.t * string) list * resource ref =
   fun r ->
    match !r with
    | Resource.Dir dir -> (dir.entries, r)
    | Symlink sym ->
        follow_link follows;
        Option.iter (fun f -> f sym.link_to) check;
        let _, r =
          find_entry_relative_to_dir ~follows ~check:(Option.is_some check)
            ~follow fs
            (link_base fs ~parent sym.link_to)
            sym.link_to
        in
        entries_exn ~follows ~check ~follow ~parent fs r
    (* Walking through a non-directory: POSIX ENOTDIR, surfaced as an [Eio.Io]
         rather than a bare [Failure]. *)
    | r -> Fmt.kstr Error.not_found "not a directory: %a" Resource.pp r

  let lookup ~follow dir_fd path =
    let { tree; cwd; _ } = Dir.fs dir_fd in
    let root = Entries.find 0L tree in
    let follows = ref max_follows in
    match dir_fd with
    | Dir.Fs _ ->
        (* Full access: a relative path resolves against the current
           directory. *)
        let path =
          if Eio_fs_path.is_abs path then path else Eio_fs_path.append cwd path
        in
        let _, r =
          find_entry_relative_to_dir ~follows ~check:false ~follow tree root
            path
        in
        r
    | Dir.Cwd _ ->
        (* Confined to the current directory. *)
        let _, cwddir =
          find_entry_relative_to_dir ~follows ~check:false ~follow tree root cwd
        in
        let resource_path, r =
          find_entry_relative_to_dir ~follows ~check:true ~follow tree cwddir
            path
        in
        if is_confined ~root:cwd resource_path then r
        else Error.permission_denied resource_path
    | Dir.Dir_fd fd ->
        (* Confined to the descriptor's directory. A directory fd closed with
           its switch is EBADF, as for a real O_DIRECTORY fd, never a silent
           success. *)
        let dir = resource_of fd in
        let resource_path, r =
          find_entry_relative_to_dir ~follows ~check:true ~follow tree dir path
        in
        if
          is_confined
            ~root:(absolute_path tree (as_dir ~label:"lookup" !dir))
            resource_path
        then r
        else Error.permission_denied resource_path
end

let v ?(cwd = "/") () =
  {
    tree = Filesystem.empty ();
    fds = Fd_table.empty;
    last_ino = 0L;
    cwd;
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

let dir_base path = (Eio_fs_path.parent path, Eio_fs_path.basename path)

(* The entry [path] names, its last component not followed, or [None] when
   that component is missing. *)
let lookup_entry dir_fd path =
  match Filesystem.lookup ~follow:false dir_fd path with
  | r -> Some r
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> None

(* The path the symbolic link [path] to [link_to] resolves to. *)
let link_target path link_to =
  check_path (Eio_fs_path.append (Eio_fs_path.parent path) link_to)

(* Create the entry [path] with [add parent name] unless its name is taken,
   by a dangling symbolic link included: EEXIST, as mkdir(2) and symlink(2)
   answer. *)
let create_entry dir_fd path add =
  match lookup_entry dir_fd path with
  | Some _ -> Error.already_exists path
  | None ->
      let dir, base = dir_base path in
      add (Filesystem.lookup ~follow:true dir_fd dir) base

(* Create [path] in its parent directory, register the new file in the
   filesystem, and return its resource. *)
let new_file ~mode dir_fd path =
  (* A path in directory syntax names a directory, which O_CREAT never makes:
     macOS answers ENOENT, Linux EISDIR. *)
  if String.equal (Eio_fs_path.to_dir_path path) path then Error.not_found path;
  let dir, fname = dir_base path in
  let rdir = Filesystem.lookup ~follow:true dir_fd dir in
  let dir =
    match !rdir with
    | Resource.Dir d -> d
    (* open(2) with O_CREAT answers ENOTDIR when a component of the path prefix
       is not a directory. Eio.Fs has no ENOTDIR constructor, so report what a
       walk through such a component already reports here: the path does not
       resolve. *)
    | _ -> Error.not_found path
  in
  let fs = Dir.fs dir_fd in
  let stat = new_stat fs ~perm:(mode land 0o7777) () in
  let f = File.v ~stat fname in
  let dir = { dir with entries = (f.stat.ino, f.name) :: dir.entries } in
  rdir := Resource.Dir dir;
  let file = ref (Resource.File f) in
  Filesystem.add_file fs f.stat.ino file;
  file

(* The descriptor is registered with [sw] once the lock is released: a
   finished switch runs [close] at once, which takes the lock. *)
let open_locked ~mode dir_fd path flags =
  let path = check_path path in
  let is_create = Open_flags.(mem o_creat flags) in
  let is_excl = Open_flags.(mem o_excl flags) in
  let append = Open_flags.(mem o_append flags) in
  let truncate = Open_flags.(mem o_trunc flags) in
  let nofollow = Open_flags.(mem o_nofollow flags) in
  let directory = Open_flags.(mem o_directory flags) in
  let fs = Dir.fs dir_fd in
  let opened r =
    (match !r with
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
           its target is missing, creates that target. *)
        let follows = ref Filesystem.max_follows in
        let rec create path =
          match lookup_entry dir_fd path with
          | None -> new_file ~mode dir_fd path
          | Some _ when is_excl -> Error.already_exists path
          | Some r -> (
              match !r with
              | Resource.Symlink _ when nofollow -> Error.symlink_loop ()
              | Resource.Symlink s -> (
                  match Filesystem.lookup ~follow:true dir_fd path with
                  | r -> opened r
                  | exception Eio.Io (Eio.Fs.E (Not_found _), _) ->
                      Filesystem.follow_link follows;
                      create (link_target path s.link_to))
              | _ -> opened r)
        in
        create path
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
  let path = check_path path in
  let f = Filesystem.lookup ~follow dir_fd path in
  Resource.stat !f

let chmod ~follow dir_fd ~perm path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let r = Filesystem.lookup ~follow dir_fd path in
  r := Resource.set_stat !r { (Resource.stat !r) with perm }

let chown ~follow ?uid ?gid dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let r = Filesystem.lookup ~follow dir_fd path in
  let s = Resource.stat !r in
  let uid = Option.value ~default:s.uid uid in
  let gid = Option.value ~default:s.gid gid in
  r := Resource.set_stat !r { s with uid; gid }

let unlink dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  (match lookup_entry dir_fd path with
  | None -> Error.not_found path
  (* EISDIR, as Linux answers; macOS answers EPERM. *)
  | Some { contents = Resource.Dir _ } ->
      Error.err ("unlink: is a directory: " ^ path)
  | Some _ -> ());
  let dir, base = dir_base path in
  let r = Filesystem.lookup ~follow:true dir_fd dir in
  let d = as_dir ~label:"unlink" !r in
  let entries =
    List.filter (fun (_, name) -> not (String.equal base name)) d.entries
  in
  r := Resource.Dir { d with entries }

let mkdirat dir_fd ~perm path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let fs = Dir.fs dir_fd in
  let mkdir r name =
    let new_stat = new_stat fs ~kind:`Directory ~perm () in
    let d = as_dir ~label:"mkdirat" !r in
    let dir = Dir_node.v ~parent:d.stat.ino name new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file fs ino (ref (Resource.Dir dir));
    let entries = (ino, name) :: d.entries in
    r := Dir { d with entries }
  in
  create_entry dir_fd path mkdir

let embed dir_fd ~perm path ~contents =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let fs = Dir.fs dir_fd in
  let add r name =
    let stat =
      {
        (new_stat fs ~perm:(perm land 0o7777) ()) with
        size = Optint.Int63.of_int (String.length contents);
      }
    in
    let d = as_dir ~label:"embed" !r in
    let f = File.literal ~stat name contents in
    Filesystem.add_file fs stat.ino (ref (Resource.File f));
    r := Dir { d with entries = (stat.ino, name) :: d.entries }
  in
  create_entry dir_fd path add

let symlinkat ~link_to dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let fs = Dir.fs dir_fd in
  let symlink r name =
    let new_stat = new_stat fs ~kind:`Symbolic_link ~perm:0o644 () in
    let d = as_dir ~label:"symlink" !r in
    let dir = Symlink.v ~link_to name new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file fs ino (ref (Resource.Symlink dir));
    let entries = (ino, name) :: d.entries in
    r := Dir { d with entries }
  in
  create_entry dir_fd path symlink

let read_link dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let r = Filesystem.lookup ~follow:false dir_fd path in
  match !r with
  | Symlink s -> s.link_to
  | r -> Fmt.kstr Error.err "not a symlink: %a" Resource.pp r

let read_dir dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let r = Filesystem.lookup ~follow:true dir_fd path in
  match !r with
  | Resource.Dir dir ->
      List.map snd dir.entries
      |> List.filter (function "." | ".." -> false | _ -> true)
  | _ -> Error.err "Not a directory"

let read_dir_entries dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let path = check_path path in
  let r = Filesystem.lookup ~follow:true dir_fd path in
  match !r with
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
  let path = check_path path in
  (* A final symbolic link is not followed: ENOTDIR. *)
  let r = Filesystem.lookup ~follow:false dir_fd path in
  match !r with
  | Resource.Dir d ->
      if not (Dir_node.is_empty d) then Error.err "Directory is not empty"
      else begin
        let dir, base = dir_base path in
        let parent = Filesystem.lookup ~follow:true dir_fd dir in
        let r = as_dir ~label:"rmdir" !parent in
        let entries =
          List.filter (fun (_, name) -> not (String.equal name base)) r.entries
        in
        parent := Dir { r with entries }
      end
  | _ -> Error.err ("rmdir: not a directory: " ^ path)

(* Whether the directory [r] is the directory [ino] or lies below it. *)
let rec within tree r ino =
  match !r with
  | Resource.Dir d -> (
      Inode.equal d.stat.ino ino
      || (not (Inode.equal d.stat.ino 0L))
         &&
         match Entries.find_opt (fst (Dir_node.parent d)) tree with
         | Some parent -> within tree parent ino
         | None -> false)
  | _ -> false

(* rename(2) refuses to move a directory below itself (EINVAL), a directory
   over a non-empty directory (ENOTEMPTY) or over a non-directory (ENOTDIR),
   and a non-directory over a directory (EISDIR). Eio.Fs has no constructor
   for these, which eio_posix raises as a bare [Eio.Io]. *)
let check_rename tree ~src ~new_parent ~dst new_path =
  let ino = (Resource.stat !src).ino in
  match (!src, Option.map ( ! ) dst) with
  | Resource.Dir _, _ when within tree new_parent ino ->
      Error.err ("rename: invalid argument, into its own subtree: " ^ new_path)
  | Resource.Dir _, Some (Resource.Dir d) when not (Dir_node.is_empty d) ->
      Error.err ("rename: directory not empty: " ^ new_path)
  | Resource.Dir _, Some (File _ | Symlink _) ->
      Error.err ("rename: not a directory: " ^ new_path)
  | (File _ | Symlink _), Some (Resource.Dir _) ->
      Error.err ("rename: is a directory: " ^ new_path)
  | _ -> ()

let rename ?(replace = true) dir_fd old_path new_dir new_path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let old_path = check_path old_path in
  let new_path = check_path new_path in
  (* rename(2) does not move an entry between two file systems: EXDEV, which
     Eio.Fs has no constructor for. *)
  if Dir.fs dir_fd != Dir.fs new_dir then
    Error.err ("rename: invalid cross-device link: " ^ new_path);
  let tree = (Dir.fs dir_fd).tree in
  (* Rename the directory entry itself. In particular, a symlink's target need
     not exist and must not be followed while moving the link. *)
  let old_res = Filesystem.lookup ~follow:false dir_fd old_path in
  let ino = Resource.(stat !old_res).ino in
  let old_parent_path, old_name = dir_base old_path in
  let new_parent_path, new_name = dir_base new_path in
  let old_parent = Filesystem.lookup ~follow:true dir_fd old_parent_path in
  let new_parent = Filesystem.lookup ~follow:true new_dir new_parent_path in
  let existing =
    List.find_opt
      (fun (_, name) -> String.equal name new_name)
      (as_dir ~label:"rename-dst" !new_parent).entries
  in
  match existing with
  (* Two names of one entry: rename(2) does nothing. *)
  | Some (dst_ino, _) when Inode.equal dst_ino ino -> ()
  | _ -> (
      if (not replace) && Option.is_some existing then
        Error.already_exists new_path;
      let dst =
        Option.bind existing (fun (dst_ino, _) -> Entries.find_opt dst_ino tree)
      in
      check_rename tree ~src:old_res ~new_parent ~dst new_path;
      let without name entries =
        List.filter (fun (_, n) -> not (String.equal n name)) entries
      in
      (* A rename MOVES: the source loses [old_name] and the target gains
         [new_name] pointing at [ino]. Same-parent edits go to one entries
         list, so the move is atomic. *)
      if old_parent == new_parent then begin
        let d = as_dir ~label:"rename" !new_parent in
        let entries =
          (ino, new_name) :: without new_name (without old_name d.entries)
        in
        new_parent := Resource.Dir { d with entries }
      end
      else begin
        let dst = as_dir ~label:"rename-dst" !new_parent in
        let entries = (ino, new_name) :: without new_name dst.entries in
        new_parent := Resource.Dir { dst with entries };
        let src = as_dir ~label:"rename-src" !old_parent in
        old_parent :=
          Resource.Dir { src with entries = without old_name src.entries }
      end;
      (* The entry answers to its new name, and a directory's [..] names its
         new parent. *)
      old_res :=
        match !old_res with
        | Resource.Dir d ->
            let parent = (Resource.stat !new_parent).ino in
            let entries =
              List.map
                (fun ((_, name) as e) ->
                  if String.equal name ".." then (parent, "..") else e)
                d.entries
            in
            Resource.Dir { d with name = new_name; entries }
        | File f -> File { f with name = new_name }
        | Symlink l -> Symlink { l with name = new_name })

let stat t = locked t.Fd.fs @@ fun () -> Resource.stat !(resource_of t)

(* TODO: update access times *)

let blit_and_grow ~src ~dst ~off =
  let src_len = Cstruct.length src in
  let dst_len = Cstruct.length dst in
  let required_size = off + src_len in
  (* If there's enough space in the destination, then we can blit. *)
  if required_size <= dst_len then begin
    Cstruct.blit src 0 dst off src_len;
    dst
  end
  else begin
    (* Otherwise we need a big buf. *)
    let new_dst = Cstruct.create required_size in
    Cstruct.blit dst 0 new_dst 0 dst_len;
    Cstruct.blit src 0 new_dst off src_len;
    new_dst
  end

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
      let contents =
        File.Buffer
          (blit_and_grow ~src ~dst:(File.buffer e.contents)
             ~off:(Optint.Int63.to_int t.pos))
      in
      let new_size = Optint.Int63.of_int @@ File.length contents in
      let stat = { e.stat with size = new_size } in
      r := File { e with stat; contents };
      t.pos <- Optint.Int63.add t.pos (Optint.Int63.of_int (Cstruct.length src));
      Cstruct.length src
  | r -> Fmt.kstr Error.err "writev: not a file: %a" Resource.pp r

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
          File.blit e.contents ~src_off:posi buf ~dst_off:0 ~len:actual_len;
          pos := Optint.Int63.add !pos (Optint.Int63.of_int actual_len);
          read := !read + actual_len)
        bufs;
      t.pos <- !pos;
      !read
  | r -> Fmt.kstr Error.err "readv: not a file: %a" Resource.pp r

(* Positioned reads and writes address an explicit [file_offset] and never move
   the cursor, unlike [readv]/[writev]. This mirrors [Eio.File.pread]/[pwrite]. *)
let preadv t ~file_offset bufs =
  locked t.Fd.fs @@ fun () ->
  match !(resource_of t) with
  | Resource.File e ->
      let size = File.length e.contents in
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
  | r -> Fmt.kstr Error.err "pread: not a file: %a" Resource.pp r

let pwritev t ~file_offset bufs =
  locked t.Fd.fs @@ fun () ->
  let r = resource_of t in
  match !r with
  | Resource.File e ->
      let src = Cstruct.concat bufs in
      let off = Optint.Int63.to_int file_offset in
      let contents =
        File.Buffer (blit_and_grow ~src ~dst:(File.buffer e.contents) ~off)
      in
      let new_size = Optint.Int63.of_int (File.length contents) in
      let stat = { e.stat with size = new_size } in
      r := Resource.File { e with contents; stat };
      Cstruct.length src
  | r -> Fmt.kstr Error.err "pwrite: not a file: %a" Resource.pp r

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
      t.pos <- newpos;
      newpos
  | r -> Fmt.kstr Error.err "seek: not a file: %a" Resource.pp r

let truncate_fd t len =
  locked t.Fd.fs @@ fun () ->
  let r = resource_of t in
  match !r with
  | Resource.File e ->
      let target = Optint.Int63.to_int len in
      let cur = File.length e.contents in
      let contents =
        if target = cur then e.contents
        else begin
          let c = Cstruct.create target in
          File.blit e.contents ~src_off:0 c ~dst_off:0 ~len:(min cur target);
          File.Buffer c
        end
      in
      let stat = { e.stat with size = Optint.Int63.of_int target } in
      r := Resource.File { e with contents; stat }
  | r -> Fmt.kstr Error.err "truncate: not a file: %a" Resource.pp r

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
