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
  let not_found path = raise (Eio.Fs.err (Not_found (Eio_fs path)))
  let already_exists path = raise (Eio.Fs.err (Already_exists (Eio_fs path)))

  let permission_denied path =
    raise (Eio.Fs.err (Permission_denied (Eio_fs path)))

  let outside_sandbox path =
    raise (Eio.Fs.err (Permission_denied (Outside_sandbox path)))

  let absolute_path () = raise (Eio.Fs.err (Permission_denied Absolute_path))

  (* ELOOP, which Eio raises as [Symlink]. *)
  let symlink_loop () = raise (Eio.Fs.err Eio.Fs.Symlink)

  (* A caller's mistake, as eio_posix reports it. *)
  let badf () =
    invalid_arg "Eio_fs_mem: the descriptor is closed, so it takes no operation"

  let err ctx =
    let exn = Eio.Exn.create (Eio.Exn.X (Eio_fs ctx)) in
    raise exn

  (* ENOTDIR, which Eio.Fs has no constructor for and eio_posix raises as a
     bare [Eio.Io]: never [Not_found], which a [~missing_ok] would swallow. *)
  let not_a_directory path = err ("not a directory: " ^ path)

  (* Windows' errors that Eio.Fs has no constructor for, which eio_windows
     raises as a bare [Eio.Io] carrying Windows' own message. *)
  let invalid_name path =
    err
      ("The filename, directory name, or volume label syntax is incorrect: "
     ^ path)

  let directory_name path = err ("The directory name is invalid: " ^ path)

  let reparse_data path =
    err ("The data present in the reparse point buffer is invalid: " ^ path)

  let not_supported op = err (op ^ " is not supported on windows yet")
end

(* Who runs an operation, and the POSIX permission rules it meets. *)
module User = struct
  type t = { uid : int64; gid : int64; groups : int64 list; umask : int }

  let root = { uid = 0L; gid = 0L; groups = []; umask = 0o022 }
  let r = 0o4
  let w = 0o2
  let x = 0o1
  let is_root u = Int64.equal u.uid 0L
  let owns u (stat : Eio.File.Stat.t) = is_root u || Int64.equal stat.uid u.uid
  let in_group u g = Int64.equal g u.gid || List.exists (Int64.equal g) u.groups

  (* Linux clears setuid, and setgid with the group's execute bit, from a
     regular file a chown changes or a user other than root writes. *)
  let without_privileges (stat : Eio.File.Stat.t) =
    match stat.kind with
    | `Regular_file ->
        let perm = stat.perm land lnot 0o4000 in
        let perm =
          if perm land 0o010 <> 0 then perm land lnot 0o2000 else perm
        in
        { stat with perm }
    | _ -> stat

  (* [u] may [want] on [stat] by its owner's, group's or others' bits. Root
     needs no bit to read, write or search a directory, and one x bit to
     execute a file, as on Linux. *)
  let can u (stat : Eio.File.Stat.t) want =
    if is_root u then
      want land x = 0 || stat.kind = `Directory || stat.perm land 0o111 <> 0
    else
      let bits =
        if Int64.equal stat.uid u.uid then stat.perm lsr 6
        else if in_group u stat.gid then stat.perm lsr 3
        else stat.perm
      in
      bits land want = want
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

module Names = Map.Make (String)

type os = [ `Linux | `Macos | `Windows ]

let name_key = Name.key

module Dir_node = struct
  (* A node holds no name of its own: its names are the entries of the
     directories that hold it. [entries] maps each name's key, but [.] and
     [..], to the spelling it was made with and its inode, so a lookup costs
     the log of the directory's size. *)
  type t = {
    stat : Eio.File.Stat.t;
    parent : Inode.t;
    entries : (string * Inode.t) Names.t;
  }

  let v ~parent stat = { stat; parent; entries = Names.empty }
  let is_empty dir = Names.is_empty dir.entries

  let find os name dir =
    Option.map snd (Names.find_opt (Name.key os name) dir.entries)

  (* The spelling of the entry [name] matches. *)
  let spelling os name dir =
    Option.map fst (Names.find_opt (Name.key os name) dir.entries)

  let add os name ino dir =
    { dir with entries = Names.add (Name.key os name) (name, ino) dir.entries }

  let remove os name dir =
    { dir with entries = Names.remove (Name.key os name) dir.entries }

  let names dir =
    Names.fold (fun _ (name, ino) acc -> (name, ino) :: acc) dir.entries []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
end

module Symlink = struct
  (* [to_dir]: on Windows a link is to a directory or to a file, fixed when
     it is made. *)
  type t = { stat : Eio.File.Stat.t; link_to : string; to_dir : bool }

  let v ~link_to stat = { stat; link_to; to_dir = false }
end

module Pipe = struct
  type t = { chunks : Cstruct.t option Eio.Stream.t; stat : Eio.File.Stat.t }

  let make stat = { chunks = Eio.Stream.create max_int; stat }

  let write t buf =
    Eio.Stream.add t.chunks (Some buf);
    Cstruct.length buf

  let read t = Eio.Stream.take t.chunks
  let close t = Eio.Stream.add t.chunks None
end

module File = File_node
module Fd_table = Map.Make (Num)

(* What the tree holds. *)
module Fs_resource = struct
  type 'a with_stat = Eio.File.Stat.t * 'a

  type t =
    | Stdin of Eio.Flow.source_ty Eio.Flow.source with_stat
    | Stdout of Eio.Flow.sink_ty Eio.Flow.sink with_stat
    | Stderr of Eio.Flow.sink_ty Eio.Flow.sink with_stat
    | File of File.t
    | Dir of Dir_node.t
    | Symlink of Symlink.t
    | Pipe of Pipe.t

  let stat = function
    | File f -> { f.stat with blocks = File.blocks f }
    | Dir dir -> dir.stat
    | Symlink s -> s.stat
    | Stdin (s, _) | Stdout (s, _) | Stderr (s, _) -> s
    | Pipe p -> p.stat

  let set_stat r stat =
    match r with
    | File f -> File { f with stat }
    | Dir d -> Dir { d with stat }
    | Symlink s -> Symlink { s with stat }
    | Stdin (_, v) -> Stdin (stat, v)
    | Stdout (_, v) -> Stdout (stat, v)
    | Stderr (_, v) -> Stderr (stat, v)
    | Pipe p -> Pipe { p with stat }

  let update r f = r := set_stat !r (f (stat !r))
end

(* Times as Linux keeps them: a change to what an entry holds moves its mtime
   and ctime, a change to its inode alone its ctime. *)
let changed ~now (s : Eio.File.Stat.t) = { s with mtime = now; ctime = now }
let inode_changed ~now (s : Eio.File.Stat.t) = { s with ctime = now }

(* relatime, Linux's default: a read moves atime when it is not past mtime
   or ctime, or a day after it last moved. *)
let accessed ~now (s : Eio.File.Stat.t) =
  if s.atime <= s.mtime || s.atime <= s.ctime || now -. s.atime >= 86400. then
    { s with atime = now }
  else s

(* A directory's entries changed, and its subdirectories by [subdirs]: each
   one's [..] links it. *)
let entries_changed ~now ?(subdirs = 0) r =
  Fs_resource.update r (fun s ->
      { (changed ~now s) with nlink = Int64.add s.nlink (Int64.of_int subdirs) })

(* An inode that lost its only name: an open descriptor still stats it. *)
let unlinked ~now r =
  Fs_resource.update r (fun s -> { (inode_changed ~now s) with nlink = 0L })

type resource = Fs_resource.t

(* A component that had to be a directory and is not: ENOTDIR. *)
let as_dir ~path = function
  | Fs_resource.Dir dir -> dir
  | _ -> Error.not_a_directory path

(* O_TRUNC moves mtime and ctime, even on an empty file. *)
let truncate_file ~now r =
  match !r with
  | Fs_resource.File f ->
      let stat = changed ~now { f.stat with size = Optint.Int63.zero } in
      r := File (File.v ~stat)
  | Dir _ | Symlink _ | Stdin _ | Stderr _ | Stdout _ | Pipe _ -> ()

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
  os : os;
  device : int64; (* the [dev] of its stats, of its own *)
  mutable tree : resource ref Entries.t;
  mutable fds : resource ref Fd_table.t;
  mutable shared : unit Fd_table.t;
      (* the descriptors a program opened, not the administrator: on
         Windows they hold what they are open on *)
  mutable last_ino : Inode.t;
  cwd : string;
  mutable cwd_dir : resource ref option;
      (* The directory [cwd] named when first used: it follows that
         directory, as a descriptor does, whatever later takes the path. *)
  lock : Mutex.t;
  now : unit -> float; (* the clock its times are read from *)
}

type t = fs

(* A descriptor is one open file description: the offset and the O_APPEND
   flag are its own, never shared with another open of the same file. *)
module Fd = struct
  type t = {
    fs : fs;
    num : Num.t;
    append : bool;
    writable : bool; (* opened O_RDWR *)
    positional : bool;
        (* takes pread and pwrite: eio_windows takes neither, but the
           filesystem's administrator does *)
    user : User.t; (* who opened it *)
    mutable pos : Optint.Int63.t;
    mutable closed : bool;
  }

  let v ~append ~writable ~positional ~user fs num =
    {
      fs;
      num;
      append;
      writable;
      positional;
      user;
      pos = Optint.Int63.zero;
      closed = false;
    }

  let closed t = t.closed

  (* A closed handle answers on its own state, never by its number, which the
     next open may have been handed. *)
  let num t = if t.closed then Error.badf () else t.num
end

module Resource = struct
  type 'a t = ([> `Mem_fd ] as 'a) Eio.Resource.t

  type ('t, _, _) Eio.Resource.pi +=
    | T : ('t, 't -> Fd.t, [> `Mem_fd ]) Eio.Resource.pi

  let fd (Eio.Resource.T (t, ops)) = Eio.Resource.get ops T t

  let fd_opt (Eio.Resource.T (t, ops)) =
    match Eio.Resource.get_opt ops T with Some f -> Some (f t) | None -> None
end

module Dir = struct
  type at = Fs of fs | Cwd of fs | Dir_fd of Fd.t
  type fd = { at : at; user : User.t; admin : bool }

  let fs t = match t.at with Fs fs | Cwd fs -> fs | Dir_fd fd -> fd.fs
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

(* A new entry is owned by the user who makes it, in the group of a setgid
   [parent], from which a new directory also takes the setgid bit. A new
   file in a group its maker is not in loses setgid, as Linux strips it at
   create (mode_strip_sgid). *)
let new_stat fs ~(user : User.t) ?parent ?kind ~perm () =
  fs.last_ino <- Int64.succ fs.last_ino;
  let now = fs.now () in
  let kind = Option.value ~default:default_stat.kind kind in
  let gid, perm =
    match (fs.os, parent) with
    (* macOS, as BSD, gives every new entry its directory's group. *)
    | `Macos, Some (p : Eio.File.Stat.t) -> (p.gid, perm)
    | `Linux, Some (p : Eio.File.Stat.t) when p.perm land 0o2000 <> 0 ->
        (p.gid, if kind = `Directory then perm lor 0o2000 else perm)
    | _, (Some _ | None) -> (user.gid, perm)
  in
  let perm =
    if
      kind <> `Directory
      && perm land 0o2000 <> 0
      && (not (User.is_root user))
      && not (User.in_group user gid)
    then perm land lnot 0o2000
    else perm
  in
  (* Windows keeps no owner and no mode: eio_windows reports a file's as 666
     and a directory's as 777. *)
  let uid, gid, perm =
    match (fs.os, kind) with
    | `Windows, `Regular_file -> (0L, 0L, 0o666)
    | `Windows, _ -> (0L, 0L, 0o777)
    | (`Linux | `Macos), _ -> (user.uid, gid, perm)
  in
  {
    default_stat with
    dev = fs.device;
    ino = fs.last_ino;
    kind;
    perm;
    uid;
    gid;
    nlink = (if kind = `Directory then 2L else 1L);
    atime = now;
    mtime = now;
    ctime = now;
  }

module Filesystem = struct
  type t = resource ref Entries.t

  let empty ~device ~(owner : User.t) ~now : t =
    let root =
      Fs_resource.Dir
        {
          stat =
            {
              default_stat with
              dev = device;
              size = Optint.Int63.of_int 4096;
              ino = 0L;
              kind = `Directory;
              perm = 0o755;
              uid = owner.uid;
              gid = owner.gid;
              nlink = 2L;
              atime = now;
              mtime = now;
              ctime = now;
            };
          parent = 0L;
          entries = Names.empty;
        }
    in
    Entries.empty |> Entries.add 0L (ref root)

  let add_file fs ino r = fs.tree <- Entries.add ino r fs.tree
  let root (tree : t) = Entries.find 0L tree

  (* A removed directory's parent may be gone too: ENOENT. *)
  let parent_dir (tree : t) r =
    match Entries.find_opt (as_dir ~path:".." !r).parent tree with
    | Some parent -> parent
    | None -> Error.not_found ".."

  (* Whether the directory [r] is the directory [ino] or lies below it, by the
     parent links of the tree itself. *)
  let rec within (tree : t) r ino =
    match !r with
    | Fs_resource.Dir d -> (
        Inode.equal d.stat.ino ino
        || (not (Inode.equal d.stat.ino 0L))
           &&
           match Entries.find_opt d.parent tree with
           | Some parent -> within tree parent ino
           | None -> false)
    | _ -> false

  (* path_resolution(7): a resolution follows at most 40 symbolic links on
     Linux, MAXSYMLINKS, 32, on macOS, and then fails with ELOOP. *)
  let max_follows : os -> int = function
    | `Linux | `Windows -> 40
    | `Macos -> 32

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
    os : os;
    tree : t;
    user : User.t;
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
    List.filter
      (fun name -> not (String.equal name ""))
      (String.split_on_char '/' path)

  (* Where [path] starts: the root for an absolute path, which a confined walk
     refuses, and the directory [w] is in otherwise. *)
  let enter w path =
    if not (String.starts_with ~prefix:"/" path) then w
    else
      match w.base with
      | Some _ -> Error.absolute_path ()
      | None -> { w with dirs = [ root w.tree ] }

  (* A lookup in a directory needs its search (x) bit: EACCES. *)
  let search_in w name =
    if not (User.can w.user (Fs_resource.stat !(here w)) User.x) then
      Error.permission_denied name

  (* The entry [name] names in the directory [w] is in, [.] and [..] included,
     or [None]. *)
  let child w name =
    search_in w name;
    match name with
    | "." -> Some (here w)
    | ".." -> Some (here (up w []))
    | name -> (
        let d = as_dir ~path:name !(here w) in
        match Dir_node.find w.os name d with
        | None -> None
        | Some ino -> Entries.find_opt ino w.tree)

  (* fs.protected_symlinks, 1 on Linux distributions: a last component that
     is a link in a sticky world-writable directory is followed only by the
     link's owner, or when the directory's owner owns it, root included:
     EACCES. *)
  let may_follow w name (s : Symlink.t) =
    let dir = Fs_resource.stat !(here w) in
    if
      w.os = `Linux
      && dir.perm land 0o1002 = 0o1002
      && (not (Int64.equal s.stat.uid w.user.uid))
      && not (Int64.equal s.stat.uid dir.uid)
    then Error.permission_denied name

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
    | ".." :: rest ->
        search_in w "..";
        walk ~after (up w (rest @ after)) rest
    | name :: rest -> walk ~after (into w name) rest

  and into w name =
    match child w name with
    | None -> Error.not_found name
    | Some r -> (
        match !r with
        | Fs_resource.Dir _ -> push w r
        | Symlink s ->
            follow_link w.follows;
            let target = link_target name s in
            walk (enter w target) (names target)
        | File _ | Stdout _ | Stderr _ | Stdin _ | Pipe _ ->
            Error.not_a_directory name)

  (* [locate w path] is the walk into the directory holding [path]'s last
     component, that component (["."] when [path] has none) and whether
     [path] ends in a separator. A confined walk checks by the tree's own
     parent links that the directory it reached is below its base. The walk
     needs search permission on that directory before anything looks at the
     last component, [.] and [..] included, as path_resolution(7) checks it
     for each component; [~search:false] leaves that to the caller. *)
  let locate ?(search = true) w path =
    (* NUL ends a C path: ENOENT. A name holds at most NAME_MAX bytes. *)
    if String.exists (Char.equal '\000') path then Error.not_found path;
    if List.exists (Name.too_long w.os) (names path) then
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
    (* A last [..] at the base names the directory above it. *)
    | Some _ when String.equal name ".." && List.length w.dirs = 1 ->
        Error.outside_sandbox name
    | _ -> ());
    if search && names path <> [] then search_in w name;
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
    | Fs_resource.Symlink s when follow || slash ->
        may_follow w name s;
        follow_link w.follows;
        let w, name, slash' = locate w (link_target name s) in
        final ~follow:true w name ~slash:(slash || slash')
    | Fs_resource.Dir _ -> r
    | _ when slash -> Error.not_a_directory name
    | _ -> r

  (* Windows, as eio_windows answers through Eio_utils.Nt_path, which this
     uses for every path rule rather than a second copy of them. A path is a
     Win32 path, made lexical before any link is followed: a [..] applies to
     the path as written, and a trailing separator is dropped. Beneath a
     directory a name is literal, trailing dots and spaces and reserved
     names included, and a [..] may not leave the directory, even to come
     back. The root of the tree holds a directory for each drive, ["C:"]
     first. *)
  module Nt = Eio_utils.Nt_path

  (* OCaml's runtime reads a path that is not UTF-8 in the ANSI code page,
     1252 on a Western Windows: bytes 0xA0 to 0xFF are Latin-1. *)
  let cp1252 =
    [|
      0x20AC;
      0x81;
      0x201A;
      0x0192;
      0x201E;
      0x2026;
      0x2020;
      0x2021;
      0x02C6;
      0x2030;
      0x0160;
      0x2039;
      0x0152;
      0x8D;
      0x017D;
      0x8F;
      0x90;
      0x2018;
      0x2019;
      0x201C;
      0x201D;
      0x2022;
      0x2013;
      0x2014;
      0x02DC;
      0x2122;
      0x0161;
      0x203A;
      0x0153;
      0x9D;
      0x017E;
      0x0178;
    |]

  let encode s =
    if String.is_valid_utf_8 s then s
    else begin
      let b = Buffer.create (2 * String.length s) in
      String.iter
        (fun c ->
          let n = Char.code c in
          let u = if n >= 0x80 && n < 0xA0 then cp1252.(n - 0x80) else n in
          Buffer.add_utf_8_uchar b (Uchar.of_int u))
        s;
      Buffer.contents b
    end

  (* The Win32 path of the directory [r]: ["C:\\"] for a drive. *)
  let rec win_path tree r =
    let d = as_dir ~path:"." !r in
    if Inode.equal d.stat.ino 0L then ""
    else
      let parent =
        match Entries.find_opt d.parent tree with
        | Some p -> p
        | None -> Error.not_found "."
      in
      let name =
        Names.fold
          (fun _ (n, ino) acc -> if Inode.equal ino d.stat.ino then n else acc)
          (as_dir ~path:"." !parent).entries ""
      in
      match win_path tree parent with
      | "" -> name ^ "\\"
      | up -> Nt.join up name

  let components p =
    List.filter (fun c -> c <> "") (String.split_on_char '\\' p)

  (* An NT path's components from its drive. UNC and device paths are not
     modelled: refused. *)
  let drive_components path nt =
    let rest =
      if String.starts_with ~prefix:"\\??\\" nt then
        String.sub nt 4 (String.length nt - 4)
      else nt
    in
    match components rest with
    | d :: _ as comps when String.length d = 2 && d.[1] = ':' -> comps
    | _ -> Error.permission_denied path

  let join_rest p rest =
    if rest = [] then p else Nt.join p (String.concat "\\" rest)

  (* [win_resolve ~follow_last dir_fd path] is the walk into the directory
     holding [path]'s last component and that component (["."] for the
     directory itself), every link before it followed, and the last one too
     if [follow_last]. *)
  let rec win_resolve ~follow_last (dir_fd : Dir.fd) path =
    let fs = Dir.fs dir_fd in
    let tree = fs.tree in
    let path = encode path in
    if String.exists (Char.equal '\000') path then Error.not_found path;
    let follows = ref (max_follows fs.os) in
    let at r =
      { os = fs.os; tree; user = User.root; base = None; follows; dirs = [ r ] }
    in
    let check c = if Name.too_long fs.os c then Error.invalid_name path in
    let rec walk relink r seen = function
      | [] -> (at r, ".")
      | [ last ] -> (
          check last;
          match child (at r) last with
          | Some { contents = Fs_resource.Symlink s } when follow_last ->
              follow_link follows;
              relink (List.rev (last :: seen)) s []
          | _ -> (at r, last))
      | c :: rest -> (
          check c;
          match child (at r) c with
          | None -> Error.not_found path
          | Some r' -> (
              match !r' with
              | Fs_resource.Dir _ -> walk relink r' (c :: seen) rest
              | Symlink s ->
                  follow_link follows;
                  relink (List.rev (c :: seen)) s rest
              | File _ | Stdout _ | Stderr _ | Stdin _ | Pipe _ ->
                  Error.directory_name path))
    in
    let target (s : Symlink.t) = Nt.link_target ~cwd:fs.cwd s.link_to in
    let rec from_drive nt =
      walk unrestricted (root tree) [] (drive_components path nt)
    and unrestricted link s rest =
      let parent = Nt.dirname (String.concat "\\" link) in
      match Nt.resolve_link ~parent (target s) with
      | None -> Error.not_found path
      | Some nt -> from_drive (join_rest nt rest)
    in
    let sandbox r0 =
      if not (Nt.is_relative path) then Error.absolute_path ();
      let root_path = lazy (win_path tree r0) in
      let rec from_rel rel = walk confined r0 [] (components rel)
      and confined link s rest =
        let link = String.concat "\\" link in
        match
          Nt.resolve_link ~root:(Lazy.force root_path) ~parent:(Nt.dirname link)
            (target s)
        with
        | None -> Error.outside_sandbox path
        | Some rel -> from_rel (join_rest rel rest)
      in
      match Nt.relative path with
      | None -> Error.outside_sandbox path
      | Some rel -> from_rel rel
    in
    match dir_fd.at with
    | Dir.Fs _ -> from_drive (Nt.to_nt ~cwd:fs.cwd path)
    | Dir.Cwd _ -> sandbox (win_cwd fs)
    | Dir.Dir_fd fd -> sandbox (resource_of fd)

  (* The directory [fs.cwd] named when first used: it then follows that
     directory, as a descriptor does. *)
  and win_cwd fs =
    match fs.cwd_dir with
    | Some r -> r
    | None ->
        let fd = { Dir.at = Dir.Fs fs; user = User.root; admin = true } in
        let w, name = win_resolve ~follow_last:true fd fs.cwd in
        let r =
          match child w name with Some r -> r | None -> Error.not_found fs.cwd
        in
        fs.cwd_dir <- Some r;
        r

  let win_lookup ~follow dir_fd path =
    let w, name = win_resolve ~follow_last:follow dir_fd path in
    match child w name with Some r -> r | None -> Error.not_found path

  (* Where [path] given to [dir_fd] starts. Full access resolves a relative
     path against the current directory, which an absolute path need not
     reach; the current directory and an open directory confine the walk to
     themselves. *)
  let start dir_fd path =
    let fs = Dir.fs dir_fd in
    let tree = fs.tree in
    let at_root =
      {
        os = fs.os;
        tree;
        user = dir_fd.Dir.user;
        base = None;
        follows = ref (max_follows fs.os);
        dirs = [ root tree ];
      }
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
    match dir_fd.Dir.at with
    | Dir.Fs _ when String.starts_with ~prefix:"/" path -> at_root
    | Dir.Fs _ -> cwd ()
    | Dir.Cwd _ ->
        let w = cwd () in
        { w with base = Some (Fs_resource.stat !(here w)).ino }
    | Dir.Dir_fd fd ->
        let r = resource_of fd in
        { at_root with base = Some (Fs_resource.stat !r).ino; dirs = [ r ] }

  let windows dir_fd = (Dir.fs dir_fd).os = `Windows

  let locate_from ?search ?(follow_last = false) dir_fd path =
    if windows dir_fd then
      let w, name = win_resolve ~follow_last dir_fd path in
      (w, name, false)
    else locate ?search (start dir_fd path) path

  let lookup ~follow dir_fd path =
    if windows dir_fd then win_lookup ~follow dir_fd path
    else
      let w, name, slash = locate_from dir_fd path in
      final ~follow w name ~slash
end

(* Devices are numbered up from [Int64.min_int], which no host device takes:
   no two filesystems of a process share one, nor one and the host. *)
let devices = Atomic.make 0

let fresh_device () =
  Int64.add Int64.min_int (Int64.of_int (Atomic.fetch_and_add devices 1))

let os_of = function Some (#os as os) -> os | None -> `Linux

(* The drive [letter]'s root, a directory of the tree's root. *)
let add_drive fs letter =
  let root = Filesystem.root fs.tree in
  let d = as_dir ~path:"." !root in
  let stat =
    new_stat fs ~user:User.root ~parent:d.stat ~kind:`Directory ~perm:0o777 ()
  in
  Filesystem.add_file fs stat.ino
    (ref (Fs_resource.Dir (Dir_node.v ~parent:0L stat)));
  root :=
    Fs_resource.Dir
      (Dir_node.add fs.os
         (Fmt.str "%c:" (Char.uppercase_ascii letter))
         stat.ino d)

let null = Eio.Flow.null

let std_table ?stdin ?stdout ?stderr fs user =
  let open Fs_resource in
  let stat = new_stat fs ~user ~kind:`Character_special ~perm:0o400 () in
  let stdin =
    match stdin with
    | Some f -> (f :> Eio.Flow.source_ty Eio.Flow.source)
    | None -> null
  in
  let stdout =
    match stdout with
    | Some f -> (f :> Eio.Flow.sink_ty Eio.Flow.sink)
    | None -> null
  in
  let stderr =
    match stderr with
    | Some f -> (f :> Eio.Flow.sink_ty Eio.Flow.sink)
    | None -> null
  in
  Fd_table.add (Num.of_int 0) (ref (Stdin (stat, stdin))) Fd_table.empty
  |> Fd_table.add (Num.of_int 1) (ref (Stdout (stat, stdout)))
  |> Fd_table.add (Num.of_int 2) (ref (Stderr (stat, stderr)))

let v ?clock ?cwd ?(owner = User.root) ?os ?drives ?stdin ?stdout ?stderr () =
  let now =
    match clock with Some c -> fun () -> Eio.Time.now c | None -> Fun.const 0.
  in
  let os = os_of os in
  let drives =
    match (os, drives) with
    | `Windows, None -> [ 'C' ]
    | `Windows, Some drives -> drives
    | (`Linux | `Macos), None -> []
    | (`Linux | `Macos), Some _ ->
        invalid_arg "Eio_fs_mem: only Windows has drives"
  in
  let cwd =
    match (cwd, drives) with
    | Some cwd, _ -> cwd
    | None, d :: _ -> Fmt.str "%c:\\" (Char.uppercase_ascii d)
    | None, [] -> "/"
  in
  let device = fresh_device () in
  let fs =
    {
      os;
      device;
      tree = Filesystem.empty ~device ~owner ~now:(now ());
      fds = Fd_table.empty;
      shared = Fd_table.empty;
      last_ino = 0L;
      cwd;
      cwd_dir = None;
      lock = Mutex.create ();
      now;
    }
  in
  let stdio = std_table ?stdin ?stderr ?stdout fs owner in
  let fs = { fs with fds = stdio } in
  List.iter (add_drive fs) drives;
  fs

let windows dir_fd = Filesystem.windows dir_fd

let root_path fs =
  if fs.os = `Windows then String.sub fs.cwd 0 2 ^ "\\" else "/"

let win_path (fd : Fd.t) =
  locked fd.fs @@ fun () -> Filesystem.win_path fd.fs.tree (resource_of fd)

let close_resource = function
  | Fs_resource.Pipe p ->
      Pipe.close p;
      Fs_resource.Pipe p
  | v -> v

(* Closing an already-closed handle does nothing, as [Eio.Fs_resource.close]
   specifies and eio's posix backend does. A handle with two owners, a caller
   closing early in front of its switch, is the ordinary reason. *)
let close (fd : Fd.t) =
  locked fd.fs @@ fun () ->
  if not fd.closed then begin
    fd.closed <- true;
    let r = Fd_table.find fd.num fd.fs.fds in
    r := close_resource !r;
    fd.fs.fds <- Fd_table.remove fd.num fd.fs.fds;
    fd.fs.shared <- Fd_table.remove fd.num fd.fs.shared
  end

(* Who a permission check runs as: Windows ignores Unix permissions. *)
let checker (dir_fd : Dir.fd) =
  if (Dir.fs dir_fd).os = `Windows then User.root else dir_fd.user

(* The inodes of [r] and of all below it. *)
let rec inodes tree r acc =
  let s = Fs_resource.stat !r in
  match !r with
  | Fs_resource.Dir d ->
      Names.fold
        (fun _ (_, ino) acc ->
          match Entries.find_opt ino tree with
          | Some r -> inodes tree r acc
          | None -> acc)
        d.entries (s.ino :: acc)
  | File _ | Symlink _ -> s.ino :: acc
  | Stdout _ | Stderr _ | Stdin _ | Pipe _ -> acc

(* Whether a program's descriptor holds [r] or anything below it open:
   eio_windows opens every handle without FILE_SHARE_DELETE, so Windows then
   refuses to move or remove it. The administrator's, as a crash image is
   rebuilt through, hold nothing. *)
let held_open fs r =
  let inos = inodes fs.tree r [] in
  Fd_table.exists
    (fun num r' ->
      Fd_table.mem num fs.shared && List.mem (Fs_resource.stat !r').ino inos)
    fs.fds

(* open(2): opening an existing file requires the permission bits the access
   mode asks for; the mode given at creation does not restrict the creating
   open itself. This backend opens files O_RDONLY or O_RDWR, so the read bit
   is always required and the write bit with o_rdwr. A directory opened to be
   read needs its read bit too; one opened with O_DIRECTORY needs none on
   Linux, as eio_posix opens it O_PATH, and its read bit on macOS, which has
   no O_PATH. *)
let check_access ~os ~user path flags r =
  let want = if Open_flags.(mem o_rdwr flags) then User.(r lor w) else User.r in
  match !r with
  | Fs_resource.File f when not (User.can user f.stat want) ->
      Error.permission_denied path
  | Fs_resource.Dir d
    when ((not Open_flags.(mem o_directory flags)) || os = `Macos)
         && not (User.can user d.stat want) ->
      Error.permission_denied path
  | _ -> ()

(* Making or removing an entry needs write and search on its directory. *)
let check_dir_write ~user rdir path =
  if not (User.can user (Fs_resource.stat !rdir) User.(w lor x)) then
    Error.permission_denied path

(* In a sticky directory only the entry's or the directory's owner removes or
   renames an entry: EPERM. *)
let check_sticky ~user rdir victim path =
  let dir = Fs_resource.stat !rdir in
  if
    dir.perm land 0o1000 <> 0
    && (not (User.owns user (Fs_resource.stat !victim)))
    && not (User.owns user dir)
  then Error.permission_denied path

(* The mode of a new file or directory: [perm] without the umask's bits. *)
let masked ~(user : User.t) perm = perm land 0o7777 land lnot user.umask

(* A directory removed while a handle held it takes no new entry: ENOENT. *)
let check_live fs rdir path =
  if not (Entries.mem (Fs_resource.stat !rdir).ino fs.tree) then
    Error.not_found path

(* [.] and [..] name a directory already there, never an entry to make,
   remove or move. *)
let is_dot name = String.equal name "." || String.equal name ".."

(* APFS stores only UTF-8 names: EILSEQ. *)
let check_name fs name =
  if fs.os = `Macos && not (String.is_valid_utf_8 name) then
    Error.err ("illegal byte sequence: " ^ name)

(* On macOS a last component in directory syntax that is a symbolic link is
   followed to the entry it names, even to make, remove or move it: [w] and
   [name] then name that entry. *)
let rec through_link ~slash (w : Filesystem.walk) name =
  if w.os = `Linux || not slash then (w, name)
  else
    match Filesystem.child w name with
    | Some { contents = Fs_resource.Symlink s } ->
        Filesystem.follow_link w.follows;
        let w, name, _ = Filesystem.locate w (Filesystem.link_target name s) in
        through_link ~slash w name
    | _ -> (w, name)

(* Create the entry [path] with [add parent name] unless its name is taken,
   by a dangling symbolic link included: EEXIST, as mkdir(2) and symlink(2)
   answer. Only a directory is made under a name with a trailing slash: ENOENT
   otherwise. *)
let new_entry ?(dir = false) dir_fd path add =
  let w, name, slash = Filesystem.locate_from dir_fd path in
  let w, name = if dir then through_link ~slash w name else (w, name) in
  match Filesystem.child w name with
  (* macOS looks a name in directory syntax up as a directory first. *)
  | Some { contents = Fs_resource.File _ | Symlink _ }
    when slash && w.os = `Macos ->
      Error.not_a_directory path
  | Some _ -> Error.already_exists path
  | None when slash && not dir -> Error.not_found path
  | None ->
      check_name (Dir.fs dir_fd) name;
      check_live (Dir.fs dir_fd) (Filesystem.here w) path;
      check_dir_write ~user:(checker dir_fd) (Filesystem.here w) path;
      add (Filesystem.here w) name

(* Create [name] in the directory [w] is in, register the new file in the
   filesystem, and return its resource. *)
let new_file ~mode fs (w : Filesystem.walk) name =
  let rdir = Filesystem.here w in
  check_live fs rdir name;
  check_dir_write ~user:w.user rdir name;
  let dir = as_dir ~path:name !rdir in
  let stat =
    new_stat fs ~user:w.user ~parent:dir.stat ~perm:(masked ~user:w.user mode)
      ()
  in
  let f = File.v ~stat in
  check_name fs name;
  rdir := Fs_resource.Dir (Dir_node.add fs.os name f.stat.ino dir);
  entries_changed ~now:stat.ctime rdir;
  let file = ref (Fs_resource.File f) in
  Filesystem.add_file fs f.stat.ino file;
  file

(* An existing resource [r] opened by [path] with [flags]: write access to a
   directory is EISDIR, O_DIRECTORY on anything else ENOTDIR. *)
let opened ~fs ~user path flags r =
  (* eio_windows opens a directory only as one, and a file never as one. *)
  (match (fs.os, !r) with
  | `Windows, Fs_resource.Dir _ when not Open_flags.(mem o_directory flags) ->
      Error.permission_denied path
  | `Windows, (File _ | Symlink _) when Open_flags.(mem o_directory flags) ->
      Error.directory_name path
  | _ -> ());
  (match !r with
  | Fs_resource.Dir _ when Open_flags.(mem o_rdwr flags) ->
      Error.err ("open: is a directory: " ^ path)
  | Fs_resource.Dir _ -> ()
  | _ when Open_flags.(mem o_directory flags) ->
      Error.err ("open: not a directory: " ^ path)
  | _ -> ());
  check_access ~os:fs.os ~user path flags r;
  if Open_flags.(mem o_trunc flags) then truncate_file ~now:(fs.now ()) r;
  r

(* O_CREAT looks at the last component without following it. A name in
   directory syntax there, given or a link's target, is EISDIR, as Linux
   answers whether it exists or not; a last [.] is the directory itself. A
   symbolic link there, dangling or not, makes O_EXCL fail with EEXIST and
   O_NOFOLLOW with ELOOP; otherwise the open follows it and opens its target,
   or creates it, resolved from the directory holding the link. macOS looks a
   name in directory syntax up instead: ENOENT if it is missing, ENOTDIR if it
   is not a directory, EISDIR if it is. *)
let rec open_creating ~mode ~fs ~opened path flags (w : Filesystem.walk) name
    ~slash =
  let child = Filesystem.child w name in
  if slash && (not (is_dot name)) && w.os = `Macos then begin
    if Option.is_none child then Error.not_found path;
    ignore (Filesystem.final ~follow:true w name ~slash)
  end;
  if slash && not (is_dot name) then Error.err ("open: is a directory: " ^ path);
  match child with
  | None -> new_file ~mode fs w name
  | Some { contents = Fs_resource.Dir _ } when w.os = `Windows ->
      Error.permission_denied path
  | Some _ when Open_flags.(mem o_excl flags) -> Error.already_exists path
  | Some r -> (
      match !r with
      | Fs_resource.Symlink _ when Open_flags.(mem o_nofollow flags) ->
          Error.symlink_loop ()
      | Fs_resource.Symlink s ->
          Filesystem.may_follow w name s;
          Filesystem.follow_link w.follows;
          let w, name, slash =
            Filesystem.locate w (Filesystem.link_target name s)
          in
          open_creating ~mode ~fs ~opened path flags w name ~slash
      | _ -> opened r)

(* The descriptor is registered with [sw] once the lock is released: a
   finished switch runs [close] at once, which takes the lock. *)
let open_locked ~mode dir_fd path flags =
  let append = Open_flags.(mem o_append flags) in
  let writable = Open_flags.(mem o_rdwr flags) in
  let fs = Dir.fs dir_fd in
  let opened = opened ~fs ~user:(checker dir_fd) path flags in
  let r =
    if Open_flags.(mem o_creat flags) || Open_flags.(mem o_excl flags) then
      let follow_last =
        not Open_flags.(mem o_excl flags || mem o_nofollow flags)
      in
      let w, name, slash = Filesystem.locate_from ~follow_last dir_fd path in
      open_creating ~mode ~fs ~opened path flags w name ~slash
    else
      let follow = not Open_flags.(mem o_nofollow flags) in
      let r = Filesystem.lookup ~follow dir_fd path in
      match !r with
      (* O_NOFOLLOW on a symbolic link: ELOOP. *)
      | Fs_resource.Symlink _ -> Error.symlink_loop ()
      | _ -> opened r
  in
  let num = next_fd_of_fds fs.fds in
  fs.fds <- Fd_table.add num r fs.fds;
  if not dir_fd.admin then fs.shared <- Fd_table.add num () fs.shared;
  let positional = fs.os <> `Windows || dir_fd.admin in
  Fd.v ~append ~writable ~positional ~user:dir_fd.user fs num

let descriptors dir_fd =
  let fs = Dir.fs dir_fd in
  locked fs @@ fun () -> Fd_table.cardinal fs.fds

let openat ~mode ~sw dir_fd path flags =
  let fd =
    locked (Dir.fs dir_fd) @@ fun () -> open_locked ~mode dir_fd path flags
  in
  Eio.Switch.on_release sw (fun () -> close fd);
  fd

let pipe ~sw user fs =
  let stat = default_stat in
  let pipe = Pipe.make stat in
  let resource = ref (Fs_resource.Pipe pipe) in
  let r = next_fd_of_fds fs.fds in
  fs.fds <- Fd_table.add r resource fs.fds;
  let w = next_fd_of_fds fs.fds in
  fs.fds <- Fd_table.add w resource fs.fds;
  let r = Fd.v ~append:false ~writable:false ~positional:false ~user fs r in
  let w = Fd.v ~append:false ~writable:true ~positional:false ~user fs w in
  Eio.Switch.on_release sw (fun () -> close r);
  Eio.Switch.on_release sw (fun () -> close w);
  (r, w)

let statat ~follow dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  Fs_resource.stat !(Filesystem.lookup ~follow dir_fd path)

let held dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  match !(Filesystem.lookup ~follow:true dir_fd path) with
  | Fs_resource.File f -> Some f.held
  | Dir _ | Symlink _ | Stdout _ | Stderr _ | Stdin _ | Pipe _ -> None

(* What a disk kept of an inode's times, as a power cut leaves them. *)
let set_times dir_fd path ~atime ~mtime ~ctime =
  locked (Dir.fs dir_fd) @@ fun () ->
  Fs_resource.update (Filesystem.lookup ~follow:false dir_fd path) (fun s ->
      { s with atime; mtime; ctime })

(* chmod(2): only the owner or root: EPERM. *)
let chmod ~follow dir_fd ~perm path =
  locked (Dir.fs dir_fd) @@ fun () ->
  (* Windows keeps no Unix mode: eio_windows refuses a chmod below a
     directory it opened, and passes one through the full-access root to
     the C runtime, which keeps nothing of it here. The administrator's, as
     a crash image restores one, keeps nothing either. *)
  if
    Filesystem.windows dir_fd && (not dir_fd.admin)
    && match dir_fd.at with Dir.Fs _ -> false | Dir.Cwd _ | Dir_fd _ -> true
  then Error.not_supported "chmod";
  let r = Filesystem.lookup ~follow dir_fd path in
  if not (Filesystem.windows dir_fd) then begin
    (* Linux keeps no mode on a link: EOPNOTSUPP. macOS does. *)
    (match !r with
    | Fs_resource.Symlink _ when (Dir.fs dir_fd).os = `Linux ->
        Error.err ("chmod: not supported on a link: " ^ path)
    | _ -> ());
    if not (User.owns dir_fd.user (Fs_resource.stat !r)) then
      Error.permission_denied path;
    let s = Fs_resource.stat !r in
    let perm = perm land 0o7777 in
    (* Outside the file's group, a user cannot set setgid. *)
    let perm =
      if
        (not (User.is_root dir_fd.user))
        && not (User.in_group dir_fd.user s.gid)
      then perm land lnot 0o2000
      else perm
    in
    r :=
      Fs_resource.set_stat !r
        (inode_changed ~now:((Dir.fs dir_fd).now ()) { s with perm })
  end

(* chown(2): root gives a file to anyone; its owner only moves it to a group
   of their own. EPERM otherwise. *)
let chown ~follow ?uid ?gid dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  (* Windows keeps no owner, and eio_windows refuses a chown. The
     administrator's, as a crash image restores one, keeps nothing. *)
  if Filesystem.windows dir_fd && not dir_fd.admin then
    Error.not_supported "chown";
  let r = Filesystem.lookup ~follow dir_fd path in
  if not (Filesystem.windows dir_fd) then begin
    let s = Fs_resource.stat !r in
    let uid = Option.value ~default:s.uid uid in
    let gid = Option.value ~default:s.gid gid in
    let (user : User.t) = dir_fd.user in
    if
      (not (User.is_root user))
      && not
           (User.owns user s && uid = s.uid
           && (gid = s.gid || User.in_group user gid))
    then Error.permission_denied path;
    r :=
      Fs_resource.set_stat !r
        (inode_changed ~now:((Dir.fs dir_fd).now ())
           (User.without_privileges { s with uid; gid }))
  end

(* The directory [w] is in, without its entry [name]. The entry's inode leaves
   the tree with its only name, and is freed once no descriptor holds it. *)
let remove_entry fs (w : Filesystem.walk) name =
  let now = fs.now () in
  let r = Filesystem.here w in
  let d = as_dir ~path:name !r in
  r := Fs_resource.Dir (Dir_node.remove fs.os name d);
  Option.iter
    (fun ino ->
      let subdirs =
        match Entries.find_opt ino fs.tree with
        | Some victim ->
            unlinked ~now victim;
            if (Fs_resource.stat !victim).kind = `Directory then -1 else 0
        | None -> 0
      in
      entries_changed ~now ~subdirs r;
      fs.tree <- Entries.remove ino fs.tree)
    (Dir_node.find fs.os name d)

(* Windows removes a link, to a directory too, but neither a directory nor
   anything a handle holds open. *)
let unlink_windows dir_fd path =
  let fs = Dir.fs dir_fd in
  let w, name, _ = Filesystem.locate_from dir_fd path in
  if is_dot name then Error.permission_denied path;
  match Filesystem.child w name with
  | None -> Error.not_found path
  | Some { contents = Fs_resource.Dir _ } -> Error.permission_denied path
  | Some victim when (not dir_fd.admin) && held_open fs victim ->
      Error.permission_denied path
  | Some _ -> remove_entry fs w name

let unlink dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  if Filesystem.windows dir_fd then unlink_windows dir_fd path
  else
    let w, name, slash = Filesystem.locate_from dir_fd path in
    (* EISDIR, as Linux answers; macOS answers EPERM. *)
    let is_a_directory () =
      if w.os = `Macos then Error.permission_denied path
      else Error.err ("unlink: is a directory: " ^ path)
    in
    if is_dot name then is_a_directory ();
    match Filesystem.child w name with
    | None -> Error.not_found path
    (* macOS follows a link in directory syntax: to a directory, EPERM. *)
    | Some { contents = Fs_resource.Symlink _ } when slash && w.os = `Macos ->
        ignore (Filesystem.final ~follow:true w name ~slash);
        is_a_directory ()
    (* A trailing slash is refused before permission: EISDIR on a directory,
     ENOTDIR on anything else, a link to a directory included. *)
    | Some { contents = Fs_resource.Dir _ } when slash -> is_a_directory ()
    | Some _ when slash -> Error.not_a_directory path
    | Some victim -> (
        (* Permission before kind. *)
        let rdir = Filesystem.here w in
        check_dir_write ~user:(checker dir_fd) rdir path;
        check_sticky ~user:(checker dir_fd) rdir victim path;
        match !victim with
        | Fs_resource.Dir _ -> is_a_directory ()
        | _ -> remove_entry (Dir.fs dir_fd) w name)

(* The mode bits a new directory keeps: Linux keeps the sticky bit, macOS
   only the permission bits. *)
let dir_bits : os -> int = function
  | `Linux -> 0o1777
  | `Macos | `Windows -> 0o777

let mkdirat dir_fd ~perm path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let fs = Dir.fs dir_fd in
  let mkdir r name =
    let user = dir_fd.Dir.user in
    let d = as_dir ~path !r in
    let new_stat =
      new_stat fs ~user ~parent:d.stat ~kind:`Directory
        ~perm:(masked ~user perm land dir_bits fs.os)
        ()
    in
    let dir = Dir_node.v ~parent:d.stat.ino new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file fs ino (ref (Fs_resource.Dir dir));
    r := Dir (Dir_node.add fs.os name ino d);
    entries_changed ~now:new_stat.ctime ~subdirs:1 r
  in
  new_entry ~dir:true dir_fd path mkdir

let embed dir_fd ~perm path ~contents =
  locked (Dir.fs dir_fd) @@ fun () ->
  let fs = Dir.fs dir_fd in
  let add r name =
    let stat =
      {
        (new_stat fs ~user:dir_fd.Dir.user ~perm:(perm land 0o7777) ()) with
        size = Optint.Int63.of_int (String.length contents);
      }
    in
    let d = as_dir ~path !r in
    let f = File.literal ~stat contents in
    Filesystem.add_file fs stat.ino (ref (Fs_resource.File f));
    r := Dir (Dir_node.add fs.os name stat.ino d);
    entries_changed ~now:stat.ctime r
  in
  new_entry dir_fd path add

(* Windows: only a user who holds SeCreateSymbolicLinkPrivilege, as root
   does here, makes a link, and none to the empty path. Its target is stored
   as eio_windows stores it, relative or as an NT path, and the link is to a
   directory iff its target is one when it is made. *)
let symlink_windows ~link_to dir_fd path =
  let fs = Dir.fs dir_fd in
  let link_to = Filesystem.encode link_to in
  if String.equal link_to "" then Error.reparse_data path;
  let link_to =
    match Filesystem.Nt.link_target ~cwd:fs.cwd link_to with
    | Relative p | Absolute p -> p
  in
  let add r name =
    if not (User.is_root dir_fd.user) then Error.permission_denied path;
    let new_stat =
      {
        (new_stat fs ~user:dir_fd.Dir.user ~parent:(Fs_resource.stat !r)
           ~kind:`Symbolic_link ~perm:0o777 ())
        with
        size = Optint.Int63.of_int (String.length link_to);
      }
    in
    let d = as_dir ~path !r in
    let link = ref (Fs_resource.Symlink (Symlink.v ~link_to new_stat)) in
    Filesystem.add_file fs new_stat.ino link;
    r := Dir (Dir_node.add fs.os name new_stat.ino d);
    entries_changed ~now:new_stat.ctime r;
    match !(Filesystem.lookup ~follow:true dir_fd path) with
    | Fs_resource.Dir _ ->
        link :=
          Fs_resource.Symlink
            { (Symlink.v ~link_to new_stat) with to_dir = true }
    | _ | (exception Eio.Io _) -> ()
  in
  new_entry dir_fd path add

let symlink_posix ~link_to dir_fd path =
  (* Linux makes no link to the empty path: ENOENT, once eio_posix has
     walked to the directory that would hold it. macOS does. *)
  if String.equal link_to "" && (Dir.fs dir_fd).os = `Linux then begin
    ignore (Filesystem.locate_from ~search:false dir_fd path);
    Error.not_found path
  end;
  let fs = Dir.fs dir_fd in
  let symlink r name =
    let new_stat =
      {
        (new_stat fs ~user:dir_fd.Dir.user ~parent:(Fs_resource.stat !r)
           ~kind:`Symbolic_link ~perm:0o777 ())
        with
        size = Optint.Int63.of_int (String.length link_to);
      }
    in
    let d = as_dir ~path !r in
    let link = Symlink.v ~link_to new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file fs ino (ref (Fs_resource.Symlink link));
    r := Dir (Dir_node.add fs.os name ino d);
    entries_changed ~now:new_stat.ctime r
  in
  new_entry dir_fd path symlink

let symlinkat ~link_to dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  if Filesystem.windows dir_fd then symlink_windows ~link_to dir_fd path
  else symlink_posix ~link_to dir_fd path

let read_link dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  let r = Filesystem.lookup ~follow:false dir_fd path in
  match !r with
  | Symlink s ->
      Fs_resource.update r (accessed ~now:((Dir.fs dir_fd).now ()));
      s.link_to
  | _ -> Error.err ("readlink: invalid argument: " ^ path)

(* The names and inodes of the directory [path]. Listing a directory needs
   its read bit: EACCES. *)
let entries dir_fd path =
  let r = Filesystem.lookup ~follow:true dir_fd path in
  match !r with
  | Fs_resource.Dir d when not (User.can (checker dir_fd) d.stat User.r) ->
      Error.permission_denied path
  | Fs_resource.Dir dir ->
      Fs_resource.update r (accessed ~now:((Dir.fs dir_fd).now ()));
      Dir_node.names dir
  | _ when Filesystem.windows dir_fd -> Error.directory_name path
  | _ -> Error.err ("read_dir: not a directory: " ^ path)

let read_dir dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () -> List.map fst (entries dir_fd path)

let read_dir_entries dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  List.map
    (fun (name, ino) ->
      let kind =
        match Entries.find_opt ino (Dir.fs dir_fd).tree with
        | Some res -> (Fs_resource.stat !res).kind
        | None -> `Unknown
      in
      (kind, name))
    (entries dir_fd path)

(* Windows removes a link to a directory with rmdir, as Eio says, and no
   directory a handle holds open. *)
let rmdir_windows dir_fd path =
  let fs = Dir.fs dir_fd in
  let w, name, _ = Filesystem.locate_from dir_fd path in
  if is_dot name then Error.permission_denied path;
  match Filesystem.child w name with
  | None -> Error.not_found path
  | Some { contents = Fs_resource.Symlink { to_dir = true; _ } } ->
      remove_entry fs w name
  | Some { contents = Fs_resource.File _ | Symlink _ } ->
      Error.directory_name path
  | Some { contents = Fs_resource.Dir d } when not (Dir_node.is_empty d) ->
      Error.err ("rmdir: directory not empty: " ^ path)
  | Some victim when (not dir_fd.admin) && held_open fs victim ->
      Error.permission_denied path
  | Some _ -> remove_entry fs w name

let rmdir dir_fd path =
  locked (Dir.fs dir_fd) @@ fun () ->
  if Filesystem.windows dir_fd then rmdir_windows dir_fd path
  else
    let w, name, slash = Filesystem.locate_from dir_fd path in
    (* rmdir(2) refuses a last [.] with EINVAL and a last [..] with ENOTEMPTY,
     or EINVAL on macOS. *)
    if String.equal name "." || (String.equal name ".." && w.os = `Macos) then
      Error.err ("rmdir: invalid argument: " ^ path);
    if String.equal name ".." then
      Error.err ("rmdir: directory not empty: " ^ path);
    let w, name = through_link ~slash w name in
    match Filesystem.child w name with
    | None -> Error.not_found path
    | Some victim -> (
        (* Permission before kind; a final symbolic link is not followed:
         ENOTDIR. *)
        let rdir = Filesystem.here w in
        check_dir_write ~user:(checker dir_fd) rdir path;
        check_sticky ~user:(checker dir_fd) rdir victim path;
        match !victim with
        | Fs_resource.Dir d when not (Dir_node.is_empty d) ->
            Error.err ("rmdir: directory not empty: " ^ path)
        | Fs_resource.Dir _ -> remove_entry (Dir.fs dir_fd) w name
        | _ -> Error.err ("rmdir: not a directory: " ^ path))

(* rename(2) refuses to move a directory below itself (EINVAL) and anything
   over a directory above it (ENOTEMPTY), before it checks permissions. Eio.Fs
   has no constructor for these, which eio_posix raises as a bare [Eio.Io]. *)
let check_rename_tree tree ~src ~old_parent ~new_parent ~dst new_path =
  let ino = (Fs_resource.stat !src).ino in
  match (!src, Option.map ( ! ) dst) with
  | Fs_resource.Dir _, _ when Filesystem.within tree new_parent ino ->
      Error.err ("rename: invalid argument, into its own subtree: " ^ new_path)
  | _, Some (Fs_resource.Dir d)
    when Filesystem.within tree old_parent d.stat.ino ->
      Error.err ("rename: directory not empty: " ^ new_path)
  | _ -> ()

(* After them, a directory over a non-directory (ENOTDIR), a non-directory
   over a directory (EISDIR), and last a directory over a non-empty one
   (ENOTEMPTY). *)
let check_rename_kinds ~src ~dst new_path =
  match (!src, Option.map ( ! ) dst) with
  | Fs_resource.Dir _, Some (Fs_resource.File _ | Symlink _) ->
      Error.err ("rename: not a directory: " ^ new_path)
  | (File _ | Symlink _), Some (Fs_resource.Dir _) ->
      Error.err ("rename: is a directory: " ^ new_path)
  | _ -> ()

let check_rename_empty ~src ~dst new_path =
  match (!src, Option.map ( ! ) dst) with
  | Fs_resource.Dir _, Some (Fs_resource.Dir d) when not (Dir_node.is_empty d)
    ->
      Error.err ("rename: directory not empty: " ^ new_path)
  | _ -> ()

(* A rename MOVES: the source loses [old_name] and the target gains [new_name]
   pointing at [ino]. Same-parent edits go to one entries list, so the move is
   atomic. *)
let move_entry os ino ~old_parent ~old_name old_path ~new_parent ~new_name
    new_path =
  if old_parent == new_parent then begin
    let d = as_dir ~path:new_path !new_parent in
    new_parent :=
      Fs_resource.Dir
        (Dir_node.add os new_name ino (Dir_node.remove os old_name d))
  end
  else begin
    let dst = as_dir ~path:new_path !new_parent in
    new_parent := Fs_resource.Dir (Dir_node.add os new_name ino dst);
    let src = as_dir ~path:old_path !old_parent in
    old_parent := Fs_resource.Dir (Dir_node.remove os old_name src)
  end

(* A directory's [..] names its new parent. *)
let reparent res ~new_parent =
  match !res with
  | Fs_resource.Dir d ->
      res :=
        Fs_resource.Dir { d with parent = (Fs_resource.stat !new_parent).ino }
  | File _ | Symlink _ | Stdout _ | Stderr _ | Stdin _ | Pipe _ -> ()

(* rename(2) answers EBUSY for a last [.] or [..], or EEXIST for the target's
   when it must not replace it; macOS answers EINVAL. *)
let check_rename_dots ~os ~replace ~old_path ~old_name new_path new_name =
  let busy path =
    if os = `Macos then Error.err ("rename: invalid argument: " ^ path)
    else Error.err ("rename: device or resource busy: " ^ path)
  in
  if is_dot old_name then busy old_path;
  if is_dot new_name then
    if replace || os = `Macos then busy new_path
    else Error.already_exists new_path

(* A trailing slash on either name requires a directory, never followed:
   ENOTDIR, once a target above the source is ENOTEMPTY. macOS follows a
   source link in directory syntax, and answers ENOENT for a missing target
   in directory syntax. *)
let check_rename_slashes ~os ~tree ~old_res ~old_w ~old_slash ~old_path new_w
    new_name ~new_slash new_path =
  match !old_res with
  | Fs_resource.Dir _ -> ()
  | _ when old_slash || new_slash ->
      let dst =
        Option.bind
          (Dir_node.find os new_name
             (as_dir ~path:new_path !(Filesystem.here new_w)))
          (fun ino -> Entries.find_opt ino tree)
      in
      if os = `Macos && (not old_slash) && Option.is_none dst then
        Error.not_found new_path;
      check_rename_tree tree ~src:old_res ~old_parent:(Filesystem.here old_w)
        ~new_parent:(Filesystem.here new_w) ~dst new_path;
      Error.not_a_directory old_path
  | _ -> ()

(* What a rename that replaces or adds a name checks: the trees, the write
   and sticky bits of both parents, the kinds, and the write bit of a
   directory moved to another parent, whose [..] is rewritten. *)
let check_rename_access ~user ~tree ~old_res ~old_parent ~old_path ~new_parent
    ~dst new_path =
  check_rename_tree tree ~src:old_res ~old_parent ~new_parent ~dst new_path;
  check_dir_write ~user old_parent old_path;
  check_sticky ~user old_parent old_res old_path;
  check_dir_write ~user new_parent new_path;
  Option.iter (fun dst -> check_sticky ~user new_parent dst new_path) dst;
  check_rename_kinds ~src:old_res ~dst new_path;
  (match !old_res with
  | Fs_resource.Dir _ when old_parent != new_parent ->
      if not (User.can user (Fs_resource.stat !old_res) User.w) then
        Error.permission_denied old_path
  | _ -> ());
  check_rename_empty ~src:old_res ~dst new_path

(* The times and link counts a rename changes. A directory moved takes its
   [..] link along, and one replaced gives its own up; the replaced entry
   lost its only name. *)
let renamed ~fs ~old_res ~old_parent ~new_parent ~dst existing =
  let now = fs.now () in
  let is_dir r = (Fs_resource.stat !r).kind = `Directory in
  let moved = if is_dir old_res then 1 else 0 in
  let replaced =
    match dst with Some d when is_dir d -> 1 | Some _ | None -> 0
  in
  Fs_resource.update old_res (inode_changed ~now);
  if old_parent == new_parent then
    entries_changed ~now ~subdirs:(-replaced) new_parent
  else begin
    entries_changed ~now ~subdirs:(-moved) old_parent;
    entries_changed ~now ~subdirs:(moved - replaced) new_parent
  end;
  Option.iter
    (fun dst_ino ->
      Option.iter (unlinked ~now) dst;
      fs.tree <- Entries.remove dst_ino fs.tree)
    existing

(* A rename that adds [new_name] to [new_parent], or replaces the entry
   [existing] there. macOS keeps the spelling of the entry it replaces. *)
let move_or_replace ~fs ~replace ~user ~old_res ~ino ~old_parent ~old_name
    ~old_path ~new_parent ~new_name ~new_dir_node ~existing new_path =
  let os = fs.os and tree = fs.tree in
  let new_name =
    Option.value ~default:new_name (Dir_node.spelling os new_name new_dir_node)
  in
  if (not replace) && Option.is_some existing then Error.already_exists new_path;
  let dst =
    Option.bind existing (fun dst_ino -> Entries.find_opt dst_ino tree)
  in
  check_rename_access ~user ~tree ~old_res ~old_parent ~old_path ~new_parent
    ~dst new_path;
  check_name fs new_name;
  move_entry os ino ~old_parent ~old_name old_path ~new_parent ~new_name
    new_path;
  reparent old_res ~new_parent;
  renamed ~fs ~old_res ~old_parent ~new_parent ~dst existing

(* The drive the directory [r] is on. *)
let drive tree r =
  match Filesystem.components (Filesystem.win_path tree r) with
  | d :: _ -> String.uppercase_ascii d
  | [] -> ""

(* eio_windows refuses a directory over a file, a file over a directory,
   and a directory over one that is not empty. *)
let check_rename_kinds_windows ~old_res ~dst new_path =
  match (!old_res, Option.map ( ! ) dst) with
  | Fs_resource.Dir _, Some (Fs_resource.File _ | Symlink _) ->
      Error.directory_name new_path
  | (File _ | Symlink _), Some (Fs_resource.Dir _) ->
      Error.permission_denied new_path
  | Fs_resource.Dir _, Some (Fs_resource.Dir d) when not (Dir_node.is_empty d)
    ->
      Error.err ("rename: directory not empty: " ^ new_path)
  | _ -> ()

(* eio_windows refuses to move what a handle holds open, a directory below
   itself, and anything over an entry held open or above the source. *)
let check_rename_windows ~fs ~admin ~old_res ~ino ~old_parent ~new_parent ~dst
    ~old_path new_path =
  let above (d : resource ref) =
    match !d with
    | Fs_resource.Dir dd -> Filesystem.within fs.tree old_parent dd.stat.ino
    | _ -> false
  in
  let held r = (not admin) && held_open fs r in
  if held old_res then Error.permission_denied old_path;
  (match !old_res with
  | Fs_resource.Dir _ when Filesystem.within fs.tree new_parent ino ->
      Error.permission_denied new_path
  | _ -> ());
  (match dst with
  | Some d when above d || held d -> Error.permission_denied new_path
  | _ -> ());
  check_rename_kinds_windows ~old_res ~dst new_path

(* A rename as eio_windows makes it, FILE_RENAME_FLAG_POSIX_SEMANTICS
   (eio#934): it replaces a file or an empty directory, but refuses to move
   a file over a directory, to move anything a handle holds open, or below
   it, a directory into itself or over one above it, and an entry to
   another drive, which Eio names EXDEV and raises as [Permission_denied].
   An entry renamed to another spelling of its name takes it, as does one
   that replaces another. *)
let rename_windows ~replace dir_fd old_path new_dir new_path =
  let fs = Dir.fs dir_fd in
  let os = fs.os and tree = fs.tree in
  let old_w, old_name, _ = Filesystem.locate_from dir_fd old_path in
  let new_w, new_name, _ = Filesystem.locate_from new_dir new_path in
  if is_dot old_name then Error.permission_denied old_path;
  if is_dot new_name then Error.permission_denied new_path;
  let old_parent = Filesystem.here old_w in
  let new_parent = Filesystem.here new_w in
  let old_res =
    match Filesystem.child old_w old_name with
    | Some r -> r
    | None -> Error.not_found old_path
  in
  if drive tree old_parent <> drive tree new_parent then
    raise
      (Eio.Fs.err
         (Permission_denied
            (Eio_fs ("rename: invalid cross-device link: " ^ new_path))));
  check_live fs new_parent new_path;
  let ino = (Fs_resource.stat !old_res).ino in
  let existing =
    Dir_node.find os new_name (as_dir ~path:new_path !new_parent)
  in
  match existing with
  | Some dst_ino when Inode.equal dst_ino ino ->
      if old_parent == new_parent then
        move_entry os ino ~old_parent ~old_name old_path ~new_parent ~new_name
          new_path
  | _ ->
      if (not replace) && Option.is_some existing then
        Error.already_exists new_path;
      let dst = Option.bind existing (fun i -> Entries.find_opt i tree) in
      check_rename_windows ~fs ~admin:dir_fd.admin ~old_res ~ino ~old_parent
        ~new_parent ~dst ~old_path new_path;
      move_entry os ino ~old_parent ~old_name old_path ~new_parent ~new_name
        new_path;
      reparent old_res ~new_parent;
      renamed ~fs ~old_res ~old_parent ~new_parent ~dst existing

let rename ?(replace = true) dir_fd old_path new_dir new_path =
  locked (Dir.fs dir_fd) @@ fun () ->
  (* rename(2) does not move an entry between two file systems: EXDEV, which
     Eio.Fs has no constructor for. *)
  if Dir.fs dir_fd != Dir.fs new_dir then
    Error.err ("rename: invalid cross-device link: " ^ new_path);
  if Filesystem.windows dir_fd then
    rename_windows ~replace dir_fd old_path new_dir new_path
  else
    let fs = Dir.fs dir_fd in
    let os = fs.os in
    let tree = fs.tree in
    let old_w, old_name, old_slash = Filesystem.locate_from dir_fd old_path in
    let new_w, new_name, new_slash = Filesystem.locate_from new_dir new_path in
    check_rename_dots ~os ~replace ~old_path ~old_name new_path new_name;
    let old_w, old_name = through_link ~slash:old_slash old_w old_name in
    (* Rename the directory entry itself. In particular, a symlink's target need
     not exist and must not be followed while moving the link. *)
    let old_res =
      match Filesystem.child old_w old_name with
      | Some r -> r
      | None -> Error.not_found old_path
    in
    check_rename_slashes ~os ~tree ~old_res ~old_w ~old_slash ~old_path new_w
      new_name ~new_slash new_path;
    let ino = Fs_resource.(stat !old_res).ino in
    let old_parent = Filesystem.here old_w in
    let new_parent = Filesystem.here new_w in
    check_live (Dir.fs new_dir) new_parent new_path;
    let new_dir_node = as_dir ~path:new_path !new_parent in
    let existing = Dir_node.find os new_name new_dir_node in
    match existing with
    (* Two names of one entry: rename(2) does nothing; on macOS, one that
     differs only in case or normalisation respells it. *)
    | Some dst_ino when Inode.equal dst_ino ino ->
        if os = `Macos && old_parent == new_parent then begin
          check_dir_write ~user:(checker dir_fd) old_parent old_path;
          move_entry os ino ~old_parent ~old_name old_path ~new_parent ~new_name
            new_path
        end
    | _ ->
        move_or_replace ~fs ~replace ~user:(checker dir_fd) ~old_res ~ino
          ~old_parent ~old_name ~old_path ~new_parent ~new_name ~new_dir_node
          ~existing new_path

let stat t = locked t.Fd.fs @@ fun () -> Fs_resource.stat !(resource_of t)

(* An offset before the start of a file: EINVAL, which eio_posix raises as a
   bare [Eio.Io]. *)
let check_offset op off =
  if Optint.Int63.compare off Optint.Int63.zero < 0 then
    Fmt.kstr Error.err "%s: invalid offset %a" op Optint.Int63.pp off

(* EISDIR, naming the operation and never the contents. *)
let not_a_file op r =
  match r with
  | Fs_resource.Dir _ -> Error.err (op ^ ": is a directory")
  | _ -> Error.err (op ^ ": not a regular file")

(* write(2) on a descriptor opened read-only: EBADF; ftruncate(2): EINVAL. *)
let check_writable op (t : Fd.t) =
  if not t.writable then Error.err (op ^ ": not open for writing")

(* A descriptor open on a directory still answers the file interface, as the
   portable directory barrier opens one with [Eio.Path.open_in], so the
   operations below refuse it with EISDIR, as read(2) and write(2) do. *)
(* A write by a user other than root clears the file's privilege bits. A
   write of no bytes changes nothing, as Linux returns before the file
   system sees it. *)
let written (t : Fd.t) (f : File.t) =
  if User.is_root t.user then f
  else { f with stat = User.without_privileges f.stat }

(* A write of [n] bytes moves mtime and ctime, one of none neither. *)
let written_at (t : Fd.t) (f : File.t) n =
  let f = written t f in
  if n = 0 then f else { f with stat = changed ~now:(t.fs.now ()) f.stat }

(* A read that asks for bytes, at the end of the file included. *)
let read_from (t : Fd.t) r bufs =
  if Cstruct.lenv bufs > 0 then
    Fs_resource.update r (accessed ~now:(t.fs.now ()))

let writev t bufs =
  locked t.Fd.fs @@ fun () ->
  let r = resource_of t in
  check_writable "writev" t;
  match !r with
  | File _ when Cstruct.lenv bufs = 0 -> 0
  | File e ->
      (* The buffers are one contiguous run, written at the offset or, for an
         append descriptor, at the end. *)
      let src = Cstruct.concat bufs in
      if t.append then t.pos <- e.stat.size;
      r :=
        File
          (written_at t
             (File.write e ~off:(Optint.Int63.to_int t.pos) src)
             (Cstruct.length src));
      t.pos <- Optint.Int63.add t.pos (Optint.Int63.of_int (Cstruct.length src));
      Cstruct.length src
  | Stdout (_, v) -> Eio.Flow.single_write v bufs
  | Stderr (_, v) -> Eio.Flow.single_write v bufs
  | Pipe pipe ->
      List.fold_left (fun acc buf -> Pipe.write pipe buf + acc) 0 bufs
  | r -> not_a_file "writev" r

(* The bytes of [e] from [off] on into [bufs]: how many. At or past the end
   nothing is read. *)
let read_at (e : File.t) off bufs =
  let size = File.size e in
  List.fold_left
    (fun read buf ->
      let off = off + read in
      let n = max 0 (min (Cstruct.length buf) (size - off)) in
      if n > 0 then File.blit e.contents ~src_off:off buf ~dst_off:0 ~len:n;
      read + n)
    0 bufs

let blit_into_bufs buf bufs =
  let i = ref 0 in
  let len = Cstruct.length buf in
  List.iter
    (fun obuf ->
      if !i >= len then ()
      else
        let olen = Cstruct.length obuf in
        let actual_len = min (len - !i) olen in
        Cstruct.blit buf !i obuf 0 actual_len;
        i := !i + actual_len)
    bufs;
  !i

let readv t bufs =
  locked t.Fd.fs @@ fun () ->
  let r = resource_of t in
  match !r with
  | File e ->
      read_from t r bufs;
      let read = read_at e (Optint.Int63.to_int t.pos) bufs in
      t.pos <- Optint.Int63.add t.pos (Optint.Int63.of_int read);
      read
  | Stdin (_, r) -> Eio.Flow.single_read r (List.hd bufs)
  | Pipe p -> (
      match Pipe.read p with
      | None -> raise End_of_file
      | Some buf -> blit_into_bufs buf bufs)
  | r -> not_a_file "readv" r

(* Positioned reads and writes never move the cursor. *)
let preadv t ~file_offset bufs =
  locked t.Fd.fs @@ fun () ->
  if not t.positional then Error.not_supported "preadv";
  check_offset "pread" file_offset;
  let r = resource_of t in
  match !r with
  | Fs_resource.File e -> (
      read_from t r bufs;
      let start = Optint.Int63.to_int file_offset in
      if start >= File.size e then raise End_of_file
      else match read_at e start bufs with 0 -> raise End_of_file | n -> n)
  | r -> not_a_file "pread" r

let pwritev t ~file_offset bufs =
  locked t.Fd.fs @@ fun () ->
  if not t.positional then Error.not_supported "pwritev";
  check_offset "pwrite" file_offset;
  let r = resource_of t in
  check_writable "pwrite" t;
  match !r with
  | Fs_resource.File _ when Cstruct.lenv bufs = 0 -> 0
  | Fs_resource.File e ->
      let src = Cstruct.concat bufs in
      (* On an O_APPEND descriptor Linux appends, whatever the offset;
         macOS writes at the offset. *)
      let off =
        if t.append && t.fs.os = `Linux then File.size e
        else Optint.Int63.to_int file_offset
      in
      r :=
        Fs_resource.File
          (written_at t (File.write e ~off src) (Cstruct.length src));
      Cstruct.length src
  | r -> not_a_file "pwrite" r

let seek_fd t off cmd =
  locked t.Fd.fs @@ fun () ->
  match !(resource_of t) with
  | Fs_resource.File e ->
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
  | Fs_resource.Dir _ -> Optint.Int63.zero
  | r -> not_a_file "seek" r

let truncate_fd t len =
  locked t.Fd.fs @@ fun () ->
  check_offset "truncate" len;
  let r = resource_of t in
  check_writable "truncate" t;
  match !r with
  | Fs_resource.File e ->
      let f = written t (File.truncate e (Optint.Int63.to_int len)) in
      r := Fs_resource.File { f with stat = changed ~now:(t.fs.now ()) f.stat }
  | r -> not_a_file "truncate" r

module Flow = struct
  module Impl = struct
    type t = Fd.t

    let fd (t : t) = t
    let stat = stat
    let single_write = writev

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

    let read_methods = []
    let pread = preadv
    let pwrite = pwritev
    let seek = seek_fd

    (* The in-memory store is already durable, so [sync] has nothing to
       flush; a closed descriptor still refuses it. *)
    let sync t = ignore (Fd.num t)
    let truncate = truncate_fd
    let close = close
  end

  (* A file and no socket, as eio_posix's. *)
  let handler =
    let bindings = Eio.Resource.bindings @@ Eio.File.Pi.rw (module Impl) in
    Eio.Resource.handler (bindings @ [ Eio.Resource.H (Resource.T, Impl.fd) ])

  let of_fd fd = Eio.Resource.T (fd, handler)
end

let stdin user t =
  Fd.v ~append:false ~writable:false ~positional:false ~user t (Num.of_int 0)

let stdout user t =
  Fd.v ~append:false ~writable:true ~positional:false ~user t (Num.of_int 1)

let stderr user t =
  Fd.v ~append:false ~writable:true ~positional:false ~user t (Num.of_int 2)
