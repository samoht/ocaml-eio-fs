(* Eio paths always use '/' as their separator, including on Windows. Keep the
   small amount of lexical path handling needed by the backend here rather than
   depending on the host-sensitive [Filename] or [Fpath] modules. *)
module Path = struct
  let is_abs p = String.length p > 0 && p.[0] = '/'
  let is_rel p = not (is_abs p)
  let segs p = String.split_on_char '/' p

  let is_directory_syntax p =
    let len = String.length p in
    len > 0
    && (p.[len - 1] = '/'
       ||
       match List.rev (segs p) with
       | "." :: _ | ".." :: _ -> true
       | _ -> false)

  let normalize p =
    let absolute = is_abs p in
    let rec loop acc = function
      | [] -> List.rev acc
      | ("" | ".") :: rest -> loop acc rest
      | ".." :: rest -> (
          match acc with
          | segment :: acc when not (String.equal segment "..") -> loop acc rest
          | [] when absolute -> loop [] rest
          | acc -> loop (".." :: acc) rest)
      | segment :: rest -> loop (segment :: acc) rest
    in
    let directory = is_directory_syntax p in
    let body = String.concat "/" (loop [] (segs p)) in
    let path =
      match (absolute, body) with
      | true, "" -> "/"
      | true, body -> "/" ^ body
      | false, "" -> "."
      | false, body -> body
    in
    if directory && not (String.equal path "/") then path ^ "/" else path

  let append base path =
    if is_abs path then path
    else
      let len = String.length base in
      if len = 0 || base.[len - 1] = '/' then base ^ path else base ^ "/" ^ path

  let to_dir_path path =
    let len = String.length path in
    if len > 0 && path.[len - 1] = '/' then path else path ^ "/"

  let without_trailing_slashes path =
    let rec last_non_slash i =
      if i <= 0 || path.[i] <> '/' then i else last_non_slash (i - 1)
    in
    let last = last_non_slash (String.length path - 1) in
    if last < 0 then "/" else String.sub path 0 (last + 1)

  let basename path =
    let path = without_trailing_slashes path in
    if String.equal path "/" then ""
    else
      let name =
        match String.rindex_opt path '/' with
        | None -> path
        | Some index ->
            String.sub path (index + 1) (String.length path - index - 1)
      in
      match name with "." | ".." -> "" | name -> name

  let parent path =
    let path = without_trailing_slashes path in
    if String.equal path "/" then path
    else
      match String.rindex_opt path '/' with
      | None -> "./"
      | Some 0 -> "/"
      | Some index -> String.sub path 0 (index + 1)

  let components path =
    normalize path |> segs
    |> List.filter (fun segment ->
        not (String.equal segment "" || String.equal segment "."))

  let is_rooted ~root path =
    if is_abs root <> is_abs path then false
    else
      let rec is_prefix prefix items =
        match (prefix, items) with
        | [], _ -> true
        | x :: xs, y :: ys when String.equal x y -> is_prefix xs ys
        | _ -> false
      in
      is_prefix (components root) (components path)
end

(* The fake file-descriptor table and filesystem. *)
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
let check_path p = if String.equal p "" then "." else Path.normalize p

let () =
  let pp_eio_mem ppf = function
    | Eio_fs "" ->
        Fmt.pf ppf "eio-mem";
        true
    | Eio_fs s ->
        Fmt.pf ppf "eio-mem:%s" s;
        true
    | _ -> false
  in
  Eio.Exn.Backend.register_pp pp_eio_mem

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

  let badf () =
    let exn = Eio.Exn.create (Eio.Exn.X (Eio_fs "Bad file descriptor")) in
    raise exn

  let err ctx =
    let exn = Eio.Exn.create (Eio.Exn.X (Eio_fs ctx)) in
    raise exn
end

module Fd : sig
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

  let fresh =
    let i = ref 0L in
    fun () ->
      i := Int64.add !i 1L;
      !i
end

module Dir = struct
  type 'a t = { name : string; stat : Eio.File.Stat.t; entries : 'a list }
  type fd = Fs | Cwd | Dir_fd of Fd.t

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

  type t = {
    stat : Eio.File.Stat.t;
    name : string;
    contents : contents;
    pos : Optint.Int63.t;
  }

  let v ~stat name =
    { stat; name; contents = Buffer Cstruct.empty; pos = Optint.Int63.zero }

  let literal ~stat name contents =
    { stat; name; contents = Literal contents; pos = Optint.Int63.zero }

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

module Fd_table = Map.Make (Fd)

module Resource = struct
  type t =
    | Stdin
    | Stdout
    | Stderr
    | File of File.t
    | Dir of (Inode.t * string) Dir.t
    | Symlink of Symlink.t

  let pp ppf = function
    | Stdin -> Fmt.string ppf "stdin"
    | Stdout -> Fmt.string ppf "stdout"
    | Stderr -> Fmt.string ppf "stderr"
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

  let name = function
    | File f -> f.name
    | Dir d -> d.name
    | Symlink s -> s.name
    | r -> Fmt.invalid_arg "No name for %a" pp r

  let stat = function
    | File f -> f.stat
    | Dir dir -> dir.stat
    | Symlink s -> s.stat
    | _ -> Fmt.failwith "No stat"

  let set_stat r stat =
    match r with
    | File f -> File { f with stat }
    | Dir d -> Dir { d with stat }
    | Symlink s -> Symlink { s with stat }
    | r -> r
end

type resource = Resource.t

let as_dir ~label = function
  | Resource.Dir dir -> dir
  | r -> Fmt.invalid_arg "as_dir(%s): %a" label Resource.pp r

let reset_file ~truncate ~append r =
  match !r with
  | Resource.File f ->
      let contents =
        if truncate then File.Buffer Cstruct.empty else f.contents
      in
      let pos =
        if append then File.length contents |> Optint.Int63.of_int
        else Optint.Int63.zero
      in
      let size = File.length contents |> Optint.Int63.of_int in
      let stat = { f.stat with size } in
      r := File { f with contents; stat; pos }
  | _ -> ()

let std_table =
  let open Resource in
  Fd_table.add (Fd.of_int 0) (ref Stdin) Fd_table.empty
  |> Fd_table.add (Fd.of_int 1) (ref Stdout)
  |> Fd_table.add (Fd.of_int 2) (ref Stderr)

let fd_table : resource ref Fd_table.t ref = ref std_table

let update_fd_table_with_result fn =
  let v, new_fd_table = fn !fd_table in
  fd_table := new_fd_table;
  v

let update_fd_table fn =
  let () = update_fd_table_with_result (fun fd -> ((), fn fd)) in
  ()

let update_entry ?(if_missing = fun () -> ()) fd fds fn =
  match Fd_table.find_opt fd fds with None -> if_missing () | Some e -> fn e

let read_fd_table fn = fn !fd_table

let next_fd_of_fds fds =
  Fd_table.to_list fds
  |> List.sort (fun (i, _) (j, _) -> Fd.compare i j)
  |> List.rev |> List.hd |> fst |> Fd.incr

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

let new_stat ?kind ?perm () =
  {
    default_stat with
    ino = Inode.fresh ();
    kind = Option.value ~default:default_stat.kind kind;
    perm = Option.value ~default:default_stat.perm perm;
  }

module Directory = struct
  let cwd : string Eio.Fiber.key = Eio.Fiber.create_key ()
end

module Filesystem = struct
  module Entries = Map.Make (Inode)

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

  let root = ref (empty ())
  let read_filesystem fn = fn !root
  let write_filesystem fn = root := fn !root
  let add_file ino r = write_filesystem (Entries.add ino r)

  let absolute_path (dir : _ Dir.t) =
    if dir.stat.ino = 0L then "/"
    else
      let rec loop (dir : _ Dir.t) acc =
        if Int64.equal dir.stat.ino 0L then "/" ^ acc
        else
          let parent =
            (* let ino_p, fname = Dir.parent dir in  *)
            read_filesystem @@ Entries.find (Dir.parent dir |> fst)
          in
          loop (as_dir ~label:"absolute" !parent) (dir.name ^ "/" ^ acc)
      in
      loop dir ""

  let is_confined ~root p =
    if Path.is_abs p then Path.is_rooted ~root p
    else
      let full_path = Path.append root p in
      Path.is_rooted ~root full_path

  let check_confinement ~root p =
    if is_confined ~root p then () else Error.permission_denied p

  let rec find_entry_relative_to_dir ~check ~follow fs dir (path : string) =
    let confinement_dir = as_dir ~label:"confinement-dir" !dir in
    let confinement_path = absolute_path confinement_dir in
    let check_confinement parent resource_name =
      let parent_path = absolute_path (as_dir ~label:"confinement" !parent) in
      let path_of_next_item =
        if resource_name = "/" then resource_name
        else begin
          Path.append parent_path resource_name
        end
      in
      check_confinement ~root:confinement_path
        (Path.to_dir_path path_of_next_item)
    in
    let rec loop ~parent r = function
      | [] -> (
          match !r with
          | Resource.Symlink s when follow ->
              if check then check_confinement dir s.link_to;
              find_entry_relative_to_dir ~check ~follow fs parent s.link_to
          | _ ->
              let parent_path =
                absolute_path (as_dir ~label:"find-loop" !parent)
              in
              let resource_name = Resource.name !r in
              if resource_name = "/" then (resource_name, r)
              else begin
                (Path.append parent_path resource_name, r)
              end)
      | "" :: segs -> loop ~parent r segs
      | seg :: segs -> (
          if check then check_confinement dir seg;
          let entries, resolved_dir =
            entries_exn
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
          read_filesystem @@ fun fs ->
          let ino, _ = Dir.parent d in
          Entries.find ino fs
      | _ -> assert false
    in
    loop ~parent dir (Path.segs path)

  and entries_exn ~check ~follow ~parent fs :
      resource ref -> (Inode.t * string) list * resource ref =
   fun r ->
    match !r with
    | Resource.Dir dir -> (dir.entries, r)
    | Symlink sym ->
        Option.iter (fun f -> f sym.link_to) check;
        let _, r =
          find_entry_relative_to_dir ~check:(Option.is_some check) ~follow fs
            parent sym.link_to
        in
        entries_exn ~check ~follow ~parent fs r
    | r -> Fmt.failwith "Resource not a directory: %a" Resource.pp r

  let lookup ~follow dir_fd path =
    read_filesystem @@ fun fs ->
    let root = Entries.find 0L fs in
    (* An absolute path with full access *)
    if Path.is_abs path && dir_fd = Dir.Fs then
      let _, r = find_entry_relative_to_dir ~check:false ~follow fs root path in
      r (* Relative path with full access *)
    else if Path.is_rel path && dir_fd = Dir.Fs then
      let cwd_path = Eio.Fiber.get Directory.cwd |> Option.value ~default:"/" in
      let path = Path.append cwd_path path in
      let _, r = find_entry_relative_to_dir ~check:false ~follow fs root path in
      r
    else
      (* Confined to cwd or dir-fd *)
      match dir_fd with
      | Dir.Fs -> assert false (* handled above *)
      | Dir.Cwd ->
          let cwd_path =
            Eio.Fiber.get Directory.cwd |> Option.value ~default:"/"
          in
          let _, cwddir =
            find_entry_relative_to_dir ~check:false ~follow fs root cwd_path
          in
          let resource_path, r =
            find_entry_relative_to_dir ~check:true ~follow fs cwddir path
          in
          (* Check for confinement *)
          if is_confined ~root:cwd_path resource_path then r
          else Error.permission_denied resource_path
      | Dir.Dir_fd dir ->
          (* A directory fd closed with its switch is EBADF, as for a real
             O_DIRECTORY fd, never a silent success. *)
          let dir =
            match read_fd_table (Fd_table.find_opt dir) with
            | Some d -> d
            | None -> Error.badf ()
          in
          let resource_path, r =
            find_entry_relative_to_dir ~check:true ~follow fs dir path
          in
          (* Check for confinement *)
          if
            is_confined
              ~root:(absolute_path (as_dir ~label:"lookup" !dir))
              resource_path
          then r
          else Error.permission_denied resource_path
end

let close fd =
  update_fd_table @@ fun fds ->
  match Fd_table.find_opt fd fds with
  | None -> Error.badf ()
  | Some _ -> Fd_table.remove fd fds

(* open(2): opening an existing file for writing requires the write permission
   bit; the mode given at creation does not restrict the creating open itself.
   The model is single-user, so the owner bit is the one consulted. *)
let check_write_access path flags r =
  if Open_flags.(mem o_rdwr flags) then
    match !r with
    | Resource.File f when f.stat.perm land 0o200 = 0 ->
        Error.permission_denied path
    | _ -> ()

(* Create [path] in its parent directory, register the new file in the
   filesystem, and return its resource. *)
let new_file ~follow ~mode dir_fd path =
  let fname = Filename.basename path in
  let dir = Filename.dirname path in
  let rdir = Filesystem.lookup ~follow dir_fd dir in
  let dir =
    match !rdir with
    | Resource.Dir d -> d
    | _ -> Fmt.failwith "Creating file: no path"
  in
  let stat = new_stat ~perm:(mode land 0o7777) () in
  let f = File.v ~stat fname in
  let dir = { dir with entries = (f.stat.ino, f.name) :: dir.entries } in
  rdir := Resource.Dir dir;
  let file = ref (Resource.File f) in
  Filesystem.add_file f.stat.ino file;
  file

let openat ~mode ~sw dir_fd path flags =
  let path = check_path path in
  let is_create = Open_flags.(mem o_creat flags) in
  let is_excl = Open_flags.(mem o_excl flags) in
  let append = Open_flags.(mem o_append flags) in
  let truncate = Open_flags.(mem o_trunc flags) in
  let nofollow = Open_flags.(mem o_nofollow flags) in
  let follow = not nofollow in
  match is_create || is_excl with
  | false ->
      update_fd_table_with_result @@ fun fds ->
      let fd = next_fd_of_fds fds in
      let r = Filesystem.lookup ~follow dir_fd path in
      check_write_access path flags r;
      reset_file ~truncate ~append r;
      Eio.Switch.on_release sw (fun () -> close fd);
      (fd, Fd_table.add fd r fds)
  | true -> (
      let () =
        try
          if is_excl then
            let _ = Filesystem.lookup ~follow dir_fd path in
            Error.already_exists path
        with Eio.Exn.Io (Eio.Fs.(E (Not_found _)), _) -> ()
      in
      update_fd_table_with_result @@ fun fds ->
      let fd = next_fd_of_fds fds in
      let r =
        try Option.some @@ Filesystem.lookup ~follow dir_fd path
        with Eio.Exn.Io (Eio.Fs.(E (Not_found _)), _) -> None
      in
      match r with
      | Some r ->
          check_write_access path flags r;
          let fds = Fd_table.add fd r fds in
          reset_file ~truncate ~append r;
          Eio.Switch.on_release sw (fun () -> close fd);
          (fd, fds)
      | None ->
          let file = new_file ~follow ~mode dir_fd path in
          let fds = Fd_table.add fd file fds in
          Eio.Switch.on_release sw (fun () -> close fd);
          (fd, fds))

let statat ~follow dir_fd path =
  let path = check_path path in
  let f = Filesystem.lookup ~follow dir_fd path in
  Resource.stat !f

let chmod ~follow dir_fd ~perm path =
  let path = check_path path in
  let r = Filesystem.lookup ~follow dir_fd path in
  r := Resource.set_stat !r { (Resource.stat !r) with perm }

let chown ~follow ?uid ?gid dir_fd path =
  let path = check_path path in
  let r = Filesystem.lookup ~follow dir_fd path in
  let s = Resource.stat !r in
  let uid = Option.value ~default:s.uid uid in
  let gid = Option.value ~default:s.gid gid in
  r := Resource.set_stat !r { s with uid; gid }

let dir_base path = (Path.parent path, Path.basename path)

let unlink dir_fd path =
  let path = check_path path in
  let dir, base = dir_base path in
  let r = Filesystem.lookup ~follow:false dir_fd dir in
  let d = as_dir ~label:"unlink" !r in
  let entries =
    List.filter (fun (_, name) -> not (String.equal base name)) d.entries
  in
  r := Resource.Dir { d with entries }

let mkdirat dir_fd ~perm path =
  let path = check_path path in
  let mkdir r name =
    let new_stat = new_stat ~kind:`Directory ~perm () in
    let d = as_dir ~label:"mkdirat" !r in
    let dir = Dir.v ~parent:d.stat.ino name new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file ino (ref (Resource.Dir dir));
    let entries = (ino, name) :: d.entries in
    r := Dir { d with entries }
  in
  try
    let _ = Filesystem.lookup ~follow:true dir_fd path in
    Error.already_exists path
  with Eio.Io (Eio.Fs.(E (Not_found _)), _) ->
    let dir, base = dir_base path in
    let r = Filesystem.lookup ~follow:true dir_fd dir in
    mkdir r base

let embed dir_fd ~perm path ~contents =
  let path = check_path path in
  let add r name =
    let stat =
      {
        (new_stat ~perm:(perm land 0o7777) ()) with
        size = Optint.Int63.of_int (String.length contents);
      }
    in
    let d = as_dir ~label:"embed" !r in
    let f = File.literal ~stat name contents in
    Filesystem.add_file stat.ino (ref (Resource.File f));
    r := Dir { d with entries = (stat.ino, name) :: d.entries }
  in
  try
    let _ = Filesystem.lookup ~follow:true dir_fd path in
    Error.already_exists path
  with Eio.Io (Eio.Fs.(E (Not_found _)), _) ->
    let dir, base = dir_base path in
    let r = Filesystem.lookup ~follow:true dir_fd dir in
    add r base

let symlinkat ~link_to dir_fd path =
  let path = check_path path in
  let symlink r name =
    let new_stat = new_stat ~kind:`Symbolic_link ~perm:0o644 () in
    let d = as_dir ~label:"symlink" !r in
    let dir = Symlink.v ~link_to name new_stat in
    let ino = new_stat.ino in
    Filesystem.add_file ino (ref (Resource.Symlink dir));
    let entries = (ino, name) :: d.entries in
    r := Dir { d with entries }
  in
  try
    let _ = Filesystem.lookup ~follow:true dir_fd path in
    Error.already_exists path
  with Eio.Io (Eio.Fs.(E (Not_found _)), _) ->
    let dir, base = dir_base path in
    let r = Filesystem.lookup ~follow:true dir_fd dir in
    symlink r base

let read_link dir_fd path =
  let path = check_path path in
  let r = Filesystem.lookup ~follow:false dir_fd path in
  match !r with
  | Symlink s -> s.link_to
  | r -> Fmt.kstr Error.err "not a symlink: %a" Resource.pp r

let read_dir dir_fd path =
  let path = check_path path in
  let r = Filesystem.lookup ~follow:true dir_fd path in
  match !r with
  | Resource.Dir dir ->
      List.map snd dir.entries
      |> List.filter (function "." | ".." -> false | _ -> true)
  | _ -> Error.err "Not a directory"

let read_dir_entries dir_fd path =
  let path = check_path path in
  let r = Filesystem.lookup ~follow:true dir_fd path in
  match !r with
  | Resource.Dir dir ->
      dir.entries
      |> List.filter (fun (_, name) -> name <> "." && name <> "..")
      |> List.map (fun (ino, name) ->
          let kind =
            match
              Filesystem.read_filesystem (Filesystem.Entries.find_opt ino)
            with
            | Some res -> (Resource.stat !res).kind
            | None -> `Unknown
          in
          (kind, name))
  | _ -> Error.err "Not a directory"

let rmdir dir_fd path =
  let path = check_path path in
  let r = Filesystem.lookup ~follow:true dir_fd path in
  match !r with
  | Resource.Dir d ->
      if not (Dir.is_empty d) then Error.err "Directory is not empty"
      else begin
        (* TODO: need to delete from FD table? *)
        let dir, _ = dir_base path in
        let parent = Filesystem.lookup ~follow:true dir_fd dir in
        let r = as_dir ~label:"rmdir" !parent in
        let entries =
          List.filter
            (fun (ino, _) -> not (Inode.equal d.stat.ino ino))
            r.entries
        in
        parent := Dir { r with entries }
      end
  | _ -> Error.err "Not a directory"

let rename dir_fd old_path new_dir new_path =
  let old_path = check_path old_path in
  let new_path = check_path new_path in
  let old_res = Filesystem.lookup ~follow:true dir_fd old_path in
  let ino = Resource.(stat !old_res).ino in
  let old_parent_path, old_name = dir_base old_path in
  let new_parent_path, new_name = dir_base new_path in
  let old_parent = Filesystem.lookup ~follow:true dir_fd old_parent_path in
  let new_parent = Filesystem.lookup ~follow:true new_dir new_parent_path in
  let without name entries =
    List.filter (fun (_, n) -> not (String.equal n name)) entries
  in
  (* [rename] MOVES: the source directory loses [old_name] and the target gains
     [new_name] (replacing any prior entry of that name) pointing at [ino]. The
     source path no longer resolves afterwards, so a file later created there
     gets a fresh inode and shares no storage with the target. When both parents
     are the same directory they resolve to the same ref, so apply both edits to
     one entries list; otherwise write each parent in turn. No fiber yields
     between the reads and the writes, so the move is atomic. *)
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
  end

let stat fd =
  read_fd_table @@ fun fds ->
  match Fd_table.find_opt fd fds |> Option.map ( ! ) with
  | None -> Error.badf ()
  | Some (File f) -> f.stat
  | Some (Dir dir) -> dir.stat
  | _ -> failwith "stat: todo"

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

let writev fd bufs =
  update_fd_table_with_result @@ fun fds ->
  let () =
    update_entry fd fds @@ fun r ->
    match !r with
    | File e ->
        let size = e.stat.size in
        (* If the position is equal to the size then we just append. *)
        if Optint.Int63.equal size e.pos then
          (* We do it one buf at a time so we can simulate some concurrency... *)
          List.iter
            (fun buf ->
              let contents =
                File.Buffer (Cstruct.concat [ File.buffer e.contents; buf ])
              in
              let new_size = Optint.Int63.of_int @@ File.length contents in
              let stat = { e.stat with size = new_size } in
              r := File { e with stat; contents; pos = new_size };
              Eio.Fiber.yield ())
            bufs
        else
          (* Position is less than the size, so need to merge contents. *)
          let src = Cstruct.concat bufs in
          let contents =
            File.Buffer
              (blit_and_grow ~src ~dst:(File.buffer e.contents)
                 ~off:(Optint.Int63.to_int e.pos))
          in
          let new_size = Optint.Int63.of_int @@ File.length contents in
          let stat = { e.stat with size = new_size } in
          let new_pos =
            Optint.Int63.add e.pos (Optint.Int63.of_int (Cstruct.length src))
          in
          r := File { e with stat; contents; pos = new_pos }
    | r -> Fmt.failwith "TODO: writev %a" Resource.pp r
  in
  (Cstruct.length (Cstruct.concat bufs), fds)

let readv fd bufs =
  update_fd_table_with_result @@ fun fds ->
  let read = ref 0 in
  let () =
    update_entry fd fds @@ fun r ->
    match !r with
    | File e ->
        let pos = ref e.pos in
        let size = Optint.Int63.to_int e.stat.size in
        List.iter
          (fun buf ->
            let len = Cstruct.length buf in
            let posi = Optint.Int63.to_int !pos in
            let actual_len = max 0 (min len (size - posi)) in
            File.blit e.contents ~src_off:posi buf ~dst_off:0 ~len:actual_len;
            pos := Optint.Int63.add !pos (Optint.Int63.of_int actual_len);
            read := !read + actual_len;
            Eio.Fiber.yield ())
          bufs;
        r := File { e with pos = !pos }
    | r -> Fmt.failwith "TODO: readv %a" Resource.pp r
  in
  (!read, fds)

(* Positioned reads and writes address an explicit [file_offset] and never move
   the cursor, unlike [readv]/[writev]. This mirrors [Eio.File.pread]/[pwrite]. *)
let preadv fd ~file_offset bufs =
  read_fd_table @@ fun fds ->
  match Fd_table.find_opt fd fds |> Option.map ( ! ) with
  | None -> Error.badf ()
  | Some (Resource.File e) ->
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
  | Some r -> Fmt.failwith "pread: not a file: %a" Resource.pp r

let pwritev fd ~file_offset bufs =
  update_fd_table_with_result @@ fun fds ->
  let written = ref 0 in
  let () =
    update_entry ~if_missing:(fun () -> ignore (Error.badf () : 'a)) fd fds
    @@ fun r ->
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
        written := Cstruct.length src
    | r -> Fmt.failwith "pwrite: not a file: %a" Resource.pp r
  in
  (!written, fds)

let seek_fd fd off cmd =
  update_fd_table_with_result @@ fun fds ->
  let result = ref Optint.Int63.zero in
  let () =
    update_entry ~if_missing:(fun () -> ignore (Error.badf () : 'a)) fd fds
    @@ fun r ->
    match !r with
    | Resource.File e ->
        let base =
          match cmd with
          | `Set -> Optint.Int63.zero
          | `Cur -> e.pos
          | `End -> e.stat.size
        in
        let newpos = Optint.Int63.add base off in
        r := Resource.File { e with pos = newpos };
        result := newpos
    | r -> Fmt.failwith "seek: not a file: %a" Resource.pp r
  in
  (!result, fds)

let truncate_fd fd len =
  update_fd_table @@ fun fds ->
  update_entry
    ~if_missing:(fun () -> ignore (Error.badf () : 'a))
    fd fds
    (fun r ->
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
          let pos = if Optint.Int63.compare e.pos len > 0 then len else e.pos in
          r := Resource.File { e with contents; stat; pos }
      | r -> Fmt.failwith "truncate: not a file: %a" Resource.pp r);
  fds

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

let reset () =
  Filesystem.root := Filesystem.empty ();
  fd_table := std_table
