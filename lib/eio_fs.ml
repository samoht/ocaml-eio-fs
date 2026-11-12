(*
 * Copyright (C) 2023 Thomas Leonard
 * Copyright (C) 2026 Memfs developers
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open! Hermetic

type user = Low_level.User.t = {
  uid : int;
  gid : int;
  groups : int list;
  umask : int;
}

let root = Low_level.User.root

module Nofollow = struct
  type error = [ `Invalid of string | `Too_large ]

  let check_bound max_bytes =
    if max_bytes < 0 || max_bytes = max_int then
      invalid_arg "Eio_fs.Nofollow: invalid byte bound"

  let run ~max_bytes ~validate ~stat ~read =
    check_bound max_bytes;
    match validate stat with
    | Error reason -> Error (`Invalid reason)
    | Ok () ->
        (* The buffer starts at the size the descriptor reports, one byte over
           so the end is told from growth, and doubles toward the bound only
           while the file keeps answering: a read costs the file, not the
           bound. A buffer outgrown is wiped as it is dropped. *)
        let limit = max_bytes + 1 in
        let size = Optint.Int63.to_int stat.Eio.File.Stat.size in
        let buffer = ref (Cstruct.create (min limit (max 0 size + 1))) in
        let returned = ref false in
        Fun.protect
          ~finally:(fun () -> if not !returned then Cstruct.memset !buffer 0)
          (fun () ->
            let grow filled =
              let larger =
                Cstruct.create (min limit (2 * Cstruct.length !buffer))
              in
              Cstruct.blit !buffer 0 larger 0 filled;
              Cstruct.memset !buffer 0;
              buffer := larger
            in
            let rec fill filled =
              if filled = Cstruct.length !buffer then
                if filled = limit then filled
                else begin
                  grow filled;
                  fill filled
                end
              else
                let dst = Cstruct.shift !buffer filled in
                match read dst with
                | 0 -> filled
                | got -> fill (filled + got)
                | exception End_of_file -> filled
            in
            let length = fill 0 in
            if length > max_bytes then Error `Too_large
            else begin
              returned := true;
              Ok (Cstruct.sub !buffer 0 length)
            end)

  let read_file ~max_bytes ~validate path =
    check_bound max_bytes;
    Eio.Switch.run @@ fun sw ->
    let file = Eio.Path.open_in ~sw ~follow:false path in
    run ~max_bytes ~validate ~stat:(Eio.File.stat file)
      ~read:(Eio.Flow.single_read file)
end

(* A path with no last component is called by the whole of itself: "/" is
   "/", ".." is "..", and the empty path is ".". *)
let last_component path =
  match Eio_fs_path.basename path with
  | "" -> if String.equal path "" then "." else path
  | base -> base

module Path = struct
  type usage = {
    allocated_bytes : int64;
    apparent_bytes : int64;
    files : int;
    latest_mtime : float;
  }

  let add_bytes a b =
    if Int64.sub Int64.max_int a < b then Int64.max_int else Int64.add a b

  let bytes_of_blocks blocks =
    if blocks <= 0L then 0L
    else if blocks > Int64.div Int64.max_int 512L then Int64.max_int
    else Int64.mul blocks 512L

  (* Only a regular file has a length a reader can act on: a directory's [size]
     is the space its entries take and a symlink's is its target. *)
  let own stat =
    let regular = stat.Eio.File.Stat.kind = `Regular_file in
    {
      allocated_bytes = bytes_of_blocks stat.Eio.File.Stat.blocks;
      apparent_bytes =
        (if regular then Optint.Int63.to_int64 stat.Eio.File.Stat.size else 0L);
      files = (if regular then 1 else 0);
      latest_mtime = stat.mtime;
    }

  let add a b =
    {
      allocated_bytes = add_bytes a.allocated_bytes b.allocated_bytes;
      apparent_bytes = add_bytes a.apparent_bytes b.apparent_bytes;
      files = a.files + b.files;
      latest_mtime = Float.max a.latest_mtime b.latest_mtime;
    }

  let empty =
    { allocated_bytes = 0L; apparent_bytes = 0L; files = 0; latest_mtime = 0. }

  (* [f] on [path] and its stat, then the entries below it when it is a
     directory. *)
  let visit f path =
    let stat = Eio.Path.stat ~follow:false path in
    f path stat;
    if stat.kind <> `Directory then []
    else
      Eio.Path.read_dir path
      |> List.rev_map (fun name -> Eio.Path.(path / name))

  let batch f paths =
    List.fold_left
      (fun next path -> List.rev_append (visit f path) next)
      [] paths

  (* Partition the level first so the number of fibers is bounded structurally;
     each worker then performs its share sequentially. *)
  let level ~max_fibers f paths =
    let worker_count = Int.min max_fibers (List.length paths) in
    let work = Array.make worker_count [] in
    List.iteri
      (fun i path ->
        let worker = i mod worker_count in
        work.(worker) <- path :: work.(worker))
      paths;
    match Array.to_list work with
    | [ paths ] -> batch f paths
    | batches -> List.concat (Eio.Fiber.List.map (batch f) batches)

  let iter ?(max_fibers = 32) f path =
    if max_fibers <= 0 then invalid_arg "Eio_fs.Path.iter: max_fibers <= 0";
    let rec loop = function
      | [] -> ()
      | paths -> loop (level ~max_fibers f paths)
    in
    loop [ path ]

  let rec grant_owner p =
    match Eio.Path.stat ~follow:false p with
    | { kind = `Directory; perm; _ } -> (
        (if perm land 0o700 <> 0o700 then
           try Eio.Path.chmod ~follow:false ~perm:(perm lor 0o700) p
           with Eio.Io _ -> ());
        match Eio.Path.read_dir p with
        | names -> List.iter (fun n -> grant_owner Eio.Path.(p / n)) names
        | exception Eio.Io _ -> ())
    | _ | (exception Eio.Io _) -> ()

  let rec revoke_writes p =
    let stat = Eio.Path.stat ~follow:false p in
    if stat.kind = `Directory then
      List.iter (fun n -> revoke_writes Eio.Path.(p / n)) (Eio.Path.read_dir p);
    if stat.kind <> `Symbolic_link && stat.perm land 0o222 <> 0 then
      Eio.Path.chmod ~follow:false ~perm:(stat.perm land lnot 0o222) p

  let usage ?max_fibers path =
    let total = ref empty in
    iter ?max_fibers (fun _ stat -> total := add !total (own stat)) path;
    !total

  (* The native spelling names a host file; [Eio.Path.pp] renders through the
     capability label, which is all an in-memory path has. *)
  let name path =
    match Eio.Path.native path with
    | Some native -> native
    | None -> Fmt.str "%a" Eio.Path.pp path

  type node = Node of string * node list

  (* Each child is drawn behind the guides of its ancestors: a branch, and
     below it a rule while siblings follow, as tree(1) draws them. *)
  let rec pp_children ppf guides = function
    | [] -> ()
    | [ child ] -> pp_child ppf guides "\u{2514}\u{2500}\u{2500} " "    " child
    | child :: rest ->
        pp_child ppf guides "\u{251C}\u{2500}\u{2500} " "\u{2502}   " child;
        pp_children ppf guides rest

  and pp_child ppf guides branch rule (Node (label, children)) =
    Fmt.pf ppf "@,%s%s%s" guides branch label;
    pp_children ppf (guides ^ rule) children

  let pp_tree ~depth =
    let name (_, p) = last_component p in
    let rec node depth path =
      match Eio.Path.kind ~follow:false path with
      | `Regular_file -> Some (Node (name path, []))
      | `Directory ->
          let children =
            if depth <= 1 then []
            else
              Eio.Path.read_dir path
              |> List.filter_map (fun child ->
                  node (depth - 1) Eio.Path.(path / child))
          in
          Some (Node (name path, children))
      | `Symbolic_link ->
          Some (Node (name path ^ " -> " ^ Eio.Path.read_link path, []))
      | _ -> None
    in
    fun ppf path ->
      if depth > 0 then
        Option.iter
          (fun (Node (label, children)) ->
            Fmt.pf ppf "@[<v>%s%a@]" label
              (fun ppf -> pp_children ppf "")
              children)
          (node depth path)
end

module Fs = Memfs

module Crunch = struct
  type entry =
    | Dir of { path : string; perm : int }
    | File of { path : string; perm : int; contents : string }
    | Symlink of { path : string; target : string }

  type t = entry list

  let child parent name =
    if parent = "/" then "/" ^ name else parent ^ "/" ^ name

  let scan root =
    let entries = ref [] in
    let push entry = entries := entry :: !entries in
    let rec walk ~follow dir path =
      let stat = Eio.Path.stat ~follow dir in
      match stat.kind with
      | `Regular_file ->
          push (File { path; perm = stat.perm; contents = Eio.Path.load dir })
      | `Directory ->
          push (Dir { path; perm = stat.perm });
          Eio.Path.read_dir dir
          |> List.iter (fun name ->
              walk ~follow:false Eio.Path.(dir / name) (child path name))
      | `Symbolic_link ->
          push (Symlink { path; target = Eio.Path.read_link dir })
      | _ -> Eio.traceln "Skipping %a" Eio.Path.pp dir
    in
    (match Eio.Path.kind ~follow:true root with
    | `Directory -> ()
    | _ ->
        Fmt.invalid_arg "Eio_fs.Crunch.scan: %a is not a directory" Eio.Path.pp
          root);
    walk ~follow:true root "/";
    List.rev !entries

  let find path entries =
    List.find_map
      (function
        | File f when String.equal f.path path -> Some f.contents | _ -> None)
      entries

  let load entries =
    let t = Low_level.v () in
    let fs = Fs.fs t in
    let mem path = (fs, path) in
    List.iter
      (function
        | Dir { path; perm } ->
            if path <> "/" then Eio.Path.mkdirs ~exists_ok:true ~perm (mem path);
            Eio.Path.chmod ~follow:false ~perm (mem path)
        | File { path; perm; contents } ->
            Low_level.embed
              { at = Low_level.Dir.Fs t; user = Low_level.User.root }
              ~perm path ~contents
        | Symlink { path; target } ->
            Eio.Path.symlink ~link_to:target (mem path))
      entries;
    (fs, "/")

  let source ppf entries =
    Fmt.pf ppf "(* Generated by eio-fs-crunch. Do not edit. *)@.@.";
    Fmt.pf ppf "let entries : Eio_fs.Crunch.t =@.";
    Fmt.pf ppf "  Eio_fs.Crunch.@.";
    Fmt.pf ppf "    [@.";
    List.iter
      (function
        | Dir { path; perm } ->
            Fmt.pf ppf "      Dir { path = %S; perm = 0o%03o };@." path perm
        | File { path; perm; contents } ->
            Fmt.pf ppf
              "      File { path = %S; perm = 0o%03o; contents = %S };@." path
              perm contents
        | Symlink { path; target } ->
            Fmt.pf ppf "      Symlink { path = %S; target = %S };@." path target)
      entries;
    Fmt.pf ppf "    ]@.@.";
    Fmt.pf ppf "let load () = Eio_fs.Crunch.load entries@."

  let interface =
    "(* Generated by eio-fs-crunch. Do not edit. *)\n\n\
     val entries : Eio_fs.Crunch.t\n\
     (** The embedded file tree. *)\n\n\
     val load : unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t\n\
     (** [load ()] is the root of a new in-memory filesystem holding\n\
    \    {!entries}. *)\n"
end

module Chaos = Chaos

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >

(* Root owns the tree and makes the current directory, which it gives to
   [user]. *)
let run ?(cwd = "/") ?(user = root) fn =
  if not (Eio_fs_path.is_abs cwd) then
    Fmt.invalid_arg "Eio_fs.run: the cwd %S is not an absolute path" cwd;
  let t = Low_level.v ~cwd () in
  let admin = (Eio.Path.of_dir (Fs.fs t) :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(admin / cwd);
  if user.uid <> 0 then
    Eio.Path.chown ~follow:true ~uid:(Int64.of_int user.uid)
      ~gid:(Int64.of_int user.gid)
      Eio.Path.(admin / cwd);
  let fs = (Eio.Path.of_dir (Fs.fs ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  let cwd = (Eio.Path.of_dir (Fs.cwd ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  (* Resolve the current directory now: it then follows its directory. *)
  ignore (Eio.Path.stat ~follow:true cwd : Eio.File.Stat.t);
  fn
    object
      method fs = fs
      method cwd = cwd
    end
