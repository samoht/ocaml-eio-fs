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
open Eio.Std

module Nofollow = struct
  type error = [ `Invalid of string | `Too_large ]
  type result = (Cstruct.t, error) Stdlib.result

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
end

module Fs = struct
  type dir_fd = Low_level.Dir.fd

  (* A path with no last component is called by the whole of itself: "/" is
     "/", ".." is "..", and the empty path is ".". *)
  let last_component path =
    match Eio_fs_path.basename path with
    | "" -> if String.equal path "" then "." else path
    | base -> base

  (* A rename hands back a plain [Eio.Fs.dir]; this checks the new location is
     within its sandbox. *)
  type (_, _, _) Eio.Resource.pi +=
    | Memfs_dir : ('t, 't -> dir_fd, [> `Memfs_dir ]) Eio.Resource.pi

  let as_memfs_dir (Eio.Resource.T (t, ops)) =
    match Eio.Resource.get_opt ops Memfs_dir with
    | None -> None
    | Some fn -> Some (fn t)

  module rec Dir : sig
    include Eio.Fs.Pi.DIR

    val v : label:string -> path:string -> dir_fd -> t
    val fd : t -> dir_fd
    val close : t -> unit

    val rename_exclusive :
      t ->
      Eio_fs_sync.Rename.t ->
      source:string ->
      target:Eio_fs_sync.Rename.path ->
      unit
  end = struct
    type t = {
      fd : dir_fd;
      dir_path : string;
      label : string;
      mutable root_closed : bool;
    }

    (* Descriptor state belongs to the descriptor, including switch release.
       Only descriptor-free root and cwd capabilities need their own flag. *)
    let closed t =
      match t.fd with
      | Fs _ | Cwd _ -> t.root_closed
      | Dir_fd fd -> Low_level.Fd.closed fd

    let fd t =
      if closed t then Low_level.Error.badf ();
      t.fd

    let close t =
      match t.fd with
      | Fs _ | Cwd _ -> t.root_closed <- true
      | Dir_fd fd -> Low_level.close fd

    let v ~label ~path:dir_path fd =
      { fd; dir_path; label; root_closed = false }

    (* [~follow:false] refuses a symlink at [path] itself, as O_NOFOLLOW. *)
    let nofollow ~follow flags =
      if follow then flags else Low_level.Open_flags.(flags + o_nofollow)

    let open_in t ~sw ~follow path =
      let flags = nofollow ~follow Low_level.Open_flags.o_rdonly in
      let fd = Low_level.openat ~mode:0 ~sw (fd t) path flags in
      (Low_level.Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

    let open_out t ~sw ~follow ~append ~create path =
      let mode, flags =
        match create with
        | `Never -> (0, Low_level.Open_flags.empty)
        | `If_missing perm -> (perm, Low_level.Open_flags.o_creat)
        | `Or_truncate perm -> (perm, Low_level.Open_flags.(o_creat + o_trunc))
        | `Exclusive perm -> (perm, Low_level.Open_flags.(o_creat + o_excl))
      in
      let flags =
        if append then Low_level.Open_flags.(flags + o_append) else flags
      in
      let flags = nofollow ~follow Low_level.Open_flags.(flags + o_rdwr) in
      match Low_level.openat ~sw ~mode (fd t) path flags with
      | fd -> (Low_level.Flow.of_fd fd :> Eio.File.rw_ty r)

    let mkdir t ~perm path = Low_level.mkdirat ~perm (fd t) path
    let unlink t path = Low_level.unlink (fd t) path
    let rmdir t path = Low_level.rmdir (fd t) path
    let stat t ~follow path = Low_level.statat ~follow (fd t) path
    let read_dir t path = Low_level.read_dir (fd t) path
    let read_link t path = Low_level.read_link (fd t) path
    let chmod t ~follow ~perm path = Low_level.chmod ~follow (fd t) ~perm path

    let chown ~follow ?uid ?gid t path =
      Low_level.chown ~follow ?uid ?gid (fd t) path

    let with_dir_entries t path fn =
      fn (Low_level.read_dir_entries (fd t) path |> List.to_seq)

    (* Volatile storage: the strongest barrier it has is nothing. *)
    let sync_dir t _ = ignore (fd t)

    let rename t old_path (new_dir : [> Eio.Fs.dir_ty ] Eio.Std.r) new_path =
      match as_memfs_dir new_dir with
      | None -> invalid_arg "Target is not an eio_mem directory"
      | Some new_dir -> Low_level.rename (fd t) old_path new_dir new_path

    let rename_exclusive t _backend ~source ~target:(new_dir, new_path) =
      match as_memfs_dir new_dir with
      | None -> invalid_arg "Target is not an eio_mem directory"
      | Some new_dir ->
          Low_level.rename ~replace:false (fd t) source new_dir new_path

    let symlink ~link_to t path = Low_level.symlinkat ~link_to (fd t) path

    let open_dir t ~sw path =
      let flags = Low_level.Open_flags.(o_rdonly + o_directory) in
      let fd = Low_level.openat ~sw ~mode:0 (fd t) path flags in
      let label = last_component path in
      let full_path = Eio_fs_path.append t.dir_path path in
      let d = v ~label ~path:full_path (Low_level.Dir.Dir_fd fd) in
      (* The upcast drops the barrier tag from the static row to fit
         [Pi.DIR]; the operation itself stays probeable on the resource. *)
      (Eio.Resource.T (d, Handler.v) :> [ `Close | `Dir ] Eio.Resource.t)

    let open_subtree t ~sw path = open_dir t ~sw path
    let pp f t = Fmt.string f (String.escaped t.label)

    (* An in-memory tree has no native path. Answering one would point callers
       at an unrelated real location -- a native open of "./x" resolves
       against the process cwd -- so a barrier or lock taken "natively" on an
       in-memory directory would silently hit the real filesystem. *)
    let native t _ =
      ignore (fd t);
      None

    (* The in-memory tree names its entries the way the POSIX backend does,
       so it splits and joins paths by the same syntax. *)
    include Eio_utils.Posix_path
  end

  and Handler : sig
    val v :
      ( Dir.t,
        [ `Dir | `Close | `Dir_sync | `Process_local ] )
      Eio.Resource.handler
  end = struct
    let v =
      Eio.Resource.handler
        [
          H (Eio.Fs.Pi.Dir, (module Dir));
          H (Eio.Resource.Close, Dir.close);
          H (Memfs_dir, Dir.fd);
          H (Eio_fs_sync.Rename.Exclusive, Dir.rename_exclusive);
          H (Eio_fs_sync.Dir_sync, fun t ~path -> Dir.sync_dir t path);
          (* The tree lives in this process's heap. It answers no native path
             and hands out no descriptor, so nothing outside this process can
             reach a file of it: an exclusion between fibers here is the whole
             exclusion these files admit. *)
          H
            ( Eio_fs_sync.Process_local,
              fun t ->
                ignore (Dir.fd t);
                true );
        ]
  end

  (* Full access to the file system [t]. *)
  let fs t =
    (Eio.Resource.T (Dir.v ~label:"fs" ~path:"." (Fs t), Handler.v)
      :> [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t)

  (* Access confined to the current directory of [t]. *)
  let cwd t =
    (Eio.Resource.T (Dir.v ~label:"cwd" ~path:"." (Cwd t), Handler.v)
      :> [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t)

  let v () = (fs (Low_level.v ()), "/")

  let load ?(filter = fun _ -> true) path =
    let t = Low_level.v () in
    let fs = fs t in
    let start = Eio.Path.native_exn path in
    let stat = Eio.Path.stat ~follow:true path in
    Eio.Path.mkdirs ~exists_ok:true ~perm:stat.perm (fs, start);
    let switch_filesystem (_, path) = (fs, path) in
    let rec make_fs v =
      let stat = Eio.Path.stat ~follow:false v in
      match stat.kind with
      | `Regular_file ->
          if not (filter v) then ()
          else
            Eio.Path.with_open_out ~create:(`If_missing stat.perm)
              (switch_filesystem v)
            @@ fun o ->
            Eio.Path.with_open_in v @@ fun i -> Eio.Flow.copy i o
      | `Directory ->
          if not (filter v) then ()
          else
            Eio.Path.mkdirs ~exists_ok:true ~perm:stat.perm
              (switch_filesystem v);
          let fnames = Eio.Path.read_dir v in
          List.iter (fun fname -> make_fs Eio.Path.(v / fname)) fnames
      | `Symbolic_link ->
          if not (filter v) then ()
          else
            let link_to = Eio.Path.read_link v in
            Eio.Path.symlink ~link_to (switch_filesystem v)
      | _ -> Eio.traceln "Skipping %a" Eio.Path.pp v
    in
    make_fs path;
    (fs, "/")

  let tree ~depth =
    let name (_, p) = last_component p in
    let rec node depth path =
      match Eio.Path.kind ~follow:false path with
      | `Regular_file ->
          Some (Console.Tree.Node (Console.Span.text (name path), []))
      | `Directory ->
          let children =
            if depth <= 1 then []
            else
              Eio.Path.read_dir path
              |> List.filter_map (fun child ->
                  node (depth - 1) Eio.Path.(path / child))
          in
          Some (Console.Tree.Node (Console.Span.text (name path), children))
      | `Symbolic_link ->
          let label = name path ^ " -> " ^ Eio.Path.read_link path in
          Some (Console.Tree.Node (Console.Span.text label, []))
      | _ -> None
    in
    fun ppf path ->
      if depth > 0 then
        Option.iter
          (fun n -> Console.Tree.pp ppf (Console.Tree.of_tree n))
          (node depth path)
end

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

  let find entries path =
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
            Low_level.embed (Low_level.Dir.Fs t) ~perm path ~contents
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

let run ?cwd fn =
  let t = Low_level.v ?cwd () in
  let fs = (Eio.Path.of_dir (Fs.fs t) :> Eio.Fs.dir_ty Eio.Path.t) in
  let cwd = (Eio.Path.of_dir (Fs.cwd t) :> Eio.Fs.dir_ty Eio.Path.t) in
  fn
    object
      method fs = fs
      method cwd = cwd
    end
