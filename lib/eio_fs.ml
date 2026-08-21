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

module Path = struct
  type usage = { allocated_bytes : int64; latest_mtime : float }

  let add_bytes a b =
    if Int64.sub Int64.max_int a < b then Int64.max_int else Int64.add a b

  let bytes_of_blocks blocks =
    if blocks <= 0L then 0L
    else if blocks > Int64.div Int64.max_int 512L then Int64.max_int
    else Int64.mul blocks 512L

  let own stat =
    {
      allocated_bytes = bytes_of_blocks stat.Eio.File.Stat.blocks;
      latest_mtime = stat.mtime;
    }

  let add a b =
    {
      allocated_bytes = add_bytes a.allocated_bytes b.allocated_bytes;
      latest_mtime = Float.max a.latest_mtime b.latest_mtime;
    }

  let usage_entry path =
    let stat = Eio.Path.stat ~follow:false path in
    let children =
      if stat.kind <> `Directory then []
      else
        Eio.Path.read_dir path
        |> List.rev_map (fun name -> Eio.Path.(path / name))
    in
    (own stat, children)

  let usage_batch paths =
    List.fold_left
      (fun (total, next) path ->
        let entry, children = usage_entry path in
        (add total entry, List.rev_append children next))
      ({ allocated_bytes = 0L; latest_mtime = 0. }, [])
      paths

  (* [Fiber.List.map ~max_fibers] creates one fiber per input and parks the
     excess on its limiter. Large, flat directories therefore create thousands
     of short-lived fibers even though only [max_fibers] can do useful work.
     Partition the level first so the number of fibers is bounded structurally;
     each worker then performs its share sequentially. *)
  let usage_level ~max_fibers paths =
    let worker_count = Int.min max_fibers (List.length paths) in
    let work = Array.make worker_count [] in
    List.iteri
      (fun i path ->
        let worker = i mod worker_count in
        work.(worker) <- path :: work.(worker))
      paths;
    match Array.to_list work with
    | [ batch ] -> [ usage_batch batch ]
    | batches -> Eio.Fiber.List.map usage_batch batches

  let usage ?(max_fibers = 32) path =
    if max_fibers <= 0 then invalid_arg "Eio_fs.Path.usage: max_fibers <= 0";
    let rec loop total = function
      | [] -> total
      | level ->
          let batches = usage_level ~max_fibers level in
          let total, next =
            List.fold_left
              (fun (total, next) (batch, children) ->
                (add total batch, List.rev_append children next))
              (total, []) batches
          in
          loop total next
    in
    loop { allocated_bytes = 0L; latest_mtime = 0. } [ path ]

  (* TODO: implement. [name] answers [Eio.Path.native] when the capability has
     a native spelling for the path, and the [Eio.Path.pp] rendering when it
     has none. *)
  let name _ = failwith "TODO: implement Eio_fs.Path.name"
end

module Fs = struct
  type dir_fd = Low_level.Dir.fd

  (* When renaming, we get a plain [Eio.Fs.dir]. We need extra access to check
   that the new location is within its sandbox. *)
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
  end = struct
    type t = { fd : dir_fd; dir_path : string; label : string }

    let fd t = t.fd
    let v ~label ~path:dir_path fd = { fd; dir_path; label }

    let open_in t ~sw path =
      let fd =
        Low_level.openat ~mode:0 ~sw t.fd path Low_level.Open_flags.o_rdonly
      in
      (Low_level.Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

    let open_out t ~sw ~append ~create path =
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
      let flags = Low_level.Open_flags.(flags + o_rdwr) in
      match Low_level.openat ~sw ~mode t.fd path flags with
      | fd -> (Low_level.Flow.of_fd fd :> Eio.File.rw_ty r)

    let mkdir t ~perm path = Low_level.mkdirat ~perm t.fd path
    let unlink t path = Low_level.unlink t.fd path
    let rmdir t path = Low_level.rmdir t.fd path
    let stat t ~follow path = Low_level.statat ~follow t.fd path
    let read_dir t path = Low_level.read_dir t.fd path
    let read_link t path = Low_level.read_link t.fd path
    let chmod t ~follow ~perm path = Low_level.chmod ~follow t.fd ~perm path

    let chown ~follow ?uid ?gid t path =
      Low_level.chown ~follow ?uid ?gid t.fd path

    let with_dir_entries t path fn =
      fn (Low_level.read_dir_entries t.fd path |> List.to_seq)

    let rename t old_path (new_dir : [> Eio.Fs.dir_ty ] Eio.Std.r) new_path =
      match as_memfs_dir new_dir with
      | None -> invalid_arg "Target is not an eio_mem directory"
      | Some new_dir -> Low_level.rename t.fd old_path new_dir new_path

    let symlink ~link_to t path = Low_level.symlinkat ~link_to t.fd path

    let open_dir t ~sw path =
      let flags = Low_level.Open_flags.(o_rdonly + o_directory) in
      let fd = Low_level.openat ~sw ~mode:0 t.fd path flags in
      let label = Filename.basename path in
      let full_path =
        if Filename.is_relative path then Filename.concat t.dir_path path
        else path
      in
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
    let native _ _ = None
  end

  and Handler : sig
    val v : (Dir.t, [ `Dir | `Close | `Dir_sync ]) Eio.Resource.handler
  end = struct
    let v =
      Eio.Resource.handler
        [
          H (Eio.Fs.Pi.Dir, (module Dir));
          H (Memfs_dir, Dir.fd);
          (* Volatile storage: the strongest barrier it has is nothing. *)
          H (Eio_fs_sync.Dir_sync, fun _ ~path:_ -> ());
        ]
  end

  (* Full access to the filesystem. *)
  let fs = Eio.Resource.T (Dir.v ~label:"fs" ~path:"." Fs, Handler.v)
  let cwd = Eio.Resource.T (Dir.v ~label:"cwd" ~path:"." Cwd, Handler.v)
  let v () = ((fs :> [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t), "/")

  let load ?(filter = fun _ -> true) path =
    Low_level.reset ();
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
    ()

  let tree ~depth =
    let name (_, p) = Filename.basename p in
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

  let load entries =
    Low_level.reset ();
    let mem path = (Fs.fs, path) in
    List.iter
      (function
        | Dir { path; perm } ->
            if path <> "/" then Eio.Path.mkdirs ~exists_ok:true ~perm (mem path);
            Eio.Path.chmod ~follow:false ~perm (mem path)
        | File { path; perm; contents } ->
            Low_level.embed Low_level.Dir.Fs ~perm path ~contents
        | Symlink { path; target } ->
            Eio.Path.symlink ~link_to:target (mem path))
      entries;
    Fs.v ()

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
     (** [load ()] resets the in-memory filesystem, populates it with\n\
    \    {!entries} and returns its root. *)\n"
end

module Chaos = Chaos

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >

let run ?(cwd = "/") fn =
  Low_level.reset ();
  let o =
    object
      method fs = ((Fs.fs, "") :> Eio.Fs.dir_ty Eio.Path.t)
      method cwd = ((Fs.cwd, "") :> Eio.Fs.dir_ty Eio.Path.t)
    end
  in
  Eio.Fiber.with_binding Low_level.Directory.cwd cwd @@ fun () -> fn o
