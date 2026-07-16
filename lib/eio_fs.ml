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
      Eio.Resource.T (d, Handler.v)

    let open_subtree t ~sw path = open_dir t ~sw path
    let pp f t = Fmt.string f (String.escaped t.label)

    let native_internal t path =
      if Filename.is_relative path then
        let p =
          if t.dir_path = "." then path else Filename.concat t.dir_path path
        in
        if p = "" then "."
        else if p = "." then p
        else if Filename.is_implicit p then "./" ^ p
        else p
      else path

    let native t path = Some (native_internal t path)
  end

  and Handler : sig
    val v : (Dir.t, [ `Dir | `Close ]) Eio.Resource.handler
  end = struct
    let v =
      Eio.Resource.handler
        [ H (Eio.Fs.Pi.Dir, (module Dir)); H (Memfs_dir, Dir.fd) ]
  end

  (* Full access to the filesystem. *)
  let fs = Eio.Resource.T (Dir.v ~label:"fs" ~path:"." Fs, Handler.v)
  let cwd = Eio.Resource.T (Dir.v ~label:"cwd" ~path:"." Cwd, Handler.v)
  let v () = (fs, "/")

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
    let name path = Filename.basename (Eio.Path.native_exn path) in
    let rec pp_nodes ~depth prefix ppf = function
      | [] -> ()
      | [ last ] -> pp_node ~depth prefix true ppf last
      | current :: rest ->
          pp_node ~depth prefix false ppf current;
          pp_nodes ~depth prefix ppf rest
    and pp_node ~depth prefix is_last ppf v =
      if depth <= 0 then ()
      else
        match Eio.Path.kind ~follow:false v with
        | `Regular_file ->
            let marker = if is_last then "└── " else "├── " in
            Fmt.pf ppf "%s%s%s@." prefix marker (name v)
        | `Directory ->
            let marker = if is_last then "└── " else "├── " in
            Fmt.pf ppf "%s%s%s@." prefix marker (name v);
            let next_prefix = prefix ^ if is_last then "    " else "│   " in
            let children = Eio.Path.read_dir v |> List.map Eio.Path.(( / ) v) in
            pp_nodes ~depth:(depth - 1) next_prefix ppf children
        | `Symbolic_link ->
            let marker = if is_last then "└── " else "├── " in
            Fmt.pf ppf "%s%s%s -> %s@." prefix marker (name v)
              (Eio.Path.read_link v)
        | _ -> ()
    in
    fun ppf v -> pp_node ~depth "" true ppf v
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
