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
open Eio_fs_mem_core

type Eio.Exn.Backend.t += Eio_fs = Low_level.Eio_fs

type user = Low_level.User.t = {
  uid : int64;
  gid : int64;
  groups : int64 list;
  umask : int;
}

let root = Low_level.User.root
let v = Memfs.v
let as_user = Memfs.as_user

let load ?os ?clock ?filter path =
  Image.load ?os ?clock (Image.scan ?filter ~at:(snd path) path)

module Crunch = Crunch
module Resource = Low_level.Resource

type stdenv =
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; cwd : Eio.Fs.dir_ty Eio.Path.t
  ; stdin : Eio.Flow.source_ty Eio.Flow.source
  ; stdout : Eio.Flow.sink_ty Eio.Flow.sink
  ; stderr : Eio.Flow.sink_ty Eio.Flow.sink
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Process.mgr >

let make_process_mgr user t =
  let module Process_mgr = struct
    type t = unit
    type tag = [ `Generic ]

    let pipe () ~sw =
      let r, w = Low_level.pipe ~sw user t in
      let r, w = (Low_level.Flow.of_fd r, Low_level.Flow.of_fd w) in
      ( (r :> [ Eio.Flow.source_ty | `Close ] Eio.Flow.source),
        (w :> [ Eio.Flow.sink_ty | `Close ] Eio.Flow.sink) )

    let spawn _ = failwith "Eio_fs_mem does not support spawning subprocesses."
    let environment _ = Eio.Process.Env.empty
    let getenv_opt t v = environment t |> Eio.Process.Env.get_opt v
  end in
  let handler = Eio.Process.Pi.mgr (module Process_mgr) in
  Eio.Resource.T ((), handler)

(* Root owns the tree and makes the current directory, which it gives to
   [user]. *)
let absolute os cwd =
  match os with
  | Some `Windows -> Eio_utils.Nt_path.classify cwd = `Absolute
  | Some (`Linux | `Macos) | None -> String.starts_with ~prefix:"/" cwd

let run ?os ?(user = root) ?clock ?drives ?cwd ?stdin ?stdout ?stderr fn =
  let t = Low_level.v ?clock ?cwd ?os ?drives ?stdin ?stdout ?stderr () in
  let cwd = Option.value ~default:(Low_level.root_path t) cwd in
  if not (absolute os cwd) then
    Fmt.invalid_arg "Eio_fs_mem.run: cwd is %S, must be an absolute path" cwd;
  let admin = (Eio.Path.of_dir (Memfs.fs t) :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(admin / cwd);
  if not (Int64.equal user.uid 0L || Low_level.root_path t <> "/") then
    Eio.Path.chown ~follow:true ~uid:user.uid ~gid:user.gid
      Eio.Path.(admin / cwd);
  let fs = (Eio.Path.of_dir (Memfs.fs ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  let cwd = (Eio.Path.of_dir (Memfs.cwd ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  let stdin =
    (Low_level.Flow.of_fd (Low_level.stdin user t)
      :> Eio.Flow.source_ty Eio.Flow.source)
  in
  let stdout =
    (Low_level.Flow.of_fd (Low_level.stdout user t)
      :> Eio.Flow.sink_ty Eio.Flow.sink)
  in
  let stderr =
    (Low_level.Flow.of_fd (Low_level.stderr user t)
      :> Eio.Flow.sink_ty Eio.Flow.sink)
  in
  (* Resolve the current directory now: it then follows its directory. *)
  ignore (Eio.Path.stat ~follow:true cwd : Eio.File.Stat.t);
  fn
    object
      method fs = fs
      method cwd = cwd
      method stdin = stdin
      method stdout = stdout
      method stderr = stderr
      method process_mgr = make_process_mgr user t
    end
