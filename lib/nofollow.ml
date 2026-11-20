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

type error = [ `Invalid of string | `Too_large ]

let read_file ~max_bytes ~validate path =
  if max_bytes < 0 || max_bytes = max_int then
    Fmt.invalid_arg
      "Eio_fs.Nofollow.read_file: max_bytes is %d, must be between 0 and \
       max_int - 1"
      max_bytes;
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw ~follow:false path in
  Eio_fs_internal.bounded_read ~max_bytes ~validate ~stat:(Eio.File.stat file)
    ~read:(Eio.Flow.single_read file)

let run ~max_bytes ~validate ~stat ~read =
  if max_bytes < 0 || max_bytes = max_int then
    Fmt.invalid_arg
      "Eio_fs.Nofollow.run: max_bytes is %d, must be between 0 and max_int - 1"
      max_bytes;
  Eio_fs_internal.bounded_read ~max_bytes ~validate ~stat ~read

(* Once opened, a file may have lost its last name to a rename over it,
   which leaves the descriptor reading the file it opened. *)
let refused ~opened (s : Eio.File.Stat.t) =
  match s.kind with
  | `Regular_file when Int64.equal s.nlink 1L -> None
  | `Regular_file when opened && Int64.equal s.nlink 0L -> None
  | `Regular_file -> Fmt.kstr Option.some "a file of %Ld links" s.nlink
  | kind ->
      Fmt.kstr Option.some "a %a, not a regular file" Eio.File.Stat.pp_kind kind

let with_regular path f =
  match refused ~opened:false (Eio.Path.stat ~follow:false path) with
  | Some why -> Error (`Invalid why)
  | None -> (
      Eio.Path.with_open_in ~follow:false path @@ fun file ->
      let stat = Eio.File.stat file in
      match refused ~opened:true stat with
      | Some why -> Error (`Invalid why)
      | None -> Ok (f file stat))

type Eio.Exn.Backend.t += Unsafe_parent of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Unsafe_parent reason ->
        Fmt.string f reason;
        true
    | _ -> false)

let fail_unsafe_parent fmt =
  Fmt.kstr
    (fun reason ->
      raise (Eio.Fs.err (Eio.Fs.Permission_denied (Unsafe_parent reason))))
    fmt

(* A missing parent is made as [create] says, and one another caller made
   meanwhile is stated all the same. *)
let stat_parent ~create next =
  match Eio.Path.stat ~follow:false next with
  | stat -> stat
  | exception (Eio.Io (Eio.Fs.E (Not_found _), _) as e) -> (
      match create with
      | `Never -> raise e
      | `If_missing perm ->
          (try Eio.Path.mkdir ~perm next
           with Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
          Eio.Path.stat ~follow:false next)

(* Each parent is stated, opened, and stated again through the opened handle:
   one replaced between the two, by a link or anything else, is refused. A
   failure of these steps names the path asked for; what [fn] raises passes
   through. *)
let with_parent ?(create = `Never) (input : _ Eio.Path.t) fn =
  let root, path = (input :> Eio.Fs.dir_ty Eio.Path.t) in
  let parent_step f =
    try f ()
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "opening the parents of %S" path
  in
  let parents, leaf =
    match Eio_fs_internal.beneath path with
    | Ok parts -> parts
    | Error reason -> fail_unsafe_parent "%s" reason
  in
  let rec descend (dir : Eio.Fs.dir_ty Eio.Path.t) = function
    | [] -> fn Eio.Path.(dir / leaf)
    | name :: rest ->
        Eio.Switch.run @@ fun sw ->
        let held =
          parent_step @@ fun () ->
          let next = Eio.Path.(dir / name) in
          let before = stat_parent ~create next in
          if before.kind <> `Directory then
            fail_unsafe_parent "%S has a non-directory parent" path;
          let held = Eio.Path.open_dir ~sw next in
          let after = Eio.Path.stat ~follow:false held in
          if
            not
              (Int64.equal before.dev after.dev
              && Int64.equal before.ino after.ino
              && after.kind = `Directory)
          then fail_unsafe_parent "%S changed while opening its parent" path;
          held
        in
        descend (held :> Eio.Fs.dir_ty Eio.Path.t) rest
  in
  match parents with
  | [] ->
      Eio.Switch.run @@ fun sw ->
      let held =
        parent_step @@ fun () ->
        Eio.Path.open_dir ~sw Eio.Path.(of_dir root / ".")
      in
      descend (held :> Eio.Fs.dir_ty Eio.Path.t) []
  | _ -> descend (Eio.Path.of_dir root) parents
