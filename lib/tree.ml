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

let allocated path = (own (Eio.Path.stat ~follow:false path)).allocated_bytes

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
  Option.iter
    (fun n ->
      if n <= 0 then
        Fmt.invalid_arg "Eio_fs.Tree.usage: max_fibers is %d, must be positive"
          n)
    max_fibers;
  let total = ref empty in
  Eio_fs_internal.walk ?max_fibers
    (fun _ _ stat -> total := add !total (own stat))
    path;
  !total

let same_file a b =
  let stat path =
    match Eio.Path.stat ~follow:true path with
    | stat -> Some stat
    | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> None
  in
  match (stat a, stat b) with
  | Some a, Some b -> Int64.equal a.dev b.dev && Int64.equal a.ino b.ino
  | _ -> false

(* A failed rmdir(2) is read again rather than by its errno, which POSIX
   lets be EEXIST or ENOTEMPTY for a directory that holds anything. *)
let rec remove_empty = function
  | [] -> Ok ()
  | d :: rest -> (
      match Eio.Path.rmdir d with
      | () -> remove_empty rest
      | exception (Eio.Io _ as e) -> (
          match Eio.Path.read_dir d with
          | _ :: _ -> Ok ()
          | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok ()
          | [] | (exception Eio.Io _) -> Error (d, e)))

let rec links dir rel =
  let at = Eio.Path.(dir / rel) in
  match Eio.Path.kind ~follow:false at with
  | `Symbolic_link -> Some [ (rel, Eio.Path.read_link at) ]
  | `Directory -> (
      let stem =
        if String.ends_with ~suffix:"/" rel then
          String.sub rel 0 (String.length rel - 1)
        else rel
      in
      match Eio.Path.read_dir at with
      | [] -> None
      | names ->
          List.fold_left
            (fun acc n ->
              Option.bind acc (fun acc ->
                  Option.map (List.append acc) (links dir (stem ^ "/" ^ n))))
            (Some []) names)
  | _ -> None

exception Shared of string

let fail_shared fmt = Fmt.kstr (fun why -> raise (Shared why)) fmt

(* Each copy is made at a name of its own beside [spare], as the walk copies
   the files of a level in several fibers. *)
let unshare ~spare path =
  let copies = Atomic.make 0 in
  let copy_apart file (stat : Eio.File.Stat.t) =
    let dir, stem = spare in
    let n = Atomic.fetch_and_add copies 1 + 1 in
    let spare = (dir, Fmt.str "%s.%d" stem n) in
    try
      Eio.Path.with_open_in file (fun src ->
          Eio.Path.with_open_out
            ~create:(`Exclusive (stat.perm land 0o777))
            spare
            (fun dst -> Eio.Flow.copy src dst));
      Eio.Path.rename spare file
    with Eio.Io _ as e ->
      fail_shared "%s, linked %Ld times, cannot be copied apart: %a"
        (Eio_fs_internal.native_name file)
        stat.nlink Eio.Exn.pp e
  in
  let shared (stat : Eio.File.Stat.t) =
    stat.kind = `Regular_file && Int64.compare stat.nlink 1L > 0
  in
  match
    Eio_fs_internal.walk
      (fun _ f stat -> if shared stat then copy_apart f stat)
      path
  with
  | () -> Ok ()
  | exception Shared why -> Error why

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
  let name (_, p) = Eio_fs_internal.last_component p in
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
