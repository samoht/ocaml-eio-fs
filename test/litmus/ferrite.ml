(* Ported from Ferrite's Rosette models (https://github.com/uwplse/ferrite,
   commit on master, MIT licence; see LICENSE.md):

   - [lang.rkt]: the inode operations and the dependency predicates;
   - [seqfs.rkt], [advfs.rkt], [ext4.rkt]: [crack], [execute], [ondisk] and
     [reorder?];
   - [verifier.rkt]: [all-outcomes], here an explicit enumeration of every
     prefix of every valid ordering instead of a solver loop.

   Deviations, each because the Rosette code relies on a numbering
   coincidence this port has no need for:

   - files are named by strings and inodes are allocated fresh on [creat];
     Rosette pre-assigns inode [i] to name [i], so a test's names and
     descriptors must both start at 0;
   - "same inode" compares inodes: Rosette's [file-same-ino-deps?] compares a
     directory operation's name slot with a file operation's descriptor slot;
   - ext4's allocated length is tracked per inode: Rosette computes it once
     per [crack] from the descriptor table, so a descriptor created in the
     body inherits the allocation of whatever inode its slot held.

   Additions: [Pwrite] over existing bytes (the paper's overwrite tests, absent
   from the Rosette language), [Mark] (the paper's Definition 3: a mark comes
   before every later event), and the [writeback] and [local_fsync] variants of
   ext4, which are ours. *)

open Litmus

type ext4 = {
  block_size : int;
  nodelalloc : bool;
  writeback : bool;
  local_fsync : bool;
}

type model = Seqfs | Advfs | Ext4 of ext4

let ext4 ?(block_size = 4096) ?(nodelalloc = false) () =
  Ext4 { block_size; nodelalloc; writeback = false; local_fsync = false }

let envelope ?(block_size = 4096) () =
  Ext4 { block_size; nodelalloc = false; writeback = true; local_fsync = true }

let pp_model ppf = function
  | Seqfs -> Fmt.string ppf "seqfs"
  | Advfs -> Fmt.string ppf "advfs"
  | Ext4 e ->
      Fmt.pf ppf "ext4%s%s%s"
        (if e.nodelalloc then "+nodelalloc" else "")
        (if e.writeback then "+writeback" else "")
        (if e.local_fsync then "+local-fsync" else "")

(* {1 Inode operations (lang.rkt)} *)

type iop =
  | Dir_add of { name : string; ino : int }
  | Dir_rename of {
      src : string;
      dst : string;
      moved : int;
      replaced : int option;
    }
  | File_write of { ino : int; off : int; data : string }
  | File_setsize of { ino : int; size : int }
  | File_extend of { ino : int; off : int; data : string; size : int }
  | Swap of int
  | Mark_op

let is_dir = function Dir_add _ | Dir_rename _ -> true | _ -> false
let is_swap = function Swap _ -> true | _ -> false

let inodes = function
  | Dir_add { ino; _ } -> [ ino ]
  | Dir_rename { moved; replaced; _ } -> moved :: Option.to_list replaced
  | File_write { ino; _ } | File_setsize { ino; _ } | File_extend { ino; _ } ->
      [ ino ]
  | Swap ino -> [ ino ]
  | Mark_op -> []

let file_ino = function
  | File_write { ino; _ } | File_setsize { ino; _ } | File_extend { ino; _ } ->
      Some ino
  | _ -> None

let size_ino = function
  | File_setsize { ino; _ } | File_extend { ino; _ } -> Some ino
  | _ -> None

let block bs = function
  | File_write { ino; off; _ } | File_extend { ino; off; _ } ->
      Some (ino, off / bs)
  | _ -> None

(* [dir-same-ino-deps?]: one directory, so any two directory operations. *)
let dir_same a b = is_dir a && is_dir b

(* [barrier-deps?] *)
let barrier a b = is_swap a || is_swap b

(* [file-same-ino-deps?] *)
let file_same_ino a b = List.exists (fun i -> List.mem i (inodes b)) (inodes a)

(* [metadata-same-ino-deps?] *)
let metadata_same_ino a b =
  match (size_ino a, size_ino b) with Some i, Some j -> i = j | _ -> false

(* [same-file-block-deps?] *)
let same_file_block bs a b =
  match (block bs a, block bs b) with Some x, Some y -> x = y | _ -> false

(* [file-write-extend-deps?], checked both ways as [valid-ordering] does. *)
let file_write_extend ~nodelalloc a b =
  let one w x =
    match (w, x) with
    | File_write { ino; _ }, File_extend { ino = j; _ } -> ino = j
    | File_write { ino; _ }, File_setsize { ino = j; _ } ->
        nodelalloc && ino = j
    | _ -> false
  in
  one a b || one b a

(* Definition 3: an fsync comes before every later event, and after every
   earlier update of its own file. *)
let local_barrier a b =
  is_swap a || match b with Swap i -> file_ino a = Some i | _ -> false

(* [a] comes before [b] in program order: must it stay before? *)
let must_precede model a b =
  match a with
  | Mark_op -> true
  | _ -> (
      match model with
      | Seqfs -> true
      | Advfs -> dir_same a b || barrier a b || file_same_ino a b
      | Ext4 e ->
          dir_same a b
          || (if e.local_fsync then local_barrier a b else barrier a b)
          || metadata_same_ino a b
          || same_file_block e.block_size a b
          || file_write_extend ~nodelalloc:e.nodelalloc a b)

(* {1 State and execution} *)

module SM = Map.Make (String)
module IM = Map.Make (Int)

(* [ondisk] holds seqfs's and ext4's bytes and advfs's on-disk bytes;
   [incache] advfs's in-core bytes; [size] ext4's size attribute. *)
type file = { ondisk : string; incache : string; size : int }
type state = { dir : int SM.t; files : file IM.t }

let empty_file = { ondisk = ""; incache = ""; size = 0 }
let empty = { dir = SM.empty; files = IM.empty }
let file st ino = Option.value ~default:empty_file (IM.find_opt ino st.files)

(* Write [data] at [off], padding a gap with NUL (Rosette's #f). *)
let splice s off data =
  let s =
    if off > String.length s then s ^ String.make (off - String.length s) '\000'
    else s
  in
  let len = String.length s in
  let off_end = off + String.length data in
  String.sub s 0 off ^ data
  ^ if off_end < len then String.sub s off_end (len - off_end) else ""

let execute model st op =
  let set ino f = { st with files = IM.add ino f st.files } in
  match op with
  | Dir_add { name; ino } -> (
      let st = { st with dir = SM.add name ino st.dir } in
      (* ext4.rkt leaves the inode as it is; the others reset it. *)
      match model with
      | Ext4 _ -> st
      | Seqfs | Advfs -> { st with files = IM.add ino empty_file st.files })
  | Dir_rename { src; dst; moved; _ } ->
      if String.equal src dst then st
      else
        let moved = Option.value ~default:moved (SM.find_opt src st.dir) in
        let files =
          match SM.find_opt dst st.dir with
          | Some old -> IM.add old empty_file st.files
          | None -> st.files
        in
        { dir = SM.add dst moved (SM.remove src st.dir); files }
  | File_write { ino; off; data } -> (
      let f = file st ino in
      match model with
      | Advfs -> set ino { f with incache = splice f.incache off data }
      | Seqfs | Ext4 _ -> set ino { f with ondisk = splice f.ondisk off data })
  | File_setsize { ino; size } -> set ino { (file st ino) with size }
  | File_extend { ino; off; data; size } ->
      let f = file st ino in
      set ino { f with ondisk = splice f.ondisk off data; size }
  | Swap ino -> (
      match model with
      | Advfs ->
          let f = file st ino in
          set ino { f with ondisk = f.incache }
      | Seqfs | Ext4 _ -> st)
  | Mark_op -> st

(* [ondisk]. ext4.rkt takes [size] bytes of the block contents; a size past
   them makes Rosette's [take] fail, which drops the state. With [writeback]
   the gap reads as zeros: allocated, never written. *)
let content model f =
  match model with
  | Seqfs | Advfs -> Some f.ondisk
  | Ext4 e ->
      let len = String.length f.ondisk in
      if f.size <= len then Some (String.sub f.ondisk 0 f.size)
      else if e.writeback then
        Some (f.ondisk ^ String.make (f.size - len) '\000')
      else None

let project model st names =
  List.fold_right
    (fun name acc ->
      match acc with
      | None -> None
      | Some l -> (
          match SM.find_opt name st.dir with
          | None -> Some ((name, Absent) :: l)
          | Some ino -> (
              match content model (file st ino) with
              | None -> None
              | Some c -> Some ((name, File c) :: l))))
    names (Some [])

(* {1 Cracking syscalls into inode operations} *)

type cracker = {
  mutable next_ino : int;
  fds : (string, int) Hashtbl.t;
  names : (string, int) Hashtbl.t;
  lengths : (int, int) Hashtbl.t;
  allocated : (int, int) Hashtbl.t;
}

let cracker () =
  {
    next_ino = 0;
    fds = Hashtbl.create 7;
    names = Hashtbl.create 7;
    lengths = Hashtbl.create 7;
    allocated = Hashtbl.create 7;
  }

let get tbl ino = Option.value ~default:0 (Hashtbl.find_opt tbl ino)
let roundup x bs = if x mod bs = 0 then x else x + bs - (x mod bs)

(* ext4.rkt: fill the current block, then one operation per block, an extend
   for a block not yet allocated. *)
let crack_ext4_write e c ino cnt =
  let bs = e.block_size in
  let len = String.length cnt in
  let offset = get c.lengths ino in
  let offset_end = offset + len in
  let ops = ref [] in
  let emit l = ops := !ops @ l in
  let grow n = Hashtbl.replace c.lengths ino (get c.lengths ino + n) in
  if offset <> roundup offset bs then begin
    let n = min (roundup offset bs - offset) len in
    emit
      [
        File_write { ino; off = offset; data = String.sub cnt 0 n };
        File_setsize { ino; size = offset + n };
      ];
    grow n
  end;
  let pos = ref (roundup offset bs) in
  while !pos < offset_end do
    let pos_end = !pos + bs in
    let lo = !pos - offset and hi = min (pos_end - offset) len in
    let chunk = String.sub cnt lo (hi - lo) in
    grow (String.length chunk);
    if pos_end <= get c.allocated ino then
      emit
        [
          File_write { ino; off = !pos; data = chunk };
          File_setsize { ino; size = pos_end };
        ]
    else begin
      let data = chunk ^ String.make (bs - String.length chunk) '\000' in
      if e.writeback then
        emit
          [
            File_write { ino; off = !pos; data };
            File_setsize { ino; size = offset_end };
          ]
      else emit [ File_extend { ino; off = !pos; data; size = offset_end } ];
      Hashtbl.replace c.allocated ino pos_end
    end;
    pos := pos_end
  done;
  !ops

let unsupported op =
  let name =
    match op with
    | Mkdir _ -> "mkdir"
    | Unlink _ -> "unlink"
    | Symlink _ -> "symlink"
    | Truncate _ -> "truncate"
    | Fsync_dir _ -> "fsync of a directory"
    | Fsync_fails _ -> "a failing fsync"
    | _ -> "this operation"
  in
  Fmt.invalid_arg "Ferrite models no %s" name

let crack model c op =
  let ino_of fd =
    match Hashtbl.find_opt c.fds fd with
    | Some ino -> ino
    | None -> Fmt.invalid_arg "unknown descriptor %s" fd
  in
  match op with
  | Creat name ->
      let ino = c.next_ino in
      c.next_ino <- ino + 1;
      Hashtbl.replace c.fds name ino;
      Hashtbl.replace c.names name ino;
      [ Dir_add { name; ino } ]
  | Rename (src, dst) ->
      let moved = Hashtbl.find c.names src in
      let replaced = Hashtbl.find_opt c.names dst in
      Hashtbl.remove c.names src;
      Hashtbl.replace c.names dst moved;
      [ Dir_rename { src; dst; moved; replaced } ]
  | Fsync fd -> [ Swap (ino_of fd) ]
  | Mark -> [ Mark_op ]
  | Write (fd, data) -> (
      let ino = ino_of fd in
      match model with
      | Seqfs | Advfs ->
          let off = get c.lengths ino in
          Hashtbl.replace c.lengths ino (off + String.length data);
          [ File_write { ino; off; data } ]
      | Ext4 e -> crack_ext4_write e c ino data)
  | Pwrite (fd, off, data) -> (
      let ino = ino_of fd in
      let len = String.length data in
      if off + len > get c.lengths ino then
        invalid_arg "Ferrite: Pwrite past the end of the file";
      match model with
      | Seqfs | Advfs -> [ File_write { ino; off; data } ]
      | Ext4 e ->
          let bs = e.block_size in
          List.init
            (((off + len - 1) / bs) - (off / bs) + 1)
            (fun i ->
              let b = (off / bs) + i in
              let lo = max off (b * bs)
              and hi = min (off + len) ((b + 1) * bs) in
              File_write
                { ino; off = lo; data = String.sub data (lo - off) (hi - lo) }))
  | Mkdir _ | Unlink _ | Symlink _ | Truncate _ | Fsync_dir _ | Fsync_fails _ ->
      unsupported op

(* {1 Enumeration (verifier.rkt)} *)

let explore model ops init names =
  let ops = Array.of_list ops in
  let n = Array.length ops in
  if n > 62 then invalid_arg "Ferrite: program too long";
  let preds =
    Array.init n (fun j ->
        let m = ref 0 in
        for i = 0 to j - 1 do
          if must_precede model ops.(i) ops.(j) then m := !m lor (1 lsl i)
        done;
        !m)
  in
  let marks = ref 0 in
  Array.iteri
    (fun i op -> if op = Mark_op then marks := !marks lor (1 lsl i))
    ops;
  let seen = Hashtbl.create 1024 in
  let out = ref Set.empty in
  let rec visit mask st =
    let key = (mask, SM.bindings st.dir, IM.bindings st.files) in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.add seen key ();
      (match project model st names with
      | Some entries ->
          let marked = !marks <> 0 && mask land !marks = !marks in
          out := Set.add { marked; entries } !out
      | None -> ());
      for j = 0 to n - 1 do
        if mask land (1 lsl j) = 0 && preds.(j) land mask = preds.(j) then
          visit (mask lor (1 lsl j)) (execute model st ops.(j))
      done
    end
  in
  visit 0 init;
  !out

(* The setup runs in order, cannot crash, and ends with the paper's implicit
   whole-system sync. *)
let outcomes model (t : Litmus.t) =
  let c = cracker () in
  let st =
    List.fold_left
      (fun st op -> List.fold_left (execute model) st (crack model c op))
      empty t.setup
  in
  let st =
    match model with
    | Advfs ->
        let sync f = { f with ondisk = f.incache } in
        { st with files = IM.map sync st.files }
    | Seqfs | Ext4 _ -> st
  in
  explore model (List.concat_map (crack model c) t.body) st t.observe
