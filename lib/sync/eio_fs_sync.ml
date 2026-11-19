open! Hermetic
module Rename = Rename

(* The directory barrier with its strength, which [Eio.Path.sync_dir] does
   not take. *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync :
      ( 't,
        't -> path:string -> full:bool -> unit,
        [> `Dir_sync ] )
      Eio.Resource.pi

let dir_barrier (Eio.Resource.T (v, ops)) =
  Option.map (fun f -> f v) (Eio.Resource.get_opt ops Dir_sync)

(* The file-data barrier. A file is itself the resource, so its operation takes
   no name. A registered barrier is answered whole: syncing the file behind it
   would be a barrier the wrapper never recorded. *)
type (_, _, _) Eio.Resource.pi +=
  | File_sync : ('t, 't -> full:bool -> unit, [> `File_sync ]) Eio.Resource.pi

let file_barrier (Eio.Resource.T (v, ops)) =
  Option.map (fun f ~full -> f v ~full) (Eio.Resource.get_opt ops File_sync)

(* Any handle on the file, read-only included: its registered barrier, else
   the write side the POSIX backends give every handle. *)
let sync_any_file (Eio.Resource.T (v, ops) as file) =
  match (file_barrier file, Eio.Resource.get_opt ops Eio.File.Pi.Write) with
  | Some barrier, _ -> barrier ~full:false
  | None, Some (module W) -> W.sync v
  | None, None ->
      invalid_arg
        "Eio_fs.Sync: the file to sync has a backend with no sync, and \
         registers no File_sync barrier"

let can_sync (Eio.Resource.T (_, ops) as file) =
  Option.is_some (file_barrier file)
  || Option.is_some (Eio.Resource.get_opt ops Eio.File.Pi.Write)

(* A barrier is the pair a publication issues, packed with its state as a
   rename backend is. [Skip] issues none, and lets a publication skip the
   walk a barrier would need. *)
module Barrier = struct
  module type S = sig
    type t

    val sync_file : t -> _ Eio.File.ro -> unit
    val sync_dir : t -> _ Eio.Path.t -> unit
  end

  type t = Skip | T : (module S with type t = 'a) * 'a -> t

  module Make (B : S) = struct
    let v b = T ((module B), b)
  end

  let none = Skip
  let issues = function Skip -> false | T _ -> true

  module Portable = struct
    type t = unit

    let sync_file () file = sync_any_file file
    let sync_dir () path = Eio.Path.sync_dir path
  end

  let sync_file t file =
    match t with Skip -> () | T ((module B), b) -> B.sync_file b file

  let sync_dir t path =
    match t with Skip -> () | T ((module B), b) -> B.sync_dir b path

  (* [Eio.Path.split] keeps the directory capability attached, where
     [Filename.dirname] would drop the confinement. A path with no parent is
     the root of the capability, so its barrier is its own. *)
  let sync_parent t p =
    match Eio.Path.split p with
    | Some (parent, _) -> sync_dir t parent
    | None -> sync_dir t p

  let portable = T ((module Portable), ())
end

(* Establish each traversed entry's durability, including an existing one:
   it may be visible after an earlier writer's barrier failed. The capability
   root is the caller's durable anchor; never ascend beyond it. *)
let rec mkdirs_with ~barrier ~exists_ok ~perm p =
  let entry = Eio.Path.split p in
  let p =
    match entry with
    | None -> p
    | Some (parent, name) -> Eio.Path.(parent / name)
  in
  Option.iter
    (fun (parent, _) ->
      match Eio.Path.split parent with
      | None -> ()
      | Some _ -> mkdirs_with ~barrier ~exists_ok:true ~perm parent)
    entry;
  (match Eio.Path.mkdir ~perm p with
  | () -> ()
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _)
    when exists_ok && Eio.Path.kind ~follow:false p = `Directory ->
      ());
  Barrier.sync_parent barrier p

let mkdirs ?(exists_ok = false) ?(barrier = Barrier.portable) ~perm p =
  mkdirs_with ~barrier ~exists_ok ~perm p

(* A backend whose files live in this process's heap admits no outside opener,
   so an in-process exclusion is the whole exclusion; it registers
   [Process_local]. Silence reads as admitting outside openers. *)
type (_, _, _) Eio.Resource.pi +=
  | Process_local : ('t, 't -> bool, [> `Process_local ]) Eio.Resource.pi

let is_process_local (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Process_local with
  | Some admits -> admits v
  | None -> false

(* A backend that stores no modes has nothing a chmod could set; it registers
   [Modeless]. Silence reads as the host's native filesystem: eio_windows
   keeps no mode and cannot declare it, eio_posix keeps the one given. *)
type (_, _, _) Eio.Resource.pi +=
  | Modeless : ('t, 't -> bool, [> `Modeless ]) Eio.Resource.pi

let is_modeless (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Modeless with
  | Some modeless -> modeless v
  | None -> Sys.win32

(* A wrapper hands out the files of its base wrapped as its opens wrap them;
   it registers [Adopt]. Silence reads as the backend's own directory, which
   hands a file out as it is. *)
type (_, _, _) Eio.Resource.pi +=
  | Adopt :
      ( 't,
        't ->
        sw:Eio.Switch.t ->
        path:string ->
        Eio.File.rw_ty Eio.Resource.t ->
        Eio.File.rw_ty Eio.Resource.t,
        [> `Adopt ] )
      Eio.Resource.pi

let adopt ~sw (Eio.Resource.T (v, ops), path) file =
  match Eio.Resource.get_opt ops Adopt with
  | Some adopt -> adopt v ~sw ~path file
  | None -> file

exception Limit_exceeded of { max_bytes : int64; attempted : int64 }

let () =
  Printexc.register_printer (function
    | Limit_exceeded { max_bytes; attempted } ->
        Fmt.kstr Option.some
          "Eio_fs.Sync.Limit_exceeded: the write reaches %Ld bytes, past \
           max_bytes %Ld"
          attempted max_bytes
    | _ -> None)

let check_max_bytes fn max_bytes =
  if Int64.compare max_bytes 0L < 0 then
    Fmt.invalid_arg "Eio_fs.Sync.%s: max_bytes is %Ld, must not be negative" fn
      max_bytes

module Bounded_sink = struct
  type t = {
    sink : Eio.Flow.sink_ty Eio.Resource.t;
    max_bytes : int64;
    mutable written : int64;
  }

  let sat_add a b =
    if Int64.compare a (Int64.sub Int64.max_int b) > 0 then Int64.max_int
    else Int64.add a b

  let single_write t bufs =
    let requested =
      List.fold_left
        (fun n buf -> sat_add n (Int64.of_int (Cstruct.length buf)))
        0L bufs
    in
    let attempted = sat_add t.written requested in
    if Int64.compare requested (Int64.sub t.max_bytes t.written) > 0 then
      raise (Limit_exceeded { max_bytes = t.max_bytes; attempted });
    let count = Eio.Flow.single_write t.sink bufs in
    t.written <- Int64.add t.written (Int64.of_int count);
    count

  (* Up to 128 KiB, larger than Eio's 4 KiB fallback, without buffering the
     value whole; it grows from 4 KiB as reads fill it, so a small value
     allocates little. *)
  let copy t ~src =
    let buf = ref (Cstruct.create 4096) in
    let rec write_all chunk =
      if not (Cstruct.is_empty chunk) then
        let written = single_write t [ chunk ] in
        write_all (Cstruct.shift chunk written)
    in
    try
      while true do
        let read = Eio.Flow.single_read src !buf in
        write_all (Cstruct.sub !buf 0 read);
        let n = Cstruct.length !buf in
        if read = n && n < 131_072 then buf := Cstruct.create (2 * n)
      done
    with End_of_file -> ()
end

let bounded_sink ~max_bytes sink =
  let ops = Eio.Flow.Pi.sink (module Bounded_sink) in
  Eio.Resource.T (Bounded_sink.{ sink; max_bytes; written = 0L }, ops)

(* A name this process makes must differ from every other process's,
   including one that crashed and left its temporaries behind: not every name
   is created exclusively and retried. So the salt takes its entropy from the
   system, which the prelude refuses because it makes results unrepeatable;
   here it decides a name and nothing else. *)
let tmp_salt = Unsafe.Random.State.bits (Unsafe.Random.State.make_self_init ())
let tmp_sequence = Atomic.make 0

(* What a name made beside a target keeps of the target's name. A long one is
   cut at a character boundary and given the digest of the whole, so two names
   cut alike stay apart and the longest suffix, 36 bytes, still fits NAME_MAX:
   255 bytes, or 255 UTF-16 units, which a byte count bounds. Only the last
   component of a relative path is cut, so the name stays in its directory. *)
let max_stem = 200

let stem path =
  let base =
    match String.rindex_opt path '/' with Some i -> i + 1 | None -> 0
  in
  let name = String.sub path base (String.length path - base) in
  if String.length name <= max_stem then path
  else
    let digest = String.sub (Digest.to_hex (Digest.string name)) 0 16 in
    let room = max_stem - String.length digest - 1 in
    let rec cut i =
      let next = i + Uchar.utf_decode_length (String.get_utf_8_uchar name i) in
      if next > room then i else cut next
    in
    String.sub path 0 base ^ String.sub name 0 (cut 0) ^ "~" ^ digest

let fresh_name name =
  let n = Atomic.fetch_and_add tmp_sequence 1 in
  stem name ^ ".tmp." ^ string_of_int tmp_salt ^ "." ^ string_of_int n

(* The first names are numbered from [0], so one program names the same
   temporaries in every run; past [replayable] taken names, the rest are
   salted, so stale leftovers cannot exhaust them. *)
let replayable = 64
let max_collisions = 128

let fresh_tmp ~collisions (dir, target) =
  if collisions < replayable then
    (dir, stem target ^ ".tmp." ^ string_of_int collisions)
  else (dir, fresh_name target)

let same_file (a : Eio.File.Stat.t) (b : Eio.File.Stat.t) =
  Int64.equal a.dev b.dev && Int64.equal a.ino b.ino

(* The temporary just created must not be the target, and carries exactly
   [perm]: the create's mode is narrowed by the umask, and the published file
   keeps [perm] on a backend that keeps one. *)
let prepare_tmp ~fn ~perm tmp path file =
  let tmp_stat = Eio.File.stat file in
  (match Eio.Path.stat ~follow:false path with
  | target_stat when same_file tmp_stat target_stat ->
      Fmt.invalid_arg
        "Eio_fs.Sync.%s: tmp is %a, the same file as the path to publish" fn
        Eio.Path.pp tmp
  | _ -> ()
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ());
  if
    (not (is_modeless (fst tmp)))
    && not (Int.equal (tmp_stat.perm land 0o7777) perm)
  then Eio.Path.chmod ~follow:false ~perm tmp

let produce ~fn ~barrier ~perm ~requested_tmp ((dir, target) as path) write =
  let rec produce collisions =
    let tmp =
      match requested_tmp with
      | Some tmp -> tmp
      | None -> fresh_tmp ~collisions path
    in
    let tmp_resource, tmp_path = tmp in
    if tmp_resource == dir && String.equal tmp_path target then
      Fmt.invalid_arg "Eio_fs.Sync.%s: tmp is %a, the path to publish" fn
        Eio.Path.pp tmp;
    let owned = ref false in
    let cleanup () =
      if !owned then
        Eio.Cancel.protect @@ fun () ->
        try Eio.Path.unlink tmp with Eio.Io _ -> ()
    in
    try
      let result =
        Eio.Path.with_open_out ~create:(`Exclusive perm) tmp (fun file ->
            owned := true;
            prepare_tmp ~fn ~perm tmp path file;
            let result = write file in
            Barrier.sync_file barrier file;
            result)
      in
      (tmp, owned, cleanup, result)
    with
    | Eio.Io (Eio.Fs.E (Already_exists _), _) as e
      when Option.is_none requested_tmp && not !owned ->
        if collisions = max_collisions - 1 then raise e
        else produce (collisions + 1)
    | e ->
        cleanup ();
        raise e
  in
  produce 0

(* Write a temporary, push its bytes to stable storage, rename it over the
   target, and barrier the target's directory. *)
let publish_file ~fn ~barrier ~perm ~exclusive ~requested_tmp path write =
  let tmp, owned, cleanup, result =
    produce ~fn ~barrier ~perm ~requested_tmp path write
  in
  (try
     Eio.Cancel.protect @@ fun () ->
     (match exclusive with
     | None -> Eio.Path.rename tmp path
     | Some backend -> Rename.exclusive backend tmp path);
     owned := false;
     Barrier.sync_parent barrier path
   with e ->
     cleanup ();
     raise e);
  result

let publish ?(barrier = Barrier.portable) ?(perm = 0o644) ?exclusive
    ?tmp:requested_tmp ~max_bytes path write =
  check_max_bytes "publish" max_bytes;
  publish_file ~fn:"publish" ~barrier ~perm ~exclusive ~requested_tmp path
    (fun file ->
      let sink = (file :> Eio.Flow.sink_ty Eio.Resource.t) in
      write (bounded_sink ~max_bytes sink))

(* Validate before creating a temporary. Subtract before comparing lengths,
   so an adversarial offset cannot wrap the end calculation. Disjoint ordered
   extents also bound total bytes written by the logical size. *)
let publish_sparse ?(barrier = Barrier.portable) ?(perm = 0o644) ?exclusive
    ?tmp:requested_tmp ~max_bytes ~size_bytes path spans =
  check_max_bytes "publish_sparse" max_bytes;
  if Int64.compare size_bytes 0L < 0 then
    Fmt.invalid_arg
      "Eio_fs.Sync.publish_sparse: size_bytes is %Ld, must not be negative"
      size_bytes;
  if Int64.compare size_bytes (Optint.Int63.to_int64 Optint.Int63.max_int) > 0
  then
    Fmt.invalid_arg
      "Eio_fs.Sync.publish_sparse: size_bytes is %Ld, past the largest file \
       offset"
      size_bytes;
  if Int64.compare size_bytes max_bytes > 0 then
    raise (Limit_exceeded { max_bytes; attempted = size_bytes });
  let rec validate previous = function
    | [] -> ()
    | (offset, data) :: rest ->
        let length = Int64.of_int (Cstruct.length data) in
        if Int64.compare offset previous < 0 then
          Fmt.invalid_arg
            "Eio_fs.Sync.publish_sparse: spans has an extent at %Ld, before \
             the end %Ld of the one before it"
            offset previous;
        if
          Int64.compare offset size_bytes > 0
          || Int64.compare length (Int64.sub size_bytes offset) > 0
        then
          Fmt.invalid_arg
            "Eio_fs.Sync.publish_sparse: spans has a %Ld-byte extent at %Ld, \
             past size_bytes %Ld"
            length offset size_bytes;
        validate (Int64.add offset length) rest
  in
  validate 0L spans;
  publish_file ~fn:"publish_sparse" ~barrier ~perm ~exclusive ~requested_tmp
    path (fun file ->
      Eio.File.truncate file (Optint.Int63.of_int64 size_bytes);
      List.iter
        (fun (offset, data) ->
          if not (Cstruct.is_empty data) then
            Eio.File.pwrite_all file
              ~file_offset:(Optint.Int63.of_int64 offset)
              [ data ])
        spans)

(* A string is its own ceiling. *)
let save ?barrier ?perm path s =
  publish ?barrier ?perm
    ~max_bytes:(Int64.of_int (String.length s))
    path
    (fun sink -> Eio.Flow.copy_string s sink)

(* A crash part way through an append leaves the last line without its LF.
   The line is ended first, so the torn bytes are a line of their own and
   what follows is not read as its end. *)
let ends_torn path =
  Eio.Switch.run @@ fun sw ->
  match Eio.Path.open_in ~sw path with
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> false
  | file ->
      let size = Optint.Int63.to_int (Eio.File.size file) in
      size > 0
      &&
      let last = Cstruct.create 1 in
      Eio.File.pread_exact file
        ~file_offset:(Optint.Int63.of_int (size - 1))
        [ last ];
      not (Char.equal (Cstruct.get_char last 0) '\n')

(* The offset just past [file]'s last LF, [0] when it has none, read back from
   the end one block at a time, whatever the file's size. *)
let committed_end file =
  let block = Cstruct.create 4096 in
  let rec scan stop =
    if stop = 0 then 0
    else
      let len = Int.min stop 4096 in
      let start = stop - len in
      Eio.File.pread_exact file
        ~file_offset:(Optint.Int63.of_int start)
        [ Cstruct.sub block 0 len ];
      let rec find i =
        if i < 0 then scan start
        else if Char.equal (Cstruct.get_char block i) '\n' then start + i + 1
        else find (i - 1)
      in
      find (len - 1)
  in
  scan (Optint.Int63.to_int (Eio.File.size file))

let append_lines ?(barrier = Barrier.portable) ?(perm = 0o644)
    ?(tail = `Preserve) path lines =
  List.iter
    (fun l ->
      if String.contains l '\n' then
        Fmt.invalid_arg
          "Eio_fs.Sync.append_lines: lines holds %S, a line with an LF in it" l)
    lines;
  let torn = ends_torn path in
  let discard =
    torn && match tail with `Discard -> true | `Preserve -> false
  in
  let b = Buffer.create 256 in
  if torn && not discard then Buffer.add_char b '\n';
  List.iter
    (fun l ->
      Buffer.add_string b l;
      Buffer.add_char b '\n')
    lines;
  let data = Buffer.contents b in
  (* Every durable append barriers the file and its directory, an empty one
     included: when the barriers of the append that created the file failed,
     nothing else would make its entry durable. *)
  if Barrier.issues barrier || data <> "" || discard then begin
    let create = if data = "" then `Never else `If_missing perm in
    let opened =
      Eio.Switch.run @@ fun sw ->
      match Eio.Path.open_out ~sw ~append:true ~create path with
      | exception Eio.Io (Eio.Fs.E (Not_found _), _) when data = "" -> false
      | file ->
          if discard then
            Eio.File.truncate file (Optint.Int63.of_int (committed_end file));
          if data <> "" then Eio.Flow.copy_string data file;
          Barrier.sync_file barrier file;
          true
    in
    if opened then Barrier.sync_parent barrier path
  end

(* A read-only handle with no barrier and no sync of its own is opened again
   for writing, without creating or truncating: any descriptor on the file
   flushes the same inode. *)
let sync_regular barrier path =
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw ~follow:false path in
  if can_sync file then Barrier.sync_file barrier file
  else
    Barrier.sync_file barrier
      (Eio.Path.open_out ~sw ~follow:false ~create:`Never path)

(* Top down: a directory's barrier commits the entries naming its children, and
   commits nothing while the directory's own entry is not durable, so [dir]'s
   entry is committed before this runs and each subdirectory is walked after
   the barrier that committed it. A file's barrier commits its bytes. *)
let rec sync_tree barrier dir =
  let subdirs =
    List.filter_map
      (fun name ->
        let child = Eio.Path.(dir / name) in
        match Eio.Path.kind ~follow:false child with
        | `Directory -> Some child
        | `Regular_file ->
            sync_regular barrier child;
            None
        | _ -> None)
      (Eio.Path.read_dir dir)
  in
  Barrier.sync_dir barrier dir;
  List.iter (sync_tree barrier) subdirs

(* A generation is [.<name>.gen.<salt>.<n>]: hidden beside its pointer, and
   told from any other name by its suffix. *)
let generation_infix = ".gen."

let fresh_generation name =
  let n = Atomic.fetch_and_add tmp_sequence 1 in
  "." ^ stem name ^ generation_infix ^ string_of_int tmp_salt ^ "."
  ^ string_of_int n

let is_digits s i j =
  j > i
  &&
  let rec all k = k >= j || (s.[k] >= '0' && s.[k] <= '9' && all (k + 1)) in
  all i

(* The name a generation is the generation of, when [s] is one. *)
let generation_of s =
  let n = String.length s in
  let dot = Option.value ~default:(-1) (String.rindex_opt s '.') in
  let infix = String.length generation_infix in
  let rec find i =
    if i < 1 then None
    else if String.sub s i infix = generation_infix then Some i
    else find (i - 1)
  in
  if
    n = 0
    || (not (Char.equal s.[0] '.'))
    || dot < 0
    || not (is_digits s (dot + 1) n)
  then None
  else
    match find (dot - infix - 1) with
    | Some i when i > 1 && is_digits s (i + infix) dot ->
        Some (String.sub s 1 (i - 1))
    | _ -> None

let is_generation s = Option.is_some (generation_of s)

(* A pointer target this publication owns: a sibling generation of the same
   name, never a path elsewhere a caller linked by hand. *)
let owned_generation ~name target =
  match generation_of target with
  | Some owner -> String.equal owner (stem name)
  | None -> false

type Eio.Exn.Backend.t += Generation of string

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Generation why ->
        Fmt.string ppf why;
        true
    | _ -> false)

let fail_generation_changed path before actual =
  Fmt.kstr
    (fun why -> raise (Eio.Fs.err (Eio.Fs.Permission_denied (Generation why))))
    "generation %a was a %a inode %Ld:%Ld, now a %a inode %Ld:%Ld" Eio.Path.pp
    path Eio.File.Stat.pp_kind before.Eio.File.Stat.kind before.dev before.ino
    Eio.File.Stat.pp_kind actual.Eio.File.Stat.kind actual.dev actual.ino

(* Only an unreferenced generation owned by this publication reaches here.
   Grant its directories the permissions removal needs, without changing file
   modes or following a symbolic link. Re-rooting on the opened parent keeps
   every descendant within that capability. *)
let rec reclaim_entry path =
  match Eio.Path.stat ~follow:false path with
  | before when before.kind = `Directory ->
      if (not (is_modeless (fst path))) && before.perm land 0o700 <> 0o700 then
        Eio.Path.chmod ~follow:false ~perm:(before.perm lor 0o700) path;
      Eio.Path.with_subtree path (fun dir ->
          let actual = Eio.Path.stat ~follow:false dir in
          if (not (same_file before actual)) || actual.kind <> `Directory then
            fail_generation_changed path before actual;
          List.iter
            (fun name -> reclaim_entry Eio.Path.(dir / name))
            (Eio.Path.read_dir dir));
      Eio.Path.rmdir path
  | _ -> Eio.Path.unlink path
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let reclaim path =
  match Eio.Path.split path with
  | Some (parent, name) ->
      Eio.Path.with_subtree parent (fun parent ->
          reclaim_entry Eio.Path.(parent / name))
  | None ->
      Fmt.invalid_arg
        "Eio_fs.Sync.publish_generation: generation is %a, which has no parent"
        Eio.Path.pp path

let discard p =
  Eio.Cancel.protect @@ fun () -> try reclaim p with Eio.Io _ -> ()

(* The generation the pointer at [path] names, read without following it:
   [None] when there is no entry. Any other entry is refused, as an existing
   target is. *)
let pointer path =
  match Eio.Path.kind ~follow:false path with
  | `Not_found -> None
  | `Symbolic_link -> Some (Eio.Path.read_link path)
  | _ ->
      let ex =
        Eio.Fs.err
          (Eio.Fs.Already_exists
             (Generation "the entry is not a generation pointer"))
      in
      raise (Eio.Exn.add_context ex "publishing generation %a" Eio.Path.pp path)

(* Stage a fresh sibling generation, commit it whole, rename a pointer to it
   over the published name, commit that, then reclaim the owned generations
   older than the one the old pointer named, which a reader may still be
   reading. *)
let publish_generation ?(barrier = Barrier.portable) ?(perm = 0o755) path
    populate =
  let parent, name =
    match Eio.Path.split path with
    | Some split -> split
    | None ->
        Fmt.invalid_arg
          "Eio_fs.Sync.publish_generation: path is %a, the root of its \
           capability, which has no parent"
          Eio.Path.pp path
  in
  let previous = pointer path in
  let generation = fresh_generation name in
  let staged = Eio.Path.(parent / generation) in
  Eio.Path.mkdir ~perm staged;
  let result =
    try
      let result = populate staged in
      if Barrier.issues barrier then begin
        Barrier.sync_dir barrier parent;
        sync_tree barrier staged
      end;
      result
    with e ->
      discard staged;
      raise e
  in
  let link = Eio.Path.(parent / fresh_name name) in
  let linked = ref false in
  (try
     Eio.Cancel.protect @@ fun () ->
     Eio.Path.symlink ~link_to:generation link;
     linked := true;
     Eio.Path.rename link path
   with e ->
     if !linked then
       Eio.Cancel.protect (fun () ->
           try Eio.Path.unlink link with Eio.Io _ -> ());
     discard staged;
     raise e);
  Barrier.sync_dir barrier parent;
  let kept entry =
    String.equal entry generation
    || Option.equal String.equal (Some entry) previous
  in
  List.iter
    (fun entry ->
      if owned_generation ~name entry && not (kept entry) then
        reclaim Eio.Path.(parent / entry))
    (Eio.Path.read_dir parent);
  result

module Lock = Lock

(* The raw target at [path], read without following it: [None] when there is
   no entry, and a refusal for an entry that is not a symbolic link. *)
let link_target path =
  match Eio.Path.kind ~follow:false path with
  | `Not_found -> Ok None
  | `Symbolic_link -> Ok (Some (Eio.Path.read_link path))
  | #Eio.File.Stat.kind as kind ->
      Error
        (`Invalid
           (Fmt.str "%a is a %a, not a symbolic link" Eio.Path.pp path
              Eio.File.Stat.pp_kind kind))

(* The link is made under a fresh name and removed unless renamed into
   place, so a refusal or a raise leaves only what was there. *)
let stage_link ~barrier ~replace ~target path =
  let parent, name =
    match Eio.Path.split path with
    | Some pair -> pair
    | None ->
        Fmt.invalid_arg
          "Eio_fs.Sync.publish_link: path is %a, the root of its capability, \
           which has no parent"
          Eio.Path.pp path
  in
  let stage = Eio.Path.(parent / fresh_name name) in
  let staged = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !staged then Eio.Cancel.protect (fun () -> Eio.Path.unlink stage))
    (fun () ->
      Eio.Path.symlink ~link_to:target stage;
      staged := true;
      Barrier.sync_parent barrier stage;
      match link_target path with
      | Error e -> Error e
      | Ok actual when not (Option.equal String.equal actual (Some replace)) ->
          Error (`Changed actual)
      | Ok _ ->
          Eio.Cancel.protect (fun () ->
              Eio.Path.rename stage path;
              staged := false;
              Barrier.sync_parent barrier path);
          Ok ())

(* Where nothing stands, symlink(2) is itself the exclusive create: a name
   taken meanwhile refuses it, and is read as any entry found there. *)
let link_in_place ~barrier ~target path =
  let found () =
    match link_target path with
    | Error e -> Error e
    | Ok (Some actual) when String.equal actual target ->
        Barrier.sync_parent barrier path;
        Ok ()
    | Ok actual -> Error (`Changed actual)
  in
  match Eio.Path.symlink ~link_to:target path with
  | () ->
      Barrier.sync_parent barrier path;
      Ok ()
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> found ()

let publish_link ?(barrier = Barrier.portable) ?replace ~target path =
  if target = "" || String.contains target '\000' then
    Error (`Invalid "a symbolic link's target is empty or holds a NUL byte")
  else
    match replace with
    | None -> link_in_place ~barrier ~target path
    | Some replace -> (
        match link_target path with
        | Error e -> Error e
        | Ok actual when not (Option.equal String.equal actual (Some replace))
          ->
            Error (`Changed actual)
        | Ok (Some actual) when String.equal actual target ->
            Barrier.sync_parent barrier path;
            Ok ()
        | Ok _ -> stage_link ~barrier ~replace ~target path)
