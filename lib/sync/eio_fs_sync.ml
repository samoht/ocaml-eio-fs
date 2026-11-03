open! Hermetic
module Rename = Rename

(* The directory-durability barrier. A backend with its own semantics registers
   [Dir_sync]; one that registers nothing gets the POSIX route, the directory
   opened read-only and its handle fsynced. Eio types [File.sync] as write-only
   but the POSIX backends build every descriptor from one read-write
   implementation, so the operation is probed from the handle. *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync : ('t, 't -> path:string -> unit, [> `Dir_sync ]) Eio.Resource.pi

let dir_barrier (Eio.Resource.T (v, ops)) =
  Option.map (fun f -> f v) (Eio.Resource.get_opt ops Dir_sync)

(* A directory handle that cannot sync is a wiring error, loudly: silence
   here would silently lose durability. *)
let fsync_handle (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Eio.File.Pi.Write with
  | Some (module W) -> W.sync v
  | None ->
      invalid_arg
        "Eio_fs_sync.sync_dir: this backend's directory handle cannot sync; \
         register a Dir_sync barrier on the directory resource"

let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match dir_barrier res with
  | Some barrier -> barrier ~path
  | None -> (
      Eio.Switch.run @@ fun sw ->
      match Eio.Path.open_in ~sw p with
      | handle -> fsync_handle handle
      | exception (Eio.Io _ as ex) ->
          let bt = Printexc.get_raw_backtrace () in
          Eio.Exn.reraise_with_context ex bt
            "syncing directory (backend registers no Dir_sync barrier)")

(* [Eio.Path.split] keeps the directory capability attached, where
   [Filename.dirname] would drop the confinement. A path with no parent is the
   root of the capability, so its barrier is its own. *)
let sync_parent p =
  match Eio.Path.split p with
  | Some (parent, _) -> sync_dir parent
  | None -> sync_dir p

(* Establish each traversed entry's durability, including an existing one:
   it may be visible after an earlier writer's barrier failed. The capability
   root is the caller's durable anchor; never ascend beyond it. *)
let rec mkdirs_with ~sync_parent ~exists_ok ~perm p =
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
      | Some _ -> mkdirs_with ~sync_parent ~exists_ok:true ~perm parent)
    entry;
  (match Eio.Path.mkdir ~perm p with
  | () -> ()
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _)
    when exists_ok && Eio.Path.kind ~follow:false p = `Directory ->
      ());
  sync_parent p

(* The file-data barrier. A file is itself the resource, so its operation takes
   no name. A registered barrier is answered whole: syncing the file behind it
   would be a barrier the wrapper never recorded. *)
type (_, _, _) Eio.Resource.pi +=
  | File_sync : ('t, 't -> unit, [> `File_sync ]) Eio.Resource.pi

let file_barrier (Eio.Resource.T (v, ops)) =
  Option.map (fun f () -> f v) (Eio.Resource.get_opt ops File_sync)

let sync_file file =
  match file_barrier file with
  | Some barrier -> barrier ()
  | None -> Eio.File.sync file

(* Any handle on the file, read-only included: its registered barrier, else
   the write side the POSIX backends give every handle. *)
let sync_any_file (Eio.Resource.T (v, ops) as file) =
  match (file_barrier file, Eio.Resource.get_opt ops Eio.File.Pi.Write) with
  | Some barrier, _ -> barrier ()
  | None, Some (module W) -> W.sync v
  | None, None ->
      invalid_arg
        "Eio_fs_sync: this backend's file handle cannot sync; register a \
         File_sync barrier on the file resource"

let can_sync (Eio.Resource.T (_, ops) as file) =
  Option.is_some (file_barrier file)
  || Option.is_some (Eio.Resource.get_opt ops Eio.File.Pi.Write)

let portable_sync_dir = sync_dir

(* A barrier is the pair a publication issues, packed with its state as a
   rename backend is. *)
module Barrier = struct
  module type S = sig
    type t

    val sync_file : t -> _ Eio.File.ro -> unit
    val sync_dir : t -> _ Eio.Path.t -> unit
  end

  type t = T : (module S with type t = 'a) * 'a -> t

  module Make (B : S) = struct
    let v b = T ((module B), b)
  end

  let sync_file (T ((module B), b)) file = B.sync_file b file
  let sync_dir (T ((module B), b)) path = B.sync_dir b path

  let sync_parent t p =
    match Eio.Path.split p with
    | Some (parent, _) -> sync_dir t parent
    | None -> sync_dir t p

  module Portable = struct
    type t = unit

    let sync_file () file = sync_any_file file
    let sync_dir () path = portable_sync_dir path
  end

  let portable = T ((module Portable), ())
end

let mkdirs ?(exists_ok = false) ?(barrier = Barrier.portable) ~perm p =
  mkdirs_with ~sync_parent:(Barrier.sync_parent barrier) ~exists_ok ~perm p

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
   [Modeless]. Silence reads as a backend that keeps the mode it is given. *)
type (_, _, _) Eio.Resource.pi +=
  | Modeless : ('t, 't -> bool, [> `Modeless ]) Eio.Resource.pi

let is_modeless (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Modeless with
  | Some modeless -> modeless v
  | None -> false

(* [barrier] is [None] when the publication keeps only its atomicity. *)
type config = {
  barrier : Barrier.t option;
  perm : Eio.File.Unix_perm.t;
  max_bytes : int64;
}

let barrier_of ~durable barrier = if durable then Some barrier else None

let config ?(durable = true) ?(barrier = Barrier.portable) ?(perm = 0o644)
    ~max_bytes () =
  if Int64.compare max_bytes 0L < 0 then
    invalid_arg "Eio_fs_sync.config: max_bytes must be non-negative";
  { barrier = barrier_of ~durable barrier; perm; max_bytes }

exception Limit_exceeded of { max_bytes : int64; attempted : int64 }

module Bounded_sink = struct
  type t = {
    sink : Eio.Flow.sink_ty Eio.Resource.t;
    max_bytes : int64;
    mutable written : int64;
  }

  let single_write t bufs =
    let requested =
      List.fold_left
        (fun n buf ->
          let len = Int64.of_int (Cstruct.length buf) in
          if Int64.compare n (Int64.sub Int64.max_int len) > 0 then
            Int64.max_int
          else Int64.add n len)
        0L bufs
    in
    let attempted =
      if Int64.compare t.written (Int64.sub Int64.max_int requested) > 0 then
        Int64.max_int
      else Int64.add t.written requested
    in
    if Int64.compare requested (Int64.sub t.max_bytes t.written) > 0 then
      raise (Limit_exceeded { max_bytes = t.max_bytes; attempted });
    let count = Eio.Flow.single_write t.sink bufs in
    t.written <- Int64.add t.written (Int64.of_int count);
    count

  let copy t ~src =
    (* Larger than Eio's 4 KiB fallback, without buffering the value whole. *)
    let buf = Cstruct.create 131_072 in
    let rec write_all chunk =
      if not (Cstruct.is_empty chunk) then
        let written = single_write t [ chunk ] in
        write_all (Cstruct.shift chunk written)
    in
    try
      while true do
        let read = Eio.Flow.single_read src buf in
        write_all (Cstruct.sub buf 0 read)
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

let fresh_name stem =
  let n = Atomic.fetch_and_add tmp_sequence 1 in
  stem ^ ".tmp." ^ string_of_int tmp_salt ^ "." ^ string_of_int n

let fresh_tmp (dir, target) = (dir, fresh_name target)

let same_file (a : Eio.File.Stat.t) (b : Eio.File.Stat.t) =
  Int64.equal a.dev b.dev && Int64.equal a.ino b.ino

let produce ~config ~requested_tmp ((dir, target) as path) write =
  let rec produce collisions =
    let tmp =
      match requested_tmp with Some tmp -> tmp | None -> fresh_tmp path
    in
    let tmp_resource, tmp_path = tmp in
    if tmp_resource == dir && String.equal tmp_path target then
      invalid_arg "Eio_fs_sync.publish: temporary and target must differ";
    let owned = ref false in
    let cleanup () =
      if !owned then
        Eio.Cancel.protect @@ fun () ->
        try Eio.Path.unlink tmp with Eio.Io _ -> ()
    in
    try
      let result =
        Eio.Path.with_open_out ~create:(`Exclusive config.perm) tmp (fun file ->
            owned := true;
            let tmp_stat = Eio.File.stat file in
            (match Eio.Path.stat ~follow:false path with
            | target_stat when same_file tmp_stat target_stat ->
                invalid_arg
                  "Eio_fs_sync.publish: temporary and target are the same file"
            | _ -> ()
            | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ());
            (* The create's mode is narrowed by the umask; the published file
               carries exactly [config.perm], on a backend that keeps one. *)
            if
              (not (is_modeless tmp_resource))
              && not (Int.equal (tmp_stat.perm land 0o7777) config.perm)
            then Eio.Path.chmod ~follow:false ~perm:config.perm tmp;
            let result = write file in
            Option.iter (fun b -> Barrier.sync_file b file) config.barrier;
            result)
      in
      (tmp, owned, cleanup, result)
    with
    | Eio.Io (Eio.Fs.E (Already_exists _), _) as e
      when Option.is_none requested_tmp && not !owned ->
        if collisions = 127 then raise e else produce (collisions + 1)
    | e ->
        cleanup ();
        raise e
  in
  produce 0

(* Write a temporary, push its bytes to stable storage, rename it over the
   target, and barrier the target's directory. *)
let publish_file ~config ?(mode = `Replace) ?(rename = Rename.capability)
    ?tmp:requested_tmp path write =
  let tmp, owned, cleanup, result = produce ~config ~requested_tmp path write in
  (try
     Eio.Cancel.protect @@ fun () ->
     (match mode with
     | `Replace -> Eio.Path.rename tmp path
     | `Exclusive -> Rename.exclusive rename tmp path);
     owned := false;
     Option.iter (fun b -> Barrier.sync_parent b path) config.barrier
   with e ->
     cleanup ();
     raise e);
  result

let publish ~config ?mode ?rename ?tmp path write =
  publish_file ~config ?mode ?rename ?tmp path (fun file ->
      let sink = (file :> Eio.Flow.sink_ty Eio.Resource.t) in
      write (bounded_sink ~max_bytes:config.max_bytes sink))

(* Validate before creating a temporary. Subtract before comparing lengths,
   so an adversarial offset cannot wrap the end calculation. Disjoint ordered
   extents also bound total bytes written by the logical size. *)
let publish_sparse ~config ?mode ?rename ?tmp ~size_bytes path spans =
  if Int64.compare size_bytes 0L < 0 then
    Fmt.invalid_arg "Eio_fs_sync.publish_sparse: negative logical size %Ld"
      size_bytes;
  if Int64.compare size_bytes (Optint.Int63.to_int64 Optint.Int63.max_int) > 0
  then
    Fmt.invalid_arg
      "Eio_fs_sync.publish_sparse: logical size %Ld exceeds file offsets"
      size_bytes;
  if Int64.compare size_bytes config.max_bytes > 0 then
    raise
      (Limit_exceeded { max_bytes = config.max_bytes; attempted = size_bytes });
  let rec validate previous = function
    | [] -> ()
    | (offset, data) :: rest ->
        let length = Int64.of_int (Cstruct.length data) in
        if Int64.compare offset previous < 0 then
          Fmt.invalid_arg
            "Eio_fs_sync.publish_sparse: extent at %Ld precedes prior end %Ld"
            offset previous;
        if
          Int64.compare offset size_bytes > 0
          || Int64.compare length (Int64.sub size_bytes offset) > 0
        then
          Fmt.invalid_arg
            "Eio_fs_sync.publish_sparse: %Ld-byte extent at %Ld exceeds \
             logical size %Ld"
            length offset size_bytes;
        validate (Int64.add offset length) rest
  in
  validate 0L spans;
  publish_file ~config ?mode ?rename ?tmp path (fun file ->
      Eio.File.truncate file (Optint.Int63.of_int64 size_bytes);
      List.iter
        (fun (offset, data) ->
          if not (Cstruct.is_empty data) then
            Eio.File.pwrite_all file
              ~file_offset:(Optint.Int63.of_int64 offset)
              [ data ])
        spans)

(* A string is its own ceiling. *)
let save ?durable ?barrier ?perm path s =
  let config =
    config ?durable ?barrier ?perm
      ~max_bytes:(Int64.of_int (String.length s))
      ()
  in
  publish ~config path (fun sink -> Eio.Flow.copy_string s sink)

(* A crash part way through an append leaves the file's last line without its
   LF. Whatever follows it would be read as the end of that line, so the line
   is ended first: the torn bytes become a line of their own, which a reader
   refuses as it refuses any malformed one, and nothing written after them is
   lost with them. *)
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

let append_lines ?(durable = true) ?(barrier = Barrier.portable) ?(perm = 0o644)
    path lines =
  List.iter
    (fun l ->
      if String.contains l '\n' then
        invalid_arg "Eio_fs_sync.append_lines: a line holds LF")
    lines;
  let torn = ends_torn path in
  let b = Buffer.create 256 in
  if torn then Buffer.add_char b '\n';
  List.iter
    (fun l ->
      Buffer.add_string b l;
      Buffer.add_char b '\n')
    lines;
  if Buffer.length b > 0 then begin
    Eio.Switch.run (fun sw ->
        let file =
          Eio.Path.open_out ~sw ~append:true ~create:(`If_missing perm) path
        in
        Eio.Flow.copy_string (Buffer.contents b) file;
        if durable then Barrier.sync_file barrier file);
    (* Every durable append barriers the directory, not only the one that
       created the file: when that one's barrier failed, nothing else would
       ever make the entry durable. *)
    if durable then Barrier.sync_parent barrier path
  end

(* A file opened read-only still names the inode fsync(2) flushes. A registered
   barrier answers first; the POSIX backends expose their sync on every handle.
   A read-only handle with neither, as a fault wrapper opens one, is opened
   again for writing, without creating or truncating, and that handle synced:
   any descriptor on the file flushes the same inode. *)
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
  "." ^ name ^ generation_infix ^ string_of_int tmp_salt ^ "." ^ string_of_int n

let is_digits s i j =
  j > i
  &&
  let rec all k = k >= j || (s.[k] >= '0' && s.[k] <= '9' && all (k + 1)) in
  all i

(* The name a generation is the generation of, when [s] is one. *)
let generation_of s =
  let n = String.length s in
  let dot = try String.rindex s '.' with Not_found -> -1 in
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
  | Some owner -> String.equal owner name
  | None -> false

type Eio.Exn.Backend.t += Changed_generation of string

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Changed_generation why ->
        Fmt.string ppf why;
        true
    | _ -> false)

let fail_generation_changed path before actual =
  Fmt.kstr
    (fun why ->
      raise (Eio.Fs.err (Eio.Fs.Permission_denied (Changed_generation why))))
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
      invalid_arg "Eio_fs_sync.publish_generation: generation has no parent"

let discard p =
  Eio.Cancel.protect @@ fun () -> try reclaim p with Eio.Io _ -> ()

(* Stage a fresh sibling generation, commit it whole, rename a pointer to it
   over the published name, commit that, then reclaim the generation the old
   pointer named. *)
let publish_generation ?(durable = true) ?(barrier = Barrier.portable)
    ?(perm = 0o755) path populate =
  let parent, name =
    match Eio.Path.split path with
    | Some split -> split
    | None ->
        invalid_arg "Eio_fs_sync.publish_generation: the root has no parent"
  in
  let previous =
    match Eio.Path.kind ~follow:false path with
    | `Not_found -> None
    | `Symbolic_link -> Some (Eio.Path.read_link path)
    | _ ->
        invalid_arg
          ("Eio_fs_sync.publish_generation: " ^ snd path
         ^ " is not a generation pointer")
  in
  let generation = fresh_generation name in
  let staged = Eio.Path.(parent / generation) in
  Eio.Path.mkdir ~perm staged;
  let result =
    try
      let result = populate staged in
      if durable then begin
        Barrier.sync_dir barrier parent;
        sync_tree barrier staged
      end;
      result
    with e ->
      discard staged;
      raise e
  in
  let link = Eio.Path.(parent / fresh_name name) in
  (try
     Eio.Cancel.protect @@ fun () ->
     Eio.Path.symlink ~link_to:generation link;
     Eio.Path.rename link path
   with e ->
     Eio.Cancel.protect (fun () ->
         try Eio.Path.unlink link with Eio.Io _ -> ());
     discard staged;
     raise e);
  if durable then Barrier.sync_dir barrier parent;
  (match previous with
  | Some target when owned_generation ~name target ->
      reclaim Eio.Path.(parent / target)
  | Some _ | None -> ());
  result

module Lock = Lock

(* The raw target at [path], read without following it: [None] when there is
   no entry, and a refusal for an entry that is not a symbolic link. *)
let link_target path =
  match Eio.Path.kind ~follow:false path with
  | `Not_found -> Ok None
  | `Symbolic_link -> Ok (Some (Eio.Path.read_link path))
  | _ -> Error (`Invalid "the entry to replace is not a symbolic link")

(* The link is made under a fresh name and removed unless renamed into
   place, so a refusal or a raise leaves only what was there. *)
let stage_link ~barrier ~expected ~link_to path =
  let parent, name =
    match Eio.Path.split path with
    | Some pair -> pair
    | None -> invalid_arg "Eio_fs_sync.publish_link: the root has no parent"
  in
  let stage = Eio.Path.(parent / fresh_name name) in
  let staged = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !staged then Eio.Cancel.protect (fun () -> Eio.Path.unlink stage))
    (fun () ->
      Eio.Path.symlink ~link_to stage;
      staged := true;
      Option.iter (fun b -> Barrier.sync_parent b stage) barrier;
      match link_target path with
      | Error e -> Error e
      | Ok actual when not (Option.equal String.equal actual expected) ->
          Error (`Changed actual)
      | Ok _ ->
          Eio.Cancel.protect (fun () ->
              Eio.Path.rename stage path;
              staged := false;
              Option.iter (fun b -> Barrier.sync_parent b path) barrier);
          Ok ())

let publish_link ?(durable = true) ?(barrier = Barrier.portable) ~lock ~expected
    ~link_to path =
  let barrier = barrier_of ~durable barrier in
  Lock.use lock @@ fun () ->
  if link_to = "" || String.contains link_to '\000' then
    Error (`Invalid "a symbolic link's target is empty or holds a NUL byte")
  else
    match link_target path with
    | Error e -> Error e
    | Ok actual when not (Option.equal String.equal actual expected) ->
        Error (`Changed actual)
    | Ok (Some actual) when String.equal actual link_to ->
        Option.iter (fun b -> Barrier.sync_parent b path) barrier;
        Ok ()
    | Ok _ -> stage_link ~barrier ~expected ~link_to path
