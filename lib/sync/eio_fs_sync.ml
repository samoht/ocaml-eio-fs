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
      failwith
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

type config = { durable : bool; perm : Eio.File.Unix_perm.t; max_bytes : int64 }

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

let tmp_salt = Random.State.bits (Random.State.make_self_init ())
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
            let sink = (file :> Eio.Flow.sink_ty Eio.Resource.t) in
            let result =
              write (bounded_sink ~max_bytes:config.max_bytes sink)
            in
            if config.durable then sync_file file;
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
let publish ~config ?(mode = `Replace) ?(rename = Rename.capability)
    ?tmp:requested_tmp path write =
  if Int64.compare config.max_bytes 0L < 0 then
    invalid_arg "Eio_fs_sync.publish: max_bytes must be non-negative";
  let tmp, owned, cleanup, result = produce ~config ~requested_tmp path write in
  (try
     Eio.Cancel.protect @@ fun () ->
     (match mode with
     | `Replace -> Eio.Path.rename tmp path
     | `Exclusive -> Rename.exclusive rename tmp path);
     owned := false;
     if config.durable then sync_parent path
   with e ->
     cleanup ();
     raise e);
  result

(* A string is its own ceiling. *)
let save ?(durable = true) ?(perm = 0o644) path s =
  let config = { durable; perm; max_bytes = Int64.of_int (String.length s) } in
  publish ~config path (fun sink -> Eio.Flow.copy_string s sink)

(* A file opened read-only still names the inode fsync(2) flushes. A registered
   barrier answers first; the POSIX backends expose their sync on every handle,
   which is the route [fsync_handle] probes. *)
let sync_readable file =
  match file_barrier file with
  | Some barrier -> barrier ()
  | None -> fsync_handle file

(* Top down: a directory's barrier commits the entries naming its children, and
   commits nothing while the directory's own entry is not durable, so [dir]'s
   entry is committed before this runs and each subdirectory is walked after
   the barrier that committed it. A file's barrier commits its bytes. *)
let rec sync_tree dir =
  let subdirs =
    List.filter_map
      (fun name ->
        let child = Eio.Path.(dir / name) in
        match Eio.Path.kind ~follow:false child with
        | `Directory -> Some child
        | `Regular_file ->
            Eio.Switch.run (fun sw ->
                sync_readable (Eio.Path.open_in ~sw child));
            None
        | _ -> None)
      (Eio.Path.read_dir dir)
  in
  sync_dir dir;
  List.iter sync_tree subdirs

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

let discard p =
  Eio.Cancel.protect @@ fun () ->
  try Eio.Path.rmtree ~missing_ok:true p with Eio.Io _ -> ()

(* Stage a fresh sibling generation, commit it whole, rename a pointer to it
   over the published name, commit that, then reclaim the generation the old
   pointer named. *)
let publish_generation ~durable ?(perm = 0o755) path populate =
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
        sync_dir parent;
        sync_tree staged
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
  if durable then sync_dir parent;
  (match previous with
  | Some target when owned_generation ~name target ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(parent / target)
  | Some _ | None -> ());
  result

module Lock = Lock
