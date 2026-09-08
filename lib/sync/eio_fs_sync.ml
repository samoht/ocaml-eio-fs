(* The directory-durability barrier. A backend with its own barrier
   semantics registers [Dir_sync] in its resource handler: the in-memory
   backend a no-op (volatile storage has nothing to sync), a crash-model
   wrapper its journal commit, a fault-injecting wrapper its faultable sync.
   A backend that registers nothing gets the POSIX route: the directory is
   opened read-only and its handle fsynced, the barrier POSIX defines for
   directory entries. Eio types [File.sync] as write-only, but the POSIX
   backends build every descriptor from one read-write implementation, so
   the operation is probed from the handle. This library uses only portable Eio
   interfaces, so unikernel targets link it freely. *)

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

(* The barrier on the directory that holds a path's entry. [Eio.Path.split]
   answers the parent with the directory capability still attached, where
   taking it through the path's string half ([Filename.dirname]) would pair a
   fresh string with the capability and drop the confinement that made the
   original path safe. A path with no parent is the root of the capability
   itself: there is nothing above it a confined path may reach for, so its
   barrier is its own. *)
let sync_parent p =
  match Eio.Path.split p with
  | Some (parent, _) -> sync_dir parent
  | None -> sync_dir p

(* The file-data barrier: the other half of what a commit protocol makes
   durable, the bytes the published entry names. A file is itself the resource,
   so the operation a backend registers takes no name where the directory's
   takes a [~path]. A backend that registers nothing keeps [Eio.File.sync],
   which is the barrier a plain POSIX file already has; the registration is
   there so a wrapper -- a crash model committing this file's data journal --
   can answer for what its base would do, not because the fallback is wrong. It
   is answered whole: syncing the file behind a declared barrier would be a
   barrier that wrapper never recorded. *)
type (_, _, _) Eio.Resource.pi +=
  | File_sync : ('t, 't -> unit, [> `File_sync ]) Eio.Resource.pi

let file_barrier (Eio.Resource.T (v, ops)) =
  Option.map (fun f () -> f v) (Eio.Resource.get_opt ops File_sync)

let sync_file file =
  match file_barrier file with
  | Some barrier -> barrier ()
  | None -> Eio.File.sync file

(* What a backend admits about the openers of its files. A backend whose files
   live in this process's heap admits none outside it: there is no name another
   process can open them by and no descriptor to inherit, so an exclusion taken
   between the fibers of this process is the whole exclusion those files admit,
   not a weaker stand-in for one. Such a backend registers [Process_local]; the
   answer is a function of the resource because a wrapper answers for whatever
   base it was given. A backend that registers nothing is read as admitting
   outside openers, so a forgotten registration costs a refusal rather than a
   critical section two processes enter. *)
type (_, _, _) Eio.Resource.pi +=
  | Process_local : ('t, 't -> bool, [> `Process_local ]) Eio.Resource.pi

let is_process_local (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Process_local with
  | Some admits -> admits v
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
    (* Eio's generic fallback uses 4 KiB. Publication commonly carries archives
       and upload bodies, so amortise the read/write and budget checks without
       buffering the value as a whole. *)
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

(* The durable-publication protocol shared by every store in the tree: write
   a temporary, push its bytes to stable storage, rename it over the target
   (atomic: readers see the old or the new file, never a torn one), and barrier
   the target's directory so a power cut cannot revert the publication. *)
let publish ~config ?tmp:requested_tmp path write =
  if Int64.compare config.max_bytes 0L < 0 then
    invalid_arg "Eio_fs_sync.publish: max_bytes must be non-negative";
  let tmp, owned, cleanup, result = produce ~config ~requested_tmp path write in
  (try
     Eio.Cancel.protect @@ fun () ->
     Eio.Path.rename tmp path;
     owned := false;
     if config.durable then sync_parent path
   with e ->
     cleanup ();
     raise e);
  result

(* The critical section a caller supplies is the other half of what a store
   needs from its deployment: the barrier above says how a write reaches stable
   storage, the section says what keeps two of them out of one another's
   read-modify-write. Neither is the store's to decide. *)
module Lock = Lock
