(* An adversarial [Eio.Fs.dir] wrapper for fault-injection testing. It composes
   with any base directory capability (the in-memory backend or a real one) and
   perturbs reads, writes, syncs, opens and stats according to a schedule that is
   either seeded (for fuzzing) or scripted (for pinning an exact scenario). *)

open Eio.Std

type code = [ `Enoent | `Eacces | `Enospc | `Eio ]

let code_name = function
  | `Enoent -> "ENOENT"
  | `Eacces -> "EACCES"
  | `Enospc -> "ENOSPC"
  | `Eio -> "EIO"

(* Faults surface as ordinary [Eio.Io] exceptions so consumers cannot tell a
   real backend failure from an injected one. *)
type Eio.Exn.Backend.t += Chaos of string

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Chaos s ->
        Fmt.pf ppf "chaos:%s" s;
        true
    | _ -> false)

let backend code ctx = Fmt.kstr (fun s -> Chaos s) "%s %s" (code_name code) ctx

let raise_open code path =
  match code with
  | `Enoent -> raise (Eio.Fs.err (Eio.Fs.Not_found (backend `Enoent path)))
  | `Eacces ->
      raise (Eio.Fs.err (Eio.Fs.Permission_denied (backend `Eacces path)))
  | (`Enospc | `Eio) as c -> raise (Eio.Exn.create (Eio.Exn.X (backend c path)))

let raise_io code ctx = raise (Eio.Exn.create (Eio.Exn.X (backend code ctx)))

module Fault = struct
  type kind =
    [ `Short_read
    | `Short_write
    | `Torn_write
    | `Write_error
    | `Sync_error
    | `Open_error
    | `Stat_error ]

  type t =
    | Short_read of int
        (** Deliver at most [n] bytes of the requested read ([n >= 1]). *)
    | Short_write of int
        (** Accept at most [n] bytes of the offered write ([n >= 1]). *)
    | Torn_write of int
        (** Commit [n] bytes to the base, then raise: the durability hazard. *)
    | Write_error of code  (** Commit nothing, raise a write error. *)
    | Sync_error of code  (** Fail a flush/sync. *)
    | Open_error of code  (** Fail an open/open_dir. *)
    | Stat_error of code  (** Fail a stat. *)

  let equal a b =
    match (a, b) with
    | Short_read x, Short_read y
    | Short_write x, Short_write y
    | Torn_write x, Torn_write y ->
        x = y
    | Write_error x, Write_error y
    | Sync_error x, Sync_error y
    | Open_error x, Open_error y
    | Stat_error x, Stat_error y ->
        x = y
    | _ -> false

  let pp ppf = function
    | Short_read n -> Fmt.pf ppf "short-read(%d)" n
    | Short_write n -> Fmt.pf ppf "short-write(%d)" n
    | Torn_write n -> Fmt.pf ppf "torn-write(%d)" n
    | Write_error c -> Fmt.pf ppf "write-error(%s)" (code_name c)
    | Sync_error c -> Fmt.pf ppf "sync-error(%s)" (code_name c)
    | Open_error c -> Fmt.pf ppf "open-error(%s)" (code_name c)
    | Stat_error c -> Fmt.pf ppf "stat-error(%s)" (code_name c)
end

module Op = struct
  type kind = [ `Read | `Write | `Sync | `Open | `Stat ]

  let equal_kind (a : kind) (b : kind) = a = b

  type t = {
    kind : kind;
    path : string;
    file_offset : int;  (** byte offset for reads/writes, [0] otherwise *)
    length : int;  (** bytes requested/offered, [0] otherwise *)
  }

  let pp_kind ppf = function
    | `Read -> Fmt.string ppf "read"
    | `Write -> Fmt.string ppf "write"
    | `Sync -> Fmt.string ppf "sync"
    | `Open -> Fmt.string ppf "open"
    | `Stat -> Fmt.string ppf "stat"

  let pp ppf t =
    Fmt.pf ppf "%a %s @%d+%d" pp_kind t.kind t.path t.file_offset t.length

  let read path ~offset ~len =
    { kind = `Read; path; file_offset = offset; length = len }

  let write path ~offset ~len =
    { kind = `Write; path; file_offset = offset; length = len }

  let sync path = { kind = `Sync; path; file_offset = 0; length = 0 }
  let open_ path = { kind = `Open; path; file_offset = 0; length = 0 }
  let stat path = { kind = `Stat; path; file_offset = 0; length = 0 }
end

module Schedule = struct
  type trigger =
    | Nth_op of int
        (** Fire on the [n]th operation the schedule is consulted for. *)
    | Op_on_path of { kind : Op.kind; path : string }
        (** Fire on the first [kind] operation on [path]. *)
    | Write_covering of { path : string; file_offset : int }
        (** Fire on the first write on [path] whose byte range covers
            [file_offset]. *)

  type rule = { trigger : trigger; fault : Fault.t }
  type config = { probability : float; kinds : Fault.kind list }

  type t =
    | Empty
    | Scripted of { mutable rules : rule list; mutable n : int }
    | Seeded of {
        seed : int;
        config : config;
        rng : Random.State.t;
        mutable n : int;
      }

  let empty = Empty
  let scripted rules = Scripted { rules; n = 0 }

  let all_kinds =
    [
      `Short_read;
      `Short_write;
      `Torn_write;
      `Write_error;
      `Sync_error;
      `Open_error;
      `Stat_error;
    ]

  let seeded ?(probability = 0.5) ?(kinds = all_kinds) ~seed () =
    Seeded
      {
        seed;
        config = { probability; kinds };
        rng = Random.State.make [| seed |];
        n = 0;
      }

  let trigger_matches ~n (op : Op.t) = function
    | Nth_op k -> k = n
    | Op_on_path { kind; path } ->
        Op.equal_kind op.kind kind && String.equal op.path path
    | Write_covering { path; file_offset } ->
        Op.equal_kind op.kind `Write
        && String.equal op.path path
        && op.file_offset <= file_offset
        && file_offset < op.file_offset + op.length

  let applies_to (kind : Op.kind) (fk : Fault.kind) =
    match (kind, fk) with
    | `Read, `Short_read -> true
    | `Write, (`Short_write | `Torn_write | `Write_error) -> true
    | `Sync, `Sync_error -> true
    | `Open, `Open_error -> true
    | `Stat, `Stat_error -> true
    | _ -> false

  (* A count in [1, len-1] so the operation is split rather than passed through
     whole (or [1] when the operation is too small to split). *)
  let split_count rng len =
    if len <= 1 then 1 else 1 + Random.State.int rng (len - 1)

  let some_code rng = if Random.State.bool rng then `Enoent else `Eacces

  let build_fault rng (op : Op.t) : Fault.kind -> Fault.t = function
    | `Short_read -> Short_read (split_count rng op.length)
    | `Short_write -> Short_write (split_count rng op.length)
    | `Torn_write -> Torn_write (split_count rng op.length)
    | `Write_error -> Write_error `Enospc
    | `Sync_error -> Sync_error `Eio
    | `Open_error -> Open_error (some_code rng)
    | `Stat_error -> Stat_error (some_code rng)

  let next t (op : Op.t) =
    match t with
    | Empty -> None
    | Scripted s ->
        s.n <- s.n + 1;
        let rec pick acc = function
          | [] -> None
          | r :: rest when trigger_matches ~n:s.n op r.trigger ->
              s.rules <- List.rev_append acc rest;
              Some r.fault
          | r :: rest -> pick (r :: acc) rest
        in
        pick [] s.rules
    | Seeded s -> (
        s.n <- s.n + 1;
        let roll = Random.State.float s.rng 1.0 in
        if roll >= s.config.probability then None
        else
          let applicable = List.filter (applies_to op.kind) s.config.kinds in
          match applicable with
          | [] -> None
          | ks ->
              let k = List.nth ks (Random.State.int s.rng (List.length ks)) in
              Some (build_fault s.rng op k))

  let pp ppf = function
    | Empty -> Fmt.string ppf "empty"
    | Scripted s -> Fmt.pf ppf "scripted(%d rules)" (List.length s.rules)
    | Seeded s -> Fmt.pf ppf "seeded(seed=%d p=%g)" s.seed s.config.probability
end

(* Take at most [n] bytes from the front of [bufs], sharing the underlying
   storage so a read still lands in the caller's buffers. *)
let take_v n bufs =
  let rec go acc remaining = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | b :: bs ->
        let len = Cstruct.length b in
        if len <= remaining then go (b :: acc) (remaining - len) bs
        else List.rev (Cstruct.sub b 0 remaining :: acc)
  in
  if n <= 0 then [] else go [] n bufs

let clamp lo hi n = max lo (min hi n)

let close_resource r =
  let (Eio.Resource.T (v, ops)) = r in
  match Eio.Resource.get_opt ops Eio.Resource.Close with
  | Some close -> close v
  | None -> ()

(* {2 File wrappers} *)

(* Reads are shared between the read-only and read-write wrappers; a read-write
   file coerces to read-only for these. [pos] is the shared streaming cursor. *)
let do_single_read sched path ~pos (base : Eio.File.ro_ty r) buf =
  let len = Cstruct.length buf in
  let op = Op.read path ~offset:!pos ~len in
  let buf =
    match Schedule.next sched op with
    | Some (Fault.Short_read n) -> Cstruct.sub buf 0 (clamp 1 len n)
    | _ -> buf
  in
  let got = Eio.Flow.single_read base buf in
  pos := !pos + got;
  got

let do_pread sched path (base : Eio.File.ro_ty r) ~file_offset bufs =
  let len = Cstruct.lenv bufs in
  let op = Op.read path ~offset:(Optint.Int63.to_int file_offset) ~len in
  let bufs =
    match Schedule.next sched op with
    | Some (Fault.Short_read n) -> take_v (clamp 1 len n) bufs
    | _ -> bufs
  in
  Eio.File.pread base ~file_offset bufs

let do_single_write sched path ~pos (base : Eio.File.rw_ty r) bufs =
  let len = Cstruct.lenv bufs in
  let op = Op.write path ~offset:!pos ~len in
  match Schedule.next sched op with
  | Some (Fault.Short_write n) ->
      let got = Eio.Flow.single_write base (take_v (clamp 1 len n) bufs) in
      pos := !pos + got;
      got
  | Some (Fault.Torn_write n) ->
      let n = clamp 0 len n in
      if n > 0 then Eio.Flow.write base (take_v n bufs);
      pos := !pos + n;
      Fmt.kstr (raise_io `Enospc) "%s torn@%d" path !pos
  | Some (Fault.Write_error c) -> raise_io c path
  | _ ->
      let got = Eio.Flow.single_write base bufs in
      pos := !pos + got;
      got

let do_pwrite sched path (base : Eio.File.rw_ty r) ~file_offset bufs =
  let len = Cstruct.lenv bufs in
  let off = Optint.Int63.to_int file_offset in
  let op = Op.write path ~offset:off ~len in
  match Schedule.next sched op with
  | Some (Fault.Short_write n) ->
      Eio.File.pwrite_single base ~file_offset (take_v (clamp 1 len n) bufs)
  | Some (Fault.Torn_write n) ->
      let n = clamp 0 len n in
      if n > 0 then Eio.File.pwrite_all base ~file_offset (take_v n bufs);
      Fmt.kstr (raise_io `Enospc) "%s torn@%d" path (off + n)
  | Some (Fault.Write_error c) -> raise_io c path
  | _ -> Eio.File.pwrite_single base ~file_offset bufs

let do_sync sched path (base : Eio.File.rw_ty r) =
  match Schedule.next sched (Op.sync path) with
  | Some (Fault.Sync_error c) -> raise_io c path
  | _ -> Eio.File.sync base

(* One record backs both wrappers. [ro] is always present (reads work on either
   kind); [rw] is present exactly when the file was opened read-write, and is
   the same underlying resource as [ro]. *)
type file = {
  sched : Schedule.t;
  path : string;
  pos : int ref;
  ro : Eio.File.ro_ty r;
  rw : Eio.File.rw_ty r option;
}

(* Read-write is invariant: the read-write handler is only ever attached to a
   file constructed with [rw = Some _]. *)
let writable t = match t.rw with Some rw -> rw | None -> assert false

module Ro_impl = struct
  type t = file

  let read_methods = []
  let single_read t buf = do_single_read t.sched t.path ~pos:t.pos t.ro buf
  let pread t ~file_offset bufs = do_pread t.sched t.path t.ro ~file_offset bufs
  let stat t = Eio.File.stat t.ro
  let seek t off cmd = Eio.File.seek t.ro off cmd
  let close t = close_resource t.ro
end

let ro_handler = Eio.File.Pi.ro (module Ro_impl)

let wrap_ro sched ~path base : Eio.File.ro_ty r =
  Eio.Resource.T ({ sched; path; pos = ref 0; ro = base; rw = None }, ro_handler)

module Rw_impl = struct
  type t = file

  let read_methods = []
  let single_read t buf = do_single_read t.sched t.path ~pos:t.pos t.ro buf
  let pread t ~file_offset bufs = do_pread t.sched t.path t.ro ~file_offset bufs
  let stat t = Eio.File.stat t.ro
  let seek t off cmd = Eio.File.seek t.ro off cmd
  let close t = close_resource t.ro

  let single_write t bufs =
    do_single_write t.sched t.path ~pos:t.pos (writable t) bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let pwrite t ~file_offset bufs =
    do_pwrite t.sched t.path (writable t) ~file_offset bufs

  let sync t = do_sync t.sched t.path (writable t)
  let truncate t len = Eio.File.truncate (writable t) len
end

let rw_handler = Eio.File.Pi.rw (module Rw_impl)

let wrap_rw sched ~path base : Eio.File.rw_ty r =
  Eio.Resource.T
    ( {
        sched;
        path;
        pos = ref 0;
        ro = (base :> Eio.File.ro_ty r);
        rw = Some base;
      },
      rw_handler )

(* {2 Directory wrapper} *)

(* The base directory provider and value, unpacked once and boxed so the wrapper
   methods can call through without re-resolving the interface. *)
type base_dir =
  | Base_dir : (module Eio.Fs.Pi.DIR with type t = 'a) * 'a -> base_dir

type dir = { base : Eio.Fs.dir_ty r; boxed : base_dir; sched : Schedule.t }

(* Expose the wrapped base so a rename target that is itself a chaos directory
   can be unwrapped before it reaches the base backend. *)
type (_, _, _) Eio.Resource.pi +=
  | Chaos_base : ('t, 't -> Eio.Fs.dir_ty r, [> `Chaos ]) Eio.Resource.pi

let wrap_dir_hook :
    (base:Eio.Fs.dir_ty r -> sched:Schedule.t -> [ `Close | Eio.Fs.dir_ty ] r)
    ref =
  ref (fun ~base:_ ~sched:_ -> assert false)

module Dir_impl = struct
  type t = dir

  let open_in t ~sw path =
    (match Schedule.next t.sched (Op.open_ path) with
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    wrap_ro t.sched ~path (B.open_in bv ~sw path)

  let open_out t ~sw ~append ~create path =
    (match Schedule.next t.sched (Op.open_ path) with
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    wrap_rw t.sched ~path (B.open_out bv ~sw ~append ~create path)

  let open_subtree t ~sw path =
    (match Schedule.next t.sched (Op.open_ path) with
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let sub = B.open_subtree bv ~sw path in
    !wrap_dir_hook ~base:(sub :> Eio.Fs.dir_ty r) ~sched:t.sched

  let stat t ~follow path =
    (match Schedule.next t.sched (Op.stat path) with
    | Some (Fault.Stat_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    B.stat bv ~follow path

  let mkdir t ~perm path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.mkdir bv ~perm path

  let read_dir t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.read_dir bv path

  let with_dir_entries t path fn =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.with_dir_entries bv path fn

  let unlink t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.unlink bv path

  let rmdir t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.rmdir bv path

  let rename t old_path new_dir new_path =
    let (Eio.Resource.T (nv, nops)) = new_dir in
    let target =
      match Eio.Resource.get_opt nops Chaos_base with
      | Some unwrap -> unwrap nv
      | None -> (new_dir :> Eio.Fs.dir_ty r)
    in
    let (Base_dir ((module B), bv)) = t.boxed in
    B.rename bv old_path target new_path

  let read_link t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.read_link bv path

  let symlink ~link_to t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.symlink ~link_to bv path

  let chmod t ~follow ~perm path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.chmod bv ~follow ~perm path

  let chown ~follow ?uid ?gid t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.chown ~follow ?uid ?gid bv path

  let native t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.native bv path

  let pp ppf t =
    let (Base_dir ((module B), bv)) = t.boxed in
    Fmt.pf ppf "chaos(%a)" B.pp bv
end

let dir_handler : (dir, [ `Dir | `Close | `Chaos ]) Eio.Resource.handler =
  Eio.Resource.handler
    [
      H (Eio.Fs.Pi.Dir, (module Dir_impl));
      H (Eio.Resource.Close, fun t -> close_resource t.base);
      H (Chaos_base, fun t -> t.base);
    ]

let wrap_dir ~base ~sched : [ `Close | Eio.Fs.dir_ty ] r =
  let (Eio.Resource.T (bv, bops)) = base in
  let boxed = Base_dir (Eio.Resource.get bops Eio.Fs.Pi.Dir, bv) in
  (Eio.Resource.T ({ base; boxed; sched }, dir_handler)
    :> [ `Close | Eio.Fs.dir_ty ] r)

let () = wrap_dir_hook := wrap_dir

let wrap sched (base, path) =
  (wrap_dir ~base:(base :> Eio.Fs.dir_ty r) ~sched, path)

(* {2 Crash / durability model} *)

(* A companion to the schedule-driven wrapper that models power-loss durability.
   Every write is tracked as "un-synced" until a successful [sync] on that file
   marks all its prior writes durable. A crash produces a post-crash view of the
   backing store where un-synced writes may be dropped and/or reordered relative
   to each other, while synced writes are always durable and never move past
   their barrier. Unlike the perturbing wrapper, this one delegates writes and
   reads to the base unchanged (so the live view stays consistent while the
   workload runs) and only rewrites the base at crash time to the reconstructed
   image. *)
module Crash = struct
  type boxed = base_dir
  (* Reuse the boxed base directory from the perturbing wrapper. *)

  let raise_power_lost () = raise_io `Eio "power lost"

  let copy_cs c =
    let n = Cstruct.length c in
    let b = Cstruct.create n in
    Cstruct.blit c 0 b 0 n;
    b

  (* Blit [src] into [dst] at [offset], growing (and zero-filling any hole) when
     the write runs past the current end. *)
  let grow_blit dst ~offset src =
    let src_len = Cstruct.length src in
    let need = offset + src_len in
    let cur = Cstruct.length dst in
    if need <= cur then (
      Cstruct.blit src 0 dst offset src_len;
      dst)
    else begin
      let b = Cstruct.create need in
      Cstruct.blit dst 0 b 0 cur;
      Cstruct.blit src 0 b offset src_len;
      b
    end

  let resize buf n =
    let cur = Cstruct.length buf in
    if n = cur then buf
    else if n < cur then Cstruct.sub buf 0 n
    else begin
      let b = Cstruct.create n in
      Cstruct.blit buf 0 b 0 cur;
      b
    end

  (* An independent copy of the first [n] bytes offered by [bufs]. The caller's
     buffers may be reused after the write returns, so the log must not alias
     them. *)
  let capture bufs n =
    let b = Cstruct.create n in
    let rec go dst = function
      | [] -> ()
      | x :: xs ->
          let take = min (Cstruct.length x) (n - dst) in
          if take > 0 then Cstruct.blit x 0 b dst take;
          let dst = dst + take in
          if dst < n then go dst xs
    in
    go 0 bufs;
    b

  (* Seeded Fisher-Yates: same seed and same list yield the same permutation. *)
  let shuffle rng lst =
    let a = Array.of_list lst in
    for i = Array.length a - 1 downto 1 do
      let j = Random.State.int rng (i + 1) in
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
    done;
    Array.to_list a

  let read_all (ro : Eio.File.ro_ty r) =
    let size = Optint.Int63.to_int (Eio.File.stat ro).size in
    if size <= 0 then Cstruct.empty
    else begin
      let buf = Cstruct.create size in
      let rec loop off =
        if off >= size then ()
        else
          let got =
            Eio.File.pread ro ~file_offset:(Optint.Int63.of_int off)
              [ Cstruct.sub buf off (size - off) ]
          in
          loop (off + got)
      in
      loop 0;
      buf
    end

  (* The base content before a truncating open, used to seed a file's durable
     baseline. A missing path, or one that is not a regular file, seeds an empty
     baseline. *)
  let read_existing (Base_dir ((module B), bv)) path =
    match B.stat bv ~follow:true path with
    | exception Eio.Io _ -> Cstruct.empty
    | { kind = `Regular_file; _ } -> (
        try Switch.run (fun sw -> read_all (B.open_in bv ~sw path))
        with Eio.Io _ -> Cstruct.empty)
    | _ -> Cstruct.empty

  (* A single durability-relevant change on a file. [Truncate] models an
     unsynced resize; [Write] carries an independent copy of the committed
     bytes so a crash can replay the survivors onto the durable baseline. *)
  type change = Write of { offset : int; data : Cstruct.t } | Truncate of int

  (* Per-file durability state. [durable] is the content as of the last
     successful [sync] (empty for a never-synced file); the pending changes for
     the file live in the controller's global log. [owner] and [rel] locate the
     file in the base so a crash can rewrite it. *)
  type fstate = {
    owner : boxed;
    rel : string;
    mutable perm : int;
    mutable durable : Cstruct.t;
  }

  type entry = { efs : fstate; change : change; seq : int }
  type unsynced = { path : string; offset : int; length : int; seq : int }
  (* An un-synced change, as seen by a scripted survival predicate. *)

  (** How the post-crash image reconstructs from the un-synced writes. *)
  type policy =
    | Drop_unsynced
        (** Every un-synced write is lost; only synced data survives. The strict
            "power cut, nothing pending reached the platter" model. *)
    | Reorder_unsynced of int
        (** Seeded: each un-synced write survives on an even coin and the
            survivors are applied in a seeded-shuffled order, so a later write
            may land while an earlier one is lost. The [int] is the seed; the
            same seed and workload reproduce the same image. *)
    | Survive of (unsynced -> bool)
        (** Scripted: an un-synced write survives iff the predicate holds;
            survivors keep program order. Pins an exact scenario -- for example
            keep the generation pointer but drop the data block it references.
        *)

  (** When a crash fires while the workload runs. *)
  type trigger =
    | Manual  (** Only an explicit {!crash} (or {!post_crash}) fires it. *)
    | After_write of { path : string; file_offset : int }
        (** Crash right after the write covering [file_offset] on [path]
            commits, before any following sync. *)
    | Nth_op of int
        (** Crash on the [n]th durability op (writes and syncs, 1-based). *)

  type base_dir = Eio.Fs.dir_ty Eio.Path.t
  (** The backing store handed back after a crash. *)

  type t = {
    policy : policy;
    trigger : trigger;
    mutable base : base_dir option;
    files : (string, fstate) Hashtbl.t;
    mutable log : entry list; (* newest first *)
    mutable seq : int;
    mutable ops : int;
    mutable crashed : bool;
  }

  let clog_change ctl fs change =
    ctl.seq <- ctl.seq + 1;
    ctl.log <- { efs = fs; change; seq = ctl.seq } :: ctl.log

  let clog_write ctl fs ~offset data =
    clog_change ctl fs (Write { offset; data })

  let to_unsynced e =
    match e.change with
    | Write { offset; data } ->
        { path = e.efs.rel; offset; length = Cstruct.length data; seq = e.seq }
    | Truncate n -> { path = e.efs.rel; offset = n; length = 0; seq = e.seq }

  let unsynced ctl = List.rev_map to_unsynced ctl.log

  let apply buf = function
    | Write { offset; data } -> grow_blit buf ~offset data
    | Truncate n -> resize buf n

  (* Rewrite the base file to [content]: truncate and re-lay the bytes. *)
  let rewrite fs content =
    let (Base_dir ((module B), bv)) = fs.owner in
    Switch.run @@ fun sw ->
    let f =
      B.open_out bv ~sw ~append:false ~create:(`Or_truncate fs.perm) fs.rel
    in
    if Cstruct.length content > 0 then
      Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [ content ]

  let do_crash ctl =
    if not ctl.crashed then begin
      ctl.crashed <- true;
      let ordered =
        List.rev ctl.log
        (* program order *)
      in
      let survivors =
        match ctl.policy with
        | Drop_unsynced -> []
        | Survive pred -> List.filter (fun e -> pred (to_unsynced e)) ordered
        | Reorder_unsynced seed ->
            let rng = Random.State.make [| seed |] in
            let kept = List.filter (fun _ -> Random.State.bool rng) ordered in
            shuffle rng kept
      in
      Hashtbl.iter
        (fun _ fs ->
          let changes =
            List.filter_map
              (fun e -> if e.efs == fs then Some e.change else None)
              survivors
          in
          let content = List.fold_left apply (copy_cs fs.durable) changes in
          rewrite fs content)
        ctl.files
    end

  (* Advance the durability op counter and fire the configured auto-trigger. *)
  let bump ctl fs ~kind ~offset ~len =
    ctl.ops <- ctl.ops + 1;
    let fire =
      match ctl.trigger with
      | Manual -> false
      | Nth_op n -> ctl.ops >= n
      | After_write { path; file_offset } ->
          (match kind with `Write -> true | `Sync -> false)
          && String.equal path fs.rel && offset <= file_offset
          && file_offset < offset + len
    in
    if fire && not ctl.crashed then do_crash ctl

  (* One record backs both file wrappers. [cfs] is present exactly for a
     read-write file (the tracked durability state); reads work on either. *)
  type cfile = {
    ctl : t;
    cfs : fstate option;
    cro : Eio.File.ro_ty r;
    crw : Eio.File.rw_ty r option;
    cpos : int ref;
  }

  let guard cf = if cf.ctl.crashed then raise_power_lost ()
  let fstate_of cf = match cf.cfs with Some fs -> fs | None -> assert false
  let writable cf = match cf.crw with Some rw -> rw | None -> assert false

  module Cro_impl = struct
    type t = cfile

    let read_methods = []

    let single_read cf buf =
      guard cf;
      let got = Eio.Flow.single_read cf.cro buf in
      cf.cpos := !(cf.cpos) + got;
      got

    let pread cf ~file_offset bufs =
      guard cf;
      Eio.File.pread cf.cro ~file_offset bufs

    let stat cf = Eio.File.stat cf.cro
    let seek cf off cmd = Eio.File.seek cf.cro off cmd
    let close cf = close_resource cf.cro
  end

  let cro_handler = Eio.File.Pi.ro (module Cro_impl)

  let wrap_cfile_ro ctl base : Eio.File.ro_ty r =
    Eio.Resource.T
      ({ ctl; cfs = None; cro = base; crw = None; cpos = ref 0 }, cro_handler)

  module Crw_impl = struct
    type t = cfile

    let read_methods = []

    let single_read cf buf =
      guard cf;
      let got = Eio.Flow.single_read cf.cro buf in
      cf.cpos := !(cf.cpos) + got;
      got

    let pread cf ~file_offset bufs =
      guard cf;
      Eio.File.pread cf.cro ~file_offset bufs

    let stat cf = Eio.File.stat cf.cro
    let seek cf off cmd = Eio.File.seek cf.cro off cmd
    let close cf = close_resource cf.cro

    let single_write cf bufs =
      guard cf;
      let fs = fstate_of cf in
      let got = Eio.Flow.single_write (writable cf) bufs in
      let off = !(cf.cpos) in
      clog_write cf.ctl fs ~offset:off (capture bufs got);
      cf.cpos := off + got;
      bump cf.ctl fs ~kind:`Write ~offset:off ~len:got;
      got

    let copy cf ~src = Eio.Flow.Pi.simple_copy ~single_write cf ~src

    let pwrite cf ~file_offset bufs =
      guard cf;
      let fs = fstate_of cf in
      let off = Optint.Int63.to_int file_offset in
      let got = Eio.File.pwrite_single (writable cf) ~file_offset bufs in
      clog_write cf.ctl fs ~offset:off (capture bufs got);
      bump cf.ctl fs ~kind:`Write ~offset:off ~len:got;
      got

    (* The sync barrier: the live content becomes durable and the file's pending
       changes leave the log, so a later crash can neither drop nor reorder
       them. *)
    let sync cf =
      guard cf;
      let fs = fstate_of cf in
      Eio.File.sync (writable cf);
      fs.durable <- read_all cf.cro;
      cf.ctl.log <- List.filter (fun e -> not (e.efs == fs)) cf.ctl.log;
      bump cf.ctl fs ~kind:`Sync ~offset:0 ~len:0

    let truncate cf len =
      guard cf;
      let fs = fstate_of cf in
      Eio.File.truncate (writable cf) len;
      clog_change cf.ctl fs (Truncate (Optint.Int63.to_int len));
      bump cf.ctl fs ~kind:`Write ~offset:0 ~len:0
  end

  let crw_handler = Eio.File.Pi.rw (module Crw_impl)

  let wrap_cfile_rw ctl fs base : Eio.File.rw_ty r =
    Eio.Resource.T
      ( {
          ctl;
          cfs = Some fs;
          cro = (base :> Eio.File.ro_ty r);
          crw = Some base;
          cpos = ref 0;
        },
        crw_handler )

  type cdir = { dctl : t; dbase : Eio.Fs.dir_ty r; dboxed : boxed }

  (* Unwrap a crash directory to its base so a rename target that is itself a
     crash directory reaches the base backend. *)
  type (_, _, _) Eio.Resource.pi +=
    | Crash_base : ('t, 't -> Eio.Fs.dir_ty r, [> `Crash_base ]) Eio.Resource.pi

  let crash_dir_hook :
      (t -> base:Eio.Fs.dir_ty r -> [ `Close | Eio.Fs.dir_ty ] r) ref =
    ref (fun _ ~base:_ -> assert false)

  let is_trunc = function
    | `Or_truncate _ | `Exclusive _ -> true
    | `Never | `If_missing _ -> false

  let create_perm = function
    | `Never -> 0o644
    | `If_missing p | `Or_truncate p | `Exclusive p -> p

  module Cdir_impl = struct
    type t = cdir

    let open_in d ~sw path =
      if d.dctl.crashed then raise_power_lost ();
      let (Base_dir ((module B), bv)) = d.dboxed in
      wrap_cfile_ro d.dctl (B.open_in bv ~sw path)

    let open_out d ~sw ~append ~create path =
      if d.dctl.crashed then raise_power_lost ();
      let (Base_dir ((module B), bv)) = d.dboxed in
      let key = match B.native bv path with Some n -> n | None -> path in
      let prev = read_existing d.dboxed path in
      let base = B.open_out bv ~sw ~append ~create path in
      let perm = create_perm create in
      let fs =
        match Hashtbl.find_opt d.dctl.files key with
        | Some fs ->
            fs.perm <- perm;
            fs
        | None ->
            let fs = { owner = d.dboxed; rel = path; perm; durable = prev } in
            Hashtbl.replace d.dctl.files key fs;
            fs
      in
      (* An unsynced truncation of pre-existing durable data is itself a change
         that a crash may lose. *)
      if is_trunc create && Cstruct.length prev > 0 then
        clog_change d.dctl fs (Truncate 0);
      wrap_cfile_rw d.dctl fs base

    let open_subtree d ~sw path =
      if d.dctl.crashed then raise_power_lost ();
      let (Base_dir ((module B), bv)) = d.dboxed in
      let sub = B.open_subtree bv ~sw path in
      !crash_dir_hook d.dctl ~base:(sub :> Eio.Fs.dir_ty r)

    let stat d ~follow path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.stat bv ~follow path

    let mkdir d ~perm path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.mkdir bv ~perm path

    let read_dir d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.read_dir bv path

    let with_dir_entries d path fn =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.with_dir_entries bv path fn

    let unlink d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.unlink bv path

    let rmdir d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.rmdir bv path

    let rename d old_path new_dir new_path =
      let (Eio.Resource.T (nv, nops)) = new_dir in
      let target =
        match Eio.Resource.get_opt nops Crash_base with
        | Some unwrap -> unwrap nv
        | None -> (new_dir :> Eio.Fs.dir_ty r)
      in
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.rename bv old_path target new_path

    let read_link d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.read_link bv path

    let symlink ~link_to d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.symlink ~link_to bv path

    let chmod d ~follow ~perm path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.chmod bv ~follow ~perm path

    let chown ~follow ?uid ?gid d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.chown ~follow ?uid ?gid bv path

    let native d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.native bv path

    let pp ppf d =
      let (Base_dir ((module B), bv)) = d.dboxed in
      Fmt.pf ppf "crash(%a)" B.pp bv
  end

  let cdir_handler :
      (cdir, [ `Dir | `Close | `Crash_base ]) Eio.Resource.handler =
    Eio.Resource.handler
      [
        H (Eio.Fs.Pi.Dir, (module Cdir_impl));
        H (Eio.Resource.Close, fun d -> close_resource d.dbase);
        H (Crash_base, fun d -> d.dbase);
      ]

  let wrap_cdir ctl ~base : [ `Close | Eio.Fs.dir_ty ] r =
    let (Eio.Resource.T (bv, bops)) = base in
    let boxed = Base_dir (Eio.Resource.get bops Eio.Fs.Pi.Dir, bv) in
    (Eio.Resource.T ({ dctl = ctl; dbase = base; dboxed = boxed }, cdir_handler)
      :> [ `Close | Eio.Fs.dir_ty ] r)

  let () = crash_dir_hook := fun ctl ~base -> wrap_cdir ctl ~base

  let v ?(policy = Drop_unsynced) ?(trigger = Manual) () =
    {
      policy;
      trigger;
      base = None;
      files = Hashtbl.create 8;
      log = [];
      seq = 0;
      ops = 0;
      crashed = false;
    }

  let wrap ctl (base, path) =
    let base = (base :> Eio.Fs.dir_ty r) in
    ctl.base <- Some (base, path);
    (wrap_cdir ctl ~base, path)

  let crash ctl = do_crash ctl
  let is_crashed ctl = ctl.crashed

  let post_crash ctl =
    do_crash ctl;
    match ctl.base with
    | Some b -> b
    | None -> invalid_arg "Chaos.Crash.post_crash: controller was never wrapped"

  let pp_unsynced ppf u =
    Fmt.pf ppf "%s @%d+%d #%d" u.path u.offset u.length u.seq
end
