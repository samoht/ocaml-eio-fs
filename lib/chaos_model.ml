(* An adversarial [Eio.Fs.dir] wrapper for fault-injection testing. It composes
   with any base directory capability (the in-memory backend or a real one) and
   perturbs reads, writes, syncs, opens, stats and directory reads according to a
   schedule that is
   either seeded (for fuzzing) or scripted (for pinning an exact scenario). *)

open! Hermetic
open Eio.Std

type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]

let code_name = function
  | `Enoent -> "ENOENT"
  | `Eacces -> "EACCES"
  | `Eperm -> "EPERM"
  | `Erofs -> "EROFS"
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
  | (`Eacces | `Eperm) as c ->
      raise (Eio.Fs.err (Eio.Fs.Permission_denied (backend c path)))
  | (`Erofs | `Enospc | `Eio) as c ->
      raise (Eio.Exn.create (Eio.Exn.X (backend c path)))

let raise_io code ctx = raise (Eio.Exn.create (Eio.Exn.X (backend code ctx)))

module Fault = struct
  type kind =
    [ `Short_read
    | `Read_error
    | `Short_write
    | `Torn_write
    | `Write_error
    | `Sync_error
    | `Open_error
    | `Stat_error
    | `Change_error
    | `Swap ]

  type t =
    | Short_read of int
        (** Deliver at most [n] bytes of the requested read ([n >= 1]). *)
    | Read_error of code  (** Deliver nothing, raise a read error. *)
    | Short_write of int
        (** Accept at most [n] bytes of the offered write ([n >= 1]). *)
    | Torn_write of int
        (** Commit [n] bytes to the base, then raise: the durability hazard. *)
    | Write_error of code  (** Commit nothing, raise a write error. *)
    | Sync_error of code  (** Fail a flush/sync. *)
    | Open_error of code  (** Fail an open/open_dir. *)
    | Stat_error of code  (** Fail a stat. *)
    | Change_error of code  (** Fail a directory change. *)
    | Swap_symlink of string
        (** Perform the operation, then replace its path with a symlink to the
            given target: the check-to-use window a TOCTOU exploits. Scripted on
            a [`Stat] op, it lets the stat see the original file and the
            following open see a symlink, so a builder that reads the same
            pathname twice reads what was swapped in. Only meaningful in a
            scripted schedule. *)

  let equal a b =
    match (a, b) with
    | Short_read x, Short_read y
    | Short_write x, Short_write y
    | Torn_write x, Torn_write y ->
        x = y
    | Read_error x, Read_error y
    | Write_error x, Write_error y
    | Sync_error x, Sync_error y
    | Open_error x, Open_error y
    | Stat_error x, Stat_error y
    | Change_error x, Change_error y ->
        x = y
    | Swap_symlink x, Swap_symlink y -> String.equal x y
    | _ -> false

  let code_rank = function
    | `Enoent -> 0
    | `Eacces -> 1
    | `Eperm -> 2
    | `Erofs -> 3
    | `Enospc -> 4
    | `Eio -> 5

  let rank = function
    | Short_read _ -> 0
    | Read_error _ -> 1
    | Short_write _ -> 2
    | Torn_write _ -> 3
    | Write_error _ -> 4
    | Sync_error _ -> 5
    | Open_error _ -> 6
    | Stat_error _ -> 7
    | Change_error _ -> 8
    | Swap_symlink _ -> 9

  let compare a b =
    match (a, b) with
    | Short_read x, Short_read y
    | Short_write x, Short_write y
    | Torn_write x, Torn_write y ->
        Int.compare x y
    | Read_error x, Read_error y
    | Write_error x, Write_error y
    | Sync_error x, Sync_error y
    | Open_error x, Open_error y
    | Stat_error x, Stat_error y
    | Change_error x, Change_error y ->
        Int.compare (code_rank x) (code_rank y)
    | Swap_symlink x, Swap_symlink y -> String.compare x y
    | _ -> Int.compare (rank a) (rank b)

  let pp ppf = function
    | Short_read n -> Fmt.pf ppf "short-read(%d)" n
    | Read_error c -> Fmt.pf ppf "read-error(%s)" (code_name c)
    | Short_write n -> Fmt.pf ppf "short-write(%d)" n
    | Torn_write n -> Fmt.pf ppf "torn-write(%d)" n
    | Write_error c -> Fmt.pf ppf "write-error(%s)" (code_name c)
    | Sync_error c -> Fmt.pf ppf "sync-error(%s)" (code_name c)
    | Open_error c -> Fmt.pf ppf "open-error(%s)" (code_name c)
    | Stat_error c -> Fmt.pf ppf "stat-error(%s)" (code_name c)
    | Change_error c -> Fmt.pf ppf "change-error(%s)" (code_name c)
    | Swap_symlink t -> Fmt.pf ppf "swap-symlink(%s)" t
end

module Op = struct
  type kind =
    [ `Read
    | `Read_dir
    | `Write
    | `Truncate
    | `Sync
    | `Open_read
    | `Open_write
    | `Stat
    | `Change ]

  let equal_kind (a : kind) (b : kind) = a = b

  type t = {
    kind : kind;
    path : string;
    file_offset : int;  (** byte offset for reads/writes, [0] otherwise *)
    length : int;  (** bytes requested/offered, [0] otherwise *)
    space : bool;
    dest : string option;  (** a rename's destination *)
  }

  let pp_kind ppf = function
    | `Read -> Fmt.string ppf "read"
    | `Read_dir -> Fmt.string ppf "read-dir"
    | `Write -> Fmt.string ppf "write"
    | `Truncate -> Fmt.string ppf "truncate"
    | `Sync -> Fmt.string ppf "sync"
    | `Open_read -> Fmt.string ppf "open"
    | `Open_write -> Fmt.string ppf "open-write"
    | `Stat -> Fmt.string ppf "stat"
    | `Change -> Fmt.string ppf "change"

  let pp ppf t =
    Fmt.pf ppf "%a %s @%d+%d" pp_kind t.kind t.path t.file_offset t.length

  let v ?(space = false) ?(offset = 0) ?(len = 0) ?dest kind path =
    { kind; path; file_offset = offset; length = len; space; dest }

  let read path ~offset ~len = v ~offset ~len `Read path
  let write path ~offset ~len = v ~space:true ~offset ~len `Write path
  let truncate path ~len = v ~offset:len `Truncate path
  let read_dir path = v `Read_dir path
  let sync path = v `Sync path
  let open_read path = v `Open_read path
  let open_write ~space path = v ~space `Open_write path
  let stat path = v `Stat path
  let change ?dest ~space path = v ?dest ~space `Change path
end

module Schedule = struct
  type trigger =
    | Nth_op of int
        (** Fire on the [n]th operation the schedule is consulted for. *)
    | Op_on_path of { kind : Op.kind; path : string }
        (** Fire on the first [kind] operation on [path]. *)
    | Nth_op_on_path of { n : int; kind : Op.kind; path : string }
        (** Fire on the [n]th [kind] operation on [path]. *)
    | Write_covering of { path : string; file_offset : int }
        (** Fire on the first write on [path] whose byte range covers
            [file_offset]. *)

  type rule = { trigger : trigger; fault : Fault.t }
  type config = { probability : float; kinds : Fault.kind list }

  type t =
    | Empty
    | Scripted of {
        mutable rules : rule list;
        mutable n : int;
        seen : (Op.kind * string, int) Hashtbl.t;
            (* How many operations of each kind each path has had. *)
      }
    | Seeded of {
        seed : int;
        config : config;
        rng : Random.State.t;
        mutable n : int;
      }
    | Observed of { f : Op.t -> unit; inner : t }
    | Hook of (Op.t -> Fault.t option)

  let empty = Empty
  let scripted rules = Scripted { rules; n = 0; seen = Hashtbl.create 8 }
  let observing f t = Observed { f; inner = t }
  let hook f = Hook f

  let all_kinds =
    [
      `Short_read;
      `Read_error;
      `Short_write;
      `Torn_write;
      `Write_error;
      `Sync_error;
      `Open_error;
      `Stat_error;
      `Change_error;
    ]

  let seeded ?(probability = 0.5) ?(kinds = all_kinds) ~seed () =
    Seeded
      {
        seed;
        config = { probability; kinds };
        rng = Random.State.make [| seed |];
        n = 0;
      }

  let trigger_matches ~n ~on_path (op : Op.t) = function
    | Nth_op k -> k = n
    | Op_on_path { kind; path } ->
        Op.equal_kind op.kind kind && String.equal op.path path
    | Nth_op_on_path { n = k; kind; path } ->
        Op.equal_kind op.kind kind && String.equal op.path path && k = on_path
    | Write_covering { path; file_offset } ->
        Op.equal_kind op.kind `Write
        && String.equal op.path path
        && op.file_offset <= file_offset
        && file_offset < op.file_offset + op.length

  let applies_to (kind : Op.kind) (fk : Fault.kind) =
    match (kind, fk) with
    | `Read, (`Short_read | `Read_error) -> true
    | `Read_dir, `Read_error -> true
    | `Write, (`Short_write | `Torn_write | `Write_error) -> true
    | `Sync, `Sync_error -> true
    | (`Open_read | `Open_write), `Open_error -> true
    | `Stat, `Stat_error -> true
    | `Change, `Change_error -> true
    | _ -> false

  (* A count in [1, len-1] so the operation is split rather than passed through
     whole (or [1] when the operation is too small to split). *)
  let split_count rng len =
    if len <= 1 then 1 else 1 + Random.State.full_int rng (len - 1)

  let some_code rng = if Random.State.bool rng then `Enoent else `Eacces

  let build_fault rng (op : Op.t) : Fault.kind -> Fault.t = function
    | `Short_read -> Short_read (split_count rng op.length)
    | `Read_error -> Read_error `Eio
    | `Short_write -> Short_write (split_count rng op.length)
    | `Torn_write -> Torn_write (split_count rng op.length)
    | `Write_error -> Write_error `Enospc
    | `Sync_error -> Sync_error `Eio
    | `Open_error -> Open_error (some_code rng)
    | `Stat_error -> Stat_error (some_code rng)
    | `Change_error ->
        Change_error
          (match Random.State.int rng 3 with
          | 0 -> `Eacces
          | 1 -> `Eperm
          | _ -> `Erofs)
    | `Swap ->
        (* A swap needs a concrete symlink target, so it has no random form; it is
           driven by a scripted rule, never a seed. [all_kinds] omits it, and
           [applies_to] excludes it, so a seeded schedule never reaches here. *)
        invalid_arg "Chaos: `Swap is scripted-only, not a seeded fault"

  let rec next t (op : Op.t) =
    match t with
    | Empty -> None
    | Observed { f; inner } ->
        f op;
        next inner op
    | Hook f -> f op
    | Scripted s ->
        s.n <- s.n + 1;
        let key = (op.kind, op.path) in
        let on_path =
          1 + Option.value ~default:0 (Hashtbl.find_opt s.seen key)
        in
        Hashtbl.replace s.seen key on_path;
        let rec pick acc = function
          | [] -> None
          | r :: rest when trigger_matches ~n:s.n ~on_path op r.trigger ->
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

  let kind_name : Fault.kind -> string = function
    | `Short_read -> "`Short_read"
    | `Read_error -> "`Read_error"
    | `Short_write -> "`Short_write"
    | `Torn_write -> "`Torn_write"
    | `Write_error -> "`Write_error"
    | `Sync_error -> "`Sync_error"
    | `Open_error -> "`Open_error"
    | `Stat_error -> "`Stat_error"
    | `Change_error -> "`Change_error"
    | `Swap -> "`Swap"

  (* OCaml syntax: a negative literal needs parentheses as a labelled
     argument. *)
  (* The shortest literal that reads back as [f], and [infinity],
     [neg_infinity] or [nan] by name. *)
  let float_literal f =
    match Float.classify_float f with
    | FP_infinite -> if f > 0. then "infinity" else "neg_infinity"
    | FP_nan -> "nan"
    | _ ->
        let s =
          List.find
            (fun s -> Float.equal (float_of_string s) f)
            [ Fmt.str "%.15g" f; Fmt.str "%.16g" f; Fmt.str "%.17g" f ]
        in
        if String.exists (fun c -> c = '.' || c = 'e') s then s else s ^ "."

  let pp_float ppf f =
    let s = float_literal f in
    if s.[0] = '-' then Fmt.pf ppf "(%s)" s else Fmt.string ppf s

  let pp_int ppf i = if i < 0 then Fmt.pf ppf "(%d)" i else Fmt.int ppf i

  (* A seeded schedule prints as the expression that rebuilds it. *)
  let rec pp ppf = function
    | Empty -> Fmt.string ppf "empty"
    | Scripted s -> Fmt.pf ppf "scripted(%d rules)" (List.length s.rules)
    | Seeded s ->
        Fmt.pf ppf
          "Eio_fs.Chaos.Private.Schedule.seeded ~probability:%a ~kinds:%s \
           ~seed:%a ()"
          pp_float s.config.probability
          (match s.config.kinds with
          | [] -> "[]"
          | kinds -> "[ " ^ String.concat "; " (List.map kind_name kinds) ^ " ]")
          pp_int s.seed
    | Observed { inner; _ } -> Fmt.pf ppf "observed(%a)" pp inner
    | Hook _ -> Fmt.string ppf "hook"
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

(* [n] within [[lo, hi]], and [hi] when the range is empty: a fault that
   shortens an operation never asks for more bytes than it offers, so a short
   read of an empty buffer reads it whole. *)
let clamp lo hi n = min hi (max lo n)

let close_resource r =
  let (Eio.Resource.T (v, ops)) = r in
  match Eio.Resource.get_opt ops Eio.Resource.Close with
  | Some close -> close v
  | None -> ()

(* {2 What a wrapped file carries over}

   A wrapped file is its own value: the wrapper's state, bound to the
   wrapper's read, write, sync and close and nothing of the base's. A handler
   rebuilt from its bindings so reaches no operation the wrapper does not
   implement, and so no operation that steps around the schedule or the
   journal. It registers [Eio_fs_sync.File_sync], bound to its own sync, so a
   barrier issued through [Eio_fs_sync.sync_file] reaches the wrapper.

   The base file stays reachable through [Eio_fs_internal.Base_file] alone,
   which no library outside this package can name: eio-fs.unix takes the
   host descriptor a record lock is taken on from it. *)
let rw_boxed (type s) ?(extra = [])
    (module X : Eio.File.Pi.WRITE with type t = s) (state : s)
    ~(base : Eio.File.rw_ty r) : Eio.File.rw_ty r =
  Eio.Resource.T
    ( state,
      Eio.Resource.handler
        (Eio.Resource.bindings (Eio.File.Pi.rw (module X))
        @ [
            Eio.Resource.H (Eio_fs_sync.File_sync, X.sync);
            Eio.Resource.H
              (Eio_fs_internal.Base_file, fun _ -> (base :> Eio.File.ro_ty r));
          ]
        @ extra) )

(* A sync the wrapper failed on purpose, told to the file beneath it, so a
   power-loss model there loses what that sync would have written. *)
type (_, _, _) Eio.Resource.pi +=
  | Sync_failed : ('t, 't -> unit, [> `Sync_failed ]) Eio.Resource.pi

let sync_failed (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Sync_failed with Some f -> f v | None -> ()

(* {2 File wrappers} *)

(* Reads are shared between the read-only and read-write wrappers; a read-write
   file coerces to read-only for these. [pos] is the shared streaming cursor. *)
let do_single_read sched path ~pos (base : Eio.File.ro_ty r) buf =
  let len = Cstruct.length buf in
  let op = Op.read path ~offset:!pos ~len in
  let buf =
    match Schedule.next sched op with
    | Some (Fault.Read_error c) -> raise_io c path
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
    | Some (Fault.Read_error c) -> raise_io c path
    | Some (Fault.Short_read n) -> take_v (clamp 1 len n) bufs
    | _ -> bufs
  in
  Eio.File.pread base ~file_offset bufs

(* Where a streaming write lands: at the cursor, or for a file opened to
   append at its end, wherever the cursor was. *)
let write_offset ~append ~pos base =
  if append then Optint.Int63.to_int (Eio.File.stat base).Eio.File.Stat.size
  else !pos

(* A seek moves the cursor the wrapper reports offsets from. *)
let do_seek ~pos base off cmd =
  let at = Eio.File.seek base off cmd in
  pos := Optint.Int63.to_int at;
  at

let do_single_write sched path ~append ~pos (base : Eio.File.rw_ty r) bufs =
  let len = Cstruct.lenv bufs in
  let off = write_offset ~append ~pos base in
  let op = Op.write path ~offset:off ~len in
  match Schedule.next sched op with
  | Some (Fault.Short_write n) ->
      let got = Eio.Flow.single_write base (take_v (clamp 1 len n) bufs) in
      pos := off + got;
      got
  | Some (Fault.Torn_write n) ->
      let n = clamp 0 len n in
      if n > 0 then Eio.Flow.write base (take_v n bufs);
      pos := off + n;
      Fmt.kstr (raise_io `Enospc) "%s torn@%d" path !pos
  | Some (Fault.Write_error c) -> raise_io c path
  | _ ->
      let got = Eio.Flow.single_write base bufs in
      pos := off + got;
      got

let do_pwrite sched path ~append (base : Eio.File.rw_ty r) ~file_offset bufs =
  let len = Cstruct.lenv bufs in
  let off =
    if append then write_offset ~append ~pos:(ref 0) base
    else Optint.Int63.to_int file_offset
  in
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
  | Some (Fault.Sync_error c) ->
      sync_failed base;
      raise_io c path
  | _ -> Eio.File.sync base

let do_stat sched path (base : Eio.File.ro_ty r) =
  match Schedule.next sched (Op.stat path) with
  | Some (Fault.Stat_error c) -> raise_io c path
  | _ -> Eio.File.stat base

(* One record backs both wrappers. [ro] is always present (reads work on either
   kind); [rw] is present exactly when the file was opened read-write, and is
   the same underlying resource as [ro]. *)
type file = {
  sched : Schedule.t;
  path : string;
  append : bool;
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
  let stat t = do_stat t.sched t.path t.ro
  let seek t off cmd = do_seek ~pos:t.pos t.ro off cmd
  let close t = close_resource t.ro
end

(* fsync(2) flushes the file whichever descriptor names it, so a file opened
   read-only answers [Eio_fs_sync.File_sync] too, scheduled as the read-write
   sync is: through the base's own barrier, or the sync its handle exposes. *)
let sync_base (Eio.Resource.T (v, ops) as base) =
  match Eio_fs_sync.file_barrier base with
  | Some barrier -> barrier ()
  | None -> (
      match Eio.Resource.get_opt ops Eio.File.Pi.Write with
      | Some (module W) -> W.sync v
      | None -> invalid_arg "Chaos: the base file opened read-only cannot sync")

let sync_ro t =
  match Schedule.next t.sched (Op.sync t.path) with
  | Some (Fault.Sync_error c) ->
      sync_failed t.ro;
      raise_io c t.path
  | _ -> sync_base t.ro

(* A failure a wrapper above injected is told on to the file beneath, so
   stacked wrappers reach the power-loss model at the bottom. *)
let ro_handler =
  Eio.Resource.handler
    (Eio.Resource.bindings (Eio.File.Pi.ro (module Ro_impl))
    @ [
        Eio.Resource.H (Eio_fs_sync.File_sync, sync_ro);
        Eio.Resource.H (Sync_failed, fun t -> sync_failed t.ro);
      ])

let wrap_ro sched ~path base : Eio.File.ro_ty r =
  Eio.Resource.T
    ( { sched; path; append = false; pos = ref 0; ro = base; rw = None },
      ro_handler )

module Rw_impl = struct
  type t = file

  let read_methods = []
  let single_read t buf = do_single_read t.sched t.path ~pos:t.pos t.ro buf
  let pread t ~file_offset bufs = do_pread t.sched t.path t.ro ~file_offset bufs
  let stat t = do_stat t.sched t.path t.ro
  let seek t off cmd = do_seek ~pos:t.pos t.ro off cmd
  let close t = close_resource t.ro

  let single_write t bufs =
    do_single_write t.sched t.path ~append:t.append ~pos:t.pos (writable t) bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let pwrite t ~file_offset bufs =
    do_pwrite t.sched t.path ~append:t.append (writable t) ~file_offset bufs

  let sync t = do_sync t.sched t.path (writable t)

  let truncate t len =
    match
      Schedule.next t.sched (Op.truncate t.path ~len:(Optint.Int63.to_int len))
    with
    | Some (Fault.Write_error c) -> raise_io c t.path
    | _ -> Eio.File.truncate (writable t) len
end

let wrap_rw sched ~path ~append base : Eio.File.rw_ty r =
  let state =
    {
      sched;
      path;
      append;
      pos = ref 0;
      ro = (base :> Eio.File.ro_ty r);
      rw = Some base;
    }
  in
  rw_boxed
    ~extra:[ Eio.Resource.H (Sync_failed, fun t -> sync_failed t.ro) ]
    (module Rw_impl)
    state ~base

(* {2 Directory wrapper} *)

(* The base directory provider and value, unpacked once and boxed so the wrapper
   methods can call through without re-resolving the interface. *)
type base_dir =
  | Base_dir : (module Eio.Fs.Pi.DIR with type t = 'a) * 'a -> base_dir

type dir = { base : Eio.Fs.dir_ty r; boxed : base_dir; sched : Schedule.t }

(* A rename target that is itself a chaos directory is unwrapped before it
   reaches the base backend. *)
let base_dir = Eio_fs_internal.base_dir

let wrap_dir_hook :
    (base:Eio.Fs.dir_ty r -> sched:Schedule.t -> [ `Close | Eio.Fs.dir_ty ] r)
    ref =
  ref (fun ~base:_ ~sched:_ -> assert false)

(* Free the name at [path], whatever holds it. A directory is emptied first and
   removed with [rmdir]; anything else is unlinked. Both wrappers that free a
   name go through here. *)
let rec remove_at (Base_dir ((module B), bv) as owner) path =
  match B.stat bv ~follow:false path with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
  | { kind = `Directory; _ } ->
      let rec clear () =
        match B.read_dir bv path with
        | [] -> ()
        | names ->
            List.iter
              (fun name ->
                let child = Eio_fs_path.append path name in
                remove_at owner child)
              names;
            clear ()
      in
      clear ();
      if not (String.equal path "") then B.rmdir bv path
  | _ -> B.unlink bv path

(* A swap fires AFTER the scheduled operation: the name is unlinked and made a
   symlink to the target, so everything resolving the path afterwards reaches
   the target -- the check-to-use window a TOCTOU exploits. *)
let swap_after boxed fault path =
  match fault with
  | Some (Fault.Swap_symlink target) ->
      let (Base_dir ((module B), bv)) = boxed in
      remove_at boxed path;
      B.symlink ~link_to:target bv path
  | Some _ | None -> ()

module Dir_impl = struct
  type t = dir

  let open_in t ~sw ~follow path =
    let fault = Schedule.next t.sched (Op.open_read path) in
    (match fault with
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let file = B.open_in bv ~sw ~follow path in
    swap_after t.boxed fault path;
    wrap_ro t.sched ~path file

  let open_out t ~sw ~follow ~append ~create path =
    let (Base_dir ((module B), bv)) = t.boxed in
    (* Only an open that makes its file needs room. *)
    let space =
      create <> `Never
      &&
      match B.stat bv ~follow:true path with
      | _ -> false
      | exception Eio.Io _ -> true
    in
    let fault = Schedule.next t.sched (Op.open_write ~space path) in
    (match fault with
    | Some (Fault.Open_error `Enospc) when not space -> ()
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let file = B.open_out bv ~sw ~follow ~append ~create path in
    swap_after t.boxed fault path;
    wrap_rw t.sched ~path ~append file

  let open_subtree t ~sw path =
    let fault = Schedule.next t.sched (Op.open_read path) in
    (match fault with
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let sub = B.open_subtree bv ~sw path in
    swap_after t.boxed fault path;
    !wrap_dir_hook ~base:(sub :> Eio.Fs.dir_ty r) ~sched:t.sched

  let stat t ~follow path =
    let fault = Schedule.next t.sched (Op.stat path) in
    (match fault with
    | Some (Fault.Stat_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let result = B.stat bv ~follow path in
    swap_after t.boxed fault path;
    result

  (* A change to a directory's entries, named by the entry it makes or
     removes; only one that makes an entry needs room. *)
  let change ?(space = false) ?dest t path =
    match Schedule.next t.sched (Op.change ?dest ~space path) with
    | Some (Fault.Change_error `Enospc) when not space -> ()
    | Some (Fault.Change_error c) -> raise_open c path
    | _ -> ()

  let mkdir t ~perm path =
    change ~space:true t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.mkdir bv ~perm path

  let read_dir t path =
    (match Schedule.next t.sched (Op.read_dir path) with
    | Some (Fault.Read_error c) -> raise_io c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    B.read_dir bv path

  let with_dir_entries t path fn =
    (match Schedule.next t.sched (Op.read_dir path) with
    | Some (Fault.Read_error c) -> raise_io c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    B.with_dir_entries bv path fn

  let unlink t path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.unlink bv path

  let rmdir t path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.rmdir bv path

  let rename t old_path new_dir new_path =
    change ~space:true ~dest:new_path t old_path;
    let target =
      match base_dir new_dir with
      | Some base -> base
      | None -> (new_dir :> Eio.Fs.dir_ty r)
    in
    let (Base_dir ((module B), bv)) = t.boxed in
    B.rename bv old_path target new_path

  let rename_exclusive t backend ~source ~target:(new_dir, new_path) =
    change ~space:true ~dest:new_path t source;
    let target =
      match base_dir new_dir with Some base -> base | None -> new_dir
    in
    Eio_fs_sync.Rename.exclusive backend (t.base, source) (target, new_path)

  let read_link t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.read_link bv path

  let symlink ~link_to t path =
    change ~space:true t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.symlink ~link_to bv path

  let chmod t ~follow ~perm path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.chmod bv ~follow ~perm path

  let chown ~follow ?uid ?gid t path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.chown ~follow ?uid ?gid bv path

  let native t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.native bv path

  let sync_dir t path =
    (match Schedule.next t.sched (Op.sync path) with
    | Some (Fault.Sync_error c) -> raise_io c path
    | _ -> ());
    Eio_fs_sync.sync_dir (t.base, path)

  let pp ppf t =
    let (Base_dir ((module B), bv)) = t.boxed in
    Fmt.pf ppf "chaos(%a)" B.pp bv

  (* Path syntax is a property of the backend, not of an instance, so the
     wrapper cannot take it from the base it boxes: it states the POSIX
     syntax every base here uses. *)
  include Eio_utils.Posix_path
end

(* The wrapper is its own value, bound to its own interfaces: a handler
   rebuilt from its bindings reaches nothing of the base's, so no directory
   operation steps around the schedule. The base stays reachable through
   [Eio_fs_internal.Base_dir] and [Eio_fs_internal.Admin] alone, which no
   library outside this package can name. A backend's own interfaces, such as the native directory a child
   process is started in, are not carried over. *)
let dir_handler =
  Eio.Resource.handler
    [
      Eio.Resource.H (Eio.Fs.Pi.Dir, (module Dir_impl));
      Eio.Resource.H (Eio.Resource.Close, fun t -> close_resource t.base);
      Eio.Resource.H (Eio_fs_internal.Base_dir, fun t -> t.base);
      (* The base's root rights, under the same schedule. *)
      Eio.Resource.H
        ( Eio_fs_internal.Admin,
          fun t ->
            let base =
              Option.value ~default:t.base (Eio_fs_internal.admin t.base)
            in
            (!wrap_dir_hook ~base ~sched:t.sched :> Eio.Fs.dir_ty r) );
      Eio.Resource.H (Eio_fs_sync.Rename.Exclusive, Dir_impl.rename_exclusive);
      Eio.Resource.H
        (Eio_fs_sync.Process_local, fun t -> Eio_fs_sync.is_process_local t.base);
      Eio.Resource.H
        (Eio_fs_sync.Modeless, fun t -> Eio_fs_sync.is_modeless t.base);
      Eio.Resource.H
        (Eio_fs_sync.Dir_sync, fun t ~path -> Dir_impl.sync_dir t path);
    ]

let wrap_dir ~base ~sched : [ `Close | Eio.Fs.dir_ty ] r =
  let (Eio.Resource.T (bv, bops)) = base in
  let module B = (val Eio.Resource.get bops Eio.Fs.Pi.Dir) in
  let state = { base; boxed = Base_dir ((module B), bv); sched } in
  (Eio.Resource.T (state, dir_handler) :> [ `Close | Eio.Fs.dir_ty ] r)

let () = wrap_dir_hook := wrap_dir

let wrap sched (base, path) =
  (wrap_dir ~base:(base :> Eio.Fs.dir_ty r) ~sched, path)

(* {2 Crash / durability model} *)

(* A companion to the schedule-driven wrapper that models power-loss durability:
   a write is un-synced until a [sync] on its file, and a crash may drop or
   reorder un-synced writes. Writes and reads are delegated unchanged while the
   workload runs; the base is rewritten only at crash time. *)
module Crash = struct
  type boxed = base_dir
  (* Reuse the boxed base directory from the perturbing wrapper. *)

  let raise_power_lost () = raise_io `Eio "power lost"

  let copy_cs c =
    let n = Cstruct.length c in
    let b = Cstruct.create n in
    Cstruct.blit c 0 b 0 n;
    b

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

  (* The model observes the disk as a disk, not as a user: a file whose mode
     denies reading (a write-only spool) is still bytes on the platter. Grant
     owner read for the duration of the model's own reads and restore the
     exact mode after. *)
  let with_readable (Base_dir ((module B), bv)) path fn =
    let restore =
      match B.stat bv ~follow:false path with
      | exception Eio.Io _ -> None
      | { kind = `Regular_file; perm; _ } when perm land 0o400 = 0 ->
          B.chmod bv ~follow:true ~perm:(perm lor 0o400) path;
          Some perm
      | _ -> None
    in
    Fun.protect
      ~finally:(fun () ->
        match restore with
        | Some perm -> B.chmod bv ~follow:true ~perm path
        | None -> ())
      fn

  (* The base content before a truncating open, used to seed a file's durable
     baseline. A missing path, or one that is not a regular file, seeds an empty
     baseline. *)
  let read_existing (Base_dir ((module B), bv) as owner) path =
    match B.stat bv ~follow:true path with
    | exception Eio.Io _ -> Cstruct.empty
    | { kind = `Regular_file; _ } -> (
        try
          with_readable owner path (fun () ->
              Switch.run (fun sw ->
                  read_all (B.open_in bv ~sw ~follow:true path)))
        with Eio.Io _ -> Cstruct.empty)
    | _ -> Cstruct.empty

  (* [Write] carries a copy of the committed bytes so a crash can replay the
     survivors onto the durable baseline. *)
  type change = Write of { offset : int; data : Cstruct.t } | Resize of int

  (* A file's bytes while changes replay over them: the first [len] bytes of
     [buf], which grows by doubling, so a run of appends costs linear time. *)
  type image = { mutable buf : Cstruct.t; mutable len : int }

  (* The bytes replays have copied into images, which a test bounds. *)
  let copied = Atomic.make 0

  let image_of c =
    ignore (Atomic.fetch_and_add copied (Cstruct.length c) : int);
    { buf = copy_cs c; len = Cstruct.length c }

  let bytes_of img = Cstruct.sub img.buf 0 img.len

  (* Room for [n] bytes, those past [len] zero. *)
  let extend img n =
    if n > Cstruct.length img.buf then begin
      let b = Cstruct.create (max n (2 * Cstruct.length img.buf)) in
      ignore (Atomic.fetch_and_add copied img.len : int);
      Cstruct.blit img.buf 0 b 0 img.len;
      img.buf <- b
    end
    else if n > img.len then
      Cstruct.memset (Cstruct.sub img.buf img.len (n - img.len)) 0

  let apply img = function
    | Write { offset; data } ->
        let n = offset + Cstruct.length data in
        extend img n;
        Cstruct.blit data 0 img.buf offset (Cstruct.length data);
        img.len <- max img.len n
    | Resize n ->
        extend img n;
        img.len <- n

  (* [durable] with [changes] replayed over it, in order. *)
  let replay_changes durable changes =
    let img = image_of durable in
    List.iter (apply img) changes;
    bytes_of img

  (* [durable] is the content as of the last [sync]; the pending changes live in
     the controller's log. [owner] and [rel] locate the file under the wrapped
     root so a crash can rewrite it after intermediate subtrees close; both move
     on rename. *)
  type fstate = {
    id : int;
    mutable owner : boxed;
    mutable rel : string;
    mutable perm : int;
    durable : image;
    mutable pending : entry list;
        (* this file's pending changes, newest first *)
    mutable gen : int;
        (* Bumped when the pending changes go, durable or lost: an entry of an
           older generation in the controller's log is stale. *)
    mutable cache : Cstruct.t option;
        (* The page cache's bytes as of the last sync, where a failed sync
           left them apart from the durable ones: it still holds what that
           sync lost. *)
    mutable mode : int;
        (* The file's mode as last seen, to tell when a write clears setuid
           or setgid. *)
    mutable fino : int64; (* its inode, once a write cleared its mode *)
    mutable evicted : Cstruct.t option;
        (* The durable bytes had the pages a failed sync lost been evicted
           before a later write to them, where they differ. *)
    mutable partial : (int * int * Cstruct.t) list;
        (* The blocks failed syncs lost, with the seq each failed at and as
           they stood then: a writeback that failed only partly may have
           written any of them, the one of any failure. *)
    mutable held : (int * int) list;
        (* The byte ranges durable writes covered, disjoint and in order:
           the disk holds the pages they touch, and no other. *)
    mutable freed : (int * Cstruct.t) list;
        (* The pages durable shrinks freed, by index, newest first, as the
           disk held them. *)
  }

  and entry = {
    efs : fstate;
    change : change;
    seq : int;
    egen : int;
    cached : bool;
        (* A truncation a failed sync kept pending, which the page cache
           already holds: it carries the length only and dirties no block. *)
  }

  let next_id = Atomic.make 0

  let fresh_file ?held ~owner ~rel ~perm ~durable () =
    {
      id = Atomic.fetch_and_add next_id 1;
      owner;
      rel;
      perm;
      durable = image_of durable;
      pending = [];
      gen = 0;
      cache = None;
      mode = 0;
      fino = -1L;
      evicted = None;
      partial = [];
      held =
        (let n = Cstruct.length durable in
         match held with
         | Some l ->
             List.filter_map
               (fun (x, y) -> if x >= n then None else Some (x, min y n))
               l
         | None -> if n > 0 then [ (0, n) ] else []);
      freed = [];
    }

  (* Maps keyed by a path, where the keys at or under one path are a range. *)
  module Paths = Map.Make (String)
  module Ids = Map.Make (Int)

  (* The bindings of [m] at or under the path [k]. *)
  let under k m =
    let pre = if String.equal k "" || String.equal k "/" then k else k ^ "/" in
    let exact =
      match Paths.find_opt k m with
      | Some v when not (String.equal pre k) -> [ (k, v) ]
      | Some _ | None -> []
    in
    let rec go acc seq =
      match seq () with
      | Seq.Cons ((k', v), rest) when String.starts_with ~prefix:pre k' ->
          go ((k', v) :: acc) rest
      | Seq.Cons _ | Seq.Nil -> acc
    in
    go exact (Paths.to_seq_from pre m)

  let is_pending e = e.egen = e.efs.gen

  (* What kind of un-synced change a journal entry records, as seen by a
     scripted survival predicate. *)
  type op_kind =
    | Data
    | Truncate
    | Create
    | Mkdir
    | Unlink
    | Rmdir
    | Symlink of { target : string }
    | Rename of { dest : string }
    | Chmod of { perm : int }
    | Chown of { uid : int64; gid : int64 }

  let equal_op_kind (a : op_kind) b =
    match (a, b) with
    | Data, Data | Truncate, Truncate | Create, Create | Mkdir, Mkdir -> true
    | Unlink, Unlink | Rmdir, Rmdir -> true
    | Symlink a, Symlink b -> String.equal a.target b.target
    | Rename a, Rename b -> String.equal a.dest b.dest
    | Chmod a, Chmod b -> Int.equal a.perm b.perm
    | Chown a, Chown b -> Int64.equal a.uid b.uid && Int64.equal a.gid b.gid
    | ( ( Data | Truncate | Create | Mkdir | Unlink | Rmdir | Symlink _
        | Rename _ | Chmod _ | Chown _ ),
        _ ) ->
        false

  type unsynced = {
    path : string;
    kind : op_kind;
    offset : int;
    length : int;
    seq : int;
  }
  (* An un-synced change, as seen by a scripted survival predicate. *)

  (* A directory tree as an image holds it: the durable image, and the
     subtrees a directory operation touched. *)
  type tree =
    | File of {
        data : Cstruct.t;
        perm : int;
        uid : int64;
        gid : int64;
        ino : int64;
      }
    | Dir of {
        perm : int;
        uid : int64;
        gid : int64;
        ino : int64;
        entries : tree Paths.t;
      }
    | Link of { target : string; uid : int64; gid : int64; ino : int64 }

  let ino_of = function File f -> f.ino | Dir d -> d.ino | Link l -> l.ino

  module Inos = Map.Make (Int64)

  type location = fstate * boxed * string

  (* Where each tracked file sits in an image, by its path in the image, and
     that path by the file: each file sits at one place at most. *)
  type locs = { at : location list Paths.t; where : string Ids.t }

  let no_locs = { at = Paths.empty; where = Ids.empty }

  let unlocate (fs : fstate) t =
    match Ids.find_opt fs.id t.where with
    | None -> t
    | Some k ->
        let at =
          Paths.update k
            (function
              | None -> None
              | Some ls -> (
                  match List.filter (fun (f, _, _) -> f != fs) ls with
                  | [] -> None
                  | ls -> Some ls))
            t.at
        in
        { at; where = Ids.remove fs.id t.where }

  let locate_at k ((fs, _, _) as l) t =
    let t = unlocate fs t in
    {
      at = Paths.update k (fun o -> Some (l :: Option.value ~default:[] o)) t.at;
      where = Ids.add fs.id k t.where;
    }

  let unlocate_under k t =
    List.fold_left
      (fun t (_, ls) -> List.fold_left (fun t (fs, _, _) -> unlocate fs t) t ls)
      t (under k t.at)

  (* An image of the tree: the tree, where each tracked file sits in it, and
     where each inode sits. *)
  type state = { tree : tree; locs : locs; inos : string list Inos.t }

  (* What a directory operation did, by inode: a node put in the directory
     [parent] under the path's last name, a node taken out wherever it is,
     or new attributes on a node wherever it is. The path is where it was
     when the operation ran. *)
  type edit =
    | Place of { parent : int64; node : tree }
    | Move of { parent : int64; ino : int64 }
    | Remove of int64
    | Attrs of { ino : int64; perm : int; uid : int64; gid : int64 }

  type delta = (string list * edit) list

  (* A journal entry keeps only what its operation changed. The durable
     image is the last whole snapshot with the committed entries replayed
     over it in program order, and a crash replays the committed and the
     surviving ones the same way. [recorded] is where the files under the
     paths [delta] touched sat when the entry was recorded. *)
  type metadata_entry = {
    meta : unsynced;
    dirs : int64 list;
        (* The directories, by inode, whose barrier commits the entry. *)
    mutable barred : int64 list;
        (* Those of [dirs] a barrier has reached since: the entry is durable
           once one of them is. *)
    delta : delta;
    mutable recorded : location list;
    attrs_ino : int64 option;
        (* For a chmod or a chown, the node it changed: it holds once that
           node is durable, and a sync of that file commits it too. *)
    nodes : int64 list; (* the nodes it makes *)
    parents : int64 list; (* the directories whose entries it changes *)
    ancestors : int64 list;
        (* For a rename, the directories from the root down to where it
           lands. *)
    mutable synced : bool; (* a sync of that file has run since *)
    clear : bool;
        (* A write's setuid or setgid clear: any later commit carries it. *)
    mutable committed : bool;
  }

  (* Documented in chaos.mli. *)
  type policy =
    | Drop_unsynced
    | Reorder_unsynced of int
    | Survive of (unsynced -> bool)
    | Choose of (int -> int)

  (* The draws a crash state is built from: a seeded generator, or a function
     that enumerates them. *)
  type draw = { int : int -> int; bool : unit -> bool }

  let draw_of_state rng =
    { int = Random.State.int rng; bool = (fun () -> Random.State.bool rng) }

  let draw_of_fun f = { int = f; bool = (fun () -> f 2 = 1) }

  type trigger =
    | Manual
    | After_write of { path : string; file_offset : int }
    | Nth_op of int
    | After_dir_op of { kind : op_kind; path : string }

  type base_dir = Eio.Fs.dir_ty Eio.Path.t
  (** The backing store handed back after a crash. *)

  (* Where an open subtree sits: the boxed directory the controller was wrapped
     over and the subtree's path from it. A rename through the wrapper moves
     it with its directory, as it moves a tracked file. *)
  type place = { mutable proot : boxed; mutable prefix : string }

  type t = {
    policy : policy;
    trigger : trigger;
    block_size : int;
    mutable base : base_dir option;
    files : (string, fstate) Hashtbl.t;
    known : (int, fstate) Hashtbl.t; (* every file tracked, by id *)
    mutable subtrees : place list;
    mutable log : entry list;
        (* Every file's changes, newest first, stale ones included until the
           log is compacted. *)
    mutable logged : int; (* the log's length *)
    mutable pending_changes : int; (* its entries still pending *)
    mutable journal : metadata_entry Ids.t;
        (* Every entry since the last whole snapshot, committed or not, by
           seq. *)
    mutable pool : metadata_entry Ids.t;
        (* The pending entries a barrier or a sync has reached. *)
    by_dir : (int64, metadata_entry Ids.t) Hashtbl.t;
        (* The pending entries by the directories whose barrier commits them. *)
    made : (int64, metadata_entry) Hashtbl.t;
        (* The entry that made each node, committed or not, until the
           snapshot holds the node. *)
    by_parent : (int64, metadata_entry Ids.t) Hashtbl.t;
    by_moved : (int64, metadata_entry Ids.t) Hashtbl.t;
    mutable clears : metadata_entry Ids.t; (* the pending setuid clears *)
    by_attrs : (int64, metadata_entry Ids.t) Hashtbl.t;
    mutable snapshot : state option; (* the last whole snapshot *)
    mutable snapped : (string * Cstruct.t) Inos.t;
        (* The path and bytes of every file a whole snapshot held, by inode:
           once removed, its blocks are free, even after the snapshot moved
           on. *)
    mutable left : string list;
        (* What the crash left of the unsynced writes, file by file. *)
    mutable durable : state option;
        (* The snapshot with the committed entries replayed over it, or
           [None] until it is built again. *)
    mutable root_box : boxed option; (* the base given to [wrap] *)
    mutable admin : boxed option;
        (* The base with root's rights, which the model reads and rebuilds
           the tree through: the code under test may run as a user who cannot
           read all of it. *)
    mutable by_rel : fstate list Paths.t; (* the files of [files] by path *)
    mutable held : string -> (int * int) list option;
        (* The byte ranges writes covered in a file of an in-memory base, by
           path: the rest of it is a hole the disk does not hold. *)
    mutable seq : int;
    mutable effects : int;
        (* The commits, barriers and failures of syncs, and the files and
           modes opens met, so far: with [seq], what tells a sync or an open
           that changed nothing a cut leaves. *)
    mutable ops : int;
    mutable crashed : bool;
    mutable image :
      (string * int64 * [ `File of string | `Dir | `Link of string ]) list;
        (* The image the crash rebuilt, by path below the root, with the
           inode each entry had live, for this package's tests. *)
  }

  (* [owner] with root's rights when it is the wrapped base. *)
  let as_admin ctl owner =
    match (ctl.root_box, ctl.admin) with
    | Some proot, Some admin when proot == owner -> admin
    | _ -> owner

  let clog_change ctl fs change =
    ctl.seq <- ctl.seq + 1;
    let e =
      { efs = fs; change; seq = ctl.seq; egen = fs.gen; cached = false }
    in
    ctl.log <- e :: ctl.log;
    fs.pending <- e :: fs.pending;
    ctl.logged <- ctl.logged + 1;
    ctl.pending_changes <- ctl.pending_changes + 1

  let clog_write ctl fs ~offset data =
    clog_change ctl fs (Write { offset; data })

  let to_unsynced e =
    match e.change with
    | Write { offset; data } ->
        {
          path = e.efs.rel;
          kind = Data;
          offset;
          length = Cstruct.length data;
          seq = e.seq;
        }
    | Resize n ->
        {
          path = e.efs.rel;
          kind = Truncate;
          offset = n;
          length = 0;
          seq = e.seq;
        }

  let unsynced ctl =
    let data =
      List.filter_map
        (fun e -> if is_pending e then Some (to_unsynced e) else None)
        ctl.log
    in
    let metadata =
      Ids.fold
        (fun _ e acc -> if e.committed then acc else e.meta :: acc)
        ctl.journal []
    in
    List.sort
      (fun (a : unsynced) (b : unsynced) -> Int.compare a.seq b.seq)
      (data @ metadata)

  (* The bytes of the regular file [path], read with the owner's read bit. *)
  let current_data (Base_dir ((module B), bv) as owner) path =
    with_readable owner path (fun () ->
        Switch.run (fun sw -> read_all (B.open_in bv ~sw ~follow:false path)))

  (* Rewrite the base file to [content], unless it holds it already: truncate
     and re-lay the bytes. A file staged read-only or write-only cannot be
     reopened O_RDWR, so grant the owner read and write bits for the duration
     of the rewrite and restore the exact mode after. *)
  let rewrite ctl fs content =
    let (Base_dir ((module B), bv) as owner) = as_admin ctl fs.owner in
    let holds =
      match B.stat bv ~follow:false fs.rel with
      | { kind = `Regular_file; _ } ->
          Cstruct.equal (current_data owner fs.rel) content
      | _ | (exception Eio.Io _) -> false
    in
    if not holds then begin
      let restore =
        match B.stat bv ~follow:true fs.rel with
        | exception Eio.Io _ -> None
        | { kind = `Regular_file; perm; _ } when perm land 0o600 <> 0o600 ->
            B.chmod bv ~follow:true ~perm:(perm lor 0o600) fs.rel;
            Some perm
        | _ -> None
      in
      Fun.protect ~finally:(fun () ->
          match restore with
          | Some perm -> B.chmod bv ~follow:true ~perm fs.rel
          | None -> ())
      @@ fun () ->
      Switch.run @@ fun sw ->
      let f =
        B.open_out bv ~sw ~follow:true ~append:false
          ~create:(`Or_truncate fs.perm) fs.rel
      in
      if Cstruct.length content > 0 then
        Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [ content ]
    end

  let box_dir (Eio.Resource.T (v, ops)) =
    Base_dir (Eio.Resource.get ops Eio.Fs.Pi.Dir, v)

  let rec snapshot_at (Base_dir ((module B), bv) as owner) path =
    match B.stat bv ~follow:false path with
    | { kind = `Regular_file; perm; uid; gid; ino; _ } ->
        let data =
          with_readable owner path (fun () ->
              Switch.run (fun sw ->
                  read_all (B.open_in bv ~sw ~follow:true path)))
          |> copy_cs
        in
        File { data; perm; uid; gid; ino }
    | { kind = `Directory; perm; uid; gid; ino; _ } ->
        let entries =
          B.read_dir bv path
          |> List.fold_left
               (fun m name ->
                 let child = Eio_fs_path.append path name in
                 Paths.add name (snapshot_at owner child) m)
               Paths.empty
        in
        Dir { perm; uid; gid; ino; entries }
    | { kind = `Symbolic_link; uid; gid; ino; _ } ->
        Link { target = B.read_link bv path; uid; gid; ino }
    | _ -> invalid_arg "Chaos.Crash: unsupported filesystem entry"

  (* The entry at [path] given back to its owner, unless it has them: a host
     process that is not root may give a file only to itself. *)
  let restore_owner (Base_dir ((module B), bv)) path ~uid ~gid =
    let s = B.stat bv ~follow:false path in
    if not (Int64.equal s.uid uid && Int64.equal s.gid gid) then
      B.chown ~follow:false ~uid ~gid bv path

  (* [path]'s mode made [perm], unless it is: a chmod moves the change
     time even to the same mode. *)
  let restore_perm (Base_dir ((module B), bv)) path perm =
    if not (Int.equal (B.stat bv ~follow:false path).perm perm) then
      B.chmod bv ~follow:false ~perm path

  let write_file (Base_dir ((module B), bv)) path ~perm data =
    Switch.run @@ fun sw ->
    let f =
      B.open_out bv ~sw ~follow:false ~append:false ~create:(`Or_truncate perm)
        path
    in
    if Cstruct.length data > 0 then
      Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [ data ]

  (* The base at [path] made [tree], in place: an entry that is still the one
     the image names (its kind and inode, and a file no other name links) is
     kept, its inode and times with it, as a power cut spares them on a disk,
     and only what differs is rewritten. Any other entry is replaced. *)
  let rec restore_at (Base_dir ((module B), bv) as owner) path tree =
    let now =
      match B.stat bv ~follow:false path with
      | st -> Some st
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
    in
    match (tree, now) with
    | File { data; perm; uid; gid; ino }, Some st
      when st.kind = `Regular_file && Int64.equal st.ino ino
           && Int64.equal st.nlink 1L ->
        if not (Cstruct.equal (current_data owner path) data) then begin
          if st.perm land 0o600 <> 0o600 then
            B.chmod bv ~follow:false ~perm:(st.perm lor 0o600) path;
          write_file owner path ~perm data
        end;
        (* The owner first: a chown clears setuid and setgid. *)
        restore_owner owner path ~uid ~gid;
        restore_perm owner path perm
    | File { data; perm; uid; gid; _ }, _ ->
        if Option.is_some now then remove_at owner path;
        write_file owner path ~perm data;
        restore_owner owner path ~uid ~gid;
        B.chmod bv ~follow:false ~perm path
    | Link { target; uid; gid; _ }, Some st
      when st.kind = `Symbolic_link && String.equal (B.read_link bv path) target
      ->
        restore_owner owner path ~uid ~gid
    | Link { target; uid; gid; _ }, _ ->
        if Option.is_some now then remove_at owner path;
        B.symlink ~link_to:target bv path;
        restore_owner owner path ~uid ~gid
    | Dir d, Some st when st.kind = `Directory && Int64.equal st.ino d.ino ->
        refill owner path st d.entries;
        restore_owner owner path ~uid:d.uid ~gid:d.gid;
        restore_perm owner path d.perm
    | Dir { perm; uid; gid; entries; _ }, _ ->
        if Option.is_some now then remove_at owner path;
        (* Writable while it fills; the mode, which the umask also narrows,
           is set after. *)
        B.mkdir bv ~perm:(perm lor 0o700) path;
        Paths.iter
          (fun name child ->
            restore_at owner (Eio_fs_path.append path name) child)
          entries;
        restore_owner owner path ~uid ~gid;
        B.chmod bv ~follow:false ~perm path

  (* The directory [path], whose stat is [st], made to hold [entries]: what it
     holds that they do not name goes, each of them is restored in place. *)
  and refill (Base_dir ((module B), bv) as owner) path (st : Eio.File.Stat.t)
      entries =
    if st.perm land 0o700 <> 0o700 then
      B.chmod bv ~follow:false ~perm:(st.perm lor 0o700) path;
    B.read_dir bv path
    |> List.iter (fun name ->
        if not (Paths.mem name entries) then
          remove_at owner (Eio_fs_path.append path name));
    Paths.iter
      (fun name child -> restore_at owner (Eio_fs_path.append path name) child)
      entries

  let restore_root (Base_dir ((module B), bv) as owner) path = function
    | Dir { perm; uid; gid; entries; _ } ->
        let st = B.stat bv ~follow:true path in
        refill owner path st entries;
        if String.equal path "" then restore_perm owner path st.perm
        else begin
          restore_owner owner path ~uid ~gid;
          B.chmod bv ~follow:true ~perm path
        end
    | File _ | Link _ ->
        invalid_arg "Chaos.Crash: wrapped root is not a directory"

  let locations ctl =
    Hashtbl.fold (fun _ fs acc -> (fs, fs.owner, fs.rel) :: acc) ctl.files []

  (* The tracked files at or under [rel]. *)
  let files_under ctl rel = List.concat_map snd (under rel ctl.by_rel)

  let root ctl =
    match (ctl.admin, ctl.base) with
    | Some admin, Some (_, path) -> (admin, path)
    | _ -> invalid_arg "Chaos.Crash: controller was never wrapped"

  (* A journal path as the model records it: root-relative, '/'-separated, ""
     for the root itself. A barrier names its directory in whatever spelling
     the caller had -- [Filename.dirname] answers "." for a root-level file,
     and a subtree prefix leaves a trailing slash -- so every path reaching the
     journal is reduced to one key first. *)
  let path_segments path =
    String.split_on_char '/' path
    |> List.filter (fun seg ->
        (not (String.equal seg "")) && not (String.equal seg "."))

  let path_key path = String.concat "/" (path_segments path)

  (* [resolve ~root owner path] is [path] with every symbolic link below
     [root] replaced by its target, the last one too unless [follow_last] is
     [false], and its [.], [..] and empty components gone, as
     path_resolution(7) walks it: one spelling per file. A component that does
     not resolve is kept, and so is a path outside [root]. *)
  let resolve_at ?(follow_last = true) ~root (Base_dir ((module B), bv)) path =
    let render abs rev =
      let p = String.concat "/" (List.rev rev) in
      if abs then "/" ^ p else p
    in
    let link_at p =
      match B.stat bv ~follow:false p with
      | { kind = `Symbolic_link; _ } -> (
          try Some (B.read_link bv p) with Eio.Io _ -> None)
      | _ -> None
      | exception Eio.Io _ -> None
    in
    let rec go hops abs rev = function
      | [] -> render abs rev
      | ("" | ".") :: rest -> go hops abs rev rest
      | ".." :: rest -> (
          match rev with
          | name :: up when not (String.equal name "..") -> go hops abs up rest
          | _ when abs -> go hops abs rev rest
          | _ -> go hops abs (".." :: rev) rest)
      | name :: rest -> (
          let p = render abs (name :: rev) in
          let target =
            if hops <= 0 || (rest = [] && not follow_last) then None
            else link_at p
          in
          match target with
          | None -> go hops abs (name :: rev) rest
          | Some target ->
              let segs = String.split_on_char '/' target in
              if Eio_fs_path.is_abs target then
                go (hops - 1) true [] (segs @ rest)
              else go (hops - 1) abs rev (segs @ rest))
    in
    let root = path_segments root in
    let rec strip root segs =
      match (root, segs) with
      | [], segs -> Some segs
      | r :: root, s :: segs when String.equal r s -> strip root segs
      | _ -> None
    in
    match strip root (path_segments path) with
    | None -> path
    | Some rest -> go 40 (Eio_fs_path.is_abs path) (List.rev root) rest

  (* The journal names an entry as the base directory resource sees it: the path
     {!wrap} was handed, with the entry below it. The durable image is a
     snapshot rooted at that same path, so indexing the image means dropping the
     root's own segments first. A path that is not under the root -- a rename
     out of the wrapped tree -- has no place in the image at all. *)
  let tree_path ~root path =
    let rec strip root segs =
      match (root, segs) with
      | [], segs -> Some segs
      | r :: root, s :: segs when String.equal r s -> strip root segs
      | _ -> None
    in
    strip root (path_segments path)

  let rec tree_find tree segs =
    match (tree, segs) with
    | _, [] -> Some tree
    | (File _ | Link _), _ -> None
    | Dir { entries; _ }, name :: rest -> (
        match Paths.find_opt name entries with
        | Some child -> tree_find child rest
        | None -> None)

  (* [tree_replace tree segs node] is [tree] with the entry at [segs] set to
     [node], or freed when [node] is [None]. A path whose parent the tree does
     not hold, or holds as something other than a directory, leaves the tree
     untouched: the entry went with the directory that was to hold it. *)
  let rec tree_replace tree segs node =
    match (tree, segs) with
    | _, [] -> tree
    | (File _ | Link _), _ -> tree
    | Dir d, [ name ] ->
        let entries =
          match node with
          | None -> Paths.remove name d.entries
          | Some child -> Paths.add name child d.entries
        in
        Dir { d with entries }
    | Dir d, name :: rest -> (
        match Paths.find_opt name d.entries with
        | None -> tree
        | Some child ->
            let child = tree_replace child rest node in
            Dir { d with entries = Paths.add name child d.entries })

  let rec is_prefix prefix segs =
    match (prefix, segs) with
    | [], _ -> true
    | p :: prefix, s :: segs when String.equal p s -> is_prefix prefix segs
    | _ -> false

  (* [f] on each node of [tree], whose path is [segs], and on its path. *)
  let rec fold_nodes f segs tree acc =
    let acc = f segs tree acc in
    match tree with
    | Dir d ->
        Paths.fold
          (fun name child acc -> fold_nodes f (segs @ [ name ]) child acc)
          d.entries acc
    | File _ | Link _ -> acc

  let index segs tree inos =
    fold_nodes (fun s n m -> Inos.add (ino_of n) s m) segs tree inos

  let unindex tree inos =
    fold_nodes (fun _ n m -> Inos.remove (ino_of n) m) [] tree inos

  let state_of tree locs = { tree; locs; inos = index [] tree Inos.empty }

  (* [st] with [node] at [segs], whose parent it holds, in place of what was
     there. *)
  let set_node st segs node =
    let inos =
      match tree_find st.tree segs with
      | Some old -> unindex old st.inos
      | None -> st.inos
    in
    let tree = tree_replace st.tree segs node in
    let inos = match node with Some n -> index segs n inos | None -> inos in
    { st with tree; inos }

  let remove_path st segs =
    let st =
      { st with locs = unlocate_under (String.concat "/" segs) st.locs }
    in
    set_node st segs None

  let with_attrs ~perm ~uid ~gid = function
    | File f -> File { f with perm; uid; gid }
    | Dir d -> Dir { d with perm; uid; gid }
    | Link l -> Link { l with uid; gid }

  let rec drop prefix segs =
    match (prefix, segs) with
    | [], s -> s
    | _ :: p, _ :: s -> drop p s
    | _ :: _, [] -> []

  (* Where the directory [parent], which held the entry at [segs] when the
     operation ran, is in [st]: at that path if it is still there, or
     wherever that inode is now. *)
  let target_dir st segs parent =
    let dir_at at =
      match tree_find st.tree at with
      | Some (Dir d) -> Int64.equal d.ino parent
      | Some (File _ | Link _) | None -> false
    in
    match List.rev segs with
    | [] -> None
    | name :: up -> (
        let up = List.rev up in
        if dir_at up then Some (up @ [ name ])
        else
          match Inos.find_opt parent st.inos with
          | Some at when dir_at at -> Some (at @ [ name ])
          | Some _ | None -> None)

  (* Whether every half of [e] lands in [st]: an entry takes effect whole or
     not at all, so a node put in a directory the image does not hold is not
     made, and an rmdir of a directory that still holds entries there removes
     nothing. A rename always lands. *)
  let applies st e =
    List.for_all
      (fun (segs, edit) ->
        match edit with
        | Place { parent; _ } -> Option.is_some (target_dir st segs parent)
        | Move _ -> true
        | Remove ino -> (
            match Inos.find_opt ino st.inos with
            | Some at -> (
                match tree_find st.tree at with
                | Some (Dir d) -> Paths.is_empty d.entries
                | Some (File _ | Link _) | None -> true)
            | None -> true)
        | Attrs _ -> true)
      e.delta

  (* A node a rename took where the image has no directory for it stays in
     the image under the root, by a name no directory entry can have, so a
     later rename can bring it back with what it holds. A crash leaves them
     out. *)
  let limbo ino = "\000" ^ Int64.to_string ino
  let is_limbo name = String.length name > 0 && Char.equal name.[0] '\000'
  let in_limbo = function name :: _ -> is_limbo name | [] -> false

  (* [st] with the node at [from], and what it holds, at [at] instead. *)
  let move_node st ~from ~at =
    match tree_find st.tree from with
    | None -> st
    | Some node ->
        let moved = under (String.concat "/" from) st.locs.at in
        let st = remove_path st from in
        let st = remove_path st at in
        let st = set_node st at (Some node) in
        let n = List.length from in
        let locs =
          List.fold_left
            (fun locs (k, ls) ->
              let below = List.filteri (fun i _ -> i >= n) (path_segments k) in
              let key = String.concat "/" (at @ below) in
              List.fold_left (fun locs l -> locate_at key l locs) locs ls)
            st.locs moved
        in
        { st with locs }

  (* [st] with [node], made at [segs], at [at], and the files [recorded]
     below [segs] located below [at]. *)
  let put ~root st ~segs ~at ~node ~recorded =
    let st = remove_path st at in
    let st = set_node st at (Some node) in
    let locs =
      List.fold_left
        (fun locs ((_, _, rel) as l) ->
          match tree_path ~root rel with
          | Some s when is_prefix segs s ->
              locate_at (String.concat "/" (at @ drop segs s)) l locs
          | Some _ | None -> locs)
        st.locs recorded
    in
    { st with locs }

  (* Replay entry [e]'s delta onto an image, whole or not at all. Each half
     finds its node by inode: a new node goes into the directory that held it
     when the operation ran, a moved node is the image's own, taken from
     wherever it is with what it holds there, and a node is taken out, or
     given attributes, wherever it is. A moved node the image does not hold,
     as one made in a directory the image lacks, is put where it moved as
     [made] gives it, and a node moved into a directory the image does not
     hold, or below itself, goes to limbo. *)
  let replay ~root ~made st e =
    if not (applies st e) then st
    else
      List.fold_left
        (fun st (segs, edit) ->
          match edit with
          | Remove ino -> (
              match Inos.find_opt ino st.inos with
              | Some (_ :: _ as at) -> remove_path st at
              | Some [] | None -> st)
          | Attrs { ino; perm; uid; gid } -> (
              match Inos.find_opt ino st.inos with
              | None -> st
              | Some at -> (
                  match tree_find st.tree at with
                  | None -> st
                  | Some node -> (
                      let node = with_attrs ~perm ~uid ~gid node in
                      match at with
                      | [] -> { st with tree = node }
                      | _ ->
                          { st with tree = tree_replace st.tree at (Some node) }
                      )))
          | Move { parent; ino } -> (
              let at =
                match target_dir st segs parent with
                | Some at -> at
                | None -> [ limbo ino ]
              in
              match Inos.find_opt ino st.inos with
              | Some [] -> st
              | Some from when from = at -> st
              | Some from ->
                  let at = if is_prefix from at then [ limbo ino ] else at in
                  move_node st ~from ~at
              | None -> (
                  match made ino with
                  | Some (segs, node, recorded) ->
                      put ~root st ~segs ~at ~node ~recorded
                  | None -> st))
          | Place { parent; node } -> (
              match target_dir st segs parent with
              | None -> st
              | Some _ when Inos.mem (ino_of node) st.inos -> st
              | Some at -> put ~root st ~segs ~at ~node ~recorded:e.recorded))
        st e.delta

  (* The path of [p]'s directory, spelt as [p] is. *)
  let parent_path p =
    let segs = path_segments p in
    let up = match List.rev segs with [] -> [] | _ :: up -> List.rev up in
    let s = String.concat "/" up in
    if Eio_fs_path.is_abs p then "/" ^ s else s

  let ino_at (Base_dir ((module B), bv)) p =
    match B.stat bv ~follow:false p with
    | st -> Some st.ino
    | exception Eio.Io _ -> None

  (* Whether [loc] lies at or under a node [delta] makes. *)
  let covers ~root delta (_, _, rel) =
    match tree_path ~root rel with
    | None -> false
    | Some s ->
        List.exists
          (fun (segs, edit) ->
            match edit with
            | Place _ -> is_prefix segs s
            | Move _ | Remove _ | Attrs _ -> false)
          delta

  (* [locs] in an image keyed below [root]. *)
  let locs_of ~root locs =
    List.fold_left
      (fun t ((_, _, rel) as l) ->
        match tree_path ~root rel with
        | Some s -> locate_at (String.concat "/" s) l t
        | None -> t)
      no_locs locs

  let metadata_unsynced ~path ~kind ~seq =
    { path; kind; offset = 0; length = 0; seq }

  let root_segs ctl =
    match ctl.base with
    | Some (_, root_path) -> path_segments root_path
    | None -> []

  (* {2 The journal and its indexes} *)

  let add_to tbl key e =
    let m = Option.value ~default:Ids.empty (Hashtbl.find_opt tbl key) in
    Hashtbl.replace tbl key (Ids.add e.meta.seq e m)

  let remove_from tbl key e =
    match Hashtbl.find_opt tbl key with
    | None -> ()
    | Some m ->
        let m = Ids.remove e.meta.seq m in
        if Ids.is_empty m then Hashtbl.remove tbl key
        else Hashtbl.replace tbl key m

  let candidate e =
    (not e.committed)
    && (e.barred <> [] || (e.synced && Option.is_some e.attrs_ino))

  let refresh ctl e =
    ctl.pool <-
      (if candidate e then Ids.add e.meta.seq e ctl.pool
       else Ids.remove e.meta.seq ctl.pool)

  let moved e =
    List.filter_map
      (fun (_, edit) ->
        match edit with
        | Move { ino; _ } -> Some ino
        | Place _ | Remove _ | Attrs _ -> None)
      e.delta

  let index_entry ctl e =
    ctl.journal <- Ids.add e.meta.seq e ctl.journal;
    List.iter (fun d -> add_to ctl.by_dir d e) e.dirs;
    List.iter (fun n -> Hashtbl.replace ctl.made n e) e.nodes;
    List.iter (fun p -> add_to ctl.by_parent p e) e.parents;
    List.iter (fun n -> add_to ctl.by_moved n e) (moved e);
    Option.iter (fun a -> add_to ctl.by_attrs a e) e.attrs_ino

  (* A committed entry leaves the indexes of pending ones. *)
  let mark_committed ctl e =
    ctl.effects <- ctl.effects + 1;
    e.committed <- true;
    List.iter (fun d -> remove_from ctl.by_dir d e) e.dirs;
    List.iter (fun p -> remove_from ctl.by_parent p e) e.parents;
    List.iter (fun n -> remove_from ctl.by_moved n e) (moved e);
    Option.iter (fun a -> remove_from ctl.by_attrs a e) e.attrs_ino;
    if e.clear then ctl.clears <- Ids.remove e.meta.seq ctl.clears;
    ctl.pool <- Ids.remove e.meta.seq ctl.pool

  (* The pending entries before [e] it depends on: those that changed a
     directory [e] changes, as one directory's entries stay in order; those
     that made a node [e] names; for an rmdir, those that changed the
     directory it removes; and for a rename, those that moved a directory
     above where it lands, so it never lands inside itself. A directory renamed before [e] changed it is no
     dependency: its inode is durable under one name or the other. A change
     of attributes waits for its node and pulls nothing in. *)
  let deps ctl e =
    match e.attrs_ino with
    | Some _ -> []
    | None ->
        let earlier tbl key =
          match Hashtbl.find_opt tbl key with
          | None -> []
          | Some m ->
              Ids.fold
                (fun s e' acc -> if s < e.meta.seq then e' :: acc else acc)
                m []
        in
        List.concat_map (earlier ctl.by_parent) e.parents
        @ List.concat_map (earlier ctl.by_moved) e.ancestors
        @ List.concat_map
            (fun (_, edit) ->
              match edit with
              | Remove ino -> earlier ctl.by_parent ino
              | Place _ | Move _ | Attrs _ -> [])
            e.delta

  (* [e] and, transitively, the pending entries it depends on. *)
  let closure ctl e =
    let seen = Hashtbl.create 8 in
    let rec go acc e =
      if Hashtbl.mem seen e.meta.seq then acc
      else begin
        Hashtbl.replace seen e.meta.seq ();
        List.fold_left go (e :: acc) (deps ctl e)
      end
    in
    go [] e

  (* [path] and [kind] as the caller spelled them, [resolved] the paths the
     operation touched, resolved: its own first. [gone] gives the inode a
     path held before an operation that took it away, and [victim] the node
     a rename replaced. *)
  let record_metadata ?(gone = []) ?victim ctl ~path ~kind ~resolved =
    ctl.seq <- ctl.seq + 1;
    let owner, root_path = root ctl in
    let meta = metadata_unsynced ~path ~kind ~seq:ctl.seq in
    let root = path_segments root_path in
    let parent p = ino_at owner (parent_path p) in
    let ancestors =
      match (kind, resolved) with
      | Rename _, [ _; dst ] ->
          let segs = path_segments (parent_path dst) in
          let lead = if Eio_fs_path.is_abs dst then "/" else "" in
          List.init
            (List.length segs + 1 - List.length root)
            (fun i -> List.length root + i)
          |> List.filter_map (fun n ->
              ino_at owner
                (lead ^ String.concat "/" (List.filteri (fun j _ -> j < n) segs)))
      | _ -> []
    in
    let delta, dirs, attrs_ino =
      match (kind, resolved) with
      | (Chmod _ | Chown _), [ p ] -> (
          (* New attributes on the node: committed by a barrier on the node
             itself, for a directory, or by its sync, for a file. A barrier on
             its parent does not: ext4's fsync of a directory completes only
             that directory's own last transaction. *)
          let (Base_dir ((module B), bv)) = owner in
          let st = B.stat bv ~follow:false p in
          match tree_path ~root p with
          | Some segs ->
              let edit =
                Attrs
                  { ino = st.ino; perm = st.perm; uid = st.uid; gid = st.gid }
              in
              ([ (segs, edit) ], [ st.ino ], Some st.ino)
          | None -> ([], [], None))
      | Rename _, [ src; dst ] -> (
          (* A rename moves the image's own node: nothing of what it holds
             live comes with it. It is committed by a barrier on either
             directory. *)
          match
            ( List.assoc_opt src gone,
              tree_path ~root dst,
              parent src,
              parent dst )
          with
          | Some ino, Some segs, Some from, Some up ->
              (* The node it replaces goes first: a directory the image
                 still fills stops the rename, and the entries that emptied
                 it come with it. *)
              let replaced =
                match victim with
                | Some v when not (Int64.equal v ino) -> [ (segs, Remove v) ]
                | Some _ | None -> []
              in
              ( replaced @ [ (segs, Move { parent = up; ino }) ],
                [ from; up ],
                None )
          | _ -> ([], [], None))
      | _ ->
          let halves =
            List.filter_map
              (fun p ->
                match (tree_path ~root p, parent p) with
                | None, _ | _, None -> None
                | Some segs, Some up -> (
                    match List.assoc_opt p gone with
                    | Some ino -> Some (up, (segs, Remove ino))
                    | None -> (
                        match snapshot_at owner p with
                        | node -> Some (up, (segs, Place { parent = up; node }))
                        | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
                            None)))
              resolved
          in
          (List.map snd halves, List.map fst halves, None)
    in
    let recorded =
      List.concat_map (files_under ctl) resolved
      |> List.sort_uniq (fun a b -> Int.compare a.id b.id)
      |> List.map (fun fs -> (fs, fs.owner, fs.rel))
      |> List.filter (covers ~root delta)
    in
    let nodes =
      List.filter_map
        (fun (_, edit) ->
          match edit with
          | Place { node; _ } -> Some (ino_of node)
          | Move _ | Remove _ | Attrs _ -> None)
        delta
    in
    let parents =
      match kind with
      | Chmod _ | Chown _ -> []
      | _ -> List.sort_uniq Int64.compare dirs
    in
    index_entry ctl
      {
        meta;
        dirs;
        barred = [];
        delta;
        recorded;
        attrs_ino;
        nodes;
        parents;
        ancestors;
        synced = false;
        clear = false;
        committed = false;
      }

  let reset_journal ctl =
    ctl.journal <- Ids.empty;
    ctl.pool <- Ids.empty;
    Hashtbl.reset ctl.by_dir;
    Hashtbl.reset ctl.made;
    Hashtbl.reset ctl.by_parent;
    Hashtbl.reset ctl.by_moved;
    ctl.clears <- Ids.empty;
    Hashtbl.reset ctl.by_attrs

  let snap ctl st =
    ctl.snapshot <- Some st;
    ctl.durable <- Some st;
    ctl.snapped <-
      fold_nodes
        (fun segs n acc ->
          match n with
          | File f -> Inos.add f.ino ("/" ^ String.concat "/" segs, f.data) acc
          | _ -> acc)
        [] st.tree ctl.snapped

  let commit_metadata ctl =
    let owner, root_path = root ctl in
    let st =
      state_of
        (snapshot_at owner root_path)
        (locs_of ~root:(path_segments root_path) (locations ctl))
    in
    snap ctl st;
    reset_journal ctl

  (* How the entry that made [ino] put it: where, as what, and the files it
     recorded below it. *)
  let made_node ctl ino =
    match Hashtbl.find_opt ctl.made ino with
    | None -> None
    | Some e ->
        List.find_map
          (fun (segs, edit) ->
            match edit with
            | Place { node; _ } when Int64.equal (ino_of node) ino ->
                Some (segs, node, e.recorded)
            | Place _ | Move _ | Remove _ | Attrs _ -> None)
          e.delta

  let replay_in ctl st e =
    replay ~root:(root_segs ctl) ~made:(made_node ctl) st e

  (* The durable image, built from the snapshot when it is not kept. *)
  let durable ctl =
    match (ctl.durable, ctl.snapshot) with
    | Some st, _ -> st
    | None, None -> invalid_arg "Chaos.Crash: controller was never wrapped"
    | None, Some snapshot ->
        let st =
          Ids.fold
            (fun _ e st -> if e.committed then replay_in ctl st e else st)
            ctl.journal snapshot
        in
        ctl.durable <- Some st;
        st

  (* The committed entries before the first pending one go into the
     snapshot: nothing replays them again. *)
  let fold_committed ctl =
    match ctl.snapshot with
    | None -> ()
    | Some snapshot ->
        let rec go st =
          match Ids.min_binding_opt ctl.journal with
          | Some (s, e) when e.committed ->
              ctl.journal <- Ids.remove s ctl.journal;
              let st = replay_in ctl st e in
              List.iter
                (fun n -> if Inos.mem n st.inos then Hashtbl.remove ctl.made n)
                e.nodes;
              go st
          | Some _ | None -> st
        in
        ctl.snapshot <- Some (go snapshot)

  (* Whether [e], which a barrier or a sync reached, holds in [st]: a change
     of attributes once its node is there, and an entry a barrier reached
     once a directory it changes is there, as the entry changes that one even
     where the other is not durable yet. *)
  let reached st e =
    let held ino =
      match Inos.find_opt ino st.inos with
      | Some at -> not (in_limbo at)
      | None -> false
    in
    (match e.attrs_ino with Some ino -> held ino | None -> true)
    && ((e.barred <> [] && List.exists held e.dirs)
       || (e.synced && Option.is_some e.attrs_ino))

  (* fsync(2) on a directory commits that directory's entries and nothing
     else: the journal entries that changed it, keyed by its inode, so a
     barrier follows the directory through a rename and never reaches
     another of the same name. An entry of a directory whose own entry is not
     durable cannot be durable yet: after the power cut there is no directory
     to hold it. The barrier is remembered on the entry, which becomes
     durable as soon as a directory it changes is, with the pending entries
     before it that it needs.

     The entries a barrier commits are not in general a journal prefix, and
     one committed late is replayed over the durable image as it stands: it
     changes no directory a later committed entry changes, as that entry
     would have pulled it in, so it lands as it would in program order. *)
  let rec settle ctl =
    let st = durable ctl in
    let ready e = (not e.committed) && reached st e in
    match
      Ids.fold (fun _ e acc -> if ready e then e :: acc else acc) ctl.pool []
    with
    | [] -> fold_committed ctl
    | ready ->
        let newly = List.concat_map (closure ctl) ready in
        (* A commit carries the setuid clears before it, which ext4
           journals in the running transaction. *)
        let last = List.fold_left (fun m e -> max m e.meta.seq) 0 newly in
        let clears =
          Ids.fold
            (fun s e acc -> if s < last then e :: acc else acc)
            ctl.clears []
        in
        let newly =
          List.sort_uniq
            (fun a b -> Int.compare a.meta.seq b.meta.seq)
            (clears @ newly)
        in
        List.iter (mark_committed ctl) newly;
        ctl.durable <- Some (List.fold_left (replay_in ctl) st newly);
        settle ctl

  (* Hold a barrier on the directory [ino] for the entries that changed it,
     then commit what it can. *)
  let commit_dir_ino ctl ino =
    (match Hashtbl.find_opt ctl.by_dir ino with
    | None -> ()
    | Some m ->
        Ids.iter
          (fun _ e ->
            if not (List.exists (Int64.equal ino) e.barred) then begin
              ctl.effects <- ctl.effects + 1;
              e.barred <- ino :: e.barred;
              refresh ctl e
            end)
          m);
    settle ctl

  let commit_dir ctl ~dir =
    let owner, _ = root ctl in
    match ino_at owner dir with
    | Some ino -> commit_dir_ino ctl ino
    | None -> ()

  (* fsync(2) of a file commits its inode, attributes included: they hold
     once the image holds the file. *)
  let commit_attrs ctl ~ino =
    if Hashtbl.length ctl.by_attrs > 0 then begin
      let ino = ino () in
      (match Hashtbl.find_opt ctl.by_attrs ino with
      | None -> ()
      | Some m ->
          Ids.iter
            (fun _ e ->
              if not e.synced then begin
                ctl.effects <- ctl.effects + 1;
                e.synced <- true;
                refresh ctl e
              end)
            m);
      settle ctl
    end

  (* A write or a truncation that cleared setuid or setgid changed the
     file's mode, which ext4 journals at once, in program order: a pending
     change of attributes, committed by the file's sync and by any later
     commit. *)
  let record_clear ctl fs (st : Eio.File.Stat.t) =
    fs.fino <- st.ino;
    ctl.seq <- ctl.seq + 1;
    let segs =
      Option.value ~default:[] (tree_path ~root:(root_segs ctl) fs.rel)
    in
    let e =
      {
        meta =
          metadata_unsynced ~path:fs.rel
            ~kind:(Chmod { perm = st.perm })
            ~seq:ctl.seq;
        dirs = [];
        barred = [];
        delta =
          [
            ( segs,
              Attrs { ino = st.ino; perm = st.perm; uid = st.uid; gid = st.gid }
            );
          ];
        recorded = [];
        attrs_ino = Some st.ino;
        nodes = [];
        parents = [];
        ancestors = [];
        synced = false;
        clear = true;
        committed = false;
      }
    in
    index_entry ctl e;
    ctl.clears <- Ids.add e.meta.seq e ctl.clears

  (* A file the wrapper first meets on an open that finds it on disk was made
     by nothing the model recorded. It is the file an image holds at its
     path if that node is its inode, or wherever the image holds that inode:
     the snapshot and the durable image, and the nodes pending entries put.
     Locating it there lets a crash rewrite it with what it synced. *)
  let locate_preexisting ctl ((_, _, rel) as location) ~ino =
    let root = root_segs ctl in
    match tree_path ~root rel with
    | None -> ()
    | Some segs ->
        let key = String.concat "/" segs in
        let mine tree =
          match tree_find tree segs with
          | Some (File f) -> Int64.equal f.ino ino
          | Some (Dir _ | Link _) | None -> false
        in
        (* By inode: an uncommitted rename of the file, or of a directory
           above it, leaves it under its old name in the image. *)
        let locate st =
          if mine st.tree then { st with locs = locate_at key location st.locs }
          else
            match Inos.find_opt ino st.inos with
            | Some (_ :: _ as at) -> (
                match tree_find st.tree at with
                | Some (File f) when Int64.equal f.ino ino ->
                    {
                      st with
                      locs = locate_at (String.concat "/" at) location st.locs;
                    }
                | Some _ | None -> st)
            | Some [] | None -> st
        in
        ctl.snapshot <- Option.map locate ctl.snapshot;
        ctl.durable <- Option.map locate ctl.durable;
        let in_delta e =
          List.exists
            (fun (s, edit) ->
              match edit with
              | Place { node; _ } when is_prefix s segs -> (
                  match tree_find node (drop s segs) with
                  | Some (File f) -> Int64.equal f.ino ino
                  | Some (Dir _ | Link _) | None -> false)
              | Place _ | Move _ | Remove _ | Attrs _ -> false)
            e.delta
        in
        Ids.iter
          (fun _ e -> if in_delta e then e.recorded <- location :: e.recorded)
          ctl.journal

  (* ext4's block, which it allocates whole, and the disk's sector, which
     it writes whole. *)
  let page = 4096
  let sector = 512

  (* [held] with [change] made durable: a write adds what it covers, a
     shrink frees the pages past it. *)
  let hold held = function
    | Write { offset; data } ->
        let a = offset and b = offset + Cstruct.length data in
        if b <= a then held
        else
          let before = List.filter (fun (_, y) -> y < a) held
          and after = List.filter (fun (x, _) -> x > b) held
          and over = List.filter (fun (x, y) -> y >= a && x <= b) held in
          let a = List.fold_left (fun a (x, _) -> min a x) a over
          and b = List.fold_left (fun b (_, y) -> max b y) b over in
          before @ ((a, b) :: after)
    | Resize m ->
        let m = (m + page - 1) / page * page in
        List.filter_map
          (fun (x, y) -> if x >= m then None else Some (x, min y m))
          held

  (* Whether the disk holds the page of the byte range [\[a, b)]. *)
  let held_at held a b =
    let a = a / page * page and b = (b + page - 1) / page * page in
    List.exists (fun (x, y) -> x < b && y > a) held

  (* The bytes [change] dirties in a file of [prev] bytes: a write those it
     covers and, past the end, the hole it leaves; a shrink the block it cuts
     mid-way, and with [zero_page] the rest of the page it cuts, which ext4
     zeroes; a growth the bytes it adds. *)
  let dirtied ?(zero_page = false) ~bs prev = function
    | Write { offset; data } -> (min prev offset, offset + Cstruct.length data)
    | Resize m when m < prev ->
        let block = if m mod bs = 0 then m else m + 1 in
        if zero_page && m mod page <> 0 then
          (m, max block (min prev (((m / page) + 1) * page)))
        else (m, block)
    | Resize m -> (prev, m)

  (* [durable] with [changes] replayed over it, where a block they dirty in
     the page cache, [cache] with the same changes, is written whole from
     it: the page cache still holds the bytes a failed sync lost. And the page
     cache after them. *)
  let replay_cached ~bs durable ~cache changes =
    match cache with
    | None ->
        let c = replay_changes durable (List.map fst changes) in
        (c, c)
    | Some cache ->
        let plain = copy_cs (replay_changes durable (List.map fst changes)) in
        let live =
          replay_changes cache
            (List.filter_map
               (fun (c, cached) -> if cached then None else Some c)
               changes)
        in
        let len = Cstruct.length plain and live_len = Cstruct.length live in
        let size prev = function
          | Write { offset; data } -> max prev (offset + Cstruct.length data)
          | Resize m -> m
        in
        let _ =
          List.fold_left
            (fun (pprev, cprev) (change, cached) ->
              (* A kept truncation carries its length only. *)
              let lo, hi =
                if cached then (0, 0) else dirtied ~bs cprev change
              in
              if hi > lo then
                for b = lo / bs to (hi - 1) / bs do
                  let off = b * bs in
                  let n = max 0 (min bs (min len live_len - off)) in
                  if n > 0 then Cstruct.blit live off plain off n
                done;
              (size pprev change, if cached then cprev else size cprev change))
            (Cstruct.length durable, Cstruct.length cache)
            changes
        in
        (plain, live)

  (* The blocks [changes] dirty, or only their writes when [writes], replayed
     from a file of [len] bytes. *)
  let dirty_blocks ?(writes = false) ~bs len changes =
    let _, acc =
      List.fold_left
        (fun (prev, acc) change ->
          let acc =
            match change with
            | Resize _ when writes -> acc
            | Write _ | Resize _ ->
                let lo, hi = dirtied ~bs prev change in
                if hi > lo then
                  List.init
                    (((hi - 1) / bs) - (lo / bs) + 1)
                    (fun i -> (lo / bs) + i)
                  @ acc
                else acc
          in
          ( (match change with
            | Write { offset; data } -> max prev (offset + Cstruct.length data)
            | Resize m -> m),
            acc ))
        (len, []) changes
    in
    List.sort_uniq Int.compare acc

  (* The byte that stands for stale data where the disk held nothing the
     model knows of: a digit, so that a reader that trusts what it finds
     parses a number. *)
  let stale = '7'

  (* Writeback in blocks of [bs] bytes. For each block a pending change
     dirtied, a crash keeps the block as it was durable or as it stood after
     any one of those changes, drawn from [rng]; the length is one a change
     left, or a block boundary a write grew the file across, as writeback
     lands a long append block by block. A write past the end of the file
     dirties the hole it leaves as well. A truncation changes the length in
     one step and dirties only the block it cuts, so a block kept from before
     it keeps its old bytes. A block is never torn, and a later change never
     lands in a block without the earlier ones.

     With [bytes], blocks are sectors and the length is one a change left
     or the end of a page a write put bytes in, as ext4 extends its on-disk
     size as writeback lands. A page the disk did not hold that a write put
     bytes in is written whole, and each of its sectors may also hold stale
     data: ext4 may commit the page's allocation before its bytes land.
     [old p] is what the freed blocks the page may have been given held, at
     page [p] of its file, and where each was; one of them is drawn. [describe] is told the length and, run by run, what each
     block holds. *)
  let writeback ?(over = []) ?(len_upto = max_int) ?(bytes = false) ?(held = [])
      ?(old = fun _ -> []) ?(describe = fun _ _ -> ()) ~rng ~bs ~cache durable
      changes =
    let cached = Array.of_list (List.map snd changes) in
    let changes = Array.of_list (List.map fst changes) in
    let n = Array.length changes in
    (* Dirt is counted in the page cache, which a failed sync may have left
       apart from the durable bytes. *)
    let cprev = ref (Cstruct.length (Option.value ~default:durable cache)) in
    let sizes = Array.make (n + 1) (Cstruct.length durable) in
    let versions = Hashtbl.create 16 in
    let boundaries = ref [] in
    (* The pages a pending write put bytes in. *)
    let filled = Hashtbl.create 8 in
    for k = 0 to n - 1 do
      let prev = sizes.(k) in
      (match changes.(k) with
      | Write { offset; data } ->
          let size = max prev (offset + Cstruct.length data) in
          sizes.(k + 1) <- size;
          if k < len_upto then begin
            let step = if bytes then page else bs in
            (* ext4 extends its on-disk size, up to the file's, as it writes
               back a page a write put bytes in, this one or an earlier one,
               not one of the hole before it. *)
            for b = (prev / step) + 1 to (size - 1) / step do
              if
                (not bytes)
                || b - 1 >= offset / step
                || Hashtbl.mem filled (b - 1)
              then boundaries := (b * step, k) :: !boundaries
            done
          end;
          if bytes && Cstruct.length data > 0 then
            for
              p = offset / page to (offset + Cstruct.length data - 1) / page
            do
              Hashtbl.replace filled p ()
            done
      | Resize m -> sizes.(k + 1) <- m);
      let lo, hi =
        if cached.(k) then (0, 0)
        else dirtied ~zero_page:bytes ~bs !cprev changes.(k)
      in
      (if not cached.(k) then
         cprev :=
           match changes.(k) with
           | Write { offset; data } -> max !cprev (offset + Cstruct.length data)
           | Resize m -> m);
      (* ext4 writes a page it allocates whole: with [bytes], a write into
         a page the disk does not hold dirties the rest of the page within
         the length too. *)
      let pages =
        match changes.(k) with
        | Write { offset; data } when bytes && not cached.(k) ->
            let last = (offset + Cstruct.length data - 1) / page in
            List.filter_map
              (fun pg ->
                let a = pg * page in
                let b = min (a + page) sizes.(k + 1) in
                if b > a && not (held_at held a b) then Some (a, b) else None)
              (List.init
                 (max 0 (last - (offset / page) + 1))
                 (fun i -> (offset / page) + i))
        | _ -> []
      in
      let dirty = Hashtbl.create 8 in
      List.iter
        (fun (lo, hi) ->
          if hi > lo then
            for b = lo / bs to (hi - 1) / bs do
              Hashtbl.replace dirty b ()
            done)
        ((lo, hi) :: pages);
      Hashtbl.iter
        (fun b () ->
          let vs = Option.value ~default:[] (Hashtbl.find_opt versions b) in
          Hashtbl.replace versions b ((k + 1) :: vs))
        dirty
    done;
    (* Each dirty block's version, in block order: [0] is the durable one, [k]
       the one after change [k]. *)
    let blocks =
      Hashtbl.fold (fun b _ acc -> b :: acc) versions []
      |> List.sort Int.compare
    in
    (* Whether a write put bytes in block [b], in a page the disk did not
       hold: the page is allocated by the crash, and may hold stale data. *)
    let may_be_stale b vs =
      bytes
      && (not (held_at held (b * bs) ((b + 1) * bs)))
      && List.exists
           (fun v ->
             match changes.(v - 1) with
             | Write { offset; data } when not cached.(v - 1) ->
                 let lo = offset / page * page
                 and hi =
                   (offset + Cstruct.length data + page - 1) / page * page
                 in
                 b * bs >= lo && b * bs < hi
             | _ -> false)
           vs
    in
    (* The length, and how many changes its metadata holds: a length a
       change left holds those before it, a page boundary those up to the
       write that grew the file across it. *)
    let size, committed =
      let boundaries = List.sort_uniq compare !boundaries in
      if boundaries = [] || rng.bool () then (
        (* Two counts that leave the same length and the same shrinks
           pending leave the same state: draw one of each. *)
        let shrinks = Array.make (n + 1) 0 in
        for k = n - 1 downto 0 do
          shrinks.(k) <-
            (shrinks.(k + 1) + match changes.(k) with Resize _ -> 1 | _ -> 0)
        done;
        let js =
          List.init (min n len_upto + 1) Fun.id
          |> List.fold_left
               (fun acc j ->
                 if
                   List.exists
                     (fun i ->
                       sizes.(i) = sizes.(j) && shrinks.(i) = shrinks.(j))
                     acc
                 then acc
                 else j :: acc)
               []
          |> List.rev
        in
        let j = List.nth js (rng.int (List.length js)) in
        (sizes.(j), j))
      else
        let b, k = List.nth boundaries (rng.int (List.length boundaries)) in
        (b, k + 1)
    in
    (* A shrink the length does not hold has freed no block yet: the pages
       past it keep their blocks, which no later change reaches. *)
    let upto b =
      let at = b * bs in
      let limit = ref max_int in
      for k = n - 1 downto committed do
        match changes.(k) with
        | Resize m
          when m < sizes.(k)
               && at >= (m + page - 1) / page * page
               && at < sizes.(k) ->
            limit := k
        | _ -> ()
      done;
      !limit
    in
    let chosen = Hashtbl.create 16 and stale_blocks = Hashtbl.create 4 in
    List.iter
      (fun b ->
        let limit = upto b in
        let ks =
          List.filter (fun v -> v <= limit) (List.rev (Hashtbl.find versions b))
        in
        let stale =
          if may_be_stale b ks then
            List.init
              (max 1 (List.length (old (b * bs / page))))
              (fun i -> -1 - i)
          else []
        in
        let vs = (0 :: ks) @ stale in
        let v = List.nth vs (rng.int (List.length vs)) in
        if v > 0 then Hashtbl.replace chosen b v
        else if v < 0 then Hashtbl.replace stale_blocks b (-1 - v))
      blocks;
    (* Replay once, taking each chosen block as it stood after its change. *)
    let at = Hashtbl.create 16 in
    Hashtbl.iter
      (fun b v ->
        Hashtbl.replace at v
          (b :: Option.value ~default:[] (Hashtbl.find_opt at v)))
      chosen;
    let taken = Hashtbl.create 16 in
    let block_of buf b =
      let off = b * bs in
      let len = max 0 (min bs (Cstruct.length buf - off)) in
      if len = 0 then Cstruct.empty else copy_cs (Cstruct.sub buf off len)
    in
    (* The page cache: what a failed sync lost is still there. *)
    let live = image_of (Option.value ~default:durable cache) in
    Array.iteri
      (fun k change ->
        if not cached.(k) then apply live change;
        List.iter
          (fun b -> Hashtbl.replace taken b (block_of (bytes_of live) b))
          (Option.value ~default:[] (Hashtbl.find_opt at (k + 1))))
      changes;
    let image = Cstruct.create size in
    if size > 0 then
      for b = 0 to (size - 1) / bs do
        let src =
          match Hashtbl.find_opt taken b with
          | Some blk -> blk
          | None -> (
              match List.assoc_opt b over with
              | Some blk -> blk
              | None -> block_of durable b)
        in
        let len = min (Cstruct.length src) (size - (b * bs)) in
        if len > 0 then Cstruct.blit src 0 image (b * bs) len
      done;
    Hashtbl.iter
      (fun b i ->
        let len = min bs (size - (b * bs)) in
        if len > 0 then
          let dst = Cstruct.sub image (b * bs) len in
          match List.nth_opt (old (b * bs / page)) i with
          | None -> Cstruct.memset dst (Char.code stale)
          | Some (pg, _) ->
              (* Past the old file's end its block held zeros. *)
              Cstruct.memset dst 0;
              let off = b * bs mod page in
              let n = max 0 (min len (Cstruct.length pg - off)) in
              if n > 0 then Cstruct.blit pg off dst 0 n)
      stale_blocks;
    (let source b =
       match Hashtbl.find_opt stale_blocks b with
       | Some i ->
           `Stale (Option.map snd (List.nth_opt (old (b * bs / page)) i))
       | None -> (
           match Hashtbl.find_opt chosen b with
           | Some v -> `Change (v - 1)
           | None when List.mem_assoc b over -> `Over
           | None -> `Durable)
     in
     let runs = ref [] in
     if size > 0 then
       for b = (size - 1) / bs downto 0 do
         let len = min bs (size - (b * bs)) in
         let src = source b in
         match !runs with
         | (off, n, s) :: rest when s = src && off = (b + 1) * bs ->
             runs := (b * bs, n + len, s) :: rest
         | _ -> runs := (b * bs, len, src) :: !runs
       done;
     describe size !runs);
    image

  (* A disk writes a 512-byte sector whole, so the overwrites of one sector
     land together: where a surviving write overwrote bytes the file held, its
     sector holds what the page cache held after it, the earlier overwrites
     of that sector included. *)
  let sectors_together ~cache durable mine kept image =
    let base = Option.value ~default:durable cache in
    let entries = Array.of_list mine in
    (* The sectors each write overwrites, by its place in [mine]. *)
    let _, over =
      Array.fold_left
        (fun (size, acc) (e : entry) ->
          match e.change with
          | Write { offset; data } ->
              let hi = min size (offset + Cstruct.length data) in
              let ss =
                if hi > offset then
                  List.init
                    (((hi - 1) / sector) - (offset / sector) + 1)
                    (fun i -> (offset / sector) + i)
                else []
              in
              (max size (offset + Cstruct.length data), ss :: acc)
          | Resize m -> ((if e.cached then size else m), [] :: acc))
        (Cstruct.length base, [])
        entries
    in
    let over = Array.of_list (List.rev over) in
    let kept_at = Array.map (fun e -> List.memq e kept) entries in
    let patch = Hashtbl.create 4 in
    Array.iteri
      (fun k ss ->
        if kept_at.(k) then
          List.iter
            (fun s ->
              let earlier_lost = ref false in
              for j = 0 to k - 1 do
                if (not kept_at.(j)) && List.mem s over.(j) then
                  earlier_lost := true
              done;
              if !earlier_lost then Hashtbl.replace patch s k)
            ss)
      over;
    if Hashtbl.length patch = 0 then image
    else begin
      let image = copy_cs image in
      Hashtbl.iter
        (fun s k ->
          let live =
            replay_changes base
              (List.filteri (fun j _ -> j <= k) mine
              |> List.filter_map (fun (e : entry) ->
                  if e.cached then None else Some e.change))
          in
          let off = s * sector in
          let n =
            max 0
              (min sector
                 (min (Cstruct.length image) (Cstruct.length live) - off))
          in
          if n > 0 then Cstruct.blit live off image off n)
        patch;
      image
    end

  let do_crash ctl =
    if not ctl.crashed then begin
      if Option.is_none ctl.base then
        invalid_arg "Chaos.Crash: controller was never wrapped";
      ctl.crashed <- true;
      let data_rng =
        match ctl.policy with
        | Reorder_unsynced seed ->
            Some (draw_of_state (Random.State.make [| seed |]))
        | Choose f -> Some (draw_of_fun f)
        | Drop_unsynced | Survive _ -> None
      in
      let bytes = match ctl.policy with Choose _ -> true | _ -> false in
      let contents = Hashtbl.create 16 in
      (* The earliest setuid clear the crash dropped, by inode: a length a
         later change sets lands only with or after it, as ext4 journals both
         in program order. An overwrite within the length may land without
         it. *)
      let clear_cut = Hashtbl.create 4 in
      (* The files the image no longer holds, whose blocks are free, by
         path. *)
      let removed = ref [] and unsynced_removed = ref [] in
      let page_of c p =
        if Cstruct.length c > p * page then
          Some
            (Cstruct.sub c (p * page)
               (min page (Cstruct.length c - (p * page))))
        else None
      in
      (* What a block the page [p] of [fs] may have been given held: a page
         a shrink of [fs] freed, else the first removed file that had one
         there; and the pages a removed file held unsynced, which writeback
         may have put on disk before the removal. *)
      let old fs p =
        (match List.assoc_opt p fs.freed with
          | Some pg -> [ (pg, fs.rel ^ " held there before its truncation") ]
          | None ->
              Option.to_list
                (List.find_map
                   (fun (path, c) ->
                     Option.map
                       (fun pg -> (pg, "the removed " ^ path ^ " held there"))
                       (page_of c p))
                   !removed))
        @ List.filter_map
            (fun (path, durable, live) ->
              match (page_of live p, page_of durable p) with
              | Some pg, Some d when Cstruct.equal pg d -> None
              | Some pg, _ ->
                  Some (pg, "the removed " ^ path ^ " held there unsynced")
              | None, _ -> None)
            !unsynced_removed
      in
      (* What the crash left of each file's unsynced writes. *)
      let left = ref [] in
      let describe (fs : fstate) mine size runs =
        let durable = Cstruct.length (bytes_of fs.durable) in
        let run (off, n, src) =
          let at = Fmt.str "@%d+%d" off n in
          match src with
          | `Durable when off >= durable -> Some (at ^ " zeros")
          | `Durable -> None
          | `Change k -> (
              match (List.nth mine k : entry).change with
              | Write { offset; data } ->
                  Some
                    (Fmt.str "%s as the write @%d+%d left it" at offset
                       (Cstruct.length data))
              | Resize m when off >= m ->
                  Some (Fmt.str "%s zeros, from the truncation to %d" at m)
              | Resize m when m < off + n ->
                  Some
                    (Fmt.str "%s as the truncation to %d left it, zeros from %d"
                       at m m)
              | Resize m ->
                  Some (Fmt.str "%s as the truncation to %d left it" at m))
          | `Over -> Some (at ^ " as a failed sync wrote it")
          | `Stale None -> Some (Fmt.str "%s stale %C" at stale)
          | `Stale (Some from) -> Some (Fmt.str "%s stale (what %s)" at from)
        in
        let parts = List.filter_map run runs in
        let parts =
          match List.length parts with
          | n when n > 8 ->
              List.filteri (fun i _ -> i < 8) parts
              @ [ Fmt.str "and %d more runs" (n - 8) ]
          | _ -> parts
        in
        if parts <> [] || size <> durable then
          left :=
            Fmt.str "%s, %d bytes%s" fs.rel size
              (if parts = [] then "" else ": " ^ String.concat "; " parts)
            :: !left
      in
      let content fs =
        match Hashtbl.find_opt contents fs.id with
        | Some c -> c
        | None ->
            let mine = List.rev fs.pending in
            let cut = Hashtbl.find_opt clear_cut fs.fino in
            let len_upto =
              match cut with
              | None -> max_int
              | Some c ->
                  let rec count k = function
                    | (e : entry) :: rest when e.seq < c -> count (k + 1) rest
                    | _ -> k
                  in
                  count 0 mine
            in
            (* Under a scripted policy, a change past the cut that would set
               the length does not survive. *)
            let keep_length survivors =
              match cut with
              | None -> survivors
              | Some c ->
                  let _, kept =
                    List.fold_left
                      (fun (len, acc) (e : entry) ->
                        let len' =
                          match e.change with
                          | Write { offset; data } ->
                              max len (offset + Cstruct.length data)
                          | Resize m -> m
                        in
                        if e.seq >= c && len' <> len then (len, acc)
                        else (len', e :: acc))
                      (Cstruct.length (bytes_of fs.durable), [])
                      survivors
                  in
                  List.rev kept
            in
            let c =
              match (ctl.policy, data_rng) with
              | (Reorder_unsynced _ | Choose _), Some rng ->
                  let changes = List.map (fun e -> (e.change, e.cached)) mine in
                  (* A writeback that failed only partly wrote any of the
                     blocks it lost. *)
                  let over =
                    List.sort_uniq Int.compare
                      (List.map (fun (b, _, _) -> b) fs.partial)
                    |> List.filter_map (fun b ->
                        (* Any failed writeback's version of the block, or
                           none. *)
                        let vs =
                          List.filter_map
                            (fun (b', _, blk) ->
                              if b' = b then Some blk else None)
                            fs.partial
                        in
                        match rng.int (List.length vs + 1) with
                        | 0 -> None
                        | i -> Some (b, List.nth vs (i - 1)))
                  in
                  (* The page cache kept what a failed sync lost, or evicted
                     it, whether a later sync ran or not. *)
                  if
                    (Option.is_some fs.cache || Option.is_some fs.evicted)
                    && rng.bool ()
                  then
                    writeback ~over ~len_upto ~bytes ~held:fs.held ~old:(old fs)
                      ~describe:(describe fs mine) ~rng ~bs:ctl.block_size
                      ~cache:None
                      (Option.value ~default:(bytes_of fs.durable) fs.evicted)
                      (List.map (fun (c, _) -> (c, false)) changes)
                  else
                    writeback ~over ~len_upto ~bytes ~held:fs.held ~old:(old fs)
                      ~describe:(describe fs mine) ~rng ~bs:ctl.block_size
                      ~cache:fs.cache (bytes_of fs.durable) changes
              | Survive pred, _ ->
                  let kept =
                    List.filter (fun e -> pred (to_unsynced e)) mine
                    |> keep_length
                  in
                  List.map (fun e -> (e.change, e.cached)) kept
                  |> replay_cached ~bs:ctl.block_size ~cache:fs.cache
                       (bytes_of fs.durable)
                  |> fst
                  |> sectors_together ~cache:fs.cache (bytes_of fs.durable) mine
                       kept
              | _ -> copy_cs (bytes_of fs.durable)
            in
            Hashtbl.replace contents fs.id c;
            c
      in
      let journal =
        List.rev (Ids.fold (fun _ e acc -> e :: acc) ctl.journal [])
      in
      let pending = List.filter (fun e -> not e.committed) journal in
      let rec take_prefix pred = function
        | x :: xs when pred x -> x :: take_prefix pred xs
        | _ -> []
      in
      let survivors =
        match ctl.policy with
        | Drop_unsynced -> []
        | Survive pred -> take_prefix (fun e -> pred e.meta) pending
        | Reorder_unsynced seed ->
            (* Arbitrary salt for the reorder RNG. Written as a shift so the
               32-bit "META" value (0x4d455441) is not an int literal that
               overflows a 31-bit int (js_of_ocaml, wasm_of_ocaml); the 64-bit
               value, and so the fault-injection sequence, is unchanged. *)
            let salt = (0x4d45 lsl 16) lor 0x5441 in
            let rng = Random.State.make [| seed; salt |] in
            (* A uniform length: late crash points are as likely as early
               ones. *)
            let n = Random.State.int rng (List.length pending + 1) in
            List.filteri (fun i _ -> i < n) pending
        | Choose f ->
            let n = f (List.length pending + 1) in
            List.filteri (fun i _ -> i < n) pending
      in
      let survives e = List.memq e survivors in
      let owner, root_path = root ctl in
      (* The snapshot with, in program order, every committed entry, every
         surviving one, and every one a barrier reached that holds in the
         image by then, with the entries it needs, as [settle] commits it: a
         deferred barrier holds once its directory does, wherever the cut
         fell. An entry pulled in that way may come before entries already
         replayed, so the replay starts again with it. *)
      let included = Hashtbl.create 16 in
      let { tree; locs; _ } =
        let snapshot =
          match ctl.snapshot with Some st -> st | None -> assert false
        in
        List.iter
          (fun e ->
            if e.committed || survives e then
              Hashtbl.replace included e.meta.seq ())
          journal;
        let rec build () =
          let again = ref false in
          let st =
            List.fold_left
              (fun st e ->
                if !again then st
                else if Hashtbl.mem included e.meta.seq then replay_in ctl st e
                else if reached st e then begin
                  let clears =
                    Ids.fold
                      (fun s c acc -> if s < e.meta.seq then c :: acc else acc)
                      ctl.clears []
                  in
                  List.iter
                    (fun d ->
                      if not (Hashtbl.mem included d.meta.seq) then begin
                        Hashtbl.replace included d.meta.seq ();
                        if d != e then again := true
                      end)
                    (clears @ closure ctl e);
                  replay_in ctl st e
                end
                else st)
              snapshot journal
          in
          if !again then build () else st
        in
        build ()
      in
      List.iter
        (fun e ->
          match e.attrs_ino with
          | Some ino when e.clear && not (Hashtbl.mem included e.meta.seq) -> (
              (* The write that cleared it was logged just before it. *)
              let at = e.meta.seq - 1 in
              match Hashtbl.find_opt clear_cut ino with
              | Some s when s <= at -> ()
              | Some _ | None -> Hashtbl.replace clear_cut ino at)
          | Some _ | None -> ())
        journal;
      let tree =
        match tree with
        | Dir d ->
            Dir
              {
                d with
                entries = Paths.filter (fun k _ -> not (is_limbo k)) d.entries;
              }
        | t -> t
      in
      (let inos = index [] tree Inos.empty in
       let located = Hashtbl.create 8 in
       Paths.iter
         (fun _ ls ->
           List.iter (fun (fs, _, _) -> Hashtbl.replace located fs.id ()) ls)
         locs.at;
       (* A file the program still names keeps its blocks, even where the
          cut lost its creation: only a removal frees them. *)
       let live = Hashtbl.create 8 in
       Hashtbl.iter
         (fun _ (fs : fstate) -> Hashtbl.replace live fs.id ())
         ctl.files;
       let kept id = Hashtbl.mem located id || Hashtbl.mem live id in
       let gone_files =
         Hashtbl.fold
           (fun id (fs : fstate) acc ->
             if kept id then acc else (fs.rel, bytes_of fs.durable) :: acc)
           ctl.known []
       and gone_nodes =
         Inos.fold
           (fun ino (path, data) acc ->
             if Inos.mem ino inos then acc else (path, data) :: acc)
           ctl.snapped []
       in
       removed := gone_files @ gone_nodes;
       unsynced_removed :=
         Hashtbl.fold
           (fun id (fs : fstate) acc ->
             if kept id || fs.pending = [] then acc
             else
               let durable = bytes_of fs.durable in
               let pending =
                 List.rev_map (fun e -> (e.change, e.cached)) fs.pending
               in
               let _, live =
                 replay_cached ~bs:ctl.block_size durable ~cache:fs.cache
                   pending
               in
               (fs.rel, durable, live) :: acc)
           ctl.known []);
      restore_root owner root_path tree;
      (* Each tracked file is rewritten where the image holds it, which a
         rename may have made another name than the one it was met at. *)
      let rel_of key =
        if String.equal root_path "" then key
        else if String.ends_with ~suffix:"/" root_path then root_path ^ key
        else root_path ^ "/" ^ key
      in
      let proot = Option.get ctl.root_box in
      Paths.iter
        (fun key ls ->
          if not (in_limbo (path_segments key)) then
            let rel = rel_of key in
            List.iter
              (fun (fs, _, _) ->
                let content = content fs in
                let (Base_dir ((module B), bv)) = owner in
                match B.stat bv ~follow:false rel with
                | { kind = `Regular_file; _ } ->
                    fs.owner <- proot;
                    fs.rel <- rel;
                    rewrite ctl fs content
                | _ | (exception Eio.Io _) -> ())
              ls)
        locs.at;
      ctl.left <- List.rev !left;
      ctl.image <-
        fold_nodes
          (fun segs node acc ->
            match segs with
            | [] -> acc
            | _ ->
                let key = String.concat "/" segs in
                let kind =
                  match node with
                  | Dir _ -> `Dir
                  | Link l -> `Link l.target
                  | File f -> (
                      match Paths.find_opt key locs.at with
                      | Some ((fs, _, _) :: _) ->
                          `File (Cstruct.to_string (content fs))
                      | Some [] | None -> `File (Cstruct.to_string f.data))
                in
                (key, ino_of node, kind) :: acc)
          [] tree []
    end

  (* Advance the durability op counter and fire the configured auto-trigger. *)
  let bump ctl fs ~kind ~offset ~len =
    ctl.ops <- ctl.ops + 1;
    let fire =
      match ctl.trigger with
      | Manual -> false
      | Nth_op n -> ctl.ops >= n
      (* A write records no journal entry, so the entry [After_dir_op] names
         cannot be this op. *)
      | After_dir_op _ -> false
      | After_write { path; file_offset } ->
          (match kind with `Write -> true | `Sync -> false)
          && String.equal path fs.rel && offset <= file_offset
          && file_offset < offset + len
    in
    if fire && not ctl.crashed then do_crash ctl

  (* [After_dir_op] fires here -- after [record_metadata], before anything can
     commit the entry -- inside the window a directory operation with no barrier
     leaves open. Kind and path are matched as the journal records them. *)
  let bump_metadata ctl ~path ~kind =
    ctl.ops <- ctl.ops + 1;
    let fire =
      match ctl.trigger with
      | Manual | After_write _ -> false
      | Nth_op n -> ctl.ops >= n
      | After_dir_op { kind = k; path = p } ->
          equal_op_kind k kind && String.equal p path
    in
    if fire && not ctl.crashed then do_crash ctl

  (* One record backs both file wrappers. [cfs] is present exactly for a
     read-write file (the tracked durability state); reads work on either. *)
  type cfile = {
    ctl : t;
    cfs : fstate option;
    cro : Eio.File.ro_ty r;
    crw : Eio.File.rw_ty r option;
    cappend : bool;
    cpos : int ref;
  }

  let guard cf = if cf.ctl.crashed then raise_power_lost ()

  (* fsyncgate: a failed fsync(2) marks the file's dirty pages clean without
     writing them, so they are lost whatever a later sync reports. *)
  let lose ctl fs =
    ctl.pending_changes <- ctl.pending_changes - List.length fs.pending;
    fs.pending <- [];
    fs.gen <- fs.gen + 1;
    (* Compact once stale entries outnumber pending ones. *)
    if ctl.logged > (2 * ctl.pending_changes) + 64 then begin
      ctl.log <- List.filter is_pending ctl.log;
      ctl.logged <- ctl.pending_changes
    end

  (* A failed barrier loses the pending writes, which the page cache keeps:
     a later change that dirties one of their blocks writes it whole. *)
  let fail_sync ctl fs =
    ctl.effects <- ctl.effects + 1;
    let cache = Option.value ~default:(bytes_of fs.durable) fs.cache in
    let pending = List.rev fs.pending in
    (* A kept truncation is in the page cache already. *)
    let changes =
      List.filter_map
        (fun e -> if e.cached then None else Some e.change)
        pending
    in
    let live = replay_changes cache changes in
    (* Had an earlier failure's pages been evicted, the changes, kept
       truncations included, landed over the bytes re-read from disk. *)
    let evicted =
      match (fs.cache, fs.evicted) with
      | None, None -> None
      | _, e ->
          Some
            (replay_changes
               (Option.value ~default:(bytes_of fs.durable) e)
               (List.map (fun e -> e.change) pending))
    in
    fs.cache <- Some live;
    (* The pages that failed are clean and unwritten; those that did not
       may be on disk, as they stood. *)
    let bs = ctl.block_size in
    let written =
      dirty_blocks ~writes:true ~bs (Cstruct.length cache) changes
    in
    let version src b =
      let off = b * bs in
      let n = min bs (Cstruct.length src - off) in
      (* A page cut away by then is written no more; the tail of the last
         one past the end is zero. *)
      if n <= 0 then None else Some (b, ctl.seq, copy_cs (Cstruct.sub src off n))
    in
    let versions b =
      let v = version live b in
      match Option.bind evicted (fun e -> version e b) with
      | Some (_, _, e) as w
        when not
               (Option.fold ~none:false
                  ~some:(fun (_, _, l) -> Cstruct.equal l e)
                  v) ->
          Option.to_list v @ Option.to_list w
      | _ -> Option.to_list v
    in
    fs.partial <- List.concat_map versions written @ fs.partial;
    lose ctl fs;
    (* A truncation is metadata, journaled apart from the data: the failure
       does not lose it, and it stays pending in program order. *)
    List.iter
      (fun e ->
        match e.change with
        | Resize _ ->
            let e = { e with egen = fs.gen; cached = true } in
            ctl.log <- e :: ctl.log;
            fs.pending <- e :: fs.pending;
            ctl.logged <- ctl.logged + 1;
            ctl.pending_changes <- ctl.pending_changes + 1
        | Write _ -> ())
      pending

  (* A barrier makes the file's pending changes durable, replayed over its
     durable bytes, with the lost bytes in the blocks they dirty: a change
     lost to a failed barrier, or made around the wrapper, never becomes
     durable by itself. *)
  let barrier ctl fs ~stat sync =
    match sync () with
    | () ->
        if
          fs.pending <> [] || fs.partial <> [] || Option.is_some fs.cache
          || Option.is_some fs.evicted
        then ctl.effects <- ctl.effects + 1;
        let pending = List.rev_map (fun e -> (e.change, e.cached)) fs.pending in
        (* A block written now is no longer one a failed sync may have
           left. *)
        (match fs.partial with
        | [] -> ()
        | partial ->
            let cache = Option.value ~default:(bytes_of fs.durable) fs.cache in
            let written =
              dirty_blocks ~bs:ctl.block_size (Cstruct.length cache)
                (List.filter_map
                   (fun (c, cached) -> if cached then None else Some c)
                   pending)
            in
            (* A shrink made durable now frees the blocks past it that a
               failure captured before it, even on a block boundary, where
               it dirties nothing. *)
            let shrinks =
              List.filter_map
                (fun e ->
                  match e.change with
                  | Resize m -> Some (e.seq, m)
                  | Write _ -> None)
                fs.pending
            in
            fs.partial <-
              List.filter
                (fun (b, at, _) ->
                  (not (List.mem b written))
                  && not
                       (List.exists
                          (fun (seq, m) -> seq > at && b * ctl.block_size >= m)
                          shrinks))
                partial);
        let before = copy_cs (bytes_of fs.durable) and held = fs.held in
        (match (fs.cache, fs.evicted) with
        | None, None -> List.iter (fun (c, _) -> apply fs.durable c) pending
        | cache, evicted ->
            let c, live =
              replay_cached ~bs:ctl.block_size ~cache (bytes_of fs.durable)
                pending
            in
            (* Had the lost pages been evicted, a later write lands over the
               bytes on disk. *)
            let e =
              replay_changes
                (Option.value ~default:(bytes_of fs.durable) evicted)
                (List.map fst pending)
            in
            fs.durable.buf <- c;
            fs.durable.len <- Cstruct.length c;
            fs.cache <- (if Cstruct.equal c live then None else Some live);
            fs.evicted <- (if Cstruct.equal c e then None else Some e));
        fs.held <- List.fold_left (fun h (c, _) -> hold h c) fs.held pending;
        (* The pages a shrink freed, as the disk held them. *)
        List.iter
          (fun (x, y) ->
            for p = x / page to (y - 1) / page do
              let off = p * page in
              if
                (not (held_at fs.held off (off + 1)))
                && off < Cstruct.length before
              then
                fs.freed <-
                  ( p,
                    copy_cs
                      (Cstruct.sub before off
                         (min page (Cstruct.length before - off))) )
                  :: List.remove_assoc p fs.freed
            done)
          held;
        lose ctl fs;
        let st = stat () in
        commit_attrs ctl ~ino:(fun () -> st.Eio.File.Stat.ino);
        bump ctl fs ~kind:`Sync ~offset:0 ~len:0
    | exception (Eio.Io _ as e) ->
        fail_sync ctl fs;
        raise e

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
    let seek cf off cmd = do_seek ~pos:cf.cpos cf.cro off cmd
    let close cf = close_resource cf.cro
  end

  (* fsync(2) flushes the file, whichever descriptor names it, so a barrier
     through a read-only handle commits the file as one through a read-write
     handle does. A file the model does not track was never written through
     the wrapper and is durable already. *)
  let file_ino cf () = (Eio.File.stat cf.cro).ino

  let sync_ro cf fs =
    guard cf;
    match fs with
    | None -> commit_attrs cf.ctl ~ino:(file_ino cf)
    | Some fs ->
        barrier cf.ctl fs
          ~stat:(fun () -> Eio.File.stat cf.cro)
          (fun () -> sync_base cf.cro)

  (* A directory opened as a file syncs as that directory's barrier, on the
     inode it opened. *)
  let wrap_cfile_ro ?fs ?dir ctl base : Eio.File.ro_ty r =
    let cf =
      { ctl; cfs = None; cro = base; crw = None; cappend = false; cpos = ref 0 }
    in
    let sync cf =
      match dir with
      | Some dir ->
          guard cf;
          commit_dir_ino ctl dir
      | None -> sync_ro cf fs
    in
    Eio.Resource.T
      ( cf,
        Eio.Resource.handler
          (Eio.Resource.bindings (Eio.File.Pi.ro (module Cro_impl))
          @ [
              Eio.Resource.H (Eio_fs_sync.File_sync, sync);
              Eio.Resource.H
                (Sync_failed, fun cf -> Option.iter (fail_sync cf.ctl) fs);
            ]) )

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
    let seek cf off cmd = do_seek ~pos:cf.cpos cf.cro off cmd
    let close cf = close_resource cf.cro

    (* A write that cleared setuid or setgid, as a user's does. *)
    let note_clear cf fs =
      if fs.mode land 0o6000 <> 0 then begin
        let st = Eio.File.stat cf.cro in
        if st.perm <> fs.mode then begin
          fs.mode <- st.perm;
          record_clear cf.ctl fs st
        end
      end

    let single_write cf bufs =
      guard cf;
      let fs = fstate_of cf in
      let off = write_offset ~append:cf.cappend ~pos:cf.cpos cf.cro in
      let got = Eio.Flow.single_write (writable cf) bufs in
      clog_write cf.ctl fs ~offset:off (capture bufs got);
      note_clear cf fs;
      cf.cpos := off + got;
      bump cf.ctl fs ~kind:`Write ~offset:off ~len:got;
      got

    let copy cf ~src = Eio.Flow.Pi.simple_copy ~single_write cf ~src

    let pwrite cf ~file_offset bufs =
      guard cf;
      let fs = fstate_of cf in
      let off =
        if cf.cappend then write_offset ~append:true ~pos:cf.cpos cf.cro
        else Optint.Int63.to_int file_offset
      in
      let got = Eio.File.pwrite_single (writable cf) ~file_offset bufs in
      clog_write cf.ctl fs ~offset:off (capture bufs got);
      note_clear cf fs;
      bump cf.ctl fs ~kind:`Write ~offset:off ~len:got;
      got

    (* The sync barrier: the live content becomes durable and the file's pending
       changes leave the log, so a later crash can neither drop nor reorder
       them. It commits this file's data and nothing else -- the directory
       entries a name lives in are the metadata journal's business, committed by
       [sync_metadata] or a registered [Dir_sync], as fsync(2) divides them. *)
    let sync cf =
      guard cf;
      let fs = fstate_of cf in
      barrier cf.ctl fs
        ~stat:(fun () -> Eio.File.stat cf.cro)
        (fun () -> Eio.File.sync (writable cf))

    let truncate cf len =
      guard cf;
      let fs = fstate_of cf in
      Eio.File.truncate (writable cf) len;
      clog_change cf.ctl fs (Resize (Optint.Int63.to_int len));
      note_clear cf fs;
      bump cf.ctl fs ~kind:`Write ~offset:0 ~len:0
  end

  let wrap_cfile_rw ctl fs ~append base : Eio.File.rw_ty r =
    let state =
      {
        ctl;
        cfs = Some fs;
        cro = (base :> Eio.File.ro_ty r);
        crw = Some base;
        cappend = append;
        cpos = ref 0;
      }
    in
    rw_boxed
      ~extra:[ Eio.Resource.H (Sync_failed, fun cf -> fail_sync cf.ctl fs) ]
      (module Crw_impl)
      state ~base

  (* [dplace] is where this directory sits under the boxed directory the
     controller was wrapped over: tracked files are addressed through that
     long-lived root so a crash can rewrite them after any intermediate staging
     subtree is closed, and a subtree held across the rename of its directory
     follows it there, as the descriptor beneath it does. *)
  type cdir = {
    dctl : t;
    dbase : Eio.Fs.dir_ty r;
    dboxed : boxed;
    dplace : place;
    dino : int64 option; (* the inode a subtree opened *)
  }

  (* Expose the whole crash directory so a rename whose target is itself a
     crash directory reaches both its base (for the backend call) and its
     root-relative location (to rehome durability state). *)
  type (_, _, _) Eio.Resource.pi +=
    | Crash_dir : ('t, 't -> cdir, [> `Crash_dir ]) Eio.Resource.pi

  let crash_dir_hook :
      (t ->
      ino:int64 option ->
      root:boxed ->
      prefix:string ->
      base:Eio.Fs.dir_ty r ->
      [ `Close | Eio.Fs.dir_ty ] r)
      ref =
    ref (fun _ ~ino:_ ~root:_ ~prefix:_ ~base:_ -> assert false)

  let is_trunc = function
    | `Or_truncate _ | `Exclusive _ -> true
    | `Never | `If_missing _ -> false

  let create_perm = function
    | `Never -> 0o644
    | `If_missing p | `Or_truncate p | `Exclusive p -> p

  (* Compose a path opened through a subtree with the subtree's own path from
     the wrap root. *)
  let at prefix path = Eio_fs_path.append prefix path

  (* One key per file, whatever spelling [path] is. *)
  let file_key (Base_dir ((module B), bv)) path =
    match B.native bv path with
    | Some native -> native
    | None -> path_key path
    | exception Eio.Io _ -> path_key path

  let path_is_at_or_under ~parent path =
    String.equal parent path
    ||
    let n = String.length parent in
    String.length path > n
    && String.equal (String.sub path 0 n) parent
    && path.[n] = '/'

  let track ctl fs =
    Hashtbl.replace ctl.files (file_key fs.owner fs.rel) fs;
    Hashtbl.replace ctl.known fs.id fs;
    ctl.by_rel <-
      Paths.update fs.rel
        (fun o -> Some (fs :: Option.value ~default:[] o))
        ctl.by_rel

  let untrack ctl fs =
    let key = file_key fs.owner fs.rel in
    (match Hashtbl.find_opt ctl.files key with
    | Some fs' when fs' == fs -> Hashtbl.remove ctl.files key
    | Some _ | None -> ());
    ctl.by_rel <-
      Paths.update fs.rel
        (function
          | None -> None
          | Some l -> (
              match List.filter (fun f -> f != fs) l with
              | [] -> None
              | l -> Some l))
        ctl.by_rel

  let move_state ctl ~old_owner ~old_rel ~new_owner ~new_rel =
    let mine owner = List.filter (fun fs -> fs.owner == owner) in
    let moved = mine old_owner (files_under ctl old_rel) in
    let replaced =
      mine new_owner (files_under ctl new_rel)
      |> List.filter (fun fs -> not (List.memq fs moved))
    in
    (* A replaced file leaves the name table but keeps its pending writes: a
       handle may still sync them, and a crash that loses the rename brings
       the file back under its name. *)
    List.iter (untrack ctl) replaced;
    List.iter
      (fun fs ->
        untrack ctl fs;
        let suffix =
          String.sub fs.rel (String.length old_rel)
            (String.length fs.rel - String.length old_rel)
        in
        fs.owner <- new_owner;
        fs.rel <- new_rel ^ suffix;
        track ctl fs)
      moved;
    List.iter
      (fun place ->
        if
          place.proot == old_owner
          && path_is_at_or_under ~parent:old_rel place.prefix
        then begin
          let suffix =
            String.sub place.prefix (String.length old_rel)
              (String.length place.prefix - String.length old_rel)
          in
          place.proot <- new_owner;
          place.prefix <- new_rel ^ suffix
        end)
      ctl.subtrees

  (* After the crash nothing reaches the store: a killed process does nothing
     more, so every operation through the wrapper fails, a directory operation
     as a read or a write does. *)
  let live d = if d.dctl.crashed then raise_power_lost ()

  (* The inode at [p] before an operation takes it away. *)
  let gone d p =
    match ino_at (as_admin d.dctl d.dplace.proot) p with
    | Some ino -> [ (p, ino) ]
    | None -> []

  (* A path from [d]'s root, resolved below the wrapped root. *)
  let resolve ?follow_last d path =
    match d.dctl.base with
    | None -> path
    | Some (_, root) -> resolve_at ?follow_last ~root d.dplace.proot path

  module Cdir_impl = struct
    type t = cdir

    let open_in d ~sw ~follow path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let rel = resolve d (at d.dplace.prefix path) in
      let fs = Hashtbl.find_opt d.dctl.files (file_key d.dplace.proot rel) in
      let file = B.open_in bv ~sw ~follow path in
      let dir =
        match Eio.File.stat file with
        | { kind = `Directory; ino; _ } -> Some ino
        | _ -> None
      in
      wrap_cfile_ro ?fs ?dir d.dctl file

    let open_out d ~sw ~follow ~append ~create path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let spelt = at d.dplace.prefix path in
      let rel = resolve d spelt in
      let key = file_key d.dplace.proot rel in
      (* A dangling link names a file the open creates. *)
      let found =
        match B.stat bv ~follow:true path with
        | stat -> Some stat
        | exception Eio.Io _ -> None
      in
      let existed = Option.is_some found in
      (* A file's bytes are read once, when the model first meets it, before
         a truncating open loses them. *)
      let known = Hashtbl.mem d.dctl.files key in
      let prev =
        if known then Cstruct.empty
        else read_existing (as_admin d.dctl d.dplace.proot) rel
      in
      let had_bytes =
        match found with
        | Some { size; _ } -> Optint.Int63.compare size Optint.Int63.zero > 0
        | None -> false
      in
      let base = B.open_out bv ~sw ~follow ~append ~create path in
      let perm = create_perm create in
      let fs =
        match Hashtbl.find_opt d.dctl.files key with
        | Some fs ->
            if fs.perm <> perm then d.dctl.effects <- d.dctl.effects + 1;
            fs.perm <- perm;
            fs
        | None ->
            d.dctl.effects <- d.dctl.effects + 1;
            let fs =
              fresh_file
                ?held:(if known then None else d.dctl.held rel)
                ~owner:d.dplace.proot ~rel ~perm ~durable:prev ()
            in
            track d.dctl fs;
            Option.iter
              (fun (st : Eio.File.Stat.t) ->
                locate_preexisting d.dctl (fs, d.dplace.proot, rel) ~ino:st.ino)
              found;
            fs
      in
      let mode =
        match found with Some st -> st.perm | None -> create_perm create
      in
      if fs.mode <> mode then d.dctl.effects <- d.dctl.effects + 1;
      fs.mode <- mode;
      (* An unsynced truncation of pre-existing durable data is itself a change
         that a crash may lose. *)
      if is_trunc create && had_bytes then clog_change d.dctl fs (Resize 0);
      if not existed then begin
        record_metadata d.dctl ~path:spelt ~kind:Create ~resolved:[ rel ];
        bump_metadata d.dctl ~path:spelt ~kind:Create
      end;
      wrap_cfile_rw d.dctl fs ~append base

    let open_subtree d ~sw path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let sub = B.open_subtree bv ~sw path in
      let ino = (B.stat bv ~follow:true path).ino in
      !crash_dir_hook d.dctl ~ino:(Some ino) ~root:d.dplace.proot
        ~prefix:(resolve d (at d.dplace.prefix path))
        ~base:(sub :> Eio.Fs.dir_ty r)

    let stat d ~follow path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.stat bv ~follow path

    let mkdir d ~perm path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.mkdir bv ~perm path;
      let rel = at d.dplace.prefix path in
      let resolved = [ resolve ~follow_last:false d rel ] in
      record_metadata d.dctl ~path:rel ~kind:Mkdir ~resolved;
      bump_metadata d.dctl ~path:rel ~kind:Mkdir

    let read_dir d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.read_dir bv path

    let with_dir_entries d path fn =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.with_dir_entries bv path fn

    let unlink d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let rel = at d.dplace.prefix path in
      let resolved = resolve ~follow_last:false d rel in
      let gone = gone d resolved in
      B.unlink bv path;
      Option.iter (untrack d.dctl)
        (Hashtbl.find_opt d.dctl.files (file_key d.dplace.proot resolved));
      record_metadata d.dctl ~gone ~path:rel ~kind:Unlink ~resolved:[ resolved ];
      bump_metadata d.dctl ~path:rel ~kind:Unlink

    let rmdir d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let rel = at d.dplace.prefix path in
      let p = resolve ~follow_last:false d rel in
      let gone = gone d p in
      B.rmdir bv path;
      record_metadata d.dctl ~gone ~path:rel ~kind:Rmdir ~resolved:[ p ];
      bump_metadata d.dctl ~path:rel ~kind:Rmdir

    let rename_with exclusive d old_path new_dir new_path =
      live d;
      let (Eio.Resource.T (nv, nops)) = new_dir in
      let target_cdir =
        Option.map (fun f -> f nv) (Eio.Resource.get_opt nops Crash_dir)
      in
      let target =
        match target_cdir with
        | Some cd -> cd.dbase
        | None -> (new_dir :> Eio.Fs.dir_ty r)
      in
      let (Base_dir ((module B), bv)) = d.dboxed in
      let spelt = at d.dplace.prefix old_path in
      let old_rel = resolve ~follow_last:false d spelt in
      let gone = gone d old_rel in
      (* A crash-wrapped target rehomes the file root-relative, so its rewrite
         does not depend on the target subtree staying open. *)
      let owner, dest, rel =
        match target_cdir with
        | Some cd ->
            let dest = at cd.dplace.prefix new_path in
            (cd.dplace.proot, dest, resolve ~follow_last:false cd dest)
        | None -> (box_dir target, new_path, new_path)
      in
      (* The node the rename replaces, if any. *)
      let victim = ino_at (as_admin d.dctl owner) rel in
      (match exclusive with
      | None -> B.rename bv old_path target new_path
      | Some backend ->
          Eio_fs_sync.Rename.exclusive backend (d.dbase, old_path)
            (target, new_path));
      (* A successful rename moves the durability identity with the file. A
         tracked target it replaced keeps its own, for a crash that loses the
         rename. *)
      (* A rename of an entry onto itself does nothing. *)
      if not (owner == d.dplace.proot && String.equal old_rel rel) then begin
        move_state d.dctl ~old_owner:d.dplace.proot ~old_rel ~new_owner:owner
          ~new_rel:rel;
        let kind = Rename { dest } in
        record_metadata d.dctl ~gone ?victim ~path:spelt ~kind
          ~resolved:[ old_rel; rel ];
        bump_metadata d.dctl ~path:spelt ~kind
      end

    let rename d old_path new_dir new_path =
      rename_with None d old_path new_dir new_path

    let rename_exclusive d backend ~source ~target:(new_dir, new_path) =
      rename_with (Some backend) d source new_dir new_path

    let read_link d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.read_link bv path

    let symlink ~link_to d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.symlink ~link_to bv path;
      let rel = at d.dplace.prefix path in
      let kind = Symlink { target = link_to } in
      let resolved = [ resolve ~follow_last:false d rel ] in
      record_metadata d.dctl ~path:rel ~kind ~resolved;
      bump_metadata d.dctl ~path:rel ~kind

    (* A change of mode or owner is journaled as a directory operation is. *)
    let attrs d ~follow path kind =
      let rel = at d.dplace.prefix path in
      let resolved = [ resolve ~follow_last:follow d rel ] in

      record_metadata d.dctl ~path:rel ~kind ~resolved;
      bump_metadata d.dctl ~path:rel ~kind

    let chmod d ~follow ~perm path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.chmod bv ~follow ~perm path;
      (* A tracked file's mode, to tell when a write clears setuid. *)
      (match
         Hashtbl.find_opt d.dctl.files
           (file_key d.dplace.proot
              (resolve ~follow_last:follow d (at d.dplace.prefix path)))
       with
      | Some fs -> fs.mode <- (B.stat bv ~follow path).perm
      | None -> ());
      attrs d ~follow path (Chmod { perm = perm land 0o7777 })

    let chown ~follow ?uid ?gid d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.chown ~follow ?uid ?gid bv path;
      let st = B.stat bv ~follow path in
      attrs d ~follow path (Chown { uid = st.uid; gid = st.gid })

    (* A directory barrier commits the entries of the directory it names,
       exactly as the platform's directory fsync would, and the journal is
       asked root-relative so a barrier issued through a staging subtree still
       names the same directory. Through an opened subtree, a barrier on the
       subtree itself is on the inode it opened, even once removed or
       replaced: it commits that directory's own entries. After the power cut it fails like every other
       operation. *)
    let sync_dir d path =
      if d.dctl.crashed then raise_power_lost ();
      (* The base's barrier first: it refuses a directory that is missing or
         that the caller cannot read. *)
      Eio_fs_sync.sync_dir (d.dbase, path);
      match (d.dino, path_segments path) with
      | Some ino, [] -> commit_dir_ino d.dctl ino
      | _ -> commit_dir d.dctl ~dir:(resolve d (at d.dplace.prefix path))

    let native d path =
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.native bv path

    let pp ppf d =
      let (Base_dir ((module B), bv)) = d.dboxed in
      Fmt.pf ppf "crash(%a)" B.pp bv

    include Eio_utils.Posix_path
  end

  let cdir_handler :
      ( cdir,
        [ `Dir | `Close | `Crash_dir | `Dir_sync | `Process_local ] )
      Eio.Resource.handler =
    Eio.Resource.handler
      [
        H (Eio.Fs.Pi.Dir, (module Cdir_impl));
        H (Eio.Resource.Close, fun d -> close_resource d.dbase);
        H (Crash_dir, fun d -> d);
        H (Eio_fs_sync.Rename.Exclusive, Cdir_impl.rename_exclusive);
        (* Who can open these files is the base's to say: the model tracks
           what is written through it and adds no opener of its own, so a
           caller weighing whether an in-process exclusion is the whole
           exclusion is weighing the storage underneath. *)
        H
          ( Eio_fs_sync.Process_local,
            fun d -> Eio_fs_sync.is_process_local d.dbase );
        H (Eio_fs_sync.Modeless, fun d -> Eio_fs_sync.is_modeless d.dbase);
        H (Eio_fs_sync.Dir_sync, fun d ~path -> Cdir_impl.sync_dir d path);
      ]

  let wrap_cdir ctl ~ino ~root ~prefix ~base : [ `Close | Eio.Fs.dir_ty ] r =
    let place = { proot = root; prefix } in
    ctl.subtrees <- place :: ctl.subtrees;
    (Eio.Resource.T
       ( {
           dctl = ctl;
           dbase = base;
           dboxed = box_dir base;
           dplace = place;
           dino = ino;
         },
         cdir_handler )
      :> [ `Close | Eio.Fs.dir_ty ] r)

  let () =
    crash_dir_hook :=
      fun ctl ~ino ~root ~prefix ~base -> wrap_cdir ctl ~ino ~root ~prefix ~base

  let v ?(policy = Drop_unsynced) ?(trigger = Manual) ?(block_size = 4096) () =
    if block_size <= 0 then invalid_arg "Chaos.Crash.v: block_size <= 0";
    {
      policy;
      trigger;
      block_size;
      base = None;
      files = Hashtbl.create 8;
      known = Hashtbl.create 8;
      log = [];
      journal = Ids.empty;
      pool = Ids.empty;
      by_dir = Hashtbl.create 16;
      made = Hashtbl.create 16;
      by_parent = Hashtbl.create 16;
      by_moved = Hashtbl.create 16;
      clears = Ids.empty;
      by_attrs = Hashtbl.create 16;
      snapshot = None;
      snapped = Inos.empty;
      left = [];
      durable = None;
      by_rel = Paths.empty;
      held = (fun _ -> None);
      root_box = None;
      admin = None;
      subtrees = [];
      seq = 0;
      effects = 0;
      logged = 0;
      pending_changes = 0;
      ops = 0;
      crashed = false;
      image = [];
    }

  let wrap ctl (base, path) =
    (* One controller, one wrapper: a second would reset the durable image
       and split the files' state between the two. *)
    if Option.is_some ctl.base then
      invalid_arg "Chaos.Crash.wrap: controller already wrapped";
    let base = (base :> Eio.Fs.dir_ty r) in
    ctl.base <- Some (base, path);
    let proot = box_dir base in
    let admin =
      match Eio_fs_internal.admin base with
      | Some admin -> box_dir admin
      | None -> proot
    in
    ctl.root_box <- Some proot;
    ctl.admin <- Some admin;
    ctl.held <- (fun rel -> Memfs.held (base, rel));
    snap ctl (state_of (snapshot_at admin path) no_locs);
    (wrap_cdir ctl ~ino:None ~root:proot ~prefix:"" ~base, path)

  let crash ctl = do_crash ctl
  let is_crashed ctl = ctl.crashed

  let sync_metadata ctl =
    if ctl.crashed then raise_power_lost ();
    commit_metadata ctl

  let post_crash ctl =
    do_crash ctl;
    match ctl.base with
    | Some b -> b
    | None -> invalid_arg "Chaos.Crash.post_crash: controller was never wrapped"

  let left ctl = ctl.left
  let seq ctl = ctl.seq
  let stamp ctl = ctl.seq + ctl.effects

  let pp_unsynced ppf u =
    match u.kind with
    | Data -> Fmt.pf ppf "%s @%d+%d #%d" u.path u.offset u.length u.seq
    | Truncate -> Fmt.pf ppf "truncate %s to %d #%d" u.path u.offset u.seq
    | Create -> Fmt.pf ppf "create %s #%d" u.path u.seq
    | Mkdir -> Fmt.pf ppf "mkdir %s #%d" u.path u.seq
    | Unlink -> Fmt.pf ppf "unlink %s #%d" u.path u.seq
    | Rmdir -> Fmt.pf ppf "rmdir %s #%d" u.path u.seq
    | Symlink { target } ->
        Fmt.pf ppf "symlink %s -> %s #%d" u.path target u.seq
    | Rename { dest } -> Fmt.pf ppf "rename %s -> %s #%d" u.path dest u.seq
    | Chmod { perm } -> Fmt.pf ppf "chmod %s %o #%d" u.path perm u.seq
    | Chown { uid; gid } ->
        Fmt.pf ppf "chown %s %Ld:%Ld #%d" u.path uid gid u.seq
end

module Internal = struct
  let next = Schedule.next
  let replay_copied () = Atomic.get Crash.copied
  let crash_image (ctl : Crash.t) = List.rev ctl.image
end
