(* An adversarial [Eio.Fs.dir] wrapper for fault-injection testing. It composes
   with any base directory capability (the in-memory backend or a real one) and
   perturbs reads, writes, syncs, opens, stats and directory reads according to a
   schedule that is
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
    | `Read_error
    | `Short_write
    | `Torn_write
    | `Write_error
    | `Sync_error
    | `Open_error
    | `Stat_error
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
    | Stat_error x, Stat_error y ->
        x = y
    | Swap_symlink x, Swap_symlink y -> String.equal x y
    | _ -> false

  let pp ppf = function
    | Short_read n -> Fmt.pf ppf "short-read(%d)" n
    | Read_error c -> Fmt.pf ppf "read-error(%s)" (code_name c)
    | Short_write n -> Fmt.pf ppf "short-write(%d)" n
    | Torn_write n -> Fmt.pf ppf "torn-write(%d)" n
    | Write_error c -> Fmt.pf ppf "write-error(%s)" (code_name c)
    | Sync_error c -> Fmt.pf ppf "sync-error(%s)" (code_name c)
    | Open_error c -> Fmt.pf ppf "open-error(%s)" (code_name c)
    | Stat_error c -> Fmt.pf ppf "stat-error(%s)" (code_name c)
    | Swap_symlink t -> Fmt.pf ppf "swap-symlink(%s)" t
end

module Op = struct
  type kind = [ `Read | `Read_dir | `Write | `Sync | `Open | `Stat ]

  let equal_kind (a : kind) (b : kind) = a = b

  type t = {
    kind : kind;
    path : string;
    file_offset : int;  (** byte offset for reads/writes, [0] otherwise *)
    length : int;  (** bytes requested/offered, [0] otherwise *)
  }

  let pp_kind ppf = function
    | `Read -> Fmt.string ppf "read"
    | `Read_dir -> Fmt.string ppf "read-dir"
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

  let read_dir path = { kind = `Read_dir; path; file_offset = 0; length = 0 }
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
    | Observed of { f : Op.t -> unit; inner : t }

  let empty = Empty
  let scripted rules = Scripted { rules; n = 0 }
  let observing f t = Observed { f; inner = t }

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
    | `Read, (`Short_read | `Read_error) -> true
    | `Read_dir, `Read_error -> true
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
    | `Read_error -> Read_error `Eio
    | `Short_write -> Short_write (split_count rng op.length)
    | `Torn_write -> Torn_write (split_count rng op.length)
    | `Write_error -> Write_error `Enospc
    | `Sync_error -> Sync_error `Eio
    | `Open_error -> Open_error (some_code rng)
    | `Stat_error -> Stat_error (some_code rng)
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

  let rec pp ppf = function
    | Empty -> Fmt.string ppf "empty"
    | Scripted s -> Fmt.pf ppf "scripted(%d rules)" (List.length s.rules)
    | Seeded s -> Fmt.pf ppf "seeded(seed=%d p=%g)" s.seed s.config.probability
    | Observed { inner; _ } -> Fmt.pf ppf "observed(%a)" pp inner
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

(* {2 What a wrapped file carries over}

   A wrapped file keeps the base's value with the wrapper's read, write, sync
   and close in front, so what stays reachable is what the base offers and the
   wrapper has no answer for -- over a real directory the Unix descriptor, over
   the in-memory backend nothing. A carried descriptor is not a second way past
   the wrapper, so the wrapper also registers [Eio_fs_sync.File_sync], bound to
   its own sync. A caller that wants the platform's strongest file barrier asks
   that registration first and only reaches for the descriptor when a backend
   declares none, which is how a barrier issued through [Eio_fs_sync.sync_file]
   reaches the wrapper rather than going around it.

   Only a file opened read-write carries anything over. [Eio.File.Pi.rw]
   implements every interface that reads, writes, syncs or closes the file, so
   none of the base's own stays reachable behind it; [Eio.File.Pi.ro]
   implements no write, so the base's write would stay reachable behind a
   read-only wrapper, and a file opened for reading is re-boxed whole
   instead. *)
let rw_over (type a s) (module X : Eio.File.Pi.WRITE with type t = s)
    (state : s) (bv : a) (bops : (a, _) Eio.Resource.handler) : Eio.File.rw_ty r
    =
  let module Over = struct
    type t = a

    let read_methods = []
    let single_read _ buf = X.single_read state buf
    let pread _ ~file_offset bufs = X.pread state ~file_offset bufs
    let stat _ = X.stat state
    let seek _ off cmd = X.seek state off cmd
    let close _ = X.close state
    let single_write _ bufs = X.single_write state bufs
    let copy _ ~src = X.copy state ~src
    let pwrite _ ~file_offset bufs = X.pwrite state ~file_offset bufs
    let sync _ = X.sync state
    let truncate _ len = X.truncate state len
  end in
  Eio.Resource.T
    ( bv,
      Eio.Resource.handler
        (Eio.Resource.bindings (Eio.File.Pi.rw (module Over))
        @ [ Eio.Resource.H (Eio_fs_sync.File_sync, fun _ -> X.sync state) ]
        @ Eio.Resource.bindings bops) )

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
  let stat t = do_stat t.sched t.path t.ro
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

let wrap_rw sched ~path base : Eio.File.rw_ty r =
  let state =
    {
      sched;
      path;
      pos = ref 0;
      ro = (base :> Eio.File.ro_ty r);
      rw = Some base;
    }
  in
  let (Eio.Resource.T (bv, bops)) = base in
  rw_over (module Rw_impl) state bv bops

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

let base_dir (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops Chaos_base with
  | None -> None
  | Some get -> Some (get t)

let wrap_dir_hook :
    (base:Eio.Fs.dir_ty r -> sched:Schedule.t -> [ `Close | Eio.Fs.dir_ty ] r)
    ref =
  ref (fun ~base:_ ~sched:_ -> assert false)

(* Free the name at [path], whatever holds it. A directory is emptied first and
   removed with [rmdir]; anything else is unlinked. Both wrappers that free a
   name go through here. *)
let rec remove_at (Base_dir ((module B), bv) as owner) path =
  match B.stat bv ~follow:false path with
  | exception Eio.Io _ -> ()
  | { kind = `Directory; _ } -> (
      let rec clear () =
        match B.read_dir bv path with
        | [] -> ()
        | names ->
            List.iter
              (fun name ->
                let child =
                  if String.equal path "" then name
                  else Filename.concat path name
                in
                remove_at owner child)
              names;
            clear ()
      in
      clear ();
      if not (String.equal path "") then
        (* A deliberately adversarial in-memory tree may retain hidden
           bookkeeping entries after a chain of directory renames. The
           namespace entry is still safe to discard; real filesystems take the
           normal [rmdir] path. *)
        try B.rmdir bv path with Eio.Io _ -> B.unlink bv path)
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
    let fault = Schedule.next t.sched (Op.open_ path) in
    (match fault with
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let file = B.open_in bv ~sw ~follow path in
    swap_after t.boxed fault path;
    wrap_ro t.sched ~path file

  let open_out t ~sw ~follow ~append ~create path =
    let fault = Schedule.next t.sched (Op.open_ path) in
    (match fault with
    | Some (Fault.Open_error c) -> raise_open c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let file = B.open_out bv ~sw ~follow ~append ~create path in
    swap_after t.boxed fault path;
    wrap_rw t.sched ~path file

  let open_subtree t ~sw path =
    let fault = Schedule.next t.sched (Op.open_ path) in
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

  let mkdir t ~perm path =
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

  let rename_exclusive t backend ~source ~target:(new_dir, new_path) =
    let target =
      match base_dir new_dir with Some base -> base | None -> new_dir
    in
    Eio_fs_sync.Rename.exclusive backend (t.base, source) (target, new_path)

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

  (* Path syntax is a property of the backend, not of an instance, so the
     wrapper cannot take it from the base it boxes: it states the POSIX
     syntax every base here uses. *)
  include Eio_utils.Posix_path
end

let dir_handler :
    ( dir,
      [ `Dir | `Close | `Chaos | `Dir_sync | `Process_local ] )
    Eio.Resource.handler =
  Eio.Resource.handler
    [
      H (Eio.Fs.Pi.Dir, (module Dir_impl));
      H (Eio.Resource.Close, fun t -> close_resource t.base);
      H (Chaos_base, fun t -> t.base);
      H (Eio_fs_sync.Rename.Exclusive, Dir_impl.rename_exclusive);
      (* Who can open these files is the base's to say, and the wrapper adds
         no opener of its own: it gives a file no name outside this process
         and no descriptor it did not already have. One binding serves every
         base because the answer is taken from the resource -- over the
         in-memory backend nothing outside this process reaches its files,
         over a real directory any process opens them by name. *)
      H (Eio_fs_sync.Process_local, fun t -> Eio_fs_sync.is_process_local t.base);
      (* Whether a mode is kept is the base's storage model too. *)
      H (Eio_fs_sync.Modeless, fun t -> Eio_fs_sync.is_modeless t.base);
      (* The barrier is a sync the schedule may fault; it then delegates to
         the base's barrier, so a wrapped POSIX directory still reaches the
         real fsync. *)
      H
        ( Eio_fs_sync.Dir_sync,
          fun t ~path ->
            (match Schedule.next t.sched (Op.sync path) with
            | Some (Fault.Sync_error c) -> raise_io c path
            | _ -> ());
            Eio_fs_sync.sync_dir (t.base, path) );
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
  type change = Write of { offset : int; data : Cstruct.t } | Truncate of int

  (* [durable] is the content as of the last [sync]; the pending changes live in
     the controller's log. [owner] and [rel] locate the file under the wrapped
     root so a crash can rewrite it after intermediate subtrees close; both move
     on rename. *)
  type fstate = {
    mutable owner : boxed;
    mutable rel : string;
    mutable perm : int;
    mutable durable : Cstruct.t;
  }

  type entry = { efs : fstate; change : change; seq : int }

  (* What kind of un-synced change a journal entry records, as seen by a
     scripted survival predicate. *)
  type op_kind =
    | Data
    | Create
    | Mkdir
    | Unlink
    | Rmdir
    | Symlink of { target : string }
    | Rename of { dest : string }

  let equal_op_kind (a : op_kind) b =
    match (a, b) with
    | Data, Data | Create, Create | Mkdir, Mkdir -> true
    | Unlink, Unlink | Rmdir, Rmdir -> true
    | Symlink a, Symlink b -> String.equal a.target b.target
    | Rename a, Rename b -> String.equal a.dest b.dest
    | (Data | Create | Mkdir | Unlink | Rmdir | Symlink _ | Rename _), _ ->
        false

  type unsynced = {
    path : string;
    kind : op_kind;
    offset : int;
    length : int;
    seq : int;
  }
  (* An un-synced change, as seen by a scripted survival predicate. *)

  (* Directory durability uses whole-tree snapshots: metadata operations are
     rare, and a snapshot makes rename rollback atomic and unambiguous. *)
  type tree =
    | File of { data : Cstruct.t; perm : int }
    | Dir of { perm : int; entries : (string * tree) list }
    | Link of string

  type location = fstate * boxed * string
  type delta = (string list * tree option) list
  (* The namespace effect of one directory operation: each path it touched and
     the node it holds afterwards, [None] for a name it freed. Replaying a
     set's deltas in program order reproduces those operations alone, which a
     whole-tree snapshot cannot do once the set is not a journal prefix. *)

  type metadata_entry = {
    meta : unsynced;
    tree : tree;
    delta : delta;
    locations : location list;
  }

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
    | After_dir_op of { kind : op_kind; path : string }
        (** Crash once that directory operation has entered the metadata
            journal, before any following operation can commit it. *)

  type base_dir = Eio.Fs.dir_ty Eio.Path.t
  (** The backing store handed back after a crash. *)

  type t = {
    policy : policy;
    trigger : trigger;
    mutable base : base_dir option;
    files : (string, fstate) Hashtbl.t;
    mutable log : entry list; (* newest first *)
    mutable metadata : metadata_entry list; (* newest first *)
    mutable durable_tree : tree option;
    mutable durable_locations : location list;
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
        {
          path = e.efs.rel;
          kind = Data;
          offset;
          length = Cstruct.length data;
          seq = e.seq;
        }
    | Truncate n ->
        { path = e.efs.rel; kind = Data; offset = n; length = 0; seq = e.seq }

  let unsynced ctl =
    let data = List.map to_unsynced ctl.log in
    let metadata = List.map (fun e -> e.meta) ctl.metadata in
    List.sort
      (fun (a : unsynced) (b : unsynced) -> Int.compare a.seq b.seq)
      (data @ metadata)

  let apply buf = function
    | Write { offset; data } -> grow_blit buf ~offset data
    | Truncate n -> resize buf n

  (* Rewrite the base file to [content]: truncate and re-lay the bytes. A file
     staged read-only or write-only cannot be reopened O_RDWR, so grant the
     owner read and write bits for the duration of the rewrite and restore
     the exact mode after. *)
  let rewrite fs content =
    let (Base_dir ((module B), bv)) = fs.owner in
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

  let box_dir (Eio.Resource.T (v, ops)) =
    Base_dir (Eio.Resource.get ops Eio.Fs.Pi.Dir, v)

  let rec snapshot_at (Base_dir ((module B), bv) as owner) path =
    match B.stat bv ~follow:false path with
    | { kind = `Regular_file; perm; _ } ->
        let data =
          with_readable owner path (fun () ->
              Switch.run (fun sw ->
                  read_all (B.open_in bv ~sw ~follow:true path)))
          |> copy_cs
        in
        File { data; perm }
    | { kind = `Directory; perm; _ } ->
        let entries =
          B.read_dir bv path |> List.sort String.compare
          |> List.map (fun name ->
              let child =
                if String.equal path "" then name else Filename.concat path name
              in
              (name, snapshot_at owner child))
        in
        Dir { perm; entries }
    | { kind = `Symbolic_link; _ } -> Link (B.read_link bv path)
    | _ -> invalid_arg "Chaos.Crash: unsupported filesystem entry"

  let rec restore_at (Base_dir ((module B), bv) as owner) path = function
    | File { data; perm } ->
        Switch.run @@ fun sw ->
        let f =
          B.open_out bv ~sw ~follow:true ~append:false
            ~create:(`Or_truncate perm) path
        in
        if Cstruct.length data > 0 then
          Eio.File.pwrite_all f ~file_offset:Optint.Int63.zero [ data ];
        B.chmod bv ~follow:true ~perm path
    | Link target -> B.symlink ~link_to:target bv path
    | Dir { perm; entries } ->
        if not (String.equal path "") then B.mkdir bv ~perm path;
        List.iter
          (fun (name, child) ->
            let child_path =
              if String.equal path "" then name else Filename.concat path name
            in
            restore_at owner child_path child)
          entries

  let restore_root (Base_dir ((module B), bv) as owner) path = function
    | Dir { perm; entries } ->
        B.read_dir bv path
        |> List.iter (fun name ->
            let child =
              if String.equal path "" then name else Filename.concat path name
            in
            remove_at owner child);
        if not (String.equal path "") then B.chmod bv ~follow:true ~perm path;
        List.iter
          (fun (name, child) ->
            let child_path =
              if String.equal path "" then name else Filename.concat path name
            in
            restore_at owner child_path child)
          entries
    | File _ | Link _ ->
        invalid_arg "Chaos.Crash: wrapped root is not a directory"

  let locations ctl =
    Hashtbl.fold (fun _ fs acc -> (fs, fs.owner, fs.rel) :: acc) ctl.files []

  let root ctl =
    match ctl.base with
    | Some (base, path) -> (box_dir (base :> Eio.Fs.dir_ty r), path)
    | None -> invalid_arg "Chaos.Crash: controller was never wrapped"

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

  let parent_key path =
    match List.rev (path_segments path) with
    | [] -> ""
    | _ :: up -> String.concat "/" (List.rev up)

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
        match List.assoc_opt name entries with
        | Some child -> tree_find child rest
        | None -> None)

  (* [tree_replace tree segs node] is [tree] with the entry at [segs] set to
     [node], or freed when [node] is [None]. A path whose parent the tree does
     not hold, or holds as something other than a directory, leaves the tree
     untouched: the entry went with the directory that was to hold it. Entries
     stay sorted, as [snapshot_at] leaves them. *)
  let rec tree_replace tree segs node =
    match (tree, segs) with
    | _, [] -> tree
    | (File _ | Link _), _ -> tree
    | Dir d, [ name ] ->
        let entries = List.remove_assoc name d.entries in
        let entries =
          match node with
          | None -> entries
          | Some child -> (name, child) :: entries
        in
        let entries =
          List.sort (fun (a, _) (b, _) -> String.compare a b) entries
        in
        Dir { d with entries }
    | Dir d, name :: rest -> (
        match List.assoc_opt name d.entries with
        | None -> tree
        | Some child ->
            let child = tree_replace child rest node in
            let entries =
              List.map
                (fun (n, c) ->
                  if String.equal n name then (n, child) else (n, c))
                d.entries
            in
            Dir { d with entries })

  let apply_delta tree delta =
    List.fold_left
      (fun tree (segs, node) -> tree_replace tree segs node)
      tree delta

  (* The paths one directory operation touched: its own, and for a rename the
     destination as well, since a rename frees one name and fills another. *)
  let delta_of ~root ~tree ~path ~kind =
    let paths =
      match kind with Rename { dest } -> [ path; dest ] | _ -> [ path ]
    in
    List.filter_map
      (fun p ->
        match tree_path ~root p with
        | None -> None
        | Some segs -> Some (segs, tree_find tree segs))
      paths

  let metadata_unsynced ~path ~kind ~seq =
    { path; kind; offset = 0; length = 0; seq }

  let record_metadata ctl ~path ~kind =
    ctl.seq <- ctl.seq + 1;
    let owner, root_path = root ctl in
    let meta = metadata_unsynced ~path ~kind ~seq:ctl.seq in
    let tree = snapshot_at owner root_path in
    let delta = delta_of ~root:(path_segments root_path) ~tree ~path ~kind in
    ctl.metadata <-
      { meta; tree; delta; locations = locations ctl } :: ctl.metadata

  let commit_metadata ctl =
    let owner, root_path = root ctl in
    ctl.durable_tree <- Some (snapshot_at owner root_path);
    ctl.durable_locations <- locations ctl;
    ctl.metadata <- []

  (* fsync(2) on a directory commits that directory's entries and nothing else:
     the journal entries whose recorded path names a child of [dir]. A model
     that commits the journal whole cannot tell the two apart.

     Those entries are not in general a journal prefix, so no snapshot can
     become the durable image. The image advances by replaying the committed
     entries' deltas, and the entries left pending are rebased onto the result:
     [do_crash] reads the last
     surviving entry's snapshot, so that snapshot must hold everything already
     durable at the moment of the crash.

     An entry of a directory whose own entry is not durable cannot be durable
     either -- after the power cut there is no directory to hold it -- so a
     barrier on such a directory commits nothing. A rename committed through its
     source's directory lands the same way round: [apply_delta] drops the half
     whose parent the durable image does not hold. *)
  let commit_dir ctl ~dir =
    match (ctl.base, ctl.durable_tree) with
    | None, _ | _, None -> ()
    | Some (_, root_path), Some durable -> (
        let root = path_segments root_path in
        let in_image =
          match tree_path ~root dir with
          | None -> None
          | Some segs -> tree_find durable segs
        in
        let dir = path_key dir in
        match in_image with
        | None | Some (File _ | Link _) -> ()
        | Some (Dir _) -> (
            let committed, pending =
              List.partition
                (fun e -> String.equal (parent_key e.meta.path) dir)
                (List.rev ctl.metadata)
            in
            match committed with
            | [] -> ()
            | _ :: _ ->
                let durable =
                  List.fold_left
                    (fun tree e -> apply_delta tree e.delta)
                    durable committed
                in
                ctl.durable_tree <- Some durable;
                ctl.durable_locations <- locations ctl;
                (* A rebased entry's image holds every durable file, so it
                   names each one to rewrite, a file made after the entry was
                   recorded included. *)
                let with_durable own =
                  own
                  @ List.filter
                      (fun (fs, _, _) ->
                        not (List.exists (fun (fs', _, _) -> fs' == fs) own))
                      ctl.durable_locations
                in
                let _, rebased =
                  List.fold_left
                    (fun (tree, acc) e ->
                      let tree = apply_delta tree e.delta in
                      ( tree,
                        { e with tree; locations = with_durable e.locations }
                        :: acc ))
                    (durable, []) pending
                in
                ctl.metadata <- rebased))

  (* A file the wrapper first meets on an open that finds it on disk was created
     by nothing the model recorded, so its name is part of every image that
     holds a regular file at its path: the durable one and each pending entry's.
     Locating it there lets a crash rewrite it with what it synced. *)
  let locate_preexisting ctl ((_, _, rel) as location) =
    match ctl.base with
    | None -> ()
    | Some (_, root_path) ->
        let holds tree =
          match tree_path ~root:(path_segments root_path) rel with
          | None -> false
          | Some segs -> (
              match tree_find tree segs with
              | Some (File _) -> true
              | Some (Dir _ | Link _) | None -> false)
        in
        (match ctl.durable_tree with
        | Some tree when holds tree ->
            ctl.durable_locations <- location :: ctl.durable_locations
        | Some _ | None -> ());
        ctl.metadata <-
          List.map
            (fun e ->
              if holds e.tree then
                { e with locations = location :: e.locations }
              else e)
            ctl.metadata

  let do_crash ctl =
    if not ctl.crashed then begin
      ctl.crashed <- true;
      let ordered =
        List.rev ctl.log
        (* program order *)
      in
      let data_survivors =
        match ctl.policy with
        | Drop_unsynced -> []
        | Survive pred -> List.filter (fun e -> pred (to_unsynced e)) ordered
        | Reorder_unsynced seed ->
            let rng = Random.State.make [| seed |] in
            let kept = List.filter (fun _ -> Random.State.bool rng) ordered in
            shuffle rng kept
      in
      let ordered_metadata = List.rev ctl.metadata in
      let rec take_prefix pred = function
        | x :: xs when pred x -> x :: take_prefix pred xs
        | _ -> []
      in
      let metadata_survivors =
        match ctl.policy with
        | Drop_unsynced -> []
        | Survive pred -> take_prefix (fun e -> pred e.meta) ordered_metadata
        | Reorder_unsynced seed ->
            (* Arbitrary salt for the reorder RNG. Written as a shift so the
               32-bit "META" value (0x4d455441) is not an int literal that
               overflows a 31-bit int (js_of_ocaml, wasm_of_ocaml); the 64-bit
               value, and so the fault-injection sequence, is unchanged. *)
            let salt = (0x4d45 lsl 16) lor 0x5441 in
            let rng = Random.State.make [| seed; salt |] in
            take_prefix (fun _ -> Random.State.bool rng) ordered_metadata
      in
      let tree, locs =
        match List.rev metadata_survivors with
        | e :: _ -> (e.tree, e.locations)
        | [] -> (
            match ctl.durable_tree with
            | Some tree -> (tree, ctl.durable_locations)
            | None -> assert false)
      in
      let owner, root_path = root ctl in
      restore_root owner root_path tree;
      List.iter
        (fun (fs, owner, rel) ->
          let changes =
            List.filter_map
              (fun e -> if e.efs == fs then Some e.change else None)
              data_survivors
          in
          let content = List.fold_left apply (copy_cs fs.durable) changes in
          let (Base_dir ((module B), bv)) = owner in
          match B.stat bv ~follow:false rel with
          | { kind = `Regular_file; _ } ->
              fs.owner <- owner;
              fs.rel <- rel;
              rewrite fs content
          | _ | (exception Eio.Io _) -> ())
        locs
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

  (* fsync(2) flushes the file, whichever descriptor names it, so a barrier
     through a read-only handle commits the file as one through a read-write
     handle does. A file the model does not track was never written through
     the wrapper and is durable already. *)
  let sync_ro cf fs =
    guard cf;
    match fs with
    | None -> ()
    | Some fs ->
        fs.durable <- read_all cf.cro;
        cf.ctl.log <- List.filter (fun e -> not (e.efs == fs)) cf.ctl.log;
        bump cf.ctl fs ~kind:`Sync ~offset:0 ~len:0

  let wrap_cfile_ro ?fs ctl base : Eio.File.ro_ty r =
    let cf = { ctl; cfs = None; cro = base; crw = None; cpos = ref 0 } in
    Eio.Resource.T
      ( cf,
        Eio.Resource.handler
          (Eio.Resource.bindings (Eio.File.Pi.ro (module Cro_impl))
          @ [ Eio.Resource.H (Eio_fs_sync.File_sync, fun cf -> sync_ro cf fs) ]
          ) )

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
       them. It commits this file's data and nothing else -- the directory
       entries a name lives in are the metadata journal's business, committed by
       [sync_metadata] or a registered [Dir_sync], as fsync(2) divides them. *)
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

  let wrap_cfile_rw ctl fs base : Eio.File.rw_ty r =
    let state =
      {
        ctl;
        cfs = Some fs;
        cro = (base :> Eio.File.ro_ty r);
        crw = Some base;
        cpos = ref 0;
      }
    in
    let (Eio.Resource.T (bv, bops)) = base in
    rw_over (module Crw_impl) state bv bops

  (* [droot] is the boxed directory the controller was wrapped over and
     [dprefix] this directory's path from it: tracked files are addressed
     through the long-lived root so a crash can rewrite them after any
     intermediate staging subtree is closed. *)
  type cdir = {
    dctl : t;
    dbase : Eio.Fs.dir_ty r;
    dboxed : boxed;
    droot : boxed;
    dprefix : string;
  }

  (* Expose the whole crash directory so a rename whose target is itself a
     crash directory reaches both its base (for the backend call) and its
     root-relative location (to rehome durability state). *)
  type (_, _, _) Eio.Resource.pi +=
    | Crash_dir : ('t, 't -> cdir, [> `Crash_dir ]) Eio.Resource.pi

  let crash_dir_hook :
      (t ->
      root:boxed ->
      prefix:string ->
      base:Eio.Fs.dir_ty r ->
      [ `Close | Eio.Fs.dir_ty ] r)
      ref =
    ref (fun _ ~root:_ ~prefix:_ ~base:_ -> assert false)

  let is_trunc = function
    | `Or_truncate _ | `Exclusive _ -> true
    | `Never | `If_missing _ -> false

  let create_perm = function
    | `Never -> 0o644
    | `If_missing p | `Or_truncate p | `Exclusive p -> p

  (* Compose a path opened through a subtree with the subtree's own path from
     the wrap root. *)
  let at prefix path = if prefix = "" then path else Filename.concat prefix path

  let file_key (Base_dir ((module B), bv)) path =
    match B.native bv path with Some native -> native | None -> path

  let path_is_at_or_under ~parent path =
    String.equal parent path
    ||
    let n = String.length parent in
    String.length path > n
    && String.equal (String.sub path 0 n) parent
    && path.[n] = '/'

  let move_state ctl ~old_owner ~old_rel ~new_owner ~new_rel =
    let states = Hashtbl.fold (fun _ fs acc -> fs :: acc) ctl.files [] in
    Hashtbl.clear ctl.files;
    List.iter
      (fun fs ->
        let moved =
          fs.owner == old_owner && path_is_at_or_under ~parent:old_rel fs.rel
        in
        let replaced =
          fs.owner == new_owner && path_is_at_or_under ~parent:new_rel fs.rel
        in
        if moved then begin
          let suffix =
            String.sub fs.rel (String.length old_rel)
              (String.length fs.rel - String.length old_rel)
          in
          fs.owner <- new_owner;
          fs.rel <- new_rel ^ suffix;
          Hashtbl.replace ctl.files (file_key new_owner fs.rel) fs
        end
        else if replaced then
          ctl.log <- List.filter (fun e -> not (e.efs == fs)) ctl.log
        else Hashtbl.replace ctl.files (file_key fs.owner fs.rel) fs)
      states

  (* After the crash nothing reaches the store: a killed process does nothing
     more, so every operation through the wrapper fails, a directory operation
     as a read or a write does. *)
  let live d = if d.dctl.crashed then raise_power_lost ()

  module Cdir_impl = struct
    type t = cdir

    let open_in d ~sw ~follow path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let fs =
        Hashtbl.find_opt d.dctl.files (file_key d.droot (at d.dprefix path))
      in
      wrap_cfile_ro ?fs d.dctl (B.open_in bv ~sw ~follow path)

    let open_out d ~sw ~follow ~append ~create path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let rel = at d.dprefix path in
      let key = file_key d.droot rel in
      let existed =
        match B.stat bv ~follow:false path with
        | _ -> true
        | exception Eio.Io _ -> false
      in
      let prev = read_existing d.dboxed path in
      let base = B.open_out bv ~sw ~follow ~append ~create path in
      let perm = create_perm create in
      let fs =
        match Hashtbl.find_opt d.dctl.files key with
        | Some fs ->
            fs.perm <- perm;
            fs
        | None ->
            let fs = { owner = d.droot; rel; perm; durable = prev } in
            Hashtbl.replace d.dctl.files key fs;
            if existed then locate_preexisting d.dctl (fs, d.droot, rel);
            fs
      in
      (* An unsynced truncation of pre-existing durable data is itself a change
         that a crash may lose. *)
      if is_trunc create && Cstruct.length prev > 0 then
        clog_change d.dctl fs (Truncate 0);
      if not existed then begin
        record_metadata d.dctl ~path:rel ~kind:Create;
        bump_metadata d.dctl ~path:rel ~kind:Create
      end;
      wrap_cfile_rw d.dctl fs base

    let open_subtree d ~sw path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      let sub = B.open_subtree bv ~sw path in
      !crash_dir_hook d.dctl ~root:d.droot ~prefix:(at d.dprefix path)
        ~base:(sub :> Eio.Fs.dir_ty r)

    let stat d ~follow path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.stat bv ~follow path

    let mkdir d ~perm path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.mkdir bv ~perm path;
      let rel = at d.dprefix path in
      record_metadata d.dctl ~path:rel ~kind:Mkdir;
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
      B.unlink bv path;
      let rel = at d.dprefix path in
      Hashtbl.remove d.dctl.files (file_key d.droot rel);
      record_metadata d.dctl ~path:rel ~kind:Unlink;
      bump_metadata d.dctl ~path:rel ~kind:Unlink

    let rmdir d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.rmdir bv path;
      let rel = at d.dprefix path in
      record_metadata d.dctl ~path:rel ~kind:Rmdir;
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
      let old_rel = at d.dprefix old_path in
      (* A crash-wrapped target rehomes the file root-relative, so its rewrite
         does not depend on the target subtree staying open. *)
      let owner, rel =
        match target_cdir with
        | Some cd -> (cd.droot, at cd.dprefix new_path)
        | None -> (box_dir target, new_path)
      in
      (match exclusive with
      | None -> B.rename bv old_path target new_path
      | Some backend ->
          Eio_fs_sync.Rename.exclusive backend (d.dbase, old_path)
            (target, new_path));
      (* A successful rename moves the durability identity with the file. If a
         tracked target was replaced, its pending writes no longer describe a
         reachable file and must not be replayed after a crash. *)
      move_state d.dctl ~old_owner:d.droot ~old_rel ~new_owner:owner
        ~new_rel:rel;
      let kind = Rename { dest = rel } in
      record_metadata d.dctl ~path:old_rel ~kind;
      bump_metadata d.dctl ~path:old_rel ~kind

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
      let rel = at d.dprefix path in
      let kind = Symlink { target = link_to } in
      record_metadata d.dctl ~path:rel ~kind;
      bump_metadata d.dctl ~path:rel ~kind

    let chmod d ~follow ~perm path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.chmod bv ~follow ~perm path

    let chown ~follow ?uid ?gid d path =
      live d;
      let (Base_dir ((module B), bv)) = d.dboxed in
      B.chown ~follow ?uid ?gid bv path

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
        (* A store's directory barrier commits the entries of the directory it
           names, exactly as the platform's directory fsync would, and the
           journal is asked root-relative so a barrier issued through a staging
           subtree still names the same directory. After the power cut it fails
           like every other operation. *)
        H
          ( Eio_fs_sync.Dir_sync,
            fun d ~path ->
              if d.dctl.crashed then raise_power_lost ();
              commit_dir d.dctl ~dir:(at d.dprefix path) );
      ]

  let wrap_cdir ctl ~root ~prefix ~base : [ `Close | Eio.Fs.dir_ty ] r =
    (Eio.Resource.T
       ( {
           dctl = ctl;
           dbase = base;
           dboxed = box_dir base;
           droot = root;
           dprefix = prefix;
         },
         cdir_handler )
      :> [ `Close | Eio.Fs.dir_ty ] r)

  let () =
    crash_dir_hook :=
      fun ctl ~root ~prefix ~base -> wrap_cdir ctl ~root ~prefix ~base

  let v ?(policy = Drop_unsynced) ?(trigger = Manual) () =
    {
      policy;
      trigger;
      base = None;
      files = Hashtbl.create 8;
      log = [];
      metadata = [];
      durable_tree = None;
      durable_locations = [];
      seq = 0;
      ops = 0;
      crashed = false;
    }

  let wrap ctl (base, path) =
    let base = (base :> Eio.Fs.dir_ty r) in
    ctl.base <- Some (base, path);
    let owner = box_dir base in
    ctl.durable_tree <- Some (snapshot_at owner path);
    ctl.durable_locations <- [];
    (wrap_cdir ctl ~root:(box_dir base) ~prefix:"" ~base, path)

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

  let pp_unsynced ppf u =
    match u.kind with
    | Data -> Fmt.pf ppf "%s @%d+%d #%d" u.path u.offset u.length u.seq
    | Create -> Fmt.pf ppf "create %s #%d" u.path u.seq
    | Mkdir -> Fmt.pf ppf "mkdir %s #%d" u.path u.seq
    | Unlink -> Fmt.pf ppf "unlink %s #%d" u.path u.seq
    | Rmdir -> Fmt.pf ppf "rmdir %s #%d" u.path u.seq
    | Symlink { target } ->
        Fmt.pf ppf "symlink %s -> %s #%d" u.path target u.seq
    | Rename { dest } -> Fmt.pf ppf "rename %s -> %s #%d" u.path dest u.seq
end
