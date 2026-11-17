(* Record locks, lock files, held locks, leases and lock sections, and the
   calls the rest of the library shares with them. *)

external is_darwin : unit -> bool = "caml_eio_fs_is_darwin"
external flock_is_separate : unit -> bool = "caml_eio_fs_flock_is_separate"

(* A call that takes an [Eio.Path.t] or an [Eio.File.t] reports its failure as
   Eio's own backends do: the [Unix.Unix_error] beneath becomes [Eio.Io]. *)
let eio_unix_errors fn =
  match fn () with
  | value -> value
  | exception Unix.Unix_error (code, operation, argument) ->
      raise (Eio_unix.Err.v code operation argument)

let rec retry_eintr f fd =
  match f fd with
  | () -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> retry_eintr f fd

(* The host descriptor beneath [file], through any wrapper of this package in
   front of it: the descriptor a lock is taken on. *)
let rec file_fd : 'a. 'a Eio.Resource.t -> Eio_unix.Fd.t option =
 fun file ->
  match Eio_unix.Resource.fd_opt file with
  | Some _ as fd -> fd
  | None -> Option.bind (Eio_fs_internal.base_file file) file_fd

type flock_mode = Exclusive | Shared

external flock_native : Unix.file_descr -> flock_mode -> unit
  = "caml_eio_fs_flock"

external flock_unlock_native : Unix.file_descr -> unit
  = "caml_eio_fs_flock_unlock"

external record_lock_holder : Unix.file_descr -> int
  = "caml_eio_fs_record_lock_holder"

external lock_range_native : Unix.file_descr -> int -> int -> int -> unit
  = "caml_eio_fs_lock_range"

external range_holder_native : Unix.file_descr -> int -> int -> int
  = "caml_eio_fs_range_holder"

(* Another process's conflicting lock fails F_SETLK with EAGAIN or EACCES,
   as POSIX allows either. *)
let lock_range fd kind ~file_offset ~length =
  let kind = match kind with `Read -> 0 | `Write -> 1 | `Unlock -> 2 in
  match lock_range_native fd kind file_offset length with
  | () -> true
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> false

let range_holder fd ~file_offset ~length =
  match range_holder_native fd file_offset length with
  | -1 -> `Free
  | 0 -> `Held None
  | pid -> `Held (Some pid)

external pread_head : Unix.file_descr -> int -> string
  = "caml_eio_fs_pread_head"

type record_lock_wait

external record_lock_wait_start :
  Unix.file_descr -> Unix.file_descr -> bool -> record_lock_wait
  = "caml_eio_fs_record_lock_wait_start"

external record_lock_wait_end : record_lock_wait -> bool -> bool
  = "caml_eio_fs_record_lock_wait_end"

type block = [ `Yes | `No | `Fibers | `For of float ]
type create = [ `Never | `If_missing of Eio.File.Unix_perm.t ]
type refusal = { path : string; holder : int option; contents : string }

exception Lock_unavailable of refusal

(* What the holder wrote: the record's first line that is not blank. *)
let pp_doing f contents =
  match
    List.find_opt
      (fun l -> String.trim l <> "")
      (String.split_on_char '\n' contents)
  with
  | Some line -> Fmt.pf f ": %s" (String.trim line)
  | None -> ()

(* POSIX record locks belong to a process, so the kernel never names this
   process as the holder: this process is named only for a fiber's hold. *)
let pp_refusal f { path; holder; contents } =
  match holder with
  | Some pid when pid = Unix.getpid () ->
      Fmt.pf f "%s is held by another fiber of this process%a" path pp_doing
        contents
  | Some pid -> Fmt.pf f "%s is held by process %d" path pid
  | None -> Fmt.pf f "%s is held" path

let () =
  Printexc.register_printer (function
    | Lock_unavailable refusal ->
        Fmt.kstr Option.some "Eio_fs_unix.Lock_unavailable: %a" pp_refusal
          refusal
    | _ -> None)

(* The holder's record is read without the lock, so it is bounded: a record
   longer than a diagnostic is cut, not a cost the asker pays in full. *)
let record_bound = 4096

let read_record file =
  let buf = Cstruct.create record_bound in
  let rec fill off =
    if off >= record_bound then off
    else
      match
        Eio.File.pread file ~file_offset:(Optint.Int63.of_int off)
          [ Cstruct.shift buf off ]
      with
      | 0 -> off
      | got -> fill (off + got)
      | exception End_of_file -> off
  in
  match fill 0 with
  | got -> Cstruct.to_string ~len:got buf
  | exception Eio.Io _ -> ""

(* The holder's record read through [file], or through [unix], the Unix
   descriptor an ask keeps open with [Eio_unix.Fd.use]: an Eio read would take
   a use of its own, which a close by the holder refuses even while the ask's
   use keeps the descriptor open. *)
let read_held_record file = function
  | None -> read_record file
  | Some unix -> (
      try pread_head unix record_bound with Unix.Unix_error _ -> "")

(* A fiber holder is this process; another process's hold is named by the
   kernel, and [None] means it let go before it could be asked. *)
let refusal ~name ~holder file =
  Lock_unavailable { path = name; holder; contents = read_record file }

let held_by_a_fiber () = Some (Unix.getpid ())

let held_by_a_process fd =
  match record_lock_holder fd with
  | 0 -> None
  | pid -> Some pid
  | exception Unix.Unix_error _ -> None

(* The fiber exclusion is keyed by device and inode, not by a name: two names
   can reach one file (a hard link), and POSIX record locks do not exclude
   within a process. The file stays open while its key is in use, so no inode
   number is reused under the table. *)
type lock_key = int64 * int64
(** The device and inode a record lock is held on. *)

type local_lock = {
  mutex : Eio.Mutex.t;
  mutable users : int;
  mutable readers : int;
  changed : Eio.Condition.t;
  mutable holding : bool;
      (** A fiber holds [mutex] and may hold the record lock. *)
  mutable parked : Unix.file_descr list;
      (** Descriptors on the file that other callers let go of while it was
          held, closed by the holder once its record lock is released. *)
  mutable file : Eio.File.ro_ty Eio.Resource.t option;
      (** The holder's own descriptor on the file, once it has opened it: an ask
          from this process reads it rather than opening one of its own. *)
}

let local_locks : (lock_key, local_lock) Hashtbl.t = Hashtbl.create 16
let entry_gates : (string, local_lock) Hashtbl.t = Hashtbl.create 16
let tables_mutex = Stdlib.Mutex.create ()

(* A mutex per key, kept only while a caller uses it. *)
let shared_mutex table key =
  Stdlib.Mutex.protect tables_mutex @@ fun () ->
  match Hashtbl.find_opt table key with
  | Some lock ->
      lock.users <- lock.users + 1;
      lock
  | None ->
      let lock =
        {
          mutex = Eio.Mutex.create ();
          users = 1;
          readers = 0;
          changed = Eio.Condition.create ();
          holding = false;
          parked = [];
          file = None;
        }
      in
      Hashtbl.add table key lock;
      lock

let done_with_shared_mutex table key lock =
  Stdlib.Mutex.protect tables_mutex @@ fun () ->
  lock.users <- lock.users - 1;
  if lock.users = 0 then Hashtbl.remove table key

let use_shared_mutex table key fn =
  let lock = shared_mutex table key in
  Fun.protect
    ~finally:(fun () -> done_with_shared_mutex table key lock)
    (fun () -> fn lock)

let use_local_lock (key : lock_key) fn = use_shared_mutex local_locks key fn

(* POSIX drops every record lock a process holds on a file when the process
   closes any descriptor for it, so a caller that leaves without the fiber
   mutex (refused, out of patience, cancelled) never closes its descriptor
   while a fiber of this process holds the file: it hands the descriptor to
   that holder, which closes it once its own record lock is released. *)
let let_go_of local file =
  match file_fd file with
  | None -> ()
  | Some fd -> (
      let park = Stdlib.Mutex.protect tables_mutex @@ fun () -> local.holding in
      if park then
        Eio.Cancel.protect @@ fun () ->
        match Eio_unix.Fd.remove fd with
        | None -> ()
        | Some unix_fd ->
            let closes_now =
              Stdlib.Mutex.protect tables_mutex @@ fun () ->
              if local.holding then begin
                local.parked <- unix_fd :: local.parked;
                false
              end
              else true
            in
            if closes_now then Unix.close unix_fd)

(* The holder's side: set once the mutex is taken, and cleared, with every
   parked descriptor closed, once the record lock is let go. *)
let start_holding local =
  Stdlib.Mutex.protect tables_mutex (fun () -> local.holding <- true)

let stop_holding local =
  let parked =
    Stdlib.Mutex.protect tables_mutex @@ fun () ->
    let parked = local.parked in
    local.holding <- false;
    local.parked <- [];
    local.file <- None;
    parked
  in
  List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) parked

let set_held_file local file =
  Stdlib.Mutex.protect tables_mutex (fun () -> local.file <- file)

(* [fn file fd] over the holder's descriptor, kept open for the call, with
   [fd] its Unix descriptor when it has one; [otherwise ()] when no fiber
   holds the file with a descriptor open, or the holder let go of it. [fn]
   reads through [fd] when there is one ({!read_held_record}), since the
   holder may close [file] during the call. An ask from this process answered
   here opens nothing, so it leaves nothing parked with the holder, however
   long the holder holds. *)
let with_held_file local ~otherwise fn =
  let file =
    Stdlib.Mutex.protect tables_mutex @@ fun () ->
    if local.holding then local.file else None
  in
  match file with
  | None -> otherwise ()
  | Some file -> (
      match file_fd file with
      | None -> fn file None
      | Some fd ->
          Eio_unix.Fd.use fd ~if_closed:otherwise (fun unix ->
              fn file (Some unix)))

(* A fiber of this process holds the file: its record is read through the
   holder's descriptor. *)
let refused_by_a_fiber ~name local =
  let contents =
    with_held_file local ~otherwise:(fun () -> "") read_held_record
  in
  Lock_unavailable { path = name; holder = held_by_a_fiber (); contents }

(* One budget covers both halves of the exclusion, so it is a moment both watch
   for rather than a duration each is handed. A daemon of the switch sleeps it
   on the scheduler's monotonic clock, from the first wait: an acquisition that
   never waits starts no timer. *)
type patience =
  | Wait  (** wait for the holder, however long it holds *)
  | Refuse  (** answer at once *)
  | Until of unit Eio.Promise.t Lazy.t  (** wait until the budget lapses *)

let budget ~sw seconds =
  Until
    (lazy
      (let lapsed, set_lapsed = Eio.Promise.create () in
       Eio.Fiber.fork_daemon ~sw (fun () ->
           Eio_unix.sleep seconds;
           Eio.Promise.resolve set_lapsed ();
           `Stop_daemon);
       lapsed))

(* [on_taken] runs the moment the mutex is held, so a caller records how to
   give it back before anything else can raise. A wait behind a holder, bounded
   or not, calls [on_wait ()] first. *)
let take_available ~on_wait ~patience ~refused ~on_taken ~try_take ~wait () =
  match patience with
  | Wait ->
      if not (try_take ()) then begin
        on_wait ();
        wait ()
      end;
      on_taken ()
  | Refuse -> if try_take () then on_taken () else raise (refused ())
  | Until _ when try_take () -> on_taken ()
  | Until lapsed ->
      on_wait ();
      (* [Fiber.first] cancels the loser, and a fiber cancelled inside
         [Eio.Mutex.lock] may have been granted the mutex all the same: the
         unlocker's wake and the cancellation race for one atomic and either
         can win. So what was taken is remembered rather than read off which
         fiber came back, and a mutex granted as the bound lapsed is held, for
         the caller to give back, rather than dropped with nobody left to
         unlock it. *)
      let taken = ref false in
      let granted =
        Eio.Fiber.first
          (fun () ->
            wait ();
            taken := true;
            true)
          (fun () ->
            Eio.Promise.await (Lazy.force lapsed);
            false)
      in
      if granted || !taken then on_taken () else raise (refused ())

let take_mutex ~on_wait ~patience ~refused ~on_taken mutex =
  take_available ~on_wait ~patience ~refused ~on_taken
    ~try_take:(fun () -> Eio.Mutex.try_lock mutex)
    ~wait:(fun () -> Eio.Mutex.lock mutex)
    ()

let unlock_local local =
  Eio.Mutex.unlock local.mutex;
  Eio.Condition.broadcast local.changed

(* Readers share the existing inode gate, but each takes its own kernel lock.
   The gate excludes record-lock users too, whose locks are process-owned. *)
let take_shared_local ~patience ~refused ~on_wait ~on_taken local =
  let try_take () =
    let acquired =
      Stdlib.Mutex.protect tables_mutex @@ fun () ->
      if local.readers > 0 || Eio.Mutex.try_lock local.mutex then begin
        local.readers <- local.readers + 1;
        true
      end
      else false
    in
    if acquired then Eio.Condition.broadcast local.changed;
    acquired
  in
  let wait () =
    Eio.Condition.loop_no_mutex local.changed (fun () ->
        if try_take () then Some () else None)
  in
  take_available ~on_wait ~patience ~refused ~try_take ~wait
    ~on_taken:(fun () ->
      let active = Atomic.make true in
      on_taken (fun () ->
          if Atomic.exchange active false then begin
            let last =
              Stdlib.Mutex.protect tables_mutex @@ fun () ->
              local.readers <- local.readers - 1;
              local.readers = 0
            in
            if last then unlock_local local
          end))
    ()

(* Entering is queued too, on a mutex keyed by the caller's name: the stat or
   the open that keys the file suspends, so without this the order fibers
   reach the file's mutex is the order those came back in. The gate is held
   only across that, since a name is not a file. *)
let in_entry_order name fn =
  use_shared_mutex entry_gates name @@ fun { mutex; _ } ->
  let entered = ref false in
  Fun.protect
    ~finally:(fun () -> if !entered then Eio.Mutex.unlock mutex)
    (fun () ->
      Eio.Mutex.lock mutex;
      entered := true;
      fn ())

let lockf fd command = retry_eintr (fun fd -> Unix.lockf fd command 0) fd

let os_unlock fd =
  Eio.Cancel.protect @@ fun () ->
  Eio_unix.Fd.use_exn "Eio_fs_unix.unlock" fd @@ fun fd ->
  Eio_unix.run_in_systhread ~label:"eio-fs-unlock" (fun () ->
      lockf fd Unix.F_ULOCK)

(* The kernel's blocking record lock over [fd], which answers when the holder
   lets go: [true] once granted. The request waits on a thread of its own
   (record_lock_wait_start), which writes to a pipe when answered, so the
   fiber waits on the pipe and a cancel reaches it; the cancel then withdraws
   the request before the fiber goes, and a lock granted in that instant is
   let go. *)
let await_record_lock ?(mode = `Exclusive) fd =
  let answered, notify = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () ->
      Unix.close answered;
      Unix.close notify)
  @@ fun () ->
  let wait = record_lock_wait_start fd notify (mode = `Shared) in
  match Eio_unix.await_readable answered with
  | () -> ignore (record_lock_wait_end wait false : bool)
  | exception (Eio.Cancel.Cancelled _ as e) ->
      if record_lock_wait_end wait true then lockf fd Unix.F_ULOCK;
      raise e

(* [on_taken] runs the moment the record lock is held, as {!take_mutex}'s
   does. A wait asks once without waiting, calls [on_wait] if the lock is
   held, then makes the kernel's blocking request; a bounded wait races that
   request against its lapse, which withdraws it. *)
let take_os_lock ~patience ~refused ~on_taken ~on_wait fd =
  let acquired = ref false in
  let ask () =
    Eio.Cancel.protect (fun () ->
        Eio_unix.Fd.use_exn "Eio_fs_unix.lock" fd @@ fun fd ->
        match
          Eio_unix.run_in_systhread ~label:"eio-fs-lock" (fun () ->
              lockf fd Unix.F_TLOCK)
        with
        | () ->
            acquired := true;
            on_taken ()
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> ())
  in
  let wait ~lapsed ~granted =
    ask ();
    if (not !acquired) && not (lapsed ()) then begin
      on_wait ();
      Eio_unix.Fd.use_exn "Eio_fs_unix.lock" fd @@ fun fd ->
      if granted fd then begin
        acquired := true;
        on_taken ()
      end
    end
  in
  (match patience with
  | Wait ->
      wait
        ~lapsed:(fun () -> false)
        ~granted:(fun fd ->
          await_record_lock fd;
          true)
  | Refuse -> ask ()
  | Until lapse ->
      wait
        ~lapsed:(fun () -> Eio.Promise.is_resolved (Lazy.force lapse))
        ~granted:(fun fd ->
          Eio.Fiber.first
            (fun () ->
              await_record_lock fd;
              true)
            (fun () ->
              Eio.Promise.await (Lazy.force lapse);
              false)));
  if not !acquired then raise (refused ())

(* A planted link under the lock file's name would send the record lock to
   whatever it names. Creating is an exclusive open (O_CREAT|O_EXCL refuses an
   existing name, a dangling link included), and the opened descriptor's device
   and inode are then weighed against the name's own. Weighing the name before
   the open is only an early refusal. *)

type Eio.Exn.Backend.t += Not_a_lock_file of string

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
    | Not_a_lock_file path ->
        Fmt.pf f "Not_a_lock_file %S" path;
        true
    | _ -> false)

let not_a_lock_file name =
  raise (Eio.Fs.err (Eio.Fs.Permission_denied (Not_a_lock_file name)))

let is_regular : Eio.File.Stat.kind -> bool = function
  | `Regular_file -> true
  | _ -> false

(* An absent name is the ordinary first acquisition: [create] decides whether
   the open makes one, and the descriptor is weighed either way. *)
let named_kind path =
  match Eio.Path.stat ~follow:false path with
  | stat -> Some stat
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let check_lock_name ~name path =
  match named_kind path with
  | None -> ()
  | Some stat -> if not (is_regular stat.kind) then not_a_lock_file name

(* The device and inode of the file the name holds, refusing a name that holds
   no regular file; [None] while it holds nothing. *)
let named_key ~name path =
  match named_kind path with
  | None -> None
  | Some stat ->
      if not (is_regular stat.kind) then not_a_lock_file name;
      Some (stat.dev, stat.ino)

(* The file opened is the regular file the name holds. *)
let check_same_file ~name ~(opened : Eio.File.Stat.t) (named : Eio.File.Stat.t)
    =
  if
    (not (is_regular opened.kind))
    || (not (is_regular named.kind))
    || Int64.compare opened.dev named.dev <> 0
    || Int64.compare opened.ino named.ino <> 0
  then not_a_lock_file name

let check_locked_file ~name ~opened path =
  match named_kind path with
  | None -> not_a_lock_file name
  | Some named -> check_same_file ~name ~opened named

let update_lock_contents file contents =
  ignore (Eio.File.seek file Optint.Int63.zero `Set : Optint.Int63.t);
  Eio.Flow.copy_string contents file;
  Eio.File.truncate file (Optint.Int63.of_int (String.length contents))

(* An absent lock file is created exclusively, so the kernel refuses the name
   a link holds rather than following it; an existing one is opened without
   creating and its identity settled by [check_locked_file]. *)
let open_lock_file ~sw ~name ~(create : create) path =
  match create with
  | `Never -> Eio.Path.open_out ~sw ~create:`Never path
  | `If_missing perm -> (
      match Eio.Path.open_out ~sw ~create:(`Exclusive perm) path with
      | file -> file
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> (
          match Eio.Path.open_out ~sw ~create:`Never path with
          | file -> file
          | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
              not_a_lock_file name))

(* The descriptor is weighed against the name it was opened by, then the
   holder's record is written through it. *)
let settle ~name ~opened ~contents ~file path =
  check_locked_file ~name ~opened path;
  Option.iter (update_lock_contents file) contents

let block_patience ~sw = function
  | `Yes -> (Wait, Wait)
  | `No -> (Refuse, Refuse)
  | `Fibers -> (Wait, Refuse)
  | `For seconds ->
      let until = budget ~sw seconds in
      (until, until)

(* An acquisition keeps how to give back each thing it took, the latest
   first. Letting go runs them in that order, and a refusal part-way gives
   back what was already taken. *)
let let_go steps =
  Eio.Cancel.protect @@ fun () ->
  eio_unix_errors @@ fun () ->
  let taken = !steps in
  steps := [];
  List.iter (fun give_back -> give_back ()) taken

(* The name was given to another file between its key and its open. *)
exception Renamed

(* The key of the file the name holds, read before anything is opened; a
   name that holds nothing yet is opened (or created) first and keyed by its
   descriptor, which is returned with it. *)
let key_of_name ~taken ~name path open_lock_file =
  in_entry_order name @@ fun () ->
  match named_key ~name path with
  | Some key -> (key, None)
  | None ->
      let file, (opened : Eio.File.Stat.t) = open_lock_file () in
      taken (fun () -> Eio.Resource.close file);
      ((opened.dev, opened.ino), Some (file, opened))

(* The lock file opened once the mutex is held, which must be the file
   [key] names: a name given to another file in between raises [Renamed]. *)
let open_keyed ~taken ~key open_lock_file =
  let file, (opened : Eio.File.Stat.t) = open_lock_file () in
  let opened_key = (opened.dev, opened.ino) in
  if opened_key <> key then (
    (* Another fiber of this process may hold the file now named. *)
    use_local_lock opened_key (fun other -> let_go_of other file);
    Eio.Resource.close file;
    raise_notrace Renamed);
  taken (fun () -> Eio.Resource.close file);
  (file, opened)

module Flock = struct
  let acquire ~shared fd =
    retry_eintr
      (fun fd -> flock_native fd (if shared then Shared else Exclusive))
      fd

  let flock ~shared fd =
    Eio_unix.run_in_systhread ~label:"eio-fs-lease-lock" (fun () ->
        acquire ~shared fd)

  (* A wait polls: [flock] is no cancellation point and a blocking request
     cannot be withdrawn, so each ask is non-blocking and a cancellable sleep
     separates them, from 1 ms doubling to [poll_cap]. A cancel or a bound
     then leaves nothing behind. *)
  let poll_cap = 0.05

  let use_os_lock ~shared ~patience ~refused ~on_wait fd fn =
    let acquired = ref false in
    let ask () =
      Eio.Cancel.protect @@ fun () ->
      match flock ~shared fd with
      | () -> acquired := true
      | exception
          Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EACCES), _, _)
        ->
          ()
    in
    let rec poll delay =
      Eio_unix.sleep delay;
      ask ();
      if not !acquired then poll (Float.min poll_cap (delay *. 2.))
    in
    ask ();
    (if not !acquired then
       match patience with
       | Refuse -> ()
       | Wait ->
           on_wait ();
           poll 0.001
       | Until lapsed ->
           if not (Eio.Promise.is_resolved (Lazy.force lapsed)) then begin
             on_wait ();
             (* A grant as the bound lapses is kept: [acquired] says so. *)
             Eio.Fiber.first
               (fun () -> poll 0.001)
               (fun () -> Eio.Promise.await (Lazy.force lapsed))
           end);
    if not !acquired then raise (refused ());
    fn ()
end

(* The record lock on the lock file's descriptor. A lock file with none has
   no record lock to take, which only a process-local backend allows. *)
let take_record_lock ~taken ~patience ~on_wait ~name file path =
  match file_fd file with
  | Some fd ->
      (* Only Linux keeps flock apart from record locks; elsewhere the two
         conflict, and taking both on one descriptor would wait on our own
         lock. *)
      if flock_is_separate () then
        Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd (fun raw_fd ->
            Flock.use_os_lock ~shared:false ~patience
              ~refused:(fun () ->
                refusal ~name ~holder:(held_by_a_process raw_fd) file)
              ~on_wait raw_fd
              (fun () ->
                taken (fun () ->
                    Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock.unlock" fd
                      flock_unlock_native)));
      take_os_lock ~patience
        ~refused:(fun () ->
          let holder =
            Eio_unix.Fd.use_exn "Eio_fs_unix.with_lock" fd held_by_a_process
          in
          refusal ~name ~holder file)
        ~on_taken:(fun () -> taken (fun () -> os_unlock fd))
        ~on_wait fd
  | None ->
      if not (Eio_fs_sync.is_process_local (fst path)) then
        Fmt.invalid_arg
          "Eio_fs_unix.with_lock: path is %s, a file with no Unix descriptor \
           on a backend that registers no Eio_fs_sync.Process_local"
          name

let take_inode_gate ~taken ~shared ~patience ~on_wait ~name ~opened_first local
    =
  let release_reader = ref ignore in
  let entered = ref false in
  Option.iter
    (fun (file, _) ->
      taken (fun () -> if not !entered then let_go_of local file))
    opened_first;
  let refused () =
    match opened_first with
    | Some (file, _) -> refusal ~name ~holder:(held_by_a_fiber ()) file
    | None -> refused_by_a_fiber ~name local
  in
  if shared then
    take_shared_local ~patience ~refused ~on_wait local
      ~on_taken:(fun release ->
        entered := true;
        release_reader := release;
        taken release)
  else begin
    take_mutex ~on_wait ~patience ~refused
      ~on_taken:(fun () ->
        entered := true;
        taken (fun () -> unlock_local local))
      local.mutex;
    start_holding local;
    taken (fun () -> stop_holding local)
  end;
  !release_reader

let take_lease_lock ~shared ~patience ~on_wait ~name file =
  match file_fd file with
  | Some fd ->
      Eio_unix.Fd.use_exn "Eio_fs_unix.Lease.acquire" fd @@ fun raw_fd ->
      Flock.use_os_lock ~shared ~patience
        ~refused:(fun () -> refusal ~name ~holder:None file)
        ~on_wait raw_fd ignore
  | None ->
      Fmt.invalid_arg
        "Eio_fs_unix.Lease: path is %s, a file with no Unix descriptor for \
         flock"
        name

(* The mutex is keyed by the file the name holds, read before anything is
   opened: an ask a fiber of this process refuses opens no descriptor, whose
   close would drop the holder's record lock. A name that holds nothing is
   opened first and keyed by its descriptor, and a name given to another file
   before the mutex is held is keyed again. [block] is answered per half: the
   mutex is held by a fiber, the record lock by a process. *)
let rec acquire ~sw ?lease ?(block : block = `Yes) ?patience
    ?(create : create = `If_missing 0o600) ?contents ?(on_wait = ignore) path =
  let name = Eio_fs_internal.native_name path in
  let steps = ref [] in
  let taken give_back = steps := give_back :: !steps in
  let shared = lease = Some `Shared in
  let open_lock_file () =
    let file = open_lock_file ~sw ~name ~create path in
    (file, (Eio.File.stat file : Eio.File.Stat.t))
  in
  match
    eio_unix_errors @@ fun () ->
    Eio.Switch.run @@ fun budget_sw ->
    let fibers, processes =
      match patience with
      | Some patience -> patience
      | None -> block_patience ~sw:budget_sw block
    in
    let key, opened_first = key_of_name ~taken ~name path open_lock_file in
    let local = shared_mutex local_locks key in
    taken (fun () -> done_with_shared_mutex local_locks key local);
    let release_reader =
      take_inode_gate ~taken ~shared ~patience:fibers ~on_wait ~name
        ~opened_first local
    in
    let file, (opened : Eio.File.Stat.t) =
      match opened_first with
      | Some opened -> opened
      | None -> open_keyed ~taken ~key open_lock_file
    in
    if not shared then begin
      set_held_file local (Some (file :> Eio.File.ro_ty Eio.Resource.t));
      taken (fun () -> set_held_file local None)
    end;
    (* An explicitly unlocked reader may finish while another fiber holds a
       record lock. Closing its descriptor must not drop that writer's lock. *)
    taken (fun () -> let_go_of local file);
    (match lease with
    | None ->
        take_record_lock ~taken ~patience:processes ~on_wait ~name file path
    | Some _ -> take_lease_lock ~shared ~patience:processes ~on_wait ~name file);
    settle ~name ~opened ~contents ~file path;
    (file, release_reader)
  with
  | file, release_reader -> (file, (fun () -> let_go steps), release_reader)
  | exception Renamed ->
      let_go steps;
      acquire ~sw ?lease ~block ?patience ~create ?contents ~on_wait path
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      let_go steps;
      Printexc.raise_with_backtrace e bt

let with_lock ?block ?create ?contents ?on_wait path fn =
  Eio.Switch.run @@ fun sw ->
  let _, let_go, _ = acquire ~sw ?block ?create ?contents ?on_wait path in
  Fun.protect ~finally:let_go fn

(* A held lock is an acquisition whose giving back waits for {!Held.release}
   or for [sw]'s release hooks, which run after every fiber of [sw] has
   finished: a hook the holder registered after taking it runs first, while
   the lock is still held. Every write through the file is serialised. *)
module Held = struct
  type t = {
    name : string;
    mutable file : Eio.File.rw_ty Eio.Resource.t option;
    let_go : unit -> unit;
    writing : Eio.Mutex.t;
  }

  (* A refused restate is the caller's mistake and poisons nothing. *)
  let writing t fn =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.lock t.writing;
    Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.writing) fn

  let release ?contents t =
    writing t @@ fun () ->
    match t.file with
    | None -> ()
    | Some file ->
        t.file <- None;
        Fun.protect ~finally:t.let_go (fun () ->
            Option.iter (update_lock_contents file) contents)

  let take ~sw ?block ?create ?contents ?on_wait path =
    let file, let_go, _ = acquire ~sw ?block ?create ?contents ?on_wait path in
    let t =
      {
        name = Eio_fs_internal.native_name path;
        file = Some (file :> Eio.File.rw_ty Eio.Resource.t);
        let_go;
        writing = Eio.Mutex.create ();
      }
    in
    Eio.Switch.on_release sw (fun () -> release t);
    t

  (* Taking the record lock again on the held descriptor changes nothing while
     it is held, and restores it after a close elsewhere in this process
     dropped it; a process that took it in between is named, and nothing is
     written. *)
  let take_back t file =
    match file_fd file with
    | None -> ()
    | Some fd ->
        take_os_lock ~patience:Refuse
          ~refused:(fun () ->
            let holder =
              Eio_unix.Fd.use_exn "Eio_fs_unix.Held.restate" fd
                held_by_a_process
            in
            refusal ~name:t.name ~holder file)
          ~on_taken:ignore ~on_wait:ignore fd

  let restate t contents =
    writing t @@ fun () ->
    match t.file with
    | None ->
        invalid_arg
          "Eio_fs_unix.Held.restate: t is released, so it holds no lock file"
    | Some file ->
        eio_unix_errors (fun () -> take_back t file);
        update_lock_contents file contents
end

(* The kernel is asked first: it names another process holding the record
   lock, and a fiber of this process waiting for that process holds the mutex
   and not the lock. A fiber holder is this process, which the kernel never
   reports. While a fiber holds the file, both are asked through the holder's
   descriptor, so the probe opens none of its own. *)
(* The lock over the open lock [file], whose stat is [opened]. *)
let probe_file ~name file (opened : Eio.File.Stat.t) =
  use_local_lock (opened.dev, opened.ino) @@ fun local ->
  Fun.protect ~finally:(fun () -> let_go_of local file) @@ fun () ->
  let another =
    match file_fd file with
    | None -> None
    | Some fd -> Eio_unix.Fd.use_exn "Eio_fs_unix.probe" fd held_by_a_process
  in
  let holder =
    match another with
    | Some _ -> another
    | None ->
        if Stdlib.Mutex.protect tables_mutex (fun () -> local.holding) then
          held_by_a_fiber ()
        else None
  in
  let contents = read_record file in
  match holder with
  | None -> `Free contents
  | Some _ -> `Held { path = name; holder; contents }

(* The probe of the file the name holds once opened. A lock file that went
   after the open, as a ticket its holder removes, is absent, as it would be
   had it gone before; a name given to another regular file meanwhile, as a
   ticket's number given to the next ticket, is [`Renamed], for the caller
   to probe the file it holds now. *)
let probe_opened ~name path =
  Eio.Switch.run @@ fun sw ->
  check_lock_name ~name path;
  let file = Eio.Path.open_in ~sw path in
  (* virtiofs and FUSE answer ENOENT to an fstat of a file replaced since the
     open. *)
  match Eio.File.stat file with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> `Renamed
  | opened -> (
      match named_kind path with
      | None ->
          raise
            (Eio.Fs.err
               (Eio.Fs.Not_found
                  (Eio_unix.Unix_error (Unix.ENOENT, "stat", name))))
      | Some named
        when is_regular opened.kind && is_regular named.kind
             && (Int64.compare opened.dev named.dev <> 0
                || Int64.compare opened.ino named.ino <> 0) ->
          `Renamed
      | Some named ->
          check_same_file ~name ~opened named;
          `Answer (probe_file ~name file opened))

(* The probes a name given to new files over and over may take before it is
   refused: each needs the name to be given to another file while the probe
   reads it. *)
let probe_attempts = 8

let rec probe_at ~attempts path =
  let name = Eio_fs_internal.native_name path in
  let from_the_holder =
    match named_key ~name path with
    | None -> None
    | Some key -> (
        match
          Stdlib.Mutex.protect tables_mutex (fun () ->
              Hashtbl.find_opt local_locks key)
        with
        | None -> None
        | Some local ->
            with_held_file local
              ~otherwise:(fun () -> None)
              (fun file fd ->
                let holder =
                  match Option.bind fd held_by_a_process with
                  | Some _ as another -> another
                  | None -> held_by_a_fiber ()
                in
                Some
                  (`Held
                     {
                       path = name;
                       holder;
                       contents = read_held_record file fd;
                     })))
  in
  match from_the_holder with
  | Some answer -> answer
  | None -> (
      match probe_opened ~name path with
      | `Answer answer -> answer
      | `Renamed when attempts > 1 -> probe_at ~attempts:(attempts - 1) path
      | `Renamed -> not_a_lock_file name)

let probe path = probe_at ~attempts:probe_attempts path

module Lease = struct
  type t = {
    fd : Eio_unix.Fd.t;
    release_reader : unit -> unit;
    let_go : unit -> unit;
  }

  let fd t = t.fd

  let unlock t =
    Eio.Cancel.protect @@ fun () ->
    eio_unix_errors (fun () ->
        Eio_unix.Fd.use_exn "Eio_fs_unix.Lease.unlock" t.fd flock_unlock_native);
    t.release_reader ()

  let release t = t.let_go ()

  let taken ~sw ~lease ?patience ?block ?create ?contents ?on_wait path =
    let file, let_go, release_reader =
      acquire ~sw ~lease ?patience ?block ?create ?contents ?on_wait path
    in
    match file_fd file with
    | Some fd ->
        let t = { fd; release_reader; let_go } in
        Eio.Switch.on_release sw (fun () -> release t);
        t
    | None ->
        let_go ();
        Fmt.invalid_arg
          "Eio_fs_unix.Lease: path is %s, a file with no Unix descriptor for \
           flock"
          (Eio_fs_internal.native_name path)

  (* The refusal an exclusive holder of [path] gives: a shared lease tried
     without waiting and let go at once is granted beside shared holders, and
     refused by an exclusive one. [None] when only shared holders hold it,
     none does or it is absent. *)
  let exclusive_refusal path =
    match
      Eio.Switch.run @@ fun sw ->
      release (taken ~sw ~lease:`Shared ~block:`No ~create:`Never path)
    with
    | () -> None
    | exception (Lock_unavailable _ as refused) -> Some refused
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

  (* A gate is taken as any exclusive lease is, and taken again when the file
     it locked is no longer the one its name holds: a remover of the lease
     unlinks the gate it holds, and a take that waited on it takes the file
     its name holds now. *)
  let rec take_gate ~sw ~patience ~on_wait ~create ~contents gate =
    match
      taken ~sw ~lease:`Exclusive ~patience ~on_wait ~create ~contents gate
    with
    | t -> t
    | exception
        (Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied (Not_a_lock_file _)), _) as
         e) -> (
        match Eio.Path.kind ~follow:false gate with
        | `Not_found | `Regular_file ->
            take_gate ~sw ~patience ~on_wait ~create ~contents gate
        | _ -> raise e)

  let once f =
    let called = ref false in
    fun () ->
      if not !called then begin
        called := true;
        f ()
      end

  let remove_file p =
    try Eio.Path.unlink p with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

  (* The exclusive lease on [path] taken holding [gate], which every
     exclusive take of [path] holds, so none is made between this take's look
     at the holders and its blocking take: with [`Readers], the gate tried
     without waiting, the wait is for the shared holders alone. One budget
     covers both waits, and [on_wait] is called once. The gate is let go
     after the lease. *)
  let gated ~sw ~gate ~block ?create ?contents ?on_wait path =
    let on_wait = once (Option.value on_wait ~default:ignore) in
    Eio.Switch.run @@ fun budget_sw ->
    let wait = match block with `Readers -> `No | #block as b -> b in
    let patience = block_patience ~sw:budget_sw wait in
    let gate_create =
      match create with
      | Some (`If_missing _ as c) -> c
      | Some `Never | None -> `If_missing 0o600
    in
    let held =
      match
        take_gate ~sw ~patience ~on_wait ~create:gate_create
          ~contents:(Option.value contents ~default:"")
          gate
      with
      | held -> held
      | exception (Lock_unavailable _ as refused) ->
          raise (Option.value (exclusive_refusal path) ~default:refused)
    in
    let lease () =
      match block with
      | `Readers -> (
          match
            taken ~sw ~lease:`Exclusive ~block:`No ?create ?contents path
          with
          | t -> t
          | exception (Lock_unavailable _ as refused) -> (
              match exclusive_refusal path with
              | None ->
                  taken ~sw ~lease:`Exclusive ~block:`Yes ?create ?contents
                    ~on_wait path
              | Some _ -> raise refused))
      | #block ->
          taken ~sw ~lease:`Exclusive ~patience ?create ?contents ~on_wait path
    in
    match lease () with
    | t ->
        {
          t with
          let_go =
            (fun () ->
              t.let_go ();
              release held);
        }
    | exception (Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) as e) ->
        (* An absent lease keeps no gate. *)
        let bt = Printexc.get_raw_backtrace () in
        remove_file gate;
        release held;
        Printexc.raise_with_backtrace e bt
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        release held;
        Printexc.raise_with_backtrace e bt

  let take ~sw ?gate ?block ?create ?contents ?on_wait path =
    match (gate, block) with
    | Some gate, _ ->
        let block = Option.value block ~default:`Yes in
        gated ~sw ~gate ~block ?create ?contents ?on_wait path
    | None, Some `Readers ->
        invalid_arg "Eio_fs_unix.Lease.take: block is `Readers, with no gate"
    | None, (Some #block as block) | None, (None as block) ->
        taken ~sw ~lease:`Exclusive ?block ?create ?contents ?on_wait path

  let with_exclusive ?gate ?block ?create ?contents ?on_wait path fn =
    Eio.Switch.run @@ fun sw ->
    let t = take ~sw ?gate ?block ?create ?contents ?on_wait path in
    Fun.protect ~finally:(fun () -> release t) (fun () -> fn t)

  let with_shared ?block ?create ?on_wait path fn =
    Eio.Switch.run @@ fun sw ->
    let t = taken ~sw ~lease:`Shared ?block ?create ?on_wait path in
    Fun.protect ~finally:(fun () -> release t) (fun () -> fn t)
end

(* A backend-free component takes its critical section as an
   [Eio_fs_sync.Lock.t]; this is that section over a lock file of this
   filesystem's. Everything [with_lock] raises before the body begins becomes
   [Eio_fs_sync.Lock.Unavailable]; what the body raises travels out untouched. *)
module Rendezvous = struct
  (* The lock file is captured in the closure, keeping the section
     polymorphic in its result. *)
  type t = { enter : 'a. (unit -> 'a) -> 'a }

  let use t f = t.enter f
end

module Section = Eio_fs_sync.Lock.Make (Rendezvous)

let fail_unavailable fmt =
  Fmt.kstr (fun m -> raise (Eio_fs_sync.Lock.Unavailable m)) fmt

let enter_lock ?block ?create ?contents path f =
  let began = ref false in
  let section () =
    began := true;
    f ()
  in
  match with_lock ?block ?create ?contents path section with
  | answer -> answer
  | exception Lock_unavailable refusal when not !began ->
      fail_unavailable "%a" pp_refusal refusal
  | exception
      Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied (Not_a_lock_file name)), _)
    when not !began ->
      fail_unavailable "%s is not the lock's own regular file" name
  | exception (Eio.Io _ as e) when not !began ->
      fail_unavailable "%s cannot be locked: %s"
        (Eio_fs_internal.native_name path)
        (Printexc.to_string e)
  | exception Invalid_argument m when not !began -> fail_unavailable "%s" m

let lock_section ?block ?create ?contents path =
  Section.v
    { Rendezvous.enter = (fun f -> enter_lock ?block ?create ?contents path f) }
