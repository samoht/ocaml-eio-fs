open! Hermetic

type kind = Op.kind
type cls = Forwarded | Intercepted of kind | Synthesized | Unsupported
type iface = Dir | Read | Write | Dir_handler | File_handler
type t = { iface : iface; name : string; cls : cls; why : string }
type none = |

let absurd : none -> 'a = function _ -> .

(* The entries of an interface's operations are made where a module of the
   interface's signature defines them, over an empty type: an operation Eio
   adds to the signature leaves the module short of a value. *)
let operations =
  let entries = ref [] in
  let op iface name cls why v =
    entries := { iface; name; cls; why } :: !entries;
    v
  in
  let dir name cls why v = op Dir name cls why v
  and read name cls why v = op Read name cls why v
  and write name cls why v = op Write name cls why v in
  let module _ : Eio.Fs.Pi.DIR = struct
    type t = none

    let open_in =
      dir "open_in" (Intercepted `Open_read)
        "A fault refuses the open before the base opens, or swaps the name for \
         a link after; the file is wrapped."
        absurd

    let open_out =
      dir "open_out" (Intercepted `Open_write)
        "A fault refuses the open before the base opens, or swaps the name for \
         a link after; the file is wrapped."
        absurd

    let mkdir =
      dir "mkdir" (Intercepted `Change)
        "A fault refuses the change before the base makes it." absurd

    let open_subtree =
      dir "open_subtree" (Intercepted `Open_read)
        "A fault refuses the open before the base opens; the directory is \
         wrapped under the same schedule."
        absurd

    let read_dir =
      dir "read_dir" (Intercepted `Read_dir)
        "A fault refuses the listing before the base lists." absurd

    let with_dir_entries =
      dir "with_dir_entries" (Intercepted `Read_dir)
        "A fault refuses the listing before the base lists." absurd

    let stat =
      dir "stat" (Intercepted `Stat)
        "A fault refuses the stat before the base answers, or swaps the name \
         for a link after."
        absurd

    let unlink =
      dir "unlink" (Intercepted `Change)
        "A fault refuses the change before the base makes it." absurd

    let rmdir =
      dir "rmdir" (Intercepted `Change)
        "A fault refuses the change before the base makes it." absurd

    let rename =
      dir "rename" (Intercepted `Change)
        "A fault refuses the change before the base makes it; a target \
         directory that is a wrapper is unwrapped for the base."
        absurd

    let read_link =
      dir "read_link" Forwarded
        "Reading a link changes nothing and fails only as the base does." absurd

    let symlink =
      dir "symlink" (Intercepted `Change)
        "A fault refuses the change before the base makes it."
        (fun ~link_to:_ -> absurd)

    let chmod =
      dir "chmod" (Intercepted `Change)
        "A fault refuses the change before the base makes it." absurd

    let chown =
      dir "chown" (Intercepted `Change)
        "A fault refuses the change before the base makes it."
        (fun ~follow:_ ?uid:_ ?gid:_ -> absurd)

    let sync_dir =
      dir "sync_dir" (Intercepted `Sync)
        "A fault refuses the barrier before the base's barrier runs." absurd

    let pp =
      dir "pp" Forwarded "A path through the wrapper is named as the base's."
        (fun _ -> absurd)

    let native =
      dir "native" Forwarded
        "The base's native path, so a lock or a barrier taken natively reaches \
         the same file."
        absurd

    let split =
      dir "split" Synthesized
        "POSIX syntax, which no instance carries and every base here uses."
        (fun _ -> None)

    let join =
      dir "join" Synthesized
        "POSIX syntax, which no instance carries and every base here uses."
        (fun a _ -> a)
  end
  in
  let module Read : Eio.File.Pi.READ with type t = none = struct
    type t = none

    let read_methods =
      read "read_methods" Synthesized
        "None, so every read goes through single_read and its fault." []

    let single_read =
      read "single_read" (Intercepted `Read)
        "A fault refuses the read or shortens the buffer before the base reads."
        absurd

    let pread =
      read "pread" (Intercepted `Read)
        "A fault refuses the read or shortens the buffers before the base \
         reads."
        absurd

    let stat =
      read "stat" (Intercepted `Stat)
        "A fault refuses the stat before the base answers." absurd

    let seek =
      read "seek" Forwarded
        "The base moves its offset; the wrapper keeps the one it reports \
         operations at."
        absurd

    let close =
      read "close" Forwarded
        "The base closes, at most once, as it would; the switch the file was \
         opened in closes it too."
        absurd
  end
  in
  let module _ : Eio.File.Pi.WRITE = struct
    include Read

    let single_write =
      write "single_write" (Intercepted `Write)
        "A fault refuses the write, shortens it, or tears it after part \
         reached the base."
        absurd

    let copy =
      write "copy" Synthesized
        "Eio's simple copy through the wrapper's single_write, so each chunk \
         is a write a fault may reach."
        absurd

    let pwrite =
      write "pwrite" (Intercepted `Write)
        "A fault refuses the write, shortens it, or tears it after part \
         reached the base."
        absurd

    let sync =
      write "sync" (Intercepted `Sync)
        "A fault refuses the sync and tells the base it failed, before the \
         base syncs."
        absurd

    let truncate =
      write "truncate" (Intercepted `Truncate)
        "A fault refuses the truncation before the base truncates." absurd
  end
  in
  List.rev !entries

let dir_handler name cls why = { iface = Dir_handler; name; cls; why }
let file_handler name cls why = { iface = File_handler; name; cls; why }

let close_dir =
  dir_handler "close" Forwarded
    "The base closes, at most once, as it would; the switch the directory was \
     opened in closes it too."

let admin =
  dir_handler "Eio_fs_internal.Admin" Synthesized
    "The base's root rights, wrapped under the same schedule, for Crash to \
     read and rebuild the tree through."

let base_dir =
  dir_handler "Eio_fs_internal.Base_dir" Synthesized
    "The base, for this package's own code alone: a rename into a wrapper, and \
     eio-fs.unix's host descriptor."

let rebase =
  dir_handler "Eio_fs_internal.Rebase" Synthesized
    "The same wrapper, under the same schedule, over another base: \
     Eio_fs_mem.as_user runs it as another user."

let exclusive =
  dir_handler "Eio_fs.Sync.Rename.Exclusive" (Intercepted `Change)
    "A fault refuses the rename before the base makes it."

let process_local =
  dir_handler "Eio_fs.Sync.Process_local" Forwarded
    "The wrapper adds no opener, so who can open the files is the base's to \
     say."

let modeless =
  dir_handler "Eio_fs.Sync.Modeless" Forwarded
    "Whether modes are kept is the base's to say."

let dir_sync =
  dir_handler "Eio_fs.Sync.Dir_sync" (Intercepted `Sync)
    "A fault refuses the barrier before the base's barrier runs."

let adopt =
  dir_handler "Eio_fs.Sync.Adopt" (Intercepted `Open_write)
    "A file of the base the caller holds is an open of its path: a fault \
     refuses it before the base adopts it, or swaps the name for a link after; \
     the file is wrapped."

let dir_other =
  dir_handler "a base's own interface" Unsupported
    "Not carried over, as anything bound to the base steps around the \
     schedule: a wrapper over a host directory is no native directory a child \
     process starts in."

let base_file =
  file_handler "Eio_fs_internal.Base_file" Synthesized
    "The base, for this package's own code alone: eio-fs.unix takes the host \
     descriptor a lock is taken on from it."

let file_sync =
  file_handler "Eio_fs.Sync.File_sync" (Intercepted `Sync)
    "A fault refuses the barrier and tells the base it failed, before the \
     base's barrier runs; a file opened for reading answers it too, as fsync \
     does."

let sync_failed =
  file_handler "Eio_fs_internal.Sync_failed" Forwarded
    "A sync a wrapper above failed is told on to the base, so stacked wrappers \
     reach the power-loss model at the bottom."

let shutdown =
  file_handler "Eio.Flow.Pi.Shutdown" Unsupported
    "A file is no socket: neither the in-memory backend's handle nor \
     eio_posix's binds it."

(* Eio hides this one, so [binding] cannot name it. *)
let socket =
  file_handler "Eio.Net.Pi.Socket" Unsupported
    "A file is no socket: neither the in-memory backend's handle nor \
     eio_posix's binds it."

let read_only_write =
  file_handler "write side of a file opened for reading" Unsupported
    "A file opened for reading is no sink: a backend's handle binds the write \
     side and refuses each write with EBADF, where the wrapper binds none; it \
     syncs through File_sync."

let file_other =
  file_handler "a base's own interface" Unsupported
    "Not carried over, as anything bound to the base steps around the \
     schedule: a file opened through a wrapper hands out no host descriptor."

let handlers =
  [
    close_dir;
    admin;
    base_dir;
    rebase;
    exclusive;
    process_local;
    modeless;
    dir_sync;
    adopt;
    dir_other;
    base_file;
    file_sync;
    sync_failed;
    shutdown;
    socket;
    read_only_write;
    file_other;
  ]

let all = operations @ handlers

let get iface name =
  List.find (fun e -> e.iface = iface && String.equal e.name name) all

(* A file's Close is its READ close. *)
let binding (type a) iface (Eio.Resource.H (pi, _) : a Eio.Resource.binding) =
  match pi with
  | Eio.Fs.Pi.Dir -> `Interface Dir
  | Eio.File.Pi.Read -> `Interface Read
  | Eio.Flow.Pi.Source -> `Interface Read
  | Eio.File.Pi.Write -> `Interface Write
  | Eio.Flow.Pi.Sink -> `Interface Write
  | Eio.Resource.Close when iface = Dir_handler -> `Op close_dir
  | Eio.Resource.Close -> `Op (get Read "close")
  | Eio.Flow.Pi.Shutdown -> `Op shutdown
  | Eio_fs_internal.Admin -> `Op admin
  | Eio_fs_internal.Base_dir -> `Op base_dir
  | Eio_fs_internal.Rebase -> `Op rebase
  | Eio_fs_internal.Base_file -> `Op base_file
  | Eio_fs_internal.Sync_failed -> `Op sync_failed
  | Eio_fs_sync.Rename.Exclusive -> `Op exclusive
  | Eio_fs_sync.Process_local -> `Op process_local
  | Eio_fs_sync.Modeless -> `Op modeless
  | Eio_fs_sync.Dir_sync -> `Op dir_sync
  | Eio_fs_sync.Adopt -> `Op adopt
  | Eio_fs_sync.File_sync -> `Op file_sync
  | _ -> `Other

let pp_iface ppf = function
  | Dir -> Fmt.string ppf "Eio.Fs.Pi.DIR"
  | Read -> Fmt.string ppf "Eio.File.Pi.READ"
  | Write -> Fmt.string ppf "Eio.File.Pi.WRITE"
  | Dir_handler -> Fmt.string ppf "directory handler"
  | File_handler -> Fmt.string ppf "file handler"

let pp_cls ppf = function
  | Forwarded -> Fmt.string ppf "forwarded"
  | Intercepted k -> Fmt.pf ppf "intercepted (%s)" (Op.kind_name k)
  | Synthesized -> Fmt.string ppf "synthesized"
  | Unsupported -> Fmt.string ppf "unsupported"

let pp ppf e = Fmt.pf ppf "%a %s" pp_iface e.iface e.name
