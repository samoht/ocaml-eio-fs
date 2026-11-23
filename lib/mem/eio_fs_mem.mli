(** In-memory {!Eio.Fs} filesystem.

    For hermetic tests: code reads and writes files through {!Eio.Path} without
    touching the host's disk. Each call to {!v}, {!load}, {!Crunch.load} or
    {!run} makes a filesystem of its own: two of them share no file. The library
    [eio-fs.chaos] tests programs and filesystems with it.

    Where Linux, macOS and Windows differ, the filesystem follows one, chosen by
    [?os]: [`Linux], the default, [`Macos] or [`Windows]. The choice comes from
    the argument, never from the host, so a test sees the same filesystem on
    every machine. {!section-macos} and {!section-windows} list what [`Macos]
    and [`Windows] change. Its answers are those recorded on Linux and macOS
    through the kernel and eio_posix, and on Windows through eio_windows, and
    [Eio_fs_chaos.conformance] checks them.

    An entry's stat carries what Linux gives on tmpfs. Its times come from the
    clock given to {!v}, and stay at [0.] without one; under [Eio_fs_chaos.run]
    they come from the run's clock. A write, a truncation and an open that
    truncates move a file's [mtime] and [ctime]. A chmod, a chown and a rename
    move the entry's [ctime]. Making, removing or renaming an entry moves its
    directory's [mtime] and [ctime]. [atime] follows relatime, Linux's default:
    a read, a listing or a link read moves it when it is not past [mtime] or
    [ctime], or a day after it last moved. [dev] is the filesystem's own, shared
    with no other filesystem and no host device. [nlink] is [2] plus the number
    of subdirectories for a directory, [1] for anything else, and [0] once its
    last name is gone. [blocks] counts the 512-byte blocks of the 4096-byte
    pages writes put bytes in: a hole, a directory and a link hold none.

    A file's bytes live in memory, holes included, and a file holds at most 4
    GiB: a write or a truncation past that raises {!Eio.Fs.File_too_large}.

    Where POSIX systems differ, the filesystem answers under [`Linux] as Linux
    does: {!Eio.Path.unlink} of a directory raises the error EISDIR names, not
    [Permission_denied], a positioned write to a file opened to append lands at
    its end, [Eio.Path.chmod ~follow:false] on a symbolic link raises the error
    EOPNOTSUPP names, and a symbolic link to the empty path is refused with
    [Not_found]. Under [`Macos] it answers as macOS does: see {!section-macos}.
    A write of no bytes returns [0] under either, where eio_posix on macOS
    raises the error EINVAL names from writev(2). Its paths have no native
    spelling ({!Eio.Path.native} is [None]). The root {!v} returns stands for
    [Eio.Stdenv.fs]: an absolute path or link target resolves from it, as
    eio_posix resolves one from [/] through [fs]. One filesystem may be used
    from several domains at once.

    A path resolves one component at a time, as path_resolution(7) describes: a
    [".."] climbs from the directory the walk has reached, a link's target
    included, never from a lexical rewrite of the path. A directory opened from
    the filesystem is confined as eio_posix confines one: an absolute path, an
    absolute link target and a [".."] above that directory are refused with
    [Eio.Fs.Permission_denied], whose message names the path asked for and not
    where the directory sits.

    {b eio_posix.} The filesystem answers as the kernel does, and eio_posix
    differs from the kernel on a few paths. Code that runs on both should not
    depend on them:

    - a path that ends in a ["."] or [".."] component, which eio_posix rewrites
      lexically ({{:https://github.com/ocaml-multicore/eio/issues/976}eio#976});
    - a creating open of a name with a trailing slash, which eio_posix fails
      with another error than the kernel's;
    - on macOS, a path through a directory with no read bit, which eio_posix
      refuses, and a stat through more than 32 symbolic links, which it follows
      up to 40.

    {b Permissions.} Each operation runs as a {!type-user} and meets the POSIX
    rules:

    - search ([x]) on every directory a path walks; write and search on the
      directory where an entry is made, removed or renamed, with the sticky bit
      keeping removal and renaming to the entry's or the directory's owner; read
      on a directory to list it; read, or read and write, on a file to open it.
    - In a sticky directory that others may write, a link is followed as the
      last component of a path only by its owner, or if the directory's owner
      owns it, root included: Linux does so with [fs.protected_symlinks] set, as
      most distributions set it.
    - A new file or directory loses the user's umask bits and belongs to the
      user's uid and gid, or to the group of a setgid directory it is made in.
    - A new directory keeps only the permission and sticky bits of its mode, and
      is setgid iff its parent is.
    - A file made in a setgid directory by a user outside that group loses
      setgid, as Linux strips it at create.
    - Only a file's owner changes its mode, and a user outside the file's group
      cannot set its setgid bit.
    - Only root gives a file away; its owner may move it to one of their own
      groups.
    - A chown of a regular file, or a write to one by a user other than root,
      clears its setuid bit, and its setgid bit when its group may execute it.
    - Root needs no bit to read, write or search, as on Linux, so a test that
      runs as {!root}, the default, misses the bugs a user meets: a directory it
      cannot create in, a file it cannot open. *)

(** {1:errors Errors}

    An error Eio classifies raises the {!Eio.Fs} error of its class:
    [Not_found], [Already_exists], [Permission_denied], [File_too_large], and
    [Symlink] for a path through too many links. Every other error is an
    {!Eio.Io} error of no class, as eio_posix raises it. Each carries {!Eio_fs},
    but [Symlink], which carries nothing, and the [Permission_denied] of a
    confined directory, whose code names the path asked for. Below, "the error
    EISDIR names" is the one whose {!Eio_fs} names EISDIR's error, "is a
    directory", and likewise for each errno. *)

type Eio.Exn.Backend.t +=
  | Eio_fs of string
        (** The backend code of the filesystem's errors: the operation, the
            error and the path, as ["rmdir: directory not empty: /d"]. *)

(** {1:users Users} *)

type user = {
  uid : int64;  (** The user id, as {!Eio.File.Stat.t} carries it. *)
  gid : int64;  (** The primary group id. *)
  groups : int64 list;  (** The supplementary group ids. *)
  umask : int;  (** The mode bits a new file or directory loses. *)
}
(** The type for the user an in-memory filesystem's operations run as. *)

val root : user
(** [root] is uid [0], gid [0], no supplementary group and umask [0o022]. *)

(** {1:filesystems Filesystems} *)

val v :
  ?os:[< `Linux | `Macos | `Windows ] ->
  ?user:user ->
  ?clock:_ Eio.Time.clock ->
  ?drives:char list ->
  unit ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [v ~os ~user ~clock ~drives ()] is the root of a fresh, empty filesystem
    following the kernel [os] (defaults to [`Linux]), whose operations run as
    [user] (defaults to {!root}), who owns its root directory. Its times are
    read from [clock]; without one, they stay at [0.]. Under [`Windows] it has
    the drives [drives] (defaults to [['C']]), and the root is the first one's,
    ["C:\\"]; see {!section-windows}. Closing the capability ends the
    filesystem, as the end of [sw] ends a network of [Eio_net_mem.v]: no
    operation goes through it or its aliases after, and only the directories
    already opened from it keep their own lifetimes.

    @raise Invalid_argument if [drives] is given for another [os]. *)

val as_user : user -> 'a Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [as_user user path] is [path], an in-memory directory, with its operations
    run as [user]. A test sets a tree up as {!root} and runs the code under test
    as a user. A directory opened from the result shares its descriptor with
    [path]'s. A wrapper of [Eio_fs_chaos] over an in-memory directory, such as a
    run's [env#fs], stays the same wrapper, over the directory as [user].

    @raise Invalid_argument
      if [path] is neither an in-memory directory nor such a wrapper. *)

val load :
  ?os:[< `Linux | `Macos | `Windows ] ->
  ?clock:_ Eio.Time.clock ->
  ?filter:('a Eio.Path.t -> bool) ->
  'a Eio.Path.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [load ~os ~clock ~filter path] is the root of a new in-memory filesystem
    that answers as [os] (defaults to [`Linux]) does, holding a copy of the
    directory tree at [path], at the same path from its root:
    [load (dir / "a/b")] holds [/a/b]. [path] is followed if it is a symbolic
    link; the entries below it are copied as they are, a link kept a link.
    [filter] (defaults to [fun _ -> true]) is called on [path], then on every
    path below it, before anything of it is copied: a path for which it is
    [false] is not copied, nor anything under it, so a refused [path] is an
    empty filesystem. [path] can be on any filesystem, an in-memory one
    included. Its times are read from [clock], as {!v} reads them: a copy is a
    new file, made now.

    An entry that is neither a regular file, a directory nor a symbolic link is
    not copied; it is traced with [Eio.traceln].

    @raise Eio.Io
      if [path] is not a directory, if it or an entry under it cannot be read,
      or if a regular file is replaced between the stat that found it and the
      open that copies it: that open does not follow a link. *)

module Crunch = Crunch
(** Directory trees embedded in an executable, loaded as {!load} loads a copy.
*)

module Resource = Eio_fs_mem_core.Low_level.Resource
(** File descriptor-backed resources. *)

type stdenv =
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; cwd : Eio.Fs.dir_ty Eio.Path.t
  ; stdin : Eio.Flow.source_ty Eio.Flow.source
  ; stdout : Eio.Flow.sink_ty Eio.Flow.sink
  ; stderr : Eio.Flow.sink_ty Eio.Flow.sink
  ; process_mgr : [ `Generic ] Eio.Process.mgr_ty Eio.Process.mgr >
(** The type for the standard environment {!run} passes on: an in-memory
    filesystem, current directory and stdio.

    The process manager only supports creating and using pipes. All other kinds
    of functionality will fail. *)

val run :
  ?os:[< `Linux | `Macos | `Windows ] ->
  ?user:user ->
  ?clock:_ Eio.Time.clock ->
  ?drives:char list ->
  ?cwd:string ->
  ?stdin:_ Eio.Flow.source ->
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  (stdenv -> 'a) ->
  'a
(** [run ~os ~user ~clock ~drives ~cwd fn] calls [fn] with a new in-memory
    filesystem in place of the host's, that answers as [os] (defaults to
    [`Linux]) does, with [drives] as {!v} takes them, whose times are read from
    [clock], as {!v} reads them. Unlike [Eio_main.run], it starts no event loop
    and installs no effect handler: call it inside one, such as [Eio_main.run]
    or [Eio_mock.Backend.run]. [fn]'s argument holds the root and the current
    directory of the new filesystem. The current directory is the directory at
    the absolute path [cwd] (defaults to ["/"], or the first drive's root under
    [`Windows]), made with its missing ancestors, mode [0o755], before [fn]
    runs. Like a process's, it follows that directory through a rename. Both run
    as [user] (defaults to {!root}). Root owns the tree and gives [user] the
    current directory.

    You may optionally pass values for the standard IO. The default is to use an
    {! Eio.Flow.null}.

    @raise Invalid_argument if [cwd] is not an absolute path. *)

(** {1:macos macOS}

    Under [`Macos] the filesystem answers as macOS does on its default APFS
    volume, where it differs from Linux:

    - A name matches every name with the same canonical caseless form: case and
      Unicode normalisation do not count. An entry keeps the spelling it was
      made with. A rename to another spelling of its own name respells it; one
      over another entry keeps that entry's spelling. A name that is not UTF-8
      is refused with the error EILSEQ names, and one longer than 255 UTF-16
      code units with ENAMETOOLONG's.
    - {!Eio.Path.unlink} of a directory raises [Permission_denied].
    - A last component in directory syntax that is a symbolic link is followed,
      even to make, remove or move an entry: {!Eio.Path.mkdir},
      {!Eio.Path.rmdir} and {!Eio.Path.rename} of ["l/"] act on the link's
      target. A creating open of a name in directory syntax raises [Not_found]
      if it is missing, and the error ENOTDIR or EISDIR names otherwise. A
      rename of a file to a missing name in directory syntax raises [Not_found].
    - {!Eio.Path.rmdir} of a last [".."], and {!Eio.Path.rename} of a last ["."]
      or [".."], raise the error EINVAL names.
    - A positioned write to a file opened to append lands at its offset.
    - A symbolic link may point to the empty path, and
      [Eio.Path.chmod ~follow:false] changes a link's mode.
    - A path follows at most 32 symbolic links.
    - A new entry takes its directory's group, and a new directory keeps only
      the permission bits of its mode.
    - Opening a directory, {!Eio.Path.open_dir} included, needs its read bit.
    - No [fs.protected_symlinks]: any link is followed. *)

(** {1:windows Windows}

    Under [`Windows] the filesystem answers as a program meets Windows through
    eio_windows 1.7, on NTFS, where it differs from Linux. Its paths are Win32
    paths, which [Eio_utils.Nt_path] resolves, as eio_windows resolves them, and
    [Eio.Path.( / )], [split] and [native] are Nt_path's too:

    - The root of the tree holds one directory per drive, ["C:"] by default, and
      {!v}'s root is the first drive's, ["C:\\"]. Through the root an absolute,
      rooted ([\a]), drive-relative ([C:a]) or verbatim ([\\?\C:\a]) path
      resolves as Win32 normalises it: [..] and trailing dots and spaces are
      applied to the path as written, a trailing separator is dropped, and a
      drive-relative path on another drive starts at its root. A rename to
      another drive raises [Permission_denied], as Eio raises EXDEV.
    - Beneath a directory opened with {!Eio.Path.open_dir}, or the current
      directory of {!run}, a name is literal: trailing dots and spaces and
      reserved names such as [NUL], [CON] and [com1.txt] are ordinary names.
      [..] applies to the path as written, before any link is followed, and may
      not leave the directory, even to come back; an absolute path is refused,
      and a link's target is followed only if it stays below the directory, an
      absolute one included. A target whose last name is made of dots and spaces
      only is refused. These refusals raise [Permission_denied].
    - Names match by their characters' simple uppercase, as NTFS's upcase table
      matches them, without Unicode normalisation: [a] and [A] are one name, [e]
      and its NFD spelling two. A rename to another spelling, and one that
      replaces an entry, gives the new spelling. A name that is not UTF-8 is
      read as code page 1252, as OCaml's runtime reads it, and one longer than
      255 UTF-16 code units raises the error whose message is Windows'
      ["The filename, directory name, or volume label syntax is incorrect"].
    - A file or directory a handle holds open, or a directory with one open
      below it, is neither renamed nor removed, nor replaced by a rename:
      [Permission_denied], as eio_windows opens every handle without
      [FILE_SHARE_DELETE]. A delete is so never left pending.
    - A rename replaces a file or an empty directory; a file over a directory, a
      directory into itself or over one above it raise [Permission_denied], a
      directory over a file the error whose message is Windows'
      ["The directory name is invalid"], as do {!Eio.Path.rmdir} of a file and a
      path through one. {!Eio.Path.unlink} of a directory raises
      [Permission_denied].
    - A symbolic link is to a file or to a directory, as its target is when it
      is made, and {!Eio.Path.rmdir} removes one to a directory. Only a user who
      holds [SeCreateSymbolicLinkPrivilege] makes one: here {!root}, the default
      user, as an elevated administrator holds it; any other user is a basic
      user, refused with [Permission_denied], as Windows refuses one without
      Developer Mode. A link to the empty path is refused with the error whose
      message is Windows'
      ["The data present in the reparse point buffer is invalid"]. A path
      follows at most 40 links.
    - Opening a directory as a file raises [Permission_denied]; opening a file
      as a directory, the error ["The directory name is invalid"] names.
    - Unix permissions are not kept: no operation checks one, a file's mode is
      [0o666] and a directory's [0o777], its uid and gid [0]. {!Eio.Path.chown}
      raises the error ["not supported"] names, as does {!Eio.Path.chmod} below
      an opened directory, as eio_windows refuses them; through the root a chmod
      keeps nothing. Its directories declare [Eio_fs.Sync.Modeless], so
      [Eio_fs.Sync] publishes a file without a chmod, as it does on eio_windows.
    - [Eio.File.pread] and [Eio.File.pwrite] raise the error ["not supported"]
      names, as eio_windows does not implement them.
    - A directory sync does nothing, as eio_windows syncs none.

    Not modelled: NTFS's own semantics beyond what eio_windows shows (short 8.3
    names, alternate data streams, attributes such as read-only or hidden,
    junctions, which Eio cannot make, and Developer Mode), UNC and device paths,
    which raise [Permission_denied], each drive's own current directory, which
    Nt_path does not know either, and the durability of a directory on a power
    cut: a power cut under [`Windows] loses what it loses under [`Linux], which
    only a power-cut test on real Windows could check. *)
