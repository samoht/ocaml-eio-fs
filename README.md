# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
with fault injection for crash-consistency tests.

Code written against `Eio.Path` runs the same on a disk or in memory. eio-fs
provides the in-memory side, for tests and for unikernels that have no disk.
Each filesystem it creates is separate from the others. Its chaos tests run
a workload under filesystem errors and power cuts.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-fs
```

## Usage

`Eio_fs.Fs.v ()` returns the root of a new, empty filesystem in memory. The
code that uses it sees an ordinary `Eio.Path`. The examples below link `eio-fs`
and `eio_main`.

```ocaml
Eio_main.run @@ fun _env ->
let fs = Eio_fs.Fs.v () in
Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / "hello") "world";
assert (Eio.Path.load Eio.Path.(fs / "hello") = "world")
```

`Eio_fs.Fs.load dir` returns a new in-memory filesystem holding a copy of the
tree at `dir`. `Eio_fs.Path.pp_tree` prints a tree on any backend.

## Chaos tests

`Eio_fs.Chaos.check` runs a workload many times on fresh in-memory
filesystems, each time with a different fault: an open, a write or a sync
fails, a write moves one byte or is torn, the filesystem turns read-only or
full. It chooses the faults from what the workload does.

Here a cache must keep working whatever the filesystem does: any error only
costs a recomputation. An entry ends with `$`, so a torn one is not trusted.

```ocaml
# let get dir key ~compute =
    let path = Eio.Path.(dir / key) in
    let soft f = try Some (f ()) with Eio.Io _ -> None in
    match soft (fun () -> Eio.Path.load path) with
    | Some v when String.ends_with ~suffix:"$" v ->
        String.sub v 0 (String.length v - 1)
    | _ ->
        let v = compute key in
        let tmp = Eio.Path.(dir / (key ^ ".tmp")) in
        ignore
          (soft (fun () ->
               Eio.Path.save ~create:(`Or_truncate 0o644) tmp (v ^ "$");
               Eio.Path.rename tmp path));
        v;;
val get :
  [> Eio.Fs.dir_ty ] Eio.Path.t ->
  string -> compute:(string -> string) -> string = <fun>
# Eio_fs.Chaos.check (fun env ->
    for _ = 1 to 2 do
      let v = get env#fs "k" ~compute:String.uppercase_ascii in
      if v <> "K" then Fmt.failwith "got %S" v
    done);;
- : unit = ()
```

`Eio_fs.Chaos.Crash.check` cuts the power during a workload, and during its
recovery. The workload calls `ack v` once its program has promised that `v` is
durable. After each cut, `recover ~acked` runs as a fresh process would, on
the directory as the disk holds it, and checks what survived. Here a config
is saved with a temporary file, a sync, a rename and a sync of the directory:

```ocaml
# let save ~sync_dir dir v =
    let tmp = Eio.Path.(dir / "config.tmp") in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
        Eio.Flow.copy_string v f;
        Eio.File.sync f);
    Eio.Path.rename tmp Eio.Path.(dir / "config");
    if sync_dir then Eio_fs_sync.sync_dir dir;;
val save : sync_dir:bool -> [> Eio.Fs.dir_ty ] Eio.Path.t -> string -> unit =
  <fun>
# let test ~sync_dir () =
    Eio_fs.Chaos.Crash.check
      ~setup:(fun env -> save ~sync_dir:true env#fs "old")
      (fun ~ack env ->
        match save ~sync_dir env#fs "new" with
        | () -> ack ()
        | exception Eio.Io _ -> ())
      ~recover:(fun ~acked env ->
        match (Eio.Path.load Eio.Path.(env#fs / "config"), acked) with
        | "new", _ | "old", [] -> ()
        | v, _ -> Fmt.failwith "config is %S" v);;
val test : sync_dir:bool -> unit -> unit = <fun>
# test ~sync_dir:true ();;
- : unit = ()
```

Without the sync of the directory, the rename may be lost after the program
said the config was saved:

```ocaml
# let cut f =
    match f () with
    | () -> "passed"
    | exception Failure m -> List.nth (String.split_on_char '\n' m) 1;;
val cut : (unit -> unit) -> string = <fun>
# cut (test ~sync_dir:false);;
- : string = "  cut:      at the end, every unsynced change lost"
```

The failure also gives the tier that found it, the operations of the failing
run, and a `~replay:"..."` argument that runs that scenario alone.

The tests follow a few rules:

- At most two faults reach a run, or one lasting condition. A program that
  tries three times gets past two faults, but no retry gets past a lasting
  condition, so the workload must accept that any operation fails.
- A lasting condition, such as a read-only filesystem, reaches every
  operation from where it starts, the retries included. A program that
  retries at once with no time passing fails as a hang.
- `recover` stands for a fresh process: it reads what the disk holds and
  what the workload acknowledged, never state the workload left in memory.
- A cut cancels the workload, as `Eio.Cancel` cancels a fiber, and every
  fiber it runs once they all wait: it must not catch
  `Eio.Cancel.Cancelled`. Any other exception that escapes the workload fails
  the check, an injected `Eio.Io` included, so the workload catches the errors
  its program may raise and checks the results itself.
- A check blocks the event loop it is called from until it ends. Each run has
  its own event loop, whose clock moves only when every fiber waits, so a
  timeout of an hour takes no time.

A check is deterministic, reads no environment variable and prints nothing.
For a scenario it does not try, `Eio_fs.Chaos.wrap f dir` asks `f` about each
operation, and `Eio_fs.Chaos.Crash.v` builds a power cut by hand.

## Testing as a user

Code that passes its tests as root in CI can fail for a user, or the reverse:
a configuration directory it cannot create, a file another user owns, a mode
that locks it out. An in-memory filesystem runs its operations as a user and
checks them as POSIX does. The default user is root, which passes every check,
so a test names a user to find these bugs. `Eio_fs.Fs.as_user` runs a
directory's operations as another user, to set a tree up as root first.

This test saves a configuration file under `/etc`, which root owns. It works as
root and fails as a user.

```ocaml
Eio_main.run @@ fun _env ->
let alice = { Eio_fs.uid = 1000; gid = 1000; groups = []; umask = 0o022 } in
let save_config user =
  Eio_fs.run ~user ~cwd:"/home/alice" @@ fun env ->
  let dir = Eio.Path.(env#fs / "/etc/app") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(dir / "conf") "x"
in
save_config Eio_fs.root;
match save_config alice with
| () -> assert false
| exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ()
```

## Embedding a file tree

The `eio-fs-crunch` binary turns a directory tree into an OCaml module, as
[ocaml-crunch](https://github.com/mirage/ocaml-crunch) does. With `-o assets.ml`
it writes the module and its interface, `assets.mli`. This dune rule
regenerates them when the `static` directory changes:

```
(rule
 (targets assets.ml assets.mli)
 (deps
  (source_tree static))
 (action
  (run %{bin:eio-fs-crunch} -o assets.ml static)))

(library
 (name assets)
 (modules assets)
 (libraries eio-fs eio))
```

`Assets.load ()` rebuilds the tree in a new in-memory filesystem and returns its
root. The program links the `assets` library. The example below uses the module built from
[examples/readme/static](examples/readme/static).

```ocaml
Eio_main.run @@ fun _env ->
let fs = Assets.load () in
assert (
  Eio.Path.load Eio.Path.(fs / "index.html") = "<h1>Hello from the image</h1>\n")
```

## Libraries

- `eio-fs` holds the in-memory filesystem, `Eio_fs.Chaos` and the support code
  for `eio-fs-crunch`.
- `eio-fs.sync` writes a file atomically and durably on any backend.
- `eio-fs.unix` binds POSIX calls that Eio and `Unix` lack, such as advisory
  locks and extended attributes, and has a root whose stats, opens and
  directory listings run on the calling thread rather than a system thread
  (`Eio_fs_unix.inline_fs`).
- `eio-fs.path` does lexical operations on path strings.

## What a test covers

### What a pass tells you

A power-cut check models ext4 with `data=writeback`: unsynced writes may be
lost, and directory changes reach the disk in program order. A pass holds for
that model, not for APFS. On macOS, `fsync` stops at the drive's cache: pass
`Eio_fs_unix.barrier` to `Eio_fs_sync` so that a sync reaches the disk.

Where POSIX systems differ, the in-memory filesystem answers as Linux does. It
has `fs.protected_symlinks` on, as most distributions do: in a sticky
directory others may write, such as `/tmp`, a link is followed only by its
owner or the directory's.

### What a check can miss

- A race during a slow sync. A filesystem operation takes no time, and no
  other fiber runs during it. Force the interleaving with an
  `Eio_fs.Chaos.wrap` hook.
- A read after a failed sync. The check keeps the lost writes in the page
  cache, where Linux may evict them. Do not trust a read after a failed sync.
- Stale data from another offset. A new block gets stale data only from the
  same offset of a freed block, and a removed file frees its blocks while
  still open. ext4 may use any offset, and waits for the last close. Checksum
  what you read back.
- A bug that needs two faults. Some pairs of faults lie past the default
  budget of 20,000 runs. Raise `~budget` in a nightly run, as the
  `CHAOS_BUDGET` example in `Eio_fs.Chaos` does.

### How big a test can be

A run that makes 100,000 filesystem operations, or 10 million scheduler
turns, fails as a busy loop or as too large for a check. The budget also has a
floor: the linear tier always runs whole, so a check that needs more runs than
its budget fails and says so. Shrink the workload, or raise `~budget`.

### Where it differs from eio_posix

Code that runs on both should avoid these:

- `x/.`, `x/d/..` and trailing slashes. The in-memory filesystem resolves them
  as Linux does. eio_posix rewrites the first two lexically
  ([eio#976](https://github.com/ocaml-multicore/eio/issues/976)), and fails a
  creating open with a trailing slash differently
  ([eio#975](https://github.com/ocaml-multicore/eio/issues/975)).
- Absolute symbolic links. Through the root `Eio_fs.Fs.v` returns, one is
  followed where eio_posix refuses it. Use relative targets.
- Times, block counts and link counts. An in-memory stat sets them all to
  zero. Test code that compares modification times on a disk.

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory backend started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and
`eio-fs.path` adapts its POSIX-only modification of
[fpath](https://github.com/dbuenzli/fpath).
