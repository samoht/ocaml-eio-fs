# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
to test what a program does when a write fails or the power goes out.

A test gets a filesystem of its own and uses it through `Eio.Path`, as it
would a directory on disk, so the code under test does not change:

```ocaml
# Eio_main.run @@ fun _env ->
  let fs = Eio_fs_mem.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(fs / "hello") "world";
  Eio.Path.load Eio.Path.(fs / "hello");;
- : string = "world"
```

You would reach for eio-fs when:

- a test needs a directory to throw away and should leave the host's disk
  alone ([a disposable directory](#a-disposable-directory));
- you want to know whether a program still starts after a disk error or a
  power cut ([testing a program](#testing-a-program));
- you wrote a filesystem for Eio and want it to answer as a real one does
  ([testing a filesystem](#testing-a-filesystem));
- a program has to replace a file without a power cut leaving it half
  written ([saving a file](#saving-a-file)).

## Installation

<!-- $MDX skip -->
```sh
opam install eio-fs
```

eio-fs needs OCaml 5.4 or later and Eio 1.7 or later.

## Usage

### A disposable directory

`Eio_fs_mem.v ()` is the root of a new, empty filesystem that lives in
memory. Two calls give two filesystems that share no file.

It answers as Linux does by default. With `~os` it answers as macOS or
Windows does, whatever the host, with the answers recorded on real machines.
On macOS, for example, two names that differ only by case are the same file:

```ocaml
# Eio_main.run @@ fun _env ->
  let fs = Eio_fs_mem.v ~os:`Macos () in
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(fs / "a") "x";
  Eio.Path.load Eio.Path.(fs / "A");;
- : string = "x"
```

`Eio_fs_mem.load` copies an existing directory into a new in-memory
filesystem. For a tree of test fixtures, the `eio-fs-crunch` binary writes a
directory as an OCaml module, and that module's `load ()` gives the tree back
as a filesystem. [`eio_fs_mem.mli`](lib/mem/eio_fs_mem.mli) says what each
operating system changes and where the simulation stops.

### Testing a program

`Eio_fs_chaos.run` takes the program as a function of its environment, the
value `Eio_main.run` would give it. It runs the program many times, each time
on a fresh in-memory disk at `env#fs`:

```ocaml
# Eio_fs_chaos.run @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / "hello") "world";;
- : unit = ()
```

The first run has no fault. Each later run meets one or two, such as a write
that fails or a disk that fills up. In some runs the power goes out, and the
disk keeps only part of what was written. After each of those runs, the
program starts again with no fault on what the disk kept, as a service does
after a reboot. The check fails if a run hangs, if the program raises
something other than an `Eio.Io` error, or if the restart fails. It then
raises `Failure` with a report, so the call is the body of a test in any test
runner.

A restart cannot see lost data, because the program writes it again. So the
program calls `Eio_fs_chaos.ack env file` once it has promised that a file is
safe on disk, and the check makes sure no fault loses it. The `save` below
writes a temporary file and syncs it, so that its bytes reach the disk. It
then renames the file over the old one:

```ocaml
# let save fs v =
    let tmp = Eio.Path.(fs / "config.tmp") in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
        Eio.Flow.copy_string v f;
        Eio.File.sync f);
    Eio.Path.rename tmp Eio.Path.(fs / "config");;
val save : [> Eio.Fs.dir_ty ] Eio.Path.t -> string -> unit = <fun>
# match
    Eio_fs_chaos.run (fun env ->
        save env#fs "new";
        Eio_fs_chaos.ack env Eio.Path.(env#fs / "config"))
  with
  | () -> print_endline "passed"
  | exception Failure report -> print_endline report;;
Eio_fs_chaos.run: a power cut lost an acknowledged file: /config held "new" when acknowledged after #4, and now is missing
  cut:      at the end, every unsynced change lost
  faults:   none
  acked:    1 value, after #4
  lost:     #1 create /config.tmp; #4 rename /config.tmp to /config
  trace:
    #1 open-write /config.tmp
    #2 write /config.tmp@0+3
    #3 sync /config.tmp
    #4 rename /config.tmp to /config
  replay:   Eio_fs_chaos.run ~replay:"fs/7a2e6ba7/none|end|drop" app
- : unit = ()
```

The power cut lost the rename, because `save` synced the file and never its
directory. The last line of the report, pasted into the test, runs that one
case again.

A program that also uses the network is tested on both at once, with
[eio-chaos](https://github.com/samoht/ocaml-eio-chaos) and
[eio-net](https://github.com/samoht/ocaml-eio-net):
`Eio_chaos.run [ Eio_fs_chaos.disk (); Eio_net_chaos.network () ] app`. The
eio-chaos README says how the faults are chosen and what a pass means.
[`eio_fs_chaos.mli`](lib/chaos/eio_fs_chaos.mli) documents the rest, such as
how to start the first run on a disk that is not empty.

### Testing a filesystem

`Eio_fs_chaos.conformance` is for a filesystem of your own, such as a wrapper
over another one. It takes a function that makes a fresh, empty directory. It
runs short scenarios in that directory and compares each answer with the one
recorded on a real machine:

```ocaml
# Eio_fs_chaos.conformance (fun ~sw:_ _env -> Eio_fs_mem.v ());;
- : unit = ()
```

The scenarios cover the places where filesystems disagree, such as case and
Unicode forms in names, long paths, symbolic links and permissions. The check
reports each scenario whose answer differs. It compares with Linux by default,
and with macOS or Windows with `~os`. With `~under`, a filesystem built over
other devices, such as one over a network, is also checked while those
devices fail.

### Saving a file

`Eio_fs.Sync.save path contents` replaces a file so that a power cut leaves
the old contents or the new ones. It writes a temporary file and syncs it.
It then renames the file into place and syncs the directory, so it passes the
check of [testing a program](#testing-a-program):

```ocaml
# Eio_fs_chaos.run @@ fun env ->
  let config = Eio.Path.(env#fs / "config") in
  Eio_fs.Sync.save config "new";
  Eio_fs_chaos.ack env config;;
- : unit = ()
```

On macOS a sync can return while the data is still in the drive's cache.
`~barrier:Eio_fs_unix.barrier` asks the drive to flush its cache as well.
[`eio_fs_sync.mli`](lib/sync/eio_fs_sync.mli) documents the other ways to
publish a file.

## Libraries

- `eio-fs` has `Eio_fs`, helpers for any Eio filesystem, such as durable
  saves and tree walks.
- `eio-fs.mem` has `Eio_fs_mem`, the in-memory filesystem.
- `eio-fs.chaos` has `Eio_fs_chaos`, the two checks above.
- `eio-fs.unix` has `Eio_fs_unix`, the POSIX calls that Eio lacks, such as
  file locks and extended attributes. It is not built on Windows.

Each library documents itself in its `.mli`, under [`lib/`](lib/).

## Related work

`conformance` checks a filesystem against recordings of real kernels, where
[SibylFS](https://sibylfs.github.io/) checked real filesystems against a
model of POSIX. The power cut follows the crash model of
[Ferrite](https://github.com/uwplse/ferrite).

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory filesystem started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and `Eio_fs.Path` adapts
its modification of [fpath](https://github.com/dbuenzli/fpath).
