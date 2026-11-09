# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
with fault injection for crash-consistency tests.

Code written against `Eio.Path` runs the same on a disk or in memory. eio-fs
provides the in-memory side, for tests and for unikernels that have no disk.
Each filesystem it creates is separate from the others. It can also wrap any
directory to inject faults or simulate a power cut.

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

## Testing crash consistency

`Eio_fs.Chaos.wrap` injects faults into a directory, such as torn writes,
failed syncs or a read-only filesystem. A torn write stores part of its bytes
and then fails.

`Eio_fs.Chaos.Crash` simulates a power cut. A write stays unsynced until
`Eio.File.sync` runs on its file. Directory operations go into a journal in the
order they ran. At the crash, each block an unsynced write dirtied may be
lost, and only a prefix of the uncommitted directory operations survives.

One crash can end in many states. The default policy, `Drop_unsynced`, builds
only one of them: the state at the last barriers. `Reorder_unsynced seed` draws
a different state for each seed, so a test loops over seeds.

In this test, a file saved with `Eio_fs_sync.save` survives every crash,
because that call syncs the file and its directory. A file written with no sync
is lost in some of them. The test also links `eio-fs.sync`.

```ocaml
Eio_main.run @@ fun _env ->
let module Crash = Eio_fs.Chaos.Crash in
let lost = ref 0 in
for seed = 0 to 99 do
  let ctl = Crash.v ~policy:(Crash.Reorder_unsynced seed) () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.v ()) in
  Eio_fs_sync.save Eio.Path.(fs / "state") "committed";
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(fs / "draft") "unsynced";
  let after = Crash.post_crash ctl in
  assert (Eio.Path.load Eio.Path.(after / "state") = "committed");
  if Eio.Path.kind ~follow:false Eio.Path.(after / "draft") = `Not_found then
    incr lost
done;
assert (!lost > 0)
```

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
  locks and extended attributes.
- `eio-fs.path` does lexical operations on path strings.

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory backend started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and
`eio-fs.path` adapts its POSIX-only modification of
[fpath](https://github.com/dbuenzli/fpath).
