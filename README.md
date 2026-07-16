# eio-fs

Filesystem helpers and an in-memory [Eio](https://github.com/ocaml-multicore/eio)
backend.

The library works with the standard `Eio.Path` interface. `Eio_fs.Fs.v ()`
creates a fresh filesystem in memory, `Eio_fs.Fs.load` populates it from an
existing directory tree, and `Eio_fs.Fs.tree` renders any `Eio.Path` tree.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-fs
```

## Usage

```ocaml
Eio_main.run @@ fun _env ->
let fs = Eio_fs.Fs.v () in
Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / "hello") "world";
assert (Eio.Path.load Eio.Path.(fs / "hello") = "world")
```

The in-memory provider is useful for tests and for scratch filesystems in
unikernels: application code receives an ordinary `Eio.Path` and does not need
to know which backend supplies it.

## Provenance

Adapted from Patrick Ferris's
[eio_mem](https://tangled.org/patrick.sirref.org/eio_mem), which is itself
derived from the Eio filesystem interfaces. Its path handling also derives from
[fpath](https://github.com/dbuenzli/fpath). All are ISC licensed; see
`LICENSE.md`.
