# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio).

`Eio_fs.Fs.v ()` builds an `Eio.Fs.dir_ty Eio.Path.t` that lives entirely
in memory, so any code written against the `Eio.Path` API runs against it
unchanged -- no block device, no host filesystem. `Eio_fs.Fs.load`
replicates a real directory tree into it.

```ocaml
Eio_main.run @@ fun _env ->
let fs = Eio_fs.Fs.v () in
Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / "hello") "world";
assert (Eio.Path.load Eio.Path.(fs / "hello") = "world")
```

It is the diskless analogue of a block-backed `Eio.Path` such as
`notafs`: unikernels and tests get a working repository or scratch tree
with no storage backend.

## Provenance

Adapted from Patrick Ferris's
[eio_mem](https://tangled.org/patrick.sirref.org/eio_mem), which is itself
derived from the Eio filesystem interfaces. `lib/path.ml` is a POSIX-only
modification of [fpath](https://github.com/dbuenzli/fpath). All are ISC
licensed; see `LICENSE.md`.
