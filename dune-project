(lang dune 3.21)

(using mdx 0.4)

(cram enable)

(name eio-fs)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>" "Thomas Leonard" "Patrick Ferris")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (github samoht/ocaml-eio-fs))

(package
 (name eio-fs)
 (synopsis "Filesystem helpers and an in-memory backend for Eio")
 (tags (eio filesystem in-memory))
 (description
  "Helpers for standard [Eio.Path] values and an in-memory filesystem backend.
   [Eio_fs.Fs.v ()] creates a fresh filesystem, [Eio_fs.Fs.load] populates it
   from an existing directory tree, and [Eio_fs.Path.pp_tree] renders a filesystem
   tree. The [eio-fs-crunch] binary embeds a directory tree as an OCaml
   module that rebuilds it in memory at runtime, in the style of ocaml-crunch.
   Adapted from Patrick Ferris's eio_mem
   (https://tangled.org/patrick.sirref.org/eio_mem), which is itself derived
   from the Eio filesystem interfaces.")
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.3))
  (eio (>= 1.6))
  (eio_posix (>= 1.6))
  fmt
  cstruct
  optint
  (cmdliner (>= 1.1.0))
  eio_main
  (mdx :with-test)
  (alcotest :with-test)
  (alcobar :with-test)
  (mtime :with-test)))
