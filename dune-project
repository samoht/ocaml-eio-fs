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
  "eio-fs is filesystem support for Eio. Eio_fs.Fs is an in-memory
   filesystem backend for Eio paths, Eio_fs.Crunch embeds a directory tree in
   a program, and Eio_fs.Path measures, walks and prints a tree on any
   backend. Eio_fs.Chaos wraps a directory to inject short and torn writes,
   failed syncs and power loss, for crash-consistency tests. The eio-fs.sync
   library publishes a file or a directory tree atomically and durably on any
   backend, through a temporary, a sync, a rename and a directory barrier.
   The eio-fs.unix library adds the POSIX calls Eio and Unix do not bind: the
   platform's strongest sync, advisory locks, filesystem capacity, the sparse
   map, extended attributes and confined batches of directory operations.
   The eio-fs.path library is lexical arithmetic on path strings. The
   eio-fs-crunch binary writes the module that embeds a tree, in the style of
   ocaml-crunch. The in-memory backend is adapted from Patrick Ferris's
   eio_mem (https://tangled.org/patrick.sirref.org/eio_mem), itself derived
   from the Eio filesystem interfaces.")
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.3))
  (eio (>= 1.6))
  fmt
  cstruct
  optint
  (cmdliner (>= 1.1.0))
  eio_main
  (mdx :with-test)
  (alcotest :with-test)
  (alcobar :with-test)
  (mtime :with-test)))
