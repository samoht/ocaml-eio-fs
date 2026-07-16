(lang dune 3.21)

(using mdx 0.4)

(name eio-fs)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Leonard" "Patrick Ferris")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (tangled gazagnaire.org/ocaml-eio-fs))

(package
 (name eio-fs)
 (synopsis "In-memory filesystem for Eio")
 (tags (org:blacksun system))
 (description
  "An in-memory Eio.Fs backend: [Eio_fs.Fs.make ()] builds an
   [Eio.Fs.dir_ty Eio.Path.t] that lives entirely in memory, so any code
   written against the [Eio.Path] API runs against it unchanged -- no block
   device, no host filesystem. [Eio_fs.Fs.load] replicates a real directory
   tree into it. Adapted from Patrick Ferris's eio_mem
   (https://tangled.org/patrick.sirref.org/eio_mem), which is itself derived
   from the Eio filesystem interfaces.")
 (depends
  (ocaml (>= 5.1))
  eio
  fmt
  cstruct
  optint
  (eio_main :with-test)
  (mdx :with-test)
  (alcotest :with-test)))
