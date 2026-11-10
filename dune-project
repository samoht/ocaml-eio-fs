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
  "eio-fs is an in-memory filesystem for Eio, with wrappers that inject
faults and simulate power loss for crash-consistency tests. It also writes
files atomically and durably on any backend, binds POSIX calls that Eio
lacks, and embeds a directory tree in a program. The in-memory backend is
adapted from Patrick Ferris's eio_mem (https://git.sirref.org/eio_mem).")
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.3))
  (eio (>= 1.6))
  fmt
  cstruct
  optint
  mtime
  psq
  domain-local-await
  (cmdliner (>= 1.1.0))
  eio_main
  (conf-linux-libc-dev (= :os linux))
  (mdx (and :with-test (>= 2.3.0)))
  (alcotest (and :with-test (>= 1.7.0)))
  (alcobar :with-test)))
