#!/bin/bash
# Records the outcomes through eio_posix as a user: macOS natively, on the
# default case-insensitive APFS, so this runs on a Mac, and Linux in Docker,
# on a tmpfs, in the image pinned below.
#   generate.sh OUT RECORD_EXE
set -euo pipefail
[ "$(uname -s)" = Darwin ] || {
  echo "generate.sh records macOS natively: run it on macOS" >&2
  exit 1
}
command -v docker >/dev/null || { echo "docker not on PATH" >&2; exit 1; }
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
record="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
image="ocaml/opam@sha256:d161f099e5d053fe198accc09fda214c6275289570847fd906f9c65f862bf460"

work="$(mktemp -d "${TMPDIR:-/tmp}/fs-traces.XXXXXX")"
trap 'chmod -R u+rwx "$work"; rm -rf "$work"' EXIT
mkdir "$work/macos" "$work/out"
(cd "$work/macos" && "$record" >"$work/out/macos.csv")

# Only the recorder is built, from the two directories it needs.
docker run --rm --name fs-apfs-rec --user opam --group-add 4242 --tmpfs /work:exec,mode=1777 \
  -v "$src/test/os:/os:ro" -v "$work/out:/out" "$image" bash -euc "
    mkdir ~/rec && cp -r /os/seq /os/scripts ~/rec
    echo '(lang dune 3.0)' >~/rec/dune-project
    cd ~/rec
    opam update -y >/dev/null
    opam install -y eio_posix fmt cstruct optint >/dev/null
    opam exec -- dune build ./scripts/record.exe
    mkdir /work/linux && cd /work/linux
    ~/rec/_build/default/scripts/record.exe >/out/linux.csv"

"$record" merge "$work/out/linux.csv" "$work/out/macos.csv" >"$out"
