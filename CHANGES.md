## 0.1.0

Initial release.

- `eio-fs`: an in-memory filesystem for Eio that checks permissions as POSIX
  does, run as root or as a user, and copies of a tree on disk
  (`Eio_fs.Fs`).
- `Eio_fs.Chaos.check` runs a workload many times on fresh in-memory
  filesystems, each run with a fault chosen from what the workload does: an
  operation fails, a read or a write moves one byte, a write is torn, the
  filesystem turns read-only, full or denies access.
  `Eio_fs.Chaos.Crash.check` cuts the power during a workload and during its
  recovery, and checks what a fresh process finds on the disk: unsynced
  writes lost or kept, torn anywhere in a sector the disk held nothing of and
  whole in a 512-byte sector that held bytes, failed syncs, and every state a
  cut allows while there are few. A failure names its scenario, and `~replay` runs it alone.
  `Eio_fs.Chaos.wrap` injects the faults a function chooses, and
  `Eio_fs.Chaos.Crash.v` builds a power cut by hand.
- `eio-fs.sync`: writes a file atomically and durably on any backend.
- `eio-fs.unix`: binds POSIX calls that Eio and `Unix` lack.
- `eio-fs.path`: lexical operations on path strings.
- `eio-fs-crunch`: embeds a directory tree in a program.
