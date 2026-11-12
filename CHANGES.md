## 0.1.0

Initial release.

- `eio-fs`: an in-memory filesystem for Eio (`Eio_fs.Fs`). It checks
  permissions as Linux does, run as root or as a user, and can start from a
  copy of a tree on disk.
- `Eio_fs.Chaos.check` runs a workload many times on fresh in-memory
  filesystems, each run with faults chosen from what the workload does: an
  operation fails, a read or a write moves one byte, a write is torn, the
  filesystem turns read-only, full or denies access.
- `Eio_fs.Chaos.Crash.check` cuts the power during a workload and during its
  recovery, and checks what a fresh process finds on the disk, as ext4 with
  `data=writeback` may leave it.
- A failure names its scenario and shows the operations of the failing run;
  `~replay` runs that scenario alone. `Eio_fs.Chaos.wrap` and
  `Eio_fs.Chaos.Crash.v` build a scenario by hand.
- `eio-fs.sync`: writes a file atomically and durably on any backend.
- `eio-fs.unix`: binds POSIX calls that Eio and `Unix` lack.
- `eio-fs.path`: lexical operations on path strings.
- `eio-fs-crunch`: embeds a directory tree in a program.
