/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

/* fsync through to the platter. On Darwin fsync(2) only flushes to the
   drive's cache, so a publish survives a crash but not a power cut;
   F_FULLFSYNC asks the drive to flush its cache too. Some filesystems
   (SMB, exFAT) do not support F_FULLFSYNC (ENOTSUP), so an unsupported
   result falls back to fsync -- the strongest barrier the volume offers,
   matching SQLite's practice. A transient EINTR is retried, never taken as
   a reason to downgrade; any other error is surfaced rather than silently
   weakening durability. Everywhere else fsync already reaches stable
   storage. */
CAMLprim value caml_eio_fs_full_fsync(value fd_v)
{
  int fd = Int_val(fd_v);
  int r;
  caml_release_runtime_system();
#ifdef __APPLE__
  do {
    r = fcntl(fd, F_FULLFSYNC, 0);
  } while (r == -1 && errno == EINTR);
  if (r == -1 && (errno == ENOTSUP || errno == EINVAL || errno == ENOTTY)) {
    /* The filesystem does not support F_FULLFSYNC: fall back to fsync. */
    do {
      r = fsync(fd);
    } while (r == -1 && errno == EINTR);
  }
#else
  do {
    r = fsync(fd);
  } while (r == -1 && errno == EINTR);
#endif
  /* Preserve the syscall's errno across the runtime reacquisition: acquiring the
     runtime may run other syscalls that clobber errno, and uerror below reads it
     to classify the failure. */
  int saved_errno = errno;
  caml_acquire_runtime_system();
  errno = saved_errno;
  if (r == -1)
    uerror("full_fsync", Nothing);
  return Val_unit;
}
