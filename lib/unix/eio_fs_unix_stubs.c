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

CAMLprim value caml_eio_fs_is_darwin(value unit)
{
#ifdef __APPLE__
  (void)unit;
  return Val_true;
#else
  (void)unit;
  return Val_false;
#endif
}

/* One Darwin F_FULLFSYNC call. Retry and fallback policy lives in OCaml. */
CAMLprim value caml_eio_fs_darwin_full_fsync(value fd_v)
{
#ifdef __APPLE__
  int fd = Int_val(fd_v);
  caml_release_runtime_system();
  int r = fcntl(fd, F_FULLFSYNC, 0);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  if (r == -1)
    caml_unix_error(saved_errno, "full_fsync", Nothing);
  return Val_unit;
#else
  (void)fd_v;
  caml_unix_error(ENOSYS, "full_fsync", Nothing);
  return Val_unit;
#endif
}
