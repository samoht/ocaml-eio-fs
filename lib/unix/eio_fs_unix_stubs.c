/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/statvfs.h>
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

/* One POSIX statvfs call. Capacity policy and arithmetic live in OCaml. */
CAMLprim value caml_eio_fs_statvfs(value path_v)
{
  CAMLparam1(path_v);
  CAMLlocal4(result, block_size, total_blocks, available_blocks);
  mlsize_t path_len = caml_string_length(path_v);
  const char *ocaml_path = String_val(path_v);
  if (memchr(ocaml_path, '\0', path_len) != NULL)
    caml_invalid_argument("Eio_fs_unix.statvfs: NUL byte in path");

  char *path = malloc(path_len + 1);
  if (path == NULL)
    caml_raise_out_of_memory();
  memcpy(path, ocaml_path, path_len);
  path[path_len] = '\0';

  struct statvfs buf;
  caml_release_runtime_system();
  int r = statvfs(path, &buf);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  free(path);
  if (r == -1)
    caml_unix_error(saved_errno, "statvfs", path_v);

  block_size = caml_copy_int64((int64_t)buf.f_frsize);
  total_blocks = caml_copy_int64((int64_t)buf.f_blocks);
  available_blocks = caml_copy_int64((int64_t)buf.f_bavail);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, block_size);
  Store_field(result, 1, total_blocks);
  Store_field(result, 2, available_blocks);
  CAMLreturn(result);
}
