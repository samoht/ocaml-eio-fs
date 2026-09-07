/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* SEEK_DATA, SEEK_HOLE and FS_IOC_FIEMAP are behind _GNU_SOURCE on glibc, so
   the define comes before every include. */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/statvfs.h>
#include <unistd.h>

#if defined(__linux__)
#include <linux/fiemap.h>
#include <linux/fs.h>
#include <sys/ioctl.h>
#endif

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
  CAMLlocal5(result, block_size, total_blocks, free_blocks, available_blocks);
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
  free_blocks = caml_copy_int64((int64_t)buf.f_bfree);
  available_blocks = caml_copy_int64((int64_t)buf.f_bavail);
  result = caml_alloc_tuple(4);
  Store_field(result, 0, block_size);
  Store_field(result, 1, total_blocks);
  Store_field(result, 2, free_blocks);
  Store_field(result, 3, available_blocks);
  CAMLreturn(result);
}

/* --- The sparse map: which bytes a file has, and where they are ---

   `Unix.lseek` takes SEEK_SET, SEEK_CUR and SEEK_END, so a caller cannot skip
   a hole; and no portable call reports which physical pieces a file's bytes
   are in. Both are answerable on every host, in ways that differ:

   - the hole map is `lseek(SEEK_DATA)`/`lseek(SEEK_HOLE)`, which both
     platforms have. The two whence values are 3 and 4 on Darwin and 4 and 3
     on Linux, which matters only to a caller that restates them: taking the
     names from the header makes this one implementation and not two.
   - the physical map has no portable spelling: F_LOG2PHYS_EXT on Darwin,
     FS_IOC_FIEMAP on Linux. That is the one function below with two bodies.

   Every call runs with the runtime system released, and the OCaml side runs
   it on a system thread, so the calling domain's other fibers keep going.
   That pair is the whole point of these living here: a walk of a fragmented
   disk image is millions of these calls, and on the caller's own thread it
   holds the scheduler for every one of them. */

/* A batch is one systhread hop, and the hop is what these calls are being
   spared. Measured 2026-09-07 on macOS/arm64: a round trip through
   Eio_unix.run_in_systhread costs 9.2 us against 0.31 us for the same lseek
   made directly, so a per-call binding would spend about 19 s of pure hop
   overhead over two million extents. The caller says how many answers it
   wants per hop; this is the ceiling on the buffer that holds them, 1.5 MB at
   24 bytes an extent. */
#define EIO_FS_EXTENTS_MAX 65536

struct eio_fs_extent {
  int64_t logical;
  int64_t physical;
  int64_t length;
};

static value eio_fs_seek(value fd_v, value offset_v, int whence,
                         const char *name)
{
  CAMLparam2(fd_v, offset_v);
  CAMLlocal1(found_v);
  int fd = Int_val(fd_v);
  off_t offset = (off_t)Int64_val(offset_v);

  caml_release_runtime_system();
  off_t found = lseek(fd, offset, whence);
  int saved_errno = errno;
  caml_acquire_runtime_system();

  if (found < 0) {
    /* A file whose tail is a hole answers ENXIO to SEEK_DATA, and an offset
       at or past the end answers ENXIO to both. That is an answer -- there is
       no next one -- and not a failure. */
    if (saved_errno == ENXIO)
      CAMLreturn(Val_none);
    caml_unix_error(saved_errno, name, Nothing);
  }
  found_v = caml_copy_int64((int64_t)found);
  CAMLreturn(caml_alloc_some(found_v));
}

CAMLprim value caml_eio_fs_seek_data(value fd_v, value offset_v)
{
  return eio_fs_seek(fd_v, offset_v, SEEK_DATA, "seek_data");
}

CAMLprim value caml_eio_fs_seek_hole(value fd_v, value offset_v)
{
  return eio_fs_seek(fd_v, offset_v, SEEK_HOLE, "seek_hole");
}

/* Fills [out] with up to [max] extents backing [offset, offset + length), in
   increasing logical order and contiguous in logical space, stopping at a
   hole, at the end of the range, or when [max] is reached. Returns how many
   were filled, or -1 with *err and *what set. Runs with the runtime system
   released, so it allocates nothing on the OCaml heap. */
static long eio_fs_extents_fill(int fd, int64_t offset, int64_t length,
                                long max, struct eio_fs_extent *out, int *err,
                                const char **what)
{
  long n = 0;

#if defined(__APPLE__)
  /* F_LOG2PHYS_EXT answers about one position, so the loop steps on by what
     it is told. struct log2phys is inside a `#pragma pack(4)` in
     <sys/fcntl.h>, so its two off_t fields are not naturally aligned; using
     the header's own declaration is what keeps that right, and the field
     offsets are not restated here. */
  int64_t at = offset;
  int64_t end = offset + length;
  while (n < max && at < end) {
    struct log2phys arg;
    memset(&arg, 0, sizeof arg);
    arg.l2p_devoffset = (off_t)at;
    arg.l2p_contigbytes = (off_t)(end - at);
    if (fcntl(fd, F_LOG2PHYS_EXT, &arg) < 0) {
      if (errno == ERANGE || errno == ENXIO)
        break; /* nothing allocated at this offset */
      *err = errno;
      *what = "F_LOG2PHYS_EXT";
      return -1;
    }
    /* A hole is not an error here: the call SUCCEEDS over one and reports a
       device offset of -1 with the hole's own length. Measured 2026-09-07 on
       APFS, walking a file with a 64 MiB gap in it. A caller that only reads
       l2p_contigbytes therefore counts holes as physical pieces and reads a
       negative device address, so the offset is what says whether there is an
       answer. */
    if (arg.l2p_contigbytes <= 0 || arg.l2p_devoffset < 0)
      break;
    int64_t contiguous = (int64_t)arg.l2p_contigbytes;
    if (contiguous > end - at)
      contiguous = end - at;
    out[n].logical = at;
    out[n].physical = (int64_t)arg.l2p_devoffset;
    out[n].length = contiguous;
    n++;
    at += contiguous;
  }
#elif defined(__linux__)
  /* FIEMAP answers about a whole range in one ioctl, which is what makes the
     batch worth having here: [max] extents cost one syscall rather than
     [max]. It reports the extent CONTAINING the range's start, which can
     begin before it, so the first answer is clamped; the last is clamped to
     the range's end. Any gap between two answers is a hole, and the walk
     stops there, so both platforms answer the same question. */
  size_t bytes = sizeof(struct fiemap) + (size_t)max * sizeof(struct fiemap_extent);
  struct fiemap *map = calloc(1, bytes);
  if (map == NULL) {
    *err = ENOMEM;
    *what = "FS_IOC_FIEMAP";
    return -1;
  }
  map->fm_start = (__u64)offset;
  map->fm_length = (__u64)length;
  map->fm_extent_count = (__u32)max;
  if (ioctl(fd, FS_IOC_FIEMAP, map) < 0) {
    *err = errno;
    *what = "FS_IOC_FIEMAP";
    free(map);
    return -1;
  }
  int64_t expected = offset;
  int64_t end = offset + length;
  for (__u32 i = 0; i < map->fm_mapped_extents && n < max; i++) {
    struct fiemap_extent *e = &map->fm_extents[i];
    if (e->fe_flags & FIEMAP_EXTENT_UNKNOWN) {
      *err = EOPNOTSUPP;
      *what = "FS_IOC_FIEMAP";
      free(map);
      return -1;
    }
    /* An extent still in the page cache has no block address yet, so there is
       no physical answer to give about it. That ends the batch the same way a
       hole does. */
    if (e->fe_flags & (FIEMAP_EXTENT_DELALLOC | FIEMAP_EXTENT_UNWRITTEN))
      break;
    int64_t logical = (int64_t)e->fe_logical;
    int64_t elength = (int64_t)e->fe_length;
    int64_t lead = expected - logical;
    if (lead < 0 || lead >= elength)
      break; /* a hole, or an answer that starts past where we are */
    int64_t physical = (int64_t)e->fe_physical + lead;
    int64_t contiguous = elength - lead;
    if (contiguous > end - expected)
      contiguous = end - expected;
    if (contiguous <= 0)
      break;
    out[n].logical = expected;
    out[n].physical = physical;
    out[n].length = contiguous;
    n++;
    expected += contiguous;
    if (expected >= end)
      break;
  }
  free(map);
#else
  (void)fd;
  (void)offset;
  (void)length;
  (void)max;
  (void)out;
  *err = ENOSYS;
  *what = "extents";
  return -1;
#endif
  return n;
}

CAMLprim value caml_eio_fs_extents(value fd_v, value offset_v, value length_v,
                                   value max_v)
{
  CAMLparam4(fd_v, offset_v, length_v, max_v);
  CAMLlocal3(list, cell, extent);
  int fd = Int_val(fd_v);
  int64_t offset = Int64_val(offset_v);
  int64_t length = Int64_val(length_v);
  long max = Long_val(max_v);
  int err = 0;
  const char *what = "extents";

  if (max <= 0 || max > EIO_FS_EXTENTS_MAX)
    caml_invalid_argument("Eio_fs_unix.extents: max out of range");
  if (offset < 0 || length < 0)
    caml_invalid_argument("Eio_fs_unix.extents: negative offset or length");

  struct eio_fs_extent *out =
      malloc((size_t)max * sizeof(struct eio_fs_extent));
  if (out == NULL)
    caml_raise_out_of_memory();

  caml_release_runtime_system();
  long n = eio_fs_extents_fill(fd, offset, length, max, out, &err, &what);
  caml_acquire_runtime_system();

  if (n < 0) {
    free(out);
    caml_unix_error(err, what, Nothing);
  }

  /* Built back to front so the list comes out in increasing logical order. */
  list = Val_emptylist;
  for (long i = n - 1; i >= 0; i--) {
    extent = caml_alloc_tuple(3);
    Store_field(extent, 0, caml_copy_int64(out[i].logical));
    Store_field(extent, 1, caml_copy_int64(out[i].physical));
    Store_field(extent, 2, caml_copy_int64(out[i].length));
    cell = caml_alloc_small(2, Tag_cons);
    Field(cell, 0) = extent;
    Field(cell, 1) = list;
    list = cell;
  }
  free(out);
  CAMLreturn(list);
}
