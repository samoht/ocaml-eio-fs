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
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/statvfs.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <sys/clonefile.h>
#endif

#if defined(__linux__)
#include <linux/fiemap.h>
#include <linux/fs.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
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

/* Acquire an exclusive BSD flock. The opened file description owns this
   lock, so a descriptor inherited by an execed supervisor keeps it held when
   the acquiring process closes its copy. */
CAMLprim value caml_eio_fs_flock_exclusive(value fd_v, value nonblocking_v)
{
  int fd = Int_val(fd_v);
  int operation = LOCK_EX | (Bool_val(nonblocking_v) ? LOCK_NB : 0);
  caml_release_runtime_system();
  int r = flock(fd, operation);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  if (r == -1)
    caml_unix_error(saved_errno, "flock", Nothing);
  return Val_unit;
}

/* Release a BSD flock. The lock belongs to the open file description, so
   this frees it for every descriptor that shares the description, one an
   execed supervisor inherited included. */
CAMLprim value caml_eio_fs_flock_unlock(value fd_v)
{
  int fd = Int_val(fd_v);
  int r;
  do
    r = flock(fd, LOCK_UN);
  while (r == -1 && errno == EINTR);
  if (r == -1)
    caml_unix_error(errno, "flock", Nothing);
  return Val_unit;
}

/* A wait for a record lock another process holds: the kernel's blocking
   request (POSIX fcntl F_SETLKW, "fcntl() shall wait until the request can be
   satisfied"), made on a thread of its own that runs no OCaml. It writes one
   byte to [notify] once answered, which the waiting fiber awaits. F_SETLKW is
   a cancellation point (POSIX, "Thread Cancellation"), so a cancel withdraws
   the request: the thread is cancelled and joined, and no lock is left to the
   process unless it was granted first. The request locks what [lockf] does,
   from the descriptor's offset to the end of the file, as a read lock when
   [shared] and a write lock otherwise. */
struct record_lock_wait {
  pthread_t thread;
  int fd;
  int notify;
  int shared;
  int err;
};

static void *record_lock_wait_run(void *arg)
{
  struct record_lock_wait *w = arg;
  struct flock request;
  int r;
  memset(&request, 0, sizeof request);
  request.l_type = w->shared ? F_RDLCK : F_WRLCK;
  request.l_whence = SEEK_CUR;
  request.l_start = 0;
  request.l_len = 0;
  do
    r = fcntl(w->fd, F_SETLKW, &request);
  while (r == -1 && errno == EINTR);
  /* Answered: a cancel from here on would lose a granted lock. */
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
  w->err = r == -1 ? errno : 0;
  {
    char answered = 0;
    ssize_t n;
    do
      n = write(w->notify, &answered, 1);
    while (n == -1 && errno == EINTR);
  }
  return NULL;
}

CAMLprim value caml_eio_fs_record_lock_wait_start(value fd_v, value notify_v,
                                                  value shared_v)
{
  struct record_lock_wait *w = malloc(sizeof *w);
  sigset_t all, previous;
  int r;
  if (w == NULL)
    caml_raise_out_of_memory();
  w->fd = Int_val(fd_v);
  w->notify = Int_val(notify_v);
  w->shared = Bool_val(shared_v);
  w->err = 0;
  /* The thread takes no signal: the process's handlers belong to OCaml. */
  sigfillset(&all);
  pthread_sigmask(SIG_BLOCK, &all, &previous);
  r = pthread_create(&w->thread, NULL, record_lock_wait_run, w);
  pthread_sigmask(SIG_SETMASK, &previous, NULL);
  if (r != 0) {
    free(w);
    caml_unix_error(r, "pthread_create", Nothing);
  }
  return caml_copy_nativeint((intnat)w);
}

/* Joins the wait, cancelling it first when [cancel]: [true] when the lock was
   granted, [false] when the cancel withdrew the request. */
CAMLprim value caml_eio_fs_record_lock_wait_end(value wait_v, value cancel_v)
{
  struct record_lock_wait *w = (struct record_lock_wait *)Nativeint_val(wait_v);
  void *ended = NULL;
  int err;
  if (Bool_val(cancel_v))
    pthread_cancel(w->thread);
  caml_release_runtime_system();
  pthread_join(w->thread, &ended);
  caml_acquire_runtime_system();
  err = w->err;
  free(w);
  if (ended == PTHREAD_CANCELED)
    return Val_false;
  if (err != 0)
    caml_unix_error(err, "fcntl", Nothing);
  return Val_true;
}

/* The first [len] bytes of the file [fd] reads, or fewer where it ends, read
   with pread(2) so the descriptor's offset is left where its owner put it. */
CAMLprim value caml_eio_fs_pread_head(value fd_v, value len_v)
{
  CAMLparam2(fd_v, len_v);
  CAMLlocal1(result);
  size_t len = Long_val(len_v);
  char *buf = malloc(len > 0 ? len : 1);
  if (buf == NULL)
    caml_raise_out_of_memory();
  size_t got = 0;
  while (got < len) {
    ssize_t n = pread(Int_val(fd_v), buf + got, len - got, (off_t)got);
    if (n == -1 && errno == EINTR)
      continue;
    if (n == -1) {
      int saved_errno = errno;
      free(buf);
      caml_unix_error(saved_errno, "pread", Nothing);
    }
    if (n == 0)
      break;
    got += n;
  }
  result = caml_alloc_initialized_string(got, buf);
  free(buf);
  CAMLreturn(result);
}

/* The process holding a record lock that would refuse an exclusive lock over
   the whole file (POSIX fcntl F_GETLK), or 0 when none does. F_GETLK answers
   about other processes only: this process's own locks never conflict. */
CAMLprim value caml_eio_fs_record_lock_holder(value fd_v)
{
  struct flock query;
  memset(&query, 0, sizeof query);
  query.l_type = F_WRLCK;
  query.l_whence = SEEK_SET;
  query.l_start = 0;
  query.l_len = 0;
  if (fcntl(Int_val(fd_v), F_GETLK, &query) == -1)
    caml_unix_error(errno, "fcntl", Nothing);
  if (query.l_type == F_UNLCK || query.l_pid <= 0)
    return Val_int(0);
  return Val_int(query.l_pid);
}

/* A record lock of [kind] (0 read, 1 write, 2 unlock) on [len] bytes from
   byte [at], set without waiting (POSIX fcntl F_SETLK) and placed from the
   start of the file, so the descriptor's offset does not move. A lock another
   process holds fails with EAGAIN or EACCES. */
CAMLprim value caml_eio_fs_lock_range(value fd_v, value kind_v, value at_v,
                                      value len_v)
{
  struct flock request;
  int kind = Int_val(kind_v);
  memset(&request, 0, sizeof request);
  request.l_type = kind == 0 ? F_RDLCK : kind == 1 ? F_WRLCK : F_UNLCK;
  request.l_whence = SEEK_SET;
  request.l_start = (off_t)Long_val(at_v);
  request.l_len = (off_t)Long_val(len_v);
  if (fcntl(Int_val(fd_v), F_SETLK, &request) == -1)
    caml_unix_error(errno, "fcntl", Nothing);
  return Val_unit;
}

/* Whether another process holds a record lock that would refuse a write lock
   on [len] bytes from byte [at] (POSIX fcntl F_GETLK). */
CAMLprim value caml_eio_fs_range_locked(value fd_v, value at_v, value len_v)
{
  struct flock query;
  memset(&query, 0, sizeof query);
  query.l_type = F_WRLCK;
  query.l_whence = SEEK_SET;
  query.l_start = (off_t)Long_val(at_v);
  query.l_len = (off_t)Long_val(len_v);
  if (fcntl(Int_val(fd_v), F_GETLK, &query) == -1)
    caml_unix_error(errno, "fcntl", Nothing);
  return Val_bool(query.l_type != F_UNLCK);
}

/* The process holding a record lock that would refuse a write lock on [len]
   bytes from byte [at] (POSIX fcntl F_GETLK), [0] where the kernel names
   none, or [-1] where no other process holds one. */
CAMLprim value caml_eio_fs_range_holder(value fd_v, value at_v, value len_v)
{
  struct flock query;
  memset(&query, 0, sizeof query);
  query.l_type = F_WRLCK;
  query.l_whence = SEEK_SET;
  query.l_start = (off_t)Long_val(at_v);
  query.l_len = (off_t)Long_val(len_v);
  if (fcntl(Int_val(fd_v), F_GETLK, &query) == -1)
    caml_unix_error(errno, "fcntl", Nothing);
  if (query.l_type == F_UNLCK)
    return Val_int(-1);
  return Val_int(query.l_pid > 0 ? query.l_pid : 0);
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

/* A batch is one systhread hop, and the hop is what these calls are spared: a
   per-call binding would pay the hop once per extent. The caller says how many
   answers it wants per hop; this is the ceiling on the buffer that holds
   them. */
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
    /* A hole is not an error here: the call succeeds over one and reports a
       device offset of -1 with the hole's own length. A caller that only reads
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

/* Linux renameat2(RENAME_NOREPLACE), Darwin renameatx_np(RENAME_EXCL).
   Both names are basenames under already-open directory descriptors. */
CAMLprim value caml_eio_fs_rename_exclusive(value srcfd_v, value src_v,
                                          value dstfd_v, value dst_v)
{
  CAMLparam4(srcfd_v, src_v, dstfd_v, dst_v);
  caml_unix_check_path(src_v, "rename_exclusive");
  caml_unix_check_path(dst_v, "rename_exclusive");
  int srcfd = Int_val(srcfd_v), dstfd = Int_val(dstfd_v);
  char *src = caml_stat_strdup(String_val(src_v));
  char *dst = caml_stat_strdup(String_val(dst_v));
  int r;
  caml_release_runtime_system();
  do {
#if defined(__APPLE__)
    r = renameatx_np(srcfd, src, dstfd, dst, RENAME_EXCL);
#elif defined(__linux__) && defined(SYS_renameat2)
    r = syscall(SYS_renameat2, srcfd, src, dstfd, dst, RENAME_NOREPLACE);
#else
    errno = ENOSYS;
    r = -1;
#endif
  } while (r == -1 && errno == EINTR);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(src);
  caml_stat_free(dst);
  if (r == -1)
    caml_unix_error(saved_errno, "rename_exclusive", dst_v);
  CAMLreturn(Val_unit);
}

/* --- Extended attributes ---

   POSIX has no extended-attribute call; Linux and Darwin each have one family
   with different spellings. Linux names the no-follow form with an [l]
   prefix (llistxattr(2), lgetxattr(2), lsetxattr(2)); Darwin passes
   XATTR_NOFOLLOW to listxattr(2), getxattr(2) and setxattr(2), which also take
   a resource-fork position, always 0 here. Every call acts on the named entry
   itself, a symbolic link included, never on what it points to. */

#if defined(__linux__) || defined(__APPLE__)
#include <sys/xattr.h>
#endif

static char *eio_fs_cstring(value v, const char *what)
{
  mlsize_t len = caml_string_length(v);
  const char *s = String_val(v);
  if (memchr(s, '\0', len) != NULL)
    caml_invalid_argument(what);
  char *c = malloc(len + 1);
  if (c == NULL)
    caml_raise_out_of_memory();
  memcpy(c, s, len);
  c[len] = '\0';
  return c;
}

static ssize_t eio_fs_llistxattr(const char *path, char *buf, size_t size)
{
#if defined(__linux__)
  return llistxattr(path, buf, size);
#elif defined(__APPLE__)
  return listxattr(path, buf, size, XATTR_NOFOLLOW);
#else
  (void)path; (void)buf; (void)size;
  errno = ENOSYS;
  return -1;
#endif
}

static ssize_t eio_fs_lgetxattr(const char *path, const char *name, void *buf,
                                size_t size)
{
#if defined(__linux__)
  return lgetxattr(path, name, buf, size);
#elif defined(__APPLE__)
  return getxattr(path, name, buf, size, 0, XATTR_NOFOLLOW);
#else
  (void)path; (void)name; (void)buf; (void)size;
  errno = ENOSYS;
  return -1;
#endif
}

static int eio_fs_lsetxattr(const char *path, const char *name,
                            const void *buf, size_t size)
{
#if defined(__linux__)
  return lsetxattr(path, name, buf, size, 0);
#elif defined(__APPLE__)
  return setxattr(path, name, buf, size, 0, XATTR_NOFOLLOW);
#else
  (void)path; (void)name; (void)buf; (void)size;
  errno = ENOSYS;
  return -1;
#endif
}

/* The size query and the read are two calls, and an attribute can grow in
   between: ERANGE on the read asks again. The result is the raw bytes the
   kernel returns, which the OCaml side cuts. */
static value eio_fs_read_sized(value path_v, value name_v, const char *what)
{
  CAMLparam2(path_v, name_v);
  CAMLlocal1(result);
  char *path = eio_fs_cstring(path_v, "Eio_fs_unix: NUL byte in path");
  char *name = NULL;
  if (Is_block(name_v))
    name = eio_fs_cstring(Field(name_v, 0), "Eio_fs_unix: NUL byte in name");
  char *buf = NULL;
  ssize_t n;
  int saved_errno;
  for (;;) {
    caml_release_runtime_system();
    n = name ? eio_fs_lgetxattr(path, name, NULL, 0)
             : eio_fs_llistxattr(path, NULL, 0);
    saved_errno = errno;
    if (n > 0) {
      buf = malloc((size_t)n);
      if (buf == NULL) {
        caml_acquire_runtime_system();
        free(path);
        free(name);
        caml_raise_out_of_memory();
      }
      ssize_t m = name ? eio_fs_lgetxattr(path, name, buf, (size_t)n)
                       : eio_fs_llistxattr(path, buf, (size_t)n);
      saved_errno = errno;
      if (m == -1 && saved_errno == ERANGE) {
        free(buf);
        buf = NULL;
        caml_acquire_runtime_system();
        continue;
      }
      n = m;
    }
    caml_acquire_runtime_system();
    break;
  }
  free(path);
  free(name);
  if (n == -1) {
    free(buf);
    caml_unix_error(saved_errno, (char *)what, path_v);
  }
  result = caml_alloc_initialized_string((mlsize_t)n, n > 0 ? buf : "");
  free(buf);
  CAMLreturn(result);
}

CAMLprim value caml_eio_fs_llistxattr(value path_v)
{
  return eio_fs_read_sized(path_v, Val_none, "llistxattr");
}

CAMLprim value caml_eio_fs_lgetxattr(value path_v, value name_v)
{
  CAMLparam2(path_v, name_v);
  CAMLlocal1(some);
  some = caml_alloc_some(name_v);
  CAMLreturn(eio_fs_read_sized(path_v, some, "lgetxattr"));
}

CAMLprim value caml_eio_fs_lsetxattr(value path_v, value name_v, value data_v)
{
  CAMLparam3(path_v, name_v, data_v);
  char *path = eio_fs_cstring(path_v, "Eio_fs_unix: NUL byte in path");
  char *name = eio_fs_cstring(name_v, "Eio_fs_unix: NUL byte in name");
  size_t len = caml_string_length(data_v);
  char *data = malloc(len > 0 ? len : 1);
  if (data == NULL) {
    free(path);
    free(name);
    caml_raise_out_of_memory();
  }
  memcpy(data, String_val(data_v), len);
  caml_release_runtime_system();
  int r = eio_fs_lsetxattr(path, name, data, len);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  free(path);
  free(name);
  free(data);
  if (r == -1)
    caml_unix_error(saved_errno, "lsetxattr", path_v);
  CAMLreturn(Val_unit);
}

/* One clonefile(2) of a file, or of a directory and everything under it, into
   a destination that does not exist. A platform or a file system that cannot
   clone, or two volumes, answer false and write nothing; any other failure is
   an error. The clone is owned by the caller (CLONE_NOOWNERCOPY), as a copy
   it made would be. */
CAMLprim value caml_eio_fs_clone_tree(value src_v, value dst_v)
{
  CAMLparam2(src_v, dst_v);
  if (memchr(String_val(src_v), '\0', caml_string_length(src_v)) != NULL ||
      memchr(String_val(dst_v), '\0', caml_string_length(dst_v)) != NULL)
    caml_invalid_argument("Eio_fs_unix.clone_tree: NUL byte in path");
  char *src = caml_stat_strdup(String_val(src_v));
  char *dst = caml_stat_strdup(String_val(dst_v));
  int r;
  caml_release_runtime_system();
#if defined(__APPLE__)
  r = clonefile(src, dst, CLONE_NOOWNERCOPY);
#else
  errno = ENOTSUP;
  r = -1;
#endif
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(src);
  caml_stat_free(dst);
  if (r == 0)
    CAMLreturn(Val_true);
  if (saved_errno == ENOTSUP || saved_errno == EXDEV)
    CAMLreturn(Val_false);
  caml_unix_error(saved_errno, "clonefile", src_v);
  CAMLreturn(Val_false);
}

/* --- Batched confined directory operations ---

   Each stub is one call of a Batch operation: it copies its arguments out of
   the OCaml heap, releases the runtime for the system calls and raises
   Unix_error on failure. Batch.run calls them from one system thread. */

#include <sys/stat.h>

static int eio_fs_batch_retry_close(int fd)
{
  int r = close(fd);
  return (r == -1 && errno == EINTR) ? 0 : r;
}

CAMLprim value caml_eio_fs_batch_open_dir(value dirfd_v, value name_v)
{
  CAMLparam2(dirfd_v, name_v);
  caml_unix_check_path(name_v, "openat");
  char *name = caml_stat_strdup(String_val(name_v));
  int fd;
  caml_release_runtime_system();
  do {
    fd = openat(Int_val(dirfd_v), name,
                O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
  } while (fd == -1 && errno == EINTR);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (fd == -1)
    caml_unix_error(saved_errno, "openat", name_v);
  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_eio_fs_batch_create(value dirfd_v, value name_v,
                                        value perm_v, value contents_v)
{
  CAMLparam4(dirfd_v, name_v, perm_v, contents_v);
  caml_unix_check_path(name_v, "openat");
  char *name = caml_stat_strdup(String_val(name_v));
  size_t len = caml_string_length(contents_v);
  char *buf = caml_stat_alloc(len + 1);
  memcpy(buf, String_val(contents_v), len);
  mode_t perm = (mode_t)Int_val(perm_v);
  const char *call = "openat";
  int r = 0, saved_errno = 0;
  caml_release_runtime_system();
  int fd;
  do {
    fd = openat(Int_val(dirfd_v), name,
                O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC, perm);
  } while (fd == -1 && errno == EINTR);
  if (fd == -1) {
    r = -1;
    saved_errno = errno;
  } else {
    size_t off = 0;
    while (off < len) {
      ssize_t w = write(fd, buf + off, len - off);
      if (w == -1) {
        if (errno == EINTR)
          continue;
        r = -1;
        saved_errno = errno;
        call = "write";
        break;
      }
      off += (size_t)w;
    }
    if (r == 0 && fchmod(fd, perm) == -1) {
      r = -1;
      saved_errno = errno;
      call = "fchmod";
    }
    if (eio_fs_batch_retry_close(fd) == -1 && r == 0) {
      r = -1;
      saved_errno = errno;
      call = "close";
    }
  }
  caml_acquire_runtime_system();
  caml_stat_free(name);
  caml_stat_free(buf);
  if (r == -1)
    caml_unix_error(saved_errno, call, name_v);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_fs_batch_mkdir(value dirfd_v, value name_v,
                                       value perm_v)
{
  CAMLparam3(dirfd_v, name_v, perm_v);
  caml_unix_check_path(name_v, "mkdirat");
  char *name = caml_stat_strdup(String_val(name_v));
  int r;
  caml_release_runtime_system();
  r = mkdirat(Int_val(dirfd_v), name, (mode_t)Int_val(perm_v));
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (r == -1)
    caml_unix_error(saved_errno, "mkdirat", name_v);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_fs_batch_symlink(value target_v, value dirfd_v,
                                         value name_v)
{
  CAMLparam3(target_v, dirfd_v, name_v);
  caml_unix_check_path(target_v, "symlinkat");
  caml_unix_check_path(name_v, "symlinkat");
  char *target = caml_stat_strdup(String_val(target_v));
  char *name = caml_stat_strdup(String_val(name_v));
  int r;
  caml_release_runtime_system();
  r = symlinkat(target, Int_val(dirfd_v), name);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(target);
  caml_stat_free(name);
  if (r == -1)
    caml_unix_error(saved_errno, "symlinkat", name_v);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_fs_batch_unlink(value dirfd_v, value name_v,
                                        value dir_v)
{
  CAMLparam3(dirfd_v, name_v, dir_v);
  caml_unix_check_path(name_v, "unlinkat");
  char *name = caml_stat_strdup(String_val(name_v));
  int flags = Bool_val(dir_v) ? AT_REMOVEDIR : 0;
  int r;
  caml_release_runtime_system();
  r = unlinkat(Int_val(dirfd_v), name, flags);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (r == -1)
    caml_unix_error(saved_errno, "unlinkat", name_v);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_fs_batch_rename(value srcfd_v, value src_v,
                                        value dstfd_v, value dst_v)
{
  CAMLparam4(srcfd_v, src_v, dstfd_v, dst_v);
  caml_unix_check_path(src_v, "renameat");
  caml_unix_check_path(dst_v, "renameat");
  char *src = caml_stat_strdup(String_val(src_v));
  char *dst = caml_stat_strdup(String_val(dst_v));
  int r;
  caml_release_runtime_system();
  r = renameat(Int_val(srcfd_v), src, Int_val(dstfd_v), dst);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(src);
  caml_stat_free(dst);
  if (r == -1)
    caml_unix_error(saved_errno, "renameat", dst_v);
  CAMLreturn(Val_unit);
}

/* linkat(2) with no flags: a symbolic link at [src] is linked itself and
   never followed. */
CAMLprim value caml_eio_fs_batch_link(value srcfd_v, value src_v,
                                      value dstfd_v, value dst_v)
{
  CAMLparam4(srcfd_v, src_v, dstfd_v, dst_v);
  caml_unix_check_path(src_v, "linkat");
  caml_unix_check_path(dst_v, "linkat");
  char *src = caml_stat_strdup(String_val(src_v));
  char *dst = caml_stat_strdup(String_val(dst_v));
  int r;
  caml_release_runtime_system();
  r = linkat(Int_val(srcfd_v), src, Int_val(dstfd_v), dst, 0);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(src);
  caml_stat_free(dst);
  if (r == -1)
    caml_unix_error(saved_errno, "linkat", dst_v);
  CAMLreturn(Val_unit);
}

/* The result is a block of 15 fields in the order of Batch.raw_stat: ten
   boxed int64 (dev, ino, mode, nlink, uid, gid, rdev, size, blksize, blocks)
   and three boxed doubles (atime, mtime, ctime). */
CAMLprim value caml_eio_fs_batch_lstat(value dirfd_v, value name_v)
{
  CAMLparam2(dirfd_v, name_v);
  CAMLlocal2(res, v);
  caml_unix_check_path(name_v, "fstatat");
  char *name = caml_stat_strdup(String_val(name_v));
  struct stat st;
  int r;
  caml_release_runtime_system();
  r = fstatat(Int_val(dirfd_v), name, &st, AT_SYMLINK_NOFOLLOW);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (r == -1)
    caml_unix_error(saved_errno, "fstatat", name_v);
#if defined(__APPLE__)
  double at = st.st_atimespec.tv_sec + st.st_atimespec.tv_nsec / 1e9;
  double mt = st.st_mtimespec.tv_sec + st.st_mtimespec.tv_nsec / 1e9;
  double ct = st.st_ctimespec.tv_sec + st.st_ctimespec.tv_nsec / 1e9;
#else
  double at = st.st_atim.tv_sec + st.st_atim.tv_nsec / 1e9;
  double mt = st.st_mtim.tv_sec + st.st_mtim.tv_nsec / 1e9;
  double ct = st.st_ctim.tv_sec + st.st_ctim.tv_nsec / 1e9;
#endif
  int64_t ints[10] = {(int64_t)st.st_dev,    (int64_t)st.st_ino,
                      (int64_t)st.st_mode,   (int64_t)st.st_nlink,
                      (int64_t)st.st_uid,    (int64_t)st.st_gid,
                      (int64_t)st.st_rdev,   (int64_t)st.st_size,
                      (int64_t)st.st_blksize, (int64_t)st.st_blocks};
  double times[3] = {at, mt, ct};
  res = caml_alloc_tuple(13);
  for (int i = 0; i < 10; i++) {
    v = caml_copy_int64(ints[i]);
    Store_field(res, i, v);
  }
  for (int i = 0; i < 3; i++) {
    v = caml_copy_double(times[i]);
    Store_field(res, 10 + i, v);
  }
  CAMLreturn(res);
}
