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

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <math.h>
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
#include <sys/uio.h>
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

/* Acquire a BSD flock without waiting (LOCK_NB). The opened file description
   owns this lock, so a descriptor inherited by an execed supervisor keeps it
   held when the acquiring process closes its copy. */
CAMLprim value caml_eio_fs_flock(value fd_v, value mode_v)
{
  int fd = Int_val(fd_v);
  /* OCaml flock_mode: Exclusive = 0, Shared = 1. */
  int operation = (Int_val(mode_v) == 1 ? LOCK_SH : LOCK_EX) | LOCK_NB;
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

/* Up to [len] bytes of [fd] from byte [off] into [buf] at [pos], with pread(2):
   one call, and the descriptor's offset untouched, so two readers sharing it
   need no lock between a seek and a read. The bytes land in a stack buffer
   while the runtime lock is released and are copied in after, since the heap
   may move [buf] meanwhile; [len] is capped at that buffer's size. The count
   read, 0 at the end of the file. */
#define EIO_FS_PREAD_MAX 65536

CAMLprim value caml_eio_fs_pread(value fd_v, value off_v, value buf_v,
                                 value pos_v, value len_v)
{
  CAMLparam5(fd_v, off_v, buf_v, pos_v, len_v);
  char stack[EIO_FS_PREAD_MAX];
  int fd = Int_val(fd_v);
  off_t off = (off_t)Long_val(off_v);
  size_t len = Long_val(len_v);
  ssize_t n;
  if (len > EIO_FS_PREAD_MAX)
    len = EIO_FS_PREAD_MAX;
  do {
    caml_enter_blocking_section();
    n = pread(fd, stack, len, off);
    caml_leave_blocking_section();
  } while (n == -1 && errno == EINTR);
  if (n == -1)
    caml_unix_error(errno, "pread", Nothing);
  memcpy(&Byte(buf_v, Long_val(pos_v)), stack, n);
  CAMLreturn(Val_long(n));
}

/* Up to [len] bytes of [fd] from byte [off] into [buf] at [pos], as
   [caml_eio_fs_pread] reads them, for a read the operating system's page cache
   answers: the bytes go straight into [buf] with the runtime lock kept, since
   nothing moves the heap while this thread holds it, and a page-sized read
   from the cache costs less than releasing and taking the lock back. Linux asks
   with RWF_NOWAIT, which refuses a read that would wait on the device, and only
   that read is made in a blocking section; elsewhere a read of up to
   [EIO_FS_PREAD_CACHED_MAX] bytes keeps the lock whatever it waits on. A longer
   read is [caml_eio_fs_pread]'s. */
#define EIO_FS_PREAD_CACHED_MAX 65536

CAMLprim value caml_eio_fs_pread_cached(value fd_v, value off_v, value buf_v,
                                        value pos_v, value len_v)
{
  int fd = Int_val(fd_v);
  off_t off = (off_t)Long_val(off_v);
  size_t len = Long_val(len_v);
  char *dst = (char *)&Byte(buf_v, Long_val(pos_v));
  ssize_t n;
  if (len > EIO_FS_PREAD_CACHED_MAX)
    return caml_eio_fs_pread(fd_v, off_v, buf_v, pos_v, len_v);
#if defined(__linux__) && defined(RWF_NOWAIT)
  struct iovec iov = {dst, len};
  do {
    n = preadv2(fd, &iov, 1, off, RWF_NOWAIT);
  } while (n == -1 && errno == EINTR);
  if (n == -1 && (errno == EAGAIN || errno == EOPNOTSUPP || errno == ENOSYS))
    return caml_eio_fs_pread(fd_v, off_v, buf_v, pos_v, len_v);
#else
  do {
    n = pread(fd, dst, len, off);
  } while (n == -1 && errno == EINTR);
#endif
  if (n == -1)
    caml_unix_error(errno, "pread", Nothing);
  return Val_long(n);
}

/* Up to [len] bytes of [buf] from [pos] written to [fd] at byte [off], with
   pwrite(2): one call, the descriptor's offset untouched. The bytes are copied
   to a stack buffer before the runtime lock is released, since the heap may
   move [buf] meanwhile; [len] is capped at that buffer's size. The count
   written. */
#define EIO_FS_PWRITE_MAX 65536

CAMLprim value caml_eio_fs_pwrite(value fd_v, value off_v, value buf_v,
                                  value pos_v, value len_v)
{
  CAMLparam5(fd_v, off_v, buf_v, pos_v, len_v);
  char stack[EIO_FS_PWRITE_MAX];
  int fd = Int_val(fd_v);
  off_t off = (off_t)Long_val(off_v);
  size_t len = Long_val(len_v);
  ssize_t n;
  if (len > EIO_FS_PWRITE_MAX)
    len = EIO_FS_PWRITE_MAX;
  memcpy(stack, String_val(buf_v) + Long_val(pos_v), len);
  do {
    caml_enter_blocking_section();
    n = pwrite(fd, stack, len, off);
    caml_leave_blocking_section();
  } while (n == -1 && errno == EINTR);
  if (n == -1)
    caml_unix_error(errno, "pwrite", Nothing);
  CAMLreturn(Val_long(n));
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

/* The entry [name] beneath [dirfd] itself, a symbolic link included, opened
   for a call that acts on a descriptor: O_PATH on Linux, which reads nothing
   and needs no permission on the entry, and O_SYMLINK on Darwin, which opens a
   final link rather than following it. A FIFO is not waited on (O_NONBLOCK)
   and a terminal does not become the controlling one (O_NOCTTY). */
static int eio_fs_open_entry(int dirfd, const char *name)
{
  int fd;
  do {
#if defined(__linux__)
    fd = openat(dirfd, name, O_PATH | O_NOFOLLOW | O_CLOEXEC);
#elif defined(__APPLE__)
    fd = openat(dirfd, name,
                O_RDONLY | O_SYMLINK | O_NONBLOCK | O_NOCTTY | O_CLOEXEC);
#else
    fd = openat(dirfd, name,
                O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_NOCTTY | O_CLOEXEC);
#endif
  } while (fd == -1 && errno == EINTR);
  return fd;
}

/* One POSIX fstatvfs call on the entry [name] beneath [dirfd], never followed.
   Capacity policy and arithmetic live in OCaml. */
CAMLprim value caml_eio_fs_statvfsat(value dirfd_v, value name_v)
{
  CAMLparam2(dirfd_v, name_v);
  CAMLlocal5(result, block_size, total_blocks, free_blocks, available_blocks);
  caml_unix_check_path(name_v, "statvfs");
  char *name = caml_stat_strdup(String_val(name_v));
  int dirfd = Int_val(dirfd_v);
  struct statvfs buf;
  int r = -1;
  caml_release_runtime_system();
  int fd = eio_fs_open_entry(dirfd, name);
  if (fd != -1) {
    r = fstatvfs(fd, &buf);
    int e = errno;
    close(fd);
    errno = e;
  }
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (r == -1)
    caml_unix_error(saved_errno, "statvfs", name_v);

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
  int fd = Int_val(fd_v);
  off_t offset = (off_t)Long_val(offset_v);

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
  if ((int64_t)found > (int64_t)Max_long)
    caml_unix_error(EOVERFLOW, name, Nothing);
  CAMLreturn(caml_alloc_some(Val_long(found)));
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
    /* An extent still in the page cache has no block address yet, and one
       stored inline in metadata, unaligned, encoded or encrypted has no block
       address of its own (btrfs and ext4 report physical 0 for inline data),
       so there is no physical answer to give about it. That ends the batch
       the same way a hole does. */
    if (e->fe_flags
        & (FIEMAP_EXTENT_DELALLOC | FIEMAP_EXTENT_UNWRITTEN
           | FIEMAP_EXTENT_DATA_INLINE | FIEMAP_EXTENT_NOT_ALIGNED
           | FIEMAP_EXTENT_ENCODED | FIEMAP_EXTENT_DATA_ENCRYPTED))
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
  int64_t offset = (int64_t)Long_val(offset_v);
  int64_t length = (int64_t)Long_val(length_v);
  long max = Long_val(max_v);
  int err = 0;
  const char *what = "extents";

  if (max <= 0 || max > EIO_FS_EXTENTS_MAX)
    caml_invalid_argument("Eio_fs_unix.extents: max out of range");
  if (offset < 0 || length < 0)
    caml_invalid_argument("Eio_fs_unix.extents: negative offset or length");
  if (length > (int64_t)Max_long - offset)
    caml_invalid_argument("Eio_fs_unix.extents: range past the largest offset");
  /* An empty range has no extent; FIEMAP refuses one with EINVAL. */
  if (length == 0)
    CAMLreturn(Val_emptylist);

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

  /* A device offset past the largest int has no answer to give. */
  for (long i = 0; i < n; i++)
    if (out[i].physical > (int64_t)Max_long) {
      free(out);
      caml_unix_error(EOVERFLOW, (char *)what, Nothing);
    }

  /* Built back to front so the list comes out in increasing logical order. */
  list = Val_emptylist;
  for (long i = n - 1; i >= 0; i--) {
    extent = caml_alloc_small(3, 0);
    Field(extent, 0) = Val_long(out[i].logical);
    Field(extent, 1) = Val_long(out[i].physical);
    Field(extent, 2) = Val_long(out[i].length);
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

   POSIX has no extended-attribute call, and neither Linux nor Darwin has one
   that takes a directory descriptor and a name. Each call here acts on the
   entry [name] beneath [dirfd] itself, a symbolic link included, never on
   what it points to. Darwin opens the entry once (O_SYMLINK) and calls
   flistxattr(2), fgetxattr(2) and fsetxattr(2) on it. Linux cannot read the
   attributes of a link through a descriptor, so it names the entry through
   the parent's own /proc link and calls llistxattr(2), lgetxattr(2) and
   lsetxattr(2): the kernel resolves that link to the directory [dirfd] holds,
   whatever has been renamed since, and does not follow [name]. */

#if defined(__linux__) || defined(__APPLE__)
#include <sys/xattr.h>
#endif

typedef struct {
  int fd;     /* Darwin: the entry itself. */
  char *path; /* Linux: the entry through its parent's /proc link. */
} eio_fs_entry;

static int eio_fs_entry_open(int dirfd, const char *name, eio_fs_entry *e)
{
  e->fd = -1;
  e->path = NULL;
#if defined(__APPLE__)
  e->fd = eio_fs_open_entry(dirfd, name);
  return e->fd == -1 ? -1 : 0;
#elif defined(__linux__)
  if (access("/proc/self/fd", F_OK) == -1) {
    errno = ENOTSUP;
    return -1;
  }
  size_t len = strlen(name) + 32;
  e->path = malloc(len);
  if (e->path == NULL) {
    errno = ENOMEM;
    return -1;
  }
  snprintf(e->path, len, "/proc/self/fd/%d/%s", dirfd, name);
  return 0;
#else
  (void)dirfd;
  (void)name;
  errno = ENOSYS;
  return -1;
#endif
}

static void eio_fs_entry_close(eio_fs_entry *e)
{
  if (e->fd != -1)
    close(e->fd);
  free(e->path);
}

static ssize_t eio_fs_listxattr(eio_fs_entry *e, char *buf, size_t size)
{
#if defined(__APPLE__)
  return flistxattr(e->fd, buf, size, 0);
#elif defined(__linux__)
  return llistxattr(e->path, buf, size);
#else
  (void)e; (void)buf; (void)size;
  errno = ENOSYS;
  return -1;
#endif
}

static ssize_t eio_fs_getxattr(eio_fs_entry *e, const char *name, void *buf,
                               size_t size)
{
#if defined(__APPLE__)
  return fgetxattr(e->fd, name, buf, size, 0, 0);
#elif defined(__linux__)
  return lgetxattr(e->path, name, buf, size);
#else
  (void)e; (void)name; (void)buf; (void)size;
  errno = ENOSYS;
  return -1;
#endif
}

static int eio_fs_setxattr(eio_fs_entry *e, const char *name, const void *buf,
                           size_t size)
{
#if defined(__APPLE__)
  return fsetxattr(e->fd, name, buf, size, 0, 0);
#elif defined(__linux__)
  return lsetxattr(e->path, name, buf, size, 0);
#else
  (void)e; (void)name; (void)buf; (void)size;
  errno = ENOSYS;
  return -1;
#endif
}

static void eio_fs_check_nul(value v, const char *what)
{
  if (memchr(String_val(v), '\0', caml_string_length(v)) != NULL)
    caml_invalid_argument(what);
}

/* The size query and the read are two calls on one opened entry, and an
   attribute can grow in between: ERANGE on the read asks again. The result is
   the raw bytes the kernel returns, which the OCaml side cuts. */
static value eio_fs_read_sized(value dirfd_v, value entry_v, value name_v,
                               const char *what)
{
  CAMLparam3(dirfd_v, entry_v, name_v);
  CAMLlocal1(result);
  /* Both are checked before either is copied, so a refusal leaks nothing. */
  eio_fs_check_nul(entry_v, "Eio_fs_unix: NUL byte in path");
  if (Is_block(name_v))
    eio_fs_check_nul(Field(name_v, 0), "Eio_fs_unix: NUL byte in name");
  char *entry = caml_stat_strdup(String_val(entry_v));
  char *name = Is_block(name_v) ? caml_stat_strdup(String_val(Field(name_v, 0)))
                                : NULL;
  int dirfd = Int_val(dirfd_v);
  char *buf = NULL;
  ssize_t n = -1;
  int saved_errno;
  eio_fs_entry e;
  caml_release_runtime_system();
  if (eio_fs_entry_open(dirfd, entry, &e) == -1) {
    saved_errno = errno;
  } else {
    for (;;) {
      n = name ? eio_fs_getxattr(&e, name, NULL, 0)
               : eio_fs_listxattr(&e, NULL, 0);
      saved_errno = errno;
      if (n > 0) {
        buf = malloc((size_t)n);
        if (buf == NULL) {
          n = -1;
          saved_errno = ENOMEM;
          break;
        }
        ssize_t m = name ? eio_fs_getxattr(&e, name, buf, (size_t)n)
                         : eio_fs_listxattr(&e, buf, (size_t)n);
        saved_errno = errno;
        if (m == -1 && saved_errno == ERANGE) {
          free(buf);
          buf = NULL;
          continue;
        }
        n = m;
      }
      break;
    }
    eio_fs_entry_close(&e);
  }
  caml_acquire_runtime_system();
  caml_stat_free(entry);
  caml_stat_free(name);
  if (n == -1) {
    free(buf);
    caml_unix_error(saved_errno, (char *)what, entry_v);
  }
  result = caml_alloc_initialized_string((mlsize_t)n, n > 0 ? buf : "");
  free(buf);
  CAMLreturn(result);
}

CAMLprim value caml_eio_fs_listxattrat(value dirfd_v, value entry_v)
{
  return eio_fs_read_sized(dirfd_v, entry_v, Val_none, "listxattr");
}

CAMLprim value caml_eio_fs_getxattrat(value dirfd_v, value entry_v,
                                      value name_v)
{
  CAMLparam3(dirfd_v, entry_v, name_v);
  CAMLlocal1(some);
  some = caml_alloc_some(name_v);
  CAMLreturn(eio_fs_read_sized(dirfd_v, entry_v, some, "getxattr"));
}

/* The errno of an attribute that is not there: ENOATTR on Darwin, ENODATA
   on Linux, which Unix reports as EUNKNOWNERR. */
CAMLprim value caml_eio_fs_enoattr(value unit)
{
  (void)unit;
#if defined(__APPLE__)
  return Val_int(ENOATTR);
#elif defined(ENODATA)
  return Val_int(ENODATA);
#else
  return Val_int(-1);
#endif
}

CAMLprim value caml_eio_fs_setxattrat(value dirfd_v, value entry_v,
                                      value name_v, value data_v)
{
  CAMLparam4(dirfd_v, entry_v, name_v, data_v);
  eio_fs_check_nul(entry_v, "Eio_fs_unix: NUL byte in path");
  eio_fs_check_nul(name_v, "Eio_fs_unix: NUL byte in name");
  char *entry = caml_stat_strdup(String_val(entry_v));
  char *name = caml_stat_strdup(String_val(name_v));
  size_t len = caml_string_length(data_v);
  char *data = caml_stat_alloc(len > 0 ? len : 1);
  memcpy(data, String_val(data_v), len);
  int dirfd = Int_val(dirfd_v);
  int r = -1;
  eio_fs_entry e;
  caml_release_runtime_system();
  if (eio_fs_entry_open(dirfd, entry, &e) != -1) {
    r = eio_fs_setxattr(&e, name, data, len);
    int saved = errno;
    eio_fs_entry_close(&e);
    errno = saved;
  }
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(entry);
  caml_stat_free(name);
  caml_stat_free(data);
  if (r == -1)
    caml_unix_error(saved_errno, "setxattr", entry_v);
  CAMLreturn(Val_unit);
}

/* One clonefileat(2) of the entry [src] beneath [srcfd], a file, a symbolic
   link (CLONE_NOFOLLOW) or a directory and everything under it, to [dst]
   beneath [dstfd], which must not exist. A platform or a file system that
   cannot clone, or two volumes, answer false and write nothing; any other
   failure is an error. The clone is owned by the caller (CLONE_NOOWNERCOPY),
   as a copy it made would be. */
CAMLprim value caml_eio_fs_clone_at(value srcfd_v, value src_v, value dstfd_v,
                                    value dst_v)
{
  CAMLparam4(srcfd_v, src_v, dstfd_v, dst_v);
  caml_unix_check_path(src_v, "clonefileat");
  caml_unix_check_path(dst_v, "clonefileat");
  char *src = caml_stat_strdup(String_val(src_v));
  char *dst = caml_stat_strdup(String_val(dst_v));
  int srcfd = Int_val(srcfd_v), dstfd = Int_val(dstfd_v);
  int r;
  caml_release_runtime_system();
#if defined(__APPLE__)
  r = clonefileat(srcfd, src, dstfd, dst, CLONE_NOFOLLOW | CLONE_NOOWNERCOPY);
#else
  (void)srcfd;
  (void)dstfd;
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
  caml_unix_error(saved_errno, "clonefileat", src_v);
  CAMLreturn(Val_false);
}

/* --- Batched confined directory operations ---

   Each stub is one call of a Batch operation: it copies its arguments out of
   the OCaml heap, releases the runtime for the system calls and raises
   Unix_error on failure. Batch.run calls them from one system thread; Link
   calls openat, mkdirat, symlinkat and fstatat one per system thread. */

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
  do {
    r = mkdirat(Int_val(dirfd_v), name, (mode_t)Int_val(perm_v));
  } while (r == -1 && errno == EINTR);
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
  do {
    r = symlinkat(target, Int_val(dirfd_v), name);
  } while (r == -1 && errno == EINTR);
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

/* [st] as a block of 13 fields in the order of At.stat: ten boxed int64
   (dev, ino, mode, nlink, uid, gid, rdev, size, blksize, blocks) and three
   boxed doubles, the times [at], [mt] and [ct]. */
static value eio_fs_stat_value(const struct stat *st, double at, double mt,
                               double ct)
{
  CAMLparam0();
  CAMLlocal2(res, v);
  int64_t ints[10] = {(int64_t)st->st_dev,    (int64_t)st->st_ino,
                      (int64_t)st->st_mode,   (int64_t)st->st_nlink,
                      (int64_t)st->st_uid,    (int64_t)st->st_gid,
                      (int64_t)st->st_rdev,   (int64_t)st->st_size,
                      (int64_t)st->st_blksize, (int64_t)st->st_blocks};
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

#if defined(__APPLE__)
#define EIO_FS_ATIME(st) ((st).st_atimespec)
#define EIO_FS_MTIME(st) ((st).st_mtimespec)
#define EIO_FS_CTIME(st) ((st).st_ctimespec)
#else
#define EIO_FS_ATIME(st) ((st).st_atim)
#define EIO_FS_MTIME(st) ((st).st_mtim)
#define EIO_FS_CTIME(st) ((st).st_ctim)
#endif

/* [t] in seconds, its whole part kept at [t]'s own second where the sum
   rounds up to the next one, as Eio's own stat does. */
static double eio_fs_timestamp(struct timespec t)
{
  double s = (double)t.tv_sec;
  double f = s + (double)t.tv_nsec / 1e9;
  return floor(f) == s ? f : nextafter(f, -INFINITY);
}

CAMLprim value caml_eio_fs_batch_lstat(value dirfd_v, value name_v)
{
  CAMLparam2(dirfd_v, name_v);
  caml_unix_check_path(name_v, "fstatat");
  char *name = caml_stat_strdup(String_val(name_v));
  struct stat st;
  int r;
  caml_release_runtime_system();
  do {
    r = fstatat(Int_val(dirfd_v), name, &st, AT_SYMLINK_NOFOLLOW);
  } while (r == -1 && errno == EINTR);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (r == -1)
    caml_unix_error(saved_errno, "fstatat", name_v);
  CAMLreturn(eio_fs_stat_value(&st, eio_fs_timestamp(EIO_FS_ATIME(st)),
                               eio_fs_timestamp(EIO_FS_MTIME(st)),
                               eio_fs_timestamp(EIO_FS_CTIME(st))));
}

/* --- Calls beneath a held directory, for Link ---

   Each is the one system call it names on a descriptor the caller keeps
   open, so they work under any Eio backend: the caller makes them from a
   system thread and the runtime is released around the call. */

CAMLprim value caml_eio_fs_fstat(value fd_v)
{
  CAMLparam1(fd_v);
  struct stat st;
  int r;
  caml_release_runtime_system();
  do {
    r = fstat(Int_val(fd_v), &st);
  } while (r == -1 && errno == EINTR);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  if (r == -1)
    caml_unix_error(saved_errno, "fstat", Nothing);
  CAMLreturn(eio_fs_stat_value(&st, eio_fs_timestamp(EIO_FS_ATIME(st)),
                               eio_fs_timestamp(EIO_FS_MTIME(st)),
                               eio_fs_timestamp(EIO_FS_CTIME(st))));
}

/* The entry [name] beneath [dirfd] opened read-only, never through a
   symbolic link (O_NOFOLLOW), without waiting for a FIFO's writer
   (O_NONBLOCK) and without a terminal becoming the controlling one
   (O_NOCTTY). */
CAMLprim value caml_eio_fs_open_leaf(value dirfd_v, value name_v)
{
  CAMLparam2(dirfd_v, name_v);
  caml_unix_check_path(name_v, "openat");
  char *name = caml_stat_strdup(String_val(name_v));
  int fd;
  caml_release_runtime_system();
  do {
    fd = openat(Int_val(dirfd_v), name,
                O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_NOCTTY | O_CLOEXEC);
  } while (fd == -1 && errno == EINTR);
  int saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(name);
  if (fd == -1)
    caml_unix_error(saved_errno, "openat", name_v);
  CAMLreturn(Val_int(fd));
}

/* The target of the symbolic link [name] beneath [dirfd], read into a
   buffer that grows until the target fits. */
CAMLprim value caml_eio_fs_readlinkat(value dirfd_v, value name_v)
{
  CAMLparam2(dirfd_v, name_v);
  CAMLlocal1(result);
  caml_unix_check_path(name_v, "readlinkat");
  char *name = caml_stat_strdup(String_val(name_v));
  int dirfd = Int_val(dirfd_v);
  size_t size = 1024;
  for (;;) {
    char *buf = malloc(size);
    if (buf == NULL) {
      caml_stat_free(name);
      caml_raise_out_of_memory();
    }
    caml_release_runtime_system();
    ssize_t n;
    do {
      n = readlinkat(dirfd, name, buf, size);
    } while (n == -1 && errno == EINTR);
    int saved_errno = errno;
    caml_acquire_runtime_system();
    if (n == -1) {
      free(buf);
      caml_stat_free(name);
      caml_unix_error(saved_errno, "readlinkat", name_v);
    }
    if ((size_t)n < size) {
      result = caml_alloc_initialized_string(n, buf);
      free(buf);
      caml_stat_free(name);
      CAMLreturn(result);
    }
    free(buf);
    size *= 4;
  }
}

/* The calls of Eio_fs_unix.Inline.fs, made on the calling thread. An
   error names the call made here, with the path; which call a backend
   makes, and so the name its error carries, is the backend's, and only the
   errno, the operation and the path are promised alike. */

CAMLprim value caml_eio_fs_inline_fstatat(value path_v, value follow_v)
{
  CAMLparam2(path_v, follow_v);
  struct stat st;
  int r, saved_errno;
  caml_unix_check_path(path_v, "fstatat");
  char *path = caml_stat_strdup(String_val(path_v));
  int flags = Bool_val(follow_v) ? 0 : AT_SYMLINK_NOFOLLOW;
  caml_release_runtime_system();
  do {
    r = fstatat(AT_FDCWD, path, &st, flags);
  } while (r == -1 && errno == EINTR);
  saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(path);
  if (r == -1)
    caml_unix_error(saved_errno, "fstatat", path_v);
  CAMLreturn(eio_fs_stat_value(&st, eio_fs_timestamp(EIO_FS_ATIME(st)),
                               eio_fs_timestamp(EIO_FS_MTIME(st)),
                               eio_fs_timestamp(EIO_FS_CTIME(st))));
}

/* The bits of [flags_v], as Eio_fs_unix.Inline.fs sets them. */
#define INLINE_WRITE 1
#define INLINE_APPEND 2
#define INLINE_CREAT 4
#define INLINE_TRUNC 8
#define INLINE_EXCL 16
#define INLINE_NOFOLLOW 32

CAMLprim value caml_eio_fs_inline_openat(value path_v, value flags_v,
                                         value perm_v)
{
  CAMLparam3(path_v, flags_v, perm_v);
  int bits = Int_val(flags_v);
  int flags = O_CLOEXEC | O_NONBLOCK;
  int fd, saved_errno;
  size_t len = caml_string_length(path_v);
  flags |= (bits & INLINE_WRITE) ? O_RDWR : O_RDONLY;
  if (bits & INLINE_APPEND) flags |= O_APPEND;
  if (bits & INLINE_CREAT) flags |= O_CREAT;
  if (bits & INLINE_TRUNC) flags |= O_TRUNC;
  if (bits & INLINE_EXCL) flags |= O_EXCL;
  if (bits & INLINE_NOFOLLOW) flags |= O_NOFOLLOW;
  /* As eio_posix: a name ending in a slash is a directory. */
  if (len > 0 && String_val(path_v)[len - 1] == '/') flags |= O_DIRECTORY;
  caml_unix_check_path(path_v, "openat");
  char *path = caml_stat_strdup(String_val(path_v));
  caml_release_runtime_system();
  do {
    fd = openat(AT_FDCWD, path, flags, Int_val(perm_v));
  } while (fd == -1 && errno == EINTR);
  saved_errno = errno;
  caml_acquire_runtime_system();
  caml_stat_free(path);
  if (fd == -1)
    caml_unix_error(saved_errno, "openat", path_v);
  CAMLreturn(Val_int(fd));
}

/* Eio.unix's reading of a directory entry's type, which eio_posix's own
   readdir uses: eio_unix_stubs.c exports it and installs no header. */
CAMLprim value eio_unix_file_type_of_dtype(int d_type);

/* The next entry of the open directory [dir_v], its kind and its name, as
   eio_posix's readdir reads one; End_of_file past the last. */
CAMLprim value caml_eio_fs_inline_readdir(value dir_v)
{
  CAMLparam1(dir_v);
  CAMLlocal1(entry_v);
  DIR *dir = DIR_Val(dir_v);
  struct dirent *ent;
  int saved_errno;
  if (!dir)
    caml_unix_error(EBADF, "readdir", Nothing);
  errno = 0;
  caml_release_runtime_system();
  ent = readdir(dir);
  saved_errno = errno;
  caml_acquire_runtime_system();
  if (!ent) {
    if (saved_errno == 0)
      caml_raise_end_of_file();
    caml_unix_error(saved_errno, "readdir", Nothing);
  }
  entry_v = caml_alloc(2, 0);
  Store_field(entry_v, 0, eio_unix_file_type_of_dtype(ent->d_type));
  Store_field(entry_v, 1, caml_copy_string(ent->d_name));
  CAMLreturn(entry_v);
}
