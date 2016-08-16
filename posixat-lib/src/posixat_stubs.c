#include "common.h"

#if defined(_WIN32)

NA(at_fdcwd);
NA(fstatat);
NA(fdopendir);

CAMLprim value shexp_fd_info(value fd)
{
  CAMLparam1(fd);
  CAMLlocal1(res);
  switch (Descr_kind_val(fd)) {
  case KIND_HANDLE:
    res = caml_alloc_small(1, 0);
    Store_field(res, 0, caml_copy_int64((int64_t)Handle_val(fd)));
    break;
  case KIND_SOCKET:
    res = caml_alloc_small(1, 1);
    Store_field(res, 0, caml_copy_int64((int64_t)Socket_val(fd)));
    break;
  }
  CAMLreturn(res);
}

#else /* defined(_WIN32) */

/* +-----------------------------------------------------------------+
   | AT_FDCWD                                                        |
   +-----------------------------------------------------------------+ */

CAMLprim value shexp_at_fdcwd()
{
  return Val_int(AT_FDCWD);
}

/* +-----------------------------------------------------------------+
   | Flags                                                           |
   +-----------------------------------------------------------------+ */

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif

int shexp_open_flag_table[] = {
  O_RDONLY,
  O_WRONLY,
  O_RDWR,
  O_NONBLOCK,
  O_APPEND,
  O_CREAT,
  O_TRUNC,
  O_EXCL,
  O_NOCTTY,
  O_DSYNC,
  O_SYNC,
  O_RSYNC,
  0, /* O_SHARE_DELETE (windows only) */
  O_CLOEXEC
};

int shexp_at_flag_table[] = {
  AT_EACCESS,
  AT_SYMLINK_FOLLOW,
  AT_SYMLINK_NOFOLLOW,
  AT_REMOVEDIR
};

int shexp_access_permission_table[] = {
  R_OK,
  W_OK,
  X_OK,
  F_OK
};

/* +-----------------------------------------------------------------+
   | fstatat                                                         |
   +-----------------------------------------------------------------+ */

#ifndef S_IFLNK
#define S_IFLNK 0
#endif
#ifndef S_IFIFO
#define S_IFIFO 0
#endif
#ifndef S_IFSOCK
#define S_IFSOCK 0
#endif
#ifndef S_IFBLK
#define S_IFBLK 0
#endif

static int file_kind_table[] = {
  S_IFREG,
  S_IFDIR,
  S_IFCHR,
  S_IFBLK,
  S_IFLNK,
  S_IFIFO,
  S_IFSOCK
};

extern value cst_to_constr(int n, int * tbl, int size, int deflt);

static value alloc_stats(struct stat *buf)
{
  CAMLparam0();
  CAMLlocal5(atime, mtime, ctime, offset, v);

#if HAS_NANOSECOND_STAT == 1
#  define NSEC(buf, field) buf->st_##field##tim.tv_nsec
#elif HAS_NANOSECOND_STAT == 2
#  define NSEC(buf, field) buf->st_##field##timespec.tv_nsec
#elif HAS_NANOSECOND_STAT == 3
#  define NSEC(buf, field) buf->st_##field##timensec
#else
#  define NSEC(buf, field) 0
#endif
  atime = caml_copy_double((double) buf->st_atime
                           + (NSEC(buf, a) / 1000000000.0));
  mtime = caml_copy_double((double) buf->st_mtime
                           + (NSEC(buf, m) / 1000000000.0));
  ctime = caml_copy_double((double) buf->st_ctime
                           + (NSEC(buf, c) / 1000000000.0));
#undef NSEC
  offset = caml_copy_int64(buf->st_size);
  v = caml_alloc(12, 0);
  Field (v, 0) = Val_int (buf->st_dev);
  Field (v, 1) = Val_int (buf->st_ino);
  Field (v, 2) = cst_to_constr(buf->st_mode & S_IFMT, file_kind_table,
                               sizeof(file_kind_table) / sizeof(int), 0);
  Field (v, 3) = Val_int (buf->st_mode & 07777);
  Field (v, 4) = Val_int (buf->st_nlink);
  Field (v, 5) = Val_int (buf->st_uid);
  Field (v, 6) = Val_int (buf->st_gid);
  Field (v, 7) = Val_int (buf->st_rdev);
  Field (v, 8) = offset;
  Field (v, 9) = atime;
  Field (v, 10) = mtime;
  Field (v, 11) = ctime;
  CAMLreturn(v);
}

CAMLprim value shexp_fstatat(value v_dir, value v_path, value v_flags)
{
  CAMLparam3(v_dir, v_path, v_flags);
  int dir;
  char* path;
  int flags;
  struct stat buf;
  int res;

  caml_unix_check_path(v_path, "fstatat");
  dir   = Int_val(v_dir);
  path  = caml_strdup(String_val(v_path));
  flags = caml_convert_flag_list(v_flags, shexp_at_flag_table);

  caml_enter_blocking_section();
  res = fstatat(dir, path, &buf, flags);
  caml_leave_blocking_section();

  caml_stat_free(path);
  if (res == -1) uerror("fstatat", v_path);
  CAMLreturn(alloc_stats(&buf));
}

/* +-----------------------------------------------------------------+
   | readlinkat                                                      |
   +-----------------------------------------------------------------+ */

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 512
#endif
#endif

CAMLprim value shexp_readlinkat(value v_dir, value v_path)
{
  CAMLparam2(v_dir, v_path);
  CAMLlocal1(result);
  int dir;
  char* path;
  char buf[PATH_MAX];
  int res;

  caml_unix_check_path(v_path, "readlinkat");
  dir  = Int_val(v_dir);
  path = caml_strdup(String_val(v_path));

  caml_enter_blocking_section();
  res = readlinkat(dir, path, buf, sizeof(buf) - 1);
  caml_leave_blocking_section();

  caml_stat_free(path);
  if (res == -1) uerror("readlinkat", v_path);
  buf[res] = 0;
  CAMLreturn(caml_copy_string(buf));
}

/* +-----------------------------------------------------------------+
   | fdopendir                                                       |
   +-----------------------------------------------------------------+ */

CAMLprim value shexp_fdopendir(value v_dir)
{
  DIR *dp;
  value res;

  caml_enter_blocking_section();
  dp = fdopendir(Int_val(v_dir));
  caml_leave_blocking_section();

  if (dp == (DIR *) NULL) uerror("fdopendir", Nothing);

  res = caml_alloc_small(1, Abstract_tag);
  DIR_Val(res) = dp;
  return res;
}

/* +-----------------------------------------------------------------+
   | Fd info                                                         |
   +-----------------------------------------------------------------+ */

CAMLprim value shexp_fd_info(value fd)
{
  value res = caml_alloc_small(1, 2);
  Field(res, 0) = fd;
  return res;
}

#endif  /* defined(_WIN32) */
