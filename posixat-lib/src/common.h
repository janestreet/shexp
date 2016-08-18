#define _ATFILE_SOURCE
#define _GNU_SOURCE

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/fail.h>

#define NA(name)                                \
  CAMLprim value shexp_##name()                 \
  {                                             \
    unix_error(ENOSYS, #name, Nothing);         \
  }                                             \
                                                \
  CAMLprim value shexp_has_##name()             \
  {                                             \
    return Val_false;                           \
  }

#if !defined(_WIN32)
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <dirent.h>
#include <stdio.h>
#include <errno.h>
#endif

extern int shexp_at_flag_table[];
extern int shexp_open_flag_table[];
extern int shexp_access_permission_table[];

