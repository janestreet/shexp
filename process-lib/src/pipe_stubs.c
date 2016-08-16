#define _GNU_SOURCE

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>

#if defined(_WIN32)

CAMLprim value shexp_pipe2()
{
  unix_error(ENOSYS, "pipe2", Nothing);
}

#else

#include <unistd.h>
#include <fcntl.h>


CAMLprim value shexp_pipe2()
{
  int fd[2];
  value res;
  if (pipe2(fd, O_CLOEXEC) == -1)
    uerror("pipe", Nothing);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);
  return res;
}

#endif
