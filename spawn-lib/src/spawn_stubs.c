#define _GNU_SOURCE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>

#if !defined(_WIN32)

#include <assert.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>

/* +-----------------------------------------------------------------+
   | Code executed in the child                                      |
   +-----------------------------------------------------------------+ */

enum error_arg { NOTHING, CWD, PROG };

/* Structure used to communicate errors from the child to the
   parent. */
struct subprocess_failure {
  /* Value of [errno]. */
  int error;
  /* System call that failed */
  char function[32];
  /* What to pass as third argument of the Unix_error exception. */
  enum error_arg arg;
};

/* Compile time asserts as described here:

   http://stackoverflow.com/questions/807244/c-compiler-asserts-how-to-implement */
#define CASSERT(predicate) _impl_CASSERT_LINE(predicate,__LINE__,__FILE__)
#define _impl_PASTE(a,b) a##b
#define _impl_CASSERT_LINE(predicate, line, file) \
    typedef char _impl_PASTE(assertion_failed_##file##_,line)[2*!!(predicate)-1];

/* Report an error to the parent. Use the current value of [errno] as
   error number. */
static void subprocess_failure(int failure_fd, char *function, enum error_arg error_arg)
{
  struct subprocess_failure failure;
  sigset_t sigset;
  CASSERT(sizeof(failure.function) == 32)
  CASSERT(sizeof(failure) < PIPE_BUF)

  failure.error = errno;
  failure.arg   = error_arg;
  strncpy(failure.function, function, sizeof(failure.function));

  /* Block all signals to avoid being interrupted in write.
     Although most of the call sites of [subprocess_failure] already block
     signals, the one after the [exec] does not. */
  sigfillset(&sigset);
  pthread_sigmask(SIG_SETMASK, &sigset, NULL);

  /* Write is atomic as buffer is smaller than PIPE_BUF
     (required by POSIX.1-2001, as claimed in [man 7 pipe]) */
  write(failure_fd, &failure, sizeof(failure));
  _exit(127);
}

/* same as [dup] but ensures the result is >= 3. */
static int safe_dup(int failure_fd, int fd)
{
  int new_fd = dup(fd);
  if (new_fd == -1) subprocess_failure(failure_fd, "dup", NOTHING);
  if (new_fd >= 3)
    return new_fd;
  else {
    int result = safe_dup(failure_fd, fd);
    close(new_fd);
    return result;
  }
}

enum working_dir_kind { PATH, FD, INHERIT };

struct spawn_info {
  char **env; /* can be 0, in which case the current environment is used */
  enum working_dir_kind cwd_kind;
  union {
    int fd;
    char *path;
  } cwd; /* Only filled if [cwd_kind != INHERIT] */
  char *prog;
  char **argv;
  int std_fds[3];
};

static void subprocess(int failure_fd, struct spawn_info *info)
{
  int i, fd, tmp_fds[3];
  struct sigaction sa;
  sigset_t sigset;

  /* Restore all signals to their default behavior before unblocking
     them, to avoid invoking handlers from the parent */
  sa.sa_handler = SIG_DFL;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  /* Ignore errors as there is no interesting way it can fail. */
  for (i = 1; i < NSIG; i++) sigaction(i, &sa, NULL);

  switch (info->cwd_kind) {
  case INHERIT: break;
  case PATH:
    if (chdir(info->cwd.path) == -1)
      subprocess_failure(failure_fd, "chdir", CWD);
    break;
  case FD:
    if (fchdir(info->cwd.fd) == -1)
      subprocess_failure(failure_fd, "fchdir", NOTHING);
    close(info->cwd.fd);
    break;
  }

  /* Use temporary file descriptors for redirections to avoid problems
     when redirecting stdout to stderr for instance-> */

  for (fd = 0; fd < 3; fd++)
    if (info->std_fds[fd] != fd)
      tmp_fds[fd] = safe_dup(failure_fd, info->std_fds[fd]);

  for (fd = 0; fd < 3; fd++)
    if (info->std_fds[fd] != fd)
      close(info->std_fds[fd]);

  for (fd = 0; fd < 3; fd++)
    if (info->std_fds[fd] != fd) {
      if (dup2(tmp_fds[fd], fd) == -1)
        subprocess_failure(failure_fd, "dup2", NOTHING);
      close(tmp_fds[fd]);
    }

  sigemptyset(&sigset);
  pthread_sigmask(SIG_SETMASK, &sigset, NULL);

  if (info->env)
    execve(info->prog, info->argv, info->env);
  else
    execv(info->prog, info->argv);
  subprocess_failure(failure_fd, "execve", PROG);
}

/* +-----------------------------------------------------------------+
   | Parent code                                                     |
   +-----------------------------------------------------------------+ */

/* Convert a [string list] into a NULL terminated array of C
   strings.

   We don't reuse the [cstringvect] function from [unix_support.h] as
   it doesn't copy the strings in the array.
*/
static char **alloc_string_vect(value v)
{
  char **result;
  mlsize_t count, i, full_size;
  value x;
  char *ptr;

  count = 0;
  full_size = sizeof(char*);
  for (x = v; Is_block(x); x = Field(x, 1)) {
    count++;
    full_size += sizeof(char*) + caml_string_length(Field(x, 0)) + 1;
  }

  /* Allocate the array of pointers followed by the space to copy the
     strings as one block. */
  result = (char**)caml_stat_alloc(full_size);

  ptr = ((char*)result) + (sizeof(char*) * (count + 1));
  for (x = v, i = 0; Is_block(x); x = Field(x, 1), i++) {
    value v_str = Field(x, 0);
    mlsize_t len = caml_string_length(v_str) + 1;
    memcpy(ptr, String_val(v_str), len);
    result[i] = ptr;
    ptr += len;
  }
  result[count] = NULL;

  return result;
}

static void free_spawn_info(struct spawn_info *info)
{
  if (info->cwd_kind == PATH) caml_stat_free(info->cwd.path);
  if (info->prog) caml_stat_free(info->prog);
  if (info->argv) caml_stat_free(info->argv);
  if (info->env)  caml_stat_free(info->env);
}

CAMLprim value shexp_spawn_unix(value v_env,
                                value v_cwd,
                                value v_prog,
                                value v_argv,
                                value v_stdin,
                                value v_stdout,
                                value v_stderr,
                                value v_use_vfork)
{
  CAMLparam4(v_env, v_cwd, v_prog, v_argv);
  pid_t ret;
  struct spawn_info info;
  int result_pipe[2];
  int cancel_state;
  sigset_t sigset;
  sigset_t saved_procmask;
  struct subprocess_failure failure;
  int got_error = 0;
  int status;

  info.std_fds[0] = Int_val(v_stdin);
  info.std_fds[1] = Int_val(v_stdout);
  info.std_fds[2] = Int_val(v_stderr);

  if (Is_long(v_cwd)) {
    assert(v_cwd == Val_long(0));
    info.cwd_kind = INHERIT;
  } else {
    switch (Tag_val(v_cwd)) {
    case 0: /* Path of string */
      assert (Tag_val(Field(v_cwd, 0)) == String_tag);
      info.cwd_kind = PATH;
      info.cwd.path = caml_strdup(String_val(Field(v_cwd, 0)));
      break;
    case 1: /* Fd of Unix.file_descr */
      assert (Is_long(Field(v_cwd, 0)));
      info.cwd_kind = FD;
      info.cwd.fd = Int_val(Field(v_cwd, 0));
      break;
    default:
      assert(0);
    }
  }

  info.prog = caml_strdup(String_val(v_prog));
  info.argv = alloc_string_vect(v_argv);
  info.env  = Is_block(v_env) ? alloc_string_vect(Field(v_env, 0)) : NULL;

  caml_enter_blocking_section();

  /* Block signals and thread cancellation. When using vfork, the
     child might share the signal handlers.

     It's not clear that we need the call to [pthread_setcancelstate],
     but implementations of posix_spawn based on vfork are doing this.

     For instance:
     http://git.musl-libc.org/cgit/musl/tree/src/process/posix_spawn.c
  */
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancel_state);
  sigfillset(&sigset);
  pthread_sigmask(SIG_SETMASK, &sigset, &saved_procmask);

  /* Pipe used by the child to send errors to the parent. */
  if (pipe2(result_pipe, O_CLOEXEC) == -1) {
    int error = errno;
    free_spawn_info(&info);
    unix_error(error, "pipe2", Nothing);
  }

  ret = Bool_val(v_use_vfork) ? vfork() : fork();

  if (ret == 0) {
    close(result_pipe[0]);
    subprocess(result_pipe[1], &info);
  }
  failure.error = errno;

  free_spawn_info(&info);
  close(result_pipe[1]);

  got_error = 0;
  if (ret == -1) {
    got_error = 1;
    strncpy(failure.function, "vfork", sizeof(failure.function));
    failure.arg = NOTHING;
  } else {
    intnat res = read(result_pipe[0], &failure, sizeof(failure));
    if (res) {
      got_error = 1;
      if (res != sizeof(failure)) {
        /* It's not clear this can happen, but just to be safe side */
        failure.error = (res == -1) ? errno : EINVAL;
        strncpy(failure.function, "read", sizeof(failure.function));
        failure.arg = NOTHING;
      };
      /* If [read] did fail for some reason then we might be stuck
         here for a while. Other implementation of posix_spawn just
         assume that [read(...) != sizeof(failure)] is a success. */
      if (got_error) waitpid(ret, &status, 0);
    }
  }

  close(result_pipe[0]);
  pthread_sigmask(SIG_SETMASK, &saved_procmask, NULL);
  pthread_setcancelstate(cancel_state, NULL);

  caml_leave_blocking_section();

  if (got_error) {
    value arg = Nothing;
    switch (failure.arg) {
    case NOTHING: arg = Nothing;         break;
    case CWD    : arg = Field(v_cwd, 0); break;
    case PROG   : arg = v_prog;          break;
    }
    unix_error(failure.error, failure.function, arg);
  }

  CAMLreturn(Val_int(ret));
}

CAMLprim value shexp_spawn_windows()
{
  unix_error(ENOSYS, "shexp_spawn_windows", Nothing);
}

#else
CAMLprim value shexp_spawn_unix()
{
  unix_error(ENOSYS, "shexp_spawn_unix", Nothing);
}

CAMLprim value shexp_spawn_windows(value v_env,
                                   value v_cwd,
                                   value v_prog,
                                   value v_cmdline,
                                   value v_stdin,
                                   value v_stdout,
                                   value v_stderr)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;

  ZeroMemory(&si, sizeof(si));
  ZeroMemory(&pi, sizeof(pi));
  si.cb = sizeof(si);
  si.dwFlags    = STARTF_USESTDHANDLES;
  si.hStdInput  = Handle_val(v_stdin);
  si.hStdOutput = Handle_val(v_stdout);
  si.hStdError  = Handle_val(v_stderr);

  if (!CreateProcess(String_val(v_prog),
                     String_val(v_cmdline),
                     NULL,
                     NULL,
                     TRUE,
                     0,
                     Is_block(v_env) ? String_val(Field(v_env, 0)) : NULL,
                     Is_block(v_cwd) ? String_val(Field(v_cwd, 0)) : NULL,
                     &si,
                     &pi)) {
    win32_maperr(GetLastError());
    uerror("CreateProcess", Nothing);
  }

  CloseHandle(pi.hThread);

  return Val_long(pi.hProcess);
}

#endif

CAMLprim value shexp_spawn_unix_byte(value * argv)
{
  return shexp_spawn_unix(argv[0],
                          argv[1],
                          argv[2],
                          argv[3],
                          argv[4],
                          argv[5],
                          argv[6],
                          argv[7]);
}

CAMLprim value shexp_spawn_windows_byte(value * argv)
{
  return shexp_spawn_windows(argv[0],
                             argv[1],
                             argv[2],
                             argv[3],
                             argv[4],
                             argv[5],
                             argv[6]);
}
