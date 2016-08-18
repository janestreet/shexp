#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#define get_buf(v_buf, v_pos) (char *) Caml_ba_data_val(v_buf) + Long_val(v_pos)

#if !defined(_WIN32)

CAMLprim value shexp_bigstring_io_read(value fd, value buf, value ofs, value len)
{
  CAMLparam1(buf);
  int ret;
  char *ptr = get_buf(buf, ofs);
  caml_enter_blocking_section();
  ret = read(Int_val(fd), ptr, Long_val(len));
  caml_leave_blocking_section();
  if (ret == -1) uerror("read", Nothing);
  CAMLreturn(Val_int(ret));
}

CAMLprim value shexp_bigstring_io_write(value fd, value buf, value ofs, value len)
{
  CAMLparam1(buf);
  int ret;
  char *ptr = get_buf(buf, ofs);
  caml_enter_blocking_section();
  ret = write(Int_val(fd), ptr, Long_val(len));
  caml_leave_blocking_section();
  if (ret == -1) uerror("write", Nothing);
  CAMLreturn(Val_int(ret));
}

#else

CAMLprim value shexp_bigstring_io_read(value fd, value buf, value ofs, value len)
{
  CAMLparam1(buf);
  DWORD numread;
  DWORD err = 0;
  char* buf = get_buf(buf, ofs);

  if (Descr_kind_val(fd) == KIND_SOCKET) {
    int ret;
    SOCKET s = Socket_val(fd);
    caml_enter_blocking_section();
    ret = recv(s, buf, Long_val(len), 0);
    if (ret == SOCKET_ERROR) err = WSAGetLastError();
    caml_leave_blocking_section();
    numread = ret;
  } else {
    HANDLE h = Handle_val(fd);
    caml_enter_blocking_section();
    if (! ReadFile(h, buf, Long_val(len), &numread, NULL))
      err = GetLastError();
    caml_leave_blocking_section();
  }
  if (err) {
    win32_maperr(err);
    uerror("read", Nothing);
  }
  return Val_int(numread);
  CAMLreturn(Val_int(numread));
}

CAMLprim value shexp_bigstring_io_write(value fd, value buf, value ofs, value len)
{
  CAMLparam1(buf);
  DWORD numwritten;
  DWORD err = 0;
  char* buf = get_buf(buf, ofs);

  if (Descr_kind_val(fd) == KIND_SOCKET) {
    int ret;
    SOCKET s = Socket_val(fd);
    caml_enter_blocking_section();
    ret = send(s, buf, Long_val(len), 0);
    if (ret == SOCKET_ERROR) err = WSAGetLastError();
    caml_leave_blocking_section();
    numwritten = ret;
  } else {
    HANDLE h = Handle_val(fd);
    caml_enter_blocking_section();
    if (! WriteFile(h, src, Long_val(len), &numwritten, NULL))
      err = GetLastError();
    caml_leave_blocking_section();
  }
  if (err) {
    win32_maperr(err);
    uerror("write", Nothing);
  }
  return Val_int(numwritten);
  CAMLreturn(Val_int(numwritten));
}

#endif
