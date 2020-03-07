#define _GNU_SOURCE

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>

#define get_buf(v_buf, v_pos) (char *) Caml_ba_data_val(v_buf) + Long_val(v_pos)

CAMLprim value shexp_bigstring_blit_stub(
  value v_src, value v_src_pos, value v_dst, value v_dst_pos, value v_len)
{
  char *src = get_buf(v_src, v_src_pos);
  char *dst = get_buf(v_dst, v_dst_pos);
  memmove(dst, src, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value shexp_bigstring_blit_string_bigstring_stub(
  value v_src, value v_src_pos, value v_buf, value v_dst_pos, value v_len)
{
  const char *str = String_val(v_src) + Long_val(v_src_pos);
  char *buf = get_buf(v_buf, v_dst_pos);
  memmove(buf, str, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value shexp_bigstring_blit_bigstring_bytes_stub(
  value v_buf, value v_src_pos, value v_dst, value v_dst_pos, value v_len)
{
  char *buf = get_buf(v_buf, v_src_pos);
  unsigned char *str = Bytes_val(v_dst) + Long_val(v_dst_pos);
  memmove(str, buf, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value shexp_bigstring_index(value v_buf, value v_pos, value v_len, value v_chr)
{
  char *buf = (char*)Caml_ba_data_val(v_buf);
  char* res = (char*)memchr((void*)(buf + Long_val(v_pos)),
                            Int_val(v_chr),
                            Long_val(v_len));
  if (res == NULL)
    return Val_long(-1);
  else
    return Val_long((intnat)(res - buf));
}

CAMLprim value shexp_bigstring_rindex(value v_buf, value v_pos, value v_len, value v_chr)
{
  char *buf = (char*)Caml_ba_data_val(v_buf);
  char* res = (char*)memchr((void*)(buf + Long_val(v_pos)),
                            Int_val(v_chr),
                            Long_val(v_len));
  if (res == NULL)
    return Val_long(-1);
  else
    return Val_long((intnat)(res - buf));
}

CAMLprim value shexp_bigstring_create_temporary(value template, value v_size)
{
  CAMLparam1(template);
  CAMLlocal1(result);
  struct caml_ba_array *ba;
  uintnat i;
  intnat size = Long_val(v_size);

  /* Duplicate [template] */
  result = caml_alloc(Wosize_val(template), Tag_val(template));
  for (i = 0; i < Wosize_val(template); i++)
    Field(result, i) = Field(template, i);

  ba         = Caml_ba_array_val(result);
  ba->data   = caml_stat_alloc(size);
  ba->flags  = (ba->flags & ~CAML_BA_MANAGED_MASK) | CAML_BA_EXTERNAL;
  ba->dim[0] = size;

  CAMLreturn(result);
}

CAMLprim value shexp_bigstring_destroy_temporary(value v_ba)
{
  struct caml_ba_array *ba = Caml_ba_array_val(v_ba);
  assert(ba->data != NULL &&
         (ba->flags & CAML_BA_MANAGED_MASK) == CAML_BA_EXTERNAL);
  free(ba->data);
  ba->dim[0] = 0;
  ba->data = NULL;
  return Val_unit;
}

CAMLprim value shexp_bigstring_resize_temporary(value v_ba, value v_size)
{
  void *data;
  intnat size = Long_val(v_size);
  struct caml_ba_array *ba = Caml_ba_array_val(v_ba);
  assert((ba->flags & CAML_BA_MANAGED_MASK) == CAML_BA_EXTERNAL);
  data = realloc(ba->data, size);
  if (data == NULL) caml_raise_out_of_memory();
  ba->data   = data;
  ba->dim[0] = size;
  return Val_unit;
}
