#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

CAMLprim value gkt_rename_noreplace(value v_src, value v_dst) {
  CAMLparam2(v_src, v_dst);
  caml_unix_check_path(v_src, "renameat2");
  caml_unix_check_path(v_dst, "renameat2");
  char *src = caml_stat_strdup(String_val(v_src));
  char *dst = caml_stat_strdup(String_val(v_dst));
  caml_enter_blocking_section();
  int ret = renameat2(AT_FDCWD, src, AT_FDCWD, dst, RENAME_NOREPLACE);
  int err = errno;
  caml_leave_blocking_section();
  caml_stat_free(src);
  caml_stat_free(dst);
  if (ret == -1) caml_unix_error(err, "renameat2", v_dst);
  CAMLreturn(Val_unit);
}
