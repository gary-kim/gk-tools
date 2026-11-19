#include <stdint.h>
#include <string.h>
#include <sys/ioctl.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#define EC_CMD_UPDATE_KEYBOARD_MATRIX 0x3e0c
#define KEYBOARD_MATRIX_PAYLOAD_SIZE 136

struct cros_ec_command {
  uint32_t version;
  uint32_t command;
  uint32_t outsize;
  uint32_t insize;
  uint32_t result;
  uint8_t data[];
};

#define CROS_EC_DEV_IOCXCMD _IOWR(0xEC, 0, struct cros_ec_command)

union keyboard_matrix_command {
  struct cros_ec_command header;
  uint8_t storage[sizeof(struct cros_ec_command) + KEYBOARD_MATRIX_PAYLOAD_SIZE];
};

CAMLprim value gkt_ec_update_keyboard_matrix(value v_fd, value v_request) {
  CAMLparam2(v_fd, v_request);
  CAMLlocal2(v_payload, v_response);
  if (caml_string_length(v_request) != KEYBOARD_MATRIX_PAYLOAD_SIZE)
    caml_invalid_argument("gkt_ec_update_keyboard_matrix: request size");
  union keyboard_matrix_command cmd = {
    .header = {
      .version = 0,
      .command = EC_CMD_UPDATE_KEYBOARD_MATRIX,
      .outsize = KEYBOARD_MATRIX_PAYLOAD_SIZE,
      .insize = KEYBOARD_MATRIX_PAYLOAD_SIZE,
      .result = UINT32_MAX,
    },
  };
  memcpy(cmd.header.data, String_val(v_request), KEYBOARD_MATRIX_PAYLOAD_SIZE);
  int fd = Int_val(v_fd);
  caml_enter_blocking_section();
  int ret = ioctl(fd, CROS_EC_DEV_IOCXCMD, &cmd);
  caml_leave_blocking_section();
  if (ret == -1) caml_uerror("ioctl", Nothing);
  size_t received =
    ret < KEYBOARD_MATRIX_PAYLOAD_SIZE ? (size_t)ret : KEYBOARD_MATRIX_PAYLOAD_SIZE;
  v_payload = caml_alloc_initialized_string(received, (const char *)cmd.header.data);
  v_response = caml_alloc_tuple(2);
  Store_field(v_response, 0, Val_long(cmd.header.result));
  Store_field(v_response, 1, v_payload);
  CAMLreturn(v_response);
}
