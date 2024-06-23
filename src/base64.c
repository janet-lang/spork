/*
* Copyright (c) 2024 Janet contributors
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to
* deal in the Software without restriction, including without limitation the
* rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
* sell copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
* IN THE SOFTWARE.
*/

#include <janet.h>

const char *const table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static inline uint8_t encode_sextet_1(uint8_t byte1) {
  return table[byte1 >> 2];
}

static inline uint8_t encode_sextet_2(uint8_t byte1, uint8_t byte2) {
  return table[((byte1 & 3) << 4) | (byte2 >> 4)];
}

static inline uint8_t encode_sextet_3(uint8_t byte2, uint8_t byte3) {
  return table[((byte2 & 15) << 2) | (byte3 >> 6)];
}

static inline uint8_t encode_sextet_4(uint8_t byte3) {
  return table[byte3 & 63];
}

static Janet base64_encode(int32_t argc, Janet *argv) {
  janet_fixarity(argc, 1);
  const uint8_t *in = janet_getstring(argv, 0);
  int32_t inlen = janet_length(argv[0]);
  int rem = inlen % 3;
  JanetBuffer *outbuf = janet_buffer(((inlen + 3 - rem) / 3) * 4);
  int cursor = 0;
  for (; cursor < (inlen - rem); cursor += 3) {
    janet_buffer_push_u8(outbuf, encode_sextet_1(in[cursor]));
    janet_buffer_push_u8(outbuf, encode_sextet_2(in[cursor], in[cursor + 1]));
    janet_buffer_push_u8(outbuf, encode_sextet_3(in[cursor + 1], in[cursor + 2]));
    janet_buffer_push_u8(outbuf, encode_sextet_4(in[cursor + 2]));
  }
  if (rem == 1) {
    janet_buffer_push_u8(outbuf, encode_sextet_1(in[cursor]));
    janet_buffer_push_u8(outbuf, encode_sextet_2(in[cursor], in[cursor + 1]));
    janet_buffer_push_u8(outbuf, '=');
    janet_buffer_push_u8(outbuf, '=');
  } else if (rem == 2) {
    janet_buffer_push_u8(outbuf, encode_sextet_1(in[cursor]));
    janet_buffer_push_u8(outbuf, encode_sextet_2(in[cursor], in[cursor + 1]));
    janet_buffer_push_u8(outbuf, encode_sextet_3(in[cursor + 1], in[cursor + 2]));
    janet_buffer_push_u8(outbuf, '=');
  }
  return janet_stringv(outbuf->data, outbuf->count);
}

static uint8_t decode_character(uint8_t c) {
  if (c >= 'a') {
    return c - 97 + 26;
  } else if (c >= 'A') {
    return c - 65;
  } else if (c >= '0' && c <= '9') {
    return c - 48 + 52;
  } else if (c == '+') {
    return 62;
  } else if (c == '/') {
    return 63;
  } else {
    janet_panicf("Wrong character: %c", c);
  }
}

static inline uint8_t decode_byte_1(uint8_t sextet1, uint8_t sextet2) {
  return (sextet1 << 2) | (sextet2 >> 4);
}

static inline uint8_t decode_byte_2(uint8_t sextet2, uint8_t sextet3) {
  return (sextet2 << 4) | (sextet3 >> 2);
}

static inline uint8_t decode_byte_3(uint8_t sextet3, uint8_t sextet4) {
  return (sextet3 << 6) | sextet4;
}

static Janet base64_decode(int32_t argc, Janet *argv) {
  janet_fixarity(argc, 1);
  int32_t inlen = janet_length(argv[0]);
  if (inlen % 4 != 0) {
    janet_panicf("Wrong length: %d", inlen);
  }
  const uint8_t *in = janet_getstring(argv, 0);
  int padding = 0;
  int end = inlen;
  if (in[inlen - 2] == '=') {
    end -= 4;
    padding = 2;
  } else if (in[inlen - 1] == '=') {
    end -= 4;
    padding = 1;
  }
  JanetBuffer *outbuf = janet_buffer((inlen / 4) * 3);
  int cursor = 0;
  uint8_t sextet1, sextet2, sextet3, sextet4;
  for (; cursor < end; cursor += 4) {
    sextet1 = decode_character(in[cursor]);
    sextet2 = decode_character(in[cursor + 1]);
    sextet3 = decode_character(in[cursor + 2]);
    sextet4 = decode_character(in[cursor + 3]);
    janet_buffer_push_u8(outbuf, decode_byte_1(sextet1, sextet2));
    janet_buffer_push_u8(outbuf, decode_byte_2(sextet2, sextet3));
    janet_buffer_push_u8(outbuf, decode_byte_3(sextet3, sextet4));
  }
  if (padding == 2) {
    sextet1 = decode_character(in[cursor]);
    sextet2 = decode_character(in[cursor + 1]);
    janet_buffer_push_u8(outbuf, decode_byte_1(sextet1, sextet2));
  } else if (padding == 1) {
    sextet1 = decode_character(in[cursor]);
    sextet2 = decode_character(in[cursor + 1]);
    sextet3 = decode_character(in[cursor + 2]);
    janet_buffer_push_u8(outbuf, decode_byte_1(sextet1, sextet2));
    janet_buffer_push_u8(outbuf, decode_byte_2(sextet2, sextet3));
  }
  return janet_stringv(outbuf->data, outbuf->count);
}

static const JanetReg cfuns[] = {
  {
    "encode",
    base64_encode,
    "(base64/encode x)\n\nEncodes a string in Base64. Returns encoded string."
  },
  {
    "decode",
    base64_decode,
    "(base64/decode x)\n\nDecodes a string from Base64. Returns decoded string."
  },
  {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
  janet_cfuns(env, "base64", cfuns);
}
