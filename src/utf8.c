/*
 * Copyright (c) 2022 Calvin Rose and contributors
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

/* TODO: It might be wise to disallow overlong sequences for security reasons.
 * See https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt for a list
 * of test cases. */

JANET_FN(cfun_utf8_decode_rune,
        "(utf8/decode-rune buf &opt start)",
        "Read a UTF-8 encoded Unicode codepoint from the buffer which starts at the given index. Returns a tuple [value width], where width = number of bytes consumed. If at the end of buffer or the buffer contains malformed UTF-8, returns [nil 0].") {
    janet_arity(argc, 1, 2);
    JanetByteView buf = janet_getbytes(argv, 0);
    int32_t start;
    if (argc > 1) {
        start = janet_getinteger(argv, 1);
    } else {
        start = 0;
    }

    Janet res[2] = { janet_wrap_nil(), janet_wrap_integer(0) };
    if (start >= buf.len) {
        goto exit;
    }

    int32_t i = start;
    uint8_t a = buf.bytes[i++];
    if ((a & 0x80) == 0) {
        res[0] = janet_wrap_integer((int32_t)a);
    } else if ((a & 0xE0) == 0xC0) {
        if (i >= buf.len) goto exit;
        uint8_t b = buf.bytes[i++];
        if ((b & 0xC0) != 0x80) goto exit;
        res[0] = janet_wrap_integer((int32_t)(a & 0x1F) << 6 |
                                    (int32_t)(b & 0x3F));
    } else if ((a & 0xF0) == 0xE0) {
        if ((i + 1) >= buf.len) goto exit;
        uint8_t b = buf.bytes[i++],
                c = buf.bytes[i++];
        if ((b & 0xC0) != 0x80) goto exit;
        if ((c & 0xC0) != 0x80) goto exit;
        res[0] = janet_wrap_integer((int32_t)(a & 0x0F) << 12 |
                                    (int32_t)(b & 0x3F) <<  6 |
                                    (int32_t)(c & 0x3F));
    } else if ((a & 0xF8) == 0xF0) {
        if ((i + 2) >= buf.len) goto exit;
        uint8_t b = buf.bytes[i++],
                c = buf.bytes[i++],
                d = buf.bytes[i++];
        if ((b & 0xC0) != 0x80) goto exit;
        if ((c & 0xC0) != 0x80) goto exit;
        if ((d & 0xC0) != 0x80) goto exit;
        res[0] = janet_wrap_integer((int32_t)(a & 0x07) << 18 |
                                    (int32_t)(b & 0x3F) << 12 |
                                    (int32_t)(c & 0x3F) <<  6 |
                                    (int32_t)(d & 0x3F));
    } else {
        goto exit;
    }

    res[1] = janet_wrap_integer(i - start);
exit:
    return janet_wrap_tuple(janet_tuple_n(&res[0], 2));
}

JANET_FN(cfun_utf8_encode_rune,
        "(utf8/encode-rune rune &opt buf)",
        "Encode a Unicode codepoint into the end of a buffer.") {
    janet_arity(argc, 1, 2);
    uint32_t rune = (uint32_t)janet_getinteger(argv, 0);

    uint8_t enc[4];
    uint32_t len = 0;

    if (rune <= 0x7F) {
        enc[len++] = (uint8_t)rune;
    } else if (rune <= 0x7FF) {
        enc[len++] = (uint8_t)(0xC0 | ((rune >> 6) & 0x1F));
        enc[len++] = (uint8_t)(0x80 | (rune & 0x3F));
    } else if (rune <= 0xFFFF) {
        enc[len++] = (uint8_t)(0xE0 | ((rune >> 12) & 0x0F));
        enc[len++] = (uint8_t)(0x80 | ((rune >> 6) & 0x3F));
        enc[len++] = (uint8_t)(0x80 | (rune & 0x3F));
    } else if (rune <= 0x10FFFF) {
        enc[len++] = (uint8_t)(0xF0 | ((rune >> 18) & 0x07));
        enc[len++] = (uint8_t)(0x80 | ((rune >> 12) & 0x3F));
        enc[len++] = (uint8_t)(0x80 | ((rune >> 6) & 0x3F));
        enc[len++] = (uint8_t)(0x80 | (rune & 0x3F));
    } else {
        janet_panicf("character %d outsize UTF-8 range", (int)rune);
    }

    JanetBuffer *out;
    if (argc > 1) {
        out = janet_getbuffer(argv, 1);
    } else {
        out = janet_buffer((int32_t)len);
    }
    janet_buffer_push_bytes(out, &enc[0], (int32_t)len);
    return janet_wrap_buffer(out);
}

JANET_FN(cfun_utf8_prefixtowidth,
        "(utf8/prefix->width c)",
        "Given the first byte in an UTF-8 sequence, get the number of bytes that the codepoint sequence takes up, including the prefix byte.") {
    janet_fixarity(argc, 1);
    uint32_t c = (uint32_t)janet_getinteger(argv, 0);
    int32_t n = ((c & 0xF8) == 0xF0) ? 4 :
                ((c & 0xF0) == 0xE0) ? 3 :
                ((c & 0xE0) == 0xC0) ? 2 :
                                       1;
    return janet_wrap_integer(n);
}

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("decode-rune", cfun_utf8_decode_rune),
        JANET_REG("encode-rune", cfun_utf8_encode_rune),
        JANET_REG("prefix->width", cfun_utf8_prefixtowidth),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "utf8", cfuns);
}
