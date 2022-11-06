/*
 * Wrapper around miniz for compression functionality.
 */

#include <janet.h>
#include "../deps/miniz/miniz.h"

JANET_FN(cfun_compress,
        "(zip/compress bytes &opt level into)",
        "Compress data and write to a buffer. Different compression levels can "
        "be used - higher compression levels trade smaller output with longer compression times. "
        "Returns `into`. If `into` not provided, a new buffer is created.") {
    janet_arity(argc, 1, 3);
    JanetByteView bytes = janet_getbytes(argv, 0);
    int32_t level = janet_optinteger(argv, argc, 1, 5);
    if (level < 0 || level > 10) {
        janet_panicf("compression level must be between 0 and 10, got %d", level);
    }
    JanetBuffer *buffer = janet_optbuffer(argv, argc, 2, (bytes.len / 2) + 10);
    mz_ulong dest_len;
    int status;
    do {
        dest_len = buffer->capacity - buffer->count;
        status = mz_compress2(buffer->data + buffer->count, &dest_len, bytes.bytes, bytes.len, level);
        if (status == MZ_BUF_ERROR) {
            if (buffer->capacity == INT32_MAX) {
                janet_panic("too large to compress");
            }
            janet_buffer_ensure(buffer, 1 + buffer->capacity, 2);
        }
    } while (status == MZ_BUF_ERROR);
    if (status != MZ_OK) janet_panicf("compression failed with code %d: %s", status, mz_error(status));
    buffer->count += (int32_t) dest_len;
    return janet_wrap_buffer(buffer);
}

JANET_FN(cfun_decompress,
        "(zip/decompress bytes &opt into)",
        "Decompress data and write to a buffer. If an `into` buffer is not provided, a new buffer will be created.") {
    janet_arity(argc, 1, 2);
    JanetByteView bytes = janet_getbytes(argv, 0);
    JanetBuffer *buffer = janet_optbuffer(argv, argc, 1, bytes.len);
    mz_ulong dest_len;
    int status;
    do {
        dest_len = buffer->capacity - buffer->count;
        status = mz_uncompress(buffer->data + buffer->count, &dest_len, bytes.bytes, bytes.len);
        if (status == MZ_BUF_ERROR) {
            if (buffer->capacity == INT32_MAX) {
                janet_panic("too large to decompress");
            }
            janet_buffer_ensure(buffer, buffer->capacity + 1, 2);
        }
    } while (status == MZ_BUF_ERROR);
    if (status != MZ_OK) janet_panicf("compression failed with code %d: %s", status, mz_error(status));
    buffer->count += (int32_t) dest_len;
    return janet_wrap_buffer(buffer);
}

JANET_FN(cfun_version,
        "(zip/version)",
        "Get the version string of the underlying miniz library.") {
    janet_fixarity(argc, 0);
    return janet_cstringv(mz_version());
}

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("compress", cfun_compress),
        JANET_REG("decompress", cfun_decompress),
        JANET_REG("version", cfun_version),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "zip", cfuns);
}

