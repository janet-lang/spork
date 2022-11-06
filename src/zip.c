/*
 * Wrapper around miniz for compression functionality.
 */

#include <janet.h>
#include <string.h>
#include "../deps/miniz/miniz.h"

/* General compression functionality */

JANET_FN(cfun_compress,
        "(zip/compress bytes &opt level into)",
        "Compress data and write to a buffer. Different compression levels can "
        "be used - higher compression levels trade smaller output with longer compression times. "
        "Returns `into`. If `into` not provided, a new buffer is created.") {
    janet_arity(argc, 1, 3);
    JanetByteView bytes = janet_getbytes(argv, 0);
    int32_t level = janet_optinteger(argv, argc, 1, MZ_DEFAULT_LEVEL);
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

/* Zip writing */

typedef struct {
    mz_zip_archive archive;
    JanetString path;
    int level;
    int archive_initialized;
    int archive_finalized;
} ZipWriter;

static int writer_gc(void *p, size_t size) {
    (void) size;
    ZipWriter *writer = (ZipWriter *)p;
    if (writer->archive_initialized) {
        mz_zip_writer_end(&writer->archive);
        writer->archive_initialized = 0;
    }
    return 0;
}

static int writer_mark(void *p, size_t size) {
    (void) size;
    ZipWriter *writer = (ZipWriter *)p;
    janet_mark(janet_wrap_string(writer->path));
    return 0;
}

static const JanetAbstractType zip_writer_type = {
    .name = "zip/writer",
    .gc = writer_gc,
    .gcmark = writer_mark,
};

JANET_FN(cfun_write_file,
        "(zip/write-file dest-path &opt level)",
        "Create a new zip archive writer") {
    janet_arity(argc, 1, 2);
    JanetString path = janet_getstring(argv, 0);
    int32_t level = janet_optinteger(argv, argc, 1, MZ_DEFAULT_LEVEL);
    if (level < 0 || level > 10) {
        janet_panicf("compression level must be between 0 and 10, got %d", level);
    }
    ZipWriter *writer = janet_abstract(&zip_writer_type, sizeof(ZipWriter));
    writer->path = path;
    writer->level = level;
    mz_zip_writer_init_file_v2(&writer->archive, (const char *)path, 0, 0);
    writer->archive_initialized = 1;
    writer->archive_finalized = 0;
    return janet_wrap_abstract(writer);
}

JANET_FN(cfun_writer_add_file,
        "(zip/writer-add-file writer path filename &opt comment)",
        "Add a file to the zip writer.") {
    janet_arity(argc, 3, 4);
    ZipWriter *writer = janet_getabstract(argv, 0, &zip_writer_type);
    if (!writer->archive_initialized || writer->archive_finalized) janet_panic("writer closed");
    const char *path = janet_getcstring(argv, 1);
    const char *filename = janet_getcstring(argv, 2);
    const char *comment = janet_optcstring(argv, argc, 3, NULL);
    if (!mz_zip_writer_add_file(&writer->archive, path, filename, comment, (comment != NULL) ? strlen(comment) : 0, MZ_DEFAULT_COMPRESSION)) {
        janet_panic("adding file failed!");
    }
    return argv[0];
}

JANET_FN(cfun_writer_add_bytes,
        "(zip/writer-add-bytes writer path data &opt comment)",
        "Add a byte sequence to the zip writer.") {
    janet_arity(argc, 3, 4);
    ZipWriter *writer = janet_getabstract(argv, 0, &zip_writer_type);
    if (!writer->archive_initialized || writer->archive_finalized) janet_panic("writer closed");
    const char *path = janet_getcstring(argv, 1);
    JanetByteView bytes = janet_getbytes(argv, 2);
    const char *comment = janet_optcstring(argv, argc, 3, NULL);
    if (!mz_zip_writer_add_mem_ex(&writer->archive, path, bytes.bytes, bytes.len, comment, (comment != NULL) ? strlen(comment) : 0, MZ_DEFAULT_COMPRESSION, 0, 0)) {
        janet_panic("adding bytes failed!");
    }
    return argv[0];
}

JANET_FN(cfun_writer_close,
        "(zip/writer-close writer)",
        "Close a ZipWriter.") {
    janet_fixarity(argc, 1);
    ZipWriter *writer = janet_getabstract(argv, 0, &zip_writer_type);
    if (!writer->archive_initialized) janet_panic("writer closed");
    if (!writer->archive_finalized) {
        mz_zip_writer_finalize_archive(&writer->archive);
        writer->archive_finalized = 1;
    }
    mz_zip_writer_end(&writer->archive);
    writer->archive_initialized = 0;
    return janet_wrap_nil();
}

JANET_FN(cfun_writer_finalize,
        "(zip/writer-finalizer writer)",
        "Finalize a writer, writing any zip files to disk. Return the writer.") {
    janet_fixarity(argc, 1);
    ZipWriter *writer = janet_getabstract(argv, 0, &zip_writer_type);
    if (!writer->archive_initialized || writer->archive_finalized) janet_panic("writer closed");
    if (!mz_zip_writer_finalize_archive(&writer->archive)) {
        janet_panic("failed to finalize archive");
    }
    writer->archive_finalized = 1;
    return argv[0];
}

/* Extra */

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
        JANET_REG("write-file", cfun_write_file),
        JANET_REG("writer-add-file", cfun_writer_add_file),
        JANET_REG("writer-add-bytes", cfun_writer_add_bytes),
        JANET_REG("writer-close", cfun_writer_close),
        JANET_REG("writer-finalize", cfun_writer_finalize),
        JANET_REG("version", cfun_version),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "zip", cfuns);
}

