/*
 * Wrapper around miniz for compression functionality.
 */

#include <janet.h>
#include <string.h>
#include "../deps/miniz/miniz.h"

/* helpers */

static mz_uint miniz_optflags(const Janet *argv, int32_t argc, int32_t n) {
    if (argc <= n || janet_checktype(argv[n], JANET_NIL)) return 0;
    return (mz_uint) janet_getflags(argv, n, "ipcslh6rfw") << 8;
}

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

/* Zip reading */

static int reader_gc(void *p, size_t size) {
    (void) size;
    mz_zip_archive *archive = p;
    mz_zip_reader_end(archive);
    return 0;
}

static const JanetAbstractType zip_reader_type = {
    .name = "zip/reader",
    .gc = reader_gc
};

JANET_FN(cfun_read_file,
        "(zip/read-file filename &opt flags)",
        "Read a file as a zip archive. Returns a new zip reader.") {
    janet_arity(argc, 1, 2);
    const char *path = janet_getcstring(argv, 0);
    mz_zip_archive *archive = janet_abstract(&zip_reader_type, sizeof(mz_zip_archive));
    mz_uint flags = miniz_optflags(argv, argc, 1);
    mz_zip_zero_struct(archive);
    mz_zip_reader_init_file_v2(archive, path, MZ_DEFAULT_COMPRESSION, flags, 0);
    return janet_wrap_abstract(archive);
}

JANET_FN(cfun_read_bytes,
        "(zip/read-bytes bytes &opt flags)",
        "Read a byte sequence as a zip archive. Returns a new zip reader.") {
    janet_arity(argc, 1, 2);
    JanetByteView bytes = janet_getbytes(argv, 0);
    mz_zip_archive *archive = janet_abstract(&zip_reader_type, sizeof(mz_zip_archive));
    mz_uint flags = miniz_optflags(argv, argc, 1);
    mz_zip_zero_struct(archive);
    mz_zip_reader_init_mem(archive, bytes.bytes, bytes.len, flags);
    return janet_wrap_abstract(archive);
}

JANET_FN(cfun_reader_count,
        "(zip/reader-count reader)",
        "Get the number of files inside the zip archive. The files can be indexed ") {
    janet_fixarity(argc, 1);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    return janet_wrap_integer(mz_zip_reader_get_num_files(archive));
}

JANET_FN(cfun_reader_close,
        "(zip/reader-close reader)",
        "Close a reader and free related memory.") {
    janet_fixarity(argc, 1);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    mz_zip_reader_end(archive);
    return janet_wrap_nil();
}

JANET_FN(cfun_reader_idx_to_path,
        "(zip/get-filename reader idx)",
        "Convert a file index index in the archive to a filename.") {
    janet_fixarity(argc, 2);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    int32_t idx = janet_getinteger(argv, 1);
    size_t num_bytes = mz_zip_reader_get_filename(archive, idx, NULL, 0);
    uint8_t *str = janet_string_begin(num_bytes - 1); /* spacing for extra null byte */
    mz_zip_reader_get_filename(archive, idx, (char *) str, num_bytes);
    return janet_wrap_string(janet_string_end(str));
}

JANET_FN(cfun_reader_locate,
        "(zip/locate-file reader path &opt comment flags)",
        "Get the index of a particular filename in the archive.") {
    janet_arity(argc, 2, 4);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    const char *path = janet_getcstring(argv, 1);
    const char *comment = janet_optcstring(argv, argc, 2, NULL);
    mz_uint flags = miniz_optflags(argv, argc, 3);
    mz_uint index = mz_zip_reader_locate_file(archive, path, comment, flags);
    return janet_wrap_number((double) index);
}

JANET_FN(cfun_reader_stat,
        "(zip/stat reader idx)",
        "Get stat information of file. Returns a new struct with the following fields:\n\n"
        "* :index - integer index in master directory\n"
        "* :version-made-by - zip verstion\n"
        "* :version-needed - zip version needed to unzip\n"
        "* :bit-flag\n"
        "* :method - compression method\n"
        "* :time - file time\n"
        "* :crc32 - checksum of file contents\n"
        "* :comp-size - size of file when compressed\n"
        "* :uncomp-size - size of file when uncompressed\n"
        "* :filename\n"
        "* :comment\n"
        "* :internal-attr\n"
        "* :external-attr\n") {
    janet_fixarity(argc, 2);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    int32_t index = janet_getinteger(argv, 1);
    mz_zip_archive_file_stat statdata;
    mz_zip_reader_file_stat(archive, index, &statdata);
    JanetKV *st = janet_struct_begin(13);
    janet_struct_put(st, janet_ckeywordv("index"), janet_wrap_integer(statdata.m_file_index));
    janet_struct_put(st, janet_ckeywordv("version-made-by"), janet_wrap_integer(statdata.m_version_made_by));
    janet_struct_put(st, janet_ckeywordv("version-needed"), janet_wrap_integer(statdata.m_version_needed));
    janet_struct_put(st, janet_ckeywordv("bit-flag"), janet_wrap_integer(statdata.m_bit_flag));
    janet_struct_put(st, janet_ckeywordv("method"), janet_wrap_integer(statdata.m_method));
    janet_struct_put(st, janet_ckeywordv("time"), janet_wrap_integer(statdata.m_time));
    janet_struct_put(st, janet_ckeywordv("crc32"), janet_wrap_integer(statdata.m_crc32));
    janet_struct_put(st, janet_ckeywordv("comp-size"), janet_wrap_integer(statdata.m_comp_size));
    janet_struct_put(st, janet_ckeywordv("uncomp-size"), janet_wrap_integer(statdata.m_uncomp_size));
    janet_struct_put(st, janet_ckeywordv("internal-attr"), janet_wrap_integer(statdata.m_internal_attr));
    janet_struct_put(st, janet_ckeywordv("external-attr"), janet_wrap_integer(statdata.m_external_attr));
    janet_struct_put(st, janet_ckeywordv("filename"), janet_cstringv(statdata.m_filename));
    janet_struct_put(st, janet_ckeywordv("comment"), janet_stringv((uint8_t *)statdata.m_comment, statdata.m_comment_size));
    return janet_wrap_struct(janet_struct_end(st));
}

JANET_FN(cfun_reader_is_directory,
        "(zip/file-directory? reader idx)",
        "Check if a file index is a directory.") {
    janet_fixarity(argc, 2);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    int32_t index = janet_getinteger(argv, 1);
    return janet_wrap_boolean(mz_zip_reader_is_file_a_directory(archive, index));
}

JANET_FN(cfun_reader_is_supported,
        "(zip/file-supported? reader idx)",
        "Check if a file is supported with this verstion of miniz.") {
    janet_fixarity(argc, 2);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    int32_t index = janet_getinteger(argv, 1);
    return janet_wrap_boolean(mz_zip_reader_is_file_supported(archive, index));
}

JANET_FN(cfun_reader_is_encrypted,
        "(zip/file-encrypted? reader idx)",
        "Check if a file is encrypted inside an archive.") {
    janet_fixarity(argc, 2);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    int32_t index = janet_getinteger(argv, 1);
    return janet_wrap_boolean(mz_zip_reader_is_file_encrypted(archive, index));
}

JANET_FN(cfun_reader_extract,
        "(zip/extract reader idx-or-filename &opt into flags)",
        "Extract a file from the zip archive, either to memory or to a file on disk.") {
    janet_arity(argc, 2, 4);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_reader_type);
    int is_filename = janet_checktype(argv[1], JANET_STRING);
    int32_t index = 0;
    const char *filename = NULL;
    int32_t flags = miniz_optflags(argv, argc, 3);
    if (is_filename) {
        filename = janet_getcstring(argv, 1);
        index = mz_zip_reader_locate_file(archive, filename, NULL, flags);
    } else {
        index = janet_getinteger(argv, 1);
    }
    mz_zip_archive_file_stat statdata;
    mz_zip_reader_file_stat(archive, index, &statdata);
    if (argc >= 3 && janet_checktype(argv[2], JANET_ABSTRACT)) {
        int32_t fflags;
        int32_t needed_flags = JANET_FILE_WRITE | JANET_FILE_BINARY;
        FILE *file = janet_getfile(argv, 2, &fflags);
        if ((fflags & needed_flags) != needed_flags) {
            janet_panicf("file must be opened in binary mode and writable");
        }
        if (is_filename) {
            mz_zip_reader_extract_file_to_cfile(archive, filename, file, flags);
        } else {
            mz_zip_reader_extract_to_cfile(archive, index, file, flags);
        }
        return argv[2];
    }
    if (argc >= 3 && janet_checktype(argv[2], JANET_STRING)) {
        const char *output_path = janet_getcstring(argv, 2);
        if (is_filename) {
            mz_zip_reader_extract_file_to_file(archive, filename, output_path, flags);
        } else {
            mz_zip_reader_extract_to_file(archive, index, output_path, flags);
        }
        return argv[2];
    }
    JanetBuffer *into = janet_optbuffer(argv, argc, 2, 0);
    janet_buffer_extra(into, statdata.m_uncomp_size);
    if (is_filename) {
        mz_zip_reader_extract_file_to_mem(archive, filename, into->data + into->count, statdata.m_uncomp_size, flags);
    } else {
        mz_zip_reader_extract_to_mem(archive, index, into->data + into->count, statdata.m_uncomp_size, flags);
    }
    into->count += statdata.m_uncomp_size;
    return janet_wrap_buffer(into);
}

/* Zip writing */

static int writer_gc(void *p, size_t size) {
    (void) size;
    mz_zip_archive *archive = p;
    mz_zip_writer_end(archive);
    return 0;
}

static const JanetAbstractType zip_writer_type = {
    .name = "zip/writer",
    .gc = writer_gc
};

JANET_FN(cfun_write_file,
        "(zip/write-file dest-path)",
        "Create a new zip archive writer that will write into an archive file.") {
    janet_fixarity(argc, 1);
    JanetString path = janet_getstring(argv, 0);
    mz_zip_archive *archive = janet_abstract(&zip_writer_type, sizeof(mz_zip_archive));
    mz_zip_zero_struct(archive);
    mz_zip_writer_init_file_v2(archive, (const char *)path, 0, 0);
    return janet_wrap_abstract(archive);
}

JANET_FN(cfun_write_buffer,
        "(zip/write-buffer)",
        "Create a new zip archive writer that write to memory.") {
    janet_fixarity(argc, 0);
    mz_zip_archive *archive = janet_abstract(&zip_writer_type, sizeof(mz_zip_archive));
    mz_zip_zero_struct(archive);
    mz_zip_writer_init_heap(archive, 0, 32 * 1024);
    return janet_wrap_abstract(archive);
}

JANET_FN(cfun_writer_add_file,
        "(zip/add-file writer path filename &opt comment flags)",
        "Add a file to the zip writer.") {
    janet_arity(argc, 3, 5);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_writer_type);
    const char *path = janet_getcstring(argv, 1);
    const char *filename = janet_getcstring(argv, 2);
    const char *comment = janet_optcstring(argv, argc, 3, NULL);
    mz_uint flags = miniz_optflags(argv, argc, 4);
    if (!mz_zip_writer_add_file(archive, path, filename, comment, (comment != NULL) ? strlen(comment) : 0, flags)) {
        janet_panic("adding file failed!");
    }
    return argv[0];
}

JANET_FN(cfun_writer_add_bytes,
        "(zip/add-bytes writer path data &opt comment flags)",
        "Add a byte sequence to the zip writer.") {
    janet_arity(argc, 3, 5);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_writer_type);
    const char *path = janet_getcstring(argv, 1);
    JanetByteView bytes = janet_getbytes(argv, 2);
    const char *comment = janet_optcstring(argv, argc, 3, NULL);
    mz_uint flags = miniz_optflags(argv, argc, 4);
    // TODO - handle pre-compressed data better
    if (!mz_zip_writer_add_mem_ex(archive, path, bytes.bytes, bytes.len, comment, (comment != NULL) ? strlen(comment) : 0, flags, 0, 0)) {
        janet_panic("adding bytes failed!");
    }
    return argv[0];
}

JANET_FN(cfun_writer_close,
        "(zip/writer-close writer)",
        "Close a ZipWriter.") {
    janet_fixarity(argc, 1);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_writer_type);
    mz_zip_writer_finalize_archive(archive);
    mz_zip_writer_end(archive);
    return janet_wrap_nil();
}

JANET_FN(cfun_writer_finalize,
        "(zip/writer-finalizer writer)",
        "Finalize a writer, writing any zip files to disk. Return the writer.") {
    janet_fixarity(argc, 1);
    mz_zip_archive *archive = janet_getabstract(argv, 0, &zip_writer_type);
    if (archive->m_zip_type == MZ_ZIP_TYPE_HEAP) {
        void *buf = NULL;
        size_t len = 0;
        mz_zip_writer_finalize_heap_archive(archive, &buf, &len);
        if (len > INT32_MAX) {
            free(buf);
            janet_panic("zip archive too large for janet buffer");
        }
        JanetBuffer *buffer = janet_buffer(0);
        if (buffer->data != NULL) {
            free(buffer->data);
        }
        buffer->data = buf;
        buffer->count = (int32_t) len;
        janet_gcpressure(len);
        return janet_wrap_buffer(buffer);
    } else {
        mz_zip_writer_finalize_archive(archive);
        return argv[0];
    }
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
        JANET_REG("read-file", cfun_read_file),
        JANET_REG("read-bytes", cfun_read_bytes),
        JANET_REG("stat", cfun_reader_stat),
        JANET_REG("reader-close", cfun_reader_close),
        JANET_REG("reader-count", cfun_reader_count),
        JANET_REG("extract", cfun_reader_extract),
        JANET_REG("get-filename", cfun_reader_idx_to_path),
        JANET_REG("locate-file", cfun_reader_locate),
        JANET_REG("file-directory?", cfun_reader_is_directory),
        JANET_REG("file-supported?", cfun_reader_is_supported),
        JANET_REG("file-encrypted?", cfun_reader_is_encrypted),
        JANET_REG("write-file", cfun_write_file),
        JANET_REG("write-buffer", cfun_write_buffer),
        JANET_REG("add-file", cfun_writer_add_file),
        JANET_REG("add-bytes", cfun_writer_add_bytes),
        JANET_REG("writer-close", cfun_writer_close),
        JANET_REG("writer-finalize", cfun_writer_finalize),
        JANET_REG("version", cfun_version),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "zip", cfuns);
}

