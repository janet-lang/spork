/*
* Copyright (c) 2021 Calvin Rose
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

#ifndef JANET_TYPED_ARRAYS_H_defined
#define JANET_TYPED_ARRAYS_H_defined

#include <janet.h>

#ifdef __cplusplus
extern "C" {
#endif

extern JANET_API const JanetAbstractType janet_ta_view_type;
extern JANET_API const JanetAbstractType janet_ta_buffer_type;

typedef enum {
    JANET_TARRAY_TYPE_U8,
    JANET_TARRAY_TYPE_S8,
    JANET_TARRAY_TYPE_U16,
    JANET_TARRAY_TYPE_S16,
    JANET_TARRAY_TYPE_U32,
    JANET_TARRAY_TYPE_S32,
    JANET_TARRAY_TYPE_U64,
    JANET_TARRAY_TYPE_S64,
    JANET_TARRAY_TYPE_F32,
    JANET_TARRAY_TYPE_F64
} JanetTArrayType;

typedef struct {
    uint8_t *data;
    size_t size;
    int32_t flags;
} JanetTArrayBuffer;

typedef struct {
    union {
        void *pointer;
        uint8_t *u8;
        int8_t *s8;
        uint16_t *u16;
        int16_t *s16;
        uint32_t *u32;
        int32_t *s32;
        uint64_t *u64;
        int64_t *s64;
        float *f32;
        double *f64;
    } as;
    JanetTArrayBuffer *buffer;
    size_t size;
    size_t stride;
    JanetTArrayType type;
} JanetTArrayView;

JANET_API JanetTArrayBuffer *janet_tarray_buffer(size_t size);
JANET_API JanetTArrayView *janet_tarray_view(JanetTArrayType type, size_t size, size_t stride, size_t offset, JanetTArrayBuffer *buffer);
JANET_API int janet_is_tarray_view(Janet x, JanetTArrayType type);
JANET_API JanetTArrayBuffer *janet_gettarray_buffer(const Janet *argv, int32_t n);
JANET_API JanetTArrayView *janet_gettarray_view(const Janet *argv, int32_t n, JanetTArrayType type);
JanetTArrayView *janet_gettarray_any(const Janet *argv, int32_t n);

#ifdef __cplusplus
}
#endif

#endif /* JANET_TYPED_ARRAYS_H_defined */
