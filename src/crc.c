/*
 * Generate CRC variants. Rather than compile separate variants,
 * we have code to generate the needed tables. Keeps build simple,
 * footprint small but with many variants accesible.
 */

#include <stdbool.h>
#include <janet.h>
#include <stdlib.h>
#include <stdio.h>

static const uint8_t nibble_reverse_lut[16] = {
    0x0, 0x8, 0x4, 0xC, 0x2, 0xA, 0x6, 0xE,
    0x1, 0x9, 0x5, 0xD, 0x3, 0xB, 0x7, 0xF
};

static uint8_t byte_reverse(uint8_t b) {
    return nibble_reverse_lut[b >> 4] | (nibble_reverse_lut[b & 0xF] << 4);
}

static uint16_t word_reverse(uint16_t w) {
    return (uint16_t) byte_reverse((uint8_t)(w >> 8)) |
        ((uint16_t) byte_reverse((uint8_t)(w & 0xFF)) << 8);
}

static uint32_t dword_reverse(uint32_t d) {
    return (uint32_t) word_reverse((uint16_t)(d >> 16)) |
        ((uint32_t) word_reverse((uint16_t)(d & 0xFFFF)) << 16);
}

/**********/
/* CRC  8 */
/**********/

typedef struct {
    uint8_t init;
    uint8_t xor;
    uint8_t lut[256];
} CRC8;

static uint8_t crc8_byte(uint8_t polynomial, uint8_t data) {
    uint8_t rem = data;
    for (int b = 8; b > 0; b--) {
        rem = (rem & 0x80)
            ? (rem << 1) ^ polynomial
            : (rem << 1);
    }
    return rem;
}

static void crc8_make_variant(CRC8 *gen, uint8_t init, uint8_t polynomial, bool byte_flip, uint8_t xor) {
    gen->init = init;
    gen->xor = xor;
    for (int i = 0; i < 256; i++) {
        uint8_t ui = (uint8_t) i;
        if (byte_flip) {
            gen->lut[i] = byte_reverse(crc8_byte(polynomial, byte_reverse(ui)));
        } else {
            gen->lut[i] = crc8_byte(polynomial, ui);
        }
    }
}

static uint8_t crc8_general(CRC8 *variant, const uint8_t *bytes, size_t len, uint8_t sum) {
    for (size_t i = 0; i < len; i++) sum = variant->lut[bytes[i] ^ sum];
    return sum ^ variant->xor;
}

/**********/
/* CRC 16 */
/**********/

typedef struct {
    uint16_t init;
    uint16_t xor;
    bool flipped;
    uint16_t lut[256];
} CRC16;

static uint16_t crc16_byte(uint16_t polynomial, uint8_t data) {
    uint16_t rem = (uint16_t) data;
    for (int b = 16; b > 0; b--) {
        rem = (rem & 0x8000)
            ? (rem << 1) ^ polynomial
            : (rem << 1);
    }
    return rem;
}

static void crc16_make_variant(CRC16 *gen, uint16_t init, uint16_t polynomial, bool byte_flip, uint16_t xor) {
    gen->init = init;
    gen->xor = xor;
    gen->flipped = byte_flip;
    for (int i = 0; i < 256; i++) {
        uint8_t ui = (uint8_t) i;
        uint16_t wi = (uint16_t) i;
        if (byte_flip) {
            gen->lut[i] = word_reverse(crc16_byte(polynomial, byte_reverse(ui)));
        } else {
            gen->lut[i] = crc16_byte(polynomial, ui);
        }
    }
}

static uint16_t crc16_general(CRC16 *variant, const uint8_t *bytes, size_t len, uint16_t sum) {
    if (variant->flipped) {
        for (size_t i = 0; i < len; i++) {
            sum = (sum >> 8) ^ variant->lut[(sum & 0xFF) ^ bytes[i]];
        }
    } else {
        for (size_t i = 0; i < len; i++) {
            sum = (sum << 8) ^ variant->lut[(sum >> 8) ^ bytes[i]];
        }
    }
    return sum ^ variant->xor;
}

/**********/
/* CRC 32 */
/**********/

typedef struct {
    uint32_t init;
    uint32_t xor;
    bool flipped;
    uint32_t lut[256];
} CRC32;

static uint32_t crc32_byte(uint32_t polynomial, uint8_t data) {
    uint32_t rem = (uint32_t) data;
    for (int b = 32; b > 0; b--) {
        rem = (rem & 0x80000000U)
            ? (rem << 1) ^ polynomial
            : (rem << 1);
    }
    return rem;
}

static void crc32_make_variant(CRC32 *gen, uint32_t init, uint32_t polynomial, bool byte_flip, uint32_t xor) {
    gen->init = init;
    gen->xor = xor;
    gen->flipped = byte_flip;
    for (int i = 0; i < 256; i++) {
        uint8_t ui = (uint8_t) i;
        uint32_t di = (uint32_t) i;
        if (byte_flip) {
            gen->lut[i] = dword_reverse(crc32_byte(polynomial, byte_reverse(ui)));
        } else {
            gen->lut[i] = crc32_byte(polynomial, ui);
        }
    }
}

static uint32_t crc32_general(CRC32 *variant, const uint8_t *bytes, size_t len, uint32_t sum) {
    if (variant->flipped) {
        for (size_t i = 0; i < len; i++) {
            sum = (sum >> 8) ^ variant->lut[(sum & 0xFF) ^ bytes[i]];
        }
    } else {
        for (size_t i = 0; i < len; i++) {
            sum = (sum << 8) ^ variant->lut[(sum >> 24) ^ bytes[i]];
        }
    }
    return sum ^ variant->xor;
}

/***************/
/* C Functions */
/***************/

static JanetRange crc_getslice(int32_t argc, const Janet *argv) {
    janet_arity(argc, 1, 4);
    JanetRange range;
    int32_t length = janet_length(argv[0]);
    if (argc == 1) {
        range.start = 0;
        range.end = length;
    } else if (argc == 2) {
        range.start = janet_checktype(argv[1], JANET_NIL)
                      ? 0
                      : janet_gethalfrange(argv, 1, length, "start");
        range.end = length;
    } else {
        range.start = janet_checktype(argv[1], JANET_NIL)
                      ? 0
                      : janet_gethalfrange(argv, 1, length, "start");
        range.end = janet_checktype(argv[2], JANET_NIL)
                    ? length
                    : janet_gethalfrange(argv, 2, length, "end");
        if (range.end < range.start)
            range.end = range.start;
    }
    return range;
}

static Janet crc8_call(void *variant, int32_t argc, Janet *argv) {
    CRC8 *crc8 = (CRC8 *)variant;
    JanetRange range = crc_getslice(argc, argv);
    JanetByteView bytes = janet_getbytes(argv, 0);
    uint8_t init = janet_optnat(argv, argc, 3, crc8->init) & 0xFF;
    return janet_wrap_integer(crc8_general(crc8,
                bytes.bytes + range.start,
                range.end - range.start, init));
}

static Janet crc16_call(void *variant, int32_t argc, Janet *argv) {
    CRC16 *crc16 = (CRC16 *)variant;
    JanetRange range = crc_getslice(argc, argv);
    JanetByteView bytes = janet_getbytes(argv, 0);
    uint16_t init = janet_optnat(argv, argc, 3, crc16->init) & 0xFFFF;
    return janet_wrap_integer(crc16_general(crc16,
                bytes.bytes + range.start,
                range.end - range.start, init));
}

static Janet crc32_call(void *variant, int32_t argc, Janet *argv) {
    CRC32 *crc32 = (CRC32 *)variant;
    JanetRange range = crc_getslice(argc, argv);
    JanetByteView bytes = janet_getbytes(argv, 0);
    uint32_t init = janet_optnat(argv, argc, 3, crc32->init);
    return janet_wrap_number(crc32_general(crc32,
                bytes.bytes + range.start,
                range.end - range.start, init));
}

static const JanetAbstractType CRC8_AT = {
    .name = "crc/crc8-variant",
    .call = crc8_call
};

static const JanetAbstractType CRC16_AT = {
    .name = "crc/crc16-variant",
    .call = crc16_call
};

static const JanetAbstractType CRC32_AT = {
    .name = "crc/crc32-variant",
    .call = crc32_call
};

typedef struct {
    const char *name;
    size_t size;
    uint32_t poly;
    uint32_t init;
    bool flipped;
    uint32_t xor;
} NamedVariant;

// Variant list from https://crccalc.com/
static const NamedVariant named_variants[] = {
    // CRC8 Variants
    {"crc8", 8, 0x07, 0x00, false, 0x00},
    {"crc8/cdma2000", 8, 0x9B, 0xFF, false, 0x00},
    {"crc8/darc", 8, 0x39, 0x00, true, 0x00},
    {"crc8/dvb-s2", 8, 0xD5, 0x00, false, 0x00},
    {"crc8/ebu", 8, 0x1D, 0xFF, true, 0x00},
    {"crc8/icode", 8, 0x1D, 0xFD, false, 0x00},
    {"crc8/itu", 8, 0x07, 0x00, false, 0x55},
    {"crc8/maxim", 8, 0x31, 0x00, true, 0x00},
    {"crc8/rohc", 8, 0x07, 0xFF, true, 0x00},
    {"crc8/wcdma", 8, 0x9B, 0x00, true, 0x00},
    // CRC16 Variants
    {"crc16/ccitt-false", 16, 0x1021, 0xFFFF, false, 0x0000},
    {"crc16/arc", 16, 0x8005, 0x0000, true, 0x0000},
    {"crc16/aug-ccitt", 16, 0x1021, 0x1D0F, false, 0x0000},
    {"crc16/buypass", 16, 0x8005, 0x0000, false, 0x0000},
    {"crc16/cdma2000", 16, 0xC867, 0xFFFF, false, 0x0000},
    {"crc16/dds-110", 16, 0x8005, 0x800D, false, 0x0000},
    {"crc16/dect-r", 16, 0x0589, 0x0000, false, 0x0001},
    {"crc16/dect-x", 16, 0x0589, 0x0000, false, 0x0000},
    {"crc16/dnp", 16, 0x3D65, 0x0000, true, 0xFFFF},
    {"crc16/en-13757", 16, 0x3D65, 0x0000, false, 0xFFFF},
    {"crc16/genibus", 16, 0x1021, 0xFFFF, false, 0xFFFF},
    {"crc16/maxim", 16, 0x8005, 0x0000, true, 0xFFFF},
    {"crc16/mcrf4xx", 16, 0x1021, 0xFFFF, true, 0x0000},
    {"crc16/riello", 16, 0x1021, 0xB2AA, true, 0x0000},
    {"crc16/t10-dif", 16, 0x8BB7, 0x0000, false, 0x0000},
    {"crc16/teledisk", 16, 0xA097, 0x0000, false, 0x0000},
    {"crc16/tms37157", 16, 0x1021, 0x89EC, true, 0x0000},
    {"crc16/usb", 16, 0x8005, 0xFFFF, true, 0xFFFF},
    {"crc-a", 16, 0x1021, 0xC6C6, true, 0x0000},
    {"crc16/kermit", 16, 0x1021, 0x0000, true, 0x0000},
    {"crc16/modbus", 16, 0x8005, 0xFFFF, true, 0x0000},
    {"crc16/x-25", 16, 0x1021, 0xFFFF, true, 0xFFFF},
    {"crc16/xmodem", 16, 0x1021, 0x0000, false, 0xFFFF},
    // CRC32 Variants
    {"crc32", 32, 0x04C11DB7, 0xFFFFFFFF, true, 0xFFFFFFFF},
    {"crc32/bzip2", 32, 0x04C11DB7, 0xFFFFFFFF, false, 0xFFFFFFFF},
    {"crc32c", 32, 0x1EDC6F41, 0xFFFFFFFF, true, 0xFFFFFFFF},
    {"crc32d", 32, 0xA833982B, 0xFFFFFFFF, true, 0xFFFFFFFF},
    {"crc32/jamcrc", 32, 0x04C11DB7, 0xFFFFFFFF, true, 0x00000000},
    {"crc32/mpeg-2", 32, 0x04C11DB7, 0xFFFFFFFF, false, 0x00000000},
    {"crc32/posix", 32, 0x04C11DB7, 0x00000000, false, 0xFFFFFFFF},
    {"crc32q", 32, 0x814141AB, 0x00000000, false, 0x00000000},
    {"crc32/xfer", 32, 0x000000AF, 0x00000000, false, 0x00000000},
};

JANET_FN(cfun_named_variant,
        "(crc/named-variant name)",
        "Get a named CRC variant.") {
    janet_fixarity(argc, 1);
    JanetKeyword kw = janet_getkeyword(argv, 0);
    const NamedVariant *nv = &named_variants[0];
    const NamedVariant *last_v = (NamedVariant *)((const char *)nv + sizeof(named_variants));
    while (nv < last_v) {
        if (0 == strcmp(nv->name, (const char *)kw)) break;
        nv++;
    }
    if (nv > last_v) janet_panicf("unknown variant %v", argv[0]);
    void *ret;
    if (nv->size == 8) {
        ret = janet_abstract(&CRC8_AT, sizeof(CRC8));
        crc8_make_variant(ret, (uint8_t) nv->init, (uint8_t) nv->poly, nv->flipped, (uint8_t) nv->xor);
    } else if (nv->size == 16) {
        ret = janet_abstract(&CRC16_AT, sizeof(CRC16));
        crc16_make_variant(ret, (uint16_t) nv->init, (uint16_t) nv->poly, nv->flipped, (uint16_t) nv->xor);
    } else if (nv->size == 32) {
        ret = janet_abstract(&CRC32_AT, sizeof(CRC32));
        crc32_make_variant(ret, (uint32_t) nv->init, (uint32_t) nv->poly, nv->flipped, (uint32_t) nv->xor);
    } else {
        janet_panic("nyi");
    }
    return janet_wrap_abstract(ret);
}

JANET_FN(cfun_make_variant,
        "(crc/make-variant size polynomial &opt init byte-flip xorout)",
        "Create a CRC function based on the given polynomial, initial value, xourout, "
        "and whether to invert input bytes.") {
    janet_arity(argc, 2, 5);
    int32_t size = janet_getnat(argv, 0);
    uint64_t poly = janet_getuinteger64(argv, 1);
    if (size != 8 && size != 16 && size != 32) {
        janet_panicf("bad size, expected 8, 16, or 32, got %v", argv[0]);
    }
    if (poly >= ((uint64_t)1 << size)) {
        janet_panicf("polynomial too large for given CRC size of %d", size);
    }
    uint64_t init = janet_optinteger64(argv, argc, 2, 0);
    if (init >= ((uint64_t)1 << size)) {
        janet_panicf("initial crc too large for given CRC size of %d", size);
    }
    int byte_flip = argc > 3 && janet_truthy(argv[3]);
    uint64_t xorout = janet_optinteger64(argv, argc, 4, 0);
    if (xorout >= ((uint64_t)1 << size)) {
        janet_panicf("xorout too large for given CRC size of %d", size);
    }
    
    /* Create table */
    void *ret = NULL;
    if (size == 8) {
        ret = janet_abstract(&CRC8_AT, sizeof(CRC8));
        crc8_make_variant(ret, (uint8_t) init, (uint8_t) poly, byte_flip, (uint8_t) xorout);
    } else if (size == 16) {
        ret = janet_abstract(&CRC16_AT, sizeof(CRC16));
        crc16_make_variant(ret, (uint16_t) init, (uint16_t) poly, byte_flip, (uint16_t) xorout);
    } else {
        ret = janet_abstract(&CRC32_AT, sizeof(CRC32));
        crc32_make_variant(ret, (uint32_t) init, (uint32_t) poly, byte_flip, (uint32_t) xorout);
    }

    return janet_wrap_abstract(ret);
}

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("make-variant", cfun_make_variant),
        JANET_REG("named-variant", cfun_named_variant),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "crc", cfuns);
}

