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

#ifdef __linox__
#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif

#include <janet.h>

JANET_FN(cfun_getline_rune_monowidth,
        "(_getline/rune-monowidth rune)",
        "Get the monospace width of a rune. Returns either 0, 1, or 2.") {
    janet_fixarity(argc, 1);
    uint32_t rune = (uint32_t)janet_getinteger(argv, 0);

    static const struct {
        uint32_t start;
        int32_t width;
        uint64_t end_or_mask;
    } k_width_classes[] = {
        /* AUTO-GENERATED BY tools/wchar_proclist.janet */
        {      0,  0,                  0    },
        {      1, -1,                 31    },
        {    127, -1,                159    },
        {    768,  0,                879    },
        {   1155,  0,               1161    },
        {   1425,  0, 0xfffffffffffd6d00ULL },
        {   1536,  0, 0xfe007ff400000000ULL },
        {   1611,  0, 0xfffffc0002000000ULL },
        {   1750,  0, 0xffbf378000000028ULL },
        {   1840,  0,               1866    },
        {   1958,  0,               1968    },
        {   2027,  0, 0xffc01000000f7fdcULL },
        {   2089,  0, 0xfc00000000007000ULL },
        {   2259,  0,               2306    },
        {   2362,  0, 0xd0ff08fe00600000ULL },
        {   2433,  0, 0xc000000000000008ULL },
        {   2497,  0, 0xf804000030000002ULL },
        {   2561,  0, 0xe000000000000008ULL },
        {   2625,  0, 0xe19c40000000c400ULL },
        {   2689,  0, 0xe000000000000008ULL },
        {   2753,  0, 0xfd8400003000003fULL },
        {   2817,  0, 0xc000000000000009ULL },
        {   2881,  0, 0xf804020030000000ULL },
        {   2946,  0, 0xc000000000000001ULL },
        {   3021,  0, 0xc000000000000880ULL },
        {   3134,  0, 0xf07780c006000000ULL },
        {   3201,  0, 0xc000000000000009ULL },
        {   3270,  0, 0xc180000600000018ULL },
        {   3387,  0, 0xe1e0100000c00000ULL },
        {   3530,  0, 0xc074000000000000ULL },
        {   3633,  0, 0xcfe001fe00000000ULL },
        {   3761,  0, 0xcff800fc00000000ULL },
        {   3864,  0, 0xe0000002a0000000ULL },
        {   3953,  0, 0xfffefb07ff000000ULL },
        {   3993,  0, 0xfffffffff8020000ULL },
        {   4141,  0, 0xfbf66000000c3800ULL },
        {   4209,  0, 0xf800260400040000ULL },
        {   4352,  2,               4447    },
        {   4448,  0,               4607    },
        {   4957,  0,               4959    },
        {   5906,  0, 0xf000000070000000ULL },
        {   5970,  0, 0xe000000060000000ULL },
        {   6068,  0, 0xefe013ff80200000ULL },
        {   6155,  0,               6158    },
        {   6277,  0, 0xe000000004000000ULL },
        {   6432,  0, 0xf0c0103800000000ULL },
        {   6679,  0, 0xe400000000000000ULL },
        {   6742,  0, 0xdfd4ff03ff200000ULL },
        {   6787,  2, 0xc6643ed5d77c0000ULL },
        {   6832,  0,               6846    },
        {   6848,  2, 0xc0ef800000000000ULL },
        {   6912,  0, 0xf8000000000005f4ULL },
        {   6978,  0, 0xc0000000003fe000ULL },
        {   7040,  0, 0xe00000001e6e0000ULL },
        {   7142,  0, 0xd8b8000000000000ULL },
        {   7212,  0, 0xff98000000000000ULL },
        {   7376,  0, 0xf7ffdfc204600000ULL },
        {   7616,  0,               7673    },
        {   7675,  0,               7679    },
        {   8203,  0, 0xfc000000f8000000ULL },
        {   8288,  0, 0xfdff800000000000ULL },
        {   8400,  0,               8432    },
        {   8986,  2, 0xe000c00000000000ULL },
        {   9193,  2, 0xf890000000000000ULL },
        {   9725,  2, 0xe00000c000000000ULL },
        {   9800,  2, 0xfff8000000000080ULL },
        {   9875,  2, 0xc00100c000183008ULL },
        {   9940,  2, 0xc0000101a1202180ULL },
        {  10024,  2, 0xc0000000050e8000ULL },
        {  10133,  2, 0xf000000800100000ULL },
        {  11035,  2, 0xe000000000000210ULL },
        {  11503,  0,              11505    },
        {  11647,  0,              11647    },
        {  11744,  0,              11775    },
        {  11904,  2,              11929    },
        {  11931,  2,              12019    },
        {  12032,  2,              12245    },
        {  12272,  2, 0xfff87fffffffffe0ULL },
        {  12330,  0,              12333    },
        {  12334,  2,              12350    },
        {  12353,  2,              12438    },
        {  12441,  0,              12442    },
        {  12443,  2,              12543    },
        {  12549,  2,              12591    },
        {  12593,  2,              12686    },
        {  12688,  2,              12730    },
        {  12736,  2,              12771    },
        {  12784,  2,              12830    },
        {  12832,  2,              12871    },
        {  12880,  2,              19903    },
        {  19968,  2,              42124    },
        {  42128,  2,              42182    },
        {  42607,  0, 0xfbff00000000c000ULL },
        {  42736,  0,              42737    },
        {  43010,  0, 0xc42000000c000000ULL },
        {  43204,  0, 0xe0000007fffe0008ULL },
        {  43302,  0, 0xff8000003ff80000ULL },
        {  43360,  2,              43388    },
        {  43392,  0, 0xf0000000000009e6ULL },
        {  43493,  0,              43493    },
        {  43561,  0, 0xfe66001008000000ULL },
        {  43644,  0, 0xc0000000000005ccULL },
        {  43710,  0, 0xe800000000018040ULL },
        {  44005,  0, 0xc840000000000000ULL },
        {  44032,  2,              55203    },
        {  61443,  2, 0xc6643ed5d77f5002ULL },
        {  61508,  2, 0xd838000000000000ULL },
        {  63744,  2,              64255    },
        {  64286,  0,              64286    },
        {  65024,  0,              65039    },
        {  65040,  2,              65049    },
        {  65056,  0,              65071    },
        {  65072,  2, 0xfffffffff7ffff78ULL },
        {  65279,  0,              65279    },
        {  65281,  2,              65376    },
        {  65504,  2,              65510    },
        {  65529,  0,              65531    },
        {  66045,  0,              66045    },
        {  66272,  0,              66272    },
        {  66422,  0,              66426    },
        {  68097,  0, 0xf60f0000000000e1ULL },
        {  68325,  0,              68326    },
        {  68900,  0,              68903    },
        {  69446,  0,              69456    },
        {  69633,  0,              69633    },
        {  69688,  0,              69702    },
        {  69759,  0, 0xf000000000000799ULL },
        {  69837,  0, 0xc000000000000e00ULL },
        {  69927,  0, 0xfdfe000000000000ULL },
        {  70003,  0, 0xc003000000000000ULL },
        {  70070,  0, 0xffc00f0000000000ULL },
        {  70191,  0, 0xf2c0800000000000ULL },
        {  70367,  0, 0xc7f8000030000000ULL },
        {  70459,  0, 0xe2000000000fe3e0ULL },
        {  70712,  0, 0xff9d000001000000ULL },
        {  70835,  0, 0xfe86c00000000000ULL },
        {  71090,  0, 0xf81b000000180000ULL },
        {  71219,  0, 0xff96000000000000ULL },
        {  71339,  0, 0xd3f4000000000000ULL },
        {  71453,  0, 0xf3df000000000000ULL },
        {  71727,  0, 0xffd8000000000000ULL },
        {  72148,  0, 0xf98400000003ff00ULL },
        {  72243,  0, 0xfe780401f9c00000ULL },
        {  72330,  0, 0xfffd800000000000ULL },
        {  72752,  0, 0xff7e800000000000ULL },
        {  72850,  0, 0xfffffe7f6c000000ULL },
        {  73009,  0, 0xfe2dfd0000000000ULL },
        {  73104,  0, 0xe280000000000000ULL },
        {  73459,  0,              73460    },
        {  78896,  0,              78904    },
        {  92912,  0,              92916    },
        {  92976,  0,              92982    },
        {  94031,  0,              94031    },
        {  94095,  0,              94098    },
        {  94176,  2,              94179    },
        {  94208,  2,             100343    },
        { 100352,  2,             101106    },
        { 110592,  2,             110878    },
        { 110928,  2, 0xf000078000000000ULL },
        { 110960,  2,             111355    },
        { 113821,  0, 0xef00000000000000ULL },
        { 119143,  0, 0xf007fff9fc000000ULL },
        { 119210,  0,             119213    },
        { 119362,  0,             119364    },
        { 121344,  0,             121398    },
        { 121403,  0, 0xffffffffffffe010ULL },
        { 121476,  0, 0xc00000fbfff80000ULL },
        { 122880,  0, 0xff7fffcfedf00000ULL },
        { 123184,  0,             123190    },
        { 123628,  0,             123631    },
        { 125136,  0,             125142    },
        { 125252,  0,             125258    },
        { 126980,  2,             126980    },
        { 127183,  2,             127183    },
        { 127374,  2, 0xcffc000000000000ULL },
        { 127488,  2, 0xf0007ffffffffff8ULL },
        { 127552,  2, 0xffc060007e000000ULL },
        { 127744,  2, 0xffffffffc003fe00ULL },
        { 127799,  2,             127868    },
        { 127870,  2,             127891    },
        { 127904,  2, 0xfffffffffff0f800ULL },
        { 127968,  2, 0xffffc40000000000ULL },
        { 127992,  2,             128062    },
        { 128064,  2,             128064    },
        { 128066,  2,             128252    },
        { 128255,  2,             128317    },
        { 128331,  2, 0xfbfffffc00008000ULL },
        { 128405,  2, 0xe000800000000000ULL },
        { 128507,  2,             128591    },
        { 128640,  2,             128709    },
        { 128716,  2, 0xc7200000c07f0000ULL },
        { 128992,  2,             129003    },
        { 129293,  2,             129393    },
        { 129395,  2, 0xf8ffffffffff9f80ULL },
        { 129454,  2,             129482    },
        { 129485,  2,             129535    },
        { 129648,  2, 0xf87070007e000000ULL },
        { 131072,  2,             196605    },
        { 196608,  2,             262141    },
        { 917505,  0,             917505    },
        { 917536,  0,             917631    },
    };

    int width;
    {
        int end = sizeof(k_width_classes) / sizeof(k_width_classes[0]);
        int l = 0, r = end;
        int i;
        while (l < (r - 1)) {
            i = (l + r) / 2;
            if (k_width_classes[i].start > rune) {
                r = i;
            } else {
                l = i;
            }
        }
        i = l;
        if (l >= end || rune < k_width_classes[i].start) {
            width = 1;
        } else if (k_width_classes[i].end_or_mask & (1ULL << 63)) {
            uint32_t diff = rune - k_width_classes[i].start;
            if (diff >= 62) {
                width = 1;
            } else {
                uint64_t mask = k_width_classes[i].end_or_mask;
                uint64_t bit = 1ULL << (62 - diff);
                width = (mask & bit) ? k_width_classes[i].width : 1;
            }
        } else {
            width = (rune <= k_width_classes[i].end_or_mask) ? k_width_classes[i].width : 1;
        }
    }

    return janet_wrap_integer(width);
}

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("rune-monowidth", cfun_getline_rune_monowidth),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "_getline", cfuns);
}