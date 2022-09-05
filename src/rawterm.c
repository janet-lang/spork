/*
* Copyright (c) 2022 Calvin Rose
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

#ifdef __linux__
#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif

#include <janet.h>
#include <stdlib.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

/* Common Implementation */

static JANET_THREAD_LOCAL bool in_raw_mode;
static JANET_THREAD_LOCAL JanetFunction *rawterm_winch_handler;
static JANET_THREAD_LOCAL bool at_exit_set = false;

/* Per-Platform Implementation */

#ifdef JANET_WINDOWS

#include <windows.h>

static void rawterm_at_exit(void);

static int check_simpleline(JanetBuffer *buffer) {
    return 0;
}

static void setup_console_output(void) {
    /* Enable color console on windows 10 console and utf8 output and other processing */
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD dwMode = 0;
    GetConsoleMode(hOut, &dwMode);
    dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    SetConsoleMode(hOut, dwMode);
    SetConsoleOutputCP(65001);
}

static int rawterm_begin(void) {
    if (in_raw_mode) return 0;
    if (!_isatty(_fileno(stdin))) {
        janet_panic("not a tty");
    }
    setup_console_output();
    HANDLE hOut = GetStdHandle(STD_INPUT_HANDLE);
    DWORD dwMode = 0;
    GetConsoleMode(hOut, &dwMode);
    dwMode &= ~ENABLE_LINE_INPUT;
    dwMode &= ~ENABLE_INSERT_MODE;
    dwMode &= ~ENABLE_ECHO_INPUT;
    dwMode |= ENABLE_VIRTUAL_TERMINAL_INPUT;
    dwMode &= ~ENABLE_PROCESSED_INPUT;
    if (!SetConsoleMode(hOut, dwMode)) return 1;
    in_raw_mode = 1;

    // one time setup
    if (!at_exit_set) {
        atexit(rawterm_at_exit);
        at_exit_set = true;
    }

    return 0;
}

static void rawterm_getch(JanetBuffer *buffer) {
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    char buf[1];
    DWORD bytes_read = 0;
    if (ReadConsoleA(h, buf, sizeof(buf), &bytes_read, NULL)) {
        if (bytes_read) {
            janet_buffer_push_u8(buffer, buf[0]);
        }
    }
}

static void rawterm_end(void) {
    if (!in_raw_mode) return;
    HANDLE hOut = GetStdHandle(STD_INPUT_HANDLE);
    DWORD dwMode = 0;
    GetConsoleMode(hOut, &dwMode);
    dwMode |= ENABLE_LINE_INPUT;
    dwMode |= ENABLE_INSERT_MODE;
    dwMode |= ENABLE_ECHO_INPUT;
    dwMode &= ~ENABLE_VIRTUAL_TERMINAL_INPUT;
    dwMode |= ENABLE_PROCESSED_INPUT;
    SetConsoleMode(hOut, dwMode);
    in_raw_mode = 0;
}

static void rawterm_at_exit(void) {
    rawterm_end();
}

static void rawterm_size(int *rows, int *cols) {
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
    *cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    *rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
}

#else

#include <termios.h>
#include <stdint.h>
#include <time.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>
#include <signal.h>

/* TODO - thread safe? */

static JANET_THREAD_LOCAL struct termios starting_term;
static JANET_THREAD_LOCAL JanetStream *rawterm_stream = NULL;

static void rawterm_end(void) {
    if (!in_raw_mode) {
        janet_panic("not in raw mode");
    }
    if (tcsetattr(STDIN_FILENO, TCSADRAIN, &starting_term) == -1) {
        janet_panic("could not reset to orignal tty attributes");
    }
    in_raw_mode = false;
}

static void rawterm_at_exit(void) {
    if (in_raw_mode) {
        /* best attempt to reset terminal, if it fails, oh well */
        tcsetattr(STDIN_FILENO, TCSADRAIN, &starting_term);
    }
}

static void rawterm_size(int *rows, int *cols) {
    struct winsize size;
    ioctl(0, TIOCGWINSZ, &size);
    *rows = size.ws_row;
    *cols = size.ws_col;
}

static void rawterm_ev_winch(JanetEVGenericMessage msg) {
    int rows, cols;
    rawterm_size(&rows, &cols);
    Janet tup[2] = {
        janet_wrap_integer(rows),
        janet_wrap_integer(cols)
    };
    JanetFiber *fiber = janet_fiber(rawterm_winch_handler, 64, 2, tup);
    janet_schedule(fiber, janet_wrap_nil());
}

static void rawterm_signal_winch(int x) {
    if (rawterm_winch_handler == NULL) return;
    JanetEVGenericMessage msg;
    memset(&msg, 0, sizeof(msg));
    janet_ev_post_event(NULL, rawterm_ev_winch, msg);
}

static void rawterm_begin(void) {
    if (in_raw_mode) {
        janet_panic("already in raw mode");
    }
    struct termios t;
    if (!isatty(STDIN_FILENO)) {
        janet_panic("input is not a tty");
    }
    if (tcgetattr(STDIN_FILENO, &starting_term) == -1)  {
        janet_panic("cannot get tty attributes");
    }
    t = starting_term;
    t.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    t.c_cflag |= (CS8);
    t.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    t.c_cc[VMIN] = 1;
    t.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSADRAIN, &t) < 0) {
        janet_panic("cannot set tty attributes");
    }
    in_raw_mode = true;

    if (rawterm_stream == NULL) {
        rawterm_stream = janet_stream(STDIN_FILENO, JANET_STREAM_READABLE, NULL);
    }

    // one time setup
    if (!at_exit_set) {
        atexit(rawterm_at_exit);
        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_flags = 0;
        sa.sa_handler = rawterm_signal_winch;
        sigaction(SIGWINCH, &sa, 0);
        at_exit_set = true;
    }
}

static void rawterm_getch(JanetBuffer *buf) {
    janet_ev_read(rawterm_stream, buf, 1);
    janet_await();
}

#endif

/* Janet bindings */

JANET_FN(cfun_rawterm_begin,
        "(rawterm/begin &opt on-winch)",
        "Begin raw terminal functionality. Return a stream that can be read from to get input.") {
    janet_arity(argc, 0, 1);
    if (argc > 0) {
        rawterm_winch_handler = janet_getfunction(argv, 0);
        janet_gcroot(janet_wrap_function(rawterm_winch_handler));
    } else {
        rawterm_winch_handler = NULL;
    }
    rawterm_begin();
    return janet_wrap_nil();
}

JANET_FN(cfun_rawterm_end,
        "(rawterm/end)",
        "End raw terminal functionality.") {
    janet_fixarity(argc, 0);
    (void) argv;
    rawterm_end();
    if (rawterm_winch_handler != NULL) {
        janet_gcunroot(janet_wrap_function(rawterm_winch_handler));
        rawterm_winch_handler = NULL;
    }
    return janet_wrap_nil();
}

JANET_FN(cfun_rawterm_isatty,
        "(rawterm/isatty)",
        "Check if the current stdin is a tty.") {
    janet_fixarity(argc, 0);
    (void) argv;
#ifdef JANET_WINDOWS
    return janet_wrap_boolean(_isatty(_fileno(stdin)));
#else
    return janet_wrap_boolean(isatty(STDIN_FILENO));
#endif
}

JANET_FN(cfun_rawterm_getch,
        "(rawterm/getch &opt into)",
        "Get a byte of input from stdin, without blocking if possible. Returns a buffer.") {
    janet_arity(argc, 0, 1);
    JanetBuffer *buffer = janet_optbuffer(argv, argc, 0, 0);
    rawterm_getch(buffer);
    return janet_wrap_buffer(buffer);
}

JANET_FN(cfun_rawterm_size,
        "(rawterm/size)",
        "Get the number of rows and columns visible in the terminal as tuple [rows cols]") {
    janet_fixarity(argc, 0);
    (void) argv;
    int rows, cols;
    rawterm_size(&rows, &cols);
    Janet tup[2] = {janet_wrap_integer(rows), janet_wrap_integer(cols)};
    return janet_wrap_tuple(janet_tuple_n(tup, 2));
}

JANET_FN(cfun_rawterm_ctrlz,
        "(rawterm/ctrl-z)",
        "A handler that a user can use to handle ctrl-z from input to suspend the current process.") {
    janet_fixarity(argc, 0);
    (void) argv;
#ifndef _WIN32
    rawterm_end();
    kill(getpid(), SIGSTOP);
    rawterm_begin();
#endif
    return janet_wrap_nil();
}

/* Measure the size of a single character in a monospaced terminal */
static int measure_rune(uint32_t rune) {
    static const struct {
        uint32_t start;
        int32_t width;
        uint64_t end_or_mask;
    } k_width_classes[] = {
        /* AUTO-GENERATED BY tools/wchar_procunicode.janet */
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
        {   2192,  0, 0xe07f800000000000ULL },
        {   2250,  0,               2306    },
        {   2362,  0, 0xd0ff08fe00600000ULL },
        {   2433,  0, 0xc000000000000008ULL },
        {   2497,  0, 0xf804000030000002ULL },
        {   2561,  0, 0xe000000000000008ULL },
        {   2625,  0, 0xe19c40000000c400ULL },
        {   2689,  0, 0xe000000000000008ULL },
        {   2753,  0, 0xfd8400003000003fULL },
        {   2817,  0, 0xc000000000000009ULL },
        {   2881,  0, 0xf804060030000000ULL },
        {   2946,  0, 0xc000000000000001ULL },
        {   3021,  0, 0xc000000000000880ULL },
        {   3132,  0, 0xdc1de03001800000ULL },
        {   3201,  0, 0xc000000000000009ULL },
        {   3270,  0, 0xc180000600000018ULL },
        {   3387,  0, 0xe1e0100000c00000ULL },
        {   3457,  0,               3457    },
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
        {   5906,  0, 0xf000000060000000ULL },
        {   5970,  0, 0xe000000060000000ULL },
        {   6068,  0, 0xefe013ff80200000ULL },
        {   6155,  0,               6159    },
        {   6277,  0, 0xe000000004000000ULL },
        {   6432,  0, 0xf0c0103800000000ULL },
        {   6679,  0, 0xe400000000000000ULL },
        {   6742,  0, 0xdfd4ff03ff200000ULL },
        {   6832,  0,               6862    },
        {   6912,  0, 0xf8000000000005f4ULL },
        {   6978,  0, 0xc0000000003fe000ULL },
        {   7040,  0, 0xe00000001e6e0000ULL },
        {   7142,  0, 0xd8b8000000000000ULL },
        {   7212,  0, 0xff98000000000000ULL },
        {   7376,  0, 0xf7ffdfc204600000ULL },
        {   7616,  0,               7679    },
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
        {  12272,  2,              12283    },
        {  12288,  2,              12350    },
        {  12353,  2,              12438    },
        {  12441,  2,              12543    },
        {  12549,  2,              12591    },
        {  12593,  2,              12686    },
        {  12688,  2,              12771    },
        {  12784,  2,              12830    },
        {  12832,  2,              12871    },
        {  12880,  2,              19903    },
        {  19968,  2,              42124    },
        {  42128,  2,              42182    },
        {  42607,  0, 0xfbff00000000c000ULL },
        {  42736,  0,              42737    },
        {  43010,  0, 0xc42000000c100000ULL },
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
        {  69291,  0,              69292    },
        {  69446,  0,              69456    },
        {  69506,  0,              69509    },
        {  69633,  0,              69633    },
        {  69688,  0, 0xffff00000000004cULL },
        {  69759,  0, 0xf000000000000799ULL },
        {  69826,  0, 0xc008000000000000ULL },
        {  69888,  0, 0xf000000000fbfc00ULL },
        {  70003,  0, 0xc003000000000000ULL },
        {  70070,  0, 0xffc00f2000000000ULL },
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
        {  71995,  0, 0xe840000000000000ULL },
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
        {  94176,  2, 0xfc00600000000000ULL },
        {  94208,  2,             100343    },
        { 100352,  2,             101589    },
        { 101632,  2,             101640    },
        { 110576,  2, 0xfbfb000000000000ULL },
        { 110592,  2,             110882    },
        { 110928,  2, 0xf000078000000000ULL },
        { 110960,  2,             111355    },
        { 113821,  0, 0xef00000000000000ULL },
        { 118528,  0,             118573    },
        { 118576,  0,             118598    },
        { 119143,  0, 0xf007fff9fc000000ULL },
        { 119210,  0,             119213    },
        { 119362,  0,             119364    },
        { 121344,  0,             121398    },
        { 121403,  0, 0xffffffffffffe010ULL },
        { 121476,  0, 0xc00000fbfff80000ULL },
        { 122880,  0, 0xff7fffcfedf00000ULL },
        { 123184,  0,             123190    },
        { 123566,  0,             123566    },
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
        { 128716,  2, 0xc7383800c07fc000ULL },
        { 128992,  2, 0xfff8400000000000ULL },
        { 129292,  2, 0xffffffffffff7fe0ULL },
        { 129351,  2,             129535    },
        { 129648,  2, 0xfc7c7f007ffffffcULL },
        { 129712,  2, 0xfff07e007fe07f80ULL },
        { 129776,  2,             129782    },
        { 131072,  2,             196605    },
        { 196608,  2,             262141    },
        { 917505,  0,             917505    },
        { 917536,  0,             917631    },
    };
    int width;
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
    return width;
}

JANET_FN(cfun_rawterm_rune_monowidth,
        "(rawterm/rune-monowidth rune)",
        "Get the monospace width of a rune. Returns either 0, 1, or 2.") {
    janet_fixarity(argc, 1);
    uint32_t rune = (uint32_t)janet_getinteger(argv, 0);
    return janet_wrap_integer(measure_rune(rune));
}

JANET_FN(cfun_rawterm_monowidth,
        "(rawterm/monowidth bytes &opt start-index)",
        "Measure the monospace width of a string.") {
    janet_arity(argc, 1, 2);
    JanetByteView buf = janet_getbytes(argv, 0);
    uint32_t i = janet_optnat(argv, argc, 1, 0);
    uint32_t length = 0;
    while (i < buf.len) {
        uint8_t a = buf.bytes[i++];
        uint32_t codepoint = 0;
        if ((a & 0x80) == 0) {
            codepoint = a;
        } else if ((a & 0xE0) == 0xC0) {
            if (i >= buf.len) goto exit;
            uint8_t b = buf.bytes[i++];
            if ((b & 0xC0) != 0x80) goto exit;
            codepoint = (uint32_t)(a & 0x1F) << 6 |
                (uint32_t)(b & 0x3F);
        } else if ((a & 0xF0) == 0xE0) {
            if ((i + 1) >= buf.len) goto exit;
            uint8_t b = buf.bytes[i++],
                    c = buf.bytes[i++];
            if ((b & 0xC0) != 0x80) goto exit;
            if ((c & 0xC0) != 0x80) goto exit;
            codepoint = (uint32_t)(a & 0x0F) << 12 |
                (uint32_t)(b & 0x3F) <<  6 |
                (uint32_t)(c & 0x3F);
        } else if ((a & 0xF8) == 0xF0) {
            if ((i + 2) >= buf.len) goto exit;
            uint8_t b = buf.bytes[i++],
                    c = buf.bytes[i++],
                    d = buf.bytes[i++];
            if ((b & 0xC0) != 0x80) goto exit;
            if ((c & 0xC0) != 0x80) goto exit;
            if ((d & 0xC0) != 0x80) goto exit;
            codepoint = (uint32_t)(a & 0x07) << 18 |
                (uint32_t)(b & 0x3F) << 12 |
                (uint32_t)(c & 0x3F) <<  6 |
                (uint32_t)(d & 0x3F);
        } else {
            goto exit;
        }
        length += measure_rune(codepoint);
    }
    return janet_wrap_integer(length);
exit:
    janet_panicf("bad utf-8 at byte position %d", i);
}

/****************/
/* Module Entry */
/****************/

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("begin", cfun_rawterm_begin),
        JANET_REG("end", cfun_rawterm_end),
        JANET_REG("isatty", cfun_rawterm_isatty),
        JANET_REG("getch", cfun_rawterm_getch),
        JANET_REG("size", cfun_rawterm_size),
        JANET_REG("ctrl-z", cfun_rawterm_ctrlz),
        JANET_REG("rune-monowidth", cfun_rawterm_rune_monowidth),
        JANET_REG("monowidth", cfun_rawterm_monowidth),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "rawterm", cfuns);
}
