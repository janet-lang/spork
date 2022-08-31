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

JANET_FN(cfun_rawterm_rune_monowidth,
        "(rawterm/rune-monowidth rune)",
        "Get the monospace width of a rune. Returns either 0, 1 or 2.") {
    janet_fixarity(argc, 1);
    int32_t rune = janet_getinteger(argv, 0);
    /* TODO: This could use wcwidth, but it's dependent on LC_CTYPE? This is
     * a simplification borrowed from bestline that is mostly, but not entirely,
     * correct; notably, this doesn't account for combining diacritics that
     * have 0 width on their own. Cosmopolitan uses its own implementation of
     * wcwidth that's based on their internal tables; perhaps that might be
     * borrowed for here as well, though a raw bitset is a bit expensive
     * (~140 KiB) whereas they use a compressed form which would be needlessly
     * complex. */
    int32_t width;
    if (rune <= 0x1f || (rune >= 0x7f && rune <= 0x9f)) {
        width = 0;
    } else if ((rune >= 0x1100 && rune <= 0x115f) ||
               (rune == 0x2329) || (rune == 0x232a) ||
               (rune >= 0x2e80 && rune <= 0xa4cf && rune != 0x303f) ||
               (rune >= 0xac00 && rune <= 0xd7a3) ||
               (rune >= 0xf900 && rune <= 0xfaff) ||
               (rune >= 0xfe10 && rune <= 0xfe19) ||
               (rune >= 0xfe30 && rune <= 0xfe6f) ||
               (rune >= 0xff00 && rune <= 0xff60) ||
               (rune >= 0xffe0 && rune <= 0xffe6) ||
               (rune >= 0x20000 && rune <= 0x2fffd) ||
               (rune >= 0x30000 && rune <= 0x3fffd)) {
        width = 2;
    } else {
        width = 1;
    }
    return janet_wrap_integer(width);
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
        JANET_REG_END
    };
    janet_cfuns_ext(env, "rawterm", cfuns);
}
