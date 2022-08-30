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
static JANET_THREAD_LOCAL JanetStream *rawterm_stream = NULL;
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

static void rawterm_begin(void) {
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

static void rawterm_get_stdin(void) {
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    rawterm_stream = janet_stream(h, JANET_STREAM_READABLE, NULL);
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

static void rawterm_end(void) {
    if (!in_raw_mode) {
        janet_panic("not in raw mode");
    }
    if (tcsetattr(STDIN_FILENO, TCSADRAIN, &starting_term) == -1) {
        janet_panic("could not reset to orignal tty attributes");
    }
    in_raw_mode = false;
    rawterm_stream = NULL;
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

static void rawterm_get_stdin(void) {
    rawterm_stream = janet_stream(STDIN_FILENO, JANET_STREAM_READABLE, NULL);
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
    if (NULL == rawterm_stream) {
        rawterm_get_stdin();
    }
    return janet_wrap_abstract(rawterm_stream);
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

JANET_FN(cfun_rawterm_stdin,
        "(rawterm/stdin)",
        "Get a stream for interaction with stdin. Do not use `stdin` in conjuction with this stream.") {
    janet_fixarity(argc, 0);
    (void) argv;
    if (NULL == rawterm_stream) {
        rawterm_get_stdin();
    }
    return janet_wrap_abstract(rawterm_stream);
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

/****************/
/* Module Entry */
/****************/

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("begin", cfun_rawterm_begin),
        JANET_REG("end", cfun_rawterm_end),
        JANET_REG("isatty", cfun_rawterm_isatty),
        JANET_REG("stdin", cfun_rawterm_stdin),
        JANET_REG("size", cfun_rawterm_size),
        JANET_REG("ctrl-z", cfun_rawterm_ctrlz),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "rawterm", cfuns);
}
