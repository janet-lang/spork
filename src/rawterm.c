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

#ifdef JANET_LINUX
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

/* Per-Platform Implementation */

#ifdef JANET_WINDOWS

static void rawterm_begin(void) {
    janet_panic("not implemented on windows");
}
static void rawterm_end(void) {
    janet_panic("not implemented on windows");
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
static JANET_THREAD_LOCAL void *input_stream = NULL;
static JANET_THREAD_LOCAL bool at_exit_set = false;

static void rawterm_end(void) {
    if (!in_raw_mode) {
        janet_panic("not in raw mode");
    }
    if (tcsetattr(STDIN_FILENO, TCSADRAIN, &starting_term) == -1) {
        janet_panic("could not reset to orignal tty attributes");
    }
    in_raw_mode = false;
    janet_stream_close(input_stream);
    input_stream = NULL;
}

static void rawterm_at_exit(void) {
    if (in_raw_mode) {
        /* best attempt to reset terminal, if it fails, oh well */
        tcsetattr(STDIN_FILENO, TCSADRAIN, &starting_term);
    }
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
    if (!at_exit_set) {
        atexit(rawterm_at_exit);
        at_exit_set = true;
    }
    input_stream = janet_stream(STDIN_FILENO, JANET_STREAM_READABLE, NULL);
}

#endif

/* Janet bindings */

JANET_FN(cfun_rawterm_begin,
        "(rawterm/begin)",
        "Begin raw terminal functionality. Return a stream that can be read from to get input.") {
    janet_fixarity(argc, 0);
    (void) argv;
    rawterm_begin();
    return janet_wrap_abstract(input_stream);
}

JANET_FN(cfun_rawterm_end,
        "(rawterm/end)",
        "End raw terminal functionality.") {
    janet_fixarity(argc, 0);
    (void) argv;
    rawterm_end();
    return janet_wrap_nil();
}

/****************/
/* Module Entry */
/****************/

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("begin", cfun_rawterm_begin),
        JANET_REG("end", cfun_rawterm_end),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "rawterm", cfuns);
}
