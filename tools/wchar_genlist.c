#ifndef COSMOPOLITAN
#define _XOPEN_SOURCE
#include <wchar.h>
#include <stdio.h>
#include <stddef.h>
#endif

int main(void) {
    int run_width = 1;
    unsigned run_start;
    for (unsigned i = 0; i <= 0x10FFFF; i += 1) {
        int width = wcwidth((wchar_t)i);
        if (width != run_width) {
            if (run_width != 1) {
                printf("%5x\t%5x\t%i\n", run_start, i - 1, run_width);
            }
            run_width = width;
            run_start = i;
        }
    }
    return 0;
}
