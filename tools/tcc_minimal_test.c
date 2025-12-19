// minimal_tcc_test.c
// Minimal test for tcc_compile_string bus error (no R involved)
// Usage: ./tcc tcc_minimal_test.c

#include "libtcc.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    TCCState *s = tcc_new();
    if (!s) {
        fprintf(stderr, "tcc_new failed\n");
        return 1;
    }
    // Set output type to memory (JIT)
    if (tcc_set_output_type(s, TCC_OUTPUT_MEMORY) != 0) {
        fprintf(stderr, "tcc_set_output_type failed\n");
        tcc_delete(s);
        return 2;
    }
    // Compile a trivial function
    const char *src = "int forty_two() { return 42; }";
    int rc = tcc_compile_string(s, src);
    if (rc != 0) {
        fprintf(stderr, "tcc_compile_string failed: %d\n", rc);
        tcc_delete(s);
        return 3;
    }
    // Relocate
#ifdef TCC_RELOCATE_AUTO
    rc = tcc_relocate(s, TCC_RELOCATE_AUTO);
#else
    rc = tcc_relocate(s);
#endif
    if (rc != 0) {
        fprintf(stderr, "tcc_relocate failed: %d\n", rc);
        tcc_delete(s);
        return 4;
    }
    // Get symbol
    int (*fn)() = (int (*)()) tcc_get_symbol(s, "forty_two");
    if (!fn) {
        fprintf(stderr, "tcc_get_symbol failed\n");
        tcc_delete(s);
        return 5;
    }
    int val = fn();
    printf("forty_two() = %d\n", val);
    tcc_delete(s);
    return 0;
}
