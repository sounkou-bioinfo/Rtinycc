# Windows smoke tests — run first (alphabetically) to diagnose JIT issues.
# Each test isolates a specific JIT capability so we can pinpoint failures.

library(tinytest)
library(Rtinycc)

# ---------- 1. Trivial: return a constant integer ----------------------------
expect_equal(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "int get_42(void) { return 42; }")
        ffi <- tcc_bind(ffi, get_42 = list(args = list(), returns = "i32"))
        compiled <- tcc_compile(ffi)
        compiled$get_42()
    },
    42L,
    info = "smoke: constant int return"
)

# ---------- 2. Two-argument function ----------------------------------------
expect_equal(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "int add(int a, int b) { return a + b; }")
        ffi <- tcc_bind(ffi, add = list(args = list("i32", "i32"), returns = "i32"))
        compiled <- tcc_compile(ffi)
        compiled$add(3L, 4L)
    },
    7L,
    info = "smoke: two-arg i32 add"
)

# ---------- 3. Double (f64) --------------------------------------------------
expect_equal(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "double mul(double a, double b) { return a * b; }")
        ffi <- tcc_bind(ffi, mul = list(args = list("f64", "f64"), returns = "f64"))
        compiled <- tcc_compile(ffi)
        compiled$mul(2.5, 4.0)
    },
    10.0,
    info = "smoke: f64 multiply"
)

# ---------- 4. Global variable -----------------------------------------------
expect_equal(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "int global_x = 99;")
        ffi <- tcc_global(ffi, "global_x", "i32")
        compiled <- tcc_compile(ffi)
        compiled$global_global_x_get()
    },
    99L,
    info = "smoke: global variable"
)

# ---------- 5. calloc + free via JIT (no R API, just CRT) --------------------
expect_equal(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "
#include <stdlib.h>
int test_calloc(void) {
    int *p = (int*)calloc(1, sizeof(int));
    if (!p) return -1;
    *p = 123;
    int v = *p;
    free(p);
    return v;
}
")
        ffi <- tcc_bind(ffi, test_calloc = list(args = list(), returns = "i32"))
        compiled <- tcc_compile(ffi)
        compiled$test_calloc()
    },
    123L,
    info = "smoke: calloc/free from CRT"
)

# ---------- 6. R_MakeExternalPtr + R_NilValue (R API data imports) -----------
expect_true(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "
#include <stdlib.h>
SEXP test_extptr(void) {
    int *p = (int*)calloc(1, sizeof(int));
    if (!p) Rf_error(\"OOM\");
    *p = 42;
    SEXP ext = R_MakeExternalPtr(p, R_NilValue, R_NilValue);
    return ext;
}
")
        ffi <- tcc_bind(ffi, test_extptr = list(args = list(), returns = "sexp"))
        compiled <- tcc_compile(ffi)
        res <- compiled$test_extptr()
        is(res, "externalptr")
    },
    info = "smoke: R_MakeExternalPtr + R_NilValue"
)

# ---------- 7. R_RegisterCFinalizerEx with RC_free_finalizer -----------------
expect_true(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "
#include <stdlib.h>
SEXP test_finalizer(void) {
    int *p = (int*)calloc(1, sizeof(int));
    if (!p) Rf_error(\"OOM\");
    *p = 99;
    SEXP ext = R_MakeExternalPtr(p, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(ext, RC_free_finalizer, TRUE);
    return ext;
}
")
        ffi <- tcc_bind(ffi, test_finalizer = list(args = list(), returns = "sexp"))
        compiled <- tcc_compile(ffi)
        res <- compiled$test_finalizer()
        is(res, "externalptr")
    },
    info = "smoke: R_RegisterCFinalizerEx + RC_free_finalizer"
)

# ---------- 8. struct: compile only (no call) ---------------------------------
expect_true(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "struct point { int x; int y; };")
        ffi <- tcc_struct(ffi, "point", accessors = c(x = "i32", y = "i32"))
        ffi <- tcc_bind(ffi)
        compiled <- tcc_compile(ffi)
        inherits(compiled, "tcc_compiled")
    },
    info = "smoke: struct compile succeeds"
)

# ---------- 9. struct: new() only --------------------------------------------
expect_true(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "struct point { int x; int y; };")
        ffi <- tcc_struct(ffi, "point", accessors = c(x = "i32", y = "i32"))
        ffi <- tcc_bind(ffi)
        compiled <- tcc_compile(ffi)

        p <- compiled$struct_point_new()
        is(p, "externalptr")
    },
    info = "smoke: struct_point_new()"
)

# ---------- 10. struct: set_x() ---------------------------------------------
expect_true(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "struct point { int x; int y; };")
        ffi <- tcc_struct(ffi, "point", accessors = c(x = "i32", y = "i32"))
        ffi <- tcc_bind(ffi)
        compiled <- tcc_compile(ffi)

        p <- compiled$struct_point_new()
        p <- compiled$struct_point_set_x(p, 10L)
        is(p, "externalptr")
    },
    info = "smoke: struct_point_set_x()"
)

# ---------- 11. struct: get_x() ---------------------------------------------
expect_equal(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "struct point { int x; int y; };")
        ffi <- tcc_struct(ffi, "point", accessors = c(x = "i32", y = "i32"))
        ffi <- tcc_bind(ffi)
        compiled <- tcc_compile(ffi)

        p <- compiled$struct_point_new()
        p <- compiled$struct_point_set_x(p, 10L)
        compiled$struct_point_get_x(p)
    },
    10L,
    info = "smoke: struct_point_get_x()"
)

# ---------- 12. struct: free() ----------------------------------------------
expect_true(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "struct point { int x; int y; };")
        ffi <- tcc_struct(ffi, "point", accessors = c(x = "i32", y = "i32"))
        ffi <- tcc_bind(ffi)
        compiled <- tcc_compile(ffi)

        p <- compiled$struct_point_new()
        compiled$struct_point_free(p)
        TRUE
    },
    info = "smoke: struct_point_free()"
)
