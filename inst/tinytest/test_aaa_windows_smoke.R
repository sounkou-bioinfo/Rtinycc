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

# ---------- 5. Struct (calloc + R_MakeExternalPtr + RC_free_finalizer) --------
expect_true(
    {
        ffi <- tcc_ffi()
        ffi <- tcc_source(ffi, "struct point { int x; int y; };")
        ffi <- tcc_struct(ffi, "point", accessors = c(x = "i32", y = "i32"))
        ffi <- tcc_bind(ffi)
        compiled <- tcc_compile(ffi)

        p <- compiled$struct_point_new()
        p <- compiled$struct_point_set_x(p, 10L)
        p <- compiled$struct_point_set_y(p, 20L)
        x <- compiled$struct_point_get_x(p)
        y <- compiled$struct_point_get_y(p)
        compiled$struct_point_free(p)

        x == 10L && y == 20L
    },
    info = "smoke: struct new/get/set/free"
)
