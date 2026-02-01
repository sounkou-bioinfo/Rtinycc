library(tinytest)
library(Rtinycc)

# Test: register an R callback, compile C code that invokes it, and call it
expect_true({
    cb <- tcc_callback(function(x) 42, signature = "double (*)(double)")
    cb_ptr <- tcc_callback_ptr(cb)

    code <- '\n#define _Complex\n#include <R.h>\n#include <Rinternals.h>\n#include <stdio.h>\n\ntypedef struct { int id; } callback_token_t;\n\nSEXP RC_invoke_callback(SEXP, SEXP);\n\ndouble call_cb(void* cb, double x) {\n  int id = ((callback_token_t*)cb)->id;\n  char buf[32];\n  snprintf(buf, sizeof(buf), "%d", id);\n  SEXP idstr = mkString(buf);\n  SEXP args = PROTECT(allocVector(VECSXP, 1));\n  SET_VECTOR_ELT(args, 0, ScalarReal(x));\n  SEXP res = RC_invoke_callback(idstr, args);\n  UNPROTECT(1);\n  return asReal(res);\n}\n'

    ffi <- tcc_ffi() |>
        tcc_source(code) |>
        tcc_bind(call_cb = list(args = list("ptr", "f64"), returns = "f64")) |>
        tcc_compile()

    expect_true(inherits(ffi, "tcc_compiled"), info = "Compiled FFI object")

    res <- ffi$call_cb(cb_ptr, 21.0)
    tcc_callback_close(cb)

    isTRUE(all.equal(res, 42.0, tolerance = 1e-12))
})
