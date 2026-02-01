library(tinytest)
library(Rtinycc)

close_if_valid <- function(cb) {
    if (inherits(cb, "tcc_callback") && isTRUE(tcc_callback_valid(cb))) {
        tcc_callback_close(cb)
    }
}

# Test: register an R callback, compile C code that invokes it, and call it
expect_true({
    cb <- tcc_callback(function(x) 42, signature = "double (*)(double)")
    cb_ptr <- tcc_callback_ptr(cb)
    on.exit(
        {
            close_if_valid(cb)
            rm(cb_ptr)
            gc()
        },
        add = TRUE
    )

    code <- '\n#define _Complex\n#include <R.h>\n#include <Rinternals.h>\n#include <stdio.h>\n\ntypedef struct { int id; } callback_token_t;\n\nSEXP RC_invoke_callback(SEXP, SEXP);\n\ndouble call_cb(void* cb, double x) {\n  int id = ((callback_token_t*)cb)->id;\n  char buf[32];\n  snprintf(buf, sizeof(buf), "%d", id);\n  SEXP idstr = mkString(buf);\n  SEXP args = PROTECT(allocVector(VECSXP, 1));\n  SET_VECTOR_ELT(args, 0, ScalarReal(x));\n  SEXP res = RC_invoke_callback(idstr, args);\n  UNPROTECT(1);\n  return asReal(res);\n}\n'

    ffi <- tcc_ffi() |>
        tcc_source(code) |>
        tcc_bind(call_cb = list(args = list("ptr", "f64"), returns = "f64")) |>
        tcc_compile()

    expect_true(inherits(ffi, "tcc_compiled"), info = "Compiled FFI object")

    res <- ffi$call_cb(cb_ptr, 21.0)
    rm(ffi)
    gc()
    isTRUE(all.equal(res, 42.0, tolerance = 1e-12))
})

# Test: callback error propagates via R_tryEval
expect_error(
    {
        cb_err <- tcc_callback(function(x) stop("boom"), signature = "double (*)(double)")
        cb_ptr_err <- tcc_callback_ptr(cb_err)
        on.exit(
            {
                close_if_valid(cb_err)
                rm(cb_ptr_err)
                gc()
            },
            add = TRUE
        )

        code_err <- '\n#define _Complex\n#include <R.h>\n#include <Rinternals.h>\n#include <stdio.h>\n\ntypedef struct { int id; } callback_token_t;\n\nSEXP RC_invoke_callback(SEXP, SEXP);\n\ndouble call_cb_err(void* cb, double x) {\n  int id = ((callback_token_t*)cb)->id;\n  char buf[32];\n  snprintf(buf, sizeof(buf), "%d", id);\n  SEXP idstr = mkString(buf);\n  SEXP args = PROTECT(allocVector(VECSXP, 1));\n  SET_VECTOR_ELT(args, 0, ScalarReal(x));\n  SEXP res = RC_invoke_callback(idstr, args);\n  UNPROTECT(1);\n  return asReal(res);\n}\n'

        ffi_err <- tcc_ffi() |>
            tcc_source(code_err) |>
            tcc_bind(call_cb_err = list(args = list("ptr", "f64"), returns = "f64")) |>
            tcc_compile()

        ffi_err$call_cb_err(cb_ptr_err, 1.0)
        rm(ffi_err)
        gc()
    },
    info = "Callback errors propagate to R"
)

# Test: pointer return and pointer args use externalptr
expect_true({
    cb_ptr_rt <- tcc_callback(function(x) x, signature = "void* (*)(void*)")
    cb_ptr_handle <- tcc_callback_ptr(cb_ptr_rt)
    on.exit(
        {
            close_if_valid(cb_ptr_rt)
            rm(cb_ptr_handle)
            gc()
        },
        add = TRUE
    )

    code_ptr <- '\n#define _Complex\n#include <R.h>\n#include <Rinternals.h>\n#include <stdio.h>\n\ntypedef struct { int id; } callback_token_t;\n\nSEXP RC_invoke_callback(SEXP, SEXP);\n\nvoid* echo_ptr(void* cb, void* x) {\n  int id = ((callback_token_t*)cb)->id;\n  char buf[32];\n  snprintf(buf, sizeof(buf), "%d", id);\n  SEXP idstr = mkString(buf);\n  SEXP args = PROTECT(allocVector(VECSXP, 1));\n  SET_VECTOR_ELT(args, 0, R_MakeExternalPtr(x, R_NilValue, R_NilValue));\n  SEXP res = RC_invoke_callback(idstr, args);\n  UNPROTECT(1);\n  return R_ExternalPtrAddr(res);\n}\n'

    ffi_ptr <- tcc_ffi() |>
        tcc_source(code_ptr) |>
        tcc_bind(echo_ptr = list(args = list("ptr", "ptr"), returns = "ptr")) |>
        tcc_compile()

    buf <- tcc_malloc(8)
    out <- ffi_ptr$echo_ptr(cb_ptr_handle, buf)
    ok <- inherits(out, "externalptr")
    tcc_free(buf)
    rm(out)
    rm(ffi_ptr)
    gc()
    ok
})

# Test: ptr handle stays alive but callback is invalid after close
expect_error(
    {
        cb_closed <- tcc_callback(function(x) x, signature = "double (*)(double)")
        cb_ptr_closed <- tcc_callback_ptr(cb_closed)
        on.exit(
            {
                close_if_valid(cb_closed)
                rm(cb_ptr_closed)
                gc()
            },
            add = TRUE
        )

        code_closed <- '\n#define _Complex\n#include <R.h>\n#include <Rinternals.h>\n#include <stdio.h>\n\ntypedef struct { int id; } callback_token_t;\n\nSEXP RC_invoke_callback(SEXP, SEXP);\n\ndouble call_cb_closed(void* cb, double x) {\n  int id = ((callback_token_t*)cb)->id;\n  char buf[32];\n  snprintf(buf, sizeof(buf), "%d", id);\n  SEXP idstr = mkString(buf);\n  SEXP args = PROTECT(allocVector(VECSXP, 1));\n  SET_VECTOR_ELT(args, 0, ScalarReal(x));\n  SEXP res = RC_invoke_callback(idstr, args);\n  UNPROTECT(1);\n  return asReal(res);\n}\n'

        ffi_closed <- tcc_ffi() |>
            tcc_source(code_closed) |>
            tcc_bind(call_cb_closed = list(args = list("ptr", "f64"), returns = "f64")) |>
            tcc_compile()

        tcc_callback_close(cb_closed)
        ffi_closed$call_cb_closed(cb_ptr_closed, 1.0)
        rm(ffi_closed)
        gc()
    },
    info = "Closed callback ptr yields error"
)
