# Codegen Soundness Framework
# ===========================================================================
#
# This file replaces grepl-based string tests with compile-and-call round-trip
# proofs. For every FFI type, we generate a C identity function, compile it
# with TinyCC, call it with known values, and verify the output matches.
#
# Sections:
#   1. Scalar input/output round-trips (every scalar FFI type)
#   2. Boundary value tests (min, max, zero, NA rejection)
#   3. Array round-trips (every array FFI type)
#   4. Struct field round-trips (every field type)
#   5. Callback return type round-trips
#   6. PROTECT balance / GC stress tests
#   7. Codegen structural properties (single-eval, no double free)
#
# ---------------------------------------------------------------------------

library(tinytest)
library(Rtinycc)

ffi_semantics <- Rtinycc:::rtinycc_ffi_semantics()
scalar_specs <- Rtinycc:::rtinycc_scalar_soundness_specs()

expect_equal(
  sort(names(ffi_semantics)),
  sort(Rtinycc:::VALID_FFI_TYPES),
  info = "ffi semantics covers every declared base ffi type"
)

# ===========================================================================
# 1. SCALAR ROUND-TRIP: compile identity(x) for each type, call, verify
# ===========================================================================

default_if_null <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

eval_case_value <- function(value) {
  if (is.language(value)) {
    eval(value, envir = baseenv())
  } else {
    value
  }
}

make_identity <- function(c_type, ffi_type, c_body = NULL) {
  if (is.null(c_body)) {
    c_body <- sprintf("%s identity(%s x) { return x; }", c_type, c_type)
  }

  tcc_ffi() |>
    tcc_source(c_body) |>
    tcc_bind(identity = list(args = list(ffi_type), returns = ffi_type)) |>
    tcc_compile()
}

make_identity_from_spec <- function(spec) {
  identity <- spec$soundness$identity
  c_body <- default_if_null(
    identity$source,
    sprintf("%s identity(%s x) { return x; }", identity$c_type, identity$c_type)
  )

  make_identity(identity$c_type, spec$ffi_type, c_body = c_body)
}

expect_scalar_roundtrip <- function(spec, actual, expected, label) {
  info <- sprintf("%s round-trip: %s", spec$ffi_type, label)
  comparator <- default_if_null(spec$soundness$comparator, "equal")

  switch(
    comparator,
    equal = expect_equal(actual, expected, info = info),
    tolerance = expect_true(
      abs(actual - expected) < spec$soundness$tolerance,
      info = info
    ),
    stop("Unsupported scalar comparator: ", comparator, call. = FALSE)
  )
}

for (spec in scalar_specs) {
  ffi <- make_identity_from_spec(spec)

  for (case in spec$soundness$roundtrip) {
    input <- eval_case_value(case$value)
    expected <- eval_case_value(default_if_null(case$expected, case$value))
    actual <- ffi$identity(input)
    expect_scalar_roundtrip(spec, actual, expected, case$label)
  }
}

# --- ptr ---
ffi <- tcc_ffi() |>
  tcc_source("void* identity(void* x) { return x; }") |>
  tcc_bind(identity = list(args = list("ptr"), returns = "ptr")) |>
  tcc_compile()
buf <- tcc_malloc(8)
result <- ffi$identity(buf)
expect_equal(
  tcc_ptr_addr(result),
  tcc_ptr_addr(buf),
  info = "ptr round-trip: address preserved"
)
expect_false(
  tcc_ptr_is_owned(result),
  info = "ptr round-trip: returned wrapper is not owned"
)
expect_error(
  tcc_free(result),
  info = "ptr round-trip: explicit free refuses unowned wrapper"
)
tcc_free(buf)

# --- void ---
ffi <- tcc_ffi() |>
  tcc_source(
    "int g_side_effect = 0;\nvoid set_flag(int v) { g_side_effect = v; }"
  ) |>
  tcc_bind(set_flag = list(args = list("i32"), returns = "void")) |>
  tcc_compile()
result <- ffi$set_flag(42L)
expect_true(is.null(result), info = "void return: returns NULL/invisible")

# normalized symbol specs encode soundness-relevant distinctions before codegen
sym_ptr <- Rtinycc:::as_rtinycc_bound_symbol(
  "identity_ptr",
  list(args = list("ptr"), returns = "ptr")
)
expect_true(
  Rtinycc:::is_rtinycc_bound_symbol(sym_ptr),
  info = "normalized symbol spec is classed"
)
expect_identical(
  Rtinycc:::ffi_type_family(sym_ptr$arg_type_info[[1]]),
  "ptr",
  info = "normalized symbol spec preserves ptr arg family"
)
expect_identical(
  Rtinycc:::ffi_type_family(sym_ptr$return_spec$type_info),
  "ptr",
  info = "normalized symbol spec preserves ptr return family"
)

sym_arr <- Rtinycc:::as_rtinycc_bound_symbol(
  "copy_ints",
  list(
    args = list("integer_array", "i32"),
    returns = list(type = "integer_array", length_arg = 2, free = TRUE)
  )
)
expect_identical(
  sym_arr$return_spec$length_arg,
  2,
  info = "normalized symbol spec preserves array return length_arg"
)
expect_true(
  isTRUE(sym_arr$return_spec$free),
  info = "normalized symbol spec preserves array return free flag"
)

sym_cb <- Rtinycc:::as_rtinycc_bound_symbol(
  "call_cb",
  list(args = list("callback:SEXP(SEXP)", "ptr", "sexp"), returns = "sexp")
)
expect_identical(
  Rtinycc:::ffi_type_family(sym_cb$arg_type_info[[1]]),
  "callback",
  info = "normalized symbol spec preserves callback arg family"
)
expect_identical(
  Rtinycc:::ffi_type_family(sym_cb$arg_type_info[[3]]),
  "sexp",
  info = "normalized symbol spec distinguishes callback from sexp pass-through arg"
)

expect_error(
  Rtinycc:::as_rtinycc_bound_symbol(
    "bad_async_sexp",
    list(
      args = list("callback_async:SEXP(int)", "ptr", "i32"),
      returns = "sexp"
    )
  ),
  info = "normalized symbol spec rejects async callback SEXP return before codegen"
)
expect_error(
  Rtinycc:::as_rtinycc_bound_symbol(
    "bad_async_sexp_arg",
    list(
      args = list("callback_async:void(SEXP)", "ptr", "sexp"),
      returns = "void"
    )
  ),
  info = "normalized symbol spec rejects async callback SEXP arg before codegen"
)
expect_error(
  Rtinycc:::as_rtinycc_bound_symbol(
    "bad_nonarray_meta",
    list(
      args = list("i32"),
      returns = list(type = "i32", length_arg = 1, free = TRUE)
    )
  ),
  info = "normalized symbol spec rejects array metadata on non-array returns"
)

# ===========================================================================
# 2. BOUNDARY VALUE TESTS: NA rejection, range errors
# ===========================================================================

for (spec in scalar_specs) {
  if (!length(spec$soundness$reject)) {
    next
  }

  ffi <- make_identity_from_spec(spec)

  for (case in spec$soundness$reject) {
    expect_error(
      ffi$identity(eval_case_value(case$value)),
      info = sprintf("%s rejects %s", spec$ffi_type, case$label)
    )
  }
}

# Arity enforcement
ffi_i32 <- make_identity_from_spec(scalar_specs[["i32"]])
expect_error(ffi_i32$identity(), info = "rejects under-arity")
expect_error(ffi_i32$identity(1L, 2L), info = "rejects over-arity")

# ===========================================================================
# 3. ARRAY ROUND-TRIPS: pass array to C, get it back
# ===========================================================================

# integer_array: pass through C, memcpy back
ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    #include <string.h>
    int* copy_ints(int* arr, int n) {
      int* out = (int*)malloc(sizeof(int) * n);
      memcpy(out, arr, sizeof(int) * n);
      return out;
    }
  "
  ) |>
  tcc_bind(
    copy_ints = list(
      args = list("integer_array", "i32"),
      returns = list(type = "integer_array", length_arg = 2, free = TRUE)
    )
  ) |>
  tcc_compile()

input_ints <- as.integer(c(-100, 0, 1, 42, .Machine$integer.max))
expect_equal(
  ffi$copy_ints(input_ints, length(input_ints)),
  input_ints,
  info = "integer_array round-trip preserves all values"
)

# numeric_array
ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    #include <string.h>
    double* copy_doubles(double* arr, int n) {
      double* out = (double*)malloc(sizeof(double) * n);
      memcpy(out, arr, sizeof(double) * n);
      return out;
    }
  "
  ) |>
  tcc_bind(
    copy_doubles = list(
      args = list("numeric_array", "i32"),
      returns = list(type = "numeric_array", length_arg = 2, free = TRUE)
    )
  ) |>
  tcc_compile()

input_dbls <- c(-1e308, 0.0, 1e-308, 3.14, 1e308)
expect_equal(
  ffi$copy_doubles(input_dbls, length(input_dbls)),
  input_dbls,
  info = "numeric_array round-trip preserves all values"
)

# raw (uint8_t) array
ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    #include <string.h>
    unsigned char* copy_raw(unsigned char* buf, int n) {
      unsigned char* out = (unsigned char*)malloc(n);
      memcpy(out, buf, n);
      return out;
    }
  "
  ) |>
  tcc_bind(
    copy_raw = list(
      args = list("raw", "i32"),
      returns = list(type = "raw", length_arg = 2, free = TRUE)
    )
  ) |>
  tcc_compile()

input_raw <- as.raw(c(0x00, 0x01, 0x7f, 0x80, 0xff))
expect_equal(
  ffi$copy_raw(input_raw, length(input_raw)),
  input_raw,
  info = "raw array round-trip preserves all values"
)

# logical_array
ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    #include <string.h>
    int* copy_logicals(int* arr, int n) {
      int* out = (int*)malloc(sizeof(int) * n);
      memcpy(out, arr, sizeof(int) * n);
      return out;
    }
  "
  ) |>
  tcc_bind(
    copy_logicals = list(
      args = list("logical_array", "i32"),
      returns = list(type = "logical_array", length_arg = 2, free = TRUE)
    )
  ) |>
  tcc_compile()

input_lgl <- c(TRUE, FALSE, TRUE, FALSE)
expect_equal(
  ffi$copy_logicals(input_lgl, length(input_lgl)),
  input_lgl,
  info = "logical_array round-trip preserves all values"
)

# Array input wrappers reject wrong SEXPTYPE before borrowing vector storage.
ffi <- tcc_ffi() |>
  tcc_source(
    "
    void touch_raw(unsigned char* buf) { (void) buf; }
    void touch_ints(int* x) { (void) x; }
    void touch_nums(double* x) { (void) x; }
    void touch_lgl(int* x) { (void) x; }
  "
  ) |>
  tcc_bind(
    touch_raw = list(args = list("raw"), returns = "void"),
    touch_ints = list(args = list("integer_array"), returns = "void"),
    touch_nums = list(args = list("numeric_array"), returns = "void"),
    touch_lgl = list(args = list("logical_array"), returns = "void")
  ) |>
  tcc_compile()

expect_error(
  ffi$touch_raw(1:3),
  info = "raw input rejects non-raw vectors before borrowing"
)
expect_error(
  ffi$touch_ints(c(1, 2, 3)),
  info = "integer_array input rejects non-integer vectors before borrowing"
)
expect_error(
  ffi$touch_nums(1:3),
  info = "numeric_array input rejects non-numeric vectors before borrowing"
)
expect_error(
  ffi$touch_lgl(c(1L, 0L, 1L)),
  info = "logical_array input rejects non-logical vectors before borrowing"
)
# character_array type-check insertion is covered structurally in
# test_ffi_codegen.R. The generated wrapper uses the R-defined
# `const SEXP *` STRING_PTR_RO path (CHARSXP string-cell handles, not char**).
# A runtime compile round-trip is avoided here because TinyCC has shown
# platform-specific instability around small helpers that directly exercise
# this path on macOS CI.

# NULL array return -> R_NilValue
ffi <- tcc_ffi() |>
  tcc_source(
    "
    int* return_null(int n) { return 0; }
  "
  ) |>
  tcc_bind(
    return_null = list(
      args = list("i32"),
      returns = list(type = "integer_array", length_arg = 1, free = FALSE)
    )
  ) |>
  tcc_compile()
expect_true(
  is.null(ffi$return_null(5L)),
  info = "NULL array return gives R NULL"
)

# Zero-length array returns still round-trip as empty R vectors when C returns
# a non-NULL buffer. This guards the wrapper path used in the benchmark
# vignette, where length 0 should yield numeric(0) rather than NULL.
ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    double* zero_vec(int n) {
      if (n != 0) return NULL;
      return (double*) malloc(sizeof(double));
    }
  "
  ) |>
  tcc_bind(
    zero_vec = list(
      args = list("i32"),
      returns = list(type = "numeric_array", length_arg = 1, free = TRUE)
    )
  ) |>
  tcc_compile()
expect_equal(
  ffi$zero_vec(0L),
  numeric(0),
  info = "zero-length array return with non-NULL buffer yields empty R vector"
)

# ===========================================================================
# 4. STRUCT FIELD ROUND-TRIPS: set field, read it back for each type
# ===========================================================================

ffi <- tcc_ffi() |>
  tcc_source(
    "
    struct all_types {
      int8_t   v_i8;
      int16_t  v_i16;
      int32_t  v_i32;
      int64_t  v_i64;
      uint8_t  v_u8;
      uint16_t v_u16;
      uint32_t v_u32;
      uint64_t v_u64;
      float    v_f32;
      double   v_f64;
      bool     v_bool;
    };
  "
  ) |>
  tcc_struct(
    "all_types",
    accessors = c(
      v_i8 = "i8",
      v_i16 = "i16",
      v_i32 = "i32",
      v_i64 = "i64",
      v_u8 = "u8",
      v_u16 = "u16",
      v_u32 = "u32",
      v_u64 = "u64",
      v_f32 = "f32",
      v_f64 = "f64",
      v_bool = "bool"
    )
  ) |>
  tcc_bind() |>
  tcc_compile()

s <- ffi$struct_all_types_new()

# i8 field
ffi$struct_all_types_set_v_i8(s, 42L)
expect_equal(
  ffi$struct_all_types_get_v_i8(s),
  42L,
  info = "struct i8 field round-trip"
)

# i16 field
ffi$struct_all_types_set_v_i16(s, -1000L)
expect_equal(
  ffi$struct_all_types_get_v_i16(s),
  -1000L,
  info = "struct i16 field round-trip"
)

# i32 field
ffi$struct_all_types_set_v_i32(s, .Machine$integer.max)
expect_equal(
  ffi$struct_all_types_get_v_i32(s),
  .Machine$integer.max,
  info = "struct i32 field round-trip"
)

# i64 field (through double)
ffi$struct_all_types_set_v_i64(s, 2^52)
expect_equal(
  ffi$struct_all_types_get_v_i64(s),
  2^52,
  info = "struct i64 field round-trip"
)

# u8 field
ffi$struct_all_types_set_v_u8(s, 255L)
expect_equal(
  ffi$struct_all_types_get_v_u8(s),
  255L,
  info = "struct u8 field round-trip"
)

# u16 field
ffi$struct_all_types_set_v_u16(s, 65535L)
expect_equal(
  ffi$struct_all_types_get_v_u16(s),
  65535L,
  info = "struct u16 field round-trip"
)

# u32 field (through double)
ffi$struct_all_types_set_v_u32(s, 4294967295)
expect_equal(
  ffi$struct_all_types_get_v_u32(s),
  4294967295,
  info = "struct u32 field round-trip"
)

# u64 field (through double)
ffi$struct_all_types_set_v_u64(s, 2^53)
expect_equal(
  ffi$struct_all_types_get_v_u64(s),
  2^53,
  info = "struct u64 field round-trip"
)

# f32 field
ffi$struct_all_types_set_v_f32(s, 3.14)
expect_true(
  abs(ffi$struct_all_types_get_v_f32(s) - 3.14) < 1e-5,
  info = "struct f32 field round-trip (f32 precision)"
)

# f64 field
ffi$struct_all_types_set_v_f64(s, 3.141592653589793)
expect_equal(
  ffi$struct_all_types_get_v_f64(s),
  3.141592653589793,
  info = "struct f64 field round-trip"
)

# bool field
ffi$struct_all_types_set_v_bool(s, TRUE)
expect_equal(
  ffi$struct_all_types_get_v_bool(s),
  TRUE,
  info = "struct bool field round-trip: TRUE"
)
ffi$struct_all_types_set_v_bool(s, FALSE)
expect_equal(
  ffi$struct_all_types_get_v_bool(s),
  FALSE,
  info = "struct bool field round-trip: FALSE"
)

ffi$struct_all_types_free(s)

# ===========================================================================
# 5. MULTI-ARG WRAPPERS: verify argument ordering is preserved
# ===========================================================================

ffi <- tcc_ffi() |>
  tcc_source(
    "
    double weighted_sum(double a, int32_t b, double c) {
      return a * 1.0 + (double)b * 10.0 + c * 100.0;
    }
  "
  ) |>
  tcc_bind(
    weighted_sum = list(
      args = list("f64", "i32", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

expect_equal(
  ffi$weighted_sum(1.0, 2L, 3.0),
  1.0 * 1.0 + 2.0 * 10.0 + 3.0 * 100.0,
  info = "multi-arg: argument order preserved"
)

# Verify with different values to rule out coincidence
expect_equal(
  ffi$weighted_sum(7.0, 0L, 0.5),
  7.0 * 1.0 + 0.0 * 10.0 + 0.5 * 100.0,
  info = "multi-arg: argument order confirmed"
)

# ===========================================================================
# 6. CALLBACK ROUND-TRIPS: verify C->R->C return conversion per type
# ===========================================================================

close_if_valid <- function(cb) {
  if (inherits(cb, "tcc_callback") && isTRUE(tcc_callback_valid(cb))) {
    tcc_callback_close(cb)
  }
}

# int callback
cb <- tcc_callback(function(x) x * 2L, signature = "int (*)(int)")
cb_ptr <- tcc_callback_ptr(cb)

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #define _Complex
    int call_int_cb(int (*cb)(void*, int), void* ctx, int x) {
      return cb(ctx, x);
    }
  "
  ) |>
  tcc_bind(
    call_int_cb = list(
      args = list("callback:int(int)", "ptr", "i32"),
      returns = "i32"
    )
  ) |>
  tcc_compile()

expect_equal(
  ffi$call_int_cb(cb, cb_ptr, 21L),
  42L,
  info = "callback int round-trip: 21*2=42"
)
close_if_valid(cb)

# double callback
cb <- tcc_callback(function(x) x + 0.5, signature = "double (*)(double)")
cb_ptr <- tcc_callback_ptr(cb)

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #define _Complex
    double call_dbl_cb(double (*cb)(void*, double), void* ctx, double x) {
      return cb(ctx, x);
    }
  "
  ) |>
  tcc_bind(
    call_dbl_cb = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

expect_true(
  abs(ffi$call_dbl_cb(cb, cb_ptr, 2.5) - 3.0) < 1e-12,
  info = "callback double round-trip: 2.5+0.5=3.0"
)
close_if_valid(cb)

# bool callback
cb <- tcc_callback(function(x) !x, signature = "bool (*)(bool)")
cb_ptr <- tcc_callback_ptr(cb)

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #define _Complex
    bool call_bool_cb(bool (*cb)(void*, bool), void* ctx, bool x) {
      return cb(ctx, x);
    }
  "
  ) |>
  tcc_bind(
    call_bool_cb = list(
      args = list("callback:bool(bool)", "ptr", "bool"),
      returns = "bool"
    )
  ) |>
  tcc_compile()

expect_equal(
  ffi$call_bool_cb(cb, cb_ptr, TRUE),
  FALSE,
  info = "callback bool round-trip: !TRUE=FALSE"
)
expect_equal(
  ffi$call_bool_cb(cb, cb_ptr, FALSE),
  TRUE,
  info = "callback bool round-trip: !FALSE=TRUE"
)
close_if_valid(cb)

# void callback (side-effect only)
side_effect <- 0L
cb <- tcc_callback(
  function(x) {
    side_effect <<- x
    NULL
  },
  signature = "void (*)(int)"
)
cb_ptr <- tcc_callback_ptr(cb)

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #define _Complex
    void call_void_cb(void (*cb)(void*, int), void* ctx, int x) {
      cb(ctx, x);
    }
  "
  ) |>
  tcc_bind(
    call_void_cb = list(
      args = list("callback:void(int)", "ptr", "i32"),
      returns = "void"
    )
  ) |>
  tcc_compile()

ffi$call_void_cb(cb, cb_ptr, 99L)
expect_equal(side_effect, 99L, info = "callback void: side effect executed")
close_if_valid(cb)

# pointer callback
ptr_arg_seen_externalptr <- FALSE
ptr_arg_seen_unowned <- FALSE
cb <- tcc_callback(
  function(x) {
    ptr_arg_seen_externalptr <<- inherits(x, "externalptr")
    ptr_arg_seen_unowned <<- !.Call("RC_ptr_is_owned", x, PACKAGE = "Rtinycc")
    x
  },
  signature = "void* (*)(void*)"
)
cb_ptr <- tcc_callback_ptr(cb)

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #define _Complex
    void* call_ptr_cb(void* (*cb)(void*, void*), void* ctx, void* x) {
      return cb(ctx, x);
    }
  "
  ) |>
  tcc_bind(
    call_ptr_cb = list(
      args = list("callback:void*(void*)", "ptr", "ptr"),
      returns = "ptr"
    )
  ) |>
  tcc_compile()

buf_cb <- tcc_malloc(8)
out_ptr <- ffi$call_ptr_cb(cb, cb_ptr, buf_cb)
expect_true(
  ptr_arg_seen_externalptr,
  info = "callback ptr round-trip: callback receives externalptr"
)
expect_true(
  ptr_arg_seen_unowned,
  info = "callback ptr round-trip: callback receives non-owned wrapper"
)
expect_equal(
  tcc_ptr_addr(out_ptr),
  tcc_ptr_addr(buf_cb),
  info = "callback ptr round-trip: address preserved"
)
expect_false(
  .Call("RC_ptr_is_owned", out_ptr, PACKAGE = "Rtinycc"),
  info = "callback ptr round-trip: returned wrapper is not owned"
)
expect_error(
  tcc_free(out_ptr),
  info = "callback ptr round-trip: explicit free refuses unowned wrapper"
)
tcc_free(buf_cb)
close_if_valid(cb)

# SEXP callback
payload <- list(alpha = 1:3, beta = "ok")
cb <- tcc_callback(function(x) x, signature = "SEXP (*)(SEXP)")
cb_ptr <- tcc_callback_ptr(cb)

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #define _Complex
    SEXP call_sexp_cb(SEXP (*cb)(void*, SEXP), void* ctx, SEXP x) {
      return cb(ctx, x);
    }
  "
  ) |>
  tcc_bind(
    call_sexp_cb = list(
      args = list("callback:SEXP(SEXP)", "ptr", "sexp"),
      returns = "sexp"
    )
  ) |>
  tcc_compile()

expect_identical(
  ffi$call_sexp_cb(cb, cb_ptr, payload),
  payload,
  info = "callback SEXP round-trip: object preserved exactly"
)
close_if_valid(cb)

# async pointer callback
ptr_async_seen_externalptr <- FALSE
ptr_async_seen_unowned <- FALSE
cb <- tcc_callback(
  function(x) {
    ptr_async_seen_externalptr <<- inherits(x, "externalptr")
    ptr_async_seen_unowned <<- !.Call("RC_ptr_is_owned", x, PACKAGE = "Rtinycc")
    x
  },
  signature = "void* (*)(void*)"
)
cb_ptr <- tcc_callback_ptr(cb)

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #define _Complex
    #ifdef _WIN32
    #include <windows.h>
    struct ptask { void* (*cb)(void*,void*); void* ctx; void* in; volatile void* out; volatile int done; };
    static struct ptask g_pt;
    static HANDLE g_pth = NULL;
    static DWORD WINAPI pworker(LPVOID p) {
      struct ptask* t = (struct ptask*) p;
      t->out = t->cb(t->ctx, t->in);
      t->done = 1;
      return 0;
    }
    int start_ptr_worker(void* (*cb)(void*,void*), void* ctx, void* x) {
      g_pt.cb = cb; g_pt.ctx = ctx; g_pt.in = x; g_pt.out = NULL; g_pt.done = 0;
      g_pth = CreateThread(NULL, 0, pworker, &g_pt, 0, NULL);
      return g_pth ? 0 : -1;
    }
    int is_ptr_worker_done(void) { return g_pt.done; }
    int join_ptr_worker(void) {
      if (g_pth) { WaitForSingleObject(g_pth, INFINITE); CloseHandle(g_pth); g_pth = NULL; }
      return g_pt.out == g_pt.in;
    }
    #else
    #include <pthread.h>
    struct ptask { void* (*cb)(void*,void*); void* ctx; void* in; volatile void* out; volatile int done; };
    static struct ptask g_pt;
    static pthread_t g_pth;
    static void* pworker(void* p) {
      struct ptask* t = (struct ptask*) p;
      t->out = t->cb(t->ctx, t->in);
      t->done = 1;
      return NULL;
    }
    int start_ptr_worker(void* (*cb)(void*,void*), void* ctx, void* x) {
      g_pt.cb = cb; g_pt.ctx = ctx; g_pt.in = x; g_pt.out = NULL; g_pt.done = 0;
      return pthread_create(&g_pth, NULL, pworker, &g_pt) == 0 ? 0 : -1;
    }
    int is_ptr_worker_done(void) { return g_pt.done; }
    int join_ptr_worker(void) { pthread_join(g_pth, NULL); return g_pt.out == g_pt.in; }
    #endif
  "
  )
if (.Platform$OS.type != "windows") {
  ffi <- tcc_library(ffi, "pthread")
}
ffi <- ffi |>
  tcc_bind(
    start_ptr_worker = list(
      args = list("callback_async:void*(void*)", "ptr", "ptr"),
      returns = "i32"
    ),
    is_ptr_worker_done = list(args = list(), returns = "i32"),
    join_ptr_worker = list(args = list(), returns = "i32")
  ) |>
  tcc_compile()

buf_async <- tcc_malloc(8)
ffi$start_ptr_worker(cb, cb_ptr, buf_async)
for (i in seq_len(50)) {
  tcc_callback_async_drain()
  if (ffi$is_ptr_worker_done() != 0L) {
    break
  }
  Sys.sleep(0.01)
}
expect_true(
  ptr_async_seen_externalptr,
  info = "async callback ptr: callback receives externalptr"
)
expect_true(
  ptr_async_seen_unowned,
  info = "async callback ptr: callback receives non-owned wrapper"
)
expect_equal(
  ffi$join_ptr_worker(),
  1L,
  info = "async callback ptr: returned native address preserved"
)
tcc_free(buf_async)
close_if_valid(cb)

# Wrapper absence invariants: covered wrapper families must not construct
# external pointers or register finalizers directly in generated code.
wrapper_absence_cases <- list(
  list(
    name = "ptr_identity",
    symbols = list(identity = list(args = list("ptr"), returns = "ptr")),
    c_code = "void* identity(void* x) { return x; }",
    required = list("RC_make_unowned_ptr("),
    forbidden = list("R_MakeExternalPtr(", "R_RegisterCFinalizerEx(")
  ),
  list(
    name = "callback_ptr_wrapper",
    symbols = list(
      call_ptr_cb = list(
        args = list("callback:void*(void*)", "ptr", "ptr"),
        returns = "ptr"
      )
    ),
    c_code = "void* call_ptr_cb(void* (*cb)(void*, void*), void* ctx, void* x) { return cb(ctx, x); }",
    required = list("RC_make_unowned_ptr("),
    forbidden = list("R_MakeExternalPtr(", "R_RegisterCFinalizerEx(")
  ),
  list(
    name = "callback_sexp_wrapper",
    symbols = list(
      call_sexp_cb = list(
        args = list("callback:SEXP(SEXP)", "ptr", "sexp"),
        returns = "sexp"
      )
    ),
    c_code = "SEXP call_sexp_cb(SEXP (*cb)(void*, SEXP), void* ctx, SEXP x) { return cb(ctx, x); }",
    required = list(
      "SEXP trampoline_R_wrap_call_sexp_cb_arg1(void* cb, SEXP arg1)"
    ),
    forbidden = list("R_MakeExternalPtr(", "R_RegisterCFinalizerEx(")
  )
)

for (case in wrapper_absence_cases) {
  code <- Rtinycc:::generate_ffi_code(
    symbols = case$symbols,
    c_code = case$c_code
  )
  for (pattern in case$required) {
    expect_true(
      grepl(pattern, code, fixed = TRUE),
      info = sprintf(
        "wrapper absence invariant (%s): requires %s",
        case$name,
        pattern
      )
    )
  }
  for (pattern in case$forbidden) {
    expect_false(
      grepl(pattern, code, fixed = TRUE),
      info = sprintf(
        "wrapper absence invariant (%s): forbids %s",
        case$name,
        pattern
      )
    )
  }
}

# ==========================================================================
# 7. GC STRESS: force GC between allocations to catch PROTECT bugs
# ==========================================================================

# Generate wrappers for several types, compile, then force GC heavily
# between calls. If PROTECT is wrong, this will crash or corrupt.
ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    #include <string.h>
    int32_t add_i32(int32_t a, int32_t b) { return a + b; }
    double  add_f64(double a, double b) { return a + b; }
    const char* echo_str(const char* s) { return s; }
    int* make_seq(int n) {
      int* out = (int*)malloc(sizeof(int) * n);
      for (int i = 0; i < n; i++) out[i] = i;
      return out;
    }
  "
  ) |>
  tcc_bind(
    add_i32 = list(args = list("i32", "i32"), returns = "i32"),
    add_f64 = list(args = list("f64", "f64"), returns = "f64"),
    echo_str = list(args = list("cstring"), returns = "cstring"),
    make_seq = list(
      args = list("i32"),
      returns = list(type = "integer_array", length_arg = 1, free = TRUE)
    )
  ) |>
  tcc_compile()

gc_stress_ok <- TRUE
for (i in seq_len(100)) {
  # Force GC before each call
  gc(verbose = FALSE)

  r1 <- ffi$add_i32(i, -i)
  if (r1 != 0L) {
    gc_stress_ok <- FALSE
    break
  }

  gc(verbose = FALSE)
  r2 <- ffi$add_f64(as.double(i), -as.double(i))
  if (r2 != 0.0) {
    gc_stress_ok <- FALSE
    break
  }

  gc(verbose = FALSE)
  r3 <- ffi$echo_str(paste0("iter_", i))
  if (r3 != paste0("iter_", i)) {
    gc_stress_ok <- FALSE
    break
  }

  gc(verbose = FALSE)
  r4 <- ffi$make_seq(5L)
  if (!identical(r4, 0:4)) {
    gc_stress_ok <- FALSE
    break
  }
}
expect_true(
  gc_stress_ok,
  info = "GC stress: 100 iterations with forced GC survived"
)

# ===========================================================================
# 8. CODEGEN STRUCTURAL PROPERTIES: single-eval, PROTECT balance
# ===========================================================================

# Property: array return expressions must NOT be evaluated twice
# We test this by using a C function with side effects as the return expr
ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    #include <string.h>
    static int call_count = 0;
    int* counted_alloc(int n) {
      call_count++;
      int* out = (int*)malloc(sizeof(int) * n);
      for (int i = 0; i < n; i++) out[i] = call_count;
      return out;
    }
    int get_call_count(void) { return call_count; }
  "
  ) |>
  tcc_bind(
    counted_alloc = list(
      args = list("i32"),
      returns = list(type = "integer_array", length_arg = 1, free = TRUE)
    ),
    get_call_count = list(args = list(), returns = "i32")
  ) |>
  tcc_compile()

result <- ffi$counted_alloc(3L)
count_after <- ffi$get_call_count()
expect_equal(
  count_after,
  1L,
  info = "array return: C function called exactly once (no double-eval)"
)
expect_equal(result, rep(1L, 3), info = "array return: values from single call")

# Call again to confirm counter increments properly
result2 <- ffi$counted_alloc(2L)
count_after2 <- ffi$get_call_count()
expect_equal(
  count_after2,
  2L,
  info = "array return: second call increments counter once"
)

# Property: scalar return with side effects also single-eval
ffi <- tcc_ffi() |>
  tcc_source(
    "
    static int scalar_calls = 0;
    int64_t counted_i64(void) {
      scalar_calls++;
      return (int64_t)scalar_calls;
    }
    int get_scalar_calls(void) { return scalar_calls; }
  "
  ) |>
  tcc_bind(
    counted_i64 = list(args = list(), returns = "i64"),
    get_scalar_calls = list(args = list(), returns = "i32")
  ) |>
  tcc_compile()

r <- ffi$counted_i64()
expect_equal(
  ffi$get_scalar_calls(),
  1L,
  info = "i64 return: C function called exactly once"
)
expect_equal(r, 1.0, info = "i64 return: correct value from single eval")

# cstring return single-eval
ffi <- tcc_ffi() |>
  tcc_source(
    "
    static int str_calls = 0;
    const char* counted_str(void) {
      str_calls++;
      return \"hello\";
    }
    int get_str_calls(void) { return str_calls; }
  "
  ) |>
  tcc_bind(
    counted_str = list(args = list(), returns = "cstring"),
    get_str_calls = list(args = list(), returns = "i32")
  ) |>
  tcc_compile()

r <- ffi$counted_str()
expect_equal(
  ffi$get_str_calls(),
  1L,
  info = "cstring return: C function called exactly once"
)
expect_equal(r, "hello", info = "cstring return: correct value")

# Regression: const-qualified char* returns must compile and box correctly.
ffi <- tcc_ffi() |>
  tcc_source(
    "
    const char* const_message(void) {
      return \"const-ok\";
    }
  "
  ) |>
  tcc_bind(const_message = list(args = list(), returns = "cstring")) |>
  tcc_compile()
expect_equal(
  ffi$const_message(),
  "const-ok",
  info = "const char* return compiles and boxes into an R string"
)

# wrapper argument single-eval
ffi <- tcc_ffi() |>
  tcc_source("int add3(int a, int b, int c) { return a + b + c; }") |>
  tcc_bind(
    add3 = list(args = list("i32", "i32", "i32"), returns = "i32")
  ) |>
  tcc_compile()

arg_force_count <- 0L
mk_arg <- function(value) {
  force(value)
  function() {
    arg_force_count <<- arg_force_count + 1L
    value
  }
}
arg1 <- mk_arg(1L)
arg2 <- mk_arg(2L)
arg3 <- mk_arg(3L)
expect_equal(
  ffi$add3(arg1(), arg2(), arg3()),
  6L,
  info = "wrapper arg single-eval: correct result"
)
expect_equal(
  arg_force_count,
  3L,
  info = "wrapper arg single-eval: each R argument forced once"
)

# ===========================================================================
# 9. CSTRING_ARRAY INPUT: verify element-by-element translation
# ===========================================================================

ffi <- tcc_ffi() |>
  tcc_source(
    "
    #include <string.h>
    int find_str(const char** arr, int n, const char* needle) {
      for (int i = 0; i < n; i++) {
        if (arr[i] && strcmp(arr[i], needle) == 0) return i;
      }
      return -1;
    }
  "
  ) |>
  tcc_bind(
    find_str = list(
      args = list("cstring_array", "i32", "cstring"),
      returns = "i32"
    )
  ) |>
  tcc_compile()

haystack <- c("alpha", "beta", "gamma", "delta")
expect_equal(
  ffi$find_str(haystack, 4L, "gamma"),
  2L,
  info = "cstring_array: finds element at index 2"
)
expect_equal(
  ffi$find_str(haystack, 4L, "missing"),
  -1L,
  info = "cstring_array: returns -1 for missing"
)
expect_equal(
  ffi$find_str(haystack, 4L, "alpha"),
  0L,
  info = "cstring_array: finds first element"
)

# ===========================================================================
# 10. ENUM ROUND-TRIP
# ===========================================================================

ffi <- tcc_ffi() |>
  tcc_source(
    "
    enum color { RED = 0, GREEN = 1, BLUE = 2 };
    enum color next_color(enum color c) {
      return (enum color)((c + 1) % 3);
    }
  "
  ) |>
  tcc_enum("color", constants = c("RED", "GREEN", "BLUE")) |>
  tcc_bind(
    next_color = list(
      args = list("enum:color"),
      returns = "enum:color"
    )
  ) |>
  tcc_compile()

expect_equal(ffi$next_color(0L), 1L, info = "enum round-trip: RED->GREEN")
expect_equal(ffi$next_color(1L), 2L, info = "enum round-trip: GREEN->BLUE")
expect_equal(ffi$next_color(2L), 0L, info = "enum round-trip: BLUE->RED")

# Verify enum constant getters
expect_equal(ffi$enum_color_RED(), 0L, info = "enum constant: RED=0")
expect_equal(ffi$enum_color_GREEN(), 1L, info = "enum constant: GREEN=1")
expect_equal(ffi$enum_color_BLUE(), 2L, info = "enum constant: BLUE=2")

# ===========================================================================
# 11. GLOBAL VARIABLE ROUND-TRIPS
# ===========================================================================

ffi <- tcc_ffi() |>
  tcc_source(
    "
    int32_t g_counter = 0;
    double  g_value = 0.0;
  "
  ) |>
  tcc_global("g_counter", "i32") |>
  tcc_global("g_value", "f64") |>
  tcc_compile()

# Initial values
expect_equal(
  ffi$global_g_counter_get(),
  0L,
  info = "global i32: initial value is 0"
)
expect_equal(
  ffi$global_g_value_get(),
  0.0,
  info = "global f64: initial value is 0.0"
)

# Set and get
ffi$global_g_counter_set(42L)
expect_equal(
  ffi$global_g_counter_get(),
  42L,
  info = "global i32 round-trip: set 42, get 42"
)

ffi$global_g_value_set(3.14)
expect_equal(
  ffi$global_g_value_get(),
  3.14,
  info = "global f64 round-trip: set 3.14, get 3.14"
)
