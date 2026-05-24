# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Canonical FFI semantics matrix.
#
# This is the internal source of truth for:
# - borrow vs copy behavior at the R/C boundary
# - ownership expectations for values crossing the wrapper
# - scalar codegen rule templates
# - generated soundness cases for scalar round-trip tests
#
# The matrix is intentionally explicit because copy and protection behavior is
# part of the package contract, not just an implementation detail.

RTINYCC_DOUBLE_INT_EXACT_LIMIT <- 9007199254740992

rtinycc_render_template <- function(lines, values) {
  rendered <- as.character(lines)

  for (name in names(values)) {
    rendered <- gsub(
      paste0("{{", name, "}}"),
      values[[name]],
      rendered,
      fixed = TRUE
    )
  }

  paste(rendered, collapse = "\n")
}

rtinycc_ffi_semantics <- function() {
  RTINYCC_FFI_SEMANTICS
}

rtinycc_callback_semantics <- function() {
  RTINYCC_CALLBACK_SEMANTICS
}

rtinycc_callback_abi_specs <- function() {
  RTINYCC_CALLBACK_ABI_SPECS
}

rtinycc_composite_semantics <- function() {
  RTINYCC_COMPOSITE_SEMANTICS
}

rtinycc_composite_codegen_specs <- function() {
  RTINYCC_COMPOSITE_CODEGEN_SPECS
}

rtinycc_soundness_case <- function(label, value, expected = NULL) {
  list(label = label, value = value, expected = expected)
}

RTINYCC_FFI_SEMANTICS <- list(
  i8 = list(
    ffi_type = "i8",
    c_type = "int8_t",
    kind = "scalar",
    input = list(
      mode = "copy_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "integer",
      checks = c("NA", "range"),
      notes = "Copied/coerced into a local int8_t before the C call."
    ),
    return = list(
      mode = "box_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as a fresh scalar integer SEXP."
    ),
    codegen = list(
      input_form = "integer_range",
      input_range_min = "INT8_MIN",
      input_range_max = "INT8_MAX",
      input_range_error = "i8 out of range",
      return_form = "scalar_integer_cast"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "int8_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0L)),
        rtinycc_soundness_case("max", quote(127L)),
        rtinycc_soundness_case("min", quote(-128L)),
        rtinycc_soundness_case("typical", quote(42L))
      ),
      reject = list(
        rtinycc_soundness_case("NA_integer_", quote(NA_integer_)),
        rtinycc_soundness_case("128 (> INT8_MAX)", quote(128L)),
        rtinycc_soundness_case("-129 (< INT8_MIN)", quote(-129L))
      )
    )
  ),
  i16 = list(
    ffi_type = "i16",
    c_type = "int16_t",
    kind = "scalar",
    input = list(
      mode = "copy_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "integer",
      checks = c("NA", "range"),
      notes = "Copied/coerced into a local int16_t before the C call."
    ),
    return = list(
      mode = "box_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as a fresh scalar integer SEXP."
    ),
    codegen = list(
      input_form = "integer_range",
      input_range_min = "INT16_MIN",
      input_range_max = "INT16_MAX",
      input_range_error = "i16 out of range",
      return_form = "scalar_integer_cast"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "int16_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0L)),
        rtinycc_soundness_case("max", quote(32767L)),
        rtinycc_soundness_case("min", quote(-32768L))
      ),
      reject = list(
        rtinycc_soundness_case("32768 (> INT16_MAX)", quote(32768L)),
        rtinycc_soundness_case("-32769 (< INT16_MIN)", quote(-32769L))
      )
    )
  ),
  i32 = list(
    ffi_type = "i32",
    c_type = "int32_t",
    kind = "scalar",
    input = list(
      mode = "copy_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "integer",
      checks = c("NA", "range"),
      notes = "Copied/coerced into a local int32_t before the C call."
    ),
    return = list(
      mode = "box_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as a fresh scalar integer SEXP."
    ),
    codegen = list(
      input_form = "integer_range",
      input_range_min = "INT32_MIN",
      input_range_max = "INT32_MAX",
      input_range_error = "i32 out of range",
      return_form = "scalar_integer"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "int32_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0L)),
        rtinycc_soundness_case("INT_MAX", quote(.Machine$integer.max)),
        rtinycc_soundness_case("-INT_MAX", quote(-.Machine$integer.max))
      ),
      reject = list(
        rtinycc_soundness_case("NA_integer_", quote(NA_integer_))
      )
    )
  ),
  i64 = list(
    ffi_type = "i64",
    c_type = "int64_t",
    kind = "scalar",
    input = list(
      mode = "copy_exact_integer_numeric",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "numeric",
      checks = c("NA", "exact-integer", "range", "2^53-limit"),
      exact_integer_limit = RTINYCC_DOUBLE_INT_EXACT_LIMIT,
      notes = "Copied from R numeric into a local int64_t after exactness checks."
    ),
    return = list(
      mode = "box_numeric_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      exact_integer_limit = RTINYCC_DOUBLE_INT_EXACT_LIMIT,
      notes = "Returned as R numeric with a warning once precision exceeds exact double integer range."
    ),
    codegen = list(
      input_form = "exact_integer_real",
      input_exact_error = "i64 requires exact integer (|x| <= 2^53)",
      input_integer_error = "i64 requires integer value",
      input_range_min = "INT64_MIN",
      input_range_max = "INT64_MAX",
      input_range_error = "i64 out of range",
      return_form = "i64"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "int64_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0)),
        rtinycc_soundness_case("one", quote(1)),
        rtinycc_soundness_case("neg one", quote(-1)),
        rtinycc_soundness_case("2^53", quote(2^53)),
        rtinycc_soundness_case("-2^53", quote(-2^53))
      ),
      reject = list(
        rtinycc_soundness_case("NA_real_", quote(NA_real_)),
        rtinycc_soundness_case("Inf", quote(Inf)),
        rtinycc_soundness_case("non-integer", quote(1.5))
      )
    )
  ),
  u8 = list(
    ffi_type = "u8",
    c_type = "uint8_t",
    kind = "scalar",
    input = list(
      mode = "copy_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "integer",
      checks = c("NA", "range"),
      notes = "Copied/coerced into a local uint8_t before the C call."
    ),
    return = list(
      mode = "box_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as a fresh scalar integer SEXP."
    ),
    codegen = list(
      input_form = "integer_range",
      input_range_min = "0",
      input_range_max = "UINT8_MAX",
      input_range_error = "u8 out of range",
      return_form = "scalar_integer_cast"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "uint8_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0L)),
        rtinycc_soundness_case("max", quote(255L)),
        rtinycc_soundness_case("one", quote(1L))
      ),
      reject = list(
        rtinycc_soundness_case("-1", quote(-1L)),
        rtinycc_soundness_case("256 (> UINT8_MAX)", quote(256L))
      )
    )
  ),
  u16 = list(
    ffi_type = "u16",
    c_type = "uint16_t",
    kind = "scalar",
    input = list(
      mode = "copy_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "integer",
      checks = c("NA", "range"),
      notes = "Copied/coerced into a local uint16_t before the C call."
    ),
    return = list(
      mode = "box_integer_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as a fresh scalar integer SEXP."
    ),
    codegen = list(
      input_form = "integer_range",
      input_range_min = "0",
      input_range_max = "UINT16_MAX",
      input_range_error = "u16 out of range",
      return_form = "scalar_integer_cast"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "uint16_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0L)),
        rtinycc_soundness_case("max", quote(65535L))
      ),
      reject = list(
        rtinycc_soundness_case("-1", quote(-1L)),
        rtinycc_soundness_case("65536 (> UINT16_MAX)", quote(65536L))
      )
    )
  ),
  u32 = list(
    ffi_type = "u32",
    c_type = "uint32_t",
    kind = "scalar",
    input = list(
      mode = "copy_integer_numeric",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "numeric",
      checks = c("NA", "non-negative", "integer-valued", "range"),
      notes = "Copied from R numeric into a local uint32_t after range checks."
    ),
    return = list(
      mode = "box_numeric_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as R numeric because values can exceed INT_MAX."
    ),
    codegen = list(
      input_form = "real_integer_range",
      input_range_min = "0",
      input_range_max = "(double)UINT32_MAX",
      input_range_error = "u32 out of range",
      input_integer_error = "u32 requires integer value",
      return_form = "scalar_real_cast"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "uint32_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0)),
        rtinycc_soundness_case("UINT32_MAX", quote(4294967295)),
        rtinycc_soundness_case("one", quote(1))
      ),
      reject = list(
        rtinycc_soundness_case("-1", quote(-1)),
        rtinycc_soundness_case("non-integer", quote(1.5))
      )
    )
  ),
  u64 = list(
    ffi_type = "u64",
    c_type = "uint64_t",
    kind = "scalar",
    input = list(
      mode = "copy_exact_integer_numeric",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "numeric",
      checks = c("NA", "non-negative", "exact-integer", "2^53-limit"),
      exact_integer_limit = RTINYCC_DOUBLE_INT_EXACT_LIMIT,
      notes = "Copied from R numeric into a local uint64_t after exactness checks."
    ),
    return = list(
      mode = "box_numeric_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      exact_integer_limit = RTINYCC_DOUBLE_INT_EXACT_LIMIT,
      notes = "Returned as R numeric with a warning once precision exceeds exact double integer range."
    ),
    codegen = list(
      input_form = "exact_integer_real",
      input_nonnegative = TRUE,
      input_nonnegative_error = "u64 out of range",
      input_exact_error = "u64 requires exact integer (|x| <= 2^53)",
      input_integer_error = "u64 requires integer value",
      return_form = "u64"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "uint64_t"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0)),
        rtinycc_soundness_case("one", quote(1)),
        rtinycc_soundness_case("2^53", quote(2^53))
      ),
      reject = list(
        rtinycc_soundness_case("-1", quote(-1)),
        rtinycc_soundness_case("NA_real_", quote(NA_real_))
      )
    )
  ),
  f32 = list(
    ffi_type = "f32",
    c_type = "float",
    kind = "scalar",
    input = list(
      mode = "copy_numeric_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "numeric",
      checks = character(),
      notes = "Copied/coerced into a local float before the C call."
    ),
    return = list(
      mode = "box_numeric_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as R numeric after float to double widening."
    ),
    codegen = list(
      input_form = "real_scalar_cast",
      return_form = "scalar_real_cast"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "float"),
      comparator = "tolerance",
      tolerance = 1e-5,
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0.0)),
        rtinycc_soundness_case("pi approx (f32 precision)", quote(3.14)),
        rtinycc_soundness_case("neg one", quote(-1.0))
      ),
      reject = list()
    )
  ),
  f64 = list(
    ffi_type = "f64",
    c_type = "double",
    kind = "scalar",
    input = list(
      mode = "copy_numeric_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "numeric",
      checks = character(),
      notes = "Copied/coerced into a local double before the C call."
    ),
    return = list(
      mode = "box_numeric_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as a fresh scalar real SEXP."
    ),
    codegen = list(
      input_form = "real_scalar",
      return_form = "scalar_real"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(c_type = "double"),
      roundtrip = list(
        rtinycc_soundness_case("zero", quote(0.0)),
        rtinycc_soundness_case("pi", quote(3.141592653589793)),
        rtinycc_soundness_case("large neg", quote(-1e308)),
        rtinycc_soundness_case("tiny pos", quote(1e-308))
      ),
      reject = list()
    )
  ),
  bool = list(
    ffi_type = "bool",
    c_type = "bool",
    kind = "scalar",
    input = list(
      mode = "copy_logical_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "wrapper-local",
      r_storage = "logical",
      checks = "NA",
      notes = "Copied/coerced into a local bool before the C call."
    ),
    return = list(
      mode = "box_logical_scalar",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Returned as a fresh scalar logical SEXP."
    ),
    codegen = list(
      input_form = "logical_scalar",
      return_form = "logical"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(
        c_type = "bool",
        source = "#include <stdbool.h>\nbool identity(bool x) { return x; }"
      ),
      roundtrip = list(
        rtinycc_soundness_case("TRUE", quote(TRUE)),
        rtinycc_soundness_case("FALSE", quote(FALSE))
      ),
      reject = list(
        rtinycc_soundness_case("NA", quote(NA))
      )
    )
  ),
  cstring = list(
    ffi_type = "cstring",
    c_type = "char*",
    kind = "scalar",
    input = list(
      mode = "borrow_utf8_pointer",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R/internal translation buffer",
      r_storage = "character",
      checks = character(),
      notes = "Borrowed UTF-8 pointer from Rf_translateCharUTF8() for the duration of the call."
    ),
    return = list(
      mode = "copy_string",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Copied into R-managed string memory with mkString()."
    ),
    codegen = list(
      input_form = "cstring",
      return_form = "cstring"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(
        c_type = "const char*",
        source = "const char* identity(const char* x) { return x; }"
      ),
      roundtrip = list(
        rtinycc_soundness_case("basic", quote("hello")),
        rtinycc_soundness_case("empty", quote("")),
        rtinycc_soundness_case("special chars", quote("hello world 123!@#"))
      ),
      reject = list()
    )
  ),
  ptr = list(
    ffi_type = "ptr",
    c_type = "void*",
    kind = "scalar",
    input = list(
      mode = "borrow_externalptr",
      borrow = TRUE,
      copy = FALSE,
      ownership = "caller-defined",
      r_storage = "externalptr",
      checks = character(),
      notes = "Wrapper passes the external pointer address through without copying pointee storage."
    ),
    return = list(
      mode = "external_pointer",
      borrow = TRUE,
      copy = FALSE,
      ownership = "unchanged",
      notes = "Returned as a raw external pointer; no pointee copy or ownership transfer is implied."
    ),
    codegen = list(
      input_form = "ptr",
      return_form = "ptr"
    )
  ),
  raw = list(
    ffi_type = "raw",
    c_type = "uint8_t*",
    kind = "array",
    input = list(
      mode = "borrow_vector_storage",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R",
      r_storage = "raw",
      checks = character(),
      notes = paste(
        "Mutable pointer input through RAW(x); for ordinary materialized vectors",
        "no extra buffer is allocated. ALTREP vectors follow R's writable pointer",
        "materialization path. ALTREP-specific read-only or temp-buffer behavior",
        "needs a separate access-mode contract because this type permits mutation",
        "and pointer aliasing."
      )
    ),
    return = list(
      mode = "copy_array",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Array returns allocate a fresh raw vector and memcpy() the C buffer into it."
    )
  ),
  integer_array = list(
    ffi_type = "integer_array",
    c_type = "int32_t*",
    kind = "array",
    input = list(
      mode = "borrow_vector_storage",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R",
      r_storage = "integer",
      checks = character(),
      notes = paste(
        "Mutable pointer input through INTEGER(x); for ordinary materialized",
        "vectors no extra buffer is allocated. ALTREP vectors follow R's writable",
        "pointer materialization path. ALTREP-specific read-only or temp-buffer",
        "behavior needs a separate access-mode contract because this type permits",
        "mutation and pointer aliasing."
      )
    ),
    return = list(
      mode = "copy_array",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Array returns allocate a fresh integer vector and memcpy() the C buffer into it."
    )
  ),
  numeric_array = list(
    ffi_type = "numeric_array",
    c_type = "double*",
    kind = "array",
    input = list(
      mode = "borrow_vector_storage",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R",
      r_storage = "numeric",
      checks = character(),
      notes = paste(
        "Mutable pointer input through REAL(x); for ordinary materialized vectors",
        "no extra buffer is allocated. ALTREP vectors follow R's writable pointer",
        "materialization path. ALTREP-specific read-only or temp-buffer behavior",
        "needs a separate access-mode contract because this type permits mutation",
        "and pointer aliasing."
      )
    ),
    return = list(
      mode = "copy_array",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Array returns allocate a fresh numeric vector and memcpy() the C buffer into it."
    )
  ),
  logical_array = list(
    ffi_type = "logical_array",
    c_type = "int*",
    kind = "array",
    input = list(
      mode = "borrow_vector_storage",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R",
      r_storage = "logical",
      checks = character(),
      notes = paste(
        "Mutable pointer input through LOGICAL(x); for ordinary materialized",
        "vectors no extra buffer is allocated. ALTREP vectors follow R's writable",
        "pointer materialization path. ALTREP-specific read-only or temp-buffer",
        "behavior needs a separate access-mode contract because this type permits",
        "mutation and pointer aliasing."
      )
    ),
    return = list(
      mode = "copy_array",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R",
      notes = "Array returns allocate a fresh logical vector and memcpy() the C buffer into it."
    )
  ),
  character_array = list(
    ffi_type = "character_array",
    c_type = "const SEXP*",
    kind = "array",
    input = list(
      mode = "borrow_string_cells",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R",
      r_storage = "character",
      checks = character(),
      notes = paste(
        "Exposes STRING_PTR_RO(x), whose C type in R is const SEXP*;",
        "the pointed elements are CHARSXP string cells rather than char** C strings."
      )
    ),
    return = NULL
  ),
  cstring_array = list(
    ffi_type = "cstring_array",
    c_type = "const char**",
    kind = "array",
    input = list(
      mode = "copy_pointer_array",
      borrow = FALSE,
      copy = TRUE,
      ownership = "R_alloc-temporary",
      r_storage = "character",
      checks = "character",
      notes = "Allocates a temporary const char** with R_alloc() and fills it from translated string payloads; pointer array is copied, string payloads are borrowed."
    ),
    return = NULL
  ),
  sexp = list(
    ffi_type = "sexp",
    c_type = "SEXP",
    kind = "scalar",
    input = list(
      mode = "passthrough",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R",
      r_storage = "ANY",
      checks = character(),
      notes = "Least transformed boundary mode: wrapper passes the SEXP through directly."
    ),
    return = list(
      mode = "passthrough",
      borrow = TRUE,
      copy = FALSE,
      ownership = "R",
      notes = "Returned directly as a SEXP."
    ),
    codegen = list(
      input_form = "sexp",
      return_form = "sexp"
    ),
    soundness = list(
      generated = TRUE,
      identity = list(
        c_type = "SEXP",
        source = "SEXP identity(SEXP x) { return x; }"
      ),
      roundtrip = list(
        rtinycc_soundness_case("integer", quote(42L)),
        rtinycc_soundness_case("string", quote("hello")),
        rtinycc_soundness_case("double", quote(3.14))
      ),
      reject = list()
    )
  ),
  void = list(
    ffi_type = "void",
    c_type = "void",
    kind = "scalar",
    input = NULL,
    return = list(
      mode = "side-effect-only",
      borrow = FALSE,
      copy = FALSE,
      ownership = "none",
      notes = "Wrapper evaluates the C expression for side effects and returns R NULL."
    ),
    codegen = list(
      return_form = "void"
    )
  ),
  callback = list(
    ffi_type = "callback",
    c_type = "void*",
    kind = "scalar",
    input = list(
      mode = "borrow_callback_pointer",
      borrow = TRUE,
      copy = FALSE,
      ownership = "callback-registry",
      r_storage = "externalptr/tcc_callback",
      checks = character(),
      notes = "Wrapper passes the trampoline/callback pointer through; callback lifetime is managed separately by the registry and preserved R function."
    ),
    return = NULL,
    codegen = list(
      input_form = "ptr"
    )
  )
)

RTINYCC_CALLBACK_SEMANTICS <- list(
  sync = list(
    kind = "callback_sync",
    preserves_function = TRUE,
    preservation_api = "R_PreserveObject",
    invocation_thread = "main",
    argument_transport = "stack-to-VECSXP",
    copies = list(
      scalars = TRUE,
      cstring_payload = FALSE,
      pointer_addresses = FALSE
    ),
    borrows = list(
      callback_token = TRUE,
      pointer_pointee = TRUE,
      cstring_payload = TRUE
    ),
    returns = list(
      default_on_error = "NA-like scalar or NULL pointer",
      mode = "converted_back_to_declared_c_type"
    ),
    notes = paste(
      "Synchronous trampolines allocate a VECSXP argument list on the main",
      "thread, invoke the preserved R function directly, then convert the",
      "SEXP result back to the declared C type."
    )
  ),
  async = list(
    kind = "callback_async",
    preserves_function = TRUE,
    preservation_api = "R_PreserveObject",
    invocation_thread = "scheduled-on-main",
    scheduler = c(
      "RC_callback_async_schedule_c",
      "RC_callback_async_schedule_sync_c"
    ),
    copies = list(
      scalar_values = TRUE,
      cstring_payload = TRUE,
      pointer_addresses = TRUE
    ),
    borrows = list(
      callback_token = TRUE,
      pointer_pointee = TRUE
    ),
    returns = list(
      default_on_error = "NA-like scalar or NULL pointer",
      nonvoid_mode = "sync-result-channel"
    ),
    notes = paste(
      "Async callbacks marshal arguments into cb_arg_t task payloads,",
      "duplicate cstring payloads for cross-thread safety, and reconstruct",
      "fresh R objects on the main thread before invocation."
    )
  )
)

RTINYCC_CALLBACK_ABI_SPECS <- list(
  trampoline = list(
    list(
      name = "tramp_i8",
      signature = "int8_t (*)(int8_t)",
      pattern = "int8_t tramp_i8\\(void\\* cb, int8_t arg1\\)",
      info = "Trampoline keeps int8_t argument ABI"
    ),
    list(
      name = "tramp_i16",
      signature = "int16_t (*)(int16_t)",
      pattern = "int16_t tramp_i16\\(void\\* cb, int16_t arg1\\)",
      info = "Trampoline keeps int16_t argument ABI"
    ),
    list(
      name = "tramp_f32",
      signature = "float (*)(float)",
      pattern = "float tramp_f32\\(void\\* cb, float arg1\\)",
      info = "Trampoline keeps float argument ABI"
    ),
    list(
      name = "tramp_bool_arg",
      signature = "bool (*)(bool)",
      pattern = "bool tramp_bool_arg\\(void\\* cb, bool arg1\\)",
      info = "Trampoline keeps bool argument ABI"
    ),
    list(
      name = "tramp_ptrptr_arg",
      signature = "void (*)(char **)",
      pattern = "void tramp_ptrptr_arg\\(void\\* cb, char \\*\\* arg1\\)",
      info = "Trampoline keeps pointer subtype in argument ABI"
    )
  ),
  async_trampoline = list(
    list(
      name = "tramp_async_f32",
      signature = "callback_async:f32(f32)",
      patterns = list(
        list(
          pattern = "float tramp_async_f32\\(void\\* cb, float arg1\\)",
          info = "Async trampoline normalizes f32 to float in the C signature"
        ),
        list(
          pattern = "return \\(float\\)result.v.d;",
          info = "Async trampoline normalizes f32 return casts to float"
        )
      )
    ),
    list(
      name = "tramp_async_ptr",
      signature = "callback_async:void*(void*)",
      patterns = list(
        list(
          pattern = "void\\* tramp_async_ptr\\(void\\* cb, void\\* arg1\\)",
          info = "Async pointer trampoline preserves pointer ABI"
        ),
        list(
          pattern = "args\\[0\\]\\.kind = CB_ARG_PTR;",
          info = "Async pointer trampoline stores pointer args in CB_ARG_PTR slot"
        ),
        list(
          pattern = "args\\[0\\]\\.v\\.p = arg1;",
          info = "Async pointer trampoline stores raw pointer value without pointee copy"
        ),
        list(
          pattern = "return \\(void\\*\\)result.v.p;",
          info = "Async pointer trampoline returns pointer results through cb_result_t pointer slot"
        )
      ),
      forbidden = list(
        list(
          pattern = "SET_VECTOR_ELT\\(args, 0, arg1\\);",
          info = "Async pointer trampoline does not treat pointer args as raw SEXP pass-through"
        ),
        list(
          pattern = "return result;",
          info = "Async pointer trampoline does not return a raw SEXP result"
        ),
        list(
          pattern = "R_ExternalPtrAddr\\(result\\)",
          info = "Async pointer trampoline does not use sync-callback external-pointer extraction"
        )
      )
    )
  ),
  wrapper = list(
    list(
      name = "call_bool_cb",
      args = list("callback:bool(bool)", "ptr", "bool"),
      returns = "bool",
      c_code = "
      bool call_bool_cb(bool (*cb)(void*, bool), void* ctx, bool x) {
        return cb(ctx, x);
      }
    ",
      patterns = list(
        list(
          pattern = "bool trampoline_R_wrap_call_bool_cb_arg1\\(void\\* cb, bool arg1\\)",
          info = "Bool callback wrapper preserves bool ABI"
        ),
        list(
          pattern = "bool \\(\\*arg1\\)\\(void\\*, bool\\) = trampoline_R_wrap_call_bool_cb_arg1;",
          info = "Bool callback wrapper declaration matches trampoline ABI"
        )
      )
    ),
    list(
      name = "call_i8_cb",
      args = list("callback:int8_t(int8_t)", "ptr", "i8"),
      returns = "i8",
      c_code = "
      int8_t call_i8_cb(int8_t (*cb)(void*, int8_t), void* ctx, int8_t x) {
        return cb(ctx, x);
      }
    ",
      patterns = list(
        list(
          pattern = "int8_t trampoline_R_wrap_call_i8_cb_arg1\\(void\\* cb, int8_t arg1\\)",
          info = "int8_t callback wrapper preserves narrow integer ABI"
        ),
        list(
          pattern = "int8_t \\(\\*arg1\\)\\(void\\*, int8_t\\) = trampoline_R_wrap_call_i8_cb_arg1;",
          info = "int8_t callback wrapper declaration matches trampoline ABI"
        )
      )
    ),
    list(
      name = "call_f32_cb",
      args = list("callback:float(float)", "ptr", "f32"),
      returns = "f32",
      c_code = "
      float call_f32_cb(float (*cb)(void*, float), void* ctx, float x) {
        return cb(ctx, x);
      }
    ",
      patterns = list(
        list(
          pattern = "float trampoline_R_wrap_call_f32_cb_arg1\\(void\\* cb, float arg1\\)",
          info = "float callback wrapper preserves float ABI"
        ),
        list(
          pattern = "float \\(\\*arg1\\)\\(void\\*, float\\) = trampoline_R_wrap_call_f32_cb_arg1;",
          info = "float callback wrapper declaration matches trampoline ABI"
        )
      )
    ),
    list(
      name = "call_ptr_cb",
      args = list("callback:void*(void*)", "ptr", "ptr"),
      returns = "ptr",
      c_code = "
      void* call_ptr_cb(void* (*cb)(void*, void*), void* ctx, void* x) {
        return cb(ctx, x);
      }
    ",
      patterns = list(
        list(
          pattern = "SET_VECTOR_ELT\\(args, 0, RC_make_unowned_ptr\\(arg1, R_NilValue\\)\\);",
          info = "Pointer callback trampoline boxes pointer args through host unowned helper"
        ),
        list(
          pattern = "return R_ExternalPtrAddr\\(result\\);",
          info = "Pointer callback trampoline converts R result back to native address"
        ),
        list(
          pattern = "return RC_make_unowned_ptr\\(call_ptr_cb\\(arg1, arg2, arg3\\), R_NilValue\\);",
          info = "Pointer callback wrapper return boxes raw pointers through host unowned helper"
        )
      ),
      forbidden = list(
        list(
          pattern = "R_MakeExternalPtr",
          info = "Pointer callback codegen avoids direct external pointer construction"
        )
      )
    ),
    list(
      name = "call_sexp_cb",
      args = list("callback:SEXP(SEXP)", "ptr", "sexp"),
      returns = "sexp",
      c_code = "
      SEXP call_sexp_cb(SEXP (*cb)(void*, SEXP), void* ctx, SEXP x) {
        return cb(ctx, x);
      }
    ",
      patterns = list(
        list(
          pattern = "SEXP trampoline_R_wrap_call_sexp_cb_arg1\\(void\\* cb, SEXP arg1\\)",
          info = "SEXP callback trampoline preserves SEXP ABI"
        ),
        list(
          pattern = "SET_VECTOR_ELT\\(args, 0, arg1\\);",
          info = "SEXP callback trampoline passes SEXP arguments through directly"
        ),
        list(
          pattern = "return result;",
          info = "SEXP callback trampoline returns the callback result SEXP directly"
        ),
        list(
          pattern = "SEXP \\(\\*arg1\\)\\(void\\*, SEXP\\) = trampoline_R_wrap_call_sexp_cb_arg1;",
          info = "SEXP callback wrapper declaration matches trampoline ABI"
        )
      ),
      forbidden = list(
        list(
          pattern = "RC_make_unowned_ptr\\(arg1, R_NilValue\\)",
          info = "SEXP callback trampoline does not pointer-box SEXP arguments"
        ),
        list(
          pattern = "R_ExternalPtrAddr\\(result\\)",
          info = "SEXP callback trampoline does not coerce callback results through external-pointer reads"
        )
      )
    )
  )
)

RTINYCC_COMPOSITE_SEMANTICS <- list(
  struct_owned = list(
    kind = "struct",
    helper = "tcc_struct",
    borrow = FALSE,
    copy = FALSE,
    ownership = "owned-native-storage",
    finalizer = TRUE,
    notes = paste(
      "Struct constructors allocate owned native storage and return an",
      "external pointer tagged with the struct type and a finalizer."
    )
  ),
  struct_field_addr = list(
    kind = "struct_view",
    helper = "tcc_field_addr",
    borrow = TRUE,
    copy = FALSE,
    ownership = "borrowed-from-struct",
    protects_owner = TRUE,
    lifetime_model = "preserved owner slot",
    survives_gc_with_live_view = TRUE,
    notes = paste(
      "Field-address helpers return borrowed external pointers, preserve the",
      "owner struct, and keep it in the protected slot so storage stays alive."
    )
  ),
  struct_container_of = list(
    kind = "struct_view",
    helper = "tcc_container_of",
    borrow = TRUE,
    copy = FALSE,
    ownership = "borrowed-from-member-owner-chain",
    protects_owner = TRUE,
    lifetime_model = "preserved owner chain",
    survives_gc_with_live_view = TRUE,
    notes = paste(
      "container_of recovers a parent struct pointer from a field pointer,",
      "preserves the incoming external pointer, and keeps it in the protected",
      "slot so the owner lifetime chain remains intact."
    )
  ),
  struct_raw_access = list(
    kind = "struct_raw_access",
    helper = "tcc_struct_raw_access",
    read_copy = TRUE,
    write_copy = TRUE,
    ownership = "struct-owned-storage",
    notes = paste(
      "Raw struct helpers copy bytes out to a fresh RAWSXP with memcpy() and",
      "copy bytes from a RAWSXP back into the struct buffer with RAW_GET_REGION(),",
      "which avoids asking R for a writable raw-vector data pointer on copy-in."
    )
  ),
  struct_array_field = list(
    kind = "struct_array_field",
    helper = "tcc_struct",
    borrow = FALSE,
    copy = TRUE,
    ownership = "struct-owned-storage",
    notes = paste(
      "Struct array field element helpers copy scalar elements between R and",
      "the native struct buffer; the struct storage itself remains owned by",
      "the struct external pointer."
    )
  ),
  union_owned = list(
    kind = "union",
    helper = "tcc_union",
    borrow = FALSE,
    copy = FALSE,
    ownership = "owned-native-storage",
    finalizer = TRUE,
    notes = paste(
      "Union constructors allocate owned native storage and expose member",
      "getters/setters over that shared buffer."
    )
  ),
  union_nested_struct_view = list(
    kind = "union_view",
    helper = "tcc_union",
    borrow = TRUE,
    copy = FALSE,
    ownership = "borrowed-from-union",
    protects_owner = TRUE,
    lifetime_model = "preserved owner slot",
    survives_gc_with_live_view = TRUE,
    notes = paste(
      "Nested struct getters on unions return borrowed member views and keep",
      "the owning union in the protected slot so the shared storage remains",
      "alive while the view exists."
    )
  ),
  enum_i32 = list(
    kind = "enum",
    helper = "tcc_enum",
    boundary_mode = "i32-like",
    borrow = FALSE,
    copy = TRUE,
    ownership = "R",
    notes = paste(
      "Enums cross the wrapper boundary as integer-like values and constant",
      "helpers box them into fresh scalar integer SEXPs."
    )
  ),
  global_scalar = list(
    kind = "global",
    helper = "tcc_global",
    scalar_only = TRUE,
    arrays_forbidden = TRUE,
    borrow = FALSE,
    copy = TRUE,
    ownership = "compiled-unit",
    notes = paste(
      "Global helpers are limited to scalar types and reuse the same wrapper",
      "input/output coercion rules as ordinary scalar bindings."
    )
  ),
  bitfield_native = list(
    kind = "bitfield",
    helper = "tcc_struct/tcc_treesitter_struct_accessors",
    compiler_managed = TRUE,
    default_ffi_type = "u8",
    treesitter_bitfield_type = "u8",
    include_bitfields = TRUE,
    address_helpers_forbidden = TRUE,
    container_of_forbidden = TRUE,
    survives_forced_gc = TRUE,
    notes = paste(
      "Bitfields are stored and masked by the C compiler; helper accessors",
      "treat them as scalar fields, treesitter defaults them to u8 unless",
      "the caller overrides bitfield_type, and address-style helpers are",
      "forbidden for bitfield members."
    )
  ),
  treesitter_header_bindings = list(
    kind = "header_codegen",
    helper = "tcc_generate_bindings",
    include_bitfields = TRUE,
    bitfield_type = "u8",
    nested_struct_mode = "ptr-like",
    generates = c("functions", "structs", "unions", "enums", "globals"),
    notes = paste(
      "Treesitter helpers project parsed header declarations into the same",
      "FFI helper surface, including optional bitfield accessors and global",
      "getter/setter bindings; treesitter-generated nested struct fields",
      "currently stay ptr-like unless a richer parser-level model is added",
      "explicitly."
    )
  )
)

RTINYCC_COMPOSITE_CODEGEN_SPECS <- list(
  list(
    name = "struct_owned_host_helper",
    info = "struct constructors use host owned-composite helper instead of direct extptr registration",
    generate_args = list(
      symbols = list(),
      c_code = "struct student { int id; double grade; };",
      structs = list(student = c(id = "i32", grade = "f64"))
    ),
    patterns = list(
      list(
        pattern = "return RC_make_owned_composite_ptr(p, Rf_install(\"struct_student\"));",
        fixed = TRUE
      )
    ),
    forbidden = list(
      list(
        pattern = "R_RegisterCFinalizerEx(ext, RC_owned_native_finalizer, FALSE);",
        fixed = TRUE
      ),
      list(
        pattern = "R_MakeExternalPtr(p, Rf_install(\"struct_student\"), R_NilValue)",
        fixed = TRUE
      )
    )
  ),
  list(
    name = "struct_field_addr_owner",
    info = "field_addr helper preserves owner through borrowed-view helper",
    generate_args = list(
      symbols = list(),
      c_code = "struct student { int id; double grade; };",
      structs = list(student = c(id = "i32", grade = "f64")),
      field_addr = list(student = "id")
    ),
    patterns = list(
      list(
        pattern = "SEXP R_wrap_struct_student_id_addr(SEXP ext) {",
        fixed = TRUE
      ),
      list(
        pattern = "return RC_make_borrowed_view(field_ptr, Rf_install(\"rtinycc_borrowed\"), ext);",
        fixed = TRUE
      )
    )
  ),
  list(
    name = "struct_container_of_owner",
    info = "container_of helper preserves owner chain through borrowed-view helper",
    generate_args = list(
      symbols = list(),
      c_code = "struct student { int id; double grade; };",
      structs = list(student = c(id = "i32", grade = "f64")),
      container_of = list(student = "id")
    ),
    patterns = list(
      list(
        pattern = "SEXP R_wrap_struct_student_from_id(SEXP ext) {",
        fixed = TRUE
      ),
      list(
        pattern = "return RC_make_borrowed_view(p, Rf_install(\"struct_student\"), ext);",
        fixed = TRUE
      )
    )
  ),
  list(
    name = "struct_raw_access_copy",
    info = "struct raw access helpers use explicit copy paths",
    generate_args = list(
      symbols = list(),
      c_code = "struct packet { unsigned char data[8]; };",
      structs = list(
        packet = list(data = list(type = "u8", size = 8, array = TRUE))
      ),
      struct_raw_access = "packet"
    ),
    patterns = list(
      list(
        pattern = "SEXP R_wrap_struct_packet_get_raw(SEXP ext, SEXP len) {",
        fixed = TRUE
      ),
      list(
        pattern = "memcpy(RAW(raw), p,",
        fixed = TRUE
      ),
      list(
        pattern = "SEXP R_wrap_struct_packet_set_raw(SEXP ext, SEXP raw) {",
        fixed = TRUE
      ),
      list(
        pattern = "RAW_GET_REGION(raw, 0, n_copy, (Rbyte*)p)",
        fixed = TRUE
      )
    )
  ),
  list(
    name = "union_owned_host_helper",
    info = "union constructors use host owned-composite helper instead of direct extptr registration",
    generate_args = list(
      symbols = list(),
      c_code = "union wrapper { int raw; double value; };",
      unions = list(
        wrapper = list(
          members = list(raw = "i32", value = "f64"),
          active = "raw"
        )
      )
    ),
    patterns = list(
      list(
        pattern = "return RC_make_owned_composite_ptr(p, Rf_install(\"union_wrapper\"));",
        fixed = TRUE
      )
    ),
    forbidden = list(
      list(
        pattern = "R_RegisterCFinalizerEx(ext, RC_owned_native_finalizer, FALSE);",
        fixed = TRUE
      ),
      list(
        pattern = "R_MakeExternalPtr(p, Rf_install(\"union_wrapper\"), R_NilValue)",
        fixed = TRUE
      )
    )
  ),
  list(
    name = "union_nested_struct_owner",
    info = "union nested struct getters return borrowed views that protect the union owner",
    generate_args = list(
      symbols = list(),
      c_code = "union wrapper { struct { int x; } inner; int raw; };",
      unions = list(
        wrapper = list(
          members = list(inner = list(type = "struct"), raw = "i32"),
          active = "raw"
        )
      )
    ),
    patterns = list(
      list(
        pattern = "return RC_make_borrowed_view(&p->inner, Rf_install(\"struct_inner\"), ext);",
        fixed = TRUE
      )
    )
  ),
  list(
    name = "enum_constant_and_sizeof",
    info = "enum helpers expose constants and sizeof introspection",
    generate_args = list(
      symbols = list(),
      c_code = "enum color { RED = 1, BLUE = 2 };",
      enums = list(
        color = list(constants = c("RED", "BLUE"), export_constants = TRUE)
      ),
      introspect = TRUE
    ),
    patterns = list(
      list(
        pattern = "SEXP R_wrap_enum_color_RED(void) {",
        fixed = TRUE
      ),
      list(
        pattern = "return ScalarInteger(RED);",
        fixed = TRUE
      ),
      list(
        pattern = "SEXP R_wrap_enum_color_sizeof(void) {",
        fixed = TRUE
      )
    )
  ),
  list(
    name = "global_get_set_helpers",
    info = "global helpers generate scalar getter and setter wrappers",
    generate_args = list(
      symbols = list(),
      c_code = "int32_t global_counter = 0;",
      globals = list(global_counter = "i32")
    ),
    patterns = list(
      list(
        pattern = "SEXP R_wrap_global_global_counter_get(void) {",
        fixed = TRUE
      ),
      list(
        pattern = "SEXP R_wrap_global_global_counter_set(SEXP value_) {",
        fixed = TRUE
      ),
      list(
        pattern = "global_counter = value;",
        fixed = FALSE
      )
    )
  )
)

rtinycc_scalar_soundness_specs <- function() {
  Filter(
    function(spec) {
      !is.null(spec$soundness) && isTRUE(spec$soundness$generated)
    },
    RTINYCC_FFI_SEMANTICS
  )
}

rtinycc_scalar_input_rule_body <- function(type, arg_name, r_name) {
  spec <- RTINYCC_FFI_SEMANTICS[[type]]
  codegen <- spec$codegen

  if (is.null(codegen) || is.null(codegen$input_form)) {
    stop("No scalar input rule registered for type: ", type, call. = FALSE)
  }

  values <- c(
    list(
      arg_name = arg_name,
      r_name = r_name,
      c_type = spec$c_type
    ),
    codegen
  )

  switch(
    codegen$input_form,
    integer_range = rtinycc_render_template(
      c(
        "  int _{{arg_name}} = asInteger({{r_name}});",
        "  if (_{{arg_name}} == NA_INTEGER) Rf_error(\"integer value is NA\");",
        "  if (_{{arg_name}} < {{input_range_min}} || _{{arg_name}} > {{input_range_max}}) Rf_error(\"{{input_range_error}}\");",
        "  {{c_type}} {{arg_name}} = ({{c_type}})_{{arg_name}};"
      ),
      values
    ),
    exact_integer_real = rtinycc_render_template(
      c(
        "  double _{{arg_name}} = asReal({{r_name}});",
        "  if (ISNA(_{{arg_name}}) || ISNAN(_{{arg_name}})) Rf_error(\"numeric value is NA\");",
        if (isTRUE(codegen$input_nonnegative)) {
          "  if (_{{arg_name}} < 0) Rf_error(\"{{input_nonnegative_error}}\");"
        },
        "  if (fabs(_{{arg_name}}) > 9007199254740992.0) Rf_error(\"{{input_exact_error}}\");",
        "  if (trunc(_{{arg_name}}) != _{{arg_name}}) Rf_error(\"{{input_integer_error}}\");",
        if (
          !is.null(codegen$input_range_min) && !is.null(codegen$input_range_max)
        ) {
          "  if (_{{arg_name}} < (double){{input_range_min}} || _{{arg_name}} > (double){{input_range_max}}) Rf_error(\"{{input_range_error}}\");"
        },
        "  {{c_type}} {{arg_name}} = ({{c_type}})_{{arg_name}};"
      ),
      values
    ),
    real_integer_range = rtinycc_render_template(
      c(
        "  double _{{arg_name}} = asReal({{r_name}});",
        "  if (ISNA(_{{arg_name}}) || ISNAN(_{{arg_name}})) Rf_error(\"numeric value is NA\");",
        "  if (_{{arg_name}} < {{input_range_min}} || _{{arg_name}} > {{input_range_max}}) Rf_error(\"{{input_range_error}}\");",
        "  if (trunc(_{{arg_name}}) != _{{arg_name}}) Rf_error(\"{{input_integer_error}}\");",
        "  {{c_type}} {{arg_name}} = ({{c_type}})_{{arg_name}};"
      ),
      values
    ),
    real_scalar_cast = rtinycc_render_template(
      "  {{c_type}} {{arg_name}} = ({{c_type}})asReal({{r_name}});",
      values
    ),
    real_scalar = rtinycc_render_template(
      "  {{c_type}} {{arg_name}} = asReal({{r_name}});",
      values
    ),
    logical_scalar = rtinycc_render_template(
      c(
        "  int _{{arg_name}} = asLogical({{r_name}});",
        "  if (_{{arg_name}} == NA_LOGICAL) Rf_error(\"logical value is NA\");",
        "  bool {{arg_name}} = (bool)(_{{arg_name}} != 0);"
      ),
      values
    ),
    cstring = rtinycc_render_template(
      c(
        "  SEXP _{{arg_name}} = STRING_ELT({{r_name}}, 0);",
        "  const char* {{arg_name}} = (_{{arg_name}} == NA_STRING) ? NULL : Rf_translateCharUTF8(_{{arg_name}});"
      ),
      values
    ),
    ptr = rtinycc_render_template(
      "  void* {{arg_name}} = R_ExternalPtrAddr({{r_name}});",
      values
    ),
    sexp = rtinycc_render_template(
      "  SEXP {{arg_name}} = {{r_name}};",
      values
    ),
    stop("Unsupported scalar input form: ", codegen$input_form, call. = FALSE)
  )
}

rtinycc_scalar_return_rule_body <- function(type, value_expr) {
  spec <- RTINYCC_FFI_SEMANTICS[[type]]
  codegen <- spec$codegen

  if (is.null(codegen) || is.null(codegen$return_form)) {
    stop("No scalar return rule registered for type: ", type, call. = FALSE)
  }

  values <- c(
    list(value_expr = value_expr),
    codegen
  )

  switch(
    codegen$return_form,
    scalar_integer_cast = rtinycc_render_template(
      "return ScalarInteger((int){{value_expr}});",
      values
    ),
    scalar_integer = rtinycc_render_template(
      "return ScalarInteger({{value_expr}});",
      values
    ),
    scalar_real_cast = rtinycc_render_template(
      "return ScalarReal((double){{value_expr}});",
      values
    ),
    scalar_real = rtinycc_render_template(
      "return ScalarReal({{value_expr}});",
      values
    ),
    i64 = rtinycc_render_template(
      c(
        "  int64_t __rtinycc_ret = {{value_expr}};",
        "  if (__rtinycc_ret > INT64_C(9007199254740992) || __rtinycc_ret < -INT64_C(9007199254740992)) Rf_warning(\"i64 precision loss in R numeric\");",
        "  return ScalarReal((double)__rtinycc_ret);"
      ),
      values
    ),
    u64 = rtinycc_render_template(
      c(
        "  uint64_t __rtinycc_ret = {{value_expr}};",
        "  if (__rtinycc_ret > UINT64_C(9007199254740992)) Rf_warning(\"u64 precision loss in R numeric\");",
        "  return ScalarReal((double)__rtinycc_ret);"
      ),
      values
    ),
    logical = rtinycc_render_template(
      "return ScalarLogical((int){{value_expr}});",
      values
    ),
    cstring = rtinycc_render_template(
      c(
        "const char* __rtinycc_ret = {{value_expr}};",
        "if (__rtinycc_ret) {",
        "    SEXP out = PROTECT(mkString(__rtinycc_ret));",
        "    UNPROTECT(1);",
        "    return out;",
        "} else {",
        "    return R_NilValue;",
        "}"
      ),
      values
    ),
    ptr = rtinycc_render_template(
      "return RC_make_unowned_ptr({{value_expr}}, R_NilValue);",
      values
    ),
    sexp = rtinycc_render_template(
      "return {{value_expr}};",
      values
    ),
    void = rtinycc_render_template(
      c(
        "{{value_expr}};",
        "  return R_NilValue;"
      ),
      values
    ),
    stop("Unsupported scalar return form: ", codegen$return_form, call. = FALSE)
  )
}

rtinycc_register_scalar_codegen_rules <- function(where = parent.frame()) {
  for (type in names(RTINYCC_FFI_SEMANTICS)) {
    spec <- RTINYCC_FFI_SEMANTICS[[type]]

    if (!is.null(spec$codegen$input_form)) {
      eval(
        substitute(
          ffi_input_rule(TYPE, arg_name, r_name) %as%
            rtinycc_scalar_input_rule_body(TYPE, arg_name, r_name),
          list(TYPE = type)
        ),
        envir = where
      )
    }

    if (!is.null(spec$codegen$return_form)) {
      eval(
        substitute(
          ffi_return_rule(TYPE, value_expr) %as%
            rtinycc_scalar_return_rule_body(TYPE, value_expr),
          list(TYPE = type)
        ),
        envir = where
      )
    }
  }

  invisible()
}
