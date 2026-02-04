# FFI Type System - R/C focused type mapping
# Designed for C FFI (not Zig), optimized for R data types
#
# Key insight: R has native vector types that map directly to C arrays:
# - raw() → uint8_t* (byte buffers)
# - integer() → int32_t* (via INTEGER())
# - numeric() → double* (via REAL())
# - logical() → int* (via LOGICAL())
#
# We support both scalar types (with coercion) and array types (zero-copy)

FFI_TYPES <- list(
  # Scalar integer types (with range checking)
  i8 = list(c_type = "int8_t", r_type = "integer", size = 1L, kind = "scalar"),
  i16 = list(
    c_type = "int16_t",
    r_type = "integer",
    size = 2L,
    kind = "scalar"
  ),
  i32 = list(
    c_type = "int32_t",
    r_type = "integer",
    size = 4L,
    kind = "scalar"
  ),
  i64 = list(
    c_type = "int64_t",
    r_type = "numeric",
    size = 8L,
    kind = "scalar"
  ),

  # Scalar unsigned integers
  u8 = list(c_type = "uint8_t", r_type = "integer", size = 1L, kind = "scalar"),
  u16 = list(
    c_type = "uint16_t",
    r_type = "integer",
    size = 2L,
    kind = "scalar"
  ),
  u32 = list(
    c_type = "uint32_t",
    r_type = "numeric",
    size = 4L,
    kind = "scalar"
  ),
  u64 = list(
    c_type = "uint64_t",
    r_type = "numeric",
    size = 8L,
    kind = "scalar"
  ),

  # Scalar float types
  f32 = list(c_type = "float", r_type = "numeric", size = 4L, kind = "scalar"),
  f64 = list(c_type = "double", r_type = "numeric", size = 8L, kind = "scalar"),

  # Boolean scalar
  bool = list(c_type = "bool", r_type = "logical", size = 1L, kind = "scalar"),

  # String types
  cstring = list(
    c_type = "char*",
    r_type = "character",
    size = NA_integer_,
    kind = "scalar"
  ),

  # Pointer (opaque externalptr)
  ptr = list(
    c_type = "void*",
    r_type = "externalptr",
    size = NA_integer_,
    kind = "scalar"
  ),

  # Array types - R native vector types (zero-copy)
  # R raw vector → uint8_t* (byte buffer)
  raw = list(
    c_type = "uint8_t*",
    r_type = "raw",
    size = NA_integer_,
    kind = "array",
    r_accessor = "RAW",
    c_element = "uint8_t"
  ),

  # R integer vector → int32_t* (zero-copy via INTEGER())
  integer_array = list(
    c_type = "int32_t*",
    r_type = "integer",
    size = NA_integer_,
    kind = "array",
    r_accessor = "INTEGER",
    c_element = "int32_t"
  ),

  # R numeric vector → double* (zero-copy via REAL())
  numeric_array = list(
    c_type = "double*",
    r_type = "numeric",
    size = NA_integer_,
    kind = "array",
    r_accessor = "REAL",
    c_element = "double"
  ),

  # R logical vector → int* (zero-copy via LOGICAL())
  logical_array = list(
    c_type = "int*",
    r_type = "logical",
    size = NA_integer_,
    kind = "array",
    r_accessor = "LOGICAL",
    c_element = "int"
  ),

  # R character vector → SEXP (STRING_PTR for R >= 3.5)
  character_array = list(
    c_type = "SEXP*",
    r_type = "character",
    size = NA_integer_,
    kind = "array",
    r_accessor = "STRING_PTR",
    c_element = "SEXP"
  ),

  # R-specific: pass R object directly (SEXP)
  sexp = list(
    c_type = "SEXP",
    r_type = "ANY",
    size = NA_integer_,
    kind = "scalar"
  ),

  # Void (return only)
  void = list(c_type = "void", r_type = "NULL", size = 0L, kind = "scalar"),

  # Callback type - function pointer passed from R
  callback = list(
    c_type = "void*",
    r_type = "externalptr",
    size = NA_integer_,
    kind = "scalar"
  )
)

# Valid FFI type names
VALID_FFI_TYPES <- names(FFI_TYPES)

# Get array types
ARRAY_TYPES <- names(Filter(function(x) x$kind == "array", FFI_TYPES))
SCALAR_TYPES <- names(Filter(function(x) x$kind == "scalar", FFI_TYPES))

# Validate FFI type
check_ffi_type <- function(type, context = "argument") {
  # Check for enum:type pattern
  if (grepl("^enum:", type)) {
    # Enum types are always i32 (int)
    return(list(c_type = "int", r_type = "integer", size = 4L, kind = "scalar"))
  }

  # Check for callback:type pattern (e.g., callback:double(int,int))
  if (grepl("^callback", type) || grepl("^callback_async", type)) {
    # Callback types are always void* (function pointer)
    return(list(
      c_type = "void*",
      r_type = "externalptr",
      size = NA_integer_,
      kind = "scalar"
    ))
  }

  if (!type %in% VALID_FFI_TYPES) {
    stop(
      "Invalid FFI type '",
      type,
      "' for ",
      context,
      "\nValid types: ",
      paste(VALID_FFI_TYPES, collapse = ", "),
      call. = FALSE
    )
  }
  FFI_TYPES[[type]]
}

# Check if type is an array type
is_array_type <- function(type) {
  type %in% ARRAY_TYPES
}

# Check if type is a scalar type
is_scalar_type <- function(type) {
  type %in% SCALAR_TYPES
}
