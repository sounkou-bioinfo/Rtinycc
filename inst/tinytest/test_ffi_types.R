# Test FFI Type System
# Real tests for the Bun-style FFI type mappings

library(Rtinycc)

# Test 1: FFI type validation works and returns a classed type object
type_info <- Rtinycc:::check_ffi_type("i32", "test")
expect_true(Rtinycc:::is_rtinycc_ffi_type(type_info))
expect_equal(type_info$name, "i32")
expect_equal(Rtinycc:::ffi_type_family(type_info), "i32")
expect_equal(type_info$c_type, "int32_t")
expect_equal(type_info$r_type, "integer")
expect_equal(type_info$kind, "scalar")

# Test 1b: scalar ffi types record the intended R-side carrier types
expect_equal(Rtinycc:::check_ffi_type("i8", "test")$r_type, "integer")
expect_equal(Rtinycc:::check_ffi_type("i16", "test")$r_type, "integer")
expect_equal(Rtinycc:::check_ffi_type("i32", "test")$r_type, "integer")
expect_equal(Rtinycc:::check_ffi_type("u8", "test")$r_type, "integer")
expect_equal(Rtinycc:::check_ffi_type("u16", "test")$r_type, "integer")
expect_equal(Rtinycc:::check_ffi_type("u32", "test")$r_type, "numeric")
expect_equal(Rtinycc:::check_ffi_type("i64", "test")$r_type, "numeric")
expect_equal(Rtinycc:::check_ffi_type("u64", "test")$r_type, "numeric")
expect_equal(Rtinycc:::check_ffi_type("f32", "test")$r_type, "numeric")
expect_equal(Rtinycc:::check_ffi_type("f64", "test")$r_type, "numeric")
expect_equal(Rtinycc:::check_ffi_type("bool", "test")$r_type, "logical")
expect_equal(Rtinycc:::check_ffi_type("cstring", "test")$r_type, "character")

# Test 2: Array types have proper accessors
type_info <- Rtinycc:::check_ffi_type("raw", "test")
expect_equal(type_info$r_accessor, "RAW")
expect_equal(type_info$kind, "array")

type_info <- Rtinycc:::check_ffi_type("integer_array", "test")
expect_equal(type_info$r_accessor, "INTEGER")
expect_equal(type_info$c_type, "int32_t*")

type_info <- Rtinycc:::check_ffi_type("character_array", "test")
expect_equal(type_info$r_accessor, "STRING_PTR_RO")
expect_equal(type_info$c_type, "const SEXP*")
expect_equal(type_info$c_element, "SEXP (CHARSXP cell)")

# Test 3: Invalid types throw errors
expect_error(Rtinycc:::check_ffi_type("invalid_type", "test"))

# Test 4: All expected types exist
expected_types <- c(
  "i8",
  "i16",
  "i32",
  "i64",
  "u8",
  "u16",
  "u32",
  "u64",
  "f32",
  "f64",
  "bool",
  "cstring",
  "ptr",
  "raw",
  "integer_array",
  "numeric_array",
  "logical_array",
  "sexp",
  "void"
)
for (t in expected_types) {
  info <- Rtinycc:::check_ffi_type(t, "test")
  expect_true(!is.null(info))
}

# Test 5: is_array_type and is_scalar_type work
expect_true(Rtinycc:::is_array_type("raw"))
expect_true(Rtinycc:::is_array_type("integer_array"))
expect_false(Rtinycc:::is_array_type("i32"))
expect_true(Rtinycc:::is_scalar_type("i32"))
expect_false(Rtinycc:::is_scalar_type("raw"))

# Test 6: special type families are classed, not bare stringly metadata
expect_equal(
  Rtinycc:::ffi_type_family(Rtinycc:::check_ffi_type("enum:color", "test")),
  "enum"
)
expect_equal(
  Rtinycc:::ffi_type_family(Rtinycc:::check_ffi_type(
    "callback:double(int)",
    "test"
  )),
  "callback"
)
expect_equal(
  Rtinycc:::ffi_type_family(Rtinycc:::check_ffi_type(
    "callback_async:void*(void*)",
    "test"
  )),
  "callback_async"
)

# Test 7: all declared C type aliases map to the expected canonical FFI types

mapping_cases <- list(
  void = c("void"),
  bool = c("bool", "_Bool"),
  sexp = c("SEXP", "sexp"),
  i32 = c("int", "signed", "int32_t", "__int32", "signed int"),
  i16 = c("short", "short int", "signed short", "signed short int", "int16_t"),
  i8 = c("char", "signed char", "int8_t"),
  i64 = c(
    "long",
    "long int",
    "long long",
    "long long int",
    "signed long",
    "signed long int",
    "signed long long",
    "signed long long int",
    "int64_t",
    "__int64",
    "intptr_t",
    "ptrdiff_t",
    "ssize_t",
    "off_t"
  ),
  u32 = c("unsigned int", "unsigned", "uint32_t", "unsigned __int32"),
  u16 = c("unsigned short", "unsigned short int", "uint16_t"),
  u8 = c("unsigned char", "uint8_t"),
  u64 = c(
    "unsigned long",
    "unsigned long int",
    "unsigned long long",
    "unsigned long long int",
    "uint64_t",
    "unsigned __int64",
    "size_t",
    "uintptr_t"
  ),
  f64 = c("double", "long double"),
  f32 = c("float")
)

for (ffi_type in names(mapping_cases)) {
  for (c_type in mapping_cases[[ffi_type]]) {
    expect_equal(
      tcc_map_c_type_to_ffi(c_type),
      ffi_type,
      info = paste("C type", shQuote(c_type), "maps to", ffi_type)
    )
  }
}

# Test 8: pointer spellings stay pointer-like under conservative mapping
#
# Regression coverage for a bug where trailing-identifier stripping could drop
# the `*` from declarations such as `const char *`, causing them to collapse to
# the scalar `i8` mapping instead of the intended conservative pointer mapping.
pointer_cases <- c(
  "const char *",
  "char *",
  "const char *name",
  "char *buf",
  "void **out",
  "const void *ctx",
  "volatile char *cursor",
  "char **argv"
)

for (c_type in pointer_cases) {
  expect_equal(
    tcc_map_c_type_to_ffi(c_type),
    "ptr",
    info = paste("pointer type", shQuote(c_type), "stays on conservative ptr mapping")
  )
}

# Test 9: whitespace and qualifiers are normalized before mapping
expect_equal(tcc_map_c_type_to_ffi(" const   unsigned   long  long   int "), "u64")
expect_equal(tcc_map_c_type_to_ffi("const volatile restrict double"), "f64")
expect_equal(tcc_map_c_type_to_ffi("const volatile char * name"), "ptr")
