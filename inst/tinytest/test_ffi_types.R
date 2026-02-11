# Test FFI Type System
# Real tests for the Bun-style FFI type mappings

library(Rtinycc)

# Test 1: FFI type validation works
type_info <- Rtinycc:::check_ffi_type("i32", "test")
expect_equal(type_info$c_type, "int32_t")
expect_equal(type_info$r_type, "integer")
expect_equal(type_info$kind, "scalar")

# Test 2: Array types have proper accessors
type_info <- Rtinycc:::check_ffi_type("raw", "test")
expect_equal(type_info$r_accessor, "RAW")
expect_equal(type_info$kind, "array")

type_info <- Rtinycc:::check_ffi_type("integer_array", "test")
expect_equal(type_info$r_accessor, "INTEGER")
expect_equal(type_info$c_type, "int32_t*")

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

# Test 6: C type aliases map to expected FFI types
expect_equal(tcc_map_c_type_to_ffi("size_t"), "u64")
expect_equal(tcc_map_c_type_to_ffi("ssize_t"), "i64")
expect_equal(tcc_map_c_type_to_ffi("ptrdiff_t"), "i64")
expect_equal(tcc_map_c_type_to_ffi("intptr_t"), "i64")
expect_equal(tcc_map_c_type_to_ffi("uintptr_t"), "u64")
expect_equal(tcc_map_c_type_to_ffi("off_t"), "i64")
expect_equal(tcc_map_c_type_to_ffi("unsigned long long int"), "u64")
expect_equal(tcc_map_c_type_to_ffi("signed char"), "i8")
expect_equal(tcc_map_c_type_to_ffi("unsigned"), "u32")
expect_equal(tcc_map_c_type_to_ffi("long double"), "f64")
