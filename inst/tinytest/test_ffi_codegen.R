# Test FFI Code Generation
# Tests for C wrapper code generation

library(Rtinycc)
library(tinytest)

callback_abi_specs <- Rtinycc:::rtinycc_callback_abi_specs()

# Test 1: Generate C input code for R API mode (SEXP conversion)
code <- Rtinycc:::generate_c_input("x", "arg1_", "i32")
expect_true(grepl("int _x = asInteger", code))
expect_true(grepl("int32_t x = (int32_t)_x", code, fixed = TRUE))

code <- Rtinycc:::generate_c_input("y", "arg2_", "f64")
expect_true(grepl("double y = asReal", code))

code <- Rtinycc:::generate_c_input("u32_value", "arg4_", "u32")
expect_true(grepl("double _u32_value = asReal", code))
expect_true(grepl("uint32_t u32_value = \\(uint32_t\\)_u32_value", code))

code <- Rtinycc:::generate_c_input("i64_value", "arg5_", "i64")
expect_true(grepl("double _i64_value = asReal", code))
expect_true(grepl("int64_t i64_value = \\(int64_t\\)_i64_value", code))

code <- Rtinycc:::generate_c_input("u64_value", "arg6_", "u64")
expect_true(grepl("double _u64_value = asReal", code))
expect_true(grepl("uint64_t u64_value = \\(uint64_t\\)_u64_value", code))

code <- Rtinycc:::generate_c_input("flag", "arg3_", "bool")
expect_true(grepl("int _flag = asLogical", code))
expect_true(grepl("bool flag = (bool)(_flag != 0)", code, fixed = TRUE))

# Test 2: Generate C input for array types
code <- Rtinycc:::generate_c_input("buf", "arg1_", "raw")
expect_true(grepl("TYPEOF\\(arg1_\\) != RAWSXP", code))
expect_true(grepl("uint8_t[*] buf = RAW", code))

code <- Rtinycc:::generate_c_input("arr", "arg1_", "integer_array")
expect_true(grepl("TYPEOF\\(arg1_\\) != INTSXP", code))
expect_true(grepl("int32_t[*] arr = INTEGER", code))

code <- Rtinycc:::generate_c_input("nums", "arg1_", "numeric_array")
expect_true(grepl("TYPEOF\\(arg1_\\) != REALSXP", code))
expect_true(grepl("double[*] nums = REAL", code))

code <- Rtinycc:::generate_c_input("flags", "arg1_", "logical_array")
expect_true(grepl("TYPEOF\\(arg1_\\) != LGLSXP", code))
expect_true(grepl("int[*] flags = LOGICAL", code))

code <- Rtinycc:::generate_c_input("chars", "arg1_", "character_array")
expect_true(grepl("Rf_isString\\(arg1_\\)", code))
expect_true(grepl("const SEXP[*] chars = STRING_PTR_RO", code))
expect_true(grepl("STRING_PTR_RO", code, fixed = TRUE))

# Test 3: Generate C return code
rc <- Rtinycc:::generate_c_return("result", "i32")
expect_true(grepl("ScalarInteger", rc))

rc <- Rtinycc:::generate_c_return("result", "f64")
expect_true(grepl("ScalarReal", rc))

rc <- Rtinycc:::generate_c_return("result", "u32")
expect_true(grepl("ScalarReal", rc))

rc <- Rtinycc:::generate_c_return("result", "i64")
expect_true(grepl("ScalarReal", rc))

rc <- Rtinycc:::generate_c_return("result", "u64")
expect_true(grepl("ScalarReal", rc))

rc <- Rtinycc:::generate_c_return("result", "cstring")
expect_true(grepl("mkString", rc))

# Test 3b: Array return expressions are evaluated once and reused
rc <- Rtinycc:::generate_c_return(
  "rand_unif(arg1)",
  list(type = "numeric_array", length_arg = 1, free = TRUE),
  arg_names = c("arg1")
)
expect_true(grepl("double\\* __rtinycc_ret = rand_unif\\(arg1\\);", rc))
expect_true(grepl("memcpy\\(REAL\\(out\\), __rtinycc_ret,", rc))
expect_true(grepl("if \\(__rtinycc_ret\\) free\\(__rtinycc_ret\\);", rc))
expect_false(grepl("memcpy\\(REAL\\(out\\), rand_unif\\(arg1\\)", rc))
expect_false(grepl("free\\(rand_unif\\(arg1\\)\\)", rc))

# Test 3c: Scalar return expressions with warnings/branching are evaluated once
rc <- Rtinycc:::generate_c_return("next_i64()", "i64")
expect_true(grepl("int64_t __rtinycc_ret = next_i64\\(\\);", rc))
expect_false(grepl("next_i64\\(\\).+next_i64\\(\\)", rc))

rc <- Rtinycc:::generate_c_return("next_u64()", "u64")
expect_true(grepl("uint64_t __rtinycc_ret = next_u64\\(\\);", rc))
expect_false(grepl("next_u64\\(\\).+next_u64\\(\\)", rc))

rc <- Rtinycc:::generate_c_return("next_str()", "cstring")
expect_true(grepl("char\\* __rtinycc_ret = next_str\\(\\);", rc))
expect_true(grepl("mkString\\(__rtinycc_ret\\)", rc))
expect_false(grepl("next_str\\(\\).+next_str\\(\\)", rc))

sexp_call <- Rtinycc:::get_sexp_constructor_call("const char*", "next_str()")
expect_true(grepl("__rtinycc_tmp = next_str\\(\\);", sexp_call))
expect_false(grepl("next_str\\(\\).+next_str\\(\\)", sexp_call))

# Test 4: Generate full wrapper function
wrapper <- Rtinycc:::generate_c_wrapper(
  symbol_name = "my_add",
  wrapper_name = "R_wrap_my_add",
  arg_types = list("i32", "i32"),
  return_type = "i32",
  is_external = FALSE
)
expect_true(grepl("SEXP R_wrap_my_add", wrapper))
expect_true(grepl("int32_t", wrapper))
expect_true(grepl("my_add", wrapper))
expect_true(grepl("ScalarInteger", wrapper))

# Test 5: Generate wrapper with no arguments
wrapper <- Rtinycc:::generate_c_wrapper(
  symbol_name = "get_value",
  wrapper_name = "R_wrap_get_value",
  arg_types = list(),
  return_type = "i32",
  is_external = FALSE
)
expect_true(grepl("SEXP R_wrap_get_value", wrapper))
expect_true(grepl("void", wrapper))

# Test 6: Generate external declarations
decls <- Rtinycc:::generate_external_declarations(list(
  foo = list(args = list("i32", "f64"), returns = "cstring")
))
expect_true(grepl("extern char[*] foo", decls))

# Test 7: Generate complete FFI code
symbols <- list(
  add = list(args = list("i32", "i32"), returns = "i32"),
  greet = list(args = list("cstring"), returns = "cstring")
)

full_code <- Rtinycc:::generate_ffi_code(
  symbols = symbols,
  headers = c("#include <stdio.h>"),
  c_code = "int add(int a, int b) { return a + b; }",
  is_external = FALSE
)

expect_true(grepl("#define _Complex", full_code))
expect_true(grepl("#include <R.h>", full_code))
expect_true(grepl("#include <Rinternals.h>", full_code))
expect_true(grepl("#ifndef STRING_PTR_RO", full_code))
expect_true(grepl("#include <stdint.h>", full_code))
expect_true(grepl("#include <stdbool.h>", full_code))
expect_true(grepl("R_wrap_add", full_code))
expect_true(grepl("R_wrap_greet", full_code))
expect_true(grepl("SEXP R_wrap_add", full_code))

for (case in callback_abi_specs$wrapper) {
  cb_code <- Rtinycc:::generate_ffi_code(
    symbols = setNames(
      list(list(args = case$args, returns = case$returns)),
      case$name
    ),
    c_code = case$c_code,
    is_external = FALSE
  )

  for (pattern in case$patterns) {
    expect_true(grepl(pattern$pattern, cb_code), info = pattern$info)
  }
}

# Test 8: tcc_source stores code as chunks without introducing a leading blank line
ffi <- tcc_ffi() |>
  tcc_source("int a(void) { return 1; }") |>
  tcc_source("int b(void) { return 2; }")
expect_equal(
  ffi$c_code,
  c("int a(void) { return 1; }", "int b(void) { return 2; }")
)
