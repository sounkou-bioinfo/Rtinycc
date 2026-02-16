# Test FFI Code Generation
# Tests for C wrapper code generation

library(Rtinycc)
library(tinytest)

# Test 1: Generate C input code for R API mode (SEXP conversion)
code <- Rtinycc:::generate_c_input("x", "arg1_", "i32")
expect_true(grepl("int _x = asInteger", code))
expect_true(grepl("int32_t x = (int32_t)_x", code, fixed = TRUE))

code <- Rtinycc:::generate_c_input("y", "arg2_", "f64")
expect_true(grepl("double y = asReal", code))

code <- Rtinycc:::generate_c_input("flag", "arg3_", "bool")
expect_true(grepl("int _flag = asLogical", code))
expect_true(grepl("bool flag = (bool)(_flag != 0)", code, fixed = TRUE))

# Test 2: Generate C input for array types
code <- Rtinycc:::generate_c_input("buf", "arg1_", "raw")
expect_true(grepl("uint8_t[*] buf = RAW", code))

code <- Rtinycc:::generate_c_input("arr", "arg1_", "integer_array")
expect_true(grepl("int32_t[*] arr = INTEGER", code))

code <- Rtinycc:::generate_c_input("nums", "arg1_", "numeric_array")
expect_true(grepl("double[*] nums = REAL", code))

# Test 3: Generate C return code
rc <- Rtinycc:::generate_c_return("result", "i32")
expect_true(grepl("ScalarInteger", rc))

rc <- Rtinycc:::generate_c_return("result", "f64")
expect_true(grepl("ScalarReal", rc))

rc <- Rtinycc:::generate_c_return("result", "cstring")
expect_true(grepl("mkString", rc))

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
expect_true(grepl("#include <stdint.h>", full_code))
expect_true(grepl("#include <stdbool.h>", full_code))
expect_true(grepl("R_wrap_add", full_code))
expect_true(grepl("R_wrap_greet", full_code))
expect_true(grepl("SEXP R_wrap_add", full_code))
