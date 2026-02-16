# Test FFI Code Generation - With Real Compilation
# Tests that generated code actually compiles and runs

library(Rtinycc)

# Test 1: Generate and compile simple FFI code
symbols <- list(
  forty_two = list(args = list(), returns = "i32")
)

code <- Rtinycc:::generate_ffi_code(
  symbols = symbols,
  headers = NULL,
  c_code = "int forty_two() { return 42; }",
  is_external = FALSE
)

expect_true(grepl("int forty_two", code))
expect_true(grepl("R_wrap_forty_two", code))

r_lib_paths <- file.path(R.home("lib"))
if (.Platform$OS.type == "windows") {
  r_lib_paths <- c(
    r_lib_paths,
    normalizePath(
      file.path(R.home(), "bin", .Platform$r_arch),
      winslash = "/",
      mustWork = FALSE
    )
  )
}
state <- tcc_state(
  output = "memory",
  include_path = c(tcc_include_paths(), file.path(R.home("include"))),
  lib_path = c(tcc_lib_paths(), r_lib_paths)
)
if (.Platform$OS.type == "windows") {
  tcc_add_library(state, "R")
}
result <- tcc_compile_string(state, code)
expect_equal(result, 0L)

relocate_result <- tcc_relocate(state)
expect_equal(relocate_result, 0L)

sym_ptr <- tcc_get_symbol(state, "R_wrap_forty_two")
expect_true(inherits(sym_ptr, "tcc_symbol"))

# Test 2: Verify array type code generation compiles
symbols <- list(
  sum_ints = list(args = list("integer_array", "i32"), returns = "i64")
)

code <- Rtinycc:::generate_ffi_code(
  symbols = symbols,
  c_code = "
    int64_t sum_ints(int32_t* arr, int32_t n) {
      int64_t sum = 0;
      for(int i = 0; i < n; i++) sum += arr[i];
      return sum;
    }
  ",
  is_external = FALSE
)

expect_true(grepl("sum_ints", code))
expect_true(grepl("R_wrap_sum_ints", code))

r_lib_paths <- file.path(R.home("lib"))
if (.Platform$OS.type == "windows") {
  r_lib_paths <- c(
    r_lib_paths,
    normalizePath(
      file.path(R.home(), "bin", .Platform$r_arch),
      winslash = "/",
      mustWork = FALSE
    )
  )
}
state <- tcc_state(
  output = "memory",
  include_path = c(tcc_include_paths(), file.path(R.home("include"))),
  lib_path = c(tcc_lib_paths(), r_lib_paths)
)
if (.Platform$OS.type == "windows") {
  tcc_add_library(state, "R")
}
result <- tcc_compile_string(state, code)
expect_equal(result, 0L)
expect_equal(tcc_relocate(state), 0L)
