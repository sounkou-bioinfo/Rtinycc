# Test FFI High-Level API
# Tests for tcc_ffi(), tcc_bind(), tcc_compile() with real compilation

library(Rtinycc)

# Test 1: Create FFI context
ffi <- tcc_ffi()
expect_true(inherits(ffi, "tcc_ffi"))
expect_equal(ffi$output, "memory")

# Test 2: Chain operations with native pipe
ffi <- tcc_ffi() |>
  tcc_output("memory") |>
  tcc_bind(
    add = list(args = list("i32", "i32"), returns = "i32")
  )

expect_equal(length(ffi$symbols), 1)
expect_true("add" %in% names(ffi$symbols))

# Test 3: Compile and call simple function
ffi <- tcc_ffi() |>
  tcc_bind(
    forty_two = list(args = list(), returns = "i32")
  ) |>
  tcc_source(
    "
    int forty_two() {
      return 42;
    }
  "
  ) |>
  tcc_compile()

expect_true(inherits(ffi, "tcc_compiled"))

# Test calling the compiled function
result <- ffi$forty_two()
expect_equal(result, 42L)

# Test 4: Compile and call function with arguments
ffi <- tcc_ffi() |>
  tcc_bind(
    add = list(args = list("i32", "i32"), returns = "i32")
  ) |>
  tcc_source(
    "
    int add(int a, int b) {
      return a + b;
    }
  "
  ) |>
  tcc_compile()

expect_true(inherits(ffi, "tcc_compiled"))

# Test calling with arguments
result <- ffi$add(5L, 3L)
expect_equal(result, 8L)

result <- ffi$add(10L, 20L)
expect_equal(result, 30L)

# Test 5: Compile and call function with double arguments
ffi <- tcc_ffi() |>
  tcc_bind(
    multiply = list(args = list("f64", "f64"), returns = "f64")
  ) |>
  tcc_source(
    "
    double multiply(double a, double b) {
      return a * b;
    }
  "
  ) |>
  tcc_compile()

expect_true(inherits(ffi, "tcc_compiled"))

result <- ffi$multiply(2.5, 4.0)
expect_equal(result, 10.0)

# Test 6: Test print methods
ffi <- tcc_ffi() |>
  tcc_bind(
    foo = list(args = list("i32"), returns = "f64")
  )

# Capture print output
output <- capture.output(print(ffi))
expect_true(any(grepl("tcc_ffi", output)))
expect_true(any(grepl("foo", output)))
