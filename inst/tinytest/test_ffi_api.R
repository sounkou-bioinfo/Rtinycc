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
expect_error(
  ffi$add(1L),
  "Expected 2 arguments, got 1",
  info = "Fixed-arity wrappers enforce under-arity with stable error text"
)
expect_error(
  ffi$add(1L, 2L, 3L),
  "Expected 2 arguments, got 3",
  info = "Fixed-arity wrappers enforce over-arity with stable error text"
)

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

# Test 7: Array return type
ffi <- tcc_ffi() |>
  tcc_bind(
    dup_array = list(
      args = list("integer_array", "i32"),
      returns = list(type = "integer_array", length_arg = 2, free = TRUE)
    )
  ) |>
  tcc_source(
    "
    #include <stdlib.h>
    int* dup_array(int* arr, int n) {
      if (n <= 0) return NULL;
      int* out = (int*)malloc(sizeof(int) * n);
      for (int i = 0; i < n; i++) out[i] = arr[i] * 2;
      return out;
    }
  "
  ) |>
  tcc_compile()

expect_true(inherits(ffi, "tcc_compiled"))
result <- ffi$dup_array(as.integer(c(1, 2, 3)), 3L)
expect_equal(result, c(2L, 4L, 6L))

# Test 8: Missing wrapper bindings fail fast (no partially-broken object)
state_bind_fail <- tcc_state(output = "memory")
expect_equal(
  tcc_compile_string(
    state_bind_fail,
    "
    int R_wrap_existing(void) {
      return 1;
    }
    "
  ),
  0L
)
expect_equal(tcc_relocate(state_bind_fail), 0L)

expect_error(
  Rtinycc:::tcc_compiled_object(
    state_bind_fail,
    symbols = list(missing = list(args = list(), returns = "i32")),
    output = "memory",
    structs = NULL,
    unions = NULL,
    enums = NULL,
    globals = NULL,
    container_of = NULL,
    field_addr = NULL,
    struct_raw_access = NULL,
    introspect = NULL
  ),
  "Failed to bind compiled wrapper symbols",
  info = "Missing wrappers should raise a hard binding error"
)
