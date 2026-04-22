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
expect_true(Rtinycc:::is_rtinycc_bound_symbol(ffi$symbols$add))
expect_equal(ffi$symbols$add$name, "add")
expect_true(all(vapply(
  ffi$symbols$add$arg_type_info,
  Rtinycc:::is_rtinycc_ffi_type,
  logical(1)
)))
expect_equal(
  Rtinycc:::ffi_type_family(ffi$symbols$add$return_spec$type_info),
  "i32"
)

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

# Test 5b: scalar wrappers use the intended R-side carrier types
ffi_scalar_carriers <- tcc_ffi() |>
  tcc_bind(
    id_i32 = list(args = list("i32"), returns = "i32"),
    id_u16 = list(args = list("u16"), returns = "u16"),
    id_u32 = list(args = list("u32"), returns = "u32"),
    id_i64 = list(args = list("i64"), returns = "i64"),
    id_u64 = list(args = list("u64"), returns = "u64"),
    id_f32 = list(args = list("f32"), returns = "f32"),
    id_f64 = list(args = list("f64"), returns = "f64")
  ) |>
  tcc_source(
    "
    #include <stdint.h>

    int32_t id_i32(int32_t x) { return x; }
    uint16_t id_u16(uint16_t x) { return x; }
    uint32_t id_u32(uint32_t x) { return x; }
    int64_t id_i64(int64_t x) { return x; }
    uint64_t id_u64(uint64_t x) { return x; }
    float id_f32(float x) { return x; }
    double id_f64(double x) { return x; }
  "
  ) |>
  tcc_compile()

expect_identical(typeof(ffi_scalar_carriers$id_i32(7L)), "integer")
expect_identical(typeof(ffi_scalar_carriers$id_u16(65535L)), "integer")
expect_identical(typeof(ffi_scalar_carriers$id_u32(4294967295)), "double")
expect_identical(typeof(ffi_scalar_carriers$id_i64(2^53)), "double")
expect_identical(typeof(ffi_scalar_carriers$id_u64(2^53)), "double")
expect_identical(typeof(ffi_scalar_carriers$id_f32(1.25)), "double")
expect_identical(typeof(ffi_scalar_carriers$id_f64(1.25)), "double")

expect_equal(ffi_scalar_carriers$id_u32(4294967295), 4294967295)
expect_equal(ffi_scalar_carriers$id_i64(2^53), 2^53)
expect_equal(ffi_scalar_carriers$id_u64(2^53), 2^53)

expect_error(
  ffi_scalar_carriers$id_u32(1.5),
  "u32 requires integer value"
)
expect_error(
  ffi_scalar_carriers$id_i64(2^53 + 2),
  "i64 requires exact integer"
)
expect_error(
  ffi_scalar_carriers$id_u64(2^53 + 2),
  "u64 requires exact integer"
)

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
ffi_spec <- tcc_ffi() |>
  tcc_bind(
    dup_array = list(
      args = list("integer_array", "i32"),
      returns = list(type = "integer_array", length_arg = 2, free = TRUE)
    )
  )

expect_true(inherits(
  ffi_spec$symbols$dup_array$return_spec,
  "rtinycc_symbol_return_spec"
))
expect_equal(ffi_spec$symbols$dup_array$return_spec$type, "integer_array")
expect_equal(ffi_spec$symbols$dup_array$return_spec$length_arg, 2)
expect_true(isTRUE(ffi_spec$symbols$dup_array$return_spec$free))

ffi <- ffi_spec |>
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

# Test 8: helper specs are normalized to classed symbol specs too
ffi_helpers <- tcc_ffi() |>
  tcc_source("struct point { int x; int y; };") |>
  tcc_struct("point", accessors = c(x = "i32", y = "i32")) |>
  tcc_compile()
helper_specs <- get(".helper_specs", envir = ffi_helpers, inherits = FALSE)
expect_true(Rtinycc:::is_rtinycc_bound_symbol(helper_specs$struct_point_new))
expect_true(Rtinycc:::is_rtinycc_bound_symbol(helper_specs$struct_point_get_x))
expect_true(inherits(helper_specs$struct_point_new, "rtinycc_helper_symbol"))
expect_identical(
  Rtinycc:::helper_symbol_kind(helper_specs$struct_point_new),
  "struct"
)
expect_identical(
  Rtinycc:::helper_symbol_kind(helper_specs$struct_point_get_x),
  "struct"
)
expect_identical(
  Rtinycc:::helper_symbol_operation(helper_specs$struct_point_new),
  "constructor"
)
expect_identical(
  Rtinycc:::helper_symbol_operation(helper_specs$struct_point_get_x),
  "getter"
)
expect_identical(
  Rtinycc:::ffi_type_family(
    helper_specs$struct_point_get_x$return_spec$type_info
  ),
  "sexp"
)

# Test 9: Missing wrapper bindings fail fast (no partially-broken object)
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
