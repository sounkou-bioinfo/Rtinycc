library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# Test NaN handling
nan_test <- function(x) {
  declare(type(x = double(1)))
  if (x > 0) x else -x
}

nan_fast <- tcc_quick(nan_test)
expect_true(is.nan(nan_fast(NaN)))

# Test Inf handling
inf_test <- function(x) {
  declare(type(x = double(1)))
  x + 1
}

inf_fast <- tcc_quick(inf_test)
expect_equal(inf_fast(Inf), Inf)
expect_equal(inf_fast(-Inf), -Inf)

# Test division by zero
div_zero <- function(x, y) {
  declare(type(x = double(1)), type(y = double(1)))
  x / y
}

div_zero_fast <- tcc_quick(div_zero)
expect_equal(div_zero_fast(1.0, 0.0), Inf)
expect_equal(div_zero_fast(-1.0, 0.0), -Inf)
expect_true(is.nan(div_zero_fast(0.0, 0.0)))

# Test very large numbers
large_test <- function(x) {
  declare(type(x = double(1)))
  x * 2
}

large_fast <- tcc_quick(large_test)
expect_equal(large_fast(1e308), 2e308)

# Test very small numbers
small_test <- function(x) {
  declare(type(x = double(1)))
  x / 2
}

small_fast <- tcc_quick(small_test)
expect_equal(small_fast(1e-308), 5e-309)

# Test integer overflow (should promote to double in arithmetic)
int_arith <- function(x, y) {
  declare(type(x = integer(1)), type(y = integer(1)))
  x * y
}

int_arith_fast <- tcc_quick(int_arith)
# R will handle integer overflow internally
expect_equal(int_arith_fast(1000L, 1000L), int_arith(1000L, 1000L))

# Test cache consistency with different formatting
cache_test1 <- function(x, y) {
  declare(type(x = double(1)), type(y = double(1)))
  x + y
}

cache_test2 <- function(x,y) {
  declare(type(x=double(1)),type(y=double(1)))
  x+y
}

fast1 <- tcc_quick(cache_test1)
fast2 <- tcc_quick(cache_test2)

# Both should work correctly
expect_equal(fast1(2.0, 3.0), 5.0)
expect_equal(fast2(2.0, 3.0), 5.0)

# Test logical NA handling
logical_na <- function(x) {
  declare(type(x = logical(1)))
  if (x) 1.0 else 0.0
}

# NA in logical should be caught by R's Rf_asLogical
# This tests that the generated code properly handles R's type system
logical_na_fast <- tcc_quick(logical_na)
expect_error(logical_na_fast(NA), pattern = "NA")

# Test empty vector edge case for kernels
# (should produce empty output)
empty_kernel <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
    }
  }
  ab
}

empty_fast <- tcc_quick(empty_kernel, fallback = "never")

# Empty input should produce empty output
a_empty <- numeric(0)
b_test <- c(1.0, 2.0)
result_empty <- empty_fast(a_empty, b_test)
expect_equal(length(result_empty), 0)

# Test negative output length validation
# This should be caught by our new validation
neg_len_kernel <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  # If length(a) < length(b), n_out would be negative
  ab <- double(length(a) - length(b) + 1)
  ab
}

# This should fail with our new validation
neg_len_fast <- tcc_quick(neg_len_kernel, fallback = "never", debug = FALSE)
a_short <- c(1.0, 2.0)
b_long <- c(1.0, 2.0, 3.0, 4.0, 5.0)
expect_error(
  neg_len_fast(a_short, b_long),
  pattern = "negative"
)

# Test switch with out-of-range selector
switch_oor <- function(sel, x, y) {
  declare(type(sel = integer(1)), type(x = double(1)), type(y = double(1)))
  switch(sel, x + y, x - y)
}

switch_oor_fast <- tcc_quick(switch_oor)
# Out-of-range should return NULL (R's default behavior)
expect_true(is.null(switch_oor_fast(99L, 5.0, 2.0)))
expect_true(is.null(switch_oor_fast(0L, 5.0, 2.0)))

# Test malformed declare - missing declaration
expect_error(
  tcc_quick(function(x, y) {
    declare(type(x = double(1)))  # y is missing
    x + y
  }),
  pattern = "Missing declare"
)

# Test malformed declare - no dimensions
expect_error(
  tcc_quick(function(x) {
    declare(type(x = double()))
    x
  }),
  pattern = "dimensions"
)

# Test incompatible type branches in if
incompatible_if <- function(x) {
  declare(type(x = double(1)))
  # This is actually compatible (double and integer promote to double)
  if (x > 0) x else 1L
}

# This should work due to type promotion
incompatible_if_fast <- tcc_quick(incompatible_if)
expect_equal(incompatible_if_fast(2.0), 2.0)
expect_equal(incompatible_if_fast(-1.0), 1.0)

# Test code mode with invalid function
code_mode_test <- function(x) {
  declare(type(x = double(1)))
  .Call("something", x)
}

expect_error(
  tcc_quick(code_mode_test, mode = "code", fallback = "never"),
  pattern = "No code generated"
)
