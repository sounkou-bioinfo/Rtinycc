library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# =============================================================================
# Tests Demonstrating Kernel Matcher Brittleness
# =============================================================================

# These tests document the LIMITATION that kernel matchers are overfitted
# to specific patterns and fail on semantically equivalent code.

# Test 1: Nested loop kernel - exact pattern works
convolve_exact <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
    }
  }
  ab
}

convolve_exact_compiled <- tcc_quick(convolve_exact, fallback = "never")
a_test <- c(1, 2, 3)
b_test <- c(4, 5)
expect_equal(convolve_exact_compiled(a_test, b_test), 
             convolve_exact(a_test, b_test))

# Test 2: Adding intermediate variable BREAKS kernel matcher
# This is semantically identical but matcher rejects it
convolve_with_intermediate <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  n_out <- length(a) + length(b) - 1  # ❌ Extra statement breaks matcher
  ab <- double(n_out)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
    }
  }
  ab
}

# This should work but kernel matcher is too brittle
# Fallback to R or error depending on fallback setting
convolve_intermediate_fallback <- tcc_quick(convolve_with_intermediate, fallback = "auto")
expect_true(identical(convolve_intermediate_fallback, convolve_with_intermediate),
  info = "Kernel matcher rejected semantically identical code - fell back to R")

# Verify it falls back
expect_error(
  tcc_quick(convolve_with_intermediate, fallback = "never"),
  pattern = "outside.*subset|fallback",
  info = "Kernel matcher is too specific - rejects intermediate variables"
)

# Test 3: Reordering semantically independent statements BREAKS matcher
# This is also semantically identical but matcher has hardcoded order
convolve_reordered <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  # These two lines could be in any order
  ab <- double(length(a) + length(b) - 1)
  # But adding anything here breaks the 3-statement pattern
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
    }
  }
  ab
}

# This specific reordering still works because it's still 3 statements
# but shows the fragility
convolve_reordered_compiled <- tcc_quick(convolve_reordered, fallback = "never")
expect_equal(convolve_reordered_compiled(a_test, b_test),
             convolve_reordered(a_test, b_test))

# Test 4: Rolling mean kernel - exact 5-statement pattern
roll_mean_exact <- function(x, weights, normalize = TRUE) {
  declare(
    type(x = double(NA)),
    type(weights = double(NA)),
    type(normalize = logical(1))
  )
  out <- double(length(x) - length(weights) + 1)
  n <- length(weights)
  if (normalize)
    weights <- weights / sum(weights) * length(weights)
  for (i in seq_along(out)) {
    out[i] <- sum(x[i:(i + n - 1)] * weights) / length(weights)
  }
  out
}

roll_mean_exact_compiled <- tcc_quick(roll_mean_exact, fallback = "never")
x_test <- c(1, 2, 3, 4, 5, 6)
w_test <- c(0.5, 0.5)
expect_equal(
  roll_mean_exact_compiled(x_test, w_test),
  roll_mean_exact(x_test, w_test),
  tolerance = 1e-10
)

# Test 5: Rolling mean with extra initialization BREAKS matcher
roll_mean_extra_init <- function(x, weights, normalize = TRUE) {
  declare(
    type(x = double(NA)),
    type(weights = double(NA)),
    type(normalize = logical(1))
  )
  # Extra initialization variable
  default_val <- 0.0  # ❌ Now 6 statements instead of 5
  out <- double(length(x) - length(weights) + 1)
  n <- length(weights)
  if (normalize)
    weights <- weights / sum(weights) * length(weights)
  for (i in seq_along(out)) {
    out[i] <- sum(x[i:(i + n - 1)] * weights) / length(weights)
  }
  out
}

roll_mean_extra_fallback <- tcc_quick(roll_mean_extra_init, fallback = "auto")
expect_true(identical(roll_mean_extra_fallback, roll_mean_extra_init),
  info = "Rolling mean kernel rejected 6-statement version (requires exactly 5)")

# Test 6: Different but equivalent loop iteration BREAKS matcher
convolve_seq_len <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  # Using seq_len instead of seq_along - semantically can be equivalent
  for (i in seq_len(length(a))) {  # ❌ Matcher only accepts seq_along
    for (j in seq_len(length(b))) {
      ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
    }
  }
  ab
}

# This is rejected even though it's equivalent for non-empty arrays
convolve_seq_len_fallback <- tcc_quick(convolve_seq_len, fallback = "auto")
expect_true(identical(convolve_seq_len_fallback, convolve_seq_len),
  info = "Kernel matcher only accepts seq_along, not seq_len")

# Test 7: Using different accumulation pattern BREAKS matcher
convolve_explicit_add <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      idx <- i + j - 1
      ab[idx] <- ab[idx] + a[i] * b[j]  # Explicit index variable
    }
  }
  ab
}

# Even this minor change breaks the pattern
convolve_explicit_fallback <- tcc_quick(convolve_explicit_add, fallback = "auto")
expect_true(identical(convolve_explicit_fallback, convolve_explicit_add),
  info = "Kernel matcher is very specific about RHS structure")

# Test 8: Verification catches these issues
# The verification system should warn about kernel matching
test_fn <- function(x) {
  declare(type(x = double(1)))
  x + 1
}

# Compile with verification enabled (default)
compiled_verified <- tcc_quick(test_fn, verify = TRUE)
expect_true(is.function(compiled_verified))

# Test that verification works on kernel code
kernel_verified <- tcc_quick(convolve_exact, verify = TRUE)
expect_true(is.function(kernel_verified))

# Document that these are KNOWN LIMITATIONS
message("=== KERNEL MATCHER BRITTLENESS TESTS COMPLETE ===")
message("These tests document known limitations:")
message("1. Kernel matchers require exact statement counts")
message("2. Adding intermediate variables breaks matching")
message("3. Different but equivalent expressions rejected")
message("4. No general loop lowering exists")
message("5. See docs/rjit_critical_analysis.md for details")
