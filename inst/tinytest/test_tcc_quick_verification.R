library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# Test IR invariant verification
test_ir_valid <- function(x) {
  declare(type(x = double(1)))
  x + 1
}

test_ir <- tcc_quick_lower(test_ir_valid, tcc_quick_parse_declare(test_ir_valid))
verification <- tcc_quick_verify_ir_invariants(test_ir, tcc_quick_parse_declare(test_ir_valid))
expect_true(verification$ok, info = paste(verification$violations, collapse = "; "))

# Test IR with undeclared variable should fail
test_ir_invalid <- list(
  tag = "scalar_expr",
  expr = list(tag = "var", name = "undeclared_var"),
  result_mode = "double",
  arg_modes = list()
)
decl_empty <- list(args = list(), formal_names = character(0))
verification_bad <- tcc_quick_verify_ir_invariants(test_ir_invalid, decl_empty)
expect_false(verification_bad$ok)
expect_true(length(verification_bad$violations) > 0)

# Test PROTECT/UNPROTECT balance checking
balanced_c <- "
#include <R.h>
#include <Rinternals.h>

SEXP test(SEXP x) {
  SEXP result = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(result)[0] = 42.0;
  UNPROTECT(1);
  return result;
}
"

balance_ok <- tcc_quick_check_protect_balance(balanced_c)
expect_true(balance_ok$ok)

# Test unbalanced PROTECT/UNPROTECT
unbalanced_c <- "
#include <R.h>
#include <Rinternals.h>

SEXP test(SEXP x) {
  SEXP result = PROTECT(Rf_allocVector(REALSXP, 1));
  SEXP other = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(result)[0] = 42.0;
  UNPROTECT(1);  // Only unprot one but protected two
  return result;
}
"

balance_bad <- tcc_quick_check_protect_balance(unbalanced_c)
expect_false(balance_bad$ok)
expect_true(length(balance_bad$violations) > 0)

# Test C code validation detects missing includes
bad_includes_c <- "
SEXP test(SEXP x) {
  return x;
}
"

validation_bad <- tcc_quick_validate_generated_c(bad_includes_c, list(), list())
expect_false(validation_bad$ok)
expect_true(any(grepl("Missing required include", validation_bad$violations)))

# Test semantic equivalence checking
test_add <- function(x, y) {
  declare(type(x = double(1)), type(y = double(1)))
  x + y
}

test_add_compiled <- tcc_quick(test_add)
decl_add <- tcc_quick_parse_declare(test_add)

equivalence <- tcc_quick_test_equivalence(
  test_add, 
  test_add_compiled, 
  decl_add,
  n_tests = 10
)
expect_true(equivalence$ok, 
  info = sprintf("Equivalence test failed: %d/%d tests", 
    equivalence$n_failures, equivalence$n_tests))

# Test that verification catches actual codegen output
test_scalar <- function(x) {
  declare(type(x = double(1)))
  x * 2
}

test_scalar_ir <- tcc_quick_lower(test_scalar, tcc_quick_parse_declare(test_scalar))
test_scalar_c <- tcc_quick_codegen(test_scalar_ir, tcc_quick_parse_declare(test_scalar))

c_validation <- tcc_quick_validate_generated_c(
  test_scalar_c, 
  test_scalar_ir, 
  tcc_quick_parse_declare(test_scalar)
)
expect_true(c_validation$ok, 
  info = paste(c_validation$violations, collapse = "; "))

# Test property-based validation
properties <- tcc_quick_test_codegen_properties(
  test_scalar_ir,
  tcc_quick_parse_declare(test_scalar),
  test_scalar_c
)
expect_true(properties$ok,
  info = paste(properties$violations, collapse = "; "))

# Test kernel IR verification
# This tests the actual kernel matchers to see if they produce valid IR
slow_convolve <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
    }
  }
  ab
}

kernel_decl <- tcc_quick_parse_declare(slow_convolve)
kernel_ir <- tcc_quick_lower(slow_convolve, kernel_decl)

# Verify the kernel IR is valid
kernel_verification <- tcc_quick_verify_ir_invariants(kernel_ir, kernel_decl)
expect_true(kernel_verification$ok,
  info = paste(kernel_verification$violations, collapse = "; "))

# Verify the generated C for kernel
kernel_c <- tcc_quick_codegen(kernel_ir, kernel_decl)
kernel_c_validation <- tcc_quick_validate_generated_c(kernel_c, kernel_ir, kernel_decl)
expect_true(kernel_c_validation$ok,
  info = paste(kernel_c_validation$violations, collapse = "; "))

# Test that bounds validation is present in generated kernel code
expect_true(grepl("n_out.*<.*0", kernel_c) || grepl("if.*<.*0", kernel_c),
  info = "Kernel codegen should include negative length validation")

# Test random input generation
random_inputs <- tcc_quick_generate_random_inputs(kernel_decl)
expect_true("a" %in% names(random_inputs))
expect_true("b" %in% names(random_inputs))
expect_true(is.numeric(random_inputs$a))
expect_true(is.numeric(random_inputs$b))
expect_true(length(random_inputs$a) >= 5)
expect_true(length(random_inputs$b) >= 5)

# Test that verification catches loop kernel issues
# Create invalid loop kernel IR (missing input array declaration)
bad_kernel_ir <- list(
  tag = "loop_kernel",
  kind = "indexed_store",
  out = "result",
  out_len_expr = quote(10),
  input_arrays = c("nonexistent_array"),  # Not declared
  loop_vars = c("i"),
  loops = list(list(var = "i", kind = "seq_along", target = "nonexistent_array")),
  out_idx = quote(i),
  rhs = quote(1.0)
)

bad_kernel_verification <- tcc_quick_verify_ir_invariants(bad_kernel_ir, decl_empty)
expect_false(bad_kernel_verification$ok)
expect_true(any(grepl("undeclared", bad_kernel_verification$violations)))
