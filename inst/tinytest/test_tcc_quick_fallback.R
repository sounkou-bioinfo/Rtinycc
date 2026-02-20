# tcc_quick fallback and boundary detection tests

library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# --- .Call boundary → fallback ---

unsupported <- function(x) {
  declare(type(x = double(1)))
  .Call("some_native_symbol", x)
}

fallback_fn <- suppressWarnings(tcc_quick(unsupported, fallback = "always"))
expect_true(identical(fallback_fn, unsupported))

expect_error(
  tcc_quick(unsupported, fallback = "never"),
  pattern = "outside the current tcc_quick subset"
)

# --- .Primitive boundary → fallback ---

boundary_internal <- function(x) {
  declare(type(x = double(1)))
  .Primitive("sqrt")(x)
}

boundary_fallback <- suppressWarnings(
  tcc_quick(boundary_internal, fallback = "always")
)
expect_true(identical(boundary_fallback, boundary_internal))

expect_error(
  tcc_quick(boundary_internal, fallback = "never"),
  pattern = "outside the current tcc_quick subset"
)

# --- Nested boundary → fallback ---

boundary_nested <- function(x) {
  declare(type(x = double(1)))
  abs(.Primitive("sqrt")(x))
}

expect_true(identical(
  suppressWarnings(tcc_quick(boundary_nested, fallback = "always")),
  boundary_nested
))

# --- hard/soft policy for rf_call ---

needs_rf_call <- function(x) {
  declare(type(x = double(1)))
  mean(c(x, x + 1))
}

expect_error(
  tcc_quick(needs_rf_call, fallback = "hard"),
  pattern = "hard fallback mode forbids Rf_eval"
)

needs_rf_call_soft <- tcc_quick(needs_rf_call, fallback = "soft")
expect_true(is.function(needs_rf_call_soft))
expect_true(!identical(needs_rf_call_soft, needs_rf_call))
expect_equal(needs_rf_call_soft(2), needs_rf_call(2), tolerance = 1e-12)

is_na_reduce <- function(x) {
  declare(type(x = double(NA)))
  sum(is.na(x))
}

is_na_reduce_soft <- tcc_quick(is_na_reduce, fallback = "soft")
x_na <- c(1, NA, 3, NA)
expect_equal(is_na_reduce_soft(x_na), is_na_reduce(x_na), tolerance = 1e-12)

# --- delegated diagnostics can be toggled explicitly ---

{
  opts_old <- options(rtinycc.tcc_quick.rf_call_messages = TRUE)
  on.exit(options(opts_old), add = TRUE)
  expect_true(Rtinycc:::tcc_quick_rf_call_should_message("c"))
  expect_false(Rtinycc:::tcc_quick_rf_call_should_message("%*%"))

  options(rtinycc.tcc_quick.rf_call_messages = FALSE)
  expect_false(Rtinycc:::tcc_quick_rf_call_should_message("c"))
}

# --- IR validation: non-literal na.rm rejected in hard mode ---

bad_na_rm <- function(x, flag) {
  declare(type(x = double(NA)), type(flag = logical(1)))
  mean(x, na.rm = flag)
}

expect_error(
  tcc_quick(bad_na_rm, fallback = "hard"),
  pattern = "outside the current tcc_quick subset|na.rm must be literal"
)

# --- unsupported optional args must not be silently ignored ---

mean_trim <- function(x) {
  declare(type(x = double(NA)))
  mean(x, trim = 0.25)
}

expect_true(identical(
  suppressWarnings(tcc_quick(mean_trim, fallback = "soft")),
  mean_trim
))
expect_error(
  tcc_quick(mean_trim, fallback = "hard"),
  pattern = "unsupported argument 'trim'|outside the current tcc_quick subset"
)

quantile_type <- function(x, p) {
  declare(type(x = double(NA)), type(p = double(NA)))
  quantile(x, probs = p, type = 1)
}

expect_true(identical(
  suppressWarnings(tcc_quick(quantile_type, fallback = "soft")),
  quantile_type
))
expect_error(
  tcc_quick(quantile_type, fallback = "hard"),
  pattern = "unsupported argument 'type'|outside the current tcc_quick subset"
)

# --- which.max/min must not lower non-variable or scalar inputs ---

which_max_expr <- function(x) {
  declare(type(x = double(NA)))
  which.max(x + 1)
}

expect_true(identical(
  suppressWarnings(tcc_quick(which_max_expr, fallback = "soft")),
  which_max_expr
))
expect_error(
  tcc_quick(which_max_expr, fallback = "hard"),
  pattern = "named vector variable|outside the current tcc_quick subset"
)

which_max_scalar <- function(x) {
  declare(type(x = double(1)))
  which.max(x)
}

expect_true(identical(
  suppressWarnings(tcc_quick(which_max_scalar, fallback = "soft")),
  which_max_scalar
))
expect_error(
  tcc_quick(which_max_scalar, fallback = "hard"),
  pattern = "requires a vector argument|outside the current tcc_quick subset"
)

# --- seq_len requires scalar length (vector input should fallback) ---

seq_len_vector <- function(x) {
  declare(type(x = double(NA)))
  seq_len(x)
}

expect_true(identical(
  suppressWarnings(tcc_quick(seq_len_vector, fallback = "soft")),
  seq_len_vector
))
expect_error(
  tcc_quick(seq_len_vector, fallback = "hard"),
  pattern = "scalar length argument|outside the current tcc_quick subset"
)

# --- rf_call allowlist enforcement ---

not_allowlisted_call <- function(x) {
  declare(type(x = double(1)))
  identity(x)
}

expect_true(identical(
  suppressWarnings(tcc_quick(not_allowlisted_call, fallback = "soft")),
  not_allowlisted_call
))

expect_warning(
  tcc_quick(not_allowlisted_call, fallback = "soft"),
  pattern = "Unsupported function call: identity"
)

expect_error(
  tcc_quick(not_allowlisted_call, fallback = "hard"),
  pattern = "outside the current tcc_quick subset|Unsupported function call"
)

# --- allowlisted call without explicit delegated contract ---

no_contract_allowlisted <- function(x) {
  declare(type(x = double(1)))
  Sys.time()
}

expect_true(identical(
  suppressWarnings(tcc_quick(no_contract_allowlisted, fallback = "soft")),
  no_contract_allowlisted
))

expect_warning(
  tcc_quick(no_contract_allowlisted, fallback = "soft"),
  pattern = "Delegated call contract unavailable"
)

expect_error(
  tcc_quick(no_contract_allowlisted, fallback = "hard"),
  pattern = "outside the current tcc_quick subset|Delegated call contract unavailable"
)

# --- delegated shape contract keeps scalar is.na scalar ---

is_na_scalar <- function(x) {
  declare(type(x = double(1)))
  is.na(x)
}

is_na_scalar_soft <- tcc_quick(is_na_scalar, fallback = "soft")
expect_equal(is_na_scalar_soft(NA_real_), TRUE)
expect_equal(is_na_scalar_soft(1.0), FALSE)

# --- matrix operands in elementwise ops should fallback, not crash codegen ---

matrix_vec_minus <- function(X, y, b) {
  declare(
    type(X = double(NA, NA)),
    type(y = double(NA)),
    type(b = double(NA, NA))
  )
  y - X %*% b
}

expect_true(identical(
  suppressWarnings(tcc_quick(matrix_vec_minus, fallback = "soft")),
  matrix_vec_minus
))

expect_error(
  tcc_quick(matrix_vec_minus, fallback = "hard"),
  pattern = "outside the current tcc_quick subset|Matrix operand for operator '-'"
)

# --- omitted-index matrix assignment should fallback cleanly (no internal crash) ---

matrix_col_assign <- function(X, v) {
  declare(type(X = double(NA, NA)), type(v = double(1)))
  X[, 1L] <- v
  X
}

expect_true(identical(
  suppressWarnings(tcc_quick(matrix_col_assign, fallback = "soft")),
  matrix_col_assign
))

expect_error(
  tcc_quick(matrix_col_assign, fallback = "hard"),
  pattern = "outside the current tcc_quick subset|omitted indices"
)

# --- named/extra subscript args should fallback, not be misparsed ---

matrix_drop_false <- function(X) {
  declare(type(X = double(NA, NA)))
  X[, 1L, drop = FALSE]
}

expect_true(identical(
  suppressWarnings(tcc_quick(matrix_drop_false, fallback = "soft")),
  matrix_drop_false
))

expect_error(
  tcc_quick(matrix_drop_false, fallback = "hard"),
  pattern = "outside the current tcc_quick subset|one- and two-dimensional|Named subscript argument"
)

# --- declare() dimensions must be integer-valued ---

bad_fractional_dim <- function(x) {
  declare(type(x = double(2.5)))
  x
}

expect_error(
  tcc_quick(bad_fractional_dim, fallback = "soft"),
  pattern = "integer-valued"
)

na_integer_dim <- function(x) {
  declare(type(x = double(NA_integer_)))
  x
}

decl_na_integer <- Rtinycc:::tcc_quick_parse_declare(na_integer_dim)
expect_true(identical(decl_na_integer$args$x$dims[[1L]], NA_integer_))

# raw arithmetic is intentionally disallowed; use bitw* helpers or explicit casts
raw_add_bad <- function(x, y) {
  declare(type(x = raw(1)), type(y = raw(1)))
  x + y
}

expect_error(
  tcc_quick(raw_add_bad, fallback = "hard"),
  pattern = "raw mode is not supported"
)

# rank-3+ arrays are reserved for upcoming multidimensional support
rank3_decl <- function(x) {
  declare(type(x = double(NA, NA, NA)))
  x
}

expect_true(identical(
  suppressWarnings(tcc_quick(rank3_decl, fallback = "soft")),
  rank3_decl
))

expect_error(
  tcc_quick(rank3_decl, fallback = "hard"),
  pattern = "Rank-3\\+ array declarations"
)

# --- cache should not capture wrong lexical environment across same-body funcs ---

rm(list = ls(envir = Rtinycc:::tcc_quick_cache_env), envir = Rtinycc:::tcc_quick_cache_env)

make_is_na_quick <- function(flag) {
  local({
    `is.na` <- function(x) rep(flag, length(x))
    fn <- function(x) {
      declare(type(x = double(NA)))
      is.na(x)
    }
    tcc_quick(fn, fallback = "soft")
  })
}

is_na_true <- make_is_na_quick(TRUE)
is_na_false <- make_is_na_quick(FALSE)
probe <- c(1, NA_real_, 3)

expect_identical(is_na_true(probe), c(TRUE, TRUE, TRUE))
expect_identical(is_na_false(probe), c(FALSE, FALSE, FALSE))
