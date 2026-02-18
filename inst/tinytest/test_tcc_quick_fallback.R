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

expect_true(is.function(tcc_quick(needs_rf_call, fallback = "soft")))

# --- IR validation: non-literal na.rm rejected in hard mode ---

bad_na_rm <- function(x, flag) {
  declare(type(x = double(NA)), type(flag = logical(1)))
  mean(x, na.rm = flag)
}

expect_error(
  tcc_quick(bad_na_rm, fallback = "hard"),
  pattern = "outside the current tcc_quick subset|na.rm must be literal"
)
