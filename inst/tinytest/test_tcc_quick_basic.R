library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

add_dbl <- function(x, y) {
  declare(type(x = double(1)), type(y = double(1)))
  x + y
}

add_fast <- tcc_quick(add_dbl)
expect_equal(add_fast(1.25, 2.75), 4.0)

add_int <- function(x, y) {
  declare(type(x = integer(1)), type(y = integer(1)))
  x + y
}

add_int_fast <- tcc_quick(add_int)
expect_equal(add_int_fast(2L, 5L), 7L)

complex_expr <- function(x, y, z) {
  declare(type(x = double(1)), type(y = double(1)), type(z = double(1)))
  if ((x * y) > z) (x * y) + (z / 2) else (z - x) / (y + 1)
}

complex_fast <- tcc_quick(complex_expr)
expect_equal(complex_fast(4, 3, 5), complex_expr(4, 3, 5))
expect_equal(complex_fast(1, 2, 8), complex_expr(1, 2, 8))

logic_expr <- function(x, y) {
  declare(type(x = integer(1)), type(y = integer(1)))
  (x > y) || (x == y)
}

logic_fast <- tcc_quick(logic_expr)
expect_equal(logic_fast(4L, 2L), TRUE)
expect_equal(logic_fast(1L, 2L), FALSE)

math_expr <- function(x) {
  declare(type(x = double(1)))
  abs(sin(x)) + sqrt(x)
}

math_fast <- tcc_quick(math_expr)
expect_equal(math_fast(4.0), math_expr(4.0), tolerance = 1e-12)

block_expr <- function(x, y) {
  declare(type(x = double(1)), type(y = double(1)))
  a <- x * y
  b <- a + 2
  ifelse(b > 10, b, 10)
}

block_fast <- tcc_quick(block_expr)
expect_equal(block_fast(2.0, 3.0), block_expr(2.0, 3.0))
expect_equal(block_fast(3.0, 4.0), block_expr(3.0, 4.0))

switch_expr <- function(sel, x, y) {
  declare(type(sel = integer(1)), type(x = double(1)), type(y = double(1)))
  switch(sel, x + y, x - y, x * y)
}

switch_fast <- tcc_quick(switch_expr)
expect_equal(switch_fast(1L, 5.0, 2.0), switch_expr(1L, 5.0, 2.0))
expect_equal(switch_fast(2L, 5.0, 2.0), switch_expr(2L, 5.0, 2.0))
expect_equal(switch_fast(3L, 5.0, 2.0), switch_expr(3L, 5.0, 2.0))
expect_equal(switch_fast(9L, 5.0, 2.0), switch_expr(9L, 5.0, 2.0))

switch_named <- function(sel, x, y) {
  declare(type(sel = integer(1)), type(x = double(1)), type(y = double(1)))
  switch(sel, add = x + y, sub = x - y, x * y)
}

expect_true(identical(
  tcc_quick(switch_named, fallback = "always"),
  switch_named
))

max_dbl <- function(x, y) {
  declare(type(x = double(1)), type(y = double(1)))
  max(x, y)
}

max_fast <- tcc_quick(max_dbl)
expect_equal(max_fast(2.5, 1.5), 2.5)

sqrt_from_fallback <- function(x) {
  declare(type(x = double(1)))
  sqrt(x)
}

sqrt_fast <- tcc_quick(sqrt_from_fallback)
expect_equal(sqrt_fast(9.0), sqrt_from_fallback(9.0), tolerance = 1e-12)

pmax3 <- function(x, y, z) {
  declare(type(x = double(1)), type(y = double(1)), type(z = double(1)))
  pmax(x, y, z)
}

pmax3_fast <- tcc_quick(pmax3)
expect_equal(pmax3_fast(2.0, 9.0, 3.0), pmax3(2.0, 9.0, 3.0), tolerance = 1e-12)

pmax5 <- function(a, b, c, d, e) {
  declare(
    type(a = double(1)),
    type(b = double(1)),
    type(c = double(1)),
    type(d = double(1)),
    type(e = double(1))
  )
  pmax(a, b, c, d, e)
}

pmax5_fast <- tcc_quick(pmax5)
expect_equal(
  pmax5_fast(2.0, 9.0, 3.0, 4.0, 8.0),
  pmax5(2.0, 9.0, 3.0, 4.0, 8.0),
  tolerance = 1e-12
)

unsupported <- function(x) {
  declare(type(x = double(1)))
  .Call("some_native_symbol", x)
}

fallback_fn <- tcc_quick(unsupported, fallback = "always")
expect_true(identical(fallback_fn, unsupported))

expect_error(
  tcc_quick(unsupported, fallback = "never"),
  pattern = "outside the current tcc_quick subset"
)

boundary_internal <- function(x) {
  declare(type(x = double(1)))
  .Primitive("sqrt")(x)
}

boundary_fallback <- tcc_quick(boundary_internal, fallback = "always")
expect_true(identical(boundary_fallback, boundary_internal))

expect_error(
  tcc_quick(boundary_internal, fallback = "never"),
  pattern = "outside the current tcc_quick subset"
)

boundary_nested <- function(x) {
  declare(type(x = double(1)))
  abs(.Primitive("sqrt")(x))
}

expect_true(identical(
  tcc_quick(boundary_nested, fallback = "always"),
  boundary_nested
))

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

quick_convolve <- tcc_quick(slow_convolve, fallback = "never")

set.seed(42)
a <- runif(200)
b <- runif(20)

expect_equal(
  quick_convolve(a, b),
  slow_convolve(a, b),
  tolerance = 1e-12
)

code_only <- tcc_quick(slow_convolve, mode = "code")
expect_true(is.character(code_only))
expect_true(grepl("SEXP tcc_quick_entry", code_only, fixed = TRUE))

# Exact quickr README rolling mean example (construct tracking)
slow_roll_mean <- function(x, weights, normalize = TRUE) {
  declare(
    type(x = double(NA)),
    type(weights = double(NA)),
    type(normalize = logical(1))
  )
  out <- double(length(x) - length(weights) + 1)
  n <- length(weights)
  if (normalize) {
    weights <- weights / sum(weights) * length(weights)
  }

  for (i in seq_along(out)) {
    out[i] <- sum(x[i:(i + n - 1)] * weights) / length(weights)
  }
  out
}

quick_roll_mean <- tcc_quick(slow_roll_mean, fallback = "never")

x <- dnorm(seq(-3, 3, len = 2000))
weights <- dnorm(seq(-1, 1, len = 31))

expect_equal(
  quick_roll_mean(x, weights),
  slow_roll_mean(x, weights),
  tolerance = 1e-10
)
expect_equal(
  quick_roll_mean(x, weights, FALSE),
  slow_roll_mean(x, weights, FALSE),
  tolerance = 1e-10
)
