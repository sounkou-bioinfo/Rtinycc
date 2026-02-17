# tcc_quick scalar expression tests

library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# --- Scalar arithmetic ---

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

# --- Complex expression with if/else ---

complex_expr <- function(x, y, z) {
    declare(type(x = double(1)), type(y = double(1)), type(z = double(1)))
    if ((x * y) > z) (x * y) + (z / 2) else (z - x) / (y + 1)
}
complex_fast <- tcc_quick(complex_expr)
expect_equal(complex_fast(4, 3, 5), complex_expr(4, 3, 5))
expect_equal(complex_fast(1, 2, 8), complex_expr(1, 2, 8))

# --- Logical operators ---

logic_expr <- function(x, y) {
    declare(type(x = integer(1)), type(y = integer(1)))
    (x > y) || (x == y)
}
logic_fast <- tcc_quick(logic_expr)
expect_equal(logic_fast(4L, 2L), TRUE)
expect_equal(logic_fast(1L, 2L), FALSE)

# --- Math intrinsics ---

math_expr <- function(x) {
    declare(type(x = double(1)))
    abs(sin(x)) + sqrt(x)
}
math_fast <- tcc_quick(math_expr)
expect_equal(math_fast(4.0), math_expr(4.0), tolerance = 1e-12)

# --- Block with assignments ---

block_expr <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    a <- x * y
    b <- a + 2
    ifelse(b > 10, b, 10)
}
block_fast <- tcc_quick(block_expr)
expect_equal(block_fast(2.0, 3.0), block_expr(2.0, 3.0))
expect_equal(block_fast(3.0, 4.0), block_expr(3.0, 4.0))

# --- sqrt (native, not fallback) ---

sqrt_native <- function(x) {
    declare(type(x = double(1)))
    sqrt(x)
}
sqrt_fast <- tcc_quick(sqrt_native)
expect_equal(sqrt_fast(9.0), sqrt_native(9.0), tolerance = 1e-12)
