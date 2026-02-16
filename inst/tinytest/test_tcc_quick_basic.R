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

max_dbl <- function(x, y) {
    declare(type(x = double(1)), type(y = double(1)))
    max(x, y)
}

max_fast <- tcc_quick(max_dbl)
expect_equal(max_fast(2.5, 1.5), 2.5)

unsupported <- function(x) {
    declare(type(x = double(1)))
    sqrt(x)
}

fallback_fn <- tcc_quick(unsupported, fallback = "always")
expect_true(identical(fallback_fn, unsupported))

expect_error(
    tcc_quick(unsupported, fallback = "never"),
    pattern = "outside tcc_quick MVP subset"
)
