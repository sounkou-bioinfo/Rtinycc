# tcc_quick vector and loop tests

library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# --- Convolution (nested for-loop) ---

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
expect_equal(quick_convolve(a, b), slow_convolve(a, b), tolerance = 1e-12)

# --- Code-only mode ---

code_only <- tcc_quick(slow_convolve, mode = "code")
expect_true(is.character(code_only))
expect_true(grepl("SEXP tcc_quick_entry", code_only, fixed = TRUE))

# --- Rolling sum via 2:n (seq_range) ---

rolling_sum <- function(x) {
    declare(type(x = double(NA)))
    n <- length(x)
    out <- double(n)
    out[1L] <- x[1L]
    for (i in 2:n) {
        out[i] <- out[i - 1L] + x[i]
    }
    out
}

f_rsum <- tcc_quick(rolling_sum, fallback = "never")
x <- c(1, 2, 3, 4, 5)
expect_equal(f_rsum(x), cumsum(x))

# --- Element-wise vector ops (return expression) ---

vec_add <- function(a, b) {
    declare(type(a = double(NA)), type(b = double(NA)))
    a + b
}

f_vadd <- tcc_quick(vec_add, fallback = "never")
expect_equal(f_vadd(c(1, 2, 3), c(10, 20, 30)), c(11, 22, 33))

# --- Manual sum loop ---

manual_sum <- function(x) {
    declare(type(x = double(NA)))
    s <- 0
    for (i in seq_along(x)) {
        s <- s + x[i]
    }
    s
}

f_msum <- tcc_quick(manual_sum, fallback = "never")
expect_equal(f_msum(c(1, 2, 3, 4, 5)), 15)

# --- max index (if/else in loop body) ---

max_index <- function(x) {
    declare(type(x = double(NA)))
    best <- x[1L]
    idx <- 1L
    for (i in seq_along(x)) {
        if (x[i] > best) {
            best <- x[i]
            idx <- i
        }
    }
    idx
}

f_maxidx <- tcc_quick(max_index, fallback = "never")
expect_equal(f_maxidx(c(3, 7, 1, 9, 2)), 4L)
