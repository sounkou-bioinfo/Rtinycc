# Tests for extended tcc_quick operations:
# registry-driven math (arity 1 & 2), reductions (any/all),
# cumulative ops, pmin/pmax, rev, stop, input mutation safety.

library(Rtinycc)
declare <- function(...) invisible(NULL)
type <- function(...) NULL

# ---------------------------------------------------------------------------
# Arity-1: registry-driven math functions (new ones beyond the original 15)
# ---------------------------------------------------------------------------

tq_sinh <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    sinh(x)
})
expect_equal(tq_sinh(1.0), sinh(1.0))

tq_cosh <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    cosh(x)
})
expect_equal(tq_cosh(1.0), cosh(1.0))

tq_atanh <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    atanh(x)
})
expect_equal(tq_atanh(0.5), atanh(0.5))

tq_log1p <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    log1p(x)
})
expect_equal(tq_log1p(0.01), log1p(0.01))

tq_expm1 <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    expm1(x)
})
expect_equal(tq_expm1(0.01), expm1(0.01))

tq_log2 <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    log2(x)
})
expect_equal(tq_log2(8.0), log2(8.0))

tq_sign <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    sign(x)
})
expect_equal(tq_sign(-3.0), sign(-3.0))
expect_equal(tq_sign(0.0), sign(0.0))
expect_equal(tq_sign(5.0), sign(5.0))

# ---------------------------------------------------------------------------
# Rmath functions
# ---------------------------------------------------------------------------

tq_digamma <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    digamma(x)
})
expect_equal(tq_digamma(2.0), digamma(2.0), tolerance = 1e-12)

tq_trigamma <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    trigamma(x)
})
expect_equal(tq_trigamma(2.0), trigamma(2.0), tolerance = 1e-12)

tq_gamma_fn <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    gamma(x)
})
expect_equal(tq_gamma_fn(5.0), gamma(5.0), tolerance = 1e-12)

tq_lgamma_fn <- tcc_quick(function(x) {
    declare(type(x = double(1)))
    lgamma(x)
})
expect_equal(tq_lgamma_fn(10.0), lgamma(10.0), tolerance = 1e-12)

# ---------------------------------------------------------------------------
# Arity-2: registry-driven binary math
# ---------------------------------------------------------------------------

tq_atan2 <- tcc_quick(function(y, x) {
    declare(type(y = double(1)), type(x = double(1)))
    atan2(y, x)
})
expect_equal(tq_atan2(1.0, 1.0), atan2(1.0, 1.0))

tq_beta <- tcc_quick(function(a, b) {
    declare(type(a = double(1)), type(b = double(1)))
    beta(a, b)
})
expect_equal(tq_beta(2.0, 3.0), beta(2.0, 3.0), tolerance = 1e-12)

tq_choose <- tcc_quick(function(n, k) {
    declare(type(n = double(1)), type(k = double(1)))
    choose(n, k)
})
expect_equal(tq_choose(10, 3), choose(10, 3))

# ---------------------------------------------------------------------------
# pmin / pmax (element-wise binary on vectors)
# ---------------------------------------------------------------------------

tq_pmin <- tcc_quick(function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    pmin(x, y)
})
expect_equal(tq_pmin(c(1, 5, 3), c(2, 4, 6)), pmin(c(1, 5, 3), c(2, 4, 6)))

tq_pmax <- tcc_quick(function(x, y) {
    declare(type(x = double(NA)), type(y = double(NA)))
    pmax(x, y)
})
expect_equal(tq_pmax(c(1, 5, 3), c(2, 4, 6)), pmax(c(1, 5, 3), c(2, 4, 6)))

# ---------------------------------------------------------------------------
# any / all reductions
# ---------------------------------------------------------------------------

tq_any <- tcc_quick(function(x) {
    declare(type(x = logical(NA)))
    any(x)
})
expect_true(tq_any(c(FALSE, TRUE, FALSE)))
expect_false(tq_any(c(FALSE, FALSE, FALSE)))

tq_all <- tcc_quick(function(x) {
    declare(type(x = logical(NA)))
    all(x)
})
expect_true(tq_all(c(TRUE, TRUE, TRUE)))
expect_false(tq_all(c(TRUE, FALSE, TRUE)))

# ---------------------------------------------------------------------------
# rev
# ---------------------------------------------------------------------------

tq_rev <- tcc_quick(function(x) {
    declare(type(x = double(NA)))
    rev(x)
})
expect_equal(tq_rev(c(1, 2, 3, 4, 5)), rev(c(1, 2, 3, 4, 5)))

# ---------------------------------------------------------------------------
# cumsum / cumprod / cummax / cummin
# ---------------------------------------------------------------------------

tq_cumsum <- tcc_quick(function(x) {
    declare(type(x = double(NA)))
    cumsum(x)
})
expect_equal(tq_cumsum(c(1, 2, 3, 4, 5)), cumsum(c(1, 2, 3, 4, 5)))

tq_cumprod <- tcc_quick(function(x) {
    declare(type(x = double(NA)))
    cumprod(x)
})
expect_equal(tq_cumprod(c(1, 2, 3, 4)), cumprod(c(1, 2, 3, 4)))

tq_cummax <- tcc_quick(function(x) {
    declare(type(x = double(NA)))
    cummax(x)
})
expect_equal(tq_cummax(c(3, 1, 4, 1, 5, 9, 2, 6)), cummax(c(3, 1, 4, 1, 5, 9, 2, 6)))

tq_cummin <- tcc_quick(function(x) {
    declare(type(x = double(NA)))
    cummin(x)
})
expect_equal(tq_cummin(c(5, 3, 4, 1, 2)), cummin(c(5, 3, 4, 1, 2)))

# ---------------------------------------------------------------------------
# Input mutation safety: modifying input param should not affect caller
# ---------------------------------------------------------------------------

tq_mutate_input <- tcc_quick(function(x) {
    declare(type(x = double(NA)))
    x[1L] <- 999.0
    x
})
original <- c(1.0, 2.0, 3.0)
result <- tq_mutate_input(original)
expect_equal(result, c(999.0, 2.0, 3.0))
# The original vector must be untouched
expect_equal(original, c(1.0, 2.0, 3.0))

# ---------------------------------------------------------------------------
# Vector element-wise arity-2: atan2 on vectors
# ---------------------------------------------------------------------------

tq_atan2_vec <- tcc_quick(function(y, x) {
    declare(type(y = double(NA)), type(x = double(NA)))
    atan2(y, x)
})
y_vals <- c(1, 0, -1)
x_vals <- c(0, 1, 0)
expect_equal(tq_atan2_vec(y_vals, x_vals), atan2(y_vals, x_vals))
