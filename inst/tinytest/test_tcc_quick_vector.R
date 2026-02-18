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

# --- mean/sd native loops ---

mean_sd <- function(x) {
  declare(type(x = double(NA)))
  mean(x) + sd(x)
}

f_mean_sd <- tcc_quick(mean_sd, fallback = "never")
x2 <- c(1, 2, 3, 4, 5)
expect_equal(f_mean_sd(x2), mean(x2) + sd(x2), tolerance = 1e-12)

# --- median / quantile scalar-prob native loops ---

median_q <- function(x, p) {
  declare(type(x = double(NA)), type(p = double(1)))
  median(x) + quantile(x, p)
}

f_median_q <- tcc_quick(median_q, fallback = "never")
x3 <- c(9, 1, 6, 3, 4, 2)
expect_equal(
  f_median_q(x3, 0.25),
  as.double(median(x3) + quantile(x3, 0.25)),
  tolerance = 1e-12
)

# quantile edge probs
expect_equal(
  f_median_q(x3, 0.0),
  as.double(median(x3) + quantile(x3, 0.0)),
  tolerance = 1e-12
)
expect_equal(
  f_median_q(x3, 1.0),
  as.double(median(x3) + quantile(x3, 1.0)),
  tolerance = 1e-12
)

# quantile invalid prob should error in compiled path
expect_error(f_median_q(x3, 1.5), pattern = "probs must be in")

# sd length-1 edge case: should return NA_real_
sd_len1 <- function(x) {
  declare(type(x = double(NA)))
  sd(x)
}

f_sd1 <- tcc_quick(sd_len1, fallback = "never")
expect_true(is.na(f_sd1(c(42))))

# na.rm support across reducers
stats_na_rm <- function(x) {
  declare(type(x = double(NA)))
  mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE) + median(x, na.rm = TRUE)
}

f_stats_na_rm <- tcc_quick(stats_na_rm, fallback = "never")
x4 <- c(1, 2, NA, 4, 7)
expect_equal(
  f_stats_na_rm(x4),
  mean(x4, na.rm = TRUE) + sd(x4, na.rm = TRUE) + median(x4, na.rm = TRUE),
  tolerance = 1e-12
)

# quantile vector probs
quant_vec <- function(x, p) {
  declare(type(x = double(NA)), type(p = double(NA)))
  quantile(x, probs = p)
}

f_quant_vec <- tcc_quick(quant_vec, fallback = "never")
p4 <- c(0, 0.25, 0.5, 0.75, 1)
expect_equal(
  as.numeric(f_quant_vec(x3, p4)),
  as.numeric(quantile(x3, p4)),
  tolerance = 1e-12
)

# quantile vector probs + na.rm
quant_vec_na <- function(x, p) {
  declare(type(x = double(NA)), type(p = double(NA)))
  quantile(x, probs = p, na.rm = TRUE)
}

f_quant_vec_na <- tcc_quick(quant_vec_na, fallback = "never")
expect_equal(
  as.numeric(f_quant_vec_na(c(x3, NA), p4)),
  as.numeric(quantile(c(x3, NA), p4, na.rm = TRUE)),
  tolerance = 1e-12
)

# raw vectors
raw_copy <- function(x) {
  declare(type(x = raw(NA)))
  n <- length(x)
  y <- raw(n)
  for (i in seq_len(n)) {
    y[i] <- x[i]
  }
  y
}

f_raw_copy <- tcc_quick(raw_copy, fallback = "never")
xraw <- as.raw(sample.int(255L, 64L, replace = TRUE) - 1L)
expect_equal(f_raw_copy(xraw), raw_copy(xraw))

raw_from_int <- function(x) {
  declare(type(x = integer(NA)))
  n <- length(x)
  y <- raw(n)
  for (i in seq_len(n)) {
    y[i] <- as.raw(x[i])
  }
  y
}

f_raw_from_int <- tcc_quick(raw_from_int, fallback = "never")
xint <- sample.int(255L, 64L, replace = TRUE) - 1L
expect_equal(f_raw_from_int(xint), raw_from_int(xint))

# sapply typed subset (symbol FUN)
sapply_sqrt <- function(x) {
  declare(type(x = double(NA)))
  sapply(x, sqrt)
}

f_sapply_sqrt <- tcc_quick(sapply_sqrt, fallback = "never")
xs <- runif(32)
expect_equal(f_sapply_sqrt(xs), sapply_sqrt(xs), tolerance = 1e-12)

sapply_raw <- function(x) {
  declare(type(x = integer(NA)))
  sapply(x, as.raw)
}

f_sapply_raw <- tcc_quick(sapply_raw, fallback = "never")
xis <- sample.int(255L, 32L, replace = TRUE) - 1L
expect_equal(f_sapply_raw(xis), sapply_raw(xis))

# apply typed delegated subset
apply_row_sums <- function(X) {
  declare(type(X = double(NA, NA)))
  apply(X, 1, sum)
}

f_apply_row_sums <- tcc_quick(apply_row_sums, fallback = "soft")
Xm <- matrix(runif(50), nrow = 10)
expect_equal(f_apply_row_sums(Xm), apply_row_sums(Xm), tolerance = 1e-12)

apply_col_means_na <- function(X) {
  declare(type(X = double(NA, NA)))
  apply(X, 2, mean, na.rm = TRUE)
}

f_apply_col_means_na <- tcc_quick(apply_col_means_na, fallback = "soft")
Xm2 <- matrix(runif(30), nrow = 6)
Xm2[2, 3] <- NA_real_
expect_equal(
  f_apply_col_means_na(Xm2),
  apply_col_means_na(Xm2),
  tolerance = 1e-12
)

# raw bitwise ops
raw_mask <- function(x) {
  declare(type(x = raw(NA)))
  n <- length(x)
  y <- raw(n)
  for (i in seq_len(n)) {
    y[i] <- as.raw(bitwAnd(as.integer(x[i]), 15L))
  }
  y
}

f_raw_mask <- tcc_quick(raw_mask, fallback = "never")
xr <- as.raw(sample.int(255L, 64L, replace = TRUE) - 1L)
expect_equal(f_raw_mask(xr), raw_mask(xr))
