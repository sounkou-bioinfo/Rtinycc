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
