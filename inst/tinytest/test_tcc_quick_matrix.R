# tcc_quick matrix tests

library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# --- Matrix allocation and 2D indexing ---

mat_fill <- function(n, m) {
  declare(type(n = integer(1)), type(m = integer(1)))
  A <- matrix(0, n, m)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      A[i, j] <- i * 10 + j
    }
  }
  A
}

f_mat <- tcc_quick(mat_fill, fallback = "never")
result <- f_mat(3L, 4L)
expected <- matrix(0, 3, 4)
for (i in 1:3) {
  for (j in 1:4) {
    expected[i, j] <- i * 10 + j
  }
}
expect_equal(result, expected)

# --- Matrix sum with nrow/ncol ---

mat_sum <- function(A) {
  declare(type(A = double(NA, NA)))
  nr <- nrow(A)
  nc <- ncol(A)
  s <- 0
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      s <- s + A[i, j]
    }
  }
  s
}

f_msum <- tcc_quick(mat_sum, fallback = "never")
A <- matrix(1:12, 3, 4)
expect_equal(f_msum(A), sum(A))

# --- BLAS-backed matrix products ---

mat_ops <- function(A, B) {
  declare(type(A = double(NA, NA)), type(B = double(NA, NA)))
  A %*% B
}

f_matops <- tcc_quick(mat_ops, fallback = "never")
A2 <- matrix(runif(12), 3, 4)
B2 <- matrix(runif(20), 4, 5)
expect_equal(f_matops(A2, B2), A2 %*% B2, tolerance = 1e-10)

cp_ops <- function(A, B) {
  declare(type(A = double(NA, NA)), type(B = double(NA, NA)))
  crossprod(A, B)
}

f_cp <- tcc_quick(cp_ops, fallback = "never")
A3 <- matrix(runif(15), 5, 3)
B3 <- matrix(runif(20), 5, 4)
expect_equal(f_cp(A3, B3), crossprod(A3, B3), tolerance = 1e-10)

tcp_ops <- function(A, B) {
  declare(type(A = double(NA, NA)), type(B = double(NA, NA)))
  tcrossprod(A, B)
}

f_tcp <- tcc_quick(tcp_ops, fallback = "never")
A4 <- matrix(runif(12), 3, 4)
B4 <- matrix(runif(20), 5, 4)
expect_equal(f_tcp(A4, B4), tcrossprod(A4, B4), tolerance = 1e-10)

# --- matrix product dimension mismatch errors ---

expect_error(
  f_matops(matrix(runif(6), 2, 3), matrix(runif(8), 4, 2)),
  pattern = "dimension mismatch"
)

# --- one-arg crossprod/tcrossprod ---

cp1 <- function(A) {
  declare(type(A = double(NA, NA)))
  crossprod(A)
}

tcp1 <- function(A) {
  declare(type(A = double(NA, NA)))
  tcrossprod(A)
}

f_cp1 <- tcc_quick(cp1, fallback = "never")
f_tcp1 <- tcc_quick(tcp1, fallback = "never")
A5 <- matrix(runif(15), 5, 3)
expect_equal(f_cp1(A5), crossprod(A5), tolerance = 1e-10)
expect_equal(f_tcp1(A5), tcrossprod(A5), tolerance = 1e-10)

# --- LAPACK-backed solve(A, b) ---

solve_vec <- function(A, b) {
  declare(type(A = double(NA, NA)), type(b = double(NA)))
  solve(A, b)
}

f_solve_vec <- tcc_quick(solve_vec, fallback = "never")
A6 <- crossprod(matrix(rnorm(36), 6, 6)) + diag(6) * 0.5
b6 <- rnorm(6)
expect_equal(
  as.numeric(f_solve_vec(A6, b6)),
  as.numeric(solve(A6, b6)),
  tolerance = 1e-10
)

solve_mat <- function(A, B) {
  declare(type(A = double(NA, NA)), type(B = double(NA, NA)))
  solve(A, B)
}

f_solve_mat <- tcc_quick(solve_mat, fallback = "never")
B6 <- matrix(rnorm(18), 6, 3)
expect_equal(f_solve_mat(A6, B6), solve(A6, B6), tolerance = 1e-10)

expect_error(
  f_solve_vec(matrix(runif(12), 3, 4), rnorm(3)),
  pattern = "square matrix"
)

# --- native transpose t(A) ---

t_mat <- function(A) {
  declare(type(A = double(NA, NA)))
  t(A)
}

f_t_mat <- tcc_quick(t_mat, fallback = "never")
A7 <- matrix(runif(21), 7, 3)
expect_equal(f_t_mat(A7), t(A7), tolerance = 1e-12)

t_int_mat <- function(A) {
  declare(type(A = integer(NA, NA)))
  t(A)
}

f_t_int_mat <- tcc_quick(t_int_mat, fallback = "never")
A8 <- matrix(sample.int(100L, 20L, replace = TRUE), 5, 4)
expect_equal(f_t_int_mat(A8), t(A8))

# --- native row/col reducers ---

row_sums_native <- function(A) {
  declare(type(A = double(NA, NA)))
  rowSums(A)
}

col_sums_native <- function(A) {
  declare(type(A = double(NA, NA)))
  colSums(A)
}

row_means_native <- function(A) {
  declare(type(A = double(NA, NA)))
  rowMeans(A)
}

col_means_native <- function(A) {
  declare(type(A = double(NA, NA)))
  colMeans(A)
}

f_row_sums_native <- tcc_quick(row_sums_native, fallback = "never")
f_col_sums_native <- tcc_quick(col_sums_native, fallback = "never")
f_row_means_native <- tcc_quick(row_means_native, fallback = "never")
f_col_means_native <- tcc_quick(col_means_native, fallback = "never")

A9 <- matrix(rnorm(84), 12, 7)
expect_equal(f_row_sums_native(A9), rowSums(A9), tolerance = 1e-12)
expect_equal(f_col_sums_native(A9), colSums(A9), tolerance = 1e-12)
expect_equal(f_row_means_native(A9), rowMeans(A9), tolerance = 1e-12)
expect_equal(f_col_means_native(A9), colMeans(A9), tolerance = 1e-12)

row_means_na_native <- function(A) {
  declare(type(A = double(NA, NA)))
  rowMeans(A, na.rm = TRUE)
}

f_row_means_na_native <- tcc_quick(row_means_na_native, fallback = "never")
A10 <- A9
A10[3, 2] <- NA_real_
A10[8, 5] <- NA_real_
expect_equal(
  f_row_means_na_native(A10),
  rowMeans(A10, na.rm = TRUE),
  tolerance = 1e-12
)
