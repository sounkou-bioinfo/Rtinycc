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

is_windows <- identical(tolower(Sys.info()[["sysname"]]), "windows")

if (!is_windows) {
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
}
