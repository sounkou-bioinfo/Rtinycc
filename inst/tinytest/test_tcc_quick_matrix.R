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
for (i in 1:3) for (j in 1:4) expected[i, j] <- i * 10 + j
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
