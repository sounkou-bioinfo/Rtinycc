# tcc_quick control flow tests (while, repeat, break, next)

library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# --- While loop (Collatz sequence) ---

collatz_len <- function(n) {
    declare(type(n = integer(1)))
    steps <- 0L
    while (n > 1L) {
        if (n %% 2L == 0L) {
            n <- n / 2L
        } else {
            n <- 3L * n + 1L
        }
        steps <- steps + 1L
    }
    steps
}

f_collatz <- tcc_quick(collatz_len, fallback = "never")
collatz_r <- function(n) {
    steps <- 0L
    while (n > 1L) {
        if (n %% 2L == 0L) n <- n / 2L else n <- 3L * n + 1L
        steps <- steps + 1L
    }
    steps
}
expect_equal(f_collatz(27L), collatz_r(27L))
expect_equal(f_collatz(1L), 0L)

# --- Repeat with break ---

repeat_count <- function(n) {
    declare(type(n = integer(1)))
    i <- 0L
    repeat {
        if (i >= n) break
        i <- i + 1L
    }
    i
}

f_repeat <- tcc_quick(repeat_count, fallback = "never")
expect_equal(f_repeat(5L), 5L)
expect_equal(f_repeat(0L), 0L)

# --- For loop with next ---

count_odd <- function(n) {
    declare(type(n = integer(1)))
    count <- 0L
    for (i in seq_len(n)) {
        if (i %% 2L == 0L) next
        count <- count + 1L
    }
    count
}

f_odd <- tcc_quick(count_odd, fallback = "never")
expect_equal(f_odd(10L), 5L)
expect_equal(f_odd(1L), 1L)
