# tcc_quick generality checks (constraint-aware, metamorphic)

library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# Contracts remain explicit and constrained.
contracts <- Rtinycc:::tcc_quick_rf_call_contracts()
allowlisted <- Rtinycc:::tcc_quick_rf_call_allowlist()
expect_true(length(contracts) > 0L)
expect_true(all(names(contracts) %in% allowlisted))

# ---------------------------------------------------------------------------
# Metamorphic check 1: alpha-renaming invariance (same semantics, different ids)
# ---------------------------------------------------------------------------

roll_a <- function(x, w) {
  declare(type(x = double(NA)), type(w = double(NA)))
  n <- length(x)
  m <- length(w)
  out <- double(n - m + 1L)
  for (i in seq_len(n - m + 1L)) {
    acc <- 0
    for (j in seq_len(m)) {
      acc <- acc + x[i + j - 1L] * w[j]
    }
    out[i] <- acc
  }
  out
}

roll_b <- function(signal, kernel) {
  declare(type(signal = double(NA)), type(kernel = double(NA)))
  len_signal <- length(signal)
  len_kernel <- length(kernel)
  dest <- double(len_signal - len_kernel + 1L)
  for (ii in seq_len(len_signal - len_kernel + 1L)) {
    tmp <- 0
    for (jj in seq_len(len_kernel)) {
      tmp <- tmp + signal[ii + jj - 1L] * kernel[jj]
    }
    dest[ii] <- tmp
  }
  dest
}

fast_roll_a <- tcc_quick(roll_a, fallback = "hard")
fast_roll_b <- tcc_quick(roll_b, fallback = "hard")

set.seed(20260221)
vx <- runif(128)
vw <- runif(15)

expect_equal(fast_roll_a(vx, vw), roll_a(vx, vw), tolerance = 1e-12)
expect_equal(fast_roll_b(vx, vw), roll_b(vx, vw), tolerance = 1e-12)
expect_equal(fast_roll_a(vx, vw), fast_roll_b(vx, vw), tolerance = 1e-12)

# ---------------------------------------------------------------------------
# Metamorphic check 2: equivalent loop forms (seq_along vs seq_len(length()))
# ---------------------------------------------------------------------------

sum_seq_along <- function(x) {
  declare(type(x = double(NA)))
  s <- 0
  for (i in seq_along(x)) {
    s <- s + x[i]
  }
  s
}

sum_seq_len <- function(x) {
  declare(type(x = double(NA)))
  n <- length(x)
  s <- 0
  for (k in seq_len(n)) {
    s <- s + x[k]
  }
  s
}

fast_sum_a <- tcc_quick(sum_seq_along, fallback = "hard")
fast_sum_b <- tcc_quick(sum_seq_len, fallback = "hard")

expect_equal(fast_sum_a(vx), sum_seq_along(vx), tolerance = 1e-12)
expect_equal(fast_sum_b(vx), sum_seq_len(vx), tolerance = 1e-12)
expect_equal(fast_sum_a(vx), fast_sum_b(vx), tolerance = 1e-12)

# ---------------------------------------------------------------------------
# Generality check 3: codegen remains native (no delegated Rf_eval) in hard mode
# ---------------------------------------------------------------------------

native_templates <- list(
  function(a, b) {
    declare(type(a = double(NA)), type(b = double(NA)))
    sin(a) + cos(b)
  },
  function(a, b) {
    declare(type(a = double(NA)), type(b = double(NA)))
    pmax(a, b) - pmin(a, b)
  },
  function(a) {
    declare(type(a = double(NA)))
    rev(a)
  }
)

for (fn in native_templates) {
  src <- tcc_quick(fn, fallback = "hard", mode = "code")
  expect_false(grepl("Rf_eval\\(", src))
}
