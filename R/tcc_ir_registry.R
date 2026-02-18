# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# R → C function registry.  Each entry maps an R name to a C name, arity,
# and optional header.  The lowerer uses this for data-driven dispatch:
# arity-1 → call1 IR, arity-2 → call2 IR.

tcc_ir_c_api_registry <- function() {
  list(
    # --- math.h (arity 1) ---
    abs = list(c_fun = "fabs", arity = 1L),
    sqrt = list(c_fun = "sqrt", arity = 1L),
    sin = list(c_fun = "sin", arity = 1L),
    cos = list(c_fun = "cos", arity = 1L),
    tan = list(c_fun = "tan", arity = 1L),
    asin = list(c_fun = "asin", arity = 1L),
    acos = list(c_fun = "acos", arity = 1L),
    atan = list(c_fun = "atan", arity = 1L),
    exp = list(c_fun = "exp", arity = 1L),
    log = list(c_fun = "log", arity = 1L),
    log10 = list(c_fun = "log10", arity = 1L),
    log2 = list(c_fun = "log2", arity = 1L),
    log1p = list(c_fun = "log1p", arity = 1L),
    expm1 = list(c_fun = "expm1", arity = 1L),
    floor = list(c_fun = "floor", arity = 1L),
    ceiling = list(c_fun = "ceil", arity = 1L),
    trunc = list(c_fun = "trunc", arity = 1L),
    tanh = list(c_fun = "tanh", arity = 1L),
    sinh = list(c_fun = "sinh", arity = 1L),
    cosh = list(c_fun = "cosh", arity = 1L),
    asinh = list(c_fun = "asinh", arity = 1L),
    acosh = list(c_fun = "acosh", arity = 1L),
    atanh = list(c_fun = "atanh", arity = 1L),
    # --- math.h (arity 2) ---
    atan2 = list(c_fun = "atan2", arity = 2L),
    hypot = list(c_fun = "hypot", arity = 2L),
    # --- Rmath.h (arity 1) ---
    gamma = list(c_fun = "gammafn", arity = 1L, header = "Rmath.h"),
    lgamma = list(c_fun = "lgammafn", arity = 1L, header = "Rmath.h"),
    digamma = list(c_fun = "digamma", arity = 1L, header = "Rmath.h"),
    trigamma = list(c_fun = "trigamma", arity = 1L, header = "Rmath.h"),
    factorial = list(
      c_fun = "gammafn",
      arity = 1L,
      header = "Rmath.h",
      transform = "x+1"
    ),
    lfactorial = list(
      c_fun = "lgammafn",
      arity = 1L,
      header = "Rmath.h",
      transform = "x+1"
    ),
    # --- Rmath.h (arity 2) ---
    beta = list(c_fun = "beta", arity = 2L, header = "Rmath.h"),
    lbeta = list(c_fun = "lbeta", arity = 2L, header = "Rmath.h"),
    choose = list(c_fun = "choose", arity = 2L, header = "Rmath.h"),
    lchoose = list(c_fun = "lchoose", arity = 2L, header = "Rmath.h"),
    # --- R internals ---
    sign = list(c_fun = "sign", arity = 1L, header = "Rmath.h")
  )
}

# Convenience: look up one entry; returns NULL if not found.
tcc_ir_registry_lookup <- function(fname) {
  reg <- tcc_ir_c_api_registry()
  reg[[fname]]
}

# Functions explicitly permitted to lower as rf_call fallback nodes.
# Calls outside this list are treated as outside the current tcc_quick subset.
tcc_quick_rf_call_allowlist <- function() {
  c(
    "%*%",
    "crossprod",
    "tcrossprod",
    "t",
    "solve",
    "qr",
    "qr.solve",
    "qr.coef",
    "qr.resid",
    "qr.fitted",
    "qr.Q",
    "qr.R",
    "qr.X",
    "svd",
    "eigen",
    "chol",
    "chol2inv",
    "backsolve",
    "forwardsolve",
    "det",
    "norm",
    "rcond",
    "kappa",
    "diag",
    "outer",
    "rowSums",
    "colSums",
    "rowMeans",
    "colMeans",
    "drop",
    "dim",
    "nchar",
    "paste",
    "paste0",
    "array",
    "rbind",
    "cbind",
    "rep",
    "rep_len",
    "is.na",
    "is.finite",
    "is.infinite",
    "is.nan",
    "which",
    "tabulate",
    "table",
    "match",
    "sort",
    "order",
    "rank",
    "duplicated",
    "unique",
    "sprintf",
    "format",
    "cat",
    "print",
    "message",
    "warning",
    "list",
    "c",
    "unlist",
    "do.call",
    "Recall",
    "Sys.time",
    "proc.time"
  )
}

# rf_call entries that should not emit a fallback diagnostic message.
tcc_quick_rf_call_quiet <- function() {
  c("%*%", "crossprod", "tcrossprod")
}

# Load exported R API symbol table bundled in inst/RAPI/API.csv
tcc_rapi_table <- function() {
  p <- system.file("RAPI", "API.csv", package = "Rtinycc")
  if (!nzchar(p) || !file.exists(p)) {
    return(data.frame(
      name = character(0),
      type = character(0),
      apitype = character(0),
      stringsAsFactors = FALSE
    ))
  }
  utils::read.csv(p, stringsAsFactors = FALSE)
}

tcc_rapi_has <- function(sym) {
  tab <- tcc_rapi_table()
  any(tab$name == sym)
}

tcc_rapi_summary <- function() {
  tab <- tcc_rapi_table()
  if (nrow(tab) == 0L) {
    return(data.frame(
      category = character(0),
      count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  classify <- function(nm) {
    if (
      grepl(
        "^(d|s|z|c).*(gemm|gemv|trsm|trmm|syrk|potrf|gesv|getrf|getri)$",
        nm
      )
    ) {
      return("BLAS/LAPACK")
    }
    if (
      grepl("_rand$|^GetRNGstate$|^PutRNGstate$|^R_unif_index$|^rmultinom$", nm)
    ) {
      return("RNG")
    }
    if (
      nm %in%
        c(
          "gammafn",
          "lgammafn",
          "digamma",
          "trigamma",
          "beta",
          "lbeta",
          "choose",
          "lchoose",
          "R_pow",
          "R_pow_di",
          "log1p",
          "expm1",
          "sinpi",
          "cospi",
          "tanpi"
        )
    ) {
      return("Rmath")
    }
    if (
      nm %in%
        c(
          "R_rsort",
          "R_qsort",
          "R_qsort_I",
          "rsort_with_index",
          "R_orderVector",
          "R_orderVector1"
        )
    ) {
      return("Sorting")
    }
    "Other"
  }

  cats <- vapply(tab$name, classify, character(1))
  out <- as.data.frame(table(cats), stringsAsFactors = FALSE)
  names(out) <- c("category", "count")
  out$count <- as.integer(out$count)
  out[order(out$category), , drop = FALSE]
}
