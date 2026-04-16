# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_quick_extract_exprs <- function(fn) {
  b <- body(fn)
  if (is.call(b) && tccq_is_call(b, "{")) {
    return(as.list(b)[-1])
  }
  list(b)
}

tcc_quick_parse_dim <- function(x) {
  x_name <- tccq_node_name(x)
  if (!is.null(x_name) && x_name %in% c("NA", "NA_integer_", "NA_real_")) {
    return(NA_integer_)
  }
  if (length(x) == 1L && is.logical(x) && is.na(x)) {
    return(NA_integer_)
  }
  if (length(x) == 1L && (is.integer(x) || is.double(x))) {
    if (is.na(x[[1L]]) && !is.nan(as.double(x[[1L]]))) {
      return(NA_integer_)
    }
    if (!is.finite(as.double(x))) {
      stop("declare() dimensions must be >= 1 or NA", call. = FALSE)
    }
    if (!identical(as.double(x), trunc(as.double(x)))) {
      stop(
        "declare() dimensions must be integer-valued (>= 1) or NA",
        call. = FALSE
      )
    }
    out <- as.integer(x)
    if (is.na(out) || out < 1L) {
      stop("declare() dimensions must be >= 1 or NA", call. = FALSE)
    }
    return(out[[1L]])
  }
  stop("Unsupported declare() dimension", call. = FALSE)
}

tcc_quick_parse_type_spec <- function(spec) {
  if (!is.call(spec)) {
    stop("declare(type(...)) entries must be calls", call. = FALSE)
  }

  type_name <- tccq_call_head(spec)
  if (is.null(type_name)) {
    stop("declare(type(...)) entries must be symbol calls", call. = FALSE)
  }
  type_name <- switch(type_name, numeric = "double", type_name)

  if (!type_name %in% c("double", "integer", "logical", "raw")) {
    stop(
      "tcc_quick current subset supports only double/integer/logical/raw declarations",
      call. = FALSE
    )
  }

  dims_expr <- as.list(spec)[-1]
  if (length(dims_expr) == 0) {
    stop("declare() type must include dimensions", call. = FALSE)
  }

  dims <- vapply(dims_expr, tcc_quick_parse_dim, integer(1))

  list(
    mode = type_name,
    dims = dims,
    rank = length(dims),
    is_scalar = length(dims) == 1L && !is.na(dims[[1]]) && dims[[1]] == 1L,
    is_matrix = length(dims) == 2L,
    is_array = length(dims) > 2L
  )
}

tcc_quick_parse_declare <- function(fn) {
  exprs <- tcc_quick_extract_exprs(fn)
  if (length(exprs) < 1L) {
    stop("Function body is empty", call. = FALSE)
  }

  d <- exprs[[1]]
  if (!(is.call(d) && tccq_is_call(d, "declare"))) {
    stop(
      "tcc_quick requires declare(type(...)) as the first expression",
      call. = FALSE
    )
  }

  decl_items <- as.list(d)[-1]
  if (length(decl_items) == 0L) {
    stop("declare() must contain at least one type() entry", call. = FALSE)
  }

  arg_decl <- list()
  for (entry in decl_items) {
    if (!(is.call(entry) && tccq_is_call(entry, "type"))) {
      stop("declare() currently supports only type(...) entries", call. = FALSE)
    }
    vars <- as.list(entry)[-1]
    vnames <- names(vars)
    if (
      length(vars) == 0L ||
        is.null(vnames) ||
        length(vnames) != length(vars) ||
        any(is.na(vnames)) ||
        any(!nzchar(vnames))
    ) {
      stop("type(...) entries must be named", call. = FALSE)
    }
    for (i in seq_along(vars)) {
      arg_decl[[vnames[[i]]]] <- tcc_quick_parse_type_spec(vars[[i]])
    }
  }

  formal_names <- names(formals(fn))
  if (!all(formal_names %in% names(arg_decl))) {
    miss <- setdiff(formal_names, names(arg_decl))
    stop(
      "Missing declare(type(...)) for arguments: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    args = arg_decl,
    formal_names = formal_names
  )
}
