# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Unified lowerer for tcc_quick.
#
# Produces IR tag = "fn_body": typed parameter declarations, local variable
# declarations, a list of statement nodes, and a return node.  Scalar-only
# functions are the simple case: fn_body with zero statements and a scalar
# return expression.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

tcc_quick_fallback_ir <- function(reason) {
  list(tag = "fallback", edge = "fallback_eval", reason = reason)
}

tcc_quick_boundary_calls <- function() {
  c(".Call", ".C", ".External", ".Internal", ".Primitive")
}

tcc_quick_promote_mode <- function(lhs, rhs) {
  if (lhs == rhs) {
    return(lhs)
  }
  if (lhs %in% c("double", "integer") && rhs %in% c("double", "integer")) {
    return("double")
  }
  NULL
}

tcc_quick_numeric_unary_funs <- function() {
  reg <- tcc_ir_c_api_registry()
  names(Filter(function(e) e$arity == 1L, reg))
}

tcc_quick_numeric_binary_funs <- function() {
  reg <- tcc_ir_c_api_registry()
  names(Filter(function(e) e$arity == 2L, reg))
}

tcc_quick_body_without_declare <- function(fn) {
  exprs <- tcc_quick_extract_exprs(fn)
  exprs[-1]
}

tcc_quick_verify_ir <- function(ir) {
  if (identical(ir$tag, "fallback")) {
    if (is.null(ir$edge)) ir$edge <- "fallback_eval"
  }
  ir
}

# ---------------------------------------------------------------------------
# Scope tracking
# ---------------------------------------------------------------------------
# A scope is an environment mapping variable names → info lists:
#   list(kind = "param"|"local"|"loop_var", mode, shape = "scalar"|"vector"|"matrix")

tccq_scope_new <- function(decl) {
  sc <- new.env(parent = emptyenv())
  for (nm in names(decl$args)) {
    spec <- decl$args[[nm]]
    shape <- if (isTRUE(spec$is_scalar)) {
      "scalar"
    } else if (isTRUE(spec$is_matrix)) {
      "matrix"
    } else {
      "vector"
    }
    sc[[nm]] <- list(
      kind = "param",
      mode = spec$mode,
      shape = shape,
      spec = spec
    )
  }
  sc
}

tccq_scope_get <- function(sc, name) {
  if (exists(name, envir = sc, inherits = FALSE)) {
    return(get(name, envir = sc, inherits = FALSE))
  }
  NULL
}

tccq_scope_set <- function(sc, name, info) {
  assign(name, info, envir = sc)
}

tccq_scope_mark_mutated <- function(sc, name) {
  info <- tccq_scope_get(sc, name)
  if (!is.null(info) && info$kind == "param") {
    info$mutated <- TRUE
    tccq_scope_set(sc, name, info)
  }
}

tccq_parse_na_rm <- function(e) {
  nms <- names(e)
  if (is.null(nms) || !"na.rm" %in% nms) {
    return(list(ok = TRUE, value = FALSE))
  }
  idx <- which(nms == "na.rm")[1]
  v <- e[[idx]]
  if (is.logical(v) && length(v) == 1L && !is.na(v)) {
    return(list(ok = TRUE, value = isTRUE(v)))
  }
  list(ok = FALSE, reason = "na.rm must be literal TRUE/FALSE")
}

tccq_parse_literal_int <- function(x) {
  if (length(x) == 1L && is.integer(x) && !is.na(x)) {
    return(as.integer(x)[[1]])
  }
  if (length(x) == 1L && is.double(x) && !is.na(x)) {
    v <- as.integer(x)
    if (!is.na(v) && as.double(v) == as.double(x)) {
      return(v[[1]])
    }
  }
  NA_integer_
}

# ---------------------------------------------------------------------------
# Expression lowering — returns list(ok, node, mode, shape, reason)
# ---------------------------------------------------------------------------

tccq_lower_result <- function(
  ok,
  node = NULL,
  mode = NULL,
  shape = "scalar",
  reason = NULL
) {
  list(ok = ok, node = node, mode = mode, shape = shape, reason = reason)
}

tccq_lower_expr <- function(e, sc, decl) {
  # --- Symbols ---
  if (is.symbol(e)) {
    nm <- as.character(e)
    info <- tccq_scope_get(sc, nm)
    if (is.null(info)) {
      return(tccq_lower_result(FALSE, reason = paste0("Unknown symbol: ", nm)))
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "var", name = nm),
      mode = info$mode,
      shape = info$shape
    ))
  }

  # --- Constants ---
  if (length(e) == 1L && is.integer(e)) {
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "const", value = as.integer(e)[[1]], mode = "integer"),
      mode = "integer"
    ))
  }
  if (length(e) == 1L && is.double(e)) {
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "const", value = as.double(e)[[1]], mode = "double"),
      mode = "double"
    ))
  }
  if (length(e) == 1L && is.logical(e) && !is.na(e)) {
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "const", value = isTRUE(e), mode = "logical"),
      mode = "logical"
    ))
  }
  if (length(e) == 1L && is.raw(e)) {
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "const", value = as.integer(e)[[1]], mode = "raw"),
      mode = "raw"
    ))
  }

  if (!is.call(e)) {
    return(tccq_lower_result(FALSE, reason = "Unsupported expression node"))
  }

  fun <- e[[1]]
  if (!is.symbol(fun)) {
    return(tccq_lower_result(FALSE, reason = "Only symbol calls supported"))
  }
  fname <- as.character(fun)

  # --- Boundary calls ---
  if (fname %in% tcc_quick_boundary_calls()) {
    return(tccq_lower_result(
      FALSE,
      reason = paste0("Boundary call encountered: ", fname)
    ))
  }

  # --- Parentheses ---
  if (fname == "(") {
    return(tccq_lower_expr(e[[2]], sc, decl))
  }

  # --- length() ---
  if (fname == "length" && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape == "scalar") {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "const", value = 1L, mode = "integer"),
        mode = "integer"
      ))
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "length", arr = x$node$name),
      mode = "integer"
    ))
  }

  # --- nrow / ncol ---
  if (fname == "nrow" && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "nrow", arr = x$node$name),
      mode = "integer"
    ))
  }
  if (fname == "ncol" && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "ncol", arr = x$node$name),
      mode = "integer"
    ))
  }

  # --- 1D subscript: x[i], x[a:b], or x[mask] ---
  if (fname == "[" && length(e) == 3L) {
    arr <- tccq_lower_expr(e[[2]], sc, decl)
    idx <- tccq_lower_expr(e[[3]], sc, decl)
    if (!arr$ok) {
      return(arr)
    }
    if (!idx$ok) {
      return(idx)
    }
    if (arr$shape == "scalar") {
      return(tccq_lower_result(FALSE, reason = "Cannot subscript a scalar"))
    }
    # Range subscript → vec_slice (view, no copy)
    if (idx$shape == "vector" && identical(idx$node$tag, "seq_range")) {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "vec_slice",
          arr = arr$node$name,
          from = idx$node$from,
          to = idx$node$to
        ),
        mode = arr$mode,
        shape = "vector"
      ))
    }
    # Logical mask subscript → vec_mask (count + fill)
    if (idx$shape == "vector" && idx$mode == "logical") {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "vec_mask",
          arr = arr$node$name,
          mask = idx$node
        ),
        mode = arr$mode,
        shape = "vector"
      ))
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "vec_get", arr = arr$node$name, idx = idx$node),
      mode = arr$mode
    ))
  }

  # --- 2D subscript: x[i, j] ---
  if (fname == "[" && length(e) == 4L) {
    arr <- tccq_lower_expr(e[[2]], sc, decl)
    row <- tccq_lower_expr(e[[3]], sc, decl)
    col <- tccq_lower_expr(e[[4]], sc, decl)
    if (!arr$ok) {
      return(arr)
    }
    if (!row$ok) {
      return(row)
    }
    if (!col$ok) {
      return(col)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(
        tag = "mat_get",
        arr = arr$node$name,
        row = row$node,
        col = col$node
      ),
      mode = arr$mode
    ))
  }

  # --- Unary ops ---
  if (fname %in% c("+", "-") && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "unary", op = fname, x = x$node),
      mode = x$mode,
      shape = x$shape
    ))
  }
  if (fname == "!" && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "unary", op = "!", x = x$node),
      mode = "logical",
      shape = x$shape
    ))
  }

  # --- if expression (ternary) ---
  if (fname == "if" && length(e) == 4L) {
    cond <- tccq_lower_expr(e[[2]], sc, decl)
    yes <- tccq_lower_expr(e[[3]], sc, decl)
    no <- tccq_lower_expr(e[[4]], sc, decl)
    if (!cond$ok) {
      return(cond)
    }
    if (!yes$ok) {
      return(yes)
    }
    if (!no$ok) {
      return(no)
    }
    out_mode <- tcc_quick_promote_mode(yes$mode, no$mode)
    if (is.null(out_mode)) {
      out_mode <- yes$mode
    }
    return(tccq_lower_result(
      TRUE,
      node = list(
        tag = "if",
        cond = cond$node,
        yes = yes$node,
        no = no$node
      ),
      mode = out_mode,
      shape = yes$shape
    ))
  }

  # --- ifelse(cond, yes, no) → same as if/else ---
  if (fname == "ifelse" && length(e) == 4L) {
    cond <- tccq_lower_expr(e[[2]], sc, decl)
    yes <- tccq_lower_expr(e[[3]], sc, decl)
    no <- tccq_lower_expr(e[[4]], sc, decl)
    if (!cond$ok) {
      return(cond)
    }
    if (!yes$ok) {
      return(yes)
    }
    if (!no$ok) {
      return(no)
    }
    out_mode <- tcc_quick_promote_mode(yes$mode, no$mode)
    if (is.null(out_mode)) {
      out_mode <- yes$mode
    }
    return(tccq_lower_result(
      TRUE,
      node = list(
        tag = "if",
        cond = cond$node,
        yes = yes$node,
        no = no$node
      ),
      mode = out_mode,
      shape = yes$shape
    ))
  }

  # --- Math intrinsics (arity 1, data-driven from registry) ---
  math_funs <- tcc_quick_numeric_unary_funs()
  if (fname %in% math_funs && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    entry <- tcc_ir_registry_lookup(fname)
    node <- list(tag = "call1", fun = fname, x = x$node)
    if (!is.null(entry$transform) && entry$transform == "x+1") {
      # factorial(n) → gamma(n+1)
      node$x <- list(
        tag = "binary",
        op = "+",
        lhs = x$node,
        rhs = list(tag = "const", value = 1.0, mode = "double")
      )
    }
    return(tccq_lower_result(
      TRUE,
      node = node,
      mode = "double",
      shape = x$shape
    ))
  }

  # --- Math intrinsics (arity 2, data-driven from registry) ---
  math_funs2 <- tcc_quick_numeric_binary_funs()
  if (fname %in% math_funs2 && length(e) == 3L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    y <- tccq_lower_expr(e[[3]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (!y$ok) {
      return(y)
    }
    out_shape <- if (x$shape == "vector" || y$shape == "vector") {
      "vector"
    } else {
      "scalar"
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "call2", fun = fname, x = x$node, y = y$node),
      mode = "double",
      shape = out_shape
    ))
  }

  # --- sapply(x, FUN): typed subset (symbol FUN only) ---
  if (fname == "sapply" && length(e) == 3L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape != "vector") {
      return(tccq_lower_result(
        FALSE,
        reason = "sapply() currently requires vector input"
      ))
    }
    fun_e <- e[[3]]
    if (!is.symbol(fun_e)) {
      return(tccq_lower_result(
        FALSE,
        reason = "sapply() FUN must be a symbol in current subset"
      ))
    }
    fun_name <- as.character(fun_e)

    if (fun_name %in% math_funs) {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "call1", fun = fun_name, x = x$node),
        mode = "double",
        shape = "vector"
      ))
    }

    if (fun_name %in% c("as.integer", "as.double", "as.numeric", "as.raw")) {
      target_mode <- if (fun_name == "as.integer") {
        "integer"
      } else if (fun_name == "as.raw") {
        "raw"
      } else {
        "double"
      }
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "cast", x = x$node, target_mode = target_mode),
        mode = target_mode,
        shape = "vector"
      ))
    }

    if (fun_name == "identity") {
      return(x)
    }

    return(tccq_lower_result(
      FALSE,
      reason = paste0("Unsupported sapply FUN in current subset: ", fun_name)
    ))
  }

  # --- apply(X, MARGIN, FUN): typed delegated subset ---
  if (fname == "apply" && length(e) >= 4L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape != "matrix") {
      return(tccq_lower_result(
        FALSE,
        reason = "apply() currently requires matrix input"
      ))
    }

    margin <- tccq_parse_literal_int(e[[3]])
    if (!margin %in% c(1L, 2L)) {
      return(tccq_lower_result(
        FALSE,
        reason = "apply() MARGIN must be literal 1 or 2"
      ))
    }
    fun_e <- e[[4]]
    if (!is.symbol(fun_e)) {
      return(tccq_lower_result(
        FALSE,
        reason = "apply() FUN must be a symbol in current subset"
      ))
    }
    fun_name <- as.character(fun_e)

    if (fun_name %in% c("sum", "mean")) {
      na <- tccq_parse_na_rm(e)
      if (!na$ok) {
        return(tccq_lower_result(FALSE, reason = na$reason))
      }
      delegated <- if (fun_name == "sum") {
        if (margin == 1L) "rowSums" else "colSums"
      } else {
        if (margin == 1L) "rowMeans" else "colMeans"
      }
      args <- list(c(x$node, list(mode = x$mode, shape = x$shape)))
      if (isTRUE(na$value)) {
        args[[length(args) + 1L]] <- list(
          tag = "const",
          value = TRUE,
          mode = "logical",
          shape = "scalar"
        )
      }
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "rf_call",
          fun = delegated,
          args = args,
          mode = "double",
          shape = "vector"
        ),
        mode = "double",
        shape = "vector"
      ))
    }
    return(tccq_lower_result(
      FALSE,
      reason = paste0("Unsupported apply FUN in current subset: ", fun_name)
    ))
  }

  # --- Reductions: sum, prod, max, min ---
  if (fname %in% c("sum", "prod", "max", "min") && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape == "vector" && identical(x$node$tag, "var")) {
      # Simple case: reduce a named variable (existing optimized path)
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "reduce", op = fname, arr = x$node$name),
        mode = x$mode
      ))
    }
    if (x$shape == "vector") {
      # General case: reduce any vector expression (composition)
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "reduce_expr", op = fname, expr = x$node),
        mode = x$mode
      ))
    }
    return(tccq_lower_result(TRUE, node = x$node, mode = x$mode))
  }

  # --- mean(x) ---
  if (fname == "mean" && length(e) >= 2L) {
    na_info <- tccq_parse_na_rm(e)
    if (!na_info$ok) {
      return(tccq_lower_result(FALSE, reason = na_info$reason))
    }
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape == "vector") {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "mean_expr", expr = x$node, na_rm = na_info$value),
        mode = "double"
      ))
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "cast", x = x$node, target_mode = "double"),
      mode = "double"
    ))
  }

  # --- median(x) ---
  if (fname == "median" && length(e) >= 2L) {
    na_info <- tccq_parse_na_rm(e)
    if (!na_info$ok) {
      return(tccq_lower_result(FALSE, reason = na_info$reason))
    }
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape == "vector") {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "median_expr", expr = x$node, na_rm = na_info$value),
        mode = "double"
      ))
    }
    return(tccq_lower_result(
      FALSE,
      reason = "median requires a vector argument"
    ))
  }

  # --- quantile(x, probs) scalar probs path ---
  if (fname == "quantile" && length(e) >= 3L) {
    nms <- names(e)
    p_idx <- 3L
    if (!is.null(nms) && any(nms == "probs")) {
      p_idx <- which(nms == "probs")[1]
    }
    na_info <- tccq_parse_na_rm(e)
    if (!na_info$ok) {
      return(tccq_lower_result(FALSE, reason = na_info$reason))
    }
    x <- tccq_lower_expr(e[[2]], sc, decl)
    p <- tccq_lower_expr(e[[p_idx]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (!p$ok) {
      return(p)
    }
    if (x$shape != "vector") {
      return(tccq_lower_result(
        FALSE,
        reason = "quantile requires a vector first argument"
      ))
    }
    if (p$shape == "scalar") {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "quantile_expr",
          expr = x$node,
          prob = p$node,
          na_rm = na_info$value
        ),
        mode = "double"
      ))
    }
    if (p$shape == "vector") {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "quantile_vec_expr",
          expr = x$node,
          probs = p$node,
          na_rm = na_info$value
        ),
        mode = "double",
        shape = "vector"
      ))
    }
    return(tccq_lower_result(
      FALSE,
      reason = "quantile probs must be scalar or vector"
    ))
  }

  # --- BLAS-backed matrix products ---
  if (fname == "%*%" && length(e) == 3L) {
    a <- tccq_lower_expr(e[[2]], sc, decl)
    b <- tccq_lower_expr(e[[3]], sc, decl)
    if (!a$ok) {
      return(a)
    }
    if (!b$ok) {
      return(b)
    }
    if (
      a$shape == "matrix" &&
        b$shape == "matrix" &&
        identical(a$node$tag, "var") &&
        identical(b$node$tag, "var")
    ) {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "matmul",
          a = a$node$name,
          b = b$node$name,
          trans_a = FALSE,
          trans_b = FALSE
        ),
        mode = "double",
        shape = "matrix"
      ))
    }
  }

  if (fname == "crossprod" && (length(e) == 2L || length(e) == 3L)) {
    a <- tccq_lower_expr(e[[2]], sc, decl)
    b <- if (length(e) == 3L) tccq_lower_expr(e[[3]], sc, decl) else a
    if (!a$ok) {
      return(a)
    }
    if (!b$ok) {
      return(b)
    }
    if (
      a$shape == "matrix" &&
        b$shape == "matrix" &&
        identical(a$node$tag, "var") &&
        identical(b$node$tag, "var")
    ) {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "matmul",
          a = a$node$name,
          b = b$node$name,
          trans_a = TRUE,
          trans_b = FALSE
        ),
        mode = "double",
        shape = "matrix"
      ))
    }
  }

  if (fname == "tcrossprod" && (length(e) == 2L || length(e) == 3L)) {
    a <- tccq_lower_expr(e[[2]], sc, decl)
    b <- if (length(e) == 3L) tccq_lower_expr(e[[3]], sc, decl) else a
    if (!a$ok) {
      return(a)
    }
    if (!b$ok) {
      return(b)
    }
    if (
      a$shape == "matrix" &&
        b$shape == "matrix" &&
        identical(a$node$tag, "var") &&
        identical(b$node$tag, "var")
    ) {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "matmul",
          a = a$node$name,
          b = b$node$name,
          trans_a = FALSE,
          trans_b = TRUE
        ),
        mode = "double",
        shape = "matrix"
      ))
    }
  }

  # --- LAPACK-backed linear solve: solve(A, b) ---
  if (fname == "solve" && length(e) == 3L) {
    a <- tccq_lower_expr(e[[2]], sc, decl)
    b <- tccq_lower_expr(e[[3]], sc, decl)
    if (!a$ok) {
      return(a)
    }
    if (!b$ok) {
      return(b)
    }
    if (
      a$shape == "matrix" &&
        b$shape %in% c("vector", "matrix") &&
        identical(a$node$tag, "var") &&
        identical(b$node$tag, "var") &&
        identical(a$mode, "double") &&
        identical(b$mode, "double")
    ) {
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "solve_lin",
          a = a$node$name,
          b = b$node$name,
          b_shape = b$shape
        ),
        mode = "double",
        shape = b$shape
      ))
    }
  }

  # --- sd(x) ---
  if (fname == "sd" && length(e) >= 2L) {
    na_info <- tccq_parse_na_rm(e)
    if (!na_info$ok) {
      return(tccq_lower_result(FALSE, reason = na_info$reason))
    }
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape == "vector") {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "sd_expr", expr = x$node, na_rm = na_info$value),
        mode = "double"
      ))
    }
    return(tccq_lower_result(
      FALSE,
      reason = "sd requires a vector argument"
    ))
  }

  # --- which.max / which.min ---
  if (fname %in% c("which.max", "which.min") && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "which_reduce", op = fname, arr = x$node$name),
      mode = "integer"
    ))
  }

  # --- Binary ops: arithmetic, comparison, logical ---
  arith_ops <- c("+", "-", "*", "/", "^", "%%", "%/%")
  cmp_ops <- c("<", "<=", ">", ">=", "==", "!=")
  log_ops <- c("&&", "||", "&", "|")

  if (fname %in% c(arith_ops, cmp_ops, log_ops) && length(e) == 3L) {
    lhs <- tccq_lower_expr(e[[2]], sc, decl)
    rhs <- tccq_lower_expr(e[[3]], sc, decl)
    if (!lhs$ok) {
      return(lhs)
    }
    if (!rhs$ok) {
      return(rhs)
    }
    out_mode <- if (fname %in% cmp_ops || fname %in% log_ops) {
      "logical"
    } else if (fname %in% c("/", "^", "%%", "%/%")) {
      "double"
    } else {
      tcc_quick_promote_mode(lhs$mode, rhs$mode) %||% "double"
    }
    out_shape <- if (lhs$shape == "vector" || rhs$shape == "vector") {
      "vector"
    } else {
      "scalar"
    }
    return(tccq_lower_result(
      TRUE,
      node = list(
        tag = "binary",
        op = fname,
        lhs = lhs$node,
        rhs = rhs$node
      ),
      mode = out_mode,
      shape = out_shape
    ))
  }

  # --- Constructors: double(n), integer(n), logical(n), raw(n) ---
  if (fname %in% c("double", "integer", "logical", "raw") && length(e) == 2L) {
    len <- tccq_lower_expr(e[[2]], sc, decl)
    if (!len$ok) {
      return(len)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(
        tag = "vec_alloc",
        len_expr = len$node,
        alloc_mode = fname
      ),
      mode = fname,
      shape = "vector"
    ))
  }

  # --- matrix(fill, nrow, ncol) ---
  if (fname == "matrix" && length(e) >= 4L) {
    data_expr <- e[[2]]
    nr <- tccq_lower_expr(e[[3]], sc, decl)
    nc <- tccq_lower_expr(e[[4]], sc, decl)
    if (!nr$ok) {
      return(nr)
    }
    if (!nc$ok) {
      return(nc)
    }
    fill_val <- 0
    if (length(data_expr) == 1L && is.numeric(data_expr)) {
      fill_val <- as.double(data_expr)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(
        tag = "mat_alloc",
        nrow = nr$node,
        ncol = nc$node,
        fill = fill_val,
        alloc_mode = "double"
      ),
      mode = "double",
      shape = "matrix"
    ))
  }

  # --- Casts: as.integer(), as.double(), as.numeric(), as.raw() ---
  if (
    fname %in%
      c("as.integer", "as.double", "as.numeric", "as.raw") &&
      length(e) == 2L
  ) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    target_mode <- if (fname == "as.integer") {
      "integer"
    } else if (fname == "as.raw") {
      "raw"
    } else {
      "double"
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "cast", x = x$node, target_mode = target_mode),
      mode = target_mode,
      shape = x$shape
    ))
  }

  # --- seq_along / seq_len ---
  if (fname == "seq_along" && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "seq_along", target = x$node$name),
      mode = "integer",
      shape = "vector"
    ))
  }
  if (fname == "seq_len" && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "seq_len", n = x$node),
      mode = "integer",
      shape = "vector"
    ))
  }

  # --- Colon operator: a:b ---
  if (fname == ":" && length(e) == 3L) {
    from <- tccq_lower_expr(e[[2]], sc, decl)
    to <- tccq_lower_expr(e[[3]], sc, decl)
    if (!from$ok) {
      return(from)
    }
    if (!to$ok) {
      return(to)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "seq_range", from = from$node, to = to$node),
      mode = "integer",
      shape = "vector"
    ))
  }

  # --- seq(from, to) and seq(from, to, by) ---
  if (fname == "seq") {
    # seq(from, to) → same as from:to
    if (length(e) == 3L) {
      from <- tccq_lower_expr(e[[2]], sc, decl)
      to <- tccq_lower_expr(e[[3]], sc, decl)
      if (!from$ok) {
        return(from)
      }
      if (!to$ok) {
        return(to)
      }
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "seq_range",
          from = from$node,
          to = to$node
        ),
        mode = "integer",
        shape = "vector"
      ))
    }
    # seq(from, to, by) — positional or named `by=`
    if (length(e) == 4L) {
      nms <- names(e)
      by_pos <- 4L
      if (!is.null(nms) && any(nms == "by")) {
        by_pos <- which(nms == "by")
      }
      # Determine from/to positions (everything that isn't by)
      positional <- setdiff(2:4, by_pos)
      from <- tccq_lower_expr(e[[positional[1]]], sc, decl)
      to <- tccq_lower_expr(e[[positional[2]]], sc, decl)
      by <- tccq_lower_expr(e[[by_pos]], sc, decl)
      if (!from$ok) {
        return(from)
      }
      if (!to$ok) {
        return(to)
      }
      if (!by$ok) {
        return(by)
      }
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "seq_by",
          from = from$node,
          to = to$node,
          by = by$node
        ),
        mode = "double",
        shape = "vector"
      ))
    }
  }

  # --- any / all reductions (logical) ---
  if (fname %in% c("any", "all") && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape == "vector" && identical(x$node$tag, "var")) {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "reduce", op = fname, arr = x$node$name),
        mode = "logical"
      ))
    }
    if (x$shape == "vector") {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "reduce_expr", op = fname, expr = x$node),
        mode = "logical"
      ))
    }
    # scalar: any/all of a scalar is just the value
    return(tccq_lower_result(TRUE, node = x$node, mode = "logical"))
  }

  # --- pmin / pmax (element-wise binary) ---
  if (fname %in% c("pmin", "pmax") && length(e) == 3L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    y <- tccq_lower_expr(e[[3]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (!y$ok) {
      return(y)
    }
    out_shape <- if (x$shape == "vector" || y$shape == "vector") {
      "vector"
    } else {
      "scalar"
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "call2", fun = fname, x = x$node, y = y$node),
      mode = tcc_quick_promote_mode(x$mode, y$mode) %||% "double",
      shape = out_shape
    ))
  }

  # --- cumsum / cumprod / cummax / cummin ---
  if (
    fname %in% c("cumsum", "cumprod", "cummax", "cummin") && length(e) == 2L
  ) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape != "vector") {
      return(tccq_lower_result(
        FALSE,
        reason = paste0(fname, " requires a vector argument")
      ))
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "cumulative", op = fname, expr = x$node),
      mode = x$mode,
      shape = "vector"
    ))
  }

  # --- rev(x) ---
  if (fname == "rev" && length(e) == 2L) {
    x <- tccq_lower_expr(e[[2]], sc, decl)
    if (!x$ok) {
      return(x)
    }
    if (x$shape != "vector") {
      return(tccq_lower_result(
        FALSE,
        reason = "rev requires a vector argument"
      ))
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "rev", expr = x$node),
      mode = x$mode,
      shape = "vector"
    ))
  }

  # --- stop(msg) → Rf_error ---
  if (fname == "stop" && length(e) == 2L) {
    msg <- e[[2]]
    if (is.character(msg)) {
      return(tccq_lower_result(
        TRUE,
        node = list(tag = "stop", msg = msg),
        mode = "void"
      ))
    }
    return(tccq_lower_result(
      FALSE,
      reason = "stop() requires a string literal"
    ))
  }

  # --- Unknown function: emit rf_call (inline R evaluation) ---
  # Try to lower all arguments; if any fails, bail out entirely.

  rf_allow <- tcc_quick_rf_call_allowlist()
  if (!fname %in% rf_allow) {
    return(tccq_lower_result(
      FALSE,
      reason = paste0("Unsupported function call: ", fname)
    ))
  }

  if (!fname %in% tcc_quick_rf_call_quiet()) {
    message(
      "[tcc_quick] '",
      fname,
      "' not natively supported is delegating to R via Rf_eval() !"
    )
  }
  rf_args <- list()
  for (ai in seq_len(length(e) - 1L)) {
    a <- tccq_lower_expr(e[[ai + 1L]], sc, decl)
    if (!a$ok) {
      return(tccq_lower_result(
        FALSE,
        reason = paste0("Cannot lower argument ", ai, " of ", fname)
      ))
    }
    rf_args[[ai]] <- a
  }
  rf_arg_nodes <- lapply(rf_args, function(a) {
    nd <- a$node
    nd$shape <- a$shape
    nd$mode <- a$mode
    nd
  })
  # Determine output shape heuristically: if any arg is vector/matrix,
  # result is likely vector (e.g. %*%, t()).  For scalars, assume scalar.
  out_shape <- "scalar"
  for (a in rf_args) {
    if (a$shape %in% c("vector", "matrix")) {
      out_shape <- "vector"
      break
    }
  }
  tccq_lower_result(
    TRUE,
    node = list(
      tag = "rf_call",
      fun = fname,
      args = rf_arg_nodes,
      mode = "double",
      shape = out_shape
    ),
    mode = "double",
    shape = out_shape
  )
}

# ---------------------------------------------------------------------------
# Statement lowering
# ---------------------------------------------------------------------------

tccq_lower_stmt <- function(e, sc, decl) {
  if (!is.call(e)) {
    return(tccq_lower_expr(e, sc, decl))
  }

  fun <- e[[1]]
  if (!is.symbol(fun)) {
    return(tccq_lower_result(FALSE, reason = "Non-symbol call in statement"))
  }
  fname <- as.character(fun)

  # Block: { ... }
  if (fname == "{") {
    stmts <- as.list(e)[-1]
    nodes <- list()
    for (s in stmts) {
      r <- tccq_lower_stmt(s, sc, decl)
      if (!r$ok) {
        return(r)
      }
      nodes[[length(nodes) + 1L]] <- r$node
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "block", stmts = nodes),
      mode = "void"
    ))
  }

  # Assignment: x <- expr / x[i] <- expr / x[i,j] <- expr
  if (fname == "<-" && length(e) == 3L) {
    lhs <- e[[2]]

    if (is.call(lhs) && is.symbol(lhs[[1]]) && as.character(lhs[[1]]) == "[") {
      # x[i] <- expr
      if (length(lhs) == 3L) {
        arr <- tccq_lower_expr(lhs[[2]], sc, decl)
        idx <- tccq_lower_expr(lhs[[3]], sc, decl)
        val <- tccq_lower_expr(e[[3]], sc, decl)
        if (!arr$ok) {
          return(arr)
        }
        if (!idx$ok) {
          return(idx)
        }
        if (!val$ok) {
          return(val)
        }
        # Mark param as mutated so codegen duplicates before writing
        tccq_scope_mark_mutated(sc, arr$node$name)
        return(tccq_lower_result(
          TRUE,
          node = list(
            tag = "vec_set",
            arr = arr$node$name,
            idx = idx$node,
            value = val$node
          ),
          mode = "void"
        ))
      }
      # x[i, j] <- expr
      if (length(lhs) == 4L) {
        arr <- tccq_lower_expr(lhs[[2]], sc, decl)
        row <- tccq_lower_expr(lhs[[3]], sc, decl)
        col <- tccq_lower_expr(lhs[[4]], sc, decl)
        val <- tccq_lower_expr(e[[3]], sc, decl)
        if (!arr$ok) {
          return(arr)
        }
        if (!row$ok) {
          return(row)
        }
        if (!col$ok) {
          return(col)
        }
        if (!val$ok) {
          return(val)
        }
        # Mark param as mutated
        tccq_scope_mark_mutated(sc, arr$node$name)
        return(tccq_lower_result(
          TRUE,
          node = list(
            tag = "mat_set",
            arr = arr$node$name,
            row = row$node,
            col = col$node,
            value = val$node
          ),
          mode = "void"
        ))
      }
    }

    # x <- expr
    if (is.symbol(lhs)) {
      nm <- as.character(lhs)
      rhs <- tccq_lower_expr(e[[3]], sc, decl)
      if (!rhs$ok) {
        return(rhs)
      }

      existing <- tccq_scope_get(sc, nm)
      if (is.null(existing)) {
        tccq_scope_set(
          sc,
          nm,
          list(
            kind = "local",
            mode = rhs$mode,
            shape = rhs$shape
          )
        )
      }

      # Vector reassignment: rewrite existing array element-wise
      if (
        !is.null(existing) &&
          existing$shape %in% c("vector", "matrix") &&
          rhs$shape == "vector" &&
          !identical(rhs$node$tag, "vec_alloc") &&
          !identical(rhs$node$tag, "mat_alloc")
      ) {
        # Mark param as mutated so codegen duplicates before writing
        tccq_scope_mark_mutated(sc, nm)
        return(tccq_lower_result(
          TRUE,
          node = list(
            tag = "vec_rewrite",
            name = nm,
            expr = rhs$node
          ),
          mode = "void"
        ))
      }

      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "assign",
          name = nm,
          expr = rhs$node,
          mode = rhs$mode,
          shape = rhs$shape
        ),
        mode = "void"
      ))
    }

    return(tccq_lower_result(FALSE, reason = "Unsupported assignment target"))
  }

  # for (i in iter) body
  if (fname == "for" && length(e) == 4L) {
    var <- as.character(e[[2]])
    iter_r <- tccq_lower_expr(e[[3]], sc, decl)
    if (!iter_r$ok) {
      return(iter_r)
    }

    # Value iteration: for (x in vec) where vec is a named vector
    # Desugar to: hidden index loop + x = vec[idx] at top of body
    if (
      iter_r$shape == "vector" &&
        identical(iter_r$node$tag, "var")
    ) {
      idx_var <- paste0(".tccq_i_", var)
      vec_name <- iter_r$node$name

      tccq_scope_set(
        sc,
        idx_var,
        list(kind = "loop_var", mode = "integer", shape = "scalar")
      )
      tccq_scope_set(
        sc,
        var,
        list(
          kind = "local",
          mode = iter_r$mode,
          shape = "scalar"
        )
      )

      body_r <- tccq_lower_stmt(e[[4]], sc, decl)
      if (!body_r$ok) {
        return(body_r)
      }

      # Prepend: var <- vec[idx]
      extract_stmt <- list(
        tag = "assign",
        name = var,
        expr = list(
          tag = "vec_get",
          arr = vec_name,
          idx = list(tag = "var", name = idx_var)
        ),
        mode = iter_r$mode,
        shape = "scalar"
      )
      inner_body <- if (identical(body_r$node$tag, "block")) {
        list(
          tag = "block",
          stmts = c(list(extract_stmt), body_r$node$stmts)
        )
      } else {
        list(
          tag = "block",
          stmts = list(extract_stmt, body_r$node)
        )
      }

      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "for",
          var = idx_var,
          iter = list(tag = "seq_along", target = vec_name),
          body = inner_body
        ),
        mode = "void"
      ))
    }

    # Set loop variable mode: double for seq_by, integer otherwise
    loop_mode <- if (identical(iter_r$node$tag, "seq_by")) {
      "double"
    } else {
      "integer"
    }
    tccq_scope_set(
      sc,
      var,
      list(
        kind = "loop_var",
        mode = loop_mode,
        shape = "scalar"
      )
    )
    body_r <- tccq_lower_stmt(e[[4]], sc, decl)
    if (!body_r$ok) {
      return(body_r)
    }

    return(tccq_lower_result(
      TRUE,
      node = list(
        tag = "for",
        var = var,
        iter = iter_r$node,
        body = body_r$node
      ),
      mode = "void"
    ))
  }

  # while (cond) body
  if (fname == "while" && length(e) == 3L) {
    cond <- tccq_lower_expr(e[[2]], sc, decl)
    body_r <- tccq_lower_stmt(e[[3]], sc, decl)
    if (!cond$ok) {
      return(cond)
    }
    if (!body_r$ok) {
      return(body_r)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "while", cond = cond$node, body = body_r$node),
      mode = "void"
    ))
  }

  # repeat body
  if (fname == "repeat" && length(e) == 2L) {
    body_r <- tccq_lower_stmt(e[[2]], sc, decl)
    if (!body_r$ok) {
      return(body_r)
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "repeat", body = body_r$node),
      mode = "void"
    ))
  }

  # break / next
  if (fname == "break") {
    return(tccq_lower_result(TRUE, node = list(tag = "break"), mode = "void"))
  }
  if (fname == "next") {
    return(tccq_lower_result(TRUE, node = list(tag = "next"), mode = "void"))
  }

  # if / if-else statement
  if (fname == "if") {
    cond <- tccq_lower_expr(e[[2]], sc, decl)
    if (!cond$ok) {
      return(cond)
    }
    yes <- tccq_lower_stmt(e[[3]], sc, decl)
    if (!yes$ok) {
      return(yes)
    }
    if (length(e) == 4L) {
      no <- tccq_lower_stmt(e[[4]], sc, decl)
      if (!no$ok) {
        return(no)
      }
      return(tccq_lower_result(
        TRUE,
        node = list(
          tag = "if_else_stmt",
          cond = cond$node,
          yes = yes$node,
          no = no$node
        ),
        mode = "void"
      ))
    }
    return(tccq_lower_result(
      TRUE,
      node = list(tag = "if_stmt", cond = cond$node, yes = yes$node),
      mode = "void"
    ))
  }

  # General expression statement
  tccq_lower_expr(e, sc, decl)
}

# ---------------------------------------------------------------------------
# Top-level: lower function body → fn_body IR
# ---------------------------------------------------------------------------

tccq_lower_fn_body <- function(fn, decl) {
  exprs <- tcc_quick_body_without_declare(fn)
  if (length(exprs) < 1L) {
    return(tcc_quick_fallback_ir("Empty function body after declare()"))
  }

  sc <- tccq_scope_new(decl)
  stmts <- list()

  if (length(exprs) > 1L) {
    for (i in seq_len(length(exprs) - 1L)) {
      r <- tccq_lower_stmt(exprs[[i]], sc, decl)
      if (!r$ok) {
        return(tcc_quick_fallback_ir(
          r$reason %||% paste0("Unsupported statement at position ", i)
        ))
      }
      stmts[[length(stmts) + 1L]] <- r$node
    }
  }

  last <- exprs[[length(exprs)]]
  ret <- tccq_lower_expr(last, sc, decl)
  if (!ret$ok) {
    return(tcc_quick_fallback_ir(
      ret$reason %||% "Unsupported return expression"
    ))
  }

  locals <- list()
  mutated_params <- character(0)
  for (nm in ls(sc, all.names = TRUE)) {
    info <- sc[[nm]]
    if (info$kind == "local") {
      locals[[nm]] <- info
    }
    if (info$kind == "param" && isTRUE(info$mutated)) {
      mutated_params <- c(mutated_params, nm)
    }
  }

  list(
    tag = "fn_body",
    params = decl$args,
    formal_names = decl$formal_names,
    locals = locals,
    mutated_params = mutated_params,
    stmts = stmts,
    ret = ret$node,
    ret_mode = ret$mode,
    ret_shape = ret$shape
  )
}

# ---------------------------------------------------------------------------
# Pipeline entry point
# ---------------------------------------------------------------------------

tcc_quick_lower <- function(fn, decl) {
  ir <- tccq_lower_fn_body(fn, decl)
  tcc_quick_verify_ir(ir)
}
