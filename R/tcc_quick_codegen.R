# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Unified codegen for tcc_quick.
#
# Takes fn_body IR and produces a complete SEXP-returning C function.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Recursively walk IR, looking for call1/call2 nodes whose fun requires Rmath.h
tccq_ir_needs_rmath <- function(node) {
  if (is.null(node) || !is.list(node)) {
    return(FALSE)
  }
  if (!is.null(node$tag)) {
    if (node$tag %in% c("call1", "call2")) {
      entry <- tcc_ir_registry_lookup(node$fun)
      if (!is.null(entry) && !is.null(entry$header)) {
        return(TRUE)
      }
    }
    # Recurse into sub-nodes
    for (field in c(
      "x",
      "y",
      "lhs",
      "rhs",
      "cond",
      "yes",
      "no",
      "expr",
      "body",
      "stmts",
      "ret",
      "len_expr",
      "idx",
      "value",
      "row",
      "col",
      "from",
      "to",
      "nrow",
      "ncol",
      "iter",
      "n",
      "args",
      "mask"
    )) {
      if (tccq_ir_needs_rmath(node[[field]])) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  # Plain list (e.g. stmts)
  for (child in node) {
    if (tccq_ir_needs_rmath(child)) {
      return(TRUE)
    }
  }
  FALSE
}

# Map R function name to C function name for codegen
tccq_cg_c_fun <- function(r_fun) {
  entry <- tcc_ir_registry_lookup(r_fun)
  if (!is.null(entry)) {
    return(entry$c_fun)
  }
  # Fallback for pmin/pmax (not in registry, handled inline)
  r_fun
}

tccq_cg_sexp_type <- function(mode) {
  switch(
    mode %||% "double",
    integer = "INTSXP",
    logical = "LGLSXP",
    raw = "RAWSXP",
    "REALSXP"
  )
}

tccq_cg_scalar_extract <- function(mode, sexp_var) {
  switch(
    mode %||% "double",
    integer = sprintf("INTEGER(%s)[0]", sexp_var),
    logical = sprintf("LOGICAL(%s)[0]", sexp_var),
    raw = sprintf("RAW(%s)[0]", sexp_var),
    sprintf("REAL(%s)[0]", sexp_var)
  )
}

# ---------------------------------------------------------------------------
# Expression codegen
# ---------------------------------------------------------------------------

tccq_cg_ident <- function(x) {
  y <- gsub("[^A-Za-z0-9_]", "_", x)
  if (grepl("^[0-9]", y)) {
    y <- paste0("v_", y)
  }
  y
}

tccq_cg_expr <- function(node) {
  tag <- node$tag

  if (tag == "var") {
    return(paste0(tccq_cg_ident(node$name), "_"))
  }

  if (tag == "const") {
    if (node$mode == "integer") {
      return(sprintf("%d", as.integer(node$value)))
    }
    if (node$mode == "double") {
      return(format(as.double(node$value), scientific = FALSE, trim = TRUE))
    }
    if (node$mode == "logical") {
      return(if (isTRUE(node$value)) "1" else "0")
    }
    if (node$mode == "raw") {
      return(sprintf("((Rbyte)%d)", as.integer(node$value)))
    }
    stop("Unknown const mode", call. = FALSE)
  }

  if (tag == "unary") {
    x <- tccq_cg_expr(node$x)
    if (node$op == "!") {
      return(sprintf("(!(%s))", x))
    }
    return(sprintf("(%s(%s))", node$op, x))
  }

  if (tag == "binary") {
    lhs <- tccq_cg_expr(node$lhs)
    rhs <- tccq_cg_expr(node$rhs)
    op <- node$op
    if (op == "%/%") {
      return(sprintf("((int)floor((double)(%s) / (double)(%s)))", lhs, rhs))
    }
    if (op == "%%") {
      return(sprintf("(fmod((double)(%s), (double)(%s)))", lhs, rhs))
    }
    if (op == "^") {
      return(sprintf("(pow((double)(%s), (double)(%s)))", lhs, rhs))
    }
    if (op == "&") {
      op <- "&&"
    }
    if (op == "|") {
      op <- "||"
    }
    return(sprintf("((%s) %s (%s))", lhs, op, rhs))
  }

  if (tag == "if") {
    cond <- tccq_cg_expr(node$cond)
    yes <- tccq_cg_expr(node$yes)
    no <- tccq_cg_expr(node$no)
    return(sprintf("((%s) ? (%s) : (%s))", cond, yes, no))
  }

  if (tag == "call1") {
    x <- tccq_cg_expr(node$x)
    fun <- tccq_cg_c_fun(node$fun)
    return(sprintf("(%s((double)(%s)))", fun, x))
  }

  if (tag == "call2") {
    x <- tccq_cg_expr(node$x)
    y <- tccq_cg_expr(node$y)
    fun <- node$fun
    if (fun == "pmin") {
      return(sprintf("((%s) < (%s) ? (%s) : (%s))", x, y, x, y))
    }
    if (fun == "pmax") {
      return(sprintf("((%s) > (%s) ? (%s) : (%s))", x, y, x, y))
    }
    if (fun == "bitwAnd") {
      return(sprintf("(((int)(%s)) & ((int)(%s)))", x, y))
    }
    if (fun == "bitwOr") {
      return(sprintf("(((int)(%s)) | ((int)(%s)))", x, y))
    }
    if (fun == "bitwXor") {
      return(sprintf("(((int)(%s)) ^ ((int)(%s)))", x, y))
    }
    if (fun == "bitwShiftL") {
      return(sprintf("(((int)(%s)) << ((int)(%s)))", x, y))
    }
    if (fun == "bitwShiftR") {
      return(sprintf("(((int)(%s)) >> ((int)(%s)))", x, y))
    }
    c_fun <- tccq_cg_c_fun(fun)
    return(sprintf("(%s((double)(%s), (double)(%s)))", c_fun, x, y))
  }

  if (tag == "stop") {
    return(sprintf("Rf_error(\"%s\")", gsub("\"", "\\\\\"", node$msg)))
  }

  if (tag == "length") {
    return(sprintf("n_%s", tccq_cg_ident(node$arr)))
  }
  if (tag == "nrow") {
    return(sprintf("nrow_%s", tccq_cg_ident(node$arr)))
  }
  if (tag == "ncol") {
    return(sprintf("ncol_%s", tccq_cg_ident(node$arr)))
  }

  if (tag == "vec_get") {
    arr <- tccq_cg_ident(node$arr)
    idx <- tccq_cg_expr(node$idx)
    return(sprintf("p_%s[(R_xlen_t)((%s) - 1)]", arr, idx))
  }

  if (tag == "vec_slice") {
    # vec_slice as a scalar expression should not normally appear;
    # it's accessed element-wise via tccq_cg_vec_elem.
    # If it does appear here, return the first element.
    arr <- tccq_cg_ident(node$arr)
    from <- tccq_cg_expr(node$from)
    return(sprintf("p_%s[(R_xlen_t)((%s) - 1)]", arr, from))
  }

  if (tag == "reduce_expr") {
    # When appearing as an expression, emit the precomputed variable name.
    # The actual loop is emitted as a statement via tccq_cg_reduce_expr_stmt.
    return(node$var_name)
  }

  if (tag == "mean_expr") {
    return(node$var_name)
  }

  if (tag == "sd_expr") {
    return(node$var_name)
  }

  if (tag == "median_expr") {
    return(node$var_name)
  }

  if (tag == "quantile_expr") {
    return(node$var_name)
  }

  if (tag == "mat_get") {
    arr <- tccq_cg_ident(node$arr)
    row <- tccq_cg_expr(node$row)
    col <- tccq_cg_expr(node$col)
    return(sprintf(
      "p_%s[(R_xlen_t)((%s) - 1) + (R_xlen_t)((%s) - 1) * nrow_%s]",
      arr,
      row,
      col,
      arr
    ))
  }

  if (tag == "reduce") {
    # When appearing as an expression, emit the precomputed variable name.
    # The actual loop is emitted as a statement via tccq_cg_reduce_stmt.
    return(sprintf("red_%s_%s", node$op, tccq_cg_ident(node$arr)))
  }

  if (tag == "which_reduce") {
    return(sprintf(
      "which_%s_%s",
      gsub("\\.", "_", node$op),
      tccq_cg_ident(node$arr)
    ))
  }

  if (tag == "cast") {
    x <- tccq_cg_expr(node$x)
    if (node$target_mode == "integer") {
      return(sprintf("((int)(%s))", x))
    }
    if (node$target_mode == "double") {
      return(sprintf("((double)(%s))", x))
    }
    if (node$target_mode == "raw") {
      return(sprintf("((Rbyte)(%s))", x))
    }
    return(x)
  }

  if (tag == "rf_call") {
    # rf_call as sub-expression: the statement was hoisted before this
    # expression is evaluated.  Return the pre-assigned variable name.
    if (is.null(node$var_name)) {
      stop("rf_call missing var_name - pre-pass not run?", call. = FALSE)
    }
    # Delegated result was already runtime-validated; extract directly.
    return(tccq_cg_scalar_extract(node$mode, node$var_name))
  }

  stop(paste0("tccq_cg_expr: unsupported tag '", tag, "'"), call. = FALSE)
}

# ---------------------------------------------------------------------------
# Statement codegen
# ---------------------------------------------------------------------------

tccq_cg_stmt <- function(node, indent) {
  tag <- node$tag
  pad <- strrep("  ", indent)

  if (tag == "block") {
    lines <- vapply(
      node$stmts,
      function(s) tccq_cg_stmt(s, indent),
      character(1)
    )
    return(paste(lines, collapse = "\n"))
  }

  if (tag == "assign") {
    nm <- tccq_cg_ident(node$name)

    # --- vector allocation ---
    if (
      identical(node$shape, "vector") &&
        identical(node$expr$tag, "vec_alloc")
    ) {
      alloc_mode <- node$expr$alloc_mode
      sxp_type <- switch(
        alloc_mode,
        double = "REALSXP",
        integer = "INTSXP",
        logical = "LGLSXP",
        raw = "RAWSXP",
        "REALSXP"
      )
      len <- tccq_cg_expr(node$expr$len_expr)
      ptr_fun <- switch(
        alloc_mode,
        double = "REAL",
        integer = "INTEGER",
        logical = "LOGICAL",
        raw = "RAW",
        "REAL"
      )
      return(paste0(
        pad,
        "s_",
        nm,
        " = PROTECT(Rf_allocVector(",
        sxp_type,
        ", (R_xlen_t)(",
        len,
        ")));\n",
        pad,
        "n_",
        nm,
        " = XLENGTH(s_",
        nm,
        ");\n",
        pad,
        "p_",
        nm,
        " = ",
        ptr_fun,
        "(s_",
        nm,
        ");\n",
        pad,
        "nprotect_++;\n",
        pad,
        "for (R_xlen_t zz_ = 0; zz_ < n_",
        nm,
        "; ++zz_) p_",
        nm,
        "[zz_] = 0;"
      ))
    }

    # --- matrix allocation ---
    if (
      identical(node$shape, "matrix") &&
        identical(node$expr$tag, "mat_alloc")
    ) {
      nr <- tccq_cg_expr(node$expr$nrow)
      nc <- tccq_cg_expr(node$expr$ncol)
      fill <- format(node$expr$fill, scientific = FALSE)
      return(paste0(
        pad,
        "s_",
        nm,
        " = PROTECT(Rf_allocMatrix(REALSXP, (int)(",
        nr,
        "), (int)(",
        nc,
        ")));\n",
        pad,
        "nrow_",
        nm,
        " = (int)(",
        nr,
        ");\n",
        pad,
        "ncol_",
        nm,
        " = (int)(",
        nc,
        ");\n",
        pad,
        "n_",
        nm,
        " = (R_xlen_t)nrow_",
        nm,
        " * (R_xlen_t)ncol_",
        nm,
        ";\n",
        pad,
        "p_",
        nm,
        " = REAL(s_",
        nm,
        ");\n",
        pad,
        "nprotect_++;\n",
        pad,
        "for (R_xlen_t zz_ = 0; zz_ < n_",
        nm,
        "; ++zz_) p_",
        nm,
        "[zz_] = ",
        fill,
        ";"
      ))
    }

    # --- matrix product assignment (BLAS dgemm) ---
    if (identical(node$shape, "matrix") && identical(node$expr$tag, "matmul")) {
      return(tccq_cg_matmul_stmt(node$expr, indent, nm))
    }

    # --- matrix transpose assignment ---
    if (
      identical(node$shape, "matrix") &&
        identical(node$expr$tag, "transpose")
    ) {
      return(tccq_cg_transpose_stmt(node$expr, indent, nm, mode = node$mode))
    }

    # --- matrix row/col reducers assignment ---
    if (
      identical(node$shape, "vector") &&
        identical(node$expr$tag, "mat_reduce")
    ) {
      return(tccq_cg_mat_reduce_stmt(node$expr, indent, nm))
    }

    # --- linear solve assignment (LAPACK dgesv) ---
    if (
      identical(node$shape, "vector") &&
        identical(node$expr$tag, "solve_lin")
    ) {
      return(tccq_cg_solve_lin_stmt(node$expr, indent, nm))
    }
    if (
      identical(node$shape, "matrix") &&
        identical(node$expr$tag, "solve_lin")
    ) {
      return(tccq_cg_solve_lin_stmt(node$expr, indent, nm))
    }

    # --- scalar assignment ---
    # First handle cumulative ops which need alloc + fill loop
    if (
      identical(node$expr$tag, "cumulative") && identical(node$shape, "vector")
    ) {
      len_c <- tccq_cg_vec_len(node$expr$expr)
      sxp_type <- switch(
        node$mode,
        integer = "INTSXP",
        logical = "LGLSXP",
        raw = "RAWSXP",
        "REALSXP"
      )
      ptr_fun <- switch(
        node$mode,
        integer = "INTEGER",
        logical = "LOGICAL",
        raw = "RAW",
        "REAL"
      )
      alloc_lines <- paste0(
        pad,
        "s_",
        nm,
        " = PROTECT(Rf_allocVector(",
        sxp_type,
        ", (R_xlen_t)(",
        len_c,
        ")));\n",
        pad,
        "n_",
        nm,
        " = XLENGTH(s_",
        nm,
        ");\n",
        pad,
        "p_",
        nm,
        " = ",
        ptr_fun,
        "(s_",
        nm,
        ");\n",
        pad,
        "nprotect_++;"
      )
      cum_lines <- tccq_cg_cumulative_stmt(
        node$expr,
        indent,
        paste0("p_", nm)
      )
      return(paste(c(alloc_lines, cum_lines), collapse = "\n"))
    }

    # --- rf_call assignment: y <- fun(x, ...) via Rf_eval ---
    if (identical(node$expr$tag, "rf_call")) {
      rf_res <- tccq_cg_rf_call_stmt(node$expr, indent)
      rf_lines <- rf_res$lines
      rf_var <- rf_res$var
      if (identical(node$shape, "vector") || identical(node$shape, "matrix")) {
        ptr_fun <- switch(
          node$mode,
          integer = "INTEGER",
          logical = "LOGICAL",
          raw = "RAW",
          "REAL"
        )
        dims <- if (identical(node$shape, "matrix")) {
          paste0(
            "\n",
            pad,
            "nrow_",
            nm,
            " = Rf_nrows(s_",
            nm,
            ");\n",
            pad,
            "ncol_",
            nm,
            " = Rf_ncols(s_",
            nm,
            ");"
          )
        } else {
          ""
        }
        return(paste0(
          rf_lines,
          "\n",
          pad,
          "s_",
          nm,
          " = ",
          rf_var,
          ";\n",
          pad,
          "n_",
          nm,
          " = XLENGTH(s_",
          nm,
          ");",
          dims,
          "\n",
          pad,
          "p_",
          nm,
          " = ",
          ptr_fun,
          "(s_",
          nm,
          ");"
        ))
      } else {
        return(paste0(
          rf_lines,
          "\n",
          pad,
          nm,
          "_ = ",
          tccq_cg_scalar_extract(node$mode, rf_var),
          ";"
        ))
      }
    }

    # --- vec_mask assignment: y <- x[mask] (count + alloc + fill) ---
    if (
      identical(node$shape, "vector") &&
        identical(node$expr$tag, "vec_mask")
    ) {
      return(tccq_cg_vec_mask_assign(node, indent))
    }

    if (
      identical(node$shape, "vector") &&
        identical(node$expr$tag, "quantile_vec_expr")
    ) {
      return(tccq_cg_quantile_vec_stmt(node$expr, indent, nm))
    }

    parts <- c()
    # Hoist rf_calls
    rfs <- tccq_collect_rf_calls(node$expr)
    for (rf in rfs) {
      parts <- c(parts, tccq_cg_rf_call_stmt(rf, indent)$lines)
    }
    # Hoist reductions
    reds <- tccq_collect_reductions(node$expr)
    for (rd in reds) {
      if (rd$tag == "reduce") {
        parts <- c(parts, tccq_cg_reduce_stmt(rd, indent))
      }
      if (rd$tag == "reduce_expr") {
        parts <- c(parts, tccq_cg_reduce_expr_stmt(rd, indent))
      }
      if (rd$tag == "mean_expr") {
        parts <- c(parts, tccq_cg_mean_expr_stmt(rd, indent))
      }
      if (rd$tag == "sd_expr") {
        parts <- c(parts, tccq_cg_sd_expr_stmt(rd, indent))
      }
      if (rd$tag == "median_expr") {
        parts <- c(parts, tccq_cg_median_expr_stmt(rd, indent))
      }
      if (rd$tag == "quantile_expr") {
        parts <- c(parts, tccq_cg_quantile_expr_stmt(rd, indent))
      }
    }

    # --- Vector condensation: alloc + element-wise loop ---
    if (identical(node$shape, "vector")) {
      len_c <- tccq_cg_vec_len(node$expr)
      if (is.null(len_c)) {
        stop(
          "Cannot determine length for vector assign to '",
          node$name,
          "'",
          call. = FALSE
        )
      }
      elem_expr <- tccq_cg_vec_elem(node$expr, "zz_")
      sxp_type <- switch(
        node$mode,
        integer = "INTSXP",
        logical = "LGLSXP",
        raw = "RAWSXP",
        "REALSXP"
      )
      ptr_fun <- switch(
        node$mode,
        integer = "INTEGER",
        logical = "LOGICAL",
        raw = "RAW",
        "REAL"
      )
      parts <- c(
        parts,
        paste0(
          pad,
          "s_",
          nm,
          " = PROTECT(Rf_allocVector(",
          sxp_type,
          ", (R_xlen_t)(",
          len_c,
          ")));\n",
          pad,
          "n_",
          nm,
          " = XLENGTH(s_",
          nm,
          ");\n",
          pad,
          "p_",
          nm,
          " = ",
          ptr_fun,
          "(s_",
          nm,
          ");\n",
          pad,
          "nprotect_++;\n",
          pad,
          "for (R_xlen_t zz_ = 0; zz_ < n_",
          nm,
          "; ++zz_) ",
          "p_",
          nm,
          "[zz_] = ",
          elem_expr,
          ";"
        )
      )
      return(paste(parts, collapse = "\n"))
    }

    # --- Scalar assignment ---
    expr <- tccq_cg_expr(node$expr)
    parts <- c(parts, paste0(pad, nm, "_ = ", expr, ";"))
    return(paste(parts, collapse = "\n"))
  }

  if (tag == "vec_rewrite") {
    nm <- tccq_cg_ident(node$name)
    parts <- c()
    # Hoist rf_calls in the RHS expression
    rfs <- tccq_collect_rf_calls(node$expr)
    for (rf in rfs) {
      parts <- c(parts, tccq_cg_rf_call_stmt(rf, indent)$lines)
    }
    # Hoist any reductions in the RHS expression
    reds <- tccq_collect_reductions(node$expr)
    for (rd in reds) {
      if (rd$tag == "reduce") {
        parts <- c(parts, tccq_cg_reduce_stmt(rd, indent))
      }
      if (rd$tag == "reduce_expr") {
        parts <- c(parts, tccq_cg_reduce_expr_stmt(rd, indent))
      }
      if (rd$tag == "mean_expr") {
        parts <- c(parts, tccq_cg_mean_expr_stmt(rd, indent))
      }
      if (rd$tag == "sd_expr") {
        parts <- c(parts, tccq_cg_sd_expr_stmt(rd, indent))
      }
      if (rd$tag == "median_expr") {
        parts <- c(parts, tccq_cg_median_expr_stmt(rd, indent))
      }
      if (rd$tag == "quantile_expr") {
        parts <- c(parts, tccq_cg_quantile_expr_stmt(rd, indent))
      }
    }
    elem <- tccq_cg_vec_elem(node$expr, "vr_")
    parts <- c(
      parts,
      paste0(
        pad,
        "for (R_xlen_t vr_ = 0; vr_ < n_",
        nm,
        "; ++vr_) {\n",
        pad,
        "  p_",
        nm,
        "[vr_] = ",
        elem,
        ";\n",
        pad,
        "}"
      )
    )
    return(paste(parts, collapse = "\n"))
  }

  if (tag == "vec_set") {
    arr <- tccq_cg_ident(node$arr)
    idx <- tccq_cg_expr(node$idx)
    parts <- c()
    # Hoist any rf_calls in the value expression
    rfs <- tccq_collect_rf_calls(node$value)
    for (rf in rfs) {
      parts <- c(parts, tccq_cg_rf_call_stmt(rf, indent)$lines)
    }
    # Hoist any reductions in the value expression
    reds <- tccq_collect_reductions(node$value)
    for (rd in reds) {
      if (rd$tag == "reduce") {
        parts <- c(parts, tccq_cg_reduce_stmt(rd, indent))
      }
      if (rd$tag == "reduce_expr") {
        parts <- c(parts, tccq_cg_reduce_expr_stmt(rd, indent))
      }
      if (rd$tag == "which_reduce") {
        parts <- c(parts, tccq_cg_which_reduce_stmt(rd, indent))
      }
      if (rd$tag == "mean_expr") {
        parts <- c(parts, tccq_cg_mean_expr_stmt(rd, indent))
      }
      if (rd$tag == "sd_expr") {
        parts <- c(parts, tccq_cg_sd_expr_stmt(rd, indent))
      }
      if (rd$tag == "median_expr") {
        parts <- c(parts, tccq_cg_median_expr_stmt(rd, indent))
      }
      if (rd$tag == "quantile_expr") {
        parts <- c(parts, tccq_cg_quantile_expr_stmt(rd, indent))
      }
    }
    val <- tccq_cg_expr(node$value)
    parts <- c(
      parts,
      paste0(
        pad,
        "p_",
        arr,
        "[(R_xlen_t)((",
        idx,
        ") - 1)] = ",
        val,
        ";"
      )
    )
    return(paste(parts, collapse = "\n"))
  }

  if (tag == "mat_set") {
    arr <- tccq_cg_ident(node$arr)
    row <- tccq_cg_expr(node$row)
    col <- tccq_cg_expr(node$col)
    val <- tccq_cg_expr(node$value)
    return(paste0(
      pad,
      "p_",
      arr,
      "[(R_xlen_t)((",
      row,
      ") - 1) + (R_xlen_t)((",
      col,
      ") - 1) * nrow_",
      arr,
      "] = ",
      val,
      ";"
    ))
  }

  if (tag == "for") {
    var <- tccq_cg_ident(node$var)
    iter <- node$iter
    body_c <- tccq_cg_stmt(node$body, indent + 1)

    if (iter$tag == "seq_along") {
      lim <- sprintf("n_%s", tccq_cg_ident(iter$target))
      return(paste0(
        pad,
        "for (R_xlen_t ",
        var,
        "_ = 1; ",
        var,
        "_ <= ",
        lim,
        "; ++",
        var,
        "_) {\n",
        body_c,
        "\n",
        pad,
        "}"
      ))
    }
    if (iter$tag == "seq_len") {
      lim <- tccq_cg_expr(iter$n)
      return(paste0(
        pad,
        "for (R_xlen_t ",
        var,
        "_ = 1; ",
        var,
        "_ <= (R_xlen_t)(",
        lim,
        "); ++",
        var,
        "_) {\n",
        body_c,
        "\n",
        pad,
        "}"
      ))
    }
    if (iter$tag == "seq_range") {
      from <- tccq_cg_expr(iter$from)
      to <- tccq_cg_expr(iter$to)
      # Support both ascending (from <= to) and descending (from > to) ranges
      v <- paste0(var, "_")
      return(paste0(
        pad,
        "if ((R_xlen_t)(",
        from,
        ") <= (R_xlen_t)(",
        to,
        ")) {\n",
        pad,
        "  for (R_xlen_t ",
        v,
        " = (R_xlen_t)(",
        from,
        "); ",
        v,
        " <= (R_xlen_t)(",
        to,
        "); ++",
        v,
        ") {\n",
        body_c,
        "\n",
        pad,
        "  }\n",
        pad,
        "} else {\n",
        pad,
        "  for (R_xlen_t ",
        v,
        " = (R_xlen_t)(",
        from,
        "); ",
        v,
        " >= (R_xlen_t)(",
        to,
        "); --",
        v,
        ") {\n",
        body_c,
        "\n",
        pad,
        "  }\n",
        pad,
        "}"
      ))
    }
    if (iter$tag == "seq_by") {
      from <- tccq_cg_expr(iter$from)
      to <- tccq_cg_expr(iter$to)
      by <- tccq_cg_expr(iter$by)
      v <- paste0(var, "_")
      # by can be positive or negative - branch at runtime
      return(paste0(
        pad,
        "{\n",
        pad,
        "  double seq_by_val_ = (double)(",
        by,
        ");\n",
        pad,
        "  if (seq_by_val_ > 0) {\n",
        pad,
        "    for (double ",
        v,
        " = (double)(",
        from,
        "); ",
        v,
        " <= (double)(",
        to,
        "); ",
        v,
        " += seq_by_val_) {\n",
        body_c,
        "\n",
        pad,
        "    }\n",
        pad,
        "  } else if (seq_by_val_ < 0) {\n",
        pad,
        "    for (double ",
        v,
        " = (double)(",
        from,
        "); ",
        v,
        " >= (double)(",
        to,
        "); ",
        v,
        " += seq_by_val_) {\n",
        body_c,
        "\n",
        pad,
        "    }\n",
        pad,
        "  }\n",
        pad,
        "}"
      ))
    }
    stop("Unsupported for-loop iterator tag", call. = FALSE)
  }

  if (tag == "while") {
    cond <- tccq_cg_expr(node$cond)
    body_c <- tccq_cg_stmt(node$body, indent + 1)
    return(paste0(
      pad,
      "while (",
      cond,
      ") {\n",
      body_c,
      "\n",
      pad,
      "}"
    ))
  }

  if (tag == "repeat") {
    body_c <- tccq_cg_stmt(node$body, indent + 1)
    return(paste0(pad, "for (;;) {\n", body_c, "\n", pad, "}"))
  }

  if (tag == "break") {
    return(paste0(pad, "break;"))
  }
  if (tag == "next") {
    return(paste0(pad, "continue;"))
  }

  if (tag == "if_stmt") {
    cond <- tccq_cg_expr(node$cond)
    yes <- tccq_cg_stmt(node$yes, indent + 1)
    return(paste0(pad, "if (", cond, ") {\n", yes, "\n", pad, "}"))
  }

  if (tag == "if_else_stmt") {
    cond <- tccq_cg_expr(node$cond)
    yes <- tccq_cg_stmt(node$yes, indent + 1)
    no <- tccq_cg_stmt(node$no, indent + 1)
    return(paste0(
      pad,
      "if (",
      cond,
      ") {\n",
      yes,
      "\n",
      pad,
      "} else {\n",
      no,
      "\n",
      pad,
      "}"
    ))
  }

  if (tag == "reduce") {
    return(tccq_cg_reduce_stmt(node, indent))
  }

  if (tag == "reduce_expr") {
    return(tccq_cg_reduce_expr_stmt(node, indent))
  }

  if (tag == "stop") {
    return(paste0(
      pad,
      sprintf("Rf_error(\"%s\");", gsub("\"", "\\\\\"", node$msg))
    ))
  }

  if (tag == "rf_call") {
    return(tccq_cg_rf_call_stmt(node, indent)$lines)
  }

  if (tag == "cumulative") {
    # cumulative as a bare statement requires a target; should go through assign
    stop(
      "cumulative must appear as right-hand side of an assignment",
      call. = FALSE
    )
  }

  # Fallback: bare expression statement
  return(paste0(pad, tccq_cg_expr(node), ";"))
}

# ---------------------------------------------------------------------------
# Reduce / which_reduce statement emission
# ---------------------------------------------------------------------------

tccq_cg_reduce_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  arr <- tccq_cg_ident(node$arr)
  op <- node$op
  var <- sprintf("red_%s_%s", op, arr)

  out_mode <- node$mode %||% "double"
  c_type <- if (op %in% c("any", "all")) {
    "int"
  } else if (out_mode == "integer") {
    "int"
  } else {
    "double"
  }
  init <- switch(
    op,
    sum = if (c_type == "int") "0" else "0.0",
    prod = "1.0",
    max = sprintf("p_%s[0]", arr),
    min = sprintf("p_%s[0]", arr),
    any = "0",
    all = "1",
    "0.0"
  )
  update <- switch(
    op,
    sum = sprintf("%s += p_%s[ri_];", var, arr),
    prod = sprintf("%s *= p_%s[ri_];", var, arr),
    max = sprintf("if (p_%s[ri_] > %s) %s = p_%s[ri_];", arr, var, var, arr),
    min = sprintf("if (p_%s[ri_] < %s) %s = p_%s[ri_];", arr, var, var, arr),
    any = sprintf("if (p_%s[ri_]) { %s = 1; break; }", arr, var),
    all = sprintf("if (!p_%s[ri_]) { %s = 0; break; }", arr, var),
    ""
  )
  start <- if (op %in% c("max", "min")) "1" else "0"
  paste0(
    pad,
    c_type,
    " ",
    var,
    " = ",
    init,
    ";\n",
    pad,
    "for (R_xlen_t ri_ = ",
    start,
    "; ri_ < n_",
    arr,
    "; ++ri_) {\n",
    pad,
    "  ",
    update,
    "\n",
    pad,
    "}"
  )
}

tccq_cg_which_reduce_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  arr <- tccq_cg_ident(node$arr)
  op <- node$op
  var <- sprintf("which_%s_%s", gsub("\\.", "_", op), arr)
  cmp <- if (op == "which.max") ">" else "<"

  paste0(
    pad,
    "int ",
    var,
    " = 1;\n",
    pad,
    "double ",
    var,
    "_best = p_",
    arr,
    "[0];\n",
    pad,
    "for (R_xlen_t ri_ = 1; ri_ < n_",
    arr,
    "; ++ri_) {\n",
    pad,
    "  if (p_",
    arr,
    "[ri_] ",
    cmp,
    " ",
    var,
    "_best) {\n",
    pad,
    "    ",
    var,
    "_best = p_",
    arr,
    "[ri_];\n",
    pad,
    "    ",
    var,
    " = (int)(ri_ + 1);\n",
    pad,
    "  }\n",
    pad,
    "}"
  )
}

# reduce_expr counter for unique variable names
tccq_reduce_expr_counter_ <- new.env(parent = emptyenv())
tccq_reduce_expr_counter_$n <- 0L

tccq_reduce_expr_next_id <- function() {
  tccq_reduce_expr_counter_$n <- tccq_reduce_expr_counter_$n + 1L
  tccq_reduce_expr_counter_$n
}

# rf_call / vec_mask counters for unique variable names
tccq_rfcall_counter_ <- new.env(parent = emptyenv())
tccq_rfcall_counter_$n <- 0L

tccq_stat_expr_counter_ <- new.env(parent = emptyenv())
tccq_stat_expr_counter_$n <- 0L

tccq_rfcall_next_id <- function() {
  tccq_rfcall_counter_$n <- tccq_rfcall_counter_$n + 1L
  tccq_rfcall_counter_$n
}

tccq_stat_expr_next_id <- function() {
  tccq_stat_expr_counter_$n <- tccq_stat_expr_counter_$n + 1L
  tccq_stat_expr_counter_$n
}

tccq_cg_reduce_expr_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  op <- node$op

  # Assign a unique variable name if not already set
  if (is.null(node$var_name)) {
    node$var_name <- sprintf("redx_%s_%d", op, tccq_reduce_expr_next_id())
  }
  var <- node$var_name

  len_c <- tccq_cg_vec_len(node$expr)
  if (is.null(len_c)) {
    stop("Cannot determine length for reduce_expr", call. = FALSE)
  }

  # Use a unique index variable to avoid collisions with nested loops
  idx <- sprintf("rx%d_", tccq_reduce_expr_counter_$n)
  elem <- tccq_cg_vec_elem(node$expr, idx)

  out_mode <- node$mode %||% "double"
  c_type <- if (op %in% c("any", "all")) {
    "int"
  } else if (out_mode == "integer") {
    "int"
  } else {
    "double"
  }
  init <- switch(
    op,
    sum = if (c_type == "int") "0" else "0.0",
    prod = "1.0",
    max = paste0("(", tccq_cg_vec_elem(node$expr, "0"), ")"),
    min = paste0("(", tccq_cg_vec_elem(node$expr, "0"), ")"),
    any = "0",
    all = "1",
    "0.0"
  )
  update <- switch(
    op,
    sum = sprintf("%s += %s;", var, elem),
    prod = sprintf("%s *= %s;", var, elem),
    max = sprintf(
      "{ %s v_ = %s; if (v_ > %s) %s = v_; }",
      c_type,
      elem,
      var,
      var
    ),
    min = sprintf(
      "{ %s v_ = %s; if (v_ < %s) %s = v_; }",
      c_type,
      elem,
      var,
      var
    ),
    any = sprintf("if (%s) { %s = 1; break; }", elem, var),
    all = sprintf("if (!(%s)) { %s = 0; break; }", elem, var),
    ""
  )
  start <- if (op %in% c("max", "min")) "1" else "0"
  paste0(
    pad,
    c_type,
    " ",
    var,
    " = ",
    init,
    ";\n",
    pad,
    "for (R_xlen_t ",
    idx,
    " = ",
    start,
    "; ",
    idx,
    " < (R_xlen_t)(",
    len_c,
    "); ++",
    idx,
    ") {\n",
    pad,
    "  ",
    update,
    "\n",
    pad,
    "}"
  )
}

tccq_cg_mean_expr_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  if (is.null(node$var_name)) {
    node$var_name <- sprintf("mexpr_%d", tccq_stat_expr_next_id())
  }
  var <- node$var_name
  len_c <- tccq_cg_vec_len(node$expr)
  if (is.null(len_c)) {
    stop("Cannot determine length for mean_expr", call. = FALSE)
  }
  na_rm <- isTRUE(node$na_rm)
  idx <- sprintf("mx%d_", tccq_stat_expr_counter_$n)
  elem <- tccq_cg_vec_elem(node$expr, idx)
  if (!na_rm) {
    return(paste0(
      pad,
      "double ",
      var,
      " = NA_REAL;\n",
      pad,
      "R_xlen_t ",
      var,
      "_n = (R_xlen_t)(",
      len_c,
      ");\n",
      pad,
      "if (",
      var,
      "_n > 0) {\n",
      pad,
      "  double ",
      var,
      "_sum = 0.0;\n",
      pad,
      "  int ",
      var,
      "_has_na = 0;\n",
      pad,
      "  int ",
      var,
      "_has_nan = 0;\n",
      pad,
      "  for (R_xlen_t ",
      idx,
      " = 0; ",
      idx,
      " < ",
      var,
      "_n; ++",
      idx,
      ") {\n",
      pad,
      "    double ",
      var,
      "_v = (double)(",
      elem,
      ");\n",
      pad,
      "    if (ISNA(",
      var,
      "_v)) { ",
      var,
      "_has_na = 1; break; }\n",
      pad,
      "    if (ISNAN(",
      var,
      "_v)) { ",
      var,
      "_has_nan = 1; continue; }\n",
      pad,
      "    ",
      var,
      "_sum += ",
      var,
      "_v;\n",
      pad,
      "  }\n",
      pad,
      "  if (!",
      var,
      "_has_na) ",
      var,
      " = ",
      var,
      "_has_nan ? R_NaN : ",
      var,
      "_sum / (double)",
      var,
      "_n;\n",
      pad,
      "}"
    ))
  }
  paste0(
    pad,
    "double ",
    var,
    " = NA_REAL;\n",
    pad,
    "R_xlen_t ",
    var,
    "_n = 0;\n",
    pad,
    "double ",
    var,
    "_sum = 0.0;\n",
    pad,
    "for (R_xlen_t ",
    idx,
    " = 0; ",
    idx,
    " < (R_xlen_t)(",
    len_c,
    "); ++",
    idx,
    ") {\n",
    pad,
    "  double ",
    var,
    "_v = (double)(",
    elem,
    ");\n",
    pad,
    "  if (ISNAN(",
    var,
    "_v)) continue;\n",
    pad,
    "  ",
    var,
    "_sum += ",
    var,
    "_v;\n",
    pad,
    "  ",
    var,
    "_n++;\n",
    pad,
    "}\n",
    pad,
    "if (",
    var,
    "_n > 0) ",
    var,
    " = ",
    var,
    "_sum / (double)",
    var,
    "_n;"
  )
}

tccq_cg_sd_expr_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  if (is.null(node$var_name)) {
    node$var_name <- sprintf("sdexpr_%d", tccq_stat_expr_next_id())
  }
  var <- node$var_name
  len_c <- tccq_cg_vec_len(node$expr)
  if (is.null(len_c)) {
    stop("Cannot determine length for sd_expr", call. = FALSE)
  }
  na_rm <- isTRUE(node$na_rm)
  idx1 <- sprintf("sx%d_", tccq_stat_expr_counter_$n)
  idx2 <- sprintf("sy%d_", tccq_stat_expr_counter_$n)
  elem1 <- tccq_cg_vec_elem(node$expr, idx1)
  elem2 <- tccq_cg_vec_elem(node$expr, idx2)
  if (!na_rm) {
    return(paste0(
      pad,
      "double ",
      var,
      " = NA_REAL;\n",
      pad,
      "R_xlen_t ",
      var,
      "_n = (R_xlen_t)(",
      len_c,
      ");\n",
      pad,
      "if (",
      var,
      "_n > 1) {\n",
      pad,
      "  double ",
      var,
      "_sum = 0.0;\n",
      pad,
      "  int ",
      var,
      "_has_na = 0;\n",
      pad,
      "  for (R_xlen_t ",
      idx1,
      " = 0; ",
      idx1,
      " < ",
      var,
      "_n; ++",
      idx1,
      ") {\n",
      pad,
      "    double ",
      var,
      "_v = (double)(",
      elem1,
      ");\n",
      pad,
      "    if (ISNAN(",
      var,
      "_v)) { ",
      var,
      "_has_na = 1; break; }\n",
      pad,
      "    ",
      var,
      "_sum += ",
      var,
      "_v;\n",
      pad,
      "  }\n",
      pad,
      "  if (!",
      var,
      "_has_na) {\n",
      pad,
      "    double ",
      var,
      "_mean = ",
      var,
      "_sum / (double)",
      var,
      "_n;\n",
      pad,
      "    double ",
      var,
      "_ss = 0.0;\n",
      pad,
      "    for (R_xlen_t ",
      idx2,
      " = 0; ",
      idx2,
      " < ",
      var,
      "_n; ++",
      idx2,
      ") {\n",
      pad,
      "      double ",
      var,
      "_d = ((double)(",
      elem2,
      ")) - ",
      var,
      "_mean;\n",
      pad,
      "      ",
      var,
      "_ss += ",
      var,
      "_d * ",
      var,
      "_d;\n",
      pad,
      "    }\n",
      pad,
      "    ",
      var,
      " = sqrt(",
      var,
      "_ss / (double)(",
      var,
      "_n - 1));\n",
      pad,
      "  }\n",
      pad,
      "}"
    ))
  }
  paste0(
    pad,
    "double ",
    var,
    " = NA_REAL;\n",
    pad,
    "R_xlen_t ",
    var,
    "_n = 0;\n",
    pad,
    "  double ",
    var,
    "_sum = 0.0;\n",
    pad,
    "  for (R_xlen_t ",
    idx1,
    " = 0; ",
    idx1,
    " < (R_xlen_t)(",
    len_c,
    "); ++",
    idx1,
    ") {\n",
    pad,
    "    double ",
    var,
    "_v = (double)(",
    elem1,
    ");\n",
    pad,
    "    if (ISNAN(",
    var,
    "_v)) continue;\n",
    pad,
    "    ",
    var,
    "_sum += ",
    var,
    "_v;\n",
    pad,
    "    ",
    var,
    "_n++;\n",
    pad,
    "  }\n",
    pad,
    "  if (",
    var,
    "_n > 1) {\n",
    pad,
    "    double ",
    var,
    "_mean = ",
    var,
    "_sum / (double)",
    var,
    "_n;\n",
    pad,
    "    double ",
    var,
    "_ss = 0.0;\n",
    pad,
    "    for (R_xlen_t ",
    idx2,
    " = 0; ",
    idx2,
    " < (R_xlen_t)(",
    len_c,
    "); ++",
    idx2,
    ") {\n",
    pad,
    "      double ",
    var,
    "_v = (double)(",
    elem2,
    ");\n",
    pad,
    "      if (ISNAN(",
    var,
    "_v)) continue;\n",
    pad,
    "      double ",
    var,
    "_d = ",
    var,
    "_v - ",
    var,
    "_mean;\n",
    pad,
    "      ",
    var,
    "_ss += ",
    var,
    "_d * ",
    var,
    "_d;\n",
    pad,
    "    }\n",
    pad,
    "    ",
    var,
    " = sqrt(",
    var,
    "_ss / (double)(",
    var,
    "_n - 1));\n",
    pad,
    "  }"
  )
}

tccq_cg_median_expr_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  if (is.null(node$var_name)) {
    node$var_name <- sprintf("medexpr_%d", tccq_stat_expr_next_id())
  }
  var <- node$var_name
  len_c <- tccq_cg_vec_len(node$expr)
  if (is.null(len_c)) {
    stop("Cannot determine length for median_expr", call. = FALSE)
  }
  idx <- sprintf("md%d_", tccq_stat_expr_counter_$n)
  elem <- tccq_cg_vec_elem(node$expr, idx)
  na_rm <- isTRUE(node$na_rm)
  paste0(
    pad,
    "double ",
    var,
    " = NA_REAL;\n",
    pad,
    "R_xlen_t ",
    var,
    "_n = (R_xlen_t)(",
    len_c,
    ");\n",
    pad,
    "double *",
    var,
    "_tmp = (double *)R_alloc((size_t)",
    var,
    "_n, sizeof(double));\n",
    pad,
    "R_xlen_t ",
    var,
    "_k = 0;\n",
    pad,
    "for (R_xlen_t ",
    idx,
    " = 0; ",
    idx,
    " < ",
    var,
    "_n; ++",
    idx,
    ") {\n",
    pad,
    "  double ",
    var,
    "_v = (double)(",
    elem,
    ");\n",
    if (na_rm) {
      paste0(pad, "  if (ISNAN(", var, "_v)) continue;\n")
    } else {
      paste0(
        pad,
        "  if (ISNAN(",
        var,
        "_v)) { ",
        var,
        "_k = 0; goto ",
        var,
        "_done; }\n"
      )
    },
    pad,
    "  ",
    var,
    "_tmp[",
    var,
    "_k++] = ",
    var,
    "_v;\n",
    pad,
    "}\n",
    pad,
    "if (",
    var,
    "_k > 0) {\n",
    pad,
    "  if ((",
    var,
    "_k & 1) == 1) {\n",
    pad,
    "    int ",
    var,
    "_mid = (int)(",
    var,
    "_k / 2);\n",
    pad,
    "    rPsort(",
    var,
    "_tmp, (int)",
    var,
    "_k, ",
    var,
    "_mid);\n",
    pad,
    "    ",
    var,
    " = ",
    var,
    "_tmp[",
    var,
    "_mid];\n",
    pad,
    "  } else {\n",
    pad,
    "    int ",
    var,
    "_j0 = (int)(",
    var,
    "_k / 2) - 1;\n",
    pad,
    "    int ",
    var,
    "_j1 = (int)(",
    var,
    "_k / 2);\n",
    pad,
    "    rPsort(",
    var,
    "_tmp, (int)",
    var,
    "_k, ",
    var,
    "_j0);\n",
    pad,
    "    double ",
    var,
    "_x0 = ",
    var,
    "_tmp[",
    var,
    "_j0];\n",
    pad,
    "    rPsort(",
    var,
    "_tmp, (int)",
    var,
    "_k, ",
    var,
    "_j1);\n",
    pad,
    "    double ",
    var,
    "_x1 = ",
    var,
    "_tmp[",
    var,
    "_j1];\n",
    pad,
    "    ",
    var,
    " = (",
    var,
    "_x0 + ",
    var,
    "_x1) / 2.0;\n",
    pad,
    "  }\n",
    pad,
    "}\n",
    pad,
    var,
    "_done: ;"
  )
}

tccq_cg_quantile_expr_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  if (is.null(node$var_name)) {
    node$var_name <- sprintf("qexpr_%d", tccq_stat_expr_next_id())
  }
  var <- node$var_name
  len_c <- tccq_cg_vec_len(node$expr)
  if (is.null(len_c)) {
    stop("Cannot determine length for quantile_expr", call. = FALSE)
  }
  idx <- sprintf("qx%d_", tccq_stat_expr_counter_$n)
  elem <- tccq_cg_vec_elem(node$expr, idx)
  pexpr <- tccq_cg_expr(node$prob)
  na_rm <- isTRUE(node$na_rm)
  paste0(
    pad,
    "double ",
    var,
    " = NA_REAL;\n",
    pad,
    "double ",
    var,
    "_p = (double)(",
    pexpr,
    ");\n",
    pad,
    "R_xlen_t ",
    var,
    "_n_raw = (R_xlen_t)(",
    len_c,
    ");\n",
    pad,
    "double *",
    var,
    "_tmp = (double *)R_alloc((size_t)",
    var,
    "_n_raw, sizeof(double));\n",
    pad,
    "R_xlen_t ",
    var,
    "_n = 0;\n",
    pad,
    "for (R_xlen_t ",
    idx,
    " = 0; ",
    idx,
    " < ",
    var,
    "_n_raw; ++",
    idx,
    ") {\n",
    pad,
    "  double ",
    var,
    "_v = (double)(",
    elem,
    ");\n",
    if (na_rm) {
      paste0(pad, "  if (ISNAN(", var, "_v)) continue;\n")
    } else {
      paste0(
        pad,
        "  if (ISNAN(",
        var,
        "_v)) Rf_error(\"missing values and NaN's not allowed if 'na.rm' is FALSE\");\n"
      )
    },
    pad,
    "  ",
    var,
    "_tmp[",
    var,
    "_n++] = ",
    var,
    "_v;\n",
    pad,
    "}\n",
    pad,
    "if (ISNAN(",
    var,
    "_p)) {\n",
    pad,
    "  ",
    var,
    " = ISNA(",
    var,
    "_p) ? NA_REAL : R_NaN;\n",
    pad,
    "  goto ",
    var,
    "_done;\n",
    pad,
    "}\n",
    pad,
    "if (",
    var,
    "_p < 0.0 || ",
    var,
    "_p > 1.0) Rf_error(\"'probs' outside [0,1]\");\n",
    pad,
    "if (",
    var,
    "_n == 0) goto ",
    var,
    "_done;\n",
    pad,
    "if (",
    var,
    "_n == 1) {\n",
    pad,
    "  ",
    var,
    " = ",
    var,
    "_tmp[0];\n",
    pad,
    "} else {\n",
    pad,
    "  double ",
    var,
    "_h = (double)(",
    var,
    "_n - 1) * ",
    var,
    "_p + 1.0;\n",
    pad,
    "  double ",
    var,
    "_hf = floor(",
    var,
    "_h);\n",
    pad,
    "  double ",
    var,
    "_hc = ceil(",
    var,
    "_h);\n",
    pad,
    "  int ",
    var,
    "_i = (int)",
    var,
    "_hf - 1;\n",
    pad,
    "  int ",
    var,
    "_j = (int)",
    var,
    "_hc - 1;\n",
    pad,
    "  if (",
    var,
    "_i < 0) ",
    var,
    "_i = 0;\n",
    pad,
    "  if (",
    var,
    "_j < 0) ",
    var,
    "_j = 0;\n",
    pad,
    "  if (",
    var,
    "_i >= (int)",
    var,
    "_n) ",
    var,
    "_i = (int)",
    var,
    "_n - 1;\n",
    pad,
    "  if (",
    var,
    "_j >= (int)",
    var,
    "_n) ",
    var,
    "_j = (int)",
    var,
    "_n - 1;\n",
    pad,
    "  rPsort(",
    var,
    "_tmp, (int)",
    var,
    "_n, ",
    var,
    "_i);\n",
    pad,
    "  double ",
    var,
    "_x0 = ",
    var,
    "_tmp[",
    var,
    "_i];\n",
    pad,
    "  if (",
    var,
    "_j == ",
    var,
    "_i) {\n",
    pad,
    "    ",
    var,
    " = ",
    var,
    "_x0;\n",
    pad,
    "  } else {\n",
    pad,
    "    rPsort(",
    var,
    "_tmp, (int)",
    var,
    "_n, ",
    var,
    "_j);\n",
    pad,
    "    double ",
    var,
    "_x1 = ",
    var,
    "_tmp[",
    var,
    "_j];\n",
    pad,
    "    ",
    var,
    " = ",
    var,
    "_x0 + (",
    var,
    "_h - ",
    var,
    "_hf) * (",
    var,
    "_x1 - ",
    var,
    "_x0);\n",
    pad,
    "  }\n",
    pad,
    "}\n",
    pad,
    var,
    "_done: ;"
  )
}

tccq_cg_quantile_vec_stmt <- function(node, indent, out_name) {
  pad <- strrep("  ", indent)
  out <- tccq_cg_ident(out_name)
  len_x <- tccq_cg_vec_len(node$expr)
  len_p <- tccq_cg_vec_len(node$probs)
  if (is.null(len_x) || is.null(len_p)) {
    stop("Cannot determine length for quantile_vec_expr", call. = FALSE)
  }
  idx_x <- sprintf("qvx%d_", tccq_stat_expr_counter_$n)
  idx_p <- sprintf("qvp%d_", tccq_stat_expr_counter_$n)
  x_elem <- tccq_cg_vec_elem(node$expr, idx_x)
  p_elem <- tccq_cg_vec_elem(node$probs, idx_p)
  na_rm <- isTRUE(node$na_rm)

  paste0(
    pad,
    "R_xlen_t qv_nx_raw_ = (R_xlen_t)(",
    len_x,
    ");\n",
    pad,
    "R_xlen_t qv_np_ = (R_xlen_t)(",
    len_p,
    ");\n",
    pad,
    "s_",
    out,
    " = PROTECT(Rf_allocVector(REALSXP, qv_np_));\n",
    pad,
    "nprotect_++;\n",
    pad,
    "n_",
    out,
    " = qv_np_;\n",
    pad,
    "p_",
    out,
    " = REAL(s_",
    out,
    ");\n",
    pad,
    "for (R_xlen_t ",
    idx_p,
    " = 0; ",
    idx_p,
    " < qv_np_; ++",
    idx_p,
    ") p_",
    out,
    "[",
    idx_p,
    "] = NA_REAL;\n",
    pad,
    "double *qv_x_ = (double *)R_alloc((size_t)qv_nx_raw_, sizeof(double));\n",
    pad,
    "R_xlen_t qv_nx_ = 0;\n",
    pad,
    "for (R_xlen_t ",
    idx_x,
    " = 0; ",
    idx_x,
    " < qv_nx_raw_; ++",
    idx_x,
    ") {\n",
    pad,
    "  double v_ = (double)(",
    x_elem,
    ");\n",
    if (na_rm) {
      paste0(pad, "  if (ISNAN(v_)) continue;\n")
    } else {
      paste0(
        pad,
        "  if (ISNAN(v_)) Rf_error(\"missing values and NaN's not allowed if 'na.rm' is FALSE\");\n"
      )
    },
    pad,
    "  qv_x_[qv_nx_++] = v_;\n",
    pad,
    "}\n",
    pad,
    "if (qv_nx_ > 0) {\n",
    pad,
    "  R_rsort(qv_x_, (int)qv_nx_);\n",
    pad,
    "  for (R_xlen_t ",
    idx_p,
    " = 0; ",
    idx_p,
    " < qv_np_; ++",
    idx_p,
    ") {\n",
    pad,
    "    double p_ = (double)(",
    p_elem,
    ");\n",
    pad,
    "    if (ISNAN(p_)) { p_",
    out,
    "[",
    idx_p,
    "] = ISNA(p_) ? NA_REAL : R_NaN; continue; }\n",
    pad,
    "    if (p_ < 0.0 || p_ > 1.0) Rf_error(\"'probs' outside [0,1]\");\n",
    pad,
    "    if (qv_nx_ == 1) { p_",
    out,
    "[",
    idx_p,
    "] = qv_x_[0]; continue; }\n",
    pad,
    "    double h_ = (double)(qv_nx_ - 1) * p_ + 1.0;\n",
    pad,
    "    double hf_ = floor(h_);\n",
    pad,
    "    double hc_ = ceil(h_);\n",
    pad,
    "    int i_ = (int)hf_;\n",
    pad,
    "    int j_ = (int)hc_;\n",
    pad,
    "    double x0_ = qv_x_[i_ - 1];\n",
    pad,
    "    double x1_ = qv_x_[j_ - 1];\n",
    pad,
    "    p_",
    out,
    "[",
    idx_p,
    "] = x0_ + (h_ - hf_) * (x1_ - x0_);\n",
    pad,
    "  }\n",
    pad,
    "}"
  )
}

tccq_cg_matmul_stmt <- function(node, indent, out_name) {
  pad <- strrep("  ", indent)
  a <- tccq_cg_ident(node$a)
  b <- tccq_cg_ident(node$b)
  out <- tccq_cg_ident(out_name)
  ta <- isTRUE(node$trans_a)
  tb <- isTRUE(node$trans_b)
  trans_a <- if (ta) "T" else "N"
  trans_b <- if (tb) "T" else "N"

  m_expr <- if (ta) sprintf("ncol_%s", a) else sprintf("nrow_%s", a)
  k_expr <- if (ta) sprintf("nrow_%s", a) else sprintf("ncol_%s", a)
  kb_expr <- if (tb) sprintf("ncol_%s", b) else sprintf("nrow_%s", b)
  n_expr <- if (tb) sprintf("nrow_%s", b) else sprintf("ncol_%s", b)

  paste0(
    pad,
    "if ((int)(",
    k_expr,
    ") != (int)(",
    kb_expr,
    ")) Rf_error(\"matrix product dimension mismatch\");\n",
    pad,
    "nrow_",
    out,
    " = (int)(",
    m_expr,
    ");\n",
    pad,
    "ncol_",
    out,
    " = (int)(",
    n_expr,
    ");\n",
    pad,
    "s_",
    out,
    " = PROTECT(Rf_allocMatrix(REALSXP, nrow_",
    out,
    ", ncol_",
    out,
    "));\n",
    pad,
    "nprotect_++;\n",
    pad,
    "n_",
    out,
    " = (R_xlen_t)nrow_",
    out,
    " * (R_xlen_t)ncol_",
    out,
    ";\n",
    pad,
    "p_",
    out,
    " = REAL(s_",
    out,
    ");\n",
    pad,
    "{\n",
    pad,
    "  const char transa_ = '",
    trans_a,
    "';\n",
    pad,
    "  const char transb_ = '",
    trans_b,
    "';\n",
    pad,
    "  const int m_ = nrow_",
    out,
    ";\n",
    pad,
    "  const int n_ = ncol_",
    out,
    ";\n",
    pad,
    "  const int k_ = (int)(",
    k_expr,
    ");\n",
    pad,
    "  const double alpha_ = 1.0;\n",
    pad,
    "  const double beta_ = 0.0;\n",
    pad,
    "  const int lda_ = nrow_",
    a,
    ";\n",
    pad,
    "  const int ldb_ = nrow_",
    b,
    ";\n",
    pad,
    "  const int ldc_ = nrow_",
    out,
    ";\n",
    pad,
    "  F77_CALL(dgemm)(&transa_, &transb_, &m_, &n_, &k_, &alpha_, p_",
    a,
    ", &lda_, p_",
    b,
    ", &ldb_, &beta_, p_",
    out,
    ", &ldc_ FCONE FCONE);\n",
    pad,
    "}"
  )
}

tccq_cg_solve_lin_stmt <- function(node, indent, out_name) {
  pad <- strrep("  ", indent)
  a <- tccq_cg_ident(node$a)
  b <- tccq_cg_ident(node$b)
  out <- tccq_cg_ident(out_name)
  b_is_matrix <- identical(node$b_shape, "matrix")

  rhs_dim_check <- if (b_is_matrix) {
    paste0(
      pad,
      "  if ((int)nrow_",
      b,
      " != nsol_n_) Rf_error(\"solve dimension mismatch\");\n"
    )
  } else {
    paste0(
      pad,
      "  if ((int)n_",
      b,
      " != nsol_n_) Rf_error(\"solve dimension mismatch\");\n"
    )
  }
  rhs_n_expr <- if (b_is_matrix) sprintf("ncol_%s", b) else "1"

  out_alloc <- if (b_is_matrix) {
    paste0(
      pad,
      "  nrow_",
      out,
      " = nsol_n_;\n",
      pad,
      "  ncol_",
      out,
      " = nsol_nrhs_;\n",
      pad,
      "  s_",
      out,
      " = PROTECT(Rf_allocMatrix(REALSXP, nrow_",
      out,
      ", ncol_",
      out,
      "));\n",
      pad,
      "  nprotect_++;\n",
      pad,
      "  n_",
      out,
      " = (R_xlen_t)nrow_",
      out,
      " * (R_xlen_t)ncol_",
      out,
      ";\n",
      pad,
      "  p_",
      out,
      " = REAL(s_",
      out,
      ");\n"
    )
  } else {
    paste0(
      pad,
      "  s_",
      out,
      " = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)nsol_n_));\n",
      pad,
      "  nprotect_++;\n",
      pad,
      "  n_",
      out,
      " = (R_xlen_t)nsol_n_;\n",
      pad,
      "  p_",
      out,
      " = REAL(s_",
      out,
      ");\n"
    )
  }

  paste0(
    pad,
    "{\n",
    pad,
    "  int nsol_n_ = (int)nrow_",
    a,
    ";\n",
    pad,
    "  if ((int)ncol_",
    a,
    " != nsol_n_) Rf_error(\"solve requires a square matrix\");\n",
    rhs_dim_check,
    pad,
    "  int nsol_nrhs_ = (int)(",
    rhs_n_expr,
    ");\n",
    pad,
    "  int nsol_lda_ = nsol_n_;\n",
    pad,
    "  int nsol_ldb_ = nsol_n_;\n",
    pad,
    "  int nsol_info_ = 0;\n",
    pad,
    "  int *nsol_ipiv_ = (int *)R_alloc((size_t)nsol_n_, sizeof(int));\n",
    pad,
    "  double *nsol_a_ = (double *)R_alloc((size_t)nsol_n_ * (size_t)nsol_n_, sizeof(double));\n",
    pad,
    "  double *nsol_b_ = (double *)R_alloc((size_t)nsol_n_ * (size_t)nsol_nrhs_, sizeof(double));\n",
    pad,
    "  for (R_xlen_t i_ = 0; i_ < (R_xlen_t)nsol_n_ * (R_xlen_t)nsol_n_; ++i_) nsol_a_[i_] = p_",
    a,
    "[i_];\n",
    pad,
    "  for (R_xlen_t i_ = 0; i_ < (R_xlen_t)nsol_n_ * (R_xlen_t)nsol_nrhs_; ++i_) nsol_b_[i_] = p_",
    b,
    "[i_];\n",
    pad,
    "  F77_CALL(dgesv)(&nsol_n_, &nsol_nrhs_, nsol_a_, &nsol_lda_, nsol_ipiv_, nsol_b_, &nsol_ldb_, &nsol_info_);\n",
    pad,
    "  if (nsol_info_ < 0) Rf_error(\"dgesv: invalid argument\");\n",
    pad,
    "  if (nsol_info_ > 0) Rf_error(\"solve failed: singular system\");\n",
    out_alloc,
    pad,
    "  for (R_xlen_t i_ = 0; i_ < (R_xlen_t)nsol_n_ * (R_xlen_t)nsol_nrhs_; ++i_) p_",
    out,
    "[i_] = nsol_b_[i_];\n",
    pad,
    "}"
  )
}

tccq_cg_transpose_stmt <- function(node, indent, out_name, mode = "double") {
  pad <- strrep("  ", indent)
  a <- tccq_cg_ident(node$a)
  out <- tccq_cg_ident(out_name)

  sxp_type <- switch(
    mode,
    integer = "INTSXP",
    logical = "LGLSXP",
    raw = "RAWSXP",
    "REALSXP"
  )
  ptr_fun <- switch(
    mode,
    integer = "INTEGER",
    logical = "LOGICAL",
    raw = "RAW",
    "REAL"
  )

  paste0(
    pad,
    "nrow_",
    out,
    " = ncol_",
    a,
    ";\n",
    pad,
    "ncol_",
    out,
    " = nrow_",
    a,
    ";\n",
    pad,
    "s_",
    out,
    " = PROTECT(Rf_allocMatrix(",
    sxp_type,
    ", nrow_",
    out,
    ", ncol_",
    out,
    "));\n",
    pad,
    "nprotect_++;\n",
    pad,
    "n_",
    out,
    " = (R_xlen_t)nrow_",
    out,
    " * (R_xlen_t)ncol_",
    out,
    ";\n",
    pad,
    "p_",
    out,
    " = ",
    ptr_fun,
    "(s_",
    out,
    ");\n",
    pad,
    "for (int j_ = 0; j_ < ncol_",
    a,
    "; ++j_) {\n",
    pad,
    "  for (int i_ = 0; i_ < nrow_",
    a,
    "; ++i_) {\n",
    pad,
    "    p_",
    out,
    "[i_ * nrow_",
    out,
    " + j_] = p_",
    a,
    "[j_ * nrow_",
    a,
    " + i_];\n",
    pad,
    "  }\n",
    pad,
    "}"
  )
}

tccq_cg_mat_reduce_stmt <- function(node, indent, out_name) {
  pad <- strrep("  ", indent)
  arr <- tccq_cg_ident(node$arr)
  out <- tccq_cg_ident(out_name)
  op <- node$op
  na_rm <- isTRUE(node$na_rm)
  by_row <- op %in% c("rowSums", "rowMeans")
  is_mean <- op %in% c("rowMeans", "colMeans")

  idx_outer <- if (by_row) "i_" else "j_"
  idx_inner <- if (by_row) "j_" else "i_"
  out_len_expr <- if (by_row) {
    sprintf("nrow_%s", arr)
  } else {
    sprintf("ncol_%s", arr)
  }

  lines <- c(
    paste0(
      pad,
      "s_",
      out,
      " = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)",
      out_len_expr,
      "));"
    ),
    paste0(pad, "nprotect_++;"),
    paste0(pad, "n_", out, " = XLENGTH(s_", out, ");"),
    paste0(pad, "p_", out, " = REAL(s_", out, ");"),
    paste0(
      pad,
      "for (R_xlen_t k_ = 0; k_ < n_",
      out,
      "; ++k_) p_",
      out,
      "[k_] = 0.0;"
    )
  )

  if (!na_rm) {
    lines <- c(
      lines,
      paste0(
        pad,
        "int *",
        out,
        "_bad = (int *)R_alloc((size_t)n_",
        out,
        ", sizeof(int));"
      ),
      paste0(
        pad,
        "for (R_xlen_t k_ = 0; k_ < n_",
        out,
        "; ++k_) ",
        out,
        "_bad[k_] = 0;"
      )
    )
  }

  if (is_mean) {
    lines <- c(
      lines,
      paste0(
        pad,
        "R_xlen_t *",
        out,
        "_cnt = (R_xlen_t *)R_alloc((size_t)n_",
        out,
        ", sizeof(R_xlen_t));"
      ),
      paste0(
        pad,
        "for (R_xlen_t k_ = 0; k_ < n_",
        out,
        "; ++k_) ",
        out,
        "_cnt[k_] = 0;"
      )
    )
  }

  lines <- c(
    lines,
    paste0(
      pad,
      "for (int ",
      idx_outer,
      " = 0; ",
      idx_outer,
      " < ",
      out_len_expr,
      "; ++",
      idx_outer,
      ") {"
    ),
    paste0(
      pad,
      "  for (int ",
      idx_inner,
      " = 0; ",
      idx_inner,
      " < ",
      if (by_row) sprintf("ncol_%s", arr) else sprintf("nrow_%s", arr),
      "; ++",
      idx_inner,
      ") {"
    ),
    paste0(
      pad,
      "    double v_ = (double)p_",
      arr,
      "[",
      if (by_row) {
        paste0(idx_outer, " + ", idx_inner, " * nrow_", arr)
      } else {
        paste0(idx_inner, " + ", idx_outer, " * nrow_", arr)
      },
      "];"
    )
  )

  if (na_rm) {
    lines <- c(lines, paste0(pad, "    if (ISNAN(v_)) continue;"))
    if (is_mean) {
      lines <- c(lines, paste0(pad, "    ", out, "_cnt[", idx_outer, "]++;"))
    }
    lines <- c(lines, paste0(pad, "    p_", out, "[", idx_outer, "] += v_;"))
  } else {
    lines <- c(
      lines,
      paste0(pad, "    if (", out, "_bad[", idx_outer, "]) continue;"),
      paste0(
        pad,
        "    if (ISNAN(v_)) { ",
        out,
        "_bad[",
        idx_outer,
        "] = 1; p_",
        out,
        "[",
        idx_outer,
        "] = NA_REAL; continue; }"
      ),
      paste0(pad, "    p_", out, "[", idx_outer, "] += v_;")
    )
    if (is_mean) {
      lines <- c(lines, paste0(pad, "    ", out, "_cnt[", idx_outer, "]++;"))
    }
  }

  lines <- c(lines, paste0(pad, "  }"), paste0(pad, "}"))

  if (is_mean) {
    lines <- c(
      lines,
      paste0(pad, "for (R_xlen_t k_ = 0; k_ < n_", out, "; ++k_) {"),
      paste0(
        pad,
        "  if (",
        out,
        "_cnt[k_] > 0) p_",
        out,
        "[k_] /= (double)",
        out,
        "_cnt[k_]; else p_",
        out,
        "[k_] = NA_REAL;"
      ),
      paste0(pad, "}")
    )
  }

  paste(lines, collapse = "\n")
}

# Cumulative ops: cumsum/cumprod/cummax/cummin
# target_ptr: C pointer expression (e.g. "p_result" or "p_ret_").
tccq_cg_cumulative_stmt <- function(node, indent, target_ptr) {
  pad <- strrep("  ", indent)
  op <- node$op
  idx <- "ci_"
  elem0 <- tccq_cg_vec_elem(node$expr, "0")
  elem_i <- tccq_cg_vec_elem(node$expr, idx)
  len_c <- tccq_cg_vec_len(node$expr)

  update <- switch(
    op,
    cumsum = sprintf(
      "%s[%s] = %s[%s - 1] + %s;",
      target_ptr,
      idx,
      target_ptr,
      idx,
      elem_i
    ),
    cumprod = sprintf(
      "%s[%s] = %s[%s - 1] * %s;",
      target_ptr,
      idx,
      target_ptr,
      idx,
      elem_i
    ),
    cummax = sprintf(
      "{ double v_ = %s; %s[%s] = v_ > %s[%s - 1] ? v_ : %s[%s - 1]; }",
      elem_i,
      target_ptr,
      idx,
      target_ptr,
      idx,
      target_ptr,
      idx
    ),
    cummin = sprintf(
      "{ double v_ = %s; %s[%s] = v_ < %s[%s - 1] ? v_ : %s[%s - 1]; }",
      elem_i,
      target_ptr,
      idx,
      target_ptr,
      idx,
      target_ptr,
      idx
    ),
    ""
  )
  paste0(
    pad,
    target_ptr,
    "[0] = ",
    elem0,
    ";\n",
    pad,
    "for (R_xlen_t ",
    idx,
    " = 1; ",
    idx,
    " < (R_xlen_t)(",
    len_c,
    "); ++",
    idx,
    ") {\n",
    pad,
    "  ",
    update,
    "\n",
    pad,
    "}"
  )
}

# ---------------------------------------------------------------------------
# rf_call: emit Rf_lang + Rf_eval to call R function from generated C
# ---------------------------------------------------------------------------

tccq_cg_rf_arg_len <- function(arg, arg_sexp) {
  shape <- arg$shape %||% "scalar"
  if (shape == "scalar") {
    return("1")
  }
  if (shape %in% c("vector", "matrix")) {
    return(sprintf("XLENGTH(%s)", arg_sexp))
  }
  tccq_cg_vec_len(arg) %||% "1"
}

tccq_cg_rf_contract_checks <- function(node, var, arg_len_c, indent) {
  pad <- strrep("  ", indent)
  fun <- node$fun %||% "<unknown>"
  mode <- node$mode %||% "double"
  shape <- node$shape %||% "scalar"
  sxp_type <- tccq_cg_sexp_type(mode)
  lines <- c()

  type_msg <- sprintf(
    "tcc_quick delegated call '%s' expected %s/%s result, got TYPEOF=%%d",
    fun,
    shape,
    mode
  )
  lines <- c(
    lines,
    paste0(
      pad,
      "if (TYPEOF(",
      var,
      ") != ",
      sxp_type,
      ") Rf_error(\"",
      type_msg,
      "\", TYPEOF(",
      var,
      "));"
    )
  )

  if (shape == "scalar") {
    lines <- c(
      lines,
      paste0(
        pad,
        "if (Rf_isMatrix(",
        var,
        ") || XLENGTH(",
        var,
        ") != 1) Rf_error(\"tcc_quick delegated call '",
        fun,
        "' expected scalar result\");"
      )
    )
  } else if (shape == "vector") {
    lines <- c(
      lines,
      paste0(
        pad,
        "if (Rf_isMatrix(",
        var,
        ")) Rf_error(\"tcc_quick delegated call '",
        fun,
        "' expected vector result\");"
      )
    )
  } else if (shape == "matrix") {
    lines <- c(
      lines,
      paste0(
        pad,
        "if (!Rf_isMatrix(",
        var,
        ")) Rf_error(\"tcc_quick delegated call '",
        fun,
        "' expected matrix result\");"
      )
    )
  }

  length_rule <- node$contract$length_rule %||% "none"
  if (length_rule == "arg1" && length(arg_len_c) >= 1L) {
    expect_len <- arg_len_c[[1L]]
    lines <- c(
      lines,
      paste0(
        pad,
        "if (XLENGTH(",
        var,
        ") != (R_xlen_t)(",
        expect_len,
        ")) Rf_error(\"tcc_quick delegated call '",
        fun,
        "' length mismatch: expected %lld got %lld\", ",
        "(long long)(",
        expect_len,
        "), (long long)XLENGTH(",
        var,
        "));"
      )
    )
  } else if (length_rule == "sum_args" && length(arg_len_c) >= 1L) {
    expect_len <- paste(sprintf("((R_xlen_t)(%s))", arg_len_c), collapse = " + ")
    lines <- c(
      lines,
      paste0(
        pad,
        "if (XLENGTH(",
        var,
        ") != (R_xlen_t)(",
        expect_len,
        ")) Rf_error(\"tcc_quick delegated call '",
        fun,
        "' length mismatch: expected %lld got %lld\", ",
        "(long long)(",
        expect_len,
        "), (long long)XLENGTH(",
        var,
        "));"
      )
    )
  }

  lines
}

tccq_cg_rf_call_stmt <- function(node, indent) {
  pad <- strrep("  ", indent)
  fun <- node$fun
  args <- node$args
  n <- length(args)
  if (!is.null(node$var_name)) {
    var <- node$var_name
    uid <- as.integer(sub(".*_(\\d+)_$", "\\1", var))
  } else {
    uid <- tccq_rfcall_next_id()
    var <- sprintf("rfcall_%s_%d_", tccq_cg_ident(fun), uid)
  }

  lines <- c()

  # Build the argument SEXPs - scalars need wrapping, vectors use s_name.
  # Nested rf_calls are recursively emitted first.
  arg_c <- character(n)
  arg_len_c <- character(n)
  for (i in seq_len(n)) {
    a <- args[[i]]
    a_shape <- a$shape %||% "scalar"
    a_tag <- a$tag %||% ""

    if (a_tag == "rf_call") {
      # Nested rf_call - emit it first, use its result SEXP
      child <- tccq_cg_rf_call_stmt(a, indent)
      lines <- c(lines, child$lines)
      arg_c[i] <- child$var
      arg_len_c[i] <- tccq_cg_rf_arg_len(a, child$var)
    } else if (a_tag == "matmul") {
      # Matrix expression used as argument to an R call: materialize first
      tmpm <- sprintf("rfmat_%s_%d_%d", tccq_cg_ident(fun), uid, i)
      lines <- c(
        lines,
        paste0(pad, "SEXP s_", tmpm, " = R_NilValue;"),
        paste0(pad, "R_xlen_t n_", tmpm, " = 0;"),
        paste0(pad, "double *p_", tmpm, " = NULL;"),
        paste0(pad, "int nrow_", tmpm, " = 0;"),
        paste0(pad, "int ncol_", tmpm, " = 0;")
      )
      lines <- c(lines, tccq_cg_matmul_stmt(a, indent, tmpm))
      arg_c[i] <- sprintf("s_%s", tmpm)
      arg_len_c[i] <- sprintf("n_%s", tmpm)
    } else if (a_tag == "var" && a_shape %in% c("vector", "matrix")) {
      # Vector / matrix variable - pass the SEXP directly
      arg_c[i] <- sprintf("s_%s", tccq_cg_ident(a$name))
      arg_len_c[i] <- tccq_cg_rf_arg_len(a, arg_c[i])
    } else if (a_tag == "var" && a_shape == "scalar") {
      # Scalar variable - re-wrap from C scalar to SEXP
      c_nm <- tccq_cg_ident(a$name)
      a_mode <- a$mode %||% "double"
      wrap <- switch(
        a_mode,
        integer = sprintf("Rf_ScalarInteger(%s_)", c_nm),
        logical = sprintf("Rf_ScalarLogical(%s_)", c_nm),
        raw = sprintf("Rf_ScalarRaw((Rbyte)(%s_))", c_nm),
        sprintf("Rf_ScalarReal(%s_)", c_nm)
      )
      tmp <- sprintf("rfarg_%s_%d_%d_", tccq_cg_ident(fun), uid, i)
      lines <- c(lines, paste0(pad, "SEXP ", tmp, " = PROTECT(", wrap, ");"))
      lines <- c(lines, paste0(pad, "nprotect_++;"))
      arg_c[i] <- tmp
      arg_len_c[i] <- "1"
    } else {
      # Scalar expression - wrap as SEXP
      val <- tccq_cg_expr(a)
      mode <- a$mode %||% "double"
      wrap <- switch(
        mode,
        integer = sprintf("Rf_ScalarInteger((int)(%s))", val),
        logical = sprintf("Rf_ScalarLogical((%s) ? 1 : 0)", val),
        raw = sprintf("Rf_ScalarRaw((Rbyte)(%s))", val),
        sprintf("Rf_ScalarReal((double)(%s))", val)
      )
      tmp <- sprintf("rfarg_%s_%d_%d_", tccq_cg_ident(fun), uid, i)
      lines <- c(lines, paste0(pad, "SEXP ", tmp, " = PROTECT(", wrap, ");"))
      lines <- c(lines, paste0(pad, "nprotect_++;"))
      arg_c[i] <- tmp
      arg_len_c[i] <- "1"
    }
  }

  # Emit Rf_lang call.  Use Rf_install for the function symbol.
  # For operators like %*%, Rf_install handles them fine.
  fun_sym <- sprintf("Rf_install(\"%s\")", fun)
  lang_n <- n + 1L
  if (lang_n <= 6L) {
    lang_args <- paste(c(fun_sym, arg_c), collapse = ", ")
    lines <- c(
      lines,
      paste0(
        pad,
        "SEXP ",
        var,
        " = PROTECT(Rf_eval(PROTECT(",
        sprintf("Rf_lang%d(%s)", lang_n, lang_args),
        "), tccq_env));"
      ),
      paste0(pad, "nprotect_ += 2;")
    )
  } else {
    # Fallback: build LCONS manually for high-arity calls
    lines <- c(
      lines,
      paste0(pad, "SEXP rfcall_e_ = PROTECT(Rf_allocList(", lang_n, "));"),
      paste0(pad, "nprotect_++;"),
      paste0(pad, "SETCAR(rfcall_e_, ", fun_sym, ");"),
      paste0(pad, "SET_TYPEOF(rfcall_e_, LANGSXP);")
    )
    lines <- c(lines, paste0(pad, "SEXP rfcall_t_ = CDR(rfcall_e_);"))
    for (i in seq_len(n)) {
      lines <- c(
        lines,
        paste0(pad, "SETCAR(rfcall_t_, ", arg_c[i], ");")
      )
      if (i < n) {
        lines <- c(lines, paste0(pad, "rfcall_t_ = CDR(rfcall_t_);"))
      }
    }
    lines <- c(
      lines,
      paste0(
        pad,
        "SEXP ",
        var,
        " = PROTECT(Rf_eval(rfcall_e_, tccq_env));"
      ),
      paste0(pad, "nprotect_++;")
    )
  }

  lines <- c(lines, tccq_cg_rf_contract_checks(node, var, arg_len_c, indent))

  list(lines = paste(lines, collapse = "\n"), var = var)
} # ---------------------------------------------------------------------------
# vec_mask: x[mask] - count TRUEs, allocate, fill
# ---------------------------------------------------------------------------

tccq_cg_vec_mask_assign <- function(node, indent) {
  pad <- strrep("  ", indent)
  nm <- tccq_cg_ident(node$name)
  arr <- tccq_cg_ident(node$expr$arr)
  mask_node <- node$expr$mask

  # The mask must be a named variable for now
  if (!identical(mask_node$tag, "var")) {
    stop("vec_mask codegen requires a named mask variable", call. = FALSE)
  }
  mask <- tccq_cg_ident(mask_node$name)

  sxp_type <- switch(
    node$mode,
    integer = "INTSXP",
    logical = "LGLSXP",
    raw = "RAWSXP",
    "REALSXP"
  )
  ptr_fun <- switch(
    node$mode,
    integer = "INTEGER",
    logical = "LOGICAL",
    raw = "RAW",
    "REAL"
  )

  paste0(
    pad,
    "// vec_mask: ",
    nm,
    " = ",
    arr,
    "[",
    mask,
    "]\n",
    pad,
    "R_xlen_t vm_cnt_ = 0;\n",
    pad,
    "for (R_xlen_t vm_ = 0; vm_ < n_",
    mask,
    "; ++vm_) {\n",
    pad,
    "  if (p_",
    mask,
    "[vm_]) vm_cnt_++;\n",
    pad,
    "}\n",
    pad,
    "s_",
    nm,
    " = PROTECT(Rf_allocVector(",
    sxp_type,
    ", vm_cnt_));\n",
    pad,
    "nprotect_++;\n",
    pad,
    "n_",
    nm,
    " = vm_cnt_;\n",
    pad,
    "p_",
    nm,
    " = ",
    ptr_fun,
    "(s_",
    nm,
    ");\n",
    pad,
    "{\n",
    pad,
    "  R_xlen_t vm_j_ = 0;\n",
    pad,
    "  for (R_xlen_t vm_ = 0; vm_ < n_",
    mask,
    "; ++vm_) {\n",
    pad,
    "    if (p_",
    mask,
    "[vm_]) {\n",
    pad,
    "      p_",
    nm,
    "[vm_j_++] = p_",
    arr,
    "[vm_];\n",
    pad,
    "    }\n",
    pad,
    "  }\n",
    pad,
    "}"
  )
}

tccq_collect_reductions <- function(node) {
  if (is.null(node) || is.null(node$tag)) {
    return(list())
  }

  out <- list()
  if (
    node$tag %in%
      c(
        "reduce",
        "which_reduce",
        "reduce_expr",
        "mean_expr",
        "sd_expr",
        "median_expr",
        "quantile_expr"
      )
  ) {
    out <- list(node)
  }

  for (nm in names(node)) {
    child <- node[[nm]]
    if (is.list(child) && !is.null(child$tag)) {
      out <- c(out, tccq_collect_reductions(child))
    }
    if (is.list(child) && is.null(child$tag)) {
      for (item in child) {
        if (is.list(item) && !is.null(item$tag)) {
          out <- c(out, tccq_collect_reductions(item))
        }
      }
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Vector expression return: materialize element-wise into a new vector
# ---------------------------------------------------------------------------

tccq_cg_vec_expr_return <- function(ir, indent) {
  pad <- strrep("  ", indent)

  # Special case: cumulative ops can't be expressed element-wise
  if (!is.null(ir$ret$tag) && ir$ret$tag == "cumulative") {
    len_c <- tccq_cg_vec_len(ir$ret$expr)
    if (is.null(len_c)) {
      stop("Cannot determine length for cumulative return", call. = FALSE)
    }
    sxp_type <- switch(
      ir$ret_mode,
      integer = "INTSXP",
      logical = "LGLSXP",
      raw = "RAWSXP",
      "REALSXP"
    )
    ptr_fun <- switch(
      ir$ret_mode,
      integer = "INTEGER",
      logical = "LOGICAL",
      raw = "RAW",
      "REAL"
    )
    alloc_lines <- paste0(
      pad,
      "R_xlen_t ret_len_ = (R_xlen_t)(",
      len_c,
      ");\n",
      pad,
      "SEXP ret_ = PROTECT(Rf_allocVector(",
      sxp_type,
      ", ret_len_));\n",
      pad,
      "nprotect_++;\n",
      pad,
      switch(
        ir$ret_mode,
        integer = "int",
        logical = "int",
        raw = "Rbyte",
        "double"
      ),
      " *p_ret_ = ",
      ptr_fun,
      "(ret_);"
    )
    cum_lines <- tccq_cg_cumulative_stmt(ir$ret, indent, "p_ret_")
    return(paste0(
      alloc_lines,
      "\n",
      cum_lines,
      "\n",
      pad,
      "UNPROTECT(nprotect_);\n",
      pad,
      "return ret_;"
    ))
  }

  len_c <- tccq_cg_vec_len(ir$ret)
  if (is.null(len_c)) {
    stop("Cannot determine length for vector return expression", call. = FALSE)
  }
  elem_expr <- tccq_cg_vec_elem(ir$ret, "ei_")

  sxp_type <- switch(
    ir$ret_mode,
    double = "REALSXP",
    integer = "INTSXP",
    logical = "LGLSXP",
    raw = "RAWSXP",
    "REALSXP"
  )
  ptr_fun <- switch(
    ir$ret_mode,
    double = "REAL",
    integer = "INTEGER",
    logical = "LOGICAL",
    raw = "RAW",
    "REAL"
  )
  c_type <- switch(
    ir$ret_mode,
    double = "double",
    integer = "int",
    logical = "int",
    raw = "Rbyte",
    "double"
  )

  paste0(
    pad,
    "R_xlen_t ret_len_ = (R_xlen_t)(",
    len_c,
    ");\n",
    pad,
    "SEXP ret_ = PROTECT(Rf_allocVector(",
    sxp_type,
    ", ret_len_));\n",
    pad,
    "nprotect_++;\n",
    pad,
    c_type,
    " *p_ret_ = ",
    ptr_fun,
    "(ret_);\n",
    pad,
    "for (R_xlen_t ei_ = 0; ei_ < ret_len_; ++ei_) {\n",
    pad,
    "  p_ret_[ei_] = (",
    c_type,
    ")(",
    elem_expr,
    ");\n",
    pad,
    "}\n",
    pad,
    "UNPROTECT(nprotect_);\n",
    pad,
    "return ret_;"
  )
}

# Infer C length expression for a vector IR node
tccq_cg_vec_len <- function(node) {
  tag <- node$tag
  if (tag == "var") {
    return(sprintf("n_%s", tccq_cg_ident(node$name)))
  }
  if (tag == "seq_len") {
    n <- tccq_cg_expr(node$n)
    return(sprintf("((R_xlen_t)(%s))", n))
  }
  if (tag == "seq_range") {
    from <- tccq_cg_expr(node$from)
    to <- tccq_cg_expr(node$to)
    return(sprintf(
      "(((R_xlen_t)(%s) <= (R_xlen_t)(%s)) ? ((R_xlen_t)(%s) - (R_xlen_t)(%s) + 1) : ((R_xlen_t)(%s) - (R_xlen_t)(%s) + 1))",
      from,
      to,
      to,
      from,
      from,
      to
    ))
  }
  if (tag == "vec_slice") {
    from <- tccq_cg_expr(node$from)
    to <- tccq_cg_expr(node$to)
    return(sprintf("((R_xlen_t)(%s) - (R_xlen_t)(%s) + 1)", to, from))
  }
  if (tag == "binary") {
    return(tccq_cg_vec_len(node$lhs) %||% tccq_cg_vec_len(node$rhs))
  }
  if (tag == "unary") {
    return(tccq_cg_vec_len(node$x))
  }
  if (tag == "cast") {
    return(tccq_cg_vec_len(node$x))
  }
  if (tag == "call1") {
    return(tccq_cg_vec_len(node$x))
  }
  if (tag == "call2") {
    return(tccq_cg_vec_len(node$x) %||% tccq_cg_vec_len(node$y))
  }
  if (tag == "rev") {
    return(tccq_cg_vec_len(node$expr))
  }
  if (tag == "cumulative") {
    return(tccq_cg_vec_len(node$expr))
  }
  if (tag == "quantile_vec_expr") {
    return(tccq_cg_vec_len(node$probs))
  }
  if (tag == "if") {
    return(tccq_cg_vec_len(node$yes) %||% tccq_cg_vec_len(node$no))
  }
  if (tag == "rf_call") {
    if (!is.null(node$var_name)) {
      return(sprintf("XLENGTH(%s)", node$var_name))
    }
  }
  NULL
}

# Generate C for the i-th element (0-based) of a vector expression
tccq_cg_vec_elem <- function(node, idx_var) {
  tag <- node$tag

  if (tag == "var") {
    return(sprintf("p_%s[%s]", tccq_cg_ident(node$name), idx_var))
  }
  if (tag == "seq_len") {
    return(sprintf("((int)(1 + (R_xlen_t)(%s)))", idx_var))
  }
  if (tag == "seq_range") {
    from <- tccq_cg_expr(node$from)
    to <- tccq_cg_expr(node$to)
    return(sprintf(
      "(((R_xlen_t)(%s) <= (R_xlen_t)(%s)) ? ((R_xlen_t)(%s) + (R_xlen_t)(%s)) : ((R_xlen_t)(%s) - (R_xlen_t)(%s)))",
      from,
      to,
      from,
      idx_var,
      from,
      idx_var
    ))
  }

  if (tag == "vec_slice") {
    arr <- tccq_cg_ident(node$arr)
    from <- tccq_cg_expr(node$from)
    return(sprintf("p_%s[((R_xlen_t)(%s) - 1) + %s]", arr, from, idx_var))
  }

  if (tag == "const") {
    if (node$mode == "integer") {
      return(sprintf("%d", as.integer(node$value)))
    }
    if (node$mode == "double") {
      return(format(as.double(node$value), scientific = FALSE, trim = TRUE))
    }
    if (node$mode == "logical") {
      return(if (isTRUE(node$value)) "1" else "0")
    }
    if (node$mode == "raw") {
      return(sprintf("((Rbyte)%d)", as.integer(node$value)))
    }
  }

  if (tag == "unary") {
    x <- tccq_cg_vec_elem(node$x, idx_var)
    if (node$op == "!") {
      return(sprintf("(!(%s))", x))
    }
    return(sprintf("(%s(%s))", node$op, x))
  }

  if (tag == "cast") {
    x <- tccq_cg_vec_elem(node$x, idx_var)
    if (node$target_mode == "integer") {
      return(sprintf("((int)(%s))", x))
    }
    if (node$target_mode == "double") {
      return(sprintf("((double)(%s))", x))
    }
    if (node$target_mode == "raw") {
      return(sprintf("((Rbyte)(%s))", x))
    }
    return(x)
  }

  if (tag == "binary") {
    lhs <- tccq_cg_vec_elem(node$lhs, idx_var)
    rhs <- tccq_cg_vec_elem(node$rhs, idx_var)
    op <- node$op
    if (op == "%/%") {
      return(sprintf("((int)floor((double)(%s) / (double)(%s)))", lhs, rhs))
    }
    if (op == "%%") {
      return(sprintf("(fmod((double)(%s), (double)(%s)))", lhs, rhs))
    }
    if (op == "^") {
      return(sprintf("(pow((double)(%s), (double)(%s)))", lhs, rhs))
    }
    if (op == "&") {
      op <- "&&"
    }
    if (op == "|") {
      op <- "||"
    }
    return(sprintf("((%s) %s (%s))", lhs, op, rhs))
  }

  if (tag == "call1") {
    x <- tccq_cg_vec_elem(node$x, idx_var)
    fun <- tccq_cg_c_fun(node$fun)
    return(sprintf("(%s((double)(%s)))", fun, x))
  }

  if (tag == "call2") {
    x <- tccq_cg_vec_elem(node$x, idx_var)
    y <- tccq_cg_vec_elem(node$y, idx_var)
    fun <- node$fun
    if (fun == "pmin") {
      return(sprintf("((%s) < (%s) ? (%s) : (%s))", x, y, x, y))
    }
    if (fun == "pmax") {
      return(sprintf("((%s) > (%s) ? (%s) : (%s))", x, y, x, y))
    }
    if (fun == "bitwAnd") {
      return(sprintf("(((int)(%s)) & ((int)(%s)))", x, y))
    }
    if (fun == "bitwOr") {
      return(sprintf("(((int)(%s)) | ((int)(%s)))", x, y))
    }
    if (fun == "bitwXor") {
      return(sprintf("(((int)(%s)) ^ ((int)(%s)))", x, y))
    }
    if (fun == "bitwShiftL") {
      return(sprintf("(((int)(%s)) << ((int)(%s)))", x, y))
    }
    if (fun == "bitwShiftR") {
      return(sprintf("(((int)(%s)) >> ((int)(%s)))", x, y))
    }
    c_fun <- tccq_cg_c_fun(fun)
    return(sprintf("(%s((double)(%s), (double)(%s)))", c_fun, x, y))
  }

  if (tag == "if") {
    cond <- tccq_cg_vec_elem(node$cond, idx_var)
    yes <- tccq_cg_vec_elem(node$yes, idx_var)
    no <- tccq_cg_vec_elem(node$no, idx_var)
    return(sprintf("((%s) ? (%s) : (%s))", cond, yes, no))
  }

  if (tag == "rf_call") {
    # Hoisted rf_call result - access element via typed pointer.
    if (!is.null(node$var_name)) {
      mode <- node$mode %||% "double"
      if (mode == "integer") {
        return(sprintf("INTEGER(%s)[%s]", node$var_name, idx_var))
      }
      if (mode == "logical") {
        return(sprintf("LOGICAL(%s)[%s]", node$var_name, idx_var))
      }
      if (mode == "raw") {
        return(sprintf("RAW(%s)[%s]", node$var_name, idx_var))
      }
      return(sprintf("REAL(%s)[%s]", node$var_name, idx_var))
    }
    stop("rf_call in vec_elem without var_name", call. = FALSE)
  }

  if (tag == "rev") {
    len <- tccq_cg_vec_len(node$expr)
    inner <- tccq_cg_vec_elem(
      node$expr,
      sprintf("((%s) - 1 - %s)", len, idx_var)
    )
    return(inner)
  }

  # Scalar expression fallback (constants broadcast)
  tccq_cg_expr(node)
}

# ---------------------------------------------------------------------------
# Pre-pass: assign var_name to all rf_call nodes in the IR tree.
# ---------------------------------------------------------------------------

tccq_assign_rf_call_names <- function(node, counter_env = NULL) {
  if (is.null(counter_env)) {
    counter_env <- new.env(parent = emptyenv())
    counter_env$n <- 0L
  }
  if (is.null(node) || !is.list(node)) {
    return(node)
  }
  if (is.null(node$tag)) {
    return(lapply(node, tccq_assign_rf_call_names, counter_env))
  }

  if (node$tag == "rf_call" && is.null(node$var_name)) {
    counter_env$n <- counter_env$n + 1L
    node$var_name <- sprintf(
      "rfcall_%s_%d_",
      tccq_cg_ident(node$fun),
      counter_env$n
    )
  }

  for (nm in names(node)) {
    child <- node[[nm]]
    if (is.list(child)) {
      node[[nm]] <- tccq_assign_rf_call_names(child, counter_env)
    }
  }
  node
}

tccq_collect_rf_calls <- function(node) {
  # Post-order traversal: children first, then self.
  # This ensures nested rf_calls are emitted before the outer ones.
  if (is.null(node) || is.null(node$tag)) {
    return(list())
  }
  out <- list()
  for (nm in names(node)) {
    child <- node[[nm]]
    if (is.list(child) && !is.null(child$tag)) {
      out <- c(out, tccq_collect_rf_calls(child))
    }
    if (is.list(child) && is.null(child$tag)) {
      for (item in child) {
        if (is.list(item) && !is.null(item$tag)) {
          out <- c(out, tccq_collect_rf_calls(item))
        }
      }
    }
  }
  if (node$tag == "rf_call") {
    out <- c(out, list(node))
  }
  out
}

# ---------------------------------------------------------------------------
# Pre-pass: assign var_name to all reduce_expr nodes in the IR tree.
# Since R lists are copy-on-modify, we rebuild the tree.
# ---------------------------------------------------------------------------

tccq_assign_reduce_expr_names <- function(node, counter_env = NULL) {
  if (is.null(counter_env)) {
    counter_env <- new.env(parent = emptyenv())
    counter_env$n <- 0L
  }
  if (is.null(node) || !is.list(node)) {
    return(node)
  }
  if (is.null(node$tag)) {
    # Plain list of nodes (e.g. stmts)
    return(lapply(node, tccq_assign_reduce_expr_names, counter_env))
  }

  if (node$tag == "reduce_expr" && is.null(node$var_name)) {
    counter_env$n <- counter_env$n + 1L
    node$var_name <- sprintf("redx_%s_%d", node$op, counter_env$n)
  }

  for (nm in names(node)) {
    child <- node[[nm]]
    if (is.list(child)) {
      node[[nm]] <- tccq_assign_reduce_expr_names(child, counter_env)
    }
  }
  node
}

tccq_assign_stat_expr_names <- function(node, counter_env = NULL) {
  if (is.null(counter_env)) {
    counter_env <- new.env(parent = emptyenv())
    counter_env$n <- 0L
  }
  if (is.null(node) || !is.list(node)) {
    return(node)
  }
  if (is.null(node$tag)) {
    return(lapply(node, tccq_assign_stat_expr_names, counter_env))
  }

  if (
    node$tag %in%
      c("mean_expr", "sd_expr", "median_expr", "quantile_expr") &&
      is.null(node$var_name)
  ) {
    counter_env$n <- counter_env$n + 1L
    prefix <- switch(
      node$tag,
      mean_expr = "mexpr",
      sd_expr = "sdexpr",
      median_expr = "medexpr",
      "qexpr"
    )
    node$var_name <- sprintf("%s_%d", prefix, counter_env$n)
  }

  for (nm in names(node)) {
    child <- node[[nm]]
    if (is.list(child)) {
      node[[nm]] <- tccq_assign_stat_expr_names(child, counter_env)
    }
  }
  node
}

# ---------------------------------------------------------------------------
# Top-level fn_body codegen -> complete C source
# ---------------------------------------------------------------------------

tcc_quick_codegen <- function(ir, decl, fn_name = "tcc_quick_entry") {
  if (!identical(ir$tag, "fn_body")) {
    stop("Codegen requires fn_body IR, got: ", ir$tag, call. = FALSE)
  }
  # Pre-pass: assign stable variable names
  ir <- tccq_assign_rf_call_names(ir)
  ir <- tccq_assign_reduce_expr_names(ir)
  ir <- tccq_assign_stat_expr_names(ir)
  needs_env <- isTRUE(tccq_ir_has_tag(ir, "rf_call"))
  tccq_cg_fn_body(ir, fn_name, needs_env = needs_env)
}

tccq_cg_fn_body <- function(ir, fn_name, needs_env = TRUE) {
  formal_names <- ir$formal_names
  c_arg_names <- if (isTRUE(needs_env)) {
    c(formal_names, "tccq_env")
  } else {
    formal_names
  }
  c_args <- paste(sprintf("SEXP %s", c_arg_names), collapse = ", ")

  # Determine needed headers by scanning IR for Rmath calls
  needs_rmath <- tccq_ir_needs_rmath(ir)

  lines <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <R_ext/Utils.h>",
    "#include <R_ext/BLAS.h>",
    "#include <R_ext/Lapack.h>",
    "#ifndef FCONE",
    "# define FCONE",
    "#endif",
    "#include <math.h>",
    if (needs_rmath) "#include <Rmath.h>",
    "",
    sprintf("SEXP %s(%s) {", fn_name, c_args),
    "  int nprotect_ = 0;"
  )

  # --- Parameter extraction ---
  mutated <- ir$mutated_params %||% character(0)
  for (nm in formal_names) {
    spec <- ir$params[[nm]]
    cnm <- tccq_cg_ident(nm)
    is_mutated <- nm %in% mutated
    if (isTRUE(spec$is_scalar)) {
      extract <- switch(
        spec$mode,
        double = sprintf("  double %s_ = Rf_asReal(%s);", cnm, nm),
        integer = sprintf("  int %s_ = Rf_asInteger(%s);", cnm, nm),
        logical = sprintf("  int %s_ = Rf_asLogical(%s);", cnm, nm),
        raw = sprintf(
          "  unsigned char %s_ = (unsigned char)Rf_asInteger(%s);",
          cnm,
          nm
        )
      )
      lines <- c(lines, extract)
    } else {
      sxp_type <- switch(
        spec$mode,
        double = "REALSXP",
        integer = "INTSXP",
        logical = "LGLSXP",
        raw = "RAWSXP",
        "REALSXP"
      )
      ptr_fun <- if (is_mutated) {
        switch(
          spec$mode,
          double = "REAL",
          integer = "INTEGER",
          logical = "LOGICAL",
          raw = "RAW",
          "REAL"
        )
      } else {
        switch(
          spec$mode,
          double = "REAL_RO",
          integer = "INTEGER_RO",
          logical = "LOGICAL_RO",
          raw = "RAW",
          "REAL_RO"
        )
      }
      c_type <- switch(
        spec$mode,
        double = "double",
        integer = "int",
        logical = "int",
        raw = "Rbyte",
        "double"
      )
      sxp_tag <- switch(
        spec$mode,
        double = "REALSXP",
        integer = "INTSXP",
        logical = "LGLSXP",
        raw = "RAWSXP",
        "REALSXP"
      )
      const_q <- if (is_mutated) "" else "const "
      if (is_mutated) {
        # Duplicate before potential coercion to avoid mutating caller's object.
        # Use PROTECT_WITH_INDEX/REPROTECT so the protected slot can be updated
        # if coercion allocates a replacement object.
        lines <- c(
          lines,
          sprintf("  SEXP s_%s = R_NilValue;", cnm),
          sprintf("  PROTECT_INDEX ipx_%s;", cnm),
          sprintf(
            "  PROTECT_WITH_INDEX(s_%s = Rf_duplicate(%s), &ipx_%s);",
            cnm,
            nm,
            cnm
          ),
          "  nprotect_++;",
          sprintf(
            "  if (TYPEOF(s_%s) != %s) REPROTECT(s_%s = Rf_coerceVector(s_%s, %s), ipx_%s);",
            cnm,
            sxp_tag,
            cnm,
            cnm,
            sxp_type,
            cnm
          ),
          sprintf("  R_xlen_t n_%s = XLENGTH(s_%s);", cnm, cnm),
          sprintf("  %s *p_%s = %s(s_%s);", c_type, cnm, ptr_fun, cnm)
        )
      } else {
        # Reuse original argument buffer when already in target SEXPTYPE.
        # Only allocate/protect when coercion is required.
        lines <- c(
          lines,
          sprintf("  SEXP s_%s = %s;", cnm, nm),
          sprintf("  if (TYPEOF(s_%s) != %s) {", cnm, sxp_tag),
          sprintf("    PROTECT_INDEX ipx_%s;", cnm),
          sprintf("    PROTECT_WITH_INDEX(s_%s, &ipx_%s);", cnm, cnm),
          "    nprotect_++;",
          sprintf(
            "    REPROTECT(s_%s = Rf_coerceVector(s_%s, %s), ipx_%s);",
            cnm,
            cnm,
            sxp_type,
            cnm
          ),
          "  }",
          sprintf("  R_xlen_t n_%s = XLENGTH(s_%s);", cnm, cnm),
          sprintf(
            "  %s%s *p_%s = %s(s_%s);",
            const_q,
            c_type,
            cnm,
            ptr_fun,
            cnm
          )
        )
      }
      if (length(spec$dims) == 2L) {
        lines <- c(
          lines,
          sprintf("  int nrow_%s = Rf_nrows(s_%s);", cnm, cnm),
          sprintf("  int ncol_%s = Rf_ncols(s_%s);", cnm, cnm)
        )
      }
    }
  }

  # --- Local variable declarations ---
  for (nm in names(ir$locals)) {
    info <- ir$locals[[nm]]
    cnm <- tccq_cg_ident(nm)
    if (info$shape == "scalar") {
      c_type <- switch(
        info$mode,
        double = "double",
        integer = "int",
        logical = "int",
        raw = "Rbyte",
        "double"
      )
      lines <- c(lines, sprintf("  %s %s_ = 0;", c_type, cnm))
    } else {
      c_type <- switch(
        info$mode,
        double = "double",
        integer = "int",
        logical = "int",
        raw = "Rbyte",
        "double"
      )
      lines <- c(
        lines,
        sprintf("  SEXP s_%s = R_NilValue;", cnm),
        sprintf("  R_xlen_t n_%s = 0;", cnm),
        sprintf("  %s *p_%s = NULL;", c_type, cnm)
      )
      if (info$shape == "matrix") {
        lines <- c(
          lines,
          sprintf("  int nrow_%s = 0;", cnm),
          sprintf("  int ncol_%s = 0;", cnm)
        )
      }
    }
  }

  # --- Body statements ---
  for (s in ir$stmts) {
    lines <- c(lines, tccq_cg_stmt(s, 1))
  }

  # --- Emit rf_calls and reduction loops referenced in the return expression ---
  ret_rfs <- if (!is.null(ir$ret$tag) && ir$ret$tag == "rf_call") {
    list()
  } else {
    tccq_collect_rf_calls(ir$ret)
  }
  for (rf in ret_rfs) {
    lines <- c(lines, tccq_cg_rf_call_stmt(rf, 1)$lines)
  }
  ret_reds <- tccq_collect_reductions(ir$ret)
  for (rd in ret_reds) {
    if (rd$tag == "reduce") {
      lines <- c(lines, tccq_cg_reduce_stmt(rd, 1))
    }
    if (rd$tag == "which_reduce") {
      lines <- c(lines, tccq_cg_which_reduce_stmt(rd, 1))
    }
    if (rd$tag == "reduce_expr") {
      lines <- c(lines, tccq_cg_reduce_expr_stmt(rd, 1))
    }
    if (rd$tag == "mean_expr") {
      lines <- c(lines, tccq_cg_mean_expr_stmt(rd, 1))
    }
    if (rd$tag == "sd_expr") {
      lines <- c(lines, tccq_cg_sd_expr_stmt(rd, 1))
    }
    if (rd$tag == "median_expr") {
      lines <- c(lines, tccq_cg_median_expr_stmt(rd, 1))
    }
    if (rd$tag == "quantile_expr") {
      lines <- c(lines, tccq_cg_quantile_expr_stmt(rd, 1))
    }
  }

  # --- Return ---
  if (!is.null(ir$ret$tag) && ir$ret$tag == "rf_call") {
    # rf_call return: emit the call and return the SEXP
    rf_res <- tccq_cg_rf_call_stmt(ir$ret, 1)
    lines <- c(lines, rf_res$lines)
    rf_var <- rf_res$var
    lines <- c(
      lines,
      "  UNPROTECT(nprotect_);",
      sprintf("  return %s;", rf_var)
    )
  } else if (
    (ir$ret_shape %in% c("vector", "matrix")) &&
      !is.null(ir$ret$tag) &&
      ir$ret$tag == "var"
  ) {
    # Return the SEXP of a named variable
    ret_nm <- tccq_cg_ident(ir$ret$name)
    lines <- c(
      lines,
      "  UNPROTECT(nprotect_);",
      sprintf("  return s_%s;", ret_nm)
    )
  } else if (
    ir$ret_shape == "matrix" &&
      !is.null(ir$ret$tag) &&
      ir$ret$tag == "matmul"
  ) {
    lines <- c(
      lines,
      "  SEXP s_ret = R_NilValue;",
      "  R_xlen_t n_ret = 0;",
      "  double *p_ret = NULL;",
      "  int nrow_ret = 0;",
      "  int ncol_ret = 0;"
    )
    lines <- c(lines, tccq_cg_matmul_stmt(ir$ret, 1, "ret"))
    lines <- c(
      lines,
      "  UNPROTECT(nprotect_);",
      "  return s_ret;"
    )
  } else if (
    ir$ret_shape == "matrix" &&
      !is.null(ir$ret$tag) &&
      ir$ret$tag == "transpose"
  ) {
    c_type_ret <- switch(
      ir$ret_mode,
      integer = "int",
      logical = "int",
      raw = "Rbyte",
      "double"
    )
    lines <- c(
      lines,
      "  SEXP s_ret = R_NilValue;",
      "  R_xlen_t n_ret = 0;",
      "  int nrow_ret = 0;",
      "  int ncol_ret = 0;",
      paste0("  ", c_type_ret, " *p_ret = NULL;"),
      tccq_cg_transpose_stmt(ir$ret, 1, "ret", mode = ir$ret_mode),
      "  UNPROTECT(nprotect_);",
      "  return s_ret;"
    )
  } else if (
    ir$ret_shape == "vector" &&
      !is.null(ir$ret$tag) &&
      ir$ret$tag == "mat_reduce"
  ) {
    lines <- c(
      lines,
      "  SEXP s_ret = R_NilValue;",
      "  R_xlen_t n_ret = 0;",
      "  double *p_ret = NULL;",
      tccq_cg_mat_reduce_stmt(ir$ret, 1, "ret"),
      "  UNPROTECT(nprotect_);",
      "  return s_ret;"
    )
  } else if (
    ir$ret_shape %in%
      c("vector", "matrix") &&
      !is.null(ir$ret$tag) &&
      ir$ret$tag == "solve_lin"
  ) {
    lines <- c(
      lines,
      "  SEXP s_ret = R_NilValue;",
      "  R_xlen_t n_ret = 0;",
      "  double *p_ret = NULL;"
    )
    if (ir$ret_shape == "matrix") {
      lines <- c(
        lines,
        "  int nrow_ret = 0;",
        "  int ncol_ret = 0;"
      )
    }
    lines <- c(lines, tccq_cg_solve_lin_stmt(ir$ret, 1, "ret"))
    lines <- c(
      lines,
      "  UNPROTECT(nprotect_);",
      "  return s_ret;"
    )
  } else if (
    ir$ret_shape == "vector" &&
      !is.null(ir$ret$tag) &&
      ir$ret$tag == "quantile_vec_expr"
  ) {
    lines <- c(
      lines,
      "  SEXP s_ret = R_NilValue;",
      "  R_xlen_t n_ret = 0;",
      "  double *p_ret = NULL;"
    )
    lines <- c(lines, tccq_cg_quantile_vec_stmt(ir$ret, 1, "ret"))
    lines <- c(
      lines,
      "  UNPROTECT(nprotect_);",
      "  return s_ret;"
    )
  } else if (ir$ret_shape %in% c("vector", "matrix")) {
    # Materialize a vector expression element-wise
    lines <- c(lines, tccq_cg_vec_expr_return(ir, 1))
  } else {
    # Scalar return
    ret_expr <- tccq_cg_expr(ir$ret)
    wrap <- switch(
      ir$ret_mode,
      double = sprintf("Rf_ScalarReal((double)(%s))", ret_expr),
      integer = sprintf("Rf_ScalarInteger((int)(%s))", ret_expr),
      logical = sprintf("Rf_ScalarLogical((%s) ? 1 : 0)", ret_expr),
      raw = sprintf("Rf_ScalarRaw((Rbyte)(%s))", ret_expr),
      ret_expr
    )
    lines <- c(
      lines,
      "  UNPROTECT(nprotect_);",
      sprintf("  return %s;", wrap)
    )
  }

  lines <- c(lines, "}")
  paste(lines, collapse = "\n")
}
