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
      "x", "y", "lhs", "rhs", "cond", "yes", "no",
      "expr", "body", "stmts", "ret", "len_expr",
      "idx", "value", "row", "col", "from", "to",
      "nrow", "ncol", "iter", "n", "args", "mask"
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
    return(x)
  }

  if (tag == "rf_call") {
    # rf_call as sub-expression: the statement was hoisted before this
    # expression is evaluated.  Return the pre-assigned variable name.
    if (is.null(node$var_name)) {
      stop("rf_call missing var_name — pre-pass not run?", call. = FALSE)
    }
    # For scalar extraction: Rf_asReal / Rf_asInteger / Rf_asLogical
    mode <- node$mode %||% "double"
    extract <- switch(mode,
      integer = sprintf("Rf_asInteger(%s)", node$var_name),
      logical = sprintf("Rf_asLogical(%s)", node$var_name),
      sprintf("Rf_asReal(%s)", node$var_name)
    )
    return(extract)
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
      sxp_type <- switch(alloc_mode,
        double = "REALSXP",
        integer = "INTSXP",
        logical = "LGLSXP",
        "REALSXP"
      )
      len <- tccq_cg_expr(node$expr$len_expr)
      ptr_fun <- switch(alloc_mode,
        double = "REAL",
        integer = "INTEGER",
        logical = "LOGICAL",
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

    # --- scalar assignment ---
    # First handle cumulative ops which need alloc + fill loop
    if (identical(node$expr$tag, "cumulative") && identical(node$shape, "vector")) {
      len_c <- tccq_cg_vec_len(node$expr$expr)
      sxp_type <- switch(node$mode,
        integer = "INTSXP",
        logical = "LGLSXP",
        "REALSXP"
      )
      ptr_fun <- switch(node$mode,
        integer = "INTEGER",
        logical = "LOGICAL",
        "REAL"
      )
      alloc_lines <- paste0(
        pad, "s_", nm, " = PROTECT(Rf_allocVector(", sxp_type, ", (R_xlen_t)(", len_c, ")));\n",
        pad, "n_", nm, " = XLENGTH(s_", nm, ");\n",
        pad, "p_", nm, " = ", ptr_fun, "(s_", nm, ");\n",
        pad, "nprotect_++;"
      )
      cum_lines <- tccq_cg_cumulative_stmt(
        node$expr, indent, paste0("p_", nm)
      )
      return(paste(c(alloc_lines, cum_lines), collapse = "\n"))
    }

    # --- rf_call assignment: y <- fun(x, ...) via Rf_eval ---
    if (identical(node$expr$tag, "rf_call")) {
      rf_res <- tccq_cg_rf_call_stmt(node$expr, indent)
      rf_lines <- rf_res$lines
      rf_var <- rf_res$var
      if (identical(node$shape, "vector") || identical(node$shape, "matrix")) {
        sxp_type <- switch(node$mode,
          integer = "INTSXP",
          logical = "LGLSXP",
          "REALSXP"
        )
        ptr_fun <- switch(node$mode,
          integer = "INTEGER",
          logical = "LOGICAL",
          "REAL"
        )
        return(paste0(
          rf_lines, "\n",
          pad, "s_", nm, " = PROTECT(Rf_coerceVector(", rf_var, ", ", sxp_type, "));\n",
          pad, "nprotect_++;\n",
          pad, "n_", nm, " = XLENGTH(s_", nm, ");\n",
          pad, "p_", nm, " = ", ptr_fun, "(s_", nm, ");"
        ))
      } else {
        extract <- switch(node$mode,
          integer = sprintf("Rf_asInteger(%s)", rf_var),
          logical = sprintf("Rf_asLogical(%s)", rf_var),
          sprintf("Rf_asReal(%s)", rf_var)
        )
        return(paste0(
          rf_lines, "\n",
          pad, nm, "_ = ", extract, ";"
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
    }

    # --- Vector condensation: alloc + element-wise loop ---
    if (identical(node$shape, "vector")) {
      len_c <- tccq_cg_vec_len(node$expr)
      if (is.null(len_c)) {
        stop(
          "Cannot determine length for vector assign to '", node$name, "'",
          call. = FALSE
        )
      }
      elem_expr <- tccq_cg_vec_elem(node$expr, "zz_")
      sxp_type <- switch(node$mode,
        integer = "INTSXP",
        logical = "LGLSXP",
        "REALSXP"
      )
      ptr_fun <- switch(node$mode,
        integer = "INTEGER",
        logical = "LOGICAL",
        "REAL"
      )
      parts <- c(parts, paste0(
        pad, "s_", nm, " = PROTECT(Rf_allocVector(", sxp_type,
        ", (R_xlen_t)(", len_c, ")));\n",
        pad, "n_", nm, " = XLENGTH(s_", nm, ");\n",
        pad, "p_", nm, " = ", ptr_fun, "(s_", nm, ");\n",
        pad, "nprotect_++;\n",
        pad, "for (R_xlen_t zz_ = 0; zz_ < n_", nm, "; ++zz_) ",
        "p_", nm, "[zz_] = ", elem_expr, ";"
      ))
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
        "if ((R_xlen_t)(", from, ") <= (R_xlen_t)(", to, ")) {\n",
        pad,
        "  for (R_xlen_t ", v, " = (R_xlen_t)(", from, "); ",
        v, " <= (R_xlen_t)(", to, "); ++", v, ") {\n",
        body_c, "\n",
        pad,
        "  }\n",
        pad,
        "} else {\n",
        pad,
        "  for (R_xlen_t ", v, " = (R_xlen_t)(", from, "); ",
        v, " >= (R_xlen_t)(", to, "); --", v, ") {\n",
        body_c, "\n",
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
      # by can be positive or negative — branch at runtime
      return(paste0(
        pad,
        "{\n",
        pad,
        "  double seq_by_val_ = (double)(", by, ");\n",
        pad,
        "  if (seq_by_val_ > 0) {\n",
        pad,
        "    for (double ", v, " = (double)(", from, "); ",
        v, " <= (double)(", to, "); ", v, " += seq_by_val_) {\n",
        body_c, "\n",
        pad,
        "    }\n",
        pad,
        "  } else if (seq_by_val_ < 0) {\n",
        pad,
        "    for (double ", v, " = (double)(", from, "); ",
        v, " >= (double)(", to, "); ", v, " += seq_by_val_) {\n",
        body_c, "\n",
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
    stop("cumulative must appear as right-hand side of an assignment", call. = FALSE)
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

  c_type <- if (op %in% c("any", "all")) "int" else "double"
  init <- switch(op,
    sum = "0.0",
    prod = "1.0",
    max = sprintf("p_%s[0]", arr),
    min = sprintf("p_%s[0]", arr),
    any = "0",
    all = "1",
    "0.0"
  )
  update <- switch(op,
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
    pad, c_type, " ", var, " = ", init, ";\n",
    pad, "for (R_xlen_t ri_ = ", start, "; ri_ < n_", arr, "; ++ri_) {\n",
    pad, "  ", update, "\n",
    pad, "}"
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

tccq_rfcall_next_id <- function() {
  tccq_rfcall_counter_$n <- tccq_rfcall_counter_$n + 1L
  tccq_rfcall_counter_$n
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

  c_type <- if (op %in% c("any", "all")) "int" else "double"
  init <- switch(op,
    sum = "0.0",
    prod = "1.0",
    max = paste0("(", tccq_cg_vec_elem(node$expr, "0"), ")"),
    min = paste0("(", tccq_cg_vec_elem(node$expr, "0"), ")"),
    any = "0",
    all = "1",
    "0.0"
  )
  update <- switch(op,
    sum = sprintf("%s += %s;", var, elem),
    prod = sprintf("%s *= %s;", var, elem),
    max = sprintf("{ double v_ = %s; if (v_ > %s) %s = v_; }", elem, var, var),
    min = sprintf("{ double v_ = %s; if (v_ < %s) %s = v_; }", elem, var, var),
    any = sprintf("if (%s) { %s = 1; break; }", elem, var),
    all = sprintf("if (!(%s)) { %s = 0; break; }", elem, var),
    ""
  )
  start <- if (op %in% c("max", "min")) "1" else "0"
  paste0(
    pad, c_type, " ", var, " = ", init, ";\n",
    pad, "for (R_xlen_t ", idx, " = ", start, "; ",
    idx, " < (R_xlen_t)(", len_c, "); ++", idx, ") {\n",
    pad, "  ", update, "\n",
    pad, "}"
  )
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

  update <- switch(op,
    cumsum = sprintf("%s[%s] = %s[%s - 1] + %s;", target_ptr, idx, target_ptr, idx, elem_i),
    cumprod = sprintf("%s[%s] = %s[%s - 1] * %s;", target_ptr, idx, target_ptr, idx, elem_i),
    cummax = sprintf(
      "{ double v_ = %s; %s[%s] = v_ > %s[%s - 1] ? v_ : %s[%s - 1]; }",
      elem_i, target_ptr, idx, target_ptr, idx, target_ptr, idx
    ),
    cummin = sprintf(
      "{ double v_ = %s; %s[%s] = v_ < %s[%s - 1] ? v_ : %s[%s - 1]; }",
      elem_i, target_ptr, idx, target_ptr, idx, target_ptr, idx
    ),
    ""
  )
  paste0(
    pad, target_ptr, "[0] = ", elem0, ";\n",
    pad, "for (R_xlen_t ", idx, " = 1; ", idx, " < (R_xlen_t)(", len_c, "); ++", idx, ") {\n",
    pad, "  ", update, "\n",
    pad, "}"
  )
}

# ---------------------------------------------------------------------------
# rf_call: emit Rf_lang + Rf_eval to call R function from generated C
# ---------------------------------------------------------------------------

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

  # Build the argument SEXPs — scalars need wrapping, vectors use s_name.
  # Nested rf_calls are recursively emitted first.
  arg_c <- character(n)
  for (i in seq_len(n)) {
    a <- args[[i]]
    a_shape <- a$shape %||% "scalar"
    a_tag <- a$tag %||% ""

    if (a_tag == "rf_call") {
      # Nested rf_call — emit it first, use its result SEXP
      child <- tccq_cg_rf_call_stmt(a, indent)
      lines <- c(lines, child$lines)
      arg_c[i] <- child$var
    } else if (a_tag == "var" && a_shape %in% c("vector", "matrix")) {
      # Vector / matrix variable — pass the SEXP directly
      arg_c[i] <- sprintf("s_%s", tccq_cg_ident(a$name))
    } else if (a_tag == "var" && a_shape == "scalar") {
      # Scalar variable — re-wrap from C scalar to SEXP
      c_nm <- tccq_cg_ident(a$name)
      a_mode <- a$mode %||% "double"
      wrap <- switch(a_mode,
        integer = sprintf("Rf_ScalarInteger(%s_)", c_nm),
        logical = sprintf("Rf_ScalarLogical(%s_)", c_nm),
        sprintf("Rf_ScalarReal(%s_)", c_nm)
      )
      tmp <- sprintf("rfarg_%s_%d_%d_", tccq_cg_ident(fun), uid, i)
      lines <- c(lines, paste0(pad, "SEXP ", tmp, " = PROTECT(", wrap, ");"))
      lines <- c(lines, paste0(pad, "nprotect_++;"))
      arg_c[i] <- tmp
    } else {
      # Scalar expression — wrap as SEXP
      val <- tccq_cg_expr(a)
      mode <- a$mode %||% "double"
      wrap <- switch(mode,
        integer = sprintf("Rf_ScalarInteger((int)(%s))", val),
        logical = sprintf("Rf_ScalarLogical((%s) ? 1 : 0)", val),
        sprintf("Rf_ScalarReal((double)(%s))", val)
      )
      tmp <- sprintf("rfarg_%s_%d_%d_", tccq_cg_ident(fun), uid, i)
      lines <- c(lines, paste0(pad, "SEXP ", tmp, " = PROTECT(", wrap, ");"))
      lines <- c(lines, paste0(pad, "nprotect_++;"))
      arg_c[i] <- tmp
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
        pad, "SEXP ", var, " = PROTECT(Rf_eval(PROTECT(",
        sprintf("Rf_lang%d(%s)", lang_n, lang_args),
        "), R_GlobalEnv));"
      ),
      paste0(pad, "nprotect_ += 2;")
    )
  } else {
    # Fallback: build LCONS manually for high-arity calls
    lines <- c(
      lines,
      paste0(pad, "SEXP rfcall_e_ = Rf_allocList(", lang_n, ");"),
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
        pad, "SEXP ", var, " = PROTECT(Rf_eval(rfcall_e_, R_GlobalEnv));"
      ),
      paste0(pad, "nprotect_++;")
    )
  }

  list(lines = paste(lines, collapse = "\n"), var = var)
} # ---------------------------------------------------------------------------
# vec_mask: x[mask] — count TRUEs, allocate, fill
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

  sxp_type <- switch(node$mode,
    integer = "INTSXP",
    logical = "LGLSXP",
    "REALSXP"
  )
  ptr_fun <- switch(node$mode,
    integer = "INTEGER",
    logical = "LOGICAL",
    "REAL"
  )

  paste0(
    pad, "// vec_mask: ", nm, " = ", arr, "[", mask, "]\n",
    pad, "R_xlen_t vm_cnt_ = 0;\n",
    pad, "for (R_xlen_t vm_ = 0; vm_ < n_", mask, "; ++vm_) {\n",
    pad, "  if (p_", mask, "[vm_]) vm_cnt_++;\n",
    pad, "}\n",
    pad, "s_", nm, " = PROTECT(Rf_allocVector(", sxp_type, ", vm_cnt_));\n",
    pad, "nprotect_++;\n",
    pad, "n_", nm, " = vm_cnt_;\n",
    pad, "p_", nm, " = ", ptr_fun, "(s_", nm, ");\n",
    pad, "{\n",
    pad, "  R_xlen_t vm_j_ = 0;\n",
    pad, "  for (R_xlen_t vm_ = 0; vm_ < n_", mask, "; ++vm_) {\n",
    pad, "    if (p_", mask, "[vm_]) {\n",
    pad, "      p_", nm, "[vm_j_++] = p_", arr, "[vm_];\n",
    pad, "    }\n",
    pad, "  }\n",
    pad, "}"
  )
}

tccq_collect_reductions <- function(node) {
  if (is.null(node) || is.null(node$tag)) {
    return(list())
  }

  out <- list()
  if (node$tag %in% c("reduce", "which_reduce", "reduce_expr")) {
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
    sxp_type <- switch(ir$ret_mode,
      integer = "INTSXP",
      logical = "LGLSXP",
      "REALSXP"
    )
    ptr_fun <- switch(ir$ret_mode,
      integer = "INTEGER",
      logical = "LOGICAL",
      "REAL"
    )
    alloc_lines <- paste0(
      pad, "R_xlen_t ret_len_ = (R_xlen_t)(", len_c, ");\n",
      pad, "SEXP ret_ = PROTECT(Rf_allocVector(", sxp_type, ", ret_len_));\n",
      pad, "nprotect_++;\n",
      pad, switch(ir$ret_mode,
        integer = "int",
        logical = "int",
        "double"
      ),
      " *p_ret_ = ", ptr_fun, "(ret_);"
    )
    cum_lines <- tccq_cg_cumulative_stmt(ir$ret, indent, "p_ret_")
    return(paste0(
      alloc_lines, "\n", cum_lines, "\n",
      pad, "UNPROTECT(nprotect_);\n",
      pad, "return ret_;"
    ))
  }

  len_c <- tccq_cg_vec_len(ir$ret)
  if (is.null(len_c)) {
    stop("Cannot determine length for vector return expression", call. = FALSE)
  }
  elem_expr <- tccq_cg_vec_elem(ir$ret, "ei_")

  sxp_type <- switch(ir$ret_mode,
    double = "REALSXP",
    integer = "INTSXP",
    logical = "LGLSXP",
    "REALSXP"
  )
  ptr_fun <- switch(ir$ret_mode,
    double = "REAL",
    integer = "INTEGER",
    logical = "LOGICAL",
    "REAL"
  )
  c_type <- switch(ir$ret_mode,
    double = "double",
    integer = "int",
    logical = "int",
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
  }

  if (tag == "unary") {
    x <- tccq_cg_vec_elem(node$x, idx_var)
    if (node$op == "!") {
      return(sprintf("(!(%s))", x))
    }
    return(sprintf("(%s(%s))", node$op, x))
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
    # Hoisted rf_call result — access element via REAL(var_name)[idx]
    if (!is.null(node$var_name)) {
      return(sprintf("REAL(%s)[%s]", node$var_name, idx_var))
    }
    stop("rf_call in vec_elem without var_name", call. = FALSE)
  }

  if (tag == "rev") {
    len <- tccq_cg_vec_len(node$expr)
    inner <- tccq_cg_vec_elem(node$expr, sprintf("((%s) - 1 - %s)", len, idx_var))
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
      "rfcall_%s_%d_", tccq_cg_ident(node$fun), counter_env$n
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

# ---------------------------------------------------------------------------
# Top-level fn_body codegen → complete C source
# ---------------------------------------------------------------------------

tcc_quick_codegen <- function(ir, decl, fn_name = "tcc_quick_entry") {
  if (!identical(ir$tag, "fn_body")) {
    stop("Codegen requires fn_body IR, got: ", ir$tag, call. = FALSE)
  }
  # Pre-pass: assign stable variable names
  ir <- tccq_assign_rf_call_names(ir)
  ir <- tccq_assign_reduce_expr_names(ir)
  tccq_cg_fn_body(ir, fn_name)
}

tccq_cg_fn_body <- function(ir, fn_name) {
  formal_names <- ir$formal_names
  c_args <- paste(sprintf("SEXP %s", formal_names), collapse = ", ")

  # Determine needed headers by scanning IR for Rmath calls
  needs_rmath <- tccq_ir_needs_rmath(ir)

  lines <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
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
      extract <- switch(spec$mode,
        double = sprintf("  double %s_ = Rf_asReal(%s);", cnm, nm),
        integer = sprintf("  int %s_ = Rf_asInteger(%s);", cnm, nm),
        logical = sprintf("  int %s_ = Rf_asLogical(%s);", cnm, nm)
      )
      lines <- c(lines, extract)
    } else {
      sxp_type <- switch(spec$mode,
        double = "REALSXP",
        integer = "INTSXP",
        logical = "LGLSXP",
        "REALSXP"
      )
      ptr_fun <- if (is_mutated) {
        switch(spec$mode,
          double = "REAL",
          integer = "INTEGER",
          logical = "LOGICAL",
          "REAL"
        )
      } else {
        switch(spec$mode,
          double = "REAL_RO",
          integer = "INTEGER_RO",
          logical = "LOGICAL_RO",
          "REAL_RO"
        )
      }
      c_type <- switch(spec$mode,
        double = "double",
        integer = "int",
        logical = "int",
        "double"
      )
      const_q <- if (is_mutated) "" else "const "
      if (is_mutated) {
        # Duplicate before coerce to avoid mutating caller's object
        lines <- c(
          lines,
          sprintf(
            "  SEXP s_%s = PROTECT(Rf_coerceVector(Rf_duplicate(%s), %s));",
            cnm, nm, sxp_type
          ),
          "  nprotect_++;",
          sprintf("  R_xlen_t n_%s = XLENGTH(s_%s);", cnm, cnm),
          sprintf("  %s *p_%s = %s(s_%s);", c_type, cnm, ptr_fun, cnm)
        )
      } else {
        lines <- c(
          lines,
          sprintf(
            "  SEXP s_%s = PROTECT(Rf_coerceVector(%s, %s));",
            cnm, nm, sxp_type
          ),
          "  nprotect_++;",
          sprintf("  R_xlen_t n_%s = XLENGTH(s_%s);", cnm, cnm),
          sprintf(
            "  %s%s *p_%s = %s(s_%s);",
            const_q, c_type, cnm, ptr_fun, cnm
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
      c_type <- switch(info$mode,
        double = "double",
        integer = "int",
        logical = "int",
        "double"
      )
      lines <- c(lines, sprintf("  %s %s_ = 0;", c_type, cnm))
    } else {
      c_type <- switch(info$mode,
        double = "double",
        integer = "int",
        logical = "int",
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
  ret_rfs <- tccq_collect_rf_calls(ir$ret)
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
  }

  # --- Return ---
  if (
    !is.null(ir$ret$tag) && ir$ret$tag == "rf_call"
  ) {
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
  } else if (ir$ret_shape %in% c("vector", "matrix")) {
    # Materialize a vector expression element-wise
    lines <- c(lines, tccq_cg_vec_expr_return(ir, 1))
  } else {
    # Scalar return
    ret_expr <- tccq_cg_expr(ir$ret)
    wrap <- switch(ir$ret_mode,
      double = sprintf("Rf_ScalarReal((double)(%s))", ret_expr),
      integer = sprintf("Rf_ScalarInteger((int)(%s))", ret_expr),
      logical = sprintf("Rf_ScalarLogical((%s) ? 1 : 0)", ret_expr),
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
