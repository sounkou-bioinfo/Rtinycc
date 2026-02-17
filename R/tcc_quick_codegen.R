# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Unified codegen for tcc_quick.
#
# Takes fn_body IR and produces a complete SEXP-returning C function.

# ---------------------------------------------------------------------------
# Expression codegen
# ---------------------------------------------------------------------------

tccq_cg_ident <- function(x) {
  y <- gsub("[^A-Za-z0-9_]", "_", x)
  if (grepl("^[0-9]", y)) y <- paste0("v_", y)
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
    if (op == "&") op <- "&&"
    if (op == "|") op <- "||"
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
    fun <- switch(node$fun,
      abs = "fabs",
      ceiling = "ceil",
      node$fun
    )
    return(sprintf("(%s((double)(%s)))", fun, x))
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

  if (tag == "mat_get") {
    arr <- tccq_cg_ident(node$arr)
    row <- tccq_cg_expr(node$row)
    col <- tccq_cg_expr(node$col)
    return(sprintf(
      "p_%s[(R_xlen_t)((%s) - 1) + (R_xlen_t)((%s) - 1) * nrow_%s]",
      arr, row, col, arr
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
      gsub("\\.", "_", node$op), tccq_cg_ident(node$arr)
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

  stop(paste0("tccq_cg_expr: unsupported tag '", tag, "'"), call. = FALSE)
}

# ---------------------------------------------------------------------------
# Statement codegen
# ---------------------------------------------------------------------------

tccq_cg_stmt <- function(node, indent) {
  tag <- node$tag
  pad <- strrep("  ", indent)

  if (tag == "block") {
    lines <- vapply(node$stmts, function(s) tccq_cg_stmt(s, indent), character(1))
    return(paste(lines, collapse = "\n"))
  }

  if (tag == "assign") {
    nm <- tccq_cg_ident(node$name)

    # --- vector allocation ---
    if (identical(node$shape, "vector") &&
      identical(node$expr$tag, "vec_alloc")) {
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
        pad, "s_", nm, " = PROTECT(Rf_allocVector(", sxp_type,
        ", (R_xlen_t)(", len, ")));\n",
        pad, "n_", nm, " = XLENGTH(s_", nm, ");\n",
        pad, "p_", nm, " = ", ptr_fun, "(s_", nm, ");\n",
        pad, "nprotect_++;\n",
        pad, "for (R_xlen_t zz_ = 0; zz_ < n_", nm,
        "; ++zz_) p_", nm, "[zz_] = 0;"
      ))
    }

    # --- matrix allocation ---
    if (identical(node$shape, "matrix") &&
      identical(node$expr$tag, "mat_alloc")) {
      nr <- tccq_cg_expr(node$expr$nrow)
      nc <- tccq_cg_expr(node$expr$ncol)
      fill <- format(node$expr$fill, scientific = FALSE)
      return(paste0(
        pad, "s_", nm, " = PROTECT(Rf_allocMatrix(REALSXP, (int)(",
        nr, "), (int)(", nc, ")));\n",
        pad, "nrow_", nm, " = (int)(", nr, ");\n",
        pad, "ncol_", nm, " = (int)(", nc, ");\n",
        pad, "n_", nm, " = (R_xlen_t)nrow_", nm,
        " * (R_xlen_t)ncol_", nm, ";\n",
        pad, "p_", nm, " = REAL(s_", nm, ");\n",
        pad, "nprotect_++;\n",
        pad, "for (R_xlen_t zz_ = 0; zz_ < n_", nm,
        "; ++zz_) p_", nm, "[zz_] = ", fill, ";"
      ))
    }

    # --- scalar assignment ---
    expr <- tccq_cg_expr(node$expr)
    return(paste0(pad, nm, "_ = ", expr, ";"))
  }

  if (tag == "vec_set") {
    arr <- tccq_cg_ident(node$arr)
    idx <- tccq_cg_expr(node$idx)
    val <- tccq_cg_expr(node$value)
    return(paste0(
      pad, "p_", arr, "[(R_xlen_t)((", idx, ") - 1)] = ", val, ";"
    ))
  }

  if (tag == "mat_set") {
    arr <- tccq_cg_ident(node$arr)
    row <- tccq_cg_expr(node$row)
    col <- tccq_cg_expr(node$col)
    val <- tccq_cg_expr(node$value)
    return(paste0(
      pad, "p_", arr, "[(R_xlen_t)((", row, ") - 1) + (R_xlen_t)((",
      col, ") - 1) * nrow_", arr, "] = ", val, ";"
    ))
  }

  if (tag == "for") {
    var <- tccq_cg_ident(node$var)
    iter <- node$iter
    body_c <- tccq_cg_stmt(node$body, indent + 1)

    if (iter$tag == "seq_along") {
      lim <- sprintf("n_%s", tccq_cg_ident(iter$target))
      return(paste0(
        pad, "for (R_xlen_t ", var, "_ = 1; ", var, "_ <= ",
        lim, "; ++", var, "_) {\n",
        body_c, "\n", pad, "}"
      ))
    }
    if (iter$tag == "seq_len") {
      lim <- tccq_cg_expr(iter$n)
      return(paste0(
        pad, "for (R_xlen_t ", var, "_ = 1; ", var,
        "_ <= (R_xlen_t)(", lim, "); ++", var, "_) {\n",
        body_c, "\n", pad, "}"
      ))
    }
    if (iter$tag == "seq_range") {
      from <- tccq_cg_expr(iter$from)
      to <- tccq_cg_expr(iter$to)
      return(paste0(
        pad, "for (R_xlen_t ", var, "_ = (R_xlen_t)(", from, "); ",
        var, "_ <= (R_xlen_t)(", to, "); ++", var, "_) {\n",
        body_c, "\n", pad, "}"
      ))
    }
    stop("Unsupported for-loop iterator tag", call. = FALSE)
  }

  if (tag == "while") {
    cond <- tccq_cg_expr(node$cond)
    body_c <- tccq_cg_stmt(node$body, indent + 1)
    return(paste0(
      pad, "while (", cond, ") {\n", body_c, "\n", pad, "}"
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
      pad, "if (", cond, ") {\n", yes, "\n",
      pad, "} else {\n", no, "\n", pad, "}"
    ))
  }

  if (tag == "reduce") {
    return(tccq_cg_reduce_stmt(node, indent))
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

  init <- switch(op,
    sum = "0.0",
    prod = "1.0",
    max = sprintf("p_%s[0]", arr),
    min = sprintf("p_%s[0]", arr),
    "0.0"
  )
  update <- switch(op,
    sum = sprintf("%s += p_%s[ri_];", var, arr),
    prod = sprintf("%s *= p_%s[ri_];", var, arr),
    max = sprintf("if (p_%s[ri_] > %s) %s = p_%s[ri_];", arr, var, var, arr),
    min = sprintf("if (p_%s[ri_] < %s) %s = p_%s[ri_];", arr, var, var, arr),
    ""
  )
  start <- if (op %in% c("max", "min")) "1" else "0"
  paste0(
    pad, "double ", var, " = ", init, ";\n",
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
    pad, "int ", var, " = 1;\n",
    pad, "double ", var, "_best = p_", arr, "[0];\n",
    pad, "for (R_xlen_t ri_ = 1; ri_ < n_", arr, "; ++ri_) {\n",
    pad, "  if (p_", arr, "[ri_] ", cmp, " ", var, "_best) {\n",
    pad, "    ", var, "_best = p_", arr, "[ri_];\n",
    pad, "    ", var, " = (int)(ri_ + 1);\n",
    pad, "  }\n",
    pad, "}"
  )
}

# ---------------------------------------------------------------------------
# Collect all reduce/which_reduce nodes from an IR tree
# ---------------------------------------------------------------------------

tccq_collect_reductions <- function(node) {
  if (is.null(node) || is.null(node$tag)) {
    return(list())
  }

  out <- list()
  if (node$tag %in% c("reduce", "which_reduce")) out <- list(node)

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
    pad, "R_xlen_t ret_len_ = (R_xlen_t)(", len_c, ");\n",
    pad, "SEXP ret_ = PROTECT(Rf_allocVector(", sxp_type, ", ret_len_));\n",
    pad, "nprotect_++;\n",
    pad, c_type, " *p_ret_ = ", ptr_fun, "(ret_);\n",
    pad, "for (R_xlen_t ei_ = 0; ei_ < ret_len_; ++ei_) {\n",
    pad, "  p_ret_[ei_] = (", c_type, ")(", elem_expr, ");\n",
    pad, "}\n",
    pad, "UNPROTECT(nprotect_);\n",
    pad, "return ret_;"
  )
}

# Infer C length expression for a vector IR node
tccq_cg_vec_len <- function(node) {
  tag <- node$tag
  if (tag == "var") {
    return(sprintf("n_%s", tccq_cg_ident(node$name)))
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
  if (tag == "if") {
    return(tccq_cg_vec_len(node$yes) %||% tccq_cg_vec_len(node$no))
  }
  NULL
}

# Generate C for the i-th element (0-based) of a vector expression
tccq_cg_vec_elem <- function(node, idx_var) {
  tag <- node$tag

  if (tag == "var") {
    return(sprintf("p_%s[%s]", tccq_cg_ident(node$name), idx_var))
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
    if (op == "&") op <- "&&"
    if (op == "|") op <- "||"
    return(sprintf("((%s) %s (%s))", lhs, op, rhs))
  }

  if (tag == "call1") {
    x <- tccq_cg_vec_elem(node$x, idx_var)
    fun <- switch(node$fun,
      abs = "fabs",
      ceiling = "ceil",
      node$fun
    )
    return(sprintf("(%s((double)(%s)))", fun, x))
  }

  if (tag == "if") {
    cond <- tccq_cg_vec_elem(node$cond, idx_var)
    yes <- tccq_cg_vec_elem(node$yes, idx_var)
    no <- tccq_cg_vec_elem(node$no, idx_var)
    return(sprintf("((%s) ? (%s) : (%s))", cond, yes, no))
  }

  # Scalar expression fallback (constants broadcast)
  tccq_cg_expr(node)
}

# ---------------------------------------------------------------------------
# Top-level fn_body codegen → complete C source
# ---------------------------------------------------------------------------

tcc_quick_codegen <- function(ir, decl, fn_name = "tcc_quick_entry") {
  if (!identical(ir$tag, "fn_body")) {
    stop("Codegen requires fn_body IR, got: ", ir$tag, call. = FALSE)
  }
  tccq_cg_fn_body(ir, fn_name)
}

tccq_cg_fn_body <- function(ir, fn_name) {
  formal_names <- ir$formal_names
  c_args <- paste(sprintf("SEXP %s", formal_names), collapse = ", ")

  lines <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <math.h>",
    "",
    sprintf("SEXP %s(%s) {", fn_name, c_args),
    "  int nprotect_ = 0;"
  )

  # --- Parameter extraction ---
  for (nm in formal_names) {
    spec <- ir$params[[nm]]
    cnm <- tccq_cg_ident(nm)
    if (isTRUE(spec$is_scalar)) {
      extract <- switch(spec$mode,
        double  = sprintf("  double %s_ = Rf_asReal(%s);", cnm, nm),
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
      ptr_fun <- switch(spec$mode,
        double = "REAL",
        integer = "INTEGER",
        logical = "LOGICAL",
        "REAL"
      )
      c_type <- switch(spec$mode,
        double = "double",
        integer = "int",
        logical = "int",
        "double"
      )
      lines <- c(
        lines,
        sprintf(
          "  SEXP s_%s = PROTECT(Rf_coerceVector(%s, %s));",
          cnm, nm, sxp_type
        ),
        "  nprotect_++;",
        sprintf("  R_xlen_t n_%s = XLENGTH(s_%s);", cnm, cnm),
        sprintf("  %s *p_%s = %s(s_%s);", c_type, cnm, ptr_fun, cnm)
      )
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

  # --- Emit reduction loops referenced in the return expression ---
  ret_reds <- tccq_collect_reductions(ir$ret)
  for (rd in ret_reds) {
    if (rd$tag == "reduce") lines <- c(lines, tccq_cg_reduce_stmt(rd, 1))
    if (rd$tag == "which_reduce") {
      lines <- c(lines, tccq_cg_which_reduce_stmt(rd, 1))
    }
  }

  # --- Return ---
  ret_expr <- tccq_cg_expr(ir$ret)
  if ((ir$ret_shape %in% c("vector", "matrix")) &&
    !is.null(ir$ret$tag) && ir$ret$tag == "var") {
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
    wrap <- switch(ir$ret_mode,
      double  = sprintf("Rf_ScalarReal((double)(%s))", ret_expr),
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
