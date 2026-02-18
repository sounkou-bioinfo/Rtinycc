# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_quick_make_wrapper <- function(
  compiled_callable,
  formals_template,
  compiled_obj,
  fn
) {
  out <- function() NULL
  formals(out) <- formals_template

  arg_syms <- lapply(names(formals_template), as.name)
  call_expr <- as.call(c(
    as.name(".compiled_callable"),
    arg_syms,
    quote(environment())
  ))
  body(out) <- call_expr

  environment(out) <- list2env(
    list(
      .compiled_callable = compiled_callable,
      .compiled_obj = compiled_obj,
      .original_fn = fn
    ),
    parent = environment(fn)
  )

  out
}

tccq_ir_has_tag <- function(node, tag_name) {
  if (is.null(node) || !is.list(node)) {
    return(FALSE)
  }
  if (!is.null(node$tag) && identical(node$tag, tag_name)) {
    return(TRUE)
  }
  for (nm in names(node)) {
    child <- node[[nm]]
    if (is.list(child) && tccq_ir_has_tag(child, tag_name)) {
      return(TRUE)
    }
  }
  FALSE
}

tccq_validate_ir <- function(ir, fallback = c("auto", "soft", "hard")) {
  fallback <- match.arg(fallback)

  walk <- function(node) {
    if (is.null(node) || !is.list(node)) {
      return(invisible(NULL))
    }
    if (!is.null(node$tag)) {
      tag <- node$tag
      if (tag == "rf_call" && fallback == "hard") {
        stop(
          "hard fallback mode forbids Rf_eval paths (rf_call)",
          call. = FALSE
        )
      }
      if (tag %in% c("mean_expr", "sd_expr", "median_expr")) {
        if (is.null(node$expr)) {
          stop(tag, " missing expr", call. = FALSE)
        }
      }
      if (tag == "quantile_expr") {
        if (is.null(node$expr) || is.null(node$prob)) {
          stop("quantile_expr missing expr/prob", call. = FALSE)
        }
      }
      if (tag == "quantile_vec_expr") {
        if (is.null(node$expr) || is.null(node$probs)) {
          stop("quantile_vec_expr missing expr/probs", call. = FALSE)
        }
      }
      if (tag == "matmul") {
        if (is.null(node$a) || is.null(node$b)) {
          stop("matmul missing operands", call. = FALSE)
        }
      }
      if (tag == "solve_lin") {
        if (is.null(node$a) || is.null(node$b) || is.null(node$b_shape)) {
          stop("solve_lin missing operands", call. = FALSE)
        }
      }
      if (tag == "transpose") {
        if (is.null(node$a)) {
          stop("transpose missing operand", call. = FALSE)
        }
      }
      if (tag == "mat_reduce") {
        if (is.null(node$op) || is.null(node$arr)) {
          stop("mat_reduce missing fields", call. = FALSE)
        }
      }
    }

    for (nm in names(node)) {
      child <- node[[nm]]
      if (is.list(child)) {
        walk(child)
      }
    }
  }

  walk(ir)
  invisible(TRUE)
}

tcc_quick_compile <- function(fn, decl, ir, debug = FALSE) {
  entry <- "tcc_quick_entry"
  src <- tcc_quick_codegen(ir, decl, fn_name = entry)
  has_matmul <- isTRUE(tccq_ir_has_tag(ir, "matmul"))

  if (isTRUE(debug)) {
    message("tcc_quick generated C source:\n", src)
  }

  spec <- list(
    args = rep("sexp", length(decl$formal_names) + 1L),
    returns = "sexp"
  )
  names(spec$args) <- NULL

  ffi <- tcc_ffi() |>
    tcc_source(src)

  # Matrix products lower to BLAS dgemm. On Windows this symbol typically
  # lives in Rblas.dll (not R.dll), so add Rblas explicitly when needed.
  if (
    identical(.Platform$OS.type, "windows") &&
      has_matmul
  ) {
    ffi <- tcc_library(ffi, "Rblas")
  }

  if (isTRUE(debug)) {
    message(
      "tcc_quick compile diagnostics: ",
      "os=",
      .Platform$OS.type,
      ", has_matmul=",
      has_matmul,
      ", ffi_libraries=[",
      paste(ffi$libraries, collapse = ","),
      "]",
      ", ffi_lib_paths=[",
      paste(ffi$lib_paths, collapse = ","),
      "]"
    )
  }

  ffi <- do.call(tcc_bind, c(list(ffi), stats::setNames(list(spec), entry)))
  compiled <- tcc_compile(ffi)

  list(
    callable = compiled[[entry]],
    compiled = compiled
  )
}

#' Supported operations table for tcc_quick
#'
#' Returns a data.frame listing every R construct that `tcc_quick()` can
#' lower to C.  The table is assembled programmatically from the call
#' registry and from the lowerer/codegen, so it stays in sync with
#' the implementation.  The `vectorized` column indicates whether the
#' operation is fused element-wise into vector loops via condensation
#' (no intermediate allocation).
#'
#' @return A data.frame with columns `category`, `r`, `c`, `vectorized`.
#' @export
tcc_quick_ops <- function() {
  # --- Registry-driven math (call1 / call2) ---
  reg <- tcc_ir_c_api_registry()
  math_rows <- lapply(names(reg), function(nm) {
    e <- reg[[nm]]
    hdr <- if (!is.null(e$header)) e$header else "math.h"
    c_repr <- e$c_fun
    if (!is.null(e$transform) && e$transform == "x+1") {
      c_repr <- paste0(e$c_fun, "(x+1)")
    }
    if (e$arity == 2L) {
      c_repr <- paste0(c_repr, "(x, y)")
    } else {
      c_repr <- paste0(c_repr, "(x)")
    }
    data.frame(
      category = paste0("math (", hdr, ")"),
      r = nm,
      c = c_repr,
      vectorized = TRUE,
      stringsAsFactors = FALSE
    )
  })
  math_df <- do.call(rbind, math_rows)

  # --- Arithmetic / comparison / logical operators ---
  op_rows <- list(
    data.frame(
      category = "arithmetic",
      r = "+",
      c = "+",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "arithmetic",
      r = "-",
      c = "-",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "arithmetic",
      r = "*",
      c = "*",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "arithmetic",
      r = "/",
      c = "/",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "arithmetic",
      r = "^",
      c = "pow(x, y)",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "arithmetic",
      r = "%%",
      c = "fmod(x, y)",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "arithmetic",
      r = "%/%",
      c = "floor(x / y)",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "comparison",
      r = "< <= > >= == !=",
      c = "< <= > >= == !=",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "logical",
      r = "& | && || !",
      c = "& | && || !",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    )
  )
  ops_df <- do.call(rbind, op_rows)

  # --- Reductions ---
  red_rows <- list(
    data.frame(
      category = "reduction",
      r = "sum(x)",
      c = "accumulate loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "prod(x)",
      c = "accumulate loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "min(x)",
      c = "accumulate loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "max(x)",
      c = "accumulate loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "any(x)",
      c = "short-circuit loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "all(x)",
      c = "short-circuit loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "mean(x)",
      c = "sum/len loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "sd(x)",
      c = "two-pass loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "median(x)",
      c = "sort + midpoint",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "quantile(x, p)",
      c = "sort + type7 (scalar p)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "which.min(x)",
      c = "argmin loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "reduction",
      r = "which.max(x)",
      c = "argmax loop",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    )
  )
  red_df <- do.call(rbind, red_rows)

  # --- Cumulative ---
  cum_rows <- list(
    data.frame(
      category = "cumulative",
      r = "cumsum(x)",
      c = "sequential scan",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "cumulative",
      r = "cumprod(x)",
      c = "sequential scan",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "cumulative",
      r = "cummax(x)",
      c = "sequential scan",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "cumulative",
      r = "cummin(x)",
      c = "sequential scan",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    )
  )
  cum_df <- do.call(rbind, cum_rows)

  # --- Element-wise binary (non-registry) ---
  ew_rows <- list(
    data.frame(
      category = "element-wise",
      r = "pmin(x, y)",
      c = "ternary (x < y ? x : y)",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "element-wise",
      r = "pmax(x, y)",
      c = "ternary (x > y ? x : y)",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "element-wise",
      r = "rev(x)",
      c = "reversed index",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    )
  )
  ew_df <- do.call(rbind, ew_rows)

  # --- Vector accessors ---
  vec_rows <- list(
    data.frame(
      category = "vector",
      r = "x[i]",
      c = "p_x[i-1]",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "vector",
      r = "x[i] <- v",
      c = "p_x[i-1] = v",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "vector",
      r = "x[a:b]",
      c = "view (pointer + offset)",
      vectorized = TRUE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "vector",
      r = "length(x)",
      c = "n_x",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "vector",
      r = "double(n)",
      c = "Rf_allocVector",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "vector",
      r = "integer(n)",
      c = "Rf_allocVector",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "vector",
      r = "logical(n)",
      c = "Rf_allocVector",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    )
  )
  vec_df <- do.call(rbind, vec_rows)

  # --- Matrix ---
  mat_rows <- list(
    data.frame(
      category = "matrix",
      r = "x[i, j]",
      c = "p_x[(j-1)*nrow + (i-1)]",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "matrix",
      r = "x[i, j] <- v",
      c = "p_x[(j-1)*nrow + (i-1)] = v",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "matrix",
      r = "nrow(x)",
      c = "nrow_x",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "matrix",
      r = "ncol(x)",
      c = "ncol_x",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "matrix",
      r = "matrix(fill, nr, nc)",
      c = "Rf_allocMatrix",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "matrix",
      r = "A %*% B",
      c = "BLAS dgemm",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "matrix",
      r = "crossprod(A, B)",
      c = "BLAS dgemm (A^T B)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "matrix",
      r = "tcrossprod(A, B)",
      c = "BLAS dgemm (A B^T)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    )
  )
  mat_df <- do.call(rbind, mat_rows)

  # --- Control flow ---
  cf_rows <- list(
    data.frame(
      category = "control flow",
      r = "for (i in seq_along(x))",
      c = "for (int i = 0; ...)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "for (i in seq_len(n))",
      c = "for (int i = 0; ...)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "for (i in a:b)",
      c = "for (int i = a; ...)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "for (i in seq(a, b))",
      c = "for (int i = a; ...)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "for (x in seq(a, b, by))",
      c = "for (double x = a; ...)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "for (x in vec)",
      c = "for + x = vec[i]",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "while (cond)",
      c = "while (cond)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "repeat",
      c = "while (1)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "break",
      c = "break",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "next",
      c = "continue",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "if / if-else",
      c = "if / if-else",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "ifelse(c, a, b)",
      c = "c ? a : b",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "control flow",
      r = "stop(\"msg\")",
      c = "Rf_error(\"msg\")",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    )
  )
  cf_df <- do.call(rbind, cf_rows)

  # --- Casts / coercion ---
  cast_rows <- list(
    data.frame(
      category = "cast",
      r = "as.integer(x)",
      c = "(int)(x)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "cast",
      r = "as.double(x)",
      c = "(double)(x)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "cast",
      r = "as.numeric(x)",
      c = "(double)(x)",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    )
  )
  cast_df <- do.call(rbind, cast_rows)

  # --- R fallback (rf_call) ---
  rf_rows <- list(
    data.frame(
      category = "R fallback",
      r = "f(x, ...)",
      c = "Rf_eval(Rf_lang(...))",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      category = "R fallback",
      r = "x[mask]",
      c = "count + alloc + fill",
      vectorized = FALSE,
      stringsAsFactors = FALSE
    )
  )
  rf_df <- do.call(rbind, rf_rows)

  out <- rbind(
    ops_df,
    math_df,
    red_df,
    cum_df,
    ew_df,
    vec_df,
    mat_df,
    cf_df,
    cast_df,
    rf_df
  )
  rownames(out) <- NULL
  out
}

#' Compile a small declare()-annotated R subset with TinyCC
#'
#' `tcc_quick()` is an experimental C-first path for compiling a strict subset
#' of R functions annotated with `declare(type(...))`. In the current subset, supported
#' bodies are recursive scalar expressions (arithmetic, comparisons, logical
#' operators, unary operators, selected scalar math functions, and scalar
#' `if (cond) a else b`/`ifelse(cond, a, b)`) over declared scalar
#' `double`/`integer`/`logical` arguments. Simple statement blocks with
#' scalar `<-` assignments before the final expression are supported.
#' Rank-3+ declarations (for example `double(NA, NA, NA)`) are accepted at
#' parse time but currently reserved for upcoming multidimensional array
#' support; they fallback in `soft`/`auto` and error in `hard` mode.
#'
#' For non-arithmetic symbol calls (e.g. `max(x, y)` or `pmax(a, b, c, d, e)`),
#' `tcc_quick()` emits a wrapper that constructs the call pairlist and evaluates
#' it in base via `Rf_eval()`, so R's primitive/internal implementation is used.
#'
#' Lowering descends recursively through expressions and stops at boundary calls
#' (`.Call`, `.C`, `.External`, `.Internal`, `.Primitive`), where it currently
#' falls back according to `fallback`.
#'
#' Unsupported functions can either fallback to the original R function or
#' error, depending on `fallback`.
#'
#' @param fn Function to compile.
#' @param fallback One of `"auto"`, `"soft"`, `"hard"`.
#'   Legacy aliases `"always"` and `"never"` map to `"soft"` and `"hard"`.
#' @param mode One of `"compile"` (default) or `"code"`. Use `"code"`
#'   to return generated C source without compiling.
#' @param debug Print generated C source and lowering diagnostics.
#' @return A function with the same formals as `fn`, or `fn` itself when
#'   fallback is used. When `mode = "code"`, returns a character string
#'   containing generated C source.
#' @export
tcc_quick <- function(
  fn,
  fallback = c("auto", "soft", "hard", "always", "never"),
  mode = c("compile", "code"),
  debug = FALSE
) {
  if (!is.function(fn)) {
    stop("fn must be a function", call. = FALSE)
  }

  fallback <- match.arg(fallback)
  if (fallback == "always") {
    fallback <- "soft"
  }
  if (fallback == "never") {
    fallback <- "hard"
  }
  mode <- match.arg(mode)
  decl <- tcc_quick_parse_declare(fn)
  ir <- tcc_quick_lower(fn, decl)

  if (identical(ir$tag, "fallback")) {
    if (isTRUE(debug)) {
      message("tcc_quick fallback reason: ", ir$reason)
    }
    if (mode == "code") {
      stop(
        "No code generated because function is outside the current tcc_quick subset: ",
        ir$reason,
        call. = FALSE
      )
    }
    if (fallback == "hard") {
      stop(
        "Function is outside the current tcc_quick subset: ",
        ir$reason,
        call. = FALSE
      )
    }
    warning(
      "tcc_quick: falling back to R for '",
      deparse(substitute(fn)),
      "': ",
      ir$reason,
      call. = FALSE
    )
    return(fn)
  }

  tccq_validate_ir(ir, fallback = fallback)

  if (mode == "code") {
    return(tcc_quick_codegen(ir, decl, fn_name = "tcc_quick_entry"))
  }

  cache_key <- tcc_quick_cache_key(fn, decl)
  cache_hit <- tcc_quick_cache_get(cache_key)
  if (!is.null(cache_hit)) {
    return(cache_hit)
  }

  built <- tcc_quick_compile(fn, decl, ir, debug = debug)
  wrapped <- tcc_quick_make_wrapper(
    built$callable,
    formals(fn),
    built$compiled,
    fn
  )
  tcc_quick_cache_set(cache_key, wrapped)
  wrapped
}
