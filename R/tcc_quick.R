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
  call_expr <- as.call(c(as.name(".compiled_callable"), arg_syms))
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

tcc_quick_compile <- function(fn, decl, ir, debug = FALSE) {
  entry <- "tcc_quick_entry"
  src <- tcc_quick_codegen(ir, decl, fn_name = entry)

  if (isTRUE(debug)) {
    message("tcc_quick generated C source:\n", src)
  }

  spec <- list(args = rep("sexp", length(decl$formal_names)), returns = "sexp")
  names(spec$args) <- NULL

  ffi <- tcc_ffi() |>
    tcc_source(src)
  ffi <- do.call(tcc_bind, c(list(ffi), setNames(list(spec), entry)))
  compiled <- tcc_compile(ffi)

  list(
    callable = compiled[[entry]],
    compiled = compiled
  )
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
#' @param fallback One of `"auto"`, `"always"`, or `"never"`.
#' @param mode One of `"compile"` (default) or `"code"`. Use `"code"`
#'   to return generated C source without compiling.
#' @param debug Print generated C source and lowering diagnostics.
#' @return A function with the same formals as `fn`, or `fn` itself when
#'   fallback is used. When `mode = "code"`, returns a character string
#'   containing generated C source.
#' @export
tcc_quick <- function(
  fn,
  fallback = c("auto", "always", "never"),
  mode = c("compile", "code"),
  debug = FALSE
) {
  if (!is.function(fn)) {
    stop("fn must be a function", call. = FALSE)
  }

  fallback <- match.arg(fallback)
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
    if (fallback == "never") {
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
