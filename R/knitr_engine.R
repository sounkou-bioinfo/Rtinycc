# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Adapted in part from callme::callme_engine() by Mike Cheng.
# The original implementation is MIT-licensed. This version stores the chunk
# body in a named R object for later vignette use instead of compiling it.

rtinycc_c_block <- function(code, options = list()) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(code)
  }

  opts <- utils::modifyList(
    list(
      echo = TRUE,
      eval = FALSE,
      engine = "c",
      class.source = c("rtinycc"),
      prompt = FALSE,
      comment = "",
      tidy = FALSE,
      highlight = TRUE
    ),
    options
  )
  knitr::asis_output(knitr::engine_output(opts, code, ""))
}

#' A knitr engine for reusable inline C source chunks in vignettes
#'
#' Supported custom chunk options:
#' - `object` or `name`: R object name receiving the chunk body as a single
#'   string. Defaults to the chunk label.
#' - `rcode`: whether to append a collapsible block showing the equivalent
#'   R assignment code. Defaults to `TRUE`.
#'
#' @param options Chunk options supplied by knitr.
#'
#' @return Rendered chunk text, or `NULL` if `knitr` is unavailable.
#' @keywords internal
#' @noRd
rtinycc_engine <- function(options) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(NULL)
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  object_name <- options$object %||% options$name %||% options$label
  if (
    !is.character(object_name) ||
      length(object_name) != 1L ||
      !nzchar(object_name)
  ) {
    stop(
      "rtinycc_engine() requires a non-empty `object` option or chunk label.",
      call. = FALSE
    )
  }

  code <- paste(options$code, collapse = "\n")
  knit_env <- knitr::knit_global()
  assign(object_name, code, envir = knit_env)

  c_options <- options
  rendered <- rtinycc_c_block(code, c_options)

  if (isFALSE(options$rcode %||% TRUE)) {
    return(rendered)
  }

  rcode <- paste0(
    object_name,
    " <- ",
    paste(deparse(code, width.cutoff = 500L), collapse = "\n")
  )

  r_options <- options
  r_options$engine <- "r"
  r_options$class.source <- character()
  rendered_r <- knitr::engine_output(r_options, rcode, "")
  rendered_r <- paste(
    "<details>",
    "<summary>Click to show R code</summary>",
    rendered_r,
    "</details>",
    sep = "\n"
  )

  paste(rendered, rendered_r, sep = "\n")
}
