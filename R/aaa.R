# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

## usethis namespace: start
#' @useDynLib Rtinycc, .registration = TRUE
#' @importFrom lambda.r %as% UseFunction
## usethis namespace: end
NULL

#' we are using .Call directly, this is to make R CMD check happy
#'
#' @return The result returned by the invoked native routine. This alias is
#'   package-internal and exists to satisfy `R CMD check` native routine
#'   registration requirements.
#' @keywords internal
.RtinyccCall <- base::.Call

#' Tiny template string interpolator (glue alternative)
#' @param text String to interpolate
#' @param env Environment
#' @noRd
str_interp <- function(text, env = parent.frame()) {
  pattern <- "\\{([^}]+)\\}"
  matches <- regmatches(text, gregexpr(pattern, text))[[1]]
  if (length(matches) == 0) return(text)
  
  res <- text
  for (m in unique(matches)) {
    code <- substr(m, 2, nchar(m) - 1)
    val <- eval(parse(text = code), envir = env)
    if (is.null(val)) val <- ""
    res <- gsub(m, paste0(val, collapse = ""), res, fixed = TRUE)
  }
  res
}
