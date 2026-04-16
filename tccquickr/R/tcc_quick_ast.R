# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# ---------------------------------------------------------------------------
# AST helpers for tcc_quick
# ---------------------------------------------------------------------------

tccq_node_name <- function(x) {
  if (is.symbol(x)) {
    return(as.character(x))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x[[1L]])) {
    return(x[[1L]])
  }
  NULL
}

tccq_call_head <- function(e) {
  if (!is.call(e) || length(e) < 1L) {
    return(NULL)
  }
  tccq_node_name(e[[1L]])
}

tccq_is_call <- function(e, head) {
  identical(tccq_call_head(e), head)
}

tccq_is_missing_arg <- function(x) {
  is.symbol(x) && identical(tccq_node_name(x), "")
}

tccq_call_arg_names <- function(args) {
  nms <- names(args)
  if (is.null(nms)) {
    nms <- rep("", length(args))
  }
  nms[is.na(nms)] <- ""
  nms
}

tccq_parse_subset_call <- function(e) {
  if (!is.call(e) || !tccq_is_call(e, "[")) {
    return(list(is_subset = FALSE, ok = FALSE))
  }

  args <- as.list(e)[-1L]
  if (length(args) < 2L) {
    return(list(
      is_subset = TRUE,
      ok = FALSE,
      reason = "Subscript call must include at least one index"
    ))
  }
  if (length(args) > 3L) {
    return(list(
      is_subset = TRUE,
      ok = FALSE,
      reason = "Only one- and two-dimensional subscripting is in current subset"
    ))
  }

  nms <- tccq_call_arg_names(args)
  if (nzchar(nms[[1L]])) {
    return(list(
      is_subset = TRUE,
      ok = FALSE,
      reason = "Subscript target cannot be named in current subset"
    ))
  }

  idx <- args[-1L]
  idx_nms <- nms[-1L]
  if (any(nzchar(idx_nms))) {
    bad <- idx_nms[nzchar(idx_nms)][[1L]]
    return(list(
      is_subset = TRUE,
      ok = FALSE,
      reason = paste0(
        "Named subscript argument '",
        bad,
        "' is not in current subset"
      )
    ))
  }

  list(
    is_subset = TRUE,
    ok = TRUE,
    target = args[[1L]],
    indices = idx,
    rank = length(idx),
    missing = vapply(idx, tccq_is_missing_arg, logical(1))
  )
}
