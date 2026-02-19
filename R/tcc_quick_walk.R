# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Generic code walker following codetools walkCode/makeCodeWalker pattern.
# A walker is a list with $handler, $call, and $leaf functions plus arbitrary
# extra state. Handlers are keyed by call-head symbol name and return a
# handler function or NULL for default dispatch.

tccq_walk <- function(e, w) {
  if (typeof(e) == "language") {
    if (typeof(e[[1]]) %in% c("symbol", "character")) {
      h <- w$handler(as.character(e[[1]]), w)
      if (!is.null(h)) {
        return(h(e, w))
      }
    }
    w$call(e, w)
  } else {
    w$leaf(e, w)
  }
}

tccq_make_walker <- function(
  ...,
  handler = function(v, w) NULL,
  call = function(e, w) {
    if (typeof(e[[1]]) == "language") {
      tccq_walk(e[[1]], w)
    }
    for (ee in as.list(e)[-1]) {
      if (!missing(ee)) tccq_walk(ee, w)
    }
    invisible(NULL)
  },
  leaf = function(e, w) invisible(NULL)
) {
  list(handler = handler, call = call, leaf = leaf, ...)
}

# --- Boundary detection walker ---
# Checks whether an expression tree contains any boundary calls
# (.Call, .C, .External, .Internal, .Primitive).

tccq_boundary_calls <- function() {
  c(".Call", ".C", ".External", ".Internal", ".Primitive")
}

tccq_has_boundary <- function(e, boundary = tccq_boundary_calls()) {
  found <- FALSE
  w <- tccq_make_walker(
    handler = function(v, w) {
      if (v %in% boundary) {
        function(e, w) {
          found <<- TRUE
        }
      }
    },
    call = function(e, w) {
      if (!found) {
        if (typeof(e[[1]]) == "language") {
          tccq_walk(e[[1]], w)
        }
        for (ee in as.list(e)[-1]) {
          if (!missing(ee)) {
            tccq_walk(ee, w)
          }
          if (found) break
        }
      }
    }
  )
  tccq_walk(e, w)
  found
}

# --- Subset array collector ---
# Collects all symbol names used as `x[...]` targets.

tccq_collect_subset_arrays <- function(e) {
  arrays <- character(0)
  w <- tccq_make_walker(
    handler = function(v, w) {
      if (identical(v, "[")) {
        function(e, w) {
          if (length(e) == 3L && is.symbol(e[[2]])) {
            arrays <<- unique(c(arrays, as.character(e[[2]])))
          }
          if (typeof(e[[1]]) == "language") {
            tccq_walk(e[[1]], w)
          }
          for (ee in as.list(e)[-1]) {
            if (!missing(ee)) tccq_walk(ee, w)
          }
        }
      }
    }
  )
  tccq_walk(e, w)
  arrays
}

# --- Construct scanner ---
# Scans for call heads and loop nesting depth.

tccq_scan_constructs <- function(exprs) {
  calls <- character(0)
  max_depth <- 0L
  cur_depth <- 0L

  w <- tccq_make_walker(
    handler = function(v, w) {
      if (identical(v, "for")) {
        function(e, w) {
          calls <<- unique(c(calls, "for"))
          cur_depth <<- cur_depth + 1L
          max_depth <<- max(max_depth, cur_depth)
          for (ee in as.list(e)[-1]) {
            if (!missing(ee)) tccq_walk(ee, w)
          }
          cur_depth <<- cur_depth - 1L
        }
      }
    },
    call = function(e, w) {
      if (is.symbol(e[[1]])) {
        calls <<- unique(c(calls, as.character(e[[1]])))
      }
      if (typeof(e[[1]]) == "language") {
        tccq_walk(e[[1]], w)
      }
      for (ee in as.list(e)[-1]) {
        if (!missing(ee)) tccq_walk(ee, w)
      }
    }
  )

  for (e in exprs) {
    tccq_walk(e, w)
  }
  list(
    calls = sort(unique(calls)),
    has_for = "for" %in% calls,
    max_loop_depth = as.integer(max_depth)
  )
}
