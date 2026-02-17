# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

new_tccq_walker <- function(
  class = character(0),
  state = new.env(parent = emptyenv())
) {
  structure(
    list(state = state),
    class = unique(c(class, "tccq_walker"))
  )
}

tccq_visit <- function(walker, node) {
  if (inherits(walker, "tccq_boundary_walker")) {
    return(tccq_visit_boundary(walker, node))
  }
  if (inherits(walker, "tccq_subset_walker")) {
    return(tccq_visit_subset(walker, node))
  }
  if (inherits(walker, "tccq_construct_walker")) {
    return(tccq_visit_construct(walker, node))
  }
  tccq_visit_base(walker, node)
}

tccq_visit_base <- function(walker, node) {
  if (!is.call(node)) {
    return(invisible(NULL))
  }
  for (i in seq_along(node)[-1]) {
    tccq_visit(walker, node[[i]])
  }
  invisible(NULL)
}

tccq_make_boundary_walker <- function(boundary_calls) {
  w <- new_tccq_walker(class = "tccq_boundary_walker")
  w$state$boundary_calls <- boundary_calls
  w$state$found <- FALSE
  w
}

tccq_visit_boundary <- function(walker, node) {
  if (isTRUE(walker$state$found)) {
    return(invisible(NULL))
  }
  if (is.call(node) && is.symbol(node[[1]])) {
    fn <- as.character(node[[1]])
    if (fn %in% walker$state$boundary_calls) {
      walker$state$found <- TRUE
      return(invisible(NULL))
    }
  }
  tccq_visit_base(walker, node)
}

tccq_boundary_found <- function(walker) {
  isTRUE(walker$state$found)
}

tccq_make_subset_walker <- function() {
  w <- new_tccq_walker(class = "tccq_subset_walker")
  w$state$arrays <- character(0)
  w
}

tccq_visit_subset <- function(walker, node) {
  if (
    is.call(node) &&
      length(node) == 3L &&
      is.symbol(node[[1]]) &&
      identical(as.character(node[[1]]), "[") &&
      is.symbol(node[[2]])
  ) {
    walker$state$arrays <- unique(c(
      walker$state$arrays,
      as.character(node[[2]])
    ))
  }
  tccq_visit_base(walker, node)
}

tccq_subset_arrays <- function(walker) {
  unique(walker$state$arrays)
}

tccq_make_construct_walker <- function() {
  w <- new_tccq_walker(class = "tccq_construct_walker")
  w$state$calls <- character(0)
  w$state$max_depth <- 0L
  w$state$cur_depth <- 0L
  w
}

tccq_visit_construct <- function(walker, node) {
  if (!is.call(node)) {
    return(invisible(NULL))
  }

  if (is.symbol(node[[1]])) {
    walker$state$calls <- unique(c(walker$state$calls, as.character(node[[1]])))
  }

  if (is.symbol(node[[1]]) && identical(as.character(node[[1]]), "for")) {
    walker$state$cur_depth <- walker$state$cur_depth + 1L
    walker$state$max_depth <- max(
      walker$state$max_depth,
      walker$state$cur_depth
    )
    tccq_visit_base(walker, node)
    walker$state$cur_depth <- walker$state$cur_depth - 1L
    return(invisible(NULL))
  }

  tccq_visit_base(walker, node)
}

tccq_constructs <- function(walker) {
  list(
    calls = sort(unique(walker$state$calls)),
    has_for = "for" %in% walker$state$calls,
    max_loop_depth = as.integer(walker$state$max_depth)
  )
}
