# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# Portions copyright (C) Brian Lee Yung Rowe and lambda.r contributors
# SPDX-License-Identifier: LGPL-3.0-only

# This package-specific dispatcher adapts the multipart pattern matching used by
# lambda.r 1.2.4. It supports the literal and catch-all rules used by Rtinycc's
# C wrapper generator, without exposing a general-purpose dispatch API.

.rtinycc_codegen_rules <- new.env(parent = emptyenv())

`%as%` <- function(signature, body) {
  signature <- substitute(signature)
  body <- substitute(body)
  where <- topenv(parent.frame())

  if (!is.call(signature) || !is.symbol(signature[[1L]])) {
    stop("a code-generation rule must have a function-call signature", call. = FALSE)
  }

  name <- as.character(signature[[1L]])
  arguments <- as.list(signature)[-1L]
  is_variable <- vapply(arguments, is.symbol, logical(1))
  argument_names <- vapply(
    seq_along(arguments),
    function(index) {
      if (is_variable[[index]]) {
        as.character(arguments[[index]])
      } else {
        paste0(".rtinycc_arg_", index)
      }
    },
    character(1)
  )
  patterns <- lapply(
    seq_along(arguments),
    function(index) {
      if (is_variable[[index]]) NULL else eval(arguments[[index]], envir = where)
    }
  )
  definition <- eval(
    call(
      "function",
      as.pairlist(structure(rep(list(NULL), length(arguments)), names = argument_names)),
      body
    ),
    envir = where
  )
  rule <- list(
    signature = signature,
    argument_names = argument_names,
    is_pattern = !is_variable,
    patterns = patterns,
    definition = definition
  )

  variants <- get0(
    name,
    envir = .rtinycc_codegen_rules,
    inherits = FALSE,
    ifnotfound = list()
  )
  duplicate <- which(vapply(
    variants,
    function(candidate) identical(candidate$signature, signature),
    logical(1)
  ))
  if (length(duplicate) > 0L) {
    variants[[duplicate[[1L]]]] <- rule
  } else {
    variants[[length(variants) + 1L]] <- rule
  }
  assign(name, variants, envir = .rtinycc_codegen_rules)

  if (!exists(name, envir = where, inherits = FALSE) || length(variants) == 1L) {
    dispatcher <- eval(
      substitute(
        function(...) .rtinycc_dispatch_codegen_rule(rule_name, list(...)),
        list(rule_name = name)
      ),
      envir = where
    )
    assign(name, dispatcher, envir = where)
  }

  invisible(NULL)
}

.rtinycc_dispatch_codegen_rule <- function(name, arguments) {
  variants <- get0(
    name,
    envir = .rtinycc_codegen_rules,
    inherits = FALSE,
    ifnotfound = list()
  )
  for (rule in variants) {
    if (length(arguments) != length(rule$argument_names)) next

    bound <- arguments
    names(bound) <- rule$argument_names
    matches <- TRUE
    for (index in which(rule$is_pattern)) {
      expected <- rule$patterns[[index]]
      value <- bound[[index]]
      matched <- if (is.null(expected)) {
        is.null(value)
      } else {
        value == expected
      }
      if (length(matched) == 0L || is.na(matched[[1L]]) || !matched[[1L]]) {
        matches <- FALSE
        break
      }
    }
    if (matches) return(do.call(rule$definition, bound))
  }

  labels <- vapply(
    arguments,
    function(value) {
      if (length(value) == 1L && is.atomic(value)) {
        as.character(value)
      } else {
        paste(class(value), collapse = "/")
      }
    },
    character(1)
  )
  stop(
    "No valid function for '", name, "(", paste(labels, collapse = ","), ")'",
    call. = FALSE
  )
}
