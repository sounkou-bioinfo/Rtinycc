# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# IR node constructors. Every node is a tagged list with $tag and $mode.
# This is the single source of truth for IR node shapes.

tcc_ir_node <- function(tag, mode = NULL) {
  list(tag = tag, mode = mode)
}

tcc_ir_const <- function(value, mode) {
  node <- tcc_ir_node("const", mode = mode)
  node$value <- value
  node
}

tcc_ir_var <- function(name, mode = NULL) {
  node <- tcc_ir_node("var", mode = mode)
  node$name <- name
  node
}

tcc_ir_param <- function(name, mode = NULL) {
  node <- tcc_ir_node("param", mode = mode)
  node$name <- name
  node
}

tcc_ir_unary <- function(op, x, mode) {
  node <- tcc_ir_node("unary", mode = mode)
  node$op <- op
  node$x <- x
  node
}

tcc_ir_binary <- function(op, lhs, rhs, mode) {
  node <- tcc_ir_node("binary", mode = mode)
  node$op <- op
  node$lhs <- lhs
  node$rhs <- rhs
  node
}

tcc_ir_if <- function(cond, yes, no, mode) {
  node <- tcc_ir_node("if", mode = mode)
  node$cond <- cond
  node$yes <- yes
  node$no <- no
  node
}

tcc_ir_call <- function(fun, args, mode) {
  node <- tcc_ir_node("call", mode = mode)
  node$fun <- fun
  node$args <- args
  node
}

tcc_ir_block <- function(stmts, result) {
  node <- tcc_ir_node("block", mode = result$mode)
  node$stmts <- stmts
  node$result <- result
  node
}

tcc_ir_assign <- function(name, expr, mode, kind = "scalar") {
  node <- tcc_ir_node("assign", mode = mode)
  node$name <- name
  node$expr <- expr
  node$kind <- kind
  node
}

tcc_ir_for <- function(var, target, body) {
  node <- tcc_ir_node("for", mode = "void")
  node$var <- var
  node$target <- target
  node$body <- body
  node
}

tcc_ir_vec_alloc <- function(len_expr) {
  node <- tcc_ir_node("vec_alloc", mode = "sexp")
  node$len_expr <- len_expr
  node
}

tcc_ir_vec_get <- function(arr, idx, mode = "double") {
  node <- tcc_ir_node("vec_get", mode = mode)
  node$arr <- arr
  node$idx <- idx
  node
}

tcc_ir_vec_set <- function(arr, idx, value) {
  node <- tcc_ir_node("vec_set", mode = "void")
  node$arr <- arr
  node$idx <- idx
  node$value <- value
  node
}

tcc_ir_length <- function(arr) {
  node <- tcc_ir_node("length", mode = "integer")
  node$arr <- arr
  node
}

tcc_ir_rf_call <- function(fun, args, mode = "sexp") {
  node <- tcc_ir_node("rf_call", mode = mode)
  node$fun <- fun
  node$args <- args
  node
}

tcc_ir_fallback <- function(reason) {
  tcc_ir_node("fallback", mode = "sexp")
  list(tag = "fallback", mode = "sexp", reason = reason)
}
