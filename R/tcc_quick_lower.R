# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_quick_fallback_ir <- function(reason) {
    list(tag = "fallback", reason = reason)
}

tcc_quick_body_without_declare <- function(fn) {
    exprs <- tcc_quick_extract_exprs(fn)
    exprs[-1]
}

tcc_quick_lower <- function(fn, decl) {
    exprs <- tcc_quick_body_without_declare(fn)
    if (length(exprs) != 1L) {
        return(tcc_quick_fallback_ir("MVP supports a single expression body after declare()"))
    }

    e <- exprs[[1]]
    if (!is.call(e)) {
        return(tcc_quick_fallback_ir("MVP supports only binary operations"))
    }

    # Prefer R's own optimized internal path when users explicitly call
    # .Internal with a 2-argument inner call.
    if (is.symbol(e[[1]]) && identical(as.character(e[[1]]), ".Internal") && length(e) == 2L) {
        inner <- e[[2]]
        if (is.call(inner) && length(inner) == 3L && is.symbol(inner[[1]]) &&
            is.symbol(inner[[2]]) && is.symbol(inner[[3]])) {
            a1 <- as.character(inner[[2]])
            a2 <- as.character(inner[[3]])
            if (a1 %in% names(decl$args) && a2 %in% names(decl$args)) {
                return(list(
                    tag = "internal_call2",
                    fun = as.character(inner[[1]]),
                    arg1 = a1,
                    arg2 = a2
                ))
            }
        }
        return(tcc_quick_fallback_ir(".Internal form not supported in MVP"))
    }

    if (length(e) != 3L) {
        return(tcc_quick_fallback_ir("MVP supports only binary operations"))
    }

    op <- as.character(e[[1]])

    lhs <- e[[2]]
    rhs <- e[[3]]
    if (!is.symbol(lhs) || !is.symbol(rhs)) {
        return(tcc_quick_fallback_ir("MVP supports variable-variable binary expressions"))
    }

    lhs_name <- as.character(lhs)
    rhs_name <- as.character(rhs)
    if (!lhs_name %in% names(decl$args) || !rhs_name %in% names(decl$args)) {
        return(tcc_quick_fallback_ir("Expression references undeclared symbols"))
    }

    # High-level path: for non-arithmetic two-arg calls, route through
    # Rf_lang3 + Rf_eval so R's primitive/internal implementation executes.
    if (!op %in% c("+", "-", "*", "/") && is.symbol(e[[1]])) {
        return(list(
            tag = "rf_lang3_call",
            fun = op,
            arg1 = lhs_name,
            arg2 = rhs_name
        ))
    }

    if (!op %in% c("+", "-", "*", "/")) {
        return(tcc_quick_fallback_ir("Operator not in MVP subset"))
    }

    lhs_decl <- decl$args[[lhs_name]]
    rhs_decl <- decl$args[[rhs_name]]
    if (!isTRUE(lhs_decl$is_scalar) || !isTRUE(rhs_decl$is_scalar)) {
        return(tcc_quick_fallback_ir("MVP supports only scalar arguments"))
    }

    if (lhs_decl$mode == "logical" || rhs_decl$mode == "logical") {
        return(tcc_quick_fallback_ir("MVP scalar codegen excludes logical arithmetic"))
    }

    result_mode <- if (op == "/" || lhs_decl$mode == "double" || rhs_decl$mode == "double") {
        "double"
    } else {
        "integer"
    }

    list(
        tag = "scalar_binop",
        op = op,
        lhs = lhs_name,
        rhs = rhs_name,
        result_mode = result_mode,
        arg_modes = vapply(decl$args[decl$formal_names], function(x) x$mode, character(1))
    )
}
