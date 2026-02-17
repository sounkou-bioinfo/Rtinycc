# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_quick_fallback_ir <- function(reason) {
    list(tag = "fallback", reason = reason)
}

tcc_quick_boundary_calls <- function() {
    c(".Call", ".C", ".External", ".Internal", ".Primitive")
}

tcc_quick_promote_mode <- function(lhs, rhs) {
    if (lhs == rhs) {
        return(lhs)
    }
    if (lhs %in% c("double", "integer") && rhs %in% c("double", "integer")) {
        return("double")
    }
    NULL
}

tcc_quick_promote_many <- function(modes) {
    if (length(modes) < 1L) {
        return(NULL)
    }
    out <- modes[[1]]
    if (length(modes) == 1L) {
        return(out)
    }
    for (m in modes[-1]) {
        out2 <- tcc_quick_promote_mode(out, m)
        if (!is.null(out2)) {
            out <- out2
            next
        }
        if (identical(out, m) && identical(out, "logical")) {
            out <- "logical"
            next
        }
        return(NULL)
    }
    out
}

tcc_quick_symbol_mode <- function(sym_name, decl) {
    if (!sym_name %in% names(decl$args)) {
        return(NULL)
    }
    spec <- decl$args[[sym_name]]
    if (!isTRUE(spec$is_scalar)) {
        return(NULL)
    }
    spec$mode
}

tcc_quick_lower_result <- function(ok, node = NULL, mode = NULL, reason = NULL, boundary = NULL) {
    list(ok = ok, node = node, mode = mode, reason = reason, boundary = boundary)
}

tcc_quick_numeric_unary_funs <- function() {
    c("abs", "sqrt", "sin", "cos", "tan", "asin", "acos", "atan", "exp", "log", "log10", "floor", "ceiling", "trunc", "tanh")
}

tcc_quick_lower_expr <- function(e, decl, locals = list()) {
    if (is.symbol(e)) {
        nm <- as.character(e)
        if (nm %in% names(locals)) {
            loc <- locals[[nm]]
            return(tcc_quick_lower_result(TRUE, node = loc$node, mode = loc$mode))
        }
        mode <- tcc_quick_symbol_mode(nm, decl)
        if (is.null(mode)) {
            return(tcc_quick_lower_result(FALSE, reason = paste0("Undeclared or non-scalar symbol: ", nm)))
        }
        if (mode == "logical") {
            # logical is supported for conditions and logical ops
            return(tcc_quick_lower_result(TRUE, node = list(tag = "var", name = nm), mode = "logical"))
        }
        return(tcc_quick_lower_result(TRUE, node = list(tag = "var", name = nm), mode = mode))
    }

    if (length(e) == 1L && is.integer(e)) {
        return(tcc_quick_lower_result(TRUE, node = list(tag = "const", value = as.integer(e)[[1]], mode = "integer"), mode = "integer"))
    }
    if (length(e) == 1L && is.double(e)) {
        return(tcc_quick_lower_result(TRUE, node = list(tag = "const", value = as.double(e)[[1]], mode = "double"), mode = "double"))
    }
    if (length(e) == 1L && is.logical(e) && !is.na(e)) {
        return(tcc_quick_lower_result(TRUE, node = list(tag = "const", value = isTRUE(e), mode = "logical"), mode = "logical"))
    }

    if (!is.call(e)) {
        return(tcc_quick_lower_result(FALSE, reason = "Unsupported expression node"))
    }

    fun <- e[[1]]
    if (!is.symbol(fun)) {
        return(tcc_quick_lower_result(FALSE, reason = "Only symbol calls are supported"))
    }

    fname <- as.character(fun)
    if (fname %in% tcc_quick_boundary_calls()) {
        return(tcc_quick_lower_result(
            FALSE,
            reason = paste0("Boundary call encountered: ", fname),
            boundary = fname
        ))
    }

    if (fname == "if") {
        if (length(e) != 4L) {
            return(tcc_quick_lower_result(FALSE, reason = "if() requires condition, then, else in current subset"))
        }
        cond <- tcc_quick_lower_expr(e[[2]], decl, locals)
        yes <- tcc_quick_lower_expr(e[[3]], decl, locals)
        no <- tcc_quick_lower_expr(e[[4]], decl, locals)
        if (!cond$ok) {
            return(cond)
        }
        if (!yes$ok) {
            return(yes)
        }
        if (!no$ok) {
            return(no)
        }
        if (!cond$mode %in% c("logical", "integer", "double")) {
            return(tcc_quick_lower_result(FALSE, reason = "if() condition must be scalar logical/numeric"))
        }
        out_mode <- tcc_quick_promote_mode(yes$mode, no$mode)
        if (is.null(out_mode) && yes$mode == no$mode && yes$mode == "logical") {
            out_mode <- "logical"
        }
        if (is.null(out_mode)) {
            return(tcc_quick_lower_result(FALSE, reason = "if() branches must have compatible scalar types"))
        }
        return(tcc_quick_lower_result(TRUE,
            node = list(tag = "if", cond = cond$node, yes = yes$node, no = no$node),
            mode = out_mode
        ))
    }

    if (fname == "(") {
        if (length(e) != 2L) {
            return(tcc_quick_lower_result(FALSE, reason = "Parenthesized expression must have one argument"))
        }
        return(tcc_quick_lower_expr(e[[2]], decl, locals))
    }

    if (fname == "ifelse") {
        if (length(e) != 4L) {
            return(tcc_quick_lower_result(FALSE, reason = "ifelse() requires cond, yes, no in current subset"))
        }
        cond <- tcc_quick_lower_expr(e[[2]], decl, locals)
        yes <- tcc_quick_lower_expr(e[[3]], decl, locals)
        no <- tcc_quick_lower_expr(e[[4]], decl, locals)
        if (!cond$ok) {
            return(cond)
        }
        if (!yes$ok) {
            return(yes)
        }
        if (!no$ok) {
            return(no)
        }
        if (!cond$mode %in% c("logical", "integer", "double")) {
            return(tcc_quick_lower_result(FALSE, reason = "ifelse() condition must be scalar logical/numeric"))
        }
        out_mode <- tcc_quick_promote_mode(yes$mode, no$mode)
        if (is.null(out_mode) && yes$mode == no$mode && yes$mode == "logical") {
            out_mode <- "logical"
        }
        if (is.null(out_mode)) {
            return(tcc_quick_lower_result(FALSE, reason = "ifelse() branches must have compatible scalar types"))
        }
        return(tcc_quick_lower_result(
            TRUE,
            node = list(tag = "if", cond = cond$node, yes = yes$node, no = no$node),
            mode = out_mode
        ))
    }

    if (fname == "switch") {
        # Current subset switch support: numeric selector
        # switch(sel, case1, case2, ...)
        # Out-of-range selectors return NULL.
        if (length(e) < 3L) {
            return(tcc_quick_lower_result(FALSE, reason = "switch() requires selector and at least one case in current subset"))
        }

        arg_names <- names(as.list(e)[-1])
        if (!is.null(arg_names) && any(nzchar(arg_names))) {
            return(tcc_quick_lower_result(FALSE, reason = "switch() named arms are not supported in current subset"))
        }

        selector <- tcc_quick_lower_expr(e[[2]], decl, locals)
        if (!selector$ok) {
            return(selector)
        }
        if (!selector$mode %in% c("integer", "double", "logical")) {
            return(tcc_quick_lower_result(FALSE, reason = "switch() selector must be scalar numeric/logical in current subset"))
        }

        arm_exprs <- as.list(e)[-c(1, 2)]
        if (length(arm_exprs) < 1L) {
            return(tcc_quick_lower_result(FALSE, reason = "switch() requires at least one case"))
        }

        lowered_arms <- lapply(arm_exprs, function(a) tcc_quick_lower_expr(a, decl, locals))
        bad <- which(!vapply(lowered_arms, function(x) isTRUE(x$ok), logical(1)))
        if (length(bad) > 0L) {
            return(lowered_arms[[bad[[1]]]])
        }

        arm_modes <- vapply(lowered_arms, function(x) x$mode, character(1))
        out_mode <- tcc_quick_promote_many(arm_modes)
        if (is.null(out_mode)) {
            return(tcc_quick_lower_result(FALSE, reason = "switch() arms must have compatible scalar types"))
        }

        case_nodes <- lapply(lowered_arms, function(x) x$node)

        return(tcc_quick_lower_result(
            TRUE,
            node = list(
                tag = "switch_num",
                selector = selector$node,
                cases = case_nodes,
                result_mode = out_mode
            ),
            mode = "sexp"
        ))
    }

    if (fname %in% tcc_quick_numeric_unary_funs() && length(e) == 2L) {
        x <- tcc_quick_lower_expr(e[[2]], decl, locals)
        if (!x$ok) {
            return(x)
        }
        if (!x$mode %in% c("double", "integer")) {
            return(tcc_quick_lower_result(FALSE, reason = paste0(fname, "() requires numeric scalar")))
        }
        out_mode <- if (fname %in% c("floor", "ceiling", "trunc")) "double" else tcc_quick_promote_mode(x$mode, "double")
        return(tcc_quick_lower_result(
            TRUE,
            node = list(tag = "call1", fun = fname, x = x$node),
            mode = out_mode
        ))
    }

    # Generic fallback call path: use Rf_lang* + Rf_eval for non-operator
    # symbol calls with up to 4 symbol arguments.
    if (!fname %in% c("+", "-", "*", "/", "^", "%%", "%/%", "<", "<=", ">", ">=", "==", "!=", "&&", "||", "&", "|", "!", "if", "ifelse", "switch", "(")) {
        args <- as.list(e)[-1]
        if (length(args) >= 1L && length(args) <= 4L && all(vapply(args, is.symbol, logical(1)))) {
            arg_names <- vapply(args, as.character, character(1))
            ok_args <- all(vapply(arg_names, function(a) !is.null(tcc_quick_symbol_mode(a, decl)) && !a %in% names(locals), logical(1)))
            if (ok_args) {
                return(tcc_quick_lower_result(
                    TRUE,
                    node = list(tag = "rf_lang_call", fun = fname, args = arg_names),
                    mode = "sexp"
                ))
            }
        }
    }

    if (fname %in% c("+", "-") && length(e) == 2L) {
        x <- tcc_quick_lower_expr(e[[2]], decl, locals)
        if (!x$ok) {
            return(x)
        }
        if (!x$mode %in% c("double", "integer")) {
            return(tcc_quick_lower_result(FALSE, reason = "Unary +/- require numeric scalar"))
        }
        return(tcc_quick_lower_result(TRUE, node = list(tag = "unary", op = fname, x = x$node), mode = x$mode))
    }

    if (fname == "!" && length(e) == 2L) {
        x <- tcc_quick_lower_expr(e[[2]], decl, locals)
        if (!x$ok) {
            return(x)
        }
        if (!x$mode %in% c("logical", "integer", "double")) {
            return(tcc_quick_lower_result(FALSE, reason = "Unary ! requires logical/numeric scalar"))
        }
        return(tcc_quick_lower_result(TRUE, node = list(tag = "unary", op = "!", x = x$node), mode = "logical"))
    }

    if (length(e) != 3L) {
        return(tcc_quick_lower_result(FALSE, reason = "Unsupported call arity in current subset"))
    }

    lhs <- tcc_quick_lower_expr(e[[2]], decl, locals)
    rhs <- tcc_quick_lower_expr(e[[3]], decl, locals)
    if (!lhs$ok) {
        return(lhs)
    }
    if (!rhs$ok) {
        return(rhs)
    }

    if (fname %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
        if (!lhs$mode %in% c("double", "integer") || !rhs$mode %in% c("double", "integer")) {
            return(tcc_quick_lower_result(FALSE, reason = "Arithmetic operators require numeric scalars"))
        }
        out_mode <- if (fname %in% c("/", "^", "%%", "%/%")) {
            "double"
        } else {
            tcc_quick_promote_mode(lhs$mode, rhs$mode)
        }
        return(tcc_quick_lower_result(TRUE,
            node = list(tag = "binary", op = fname, lhs = lhs$node, rhs = rhs$node),
            mode = out_mode
        ))
    }

    if (fname %in% c("<", "<=", ">", ">=", "==", "!=")) {
        if (!lhs$mode %in% c("double", "integer", "logical") || !rhs$mode %in% c("double", "integer", "logical")) {
            return(tcc_quick_lower_result(FALSE, reason = "Comparison operators require scalar operands"))
        }
        return(tcc_quick_lower_result(TRUE,
            node = list(tag = "binary", op = fname, lhs = lhs$node, rhs = rhs$node),
            mode = "logical"
        ))
    }

    if (fname %in% c("&&", "||", "&", "|")) {
        if (!lhs$mode %in% c("logical", "integer", "double") || !rhs$mode %in% c("logical", "integer", "double")) {
            return(tcc_quick_lower_result(FALSE, reason = "Logical operators require scalar logical/numeric operands"))
        }
        return(tcc_quick_lower_result(TRUE,
            node = list(tag = "binary", op = fname, lhs = lhs$node, rhs = rhs$node),
            mode = "logical"
        ))
    }

    tcc_quick_lower_result(FALSE, reason = paste0("Operator not in current subset: ", fname))
}

tcc_quick_body_without_declare <- function(fn) {
    exprs <- tcc_quick_extract_exprs(fn)
    exprs[-1]
}

tcc_quick_is_sym <- function(x, nm = NULL) {
    ok <- is.symbol(x)
    if (!ok) {
        return(FALSE)
    }
    if (is.null(nm)) {
        return(TRUE)
    }
    identical(as.character(x), nm)
}

tcc_quick_is_call_sym <- function(x, nm) {
    is.call(x) && length(x) >= 1L && tcc_quick_is_sym(x[[1]], nm)
}

tcc_quick_parse_subset <- function(x) {
    if (!tcc_quick_is_call_sym(x, "[") || length(x) != 3L) {
        return(NULL)
    }
    if (!is.symbol(x[[2]])) {
        return(NULL)
    }
    list(arr = as.character(x[[2]]), idx = x[[3]])
}

tcc_quick_expr_has_boundary <- function(e) {
    w <- tccq_make_boundary_walker(tcc_quick_boundary_calls())
    tccq_visit(w, e)
    tccq_boundary_found(w)
}

tcc_quick_collect_subset_arrays <- function(e) {
    w <- tccq_make_subset_walker()
    tccq_visit(w, e)
    tccq_subset_arrays(w)
}

tcc_quick_validate_len_expr <- function(e, decl) {
    if (length(e) == 1L && (is.integer(e) || is.double(e))) {
        return(TRUE)
    }

    if (!is.call(e)) {
        return(FALSE)
    }

    if (is.symbol(e[[1]]) && identical(as.character(e[[1]]), "(")) {
        return(length(e) == 2L && tcc_quick_validate_len_expr(e[[2]], decl))
    }

    if (is.symbol(e[[1]]) && identical(as.character(e[[1]]), "length")) {
        if (length(e) != 2L || !is.symbol(e[[2]])) {
            return(FALSE)
        }
        nm <- as.character(e[[2]])
        if (!nm %in% names(decl$args)) {
            return(FALSE)
        }
        return(!isTRUE(decl$args[[nm]]$is_scalar))
    }

    if (!is.symbol(e[[1]])) {
        return(FALSE)
    }
    fn <- as.character(e[[1]])

    if (fn %in% c("+", "-") && length(e) == 2L) {
        return(tcc_quick_validate_len_expr(e[[2]], decl))
    }

    if (fn %in% c("+", "-", "*", "/") && length(e) == 3L) {
        return(tcc_quick_validate_len_expr(e[[2]], decl) && tcc_quick_validate_len_expr(e[[3]], decl))
    }

    FALSE
}

tcc_quick_parse_for_chain <- function(x) {
    loops <- list()
    cur <- x
    repeat {
        if (!tcc_quick_is_call_sym(cur, "for") || length(cur) != 4L || !is.symbol(cur[[2]])) {
            return(NULL)
        }
        iter_var <- as.character(cur[[2]])
        iter_expr <- cur[[3]]
        if (tcc_quick_is_call_sym(iter_expr, "seq_along") && length(iter_expr) == 2L && is.symbol(iter_expr[[2]])) {
            iter_kind <- "seq_along"
            iter_target <- as.character(iter_expr[[2]])
        } else if (tcc_quick_is_call_sym(iter_expr, "seq_len") && length(iter_expr) == 2L) {
            iter_kind <- "seq_len"
            iter_target <- iter_expr[[2]]
        } else {
            return(NULL)
        }

        loops[[length(loops) + 1L]] <- list(
            var = iter_var,
            kind = iter_kind,
            target = iter_target
        )

        body <- cur[[4]]
        if (tcc_quick_is_call_sym(body, "{") && length(body) == 2L) {
            body <- body[[2]]
        }
        if (tcc_quick_is_call_sym(body, "for")) {
            cur <- body
            next
        }
        return(list(loops = loops, terminal = body))
    }
}

tcc_quick_try_lower_nested_loop_kernel <- function(exprs, decl) {
    # Generic kernel pattern:
    # out <- double(length(x) + length(y) - 1)
    # for (i in seq_along(x)) {
    #   for (j in seq_along(y)) {
    #     out[idx(i,j)] <- <expr using [ ], i, j, constants, arithmetic>
    #   }
    # }
    # out
    if (length(exprs) != 3L) {
        return(NULL)
    }

    s1 <- exprs[[1]]
    s2 <- exprs[[2]]
    s3 <- exprs[[3]]

    if (!tcc_quick_is_call_sym(s1, "<-") || length(s1) != 3L || !is.symbol(s1[[2]])) {
        return(NULL)
    }
    out_name <- as.character(s1[[2]])
    init_rhs <- s1[[3]]
    if (!tcc_quick_is_call_sym(init_rhs, "double") || length(init_rhs) != 2L) {
        return(NULL)
    }
    out_len_expr <- init_rhs[[2]]
    if (!tcc_quick_validate_len_expr(out_len_expr, decl)) {
        return(NULL)
    }

    vector_double_args <- names(Filter(
        function(x) identical(x$mode, "double") && !isTRUE(x$is_scalar),
        decl$args
    ))

    chain <- tcc_quick_parse_for_chain(s2)
    if (is.null(chain) || length(chain$loops) < 1L) {
        return(NULL)
    }

    for (lp in chain$loops) {
        if (!identical(lp$kind, "seq_along")) {
            return(NULL)
        }
        if (!lp$target %in% vector_double_args) {
            return(NULL)
        }
    }
    loop_vars <- vapply(chain$loops, function(x) x$var, character(1))
    if (anyDuplicated(loop_vars) > 0L) {
        return(NULL)
    }

    upd <- chain$terminal
    if (!tcc_quick_is_call_sym(upd, "<-") || length(upd) != 3L) {
        return(NULL)
    }

    lhs_sub <- tcc_quick_parse_subset(upd[[2]])
    if (is.null(lhs_sub) || !identical(lhs_sub$arr, out_name)) {
        return(NULL)
    }

    # Reject kernel if any boundary call appears in indexing or RHS.
    if (tcc_quick_expr_has_boundary(lhs_sub$idx)) {
        return(NULL)
    }
    rhs <- upd[[3]]
    if (tcc_quick_expr_has_boundary(rhs)) {
        return(NULL)
    }

    arr_refs <- unique(c(
        tcc_quick_collect_subset_arrays(lhs_sub$idx),
        tcc_quick_collect_subset_arrays(rhs)
    ))
    arr_refs <- setdiff(arr_refs, out_name)
    if (length(arr_refs) > 0L && !all(arr_refs %in% vector_double_args)) {
        return(NULL)
    }

    input_arrays <- unique(c(vapply(chain$loops, function(x) x$target, character(1)), arr_refs))
    input_arrays <- setdiff(input_arrays, out_name)
    if (length(input_arrays) < 1L) {
        return(NULL)
    }

    if (!is.symbol(s3) || !identical(as.character(s3), out_name)) {
        return(NULL)
    }

    list(
        tag = "nested_loop_kernel",
        out = out_name,
        out_len_expr = out_len_expr,
        input_arrays = input_arrays,
        loop_vars = loop_vars,
        loops = chain$loops,
        out_idx = lhs_sub$idx,
        rhs = rhs
    )
}

tcc_quick_lower_block <- function(exprs, decl) {
    if (length(exprs) < 1L) {
        return(tcc_quick_fallback_ir("Empty body after declare()"))
    }

    locals <- list()
    if (length(exprs) > 1L) {
        for (i in seq_len(length(exprs) - 1L)) {
            s <- exprs[[i]]
            if (!is.call(s) || !is.symbol(s[[1]]) || as.character(s[[1]]) != "<-") {
                return(tcc_quick_fallback_ir("Only <- assignments are supported before final expression"))
            }
            if (length(s) != 3L || !is.symbol(s[[2]])) {
                return(tcc_quick_fallback_ir("Malformed <- assignment in block"))
            }
            name <- as.character(s[[2]])
            rhs <- tcc_quick_lower_expr(s[[3]], decl, locals)
            if (!isTRUE(rhs$ok)) {
                if (!is.null(rhs$boundary)) {
                    return(tcc_quick_fallback_ir(
                        paste0(
                            "Boundary call encountered (",
                            rhs$boundary,
                            ") - prefer Rf_lang*/Rf_eval path here"
                        )
                    ))
                }
                reason <- rhs$reason
                if (is.null(reason)) {
                    reason <- "Unsupported assignment RHS"
                }
                return(tcc_quick_fallback_ir(reason))
            }
            locals[[name]] <- list(node = rhs$node, mode = rhs$mode)
        }
    }

    lowered <- tcc_quick_lower_expr(exprs[[length(exprs)]], decl, locals)
    if (!isTRUE(lowered$ok)) {
        if (!is.null(lowered$boundary)) {
            return(tcc_quick_fallback_ir(
                paste0(
                    "Boundary call encountered (",
                    lowered$boundary,
                    ") - prefer Rf_lang*/Rf_eval path here"
                )
            ))
        }
        reason <- lowered$reason
        if (is.null(reason)) {
            reason <- "Unsupported expression"
        }
        return(tcc_quick_fallback_ir(reason))
    }

    if (identical(lowered$node$tag, "rf_lang_call")) {
        return(lowered$node)
    }

    if (identical(lowered$node$tag, "switch_num")) {
        return(list(tag = "switch_num_expr", switch = lowered$node, arg_modes = vapply(decl$args[decl$formal_names], function(x) x$mode, character(1))))
    }

    if (!lowered$mode %in% c("double", "integer", "logical")) {
        return(tcc_quick_fallback_ir("Lowered expression has unsupported result type"))
    }

    list(
        tag = "scalar_expr",
        expr = lowered$node,
        result_mode = lowered$mode,
        arg_modes = vapply(decl$args[decl$formal_names], function(x) x$mode, character(1))
    )
}

tcc_quick_lower <- function(fn, decl) {
    exprs <- tcc_quick_body_without_declare(fn)

    kernel <- tcc_quick_try_lower_nested_loop_kernel(exprs, decl)
    if (!is.null(kernel)) {
        return(kernel)
    }

    tcc_quick_lower_block(exprs, decl)
}
