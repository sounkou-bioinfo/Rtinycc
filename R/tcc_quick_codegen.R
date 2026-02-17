# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_quick_codegen_arg_extract <- function(arg_name, mode) {
    if (mode == "double") {
        return(sprintf("double %s_ = Rf_asReal(%s);", arg_name, arg_name))
    }
    if (mode == "integer") {
        return(sprintf("int %s_ = Rf_asInteger(%s);", arg_name, arg_name))
    }
    stop("Unsupported argument mode in codegen", call. = FALSE)
}

tcc_quick_codegen_node <- function(node) {
    if (identical(node$tag, "var")) {
        return(sprintf("%s_", node$name))
    }

    if (identical(node$tag, "const")) {
        if (node$mode == "integer") {
            return(sprintf("%d", as.integer(node$value)))
        }
        if (node$mode == "double") {
            return(format(as.double(node$value), scientific = FALSE, trim = TRUE))
        }
        if (node$mode == "logical") {
            return(if (isTRUE(node$value)) "1" else "0")
        }
        stop("Unsupported const mode", call. = FALSE)
    }

    if (identical(node$tag, "unary")) {
        x <- tcc_quick_codegen_node(node$x)
        if (node$op == "!") {
            return(sprintf("(!( %s ))", x))
        }
        return(sprintf("(%s(%s))", node$op, x))
    }

    if (identical(node$tag, "call1")) {
        x <- tcc_quick_codegen_node(node$x)
        fun <- switch(node$fun,
            abs = "fabs",
            ceiling = "ceil",
            trunc = "trunc",
            node$fun
        )
        return(sprintf("(%s((double)(%s)))", fun, x))
    }

    if (identical(node$tag, "binary")) {
        lhs <- tcc_quick_codegen_node(node$lhs)
        rhs <- tcc_quick_codegen_node(node$rhs)
        op <- node$op
        if (op == "%/%") {
            return(sprintf("(floor((double)(%s) / (double)(%s)))", lhs, rhs))
        }
        if (op == "%%") {
            return(sprintf("(fmod((double)(%s), (double)(%s)))", lhs, rhs))
        }
        if (op == "^") {
            return(sprintf("(pow((double)(%s), (double)(%s)))", lhs, rhs))
        }
        if (op == "&&") op <- "&&"
        if (op == "||") op <- "||"
        if (op == "&") op <- "&&"
        if (op == "|") op <- "||"
        return(sprintf("((%s) %s (%s))", lhs, op, rhs))
    }

    if (identical(node$tag, "if")) {
        cond <- tcc_quick_codegen_node(node$cond)
        yes <- tcc_quick_codegen_node(node$yes)
        no <- tcc_quick_codegen_node(node$no)
        return(sprintf("((%s) ? (%s) : (%s))", cond, yes, no))
    }

    if (identical(node$tag, "switch_num")) {
        sel <- tcc_quick_codegen_node(node$selector)
        n <- length(node$cases)
        out <- "R_NilValue"
        for (i in seq.int(n, 1L)) {
            case_i <- tcc_quick_codegen_node(node$cases[[i]])
            out <- sprintf("((((int)(%s)) == %d) ? (%s) : (%s))", sel, i, case_i, out)
        }
        return(out)
    }

    stop("Unsupported expression node in codegen", call. = FALSE)
}

tcc_quick_c_ident <- function(x) {
    y <- gsub("[^A-Za-z0-9_]", "_", x)
    if (grepl("^[0-9]", y)) {
        y <- paste0("v_", y)
    }
    y
}

tcc_quick_codegen_len_expr <- function(e, len_by_array) {
    if (length(e) == 1L && is.integer(e)) {
        return(sprintf("%d", as.integer(e)[[1]]))
    }
    if (length(e) == 1L && is.double(e)) {
        return(format(as.double(e)[[1]], scientific = FALSE, trim = TRUE))
    }

    if (!is.call(e) || !is.symbol(e[[1]])) {
        stop("Unsupported output-length expression node", call. = FALSE)
    }

    fn <- as.character(e[[1]])
    if (fn == "(") {
        if (length(e) != 2L) {
            stop("Malformed parenthesized length expression", call. = FALSE)
        }
        inner <- tcc_quick_codegen_len_expr(e[[2]], len_by_array)
        return(sprintf("(%s)", inner))
    }

    if (fn == "length") {
        if (length(e) != 2L || !is.symbol(e[[2]])) {
            stop("Malformed length() in output-length expression", call. = FALSE)
        }
        nm <- as.character(e[[2]])
        if (!nm %in% names(len_by_array)) {
            stop("Unknown symbol in output-length expression: ", nm, call. = FALSE)
        }
        return(len_by_array[[nm]])
    }

    if (fn %in% c("+", "-") && length(e) == 2L) {
        x <- tcc_quick_codegen_len_expr(e[[2]], len_by_array)
        return(sprintf("(%s(%s))", fn, x))
    }

    if (fn %in% c("+", "-", "*", "/") && length(e) == 3L) {
        lhs <- tcc_quick_codegen_len_expr(e[[2]], len_by_array)
        rhs <- tcc_quick_codegen_len_expr(e[[3]], len_by_array)
        return(sprintf("((%s) %s (%s))", lhs, fn, rhs))
    }

    stop("Unsupported output-length expression operator: ", fn, call. = FALSE)
}

tcc_quick_codegen_kernel_index_expr <- function(e, loop_var_c) {
    if (is.symbol(e)) {
        nm <- as.character(e)
        if (nm %in% names(loop_var_c)) {
            return(loop_var_c[[nm]])
        }
        stop("Unsupported symbol in kernel index expression: ", nm, call. = FALSE)
    }

    if (length(e) == 1L && is.integer(e)) {
        return(sprintf("%d", as.integer(e)[[1]]))
    }
    if (length(e) == 1L && is.double(e)) {
        v <- as.double(e)[[1]]
        if (!isTRUE(all.equal(v, round(v)))) {
            stop("Non-integer numeric constant in index expression", call. = FALSE)
        }
        return(sprintf("%d", as.integer(round(v))))
    }

    if (!is.call(e) || !is.symbol(e[[1]])) {
        stop("Unsupported kernel index expression node", call. = FALSE)
    }

    fn <- as.character(e[[1]])
    if (fn == "(") {
        if (length(e) != 2L) {
            stop("Malformed parenthesized index expression", call. = FALSE)
        }
        inner <- tcc_quick_codegen_kernel_index_expr(e[[2]], loop_var_c)
        return(sprintf("(%s)", inner))
    }

    if (fn %in% c("+", "-") && length(e) == 2L) {
        x <- tcc_quick_codegen_kernel_index_expr(e[[2]], loop_var_c)
        return(sprintf("(%s(%s))", fn, x))
    }

    if (fn %in% c("+", "-", "*") && length(e) == 3L) {
        lhs <- tcc_quick_codegen_kernel_index_expr(e[[2]], loop_var_c)
        rhs <- tcc_quick_codegen_kernel_index_expr(e[[3]], loop_var_c)
        return(sprintf("((%s) %s (%s))", lhs, fn, rhs))
    }

    stop("Unsupported operator in kernel index expression: ", fn, call. = FALSE)
}

tcc_quick_codegen_kernel_expr <- function(e, loop_var_c, ptr_by_array) {
    if (is.symbol(e)) {
        nm <- as.character(e)
        if (nm %in% names(loop_var_c)) {
            return(sprintf("((double)%s)", loop_var_c[[nm]]))
        }
        stop("Unsupported symbol in kernel expression: ", nm, call. = FALSE)
    }

    if (length(e) == 1L && is.integer(e)) {
        return(sprintf("%d", as.integer(e)[[1]]))
    }
    if (length(e) == 1L && is.double(e)) {
        return(format(as.double(e)[[1]], scientific = FALSE, trim = TRUE))
    }

    if (!is.call(e) || !is.symbol(e[[1]])) {
        stop("Unsupported kernel expression node", call. = FALSE)
    }

    fn <- as.character(e[[1]])
    if (fn == "[") {
        if (length(e) != 3L || !is.symbol(e[[2]])) {
            stop("Unsupported subset in kernel expression", call. = FALSE)
        }
        arr <- as.character(e[[2]])
        idx <- tcc_quick_codegen_kernel_index_expr(e[[3]], loop_var_c)
        if (!arr %in% names(ptr_by_array)) {
            stop(paste0("Unknown array symbol in kernel expression: ", arr), call. = FALSE)
        }
        ptr <- ptr_by_array[[arr]]
        return(sprintf("%s[(R_xlen_t)((%s) - 1)]", ptr, idx))
    }

    if (fn %in% c("+", "-", "*", "/") && length(e) == 3L) {
        lhs <- tcc_quick_codegen_kernel_expr(e[[2]], loop_var_c, ptr_by_array)
        rhs <- tcc_quick_codegen_kernel_expr(e[[3]], loop_var_c, ptr_by_array)
        return(sprintf("((%s) %s (%s))", lhs, fn, rhs))
    }

    if (fn %in% c("+", "-") && length(e) == 2L) {
        x <- tcc_quick_codegen_kernel_expr(e[[2]], loop_var_c, ptr_by_array)
        return(sprintf("(%s(%s))", fn, x))
    }

    stop("Unsupported operator in kernel expression: ", fn, call. = FALSE)
}

tcc_quick_codegen <- function(ir, decl, fn_name = "tcc_quick_entry") {
    if (identical(ir$tag, "nested_loop_kernel")) {
        arg_list <- decl$formal_names
        c_args <- paste(sprintf("SEXP %s", arg_list), collapse = ", ")

        arr_c <- setNames(
            vapply(ir$input_arrays, function(a) tcc_quick_c_ident(a), character(1)),
            ir$input_arrays
        )
        sexp_var <- setNames(paste0(arr_c, "_"), names(arr_c))
        len_var <- setNames(paste0("n_", arr_c), names(arr_c))
        ptr_var <- setNames(paste0("x_", arr_c), names(arr_c))
        ptr_with_out <- c(ptr_var, setNames("xo", ir$out))

        loop_var_c <- setNames(
            vapply(ir$loop_vars, function(v) tcc_quick_c_ident(v), character(1)),
            ir$loop_vars
        )

        out_len <- tcc_quick_codegen_len_expr(ir$out_len_expr, len_var)
        out_idx <- tcc_quick_codegen_kernel_index_expr(ir$out_idx, loop_var_c)
        rhs <- tcc_quick_codegen_kernel_expr(ir$rhs, loop_var_c, ptr_with_out)

        coerce_lines <- vapply(
            names(sexp_var),
            function(a) sprintf("  SEXP %s = PROTECT(Rf_coerceVector(%s, REALSXP));", sexp_var[[a]], a),
            character(1)
        )
        len_lines <- vapply(
            names(len_var),
            function(a) sprintf("  R_xlen_t %s = XLENGTH(%s);", len_var[[a]], sexp_var[[a]]),
            character(1)
        )
        ptr_lines <- vapply(
            names(ptr_var),
            function(a) sprintf("  double *%s = REAL(%s);", ptr_var[[a]], sexp_var[[a]]),
            character(1)
        )

        loop_open <- vapply(
            ir$loops,
            function(lp) {
                if (!identical(lp$kind, "seq_along")) {
                    stop("Unsupported loop kind in kernel", call. = FALSE)
                }
                if (!lp$target %in% names(len_var)) {
                    stop("Unknown loop target in kernel: ", lp$target, call. = FALSE)
                }
                v <- loop_var_c[[lp$var]]
                lim <- len_var[[lp$target]]
                sprintf("  for (R_xlen_t %s = 1; %s <= %s; ++%s) {", v, v, lim, v)
            },
            character(1)
        )

        nprotect <- length(ir$input_arrays) + 1L

        return(paste(
            "#include <R.h>",
            "#include <Rinternals.h>",
            "",
            sprintf("SEXP %s(%s) {", fn_name, c_args),
            paste(coerce_lines, collapse = "\n"),
            paste(len_lines, collapse = "\n"),
            sprintf("  R_xlen_t n_out = (R_xlen_t)(%s);", out_len),
            "  SEXP out = PROTECT(Rf_allocVector(REALSXP, n_out));",
            paste(ptr_lines, collapse = "\n"),
            "  double *xo = REAL(out);",
            "  for (R_xlen_t k = 0; k < n_out; ++k) xo[k] = 0.0;",
            paste(loop_open, collapse = "\n"),
            sprintf("      xo[(R_xlen_t)((%s) - 1)] = %s;", out_idx, rhs),
            paste(rep("  }", length(ir$loops)), collapse = "\n"),
            sprintf("  UNPROTECT(%d);", nprotect),
            "  return out;",
            "}",
            sep = "\n"
        ))
    }

    if (identical(ir$tag, "rf_lang_call")) {
        arg_list <- decl$formal_names
        c_args <- paste(sprintf("SEXP %s", arg_list), collapse = ", ")
        call_ctor <- switch(as.character(length(ir$args)),
            `1` = sprintf("Rf_lang2(Rf_install(\"%s\"), %s)", ir$fun, ir$args[[1]]),
            `2` = sprintf("Rf_lang3(Rf_install(\"%s\"), %s, %s)", ir$fun, ir$args[[1]], ir$args[[2]]),
            `3` = sprintf("Rf_lang4(Rf_install(\"%s\"), %s, %s, %s)", ir$fun, ir$args[[1]], ir$args[[2]], ir$args[[3]]),
            `4` = sprintf("Rf_lang5(Rf_install(\"%s\"), %s, %s, %s, %s)", ir$fun, ir$args[[1]], ir$args[[2]], ir$args[[3]], ir$args[[4]]),
            NULL
        )
        if (is.null(call_ctor)) {
            stop("rf_lang_call supports 1..4 args", call. = FALSE)
        }
        return(paste(
            "#include <R.h>",
            "#include <Rinternals.h>",
            "",
            sprintf("SEXP %s(%s) {", fn_name, c_args),
            sprintf("  SEXP call = PROTECT(%s);", call_ctor),
            "  SEXP out = Rf_eval(call, R_BaseEnv);",
            "  UNPROTECT(1);",
            "  return out;",
            "}",
            sep = "\n"
        ))
    }

    if (identical(ir$tag, "internal_call2")) {
        arg_list <- decl$formal_names
        c_args <- paste(sprintf("SEXP %s", arg_list), collapse = ", ")
        return(paste(
            "#include <R.h>",
            "#include <Rinternals.h>",
            "",
            sprintf("SEXP %s(%s) {", fn_name, c_args),
            sprintf("  SEXP inner = PROTECT(Rf_lang3(Rf_install(\"%s\"), %s, %s));", ir$fun, ir$arg1, ir$arg2),
            "  SEXP call = PROTECT(Rf_lang2(Rf_install(\".Internal\"), inner));",
            "  SEXP out = Rf_eval(call, R_BaseEnv);",
            "  UNPROTECT(2);",
            "  return out;",
            "}",
            sep = "\n"
        ))
    }

    if (identical(ir$tag, "switch_num_expr")) {
        arg_list <- decl$formal_names
        c_args <- paste(sprintf("SEXP %s", arg_list), collapse = ", ")

        extract_lines <- vapply(
            arg_list,
            function(a) tcc_quick_codegen_arg_extract(a, ir$arg_modes[[a]]),
            character(1)
        )

        sw <- ir$switch
        sel <- tcc_quick_codegen_node(sw$selector)
        n <- length(sw$cases)

        case_lines <- character(0)
        for (i in seq_len(n)) {
            expr_i <- tcc_quick_codegen_node(sw$cases[[i]])
            ret_i <- if (sw$result_mode == "double") {
                sprintf("return Rf_ScalarReal((double)%s);", expr_i)
            } else if (sw$result_mode == "logical") {
                sprintf("return Rf_ScalarLogical(((%s) ? 1 : 0));", expr_i)
            } else {
                sprintf("return Rf_ScalarInteger((int)%s);", expr_i)
            }
            case_lines <- c(case_lines, sprintf("    case %d: %s", i, ret_i))
        }

        return(paste(
            "#include <R.h>",
            "#include <Rinternals.h>",
            "#include <math.h>",
            "",
            sprintf("SEXP %s(%s) {", fn_name, c_args),
            paste0("  ", extract_lines, collapse = "\n"),
            sprintf("  int tcc_sw_sel = (int)(%s);", sel),
            "  switch (tcc_sw_sel) {",
            paste(case_lines, collapse = "\n"),
            "    default: return R_NilValue;",
            "  }",
            "}",
            sep = "\n"
        ))
    }

    if (!identical(ir$tag, "scalar_expr")) {
        stop("Codegen only supports nested_loop_kernel/scalar_expr/rf_lang_call/internal_call2/switch_num_expr IR in current subset", call. = FALSE)
    }

    arg_list <- decl$formal_names
    c_args <- paste(sprintf("SEXP %s", arg_list), collapse = ", ")

    extract_lines <- vapply(
        arg_list,
        function(a) tcc_quick_codegen_arg_extract(a, ir$arg_modes[[a]]),
        character(1)
    )

    expr <- tcc_quick_codegen_node(ir$expr)

    ret_line <- if (ir$result_mode == "double") {
        sprintf("return Rf_ScalarReal((double)%s);", expr)
    } else if (ir$result_mode == "logical") {
        sprintf("return Rf_ScalarLogical(((%s) ? 1 : 0));", expr)
    } else {
        sprintf("return Rf_ScalarInteger((int)%s);", expr)
    }

    paste(
        "#include <R.h>",
        "#include <Rinternals.h>",
        "#include <math.h>",
        "",
        sprintf("SEXP %s(%s) {", fn_name, c_args),
        paste0("  ", extract_lines, collapse = "\n"),
        paste0("  ", ret_line),
        "}",
        sep = "\n"
    )
}
