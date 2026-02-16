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

tcc_quick_codegen <- function(ir, decl, fn_name = "tcc_quick_entry") {
    if (identical(ir$tag, "rf_lang3_call")) {
        arg_list <- decl$formal_names
        c_args <- paste(sprintf("SEXP %s", arg_list), collapse = ", ")
        return(paste(
            "#include <R.h>",
            "#include <Rinternals.h>",
            "",
            sprintf("SEXP %s(%s) {", fn_name, c_args),
            sprintf("  SEXP call = PROTECT(Rf_lang3(Rf_install(\"%s\"), %s, %s));", ir$fun, ir$arg1, ir$arg2),
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

    if (!identical(ir$tag, "scalar_binop")) {
        stop("Codegen only supports scalar_binop/rf_lang3_call/internal_call2 IR in MVP", call. = FALSE)
    }

    arg_list <- decl$formal_names
    c_args <- paste(sprintf("SEXP %s", arg_list), collapse = ", ")

    extract_lines <- vapply(
        arg_list,
        function(a) tcc_quick_codegen_arg_extract(a, ir$arg_modes[[a]]),
        character(1)
    )

    lhs <- sprintf("%s_", ir$lhs)
    rhs <- sprintf("%s_", ir$rhs)
    expr <- sprintf("(%s %s %s)", lhs, ir$op, rhs)

    ret_line <- if (ir$result_mode == "double") {
        sprintf("return Rf_ScalarReal((double)%s);", expr)
    } else {
        sprintf("return Rf_ScalarInteger((int)%s);", expr)
    }

    paste(
        "#include <R.h>",
        "#include <Rinternals.h>",
        "",
        sprintf("SEXP %s(%s) {", fn_name, c_args),
        paste0("  ", extract_lines, collapse = "\n"),
        paste0("  ", ret_line),
        "}",
        sep = "\n"
    )
}
