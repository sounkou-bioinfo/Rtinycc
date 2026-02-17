# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_quick_extract_exprs <- function(fn) {
    b <- body(fn)
    if (is.call(b) && identical(b[[1]], quote(`{`))) {
        return(as.list(b)[-1])
    }
    list(b)
}

tcc_quick_parse_dim <- function(x) {
    if (is.symbol(x) && identical(as.character(x), "NA")) {
        return(NA_integer_)
    }
    if (length(x) == 1 && is.logical(x) && is.na(x)) {
        return(NA_integer_)
    }
    if (length(x) == 1 && (is.integer(x) || is.double(x))) {
        out <- as.integer(x)
        if (is.na(out) || out < 1L) {
            stop("declare() dimensions must be >= 1 or NA", call. = FALSE)
        }
        return(out)
    }
    stop("Unsupported declare() dimension", call. = FALSE)
}

tcc_quick_parse_type_spec <- function(spec) {
    if (!is.call(spec)) {
        stop("declare(type(...)) entries must be calls", call. = FALSE)
    }

    type_name <- as.character(spec[[1]])
    type_name <- switch(type_name,
        numeric = "double",
        type_name
    )

    if (!type_name %in% c("double", "integer", "logical")) {
        stop(
            "tcc_quick current subset supports only double/integer/logical declarations",
            call. = FALSE
        )
    }

    dims_expr <- as.list(spec)[-1]
    if (length(dims_expr) == 0) {
        stop("declare() type must include dimensions", call. = FALSE)
    }

    dims <- vapply(dims_expr, tcc_quick_parse_dim, integer(1))

    list(
        mode = type_name,
        dims = dims,
        is_scalar = length(dims) == 1L && !is.na(dims[[1]]) && dims[[1]] == 1L
    )
}

tcc_quick_parse_declare <- function(fn) {
    exprs <- tcc_quick_extract_exprs(fn)
    if (length(exprs) < 1L) {
        stop("Function body is empty", call. = FALSE)
    }

    d <- exprs[[1]]
    if (!(is.call(d) && identical(d[[1]], quote(declare)))) {
        stop(
            "tcc_quick requires declare(type(...)) as the first expression",
            call. = FALSE
        )
    }

    decl_items <- as.list(d)[-1]
    if (length(decl_items) == 0L) {
        stop("declare() must contain at least one type() entry", call. = FALSE)
    }

    arg_decl <- list()
    for (entry in decl_items) {
        if (!(is.call(entry) && identical(entry[[1]], quote(type)))) {
            stop("declare() currently supports only type(...) entries", call. = FALSE)
        }
        vars <- as.list(entry)[-1]
        vnames <- names(vars)
        if (length(vars) == 0L || any(!nzchar(vnames))) {
            stop("type(...) entries must be named", call. = FALSE)
        }
        for (i in seq_along(vars)) {
            arg_decl[[vnames[[i]]]] <- tcc_quick_parse_type_spec(vars[[i]])
        }
    }

    formal_names <- names(formals(fn))
    if (!all(formal_names %in% names(arg_decl))) {
        miss <- setdiff(formal_names, names(arg_decl))
        stop(
            "Missing declare(type(...)) for arguments: ",
            paste(miss, collapse = ", "),
            call. = FALSE
        )
    }

    list(
        args = arg_decl,
        formal_names = formal_names
    )
}
