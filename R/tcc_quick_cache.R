# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_quick_cache_env <- new.env(parent = emptyenv())

tcc_quick_cache_key <- function(fn, decl) {
    body_txt <- paste(deparse(body(fn), width.cutoff = 500L), collapse = "\n")
    formals_txt <- paste(deparse(formals(fn), width.cutoff = 500L), collapse = "\n")
    decl_txt <- paste(capture.output(str(decl)), collapse = "\n")
    paste(body_txt, formals_txt, decl_txt, sep = "\n----\n")
}

tcc_quick_cache_get <- function(key) {
    if (!exists(key, envir = tcc_quick_cache_env, inherits = FALSE)) {
        return(NULL)
    }
    get(key, envir = tcc_quick_cache_env, inherits = FALSE)
}

tcc_quick_cache_set <- function(key, value) {
    assign(key, value, envir = tcc_quick_cache_env)
    invisible(value)
}
