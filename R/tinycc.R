# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

#' Get the address of an external pointer
#'
#' Extract the memory address from an external pointer as a numeric value.
#' This is primarily useful for debugging and inspection purposes.
#'
#' @param ptr An external pointer object (e.g., from `tcc_get_symbol()`).
#' @return The memory address as a numeric value.
#' @export
get_external_ptr_addr <- function(ptr) {
  .Call(RC_get_external_ptr_addr, ptr)
}

#' TinyCC paths
#'
#' Helpers to locate the bundled tinycc installation after the package is installed.
#' @return A character scalar path.
#' @export
tcc_prefix <- function() {
  system.file("tinycc", package = "Rtinycc")
}

#' @rdname tcc_prefix
#' @export
tcc_lib_path <- function() file.path(tcc_prefix(), "lib")

#' @rdname tcc_prefix
#' @export
tcc_lib_paths <- function() {
  prefix <- tcc_prefix()
  # lib/tcc must come first: the C layer passes the first entry to

  # tcc_set_lib_path(), and TCC resolves its own headers as
  # {lib_path}/include (e.g. stdbool.h, stddef.h).
  paths <- c(file.path(prefix, "lib", "tcc"), file.path(prefix, "lib"))
  normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = FALSE)
}

#' @rdname tcc_prefix
#' @export
tcc_include_path <- function() file.path(tcc_prefix(), "include")

#' @rdname tcc_prefix
#' @export
tcc_bin_path <- function() file.path(tcc_prefix(), "bin")

#' @rdname tcc_prefix
#' @export
tcc_cli <- function() {
  exe <- if (.Platform$OS.type == "windows") "tcc.exe" else "tcc"
  candidates <- c(file.path(tcc_bin_path(), exe), file.path(tcc_prefix(), exe))
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(existing[[1L]])
  }
  candidates[[1L]]
}

#' Locate the TinyCC executable
#'
#' Returns the platform-specific `tcc` binary path (or `tcc.exe` on Windows), preferring the bundled installation.
#' @return A character scalar path.
#' @export
tcc_path <- function() {
  normalizePath(tcc_cli(), winslash = "/", mustWork = FALSE)
}

#' TinyCC include search paths
#'
#' Returns the include directories used by the bundled TinyCC (top-level include and lib/tcc/include).
#' @return A character vector of include directories.
#' @export
tcc_include_paths <- function() {
  prefix <- tcc_prefix()
  paths <- c(
    file.path(prefix, "include"),
    file.path(prefix, "lib", "tcc", "include")
  )
  if (.Platform$OS.type == "windows") {
    paths <- c(paths, file.path(prefix, "include", "winapi"))
  }
  normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = FALSE)
}

#' @rdname tcc_include_paths
#' @export
tcc_sysinclude_paths <- tcc_include_paths

tcc_parse_runtime_lib_path <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NA_character_)
  }
  s <- as.character(x[[1]])
  if (!nzchar(s) || is.na(s)) {
    return(NA_character_)
  }
  s <- trimws(sub(";.*$", "", s))
  if (!nzchar(s) || identical(s, "NA")) {
    return(NA_character_)
  }
  normalizePath(s, winslash = "/", mustWork = FALSE)
}

#' Report active BLAS/LAPACK runtime information from R
#'
#' Returns the BLAS/LAPACK runtime details as reported by R itself, plus
#' convenience flags indicating whether `Rblas` and `Rlapack` appear available
#' in loaded DLLs/shared objects.
#'
#' @return A named list with fields:
#'   `blas_path`, `lapack_path`, `has_rblas`, `has_rlapack`, `loaded_dlls`.
#' @export
blas_lapack_info <- function() {
  si <- utils::sessionInfo()
  loaded <- getLoadedDLLs()
  loaded_names <- names(loaded)
  loaded_paths <- vapply(
    loaded,
    function(x) {
      p <- x[["path"]]
      if (is.null(p) || !length(p)) "" else as.character(p[[1]])
    },
    character(1)
  )

  blas_path <- tcc_parse_runtime_lib_path(si$BLAS)
  lapack_path <- tryCatch(La_library(), error = function(e) NA_character_)
  lapack_path <- tcc_parse_runtime_lib_path(lapack_path)
  if (is.na(lapack_path) || !nzchar(lapack_path)) {
    lapack_path <- tcc_parse_runtime_lib_path(si$LAPACK)
  }

  path_has_name <- function(path, name) {
    if (
      !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)
    ) {
      return(FALSE)
    }
    grepl(name, basename(path), ignore.case = TRUE)
  }
  loaded_has_name <- function(name) {
    any(grepl(name, loaded_names, ignore.case = TRUE)) ||
      any(grepl(name, basename(loaded_paths), ignore.case = TRUE))
  }

  has_rblas <- loaded_has_name("Rblas") || path_has_name(blas_path, "Rblas")
  has_rlapack <- loaded_has_name("Rlapack") ||
    path_has_name(lapack_path, "Rlapack")

  list(
    blas_path = blas_path,
    lapack_path = lapack_path,
    has_rblas = isTRUE(has_rblas),
    has_rlapack = isTRUE(has_rlapack),
    loaded_dlls = loaded_names
  )
}

tcc_output_type <- function(output) {
  output <- match.arg(output, c("memory", "obj", "dll", "exe", "preprocess"))
  tcc_output_type_rule(output)
}

check_cli_exists <- function() {
  path <- tcc_path()
  if (!nzchar(path) || !file.exists(path)) {
    stop("tinycc CLI not found at ", path, call. = FALSE)
  }
  path
}

#' Create a libtcc state
#'
#' Initialize a libtcc compilation state, optionally pointing at the bundled include/lib paths.
#' @param output Output type: one of "memory", "obj", "dll", "exe", "preprocess".
#' @param include_path Path(s) to headers; defaults to the bundled include dirs.
#' @param lib_path Path(s) to libraries; defaults to the bundled lib dirs (lib and lib/tcc).
#' @return An external pointer of class `tcc_state`.
#' @export
tcc_state <- function(
  output = c("memory", "obj", "dll", "exe", "preprocess"),
  include_path = tcc_include_paths(),
  lib_path = tcc_lib_paths()
) {
  .Call(
    RC_libtcc_state_new,
    normalizePath(lib_path, winslash = "/", mustWork = FALSE),
    normalizePath(include_path, winslash = "/", mustWork = FALSE),
    tcc_output_type(output)
  )
}

#' Add a source file to a libtcc state
#' @param state A `tcc_state`.
#' @param path Path to a C source file.
#' @return Integer status code (0 = success).
#' @export
tcc_add_file <- function(state, path) {
  .Call(
    RC_libtcc_add_file,
    state,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
}

#' Add an include path to a libtcc state
#' @param state A `tcc_state`.
#' @param path Path to include directory.
#' @return Integer status code (0 = success).
#' @export
tcc_add_include_path <- function(state, path) {
  .Call(
    RC_libtcc_add_include_path,
    state,
    normalizePath(path, winslash = "/", mustWork = FALSE)
  )
}

#' Add a system include path to a libtcc state
#' @param state A `tcc_state`.
#' @param path Path to system include directory.
#' @return Integer status code (0 = success).
#' @export
tcc_add_sysinclude_path <- function(state, path) {
  .Call(
    RC_libtcc_add_sysinclude_path,
    state,
    normalizePath(path, winslash = "/", mustWork = FALSE)
  )
}

#' Add a library path to a libtcc state
#' @param state A `tcc_state`.
#' @param path Path to library directory.
#' @return Integer status code (0 = success).
#' @export
tcc_add_library_path <- function(state, path) {
  .Call(
    RC_libtcc_add_library_path,
    state,
    normalizePath(path, winslash = "/", mustWork = FALSE)
  )
}

#' Add a library to a libtcc state
#' @param state A `tcc_state`.
#' @param library Library name (e.g., "m" for libm, "R" for libR).
#' @return Integer status code (0 = success).
#' @export
tcc_add_library <- function(state, library) {
  .Call(RC_libtcc_add_library, state, library)
}

#' Apply raw TinyCC options to a libtcc state
#'
#' Passes options directly to `tcc_set_options()` for the given state.
#'
#' @param state A `tcc_state`.
#' @param options Character scalar of options (for example `"-O2 -Wall"`).
#' @return Integer status code (`0` on success; negative on parse error).
#' @export
tcc_set_options <- function(state, options) {
  if (
    !is.character(options) || length(options) != 1 || !nzchar(trimws(options))
  ) {
    stop("`options` must be a non-empty character scalar", call. = FALSE)
  }
  .Call(RC_libtcc_set_options, state, options)
}

#' Compile C code from a character string
#' @param state A `tcc_state`.
#' @param code C source code string.
#' @return Integer status code (0 = success).
#' @export
tcc_compile_string <- function(state, code) {
  .Call(RC_libtcc_compile_string, state, code)
}

#' Relocate compiled code
#' @param state A `tcc_state`.
#' @return Integer status code (0 = success).
#' @export
tcc_relocate <- function(state) {
  .Call(RC_libtcc_relocate, state)
}

#' Add a symbol to a libtcc state
#' @param state A `tcc_state`.
#' @param name Symbol name.
#' @param addr External pointer address or symbol value.
#' @return Integer status code (0 = success).
#' @export
tcc_add_symbol <- function(state, name, addr) {
  .Call(RC_libtcc_add_symbol, state, name, addr)
}

#' Get a symbol pointer from a libtcc state
#' @param state A `tcc_state`.
#' @param name Symbol name to look up.
#' @return External pointer of class `tcc_symbol`.
#' @export
tcc_get_symbol <- function(state, name) {
  .Call(RC_libtcc_get_symbol, state, name)
}

#' List symbols known to a libtcc state
#'
#' Return the global symbols currently reported by libtcc for a state. This is
#' a best-effort symbol-table inspection helper for compiled/linked TCC states,
#' not a portable exhaustive symbol enumerator, not a DLL export scanner, and
#' not a C signature discovery API. Platform backends may omit symbols that are
#' still resolvable with [tcc_get_symbol()]. For meaningful runtime addresses,
#' call it after [tcc_relocate()].
#'
#' @param state A `tcc_state`.
#' @return A data frame with columns `name` and `address`, where `address` is a
#'   hexadecimal character string.
#' @export
tcc_list_symbols <- function(state) {
  .Call(RC_libtcc_list_symbols, state)
}


#' Call a symbol from a TinyCC state
#'
#' With no additional arguments, `tcc_call_symbol()` preserves its historical
#' quick-test behavior: call a zero-argument symbol and box an `int`, `double`,
#' or `void` return value.
#'
#' With additional arguments, it uses an R `.C()`-style calling convention: the
#' target C function must be `void`, each atomic or character R argument is
#' copied to guarded mutable call storage and passed by pointer, and the result
#' is a list of the modified argument values. Supported argument mappings follow
#' R's `.C()` interface: raw as `unsigned char *`, integer/logical as `int *`,
#' numeric as `double *` or `float *` when `attr(x, "Csingle")` is true,
#' complex as `Rcomplex *`, character as `char **`, lists as read-only `SEXP *`,
#' and functions/environments/other R objects as read-only `SEXP`. Up to 65
#' arguments are supported. Guard bytes around copied buffers are checked after
#' the call to catch simple native underwrites and overwrites; character code may
#' edit string contents in place but must not replace `char *` elements in the
#' `char **` array.
#'
#' Non-atomic R objects are borrowed for the duration of the call only. C code
#' must not mutate them through this interface, and must call `R_PreserveObject()`
#' if it deliberately stores a `SEXP` beyond the call. This is a low-level
#' convenience interface; for typed scalar returns, explicit zero-copy arrays,
#' ownership metadata, and clearer signatures, prefer [tcc_ffi()].
#'
#' @param .state A `tcc_state`. Named `state =` is also accepted for
#'   compatibility with earlier releases.
#' @param .NAME Symbol name to call. Named `name =` is also accepted for
#'   compatibility with earlier releases.
#' @param ... Optional arguments for `.C()`-style pointer calls.
#' @param return One of `"int"`, `"double"`, or `"void"`. If `...` is present,
#'   the only supported value is `"void"`; when omitted with `...`, it defaults
#'   to `"void"`.
#' @param NAOK If `FALSE`, integer/logical `NA` and non-finite numeric/complex
#'   values are rejected before the call, matching `.C()`'s default safety
#'   check. If `TRUE`, those values are passed through.
#' @return For zero-argument scalar calls, the boxed return value (`NULL` for
#'   `void`). For `.C()`-style calls, a list mirroring `...` with any C-side
#'   modifications copied back.
#' @export
tcc_call_symbol <- function(.state, .NAME, ..., return = c("int", "double", "void"), NAOK = FALSE) {
  args <- list(...)

  if (missing(.state)) {
    if (!is.null(args$state)) {
      .state <- args$state
      args$state <- NULL
    } else {
      stop("argument '.state' is missing", call. = FALSE)
    }
  }
  if (missing(.NAME)) {
    if (!is.null(args$name)) {
      .NAME <- args$name
      args$name <- NULL
    } else {
      stop("argument '.NAME' is missing", call. = FALSE)
    }
  }

  if (missing(return) && length(args) == 1L && is.character(args[[1L]]) &&
      length(args[[1L]]) == 1L && args[[1L]] %in% c("int", "double", "void") &&
      is.null(names(args))) {
    return <- args[[1L]]
    args <- list()
  } else if (missing(return) && length(args) > 0L) {
    return <- "void"
  } else {
    return <- match.arg(return)
  }

  if (length(args) > 0L && !identical(return, "void")) {
    stop("tcc_call_symbol() with arguments uses .C-style void functions", call. = FALSE)
  }
  if (!isTRUE(NAOK) && !identical(NAOK, FALSE)) {
    stop("NAOK must be TRUE or FALSE", call. = FALSE)
  }

  .Call(RC_libtcc_call_symbol, .state, .NAME, return, args, isTRUE(NAOK))
}

#' Check if a tcc_symbol external pointer is valid
#' @param ptr External pointer from `tcc_get_symbol()`.
#' @return TRUE if the pointer address is non-null, FALSE otherwise.
#' @export
tcc_symbol_is_valid <- function(ptr) {
  .Call(RC_libtcc_ptr_valid, ptr)
}

#' Run the tinycc CLI
#' @param args Character vector of CLI arguments (e.g., `c("-c", file, "-o", out)`).
#' @param tcc_path Optional path to the `tcc` binary; defaults to the bundled CLI.
#' @return Integer status from `system2()`.
#' @export
tcc_run_cli <- function(args = character(), tcc_path = check_cli_exists()) {
  tcc_path <- normalizePath(tcc_path, winslash = "/", mustWork = TRUE)
  args <- as.character(args)
  lib_paths <- normalizePath(tcc_lib_paths(), winslash = "/", mustWork = FALSE)
  env <- character()
  if (length(lib_paths)) {
    sysname <- Sys.info()[["sysname"]]
    env_key <- tcc_loader_env_key(sysname)
    env_sep <- tcc_loader_env_sep(sysname)
    env <- sprintf(
      "%s=%s",
      env_key,
      paste(c(lib_paths, Sys.getenv(env_key)), collapse = env_sep)
    )
  }
  res <- system2(tcc_path, args, env = env)
  as.integer(res)
}
