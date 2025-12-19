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
  paths <- c(file.path(prefix, "lib"), file.path(prefix, "lib", "tcc"))
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
  if (length(existing)) return(existing[[1L]])
  candidates[[1L]]
}

#' Locate the TinyCC executable
#'
#' Returns the platform-specific `tcc` binary path (or `tcc.exe` on Windows), preferring the bundled installation.
#' @return A character scalar path.
#' @export
tcc_path <- function() normalizePath(tcc_cli(), winslash = "/", mustWork = FALSE)

#' TinyCC include search paths
#'
#' Returns the include directories used by the bundled TinyCC (top-level include and lib/tcc/include).
#' @return A character vector of include directories.
#' @export
tcc_include_paths <- function() {
  prefix <- tcc_prefix()
  paths <- c(file.path(prefix, "include"), file.path(prefix, "lib", "tcc", "include"))
  normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = FALSE)
}

tcc_output_type <- function(output) {
  output <- match.arg(output, c("memory", "obj", "dll", "exe", "preprocess"))
  switch(output,
         memory = 1L,  # TCC_OUTPUT_MEMORY
         obj = 3L,     # TCC_OUTPUT_OBJ
         dll = 4L,     # TCC_OUTPUT_DLL
         exe = 2L,     # TCC_OUTPUT_EXE
         preprocess = 5L) # TCC_OUTPUT_PREPROCESS
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
#' @param include_path Path to headers; defaults to the bundled include dir.
#' @param include_path Path(s) to headers; defaults to the bundled include dirs.
#' @param lib_path Path(s) to libraries; defaults to the bundled lib dirs (lib and lib/tcc).
#' @return An external pointer of class `tcc_state`.
#' @export
tcc_state <- function(output = c("memory", "obj", "dll", "exe", "preprocess"),
                      include_path = tcc_include_paths(),
                      lib_path = tcc_lib_paths()) {
  .Call(RC_libtcc_state_new, normalizePath(lib_path, winslash = "/", mustWork = FALSE),
        normalizePath(include_path, winslash = "/", mustWork = FALSE),
        tcc_output_type(output))
}

#' Add a source file to a libtcc state
#' @param state A `tcc_state`.
#' @param path Path to a C source file.
#' @return Integer status code (0 = success).
#' @export
tcc_add_file <- function(state, path) {
  .Call(RC_libtcc_add_file, state, normalizePath(path, winslash = "/", mustWork = TRUE))
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

#' Get a symbol pointer from a libtcc state
#' @param state A `tcc_state`.
#' @param name Symbol name to look up.
#' @return External pointer of class `tcc_symbol`.
#' @export
tcc_get_symbol <- function(state, name) {
  .Call(RC_libtcc_get_symbol, state, name)
}

#' Call a zero-argument symbol returning int from a libtcc state
#' @param state A `tcc_state`.
#' @param name Symbol name to call.
#' @return Integer return value from the symbol.
#' @export
tcc_call_symbol_int <- function(state, name) {
  .Call(RC_libtcc_call_symbol, state, name)
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
  res <- system2(tcc_path, args)
  as.integer(res)
}
