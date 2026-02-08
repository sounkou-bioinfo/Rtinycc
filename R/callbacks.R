# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Callback Registration and Invocation
# Allow R functions to be passed as callbacks to compiled C code

#' Register an R function as a callback
#'
#' Wraps an R function so it can be passed as a C function pointer
#' to compiled code. The callback will be invoked via a trampoline
#' that marshals arguments between C and R.
#'
#' @details
#' Thread safety: callbacks are executed on the R main thread only. Invoking
#' a callback from a worker thread is unsupported and may crash R. The
#' \code{threadsafe} flag is currently informational only.
#'
#' If a callback raises an error, a warning is emitted and a type-appropriate
#' default value is returned.
#'
#' When binding callbacks with `tcc_bind()`, use a `callback:<signature>`
#' argument type so a synchronous trampoline is generated. The trampoline
#' expects a `void*` user-data pointer as its first argument; pass
#' `tcc_callback_ptr(cb)` as the user-data argument to the C API. For
#' thread-safe usage from worker threads, use `callback_async:<signature>`
#' which schedules the call on the main thread and returns a default value.
#'
#' Pointer arguments (e.g., \code{double*}, \code{int*}) are passed as
#' external pointers. Lengths must be supplied separately if needed.
#'
#' The return type may be any scalar type supported by the FFI mappings
#' (e.g., \code{i32}, \code{f64}, \code{bool}, \code{cstring}), or
#' \code{SEXP} to return an R object directly.
#'
#' @param fun An R function to be called from C
#' @param signature C function signature string (e.g., "double (*)(int, double)")
#' @param threadsafe Whether to enable thread-safe invocation (experimental)
#' @return A tcc_callback object (externalptr wrapper)
#' @export
tcc_callback <- function(fun, signature, threadsafe = FALSE) {
  if (!is.function(fun)) {
    stop("fun must be a function", call. = FALSE)
  }

  if (!is.character(signature) || length(signature) != 1) {
    stop("signature must be a single string", call. = FALSE)
  }

  # Parse signature to get return type and argument types
  sig <- parse_callback_signature(signature)

  # Register with C runtime
  callback_ext <- .Call(
    RC_register_callback,
    fun,
    sig$return_type,
    sig$arg_types,
    as.integer(threadsafe)
  )

  # Add metadata as attributes
  attr(callback_ext, "signature") <- sig
  attr(callback_ext, "threadsafe") <- threadsafe
  attr(callback_ext, "fun") <- fun
  class(callback_ext) <- c("tcc_callback", "externalptr")

  callback_ext
}

#' Close/unregister a callback
#'
#' Releases the preserved R function reference and cleans up resources.
#' Must be called when the callback is no longer needed.
#'
#' @param callback A tcc_callback object returned by tcc_callback()
#' @return NULL (invisible)
#' @export
tcc_callback_close <- function(callback) {
  if (!inherits(callback, "tcc_callback")) {
    stop("Expected tcc_callback object", call. = FALSE)
  }

  .Call(RC_unregister_callback, callback)
  invisible(NULL)
}

#' Get the C-compatible function pointer
#'
#' Returns an external pointer that can be passed to compiled C code
#' as user data for trampolines. Keep this handle (and the original
#' `tcc_callback`) alive for as long as C may call back.
#' The pointer handle keeps the underlying token alive until it is garbage
#' collected, even if the original callback is closed.
#'
#' Pointer arguments and return values are treated as external pointers.
#' Use \code{tcc_read_bytes()}, \code{tcc_read_u8()}, or \code{tcc_read_f64()}
#' to inspect pointed data when needed.
#'
#' @param callback A tcc_callback object
#' @return An external pointer (address of the callback token)
#' @export
tcc_callback_ptr <- function(callback) {
  if (!inherits(callback, "tcc_callback")) {
    stop("Expected tcc_callback object", call. = FALSE)
  }

  # Get the token address as external pointer
  .Call(RC_get_callback_ptr, callback)
}

#' Print tcc_callback object
#'
#' @param x A tcc_callback object
#' @param ... Ignored
#' @export
print.tcc_callback <- function(x, ...) {
  sig <- attr(x, "signature")
  threadsafe <- attr(x, "threadsafe")

  cat("<tcc_callback>\n")
  cat("  Signature: ", format_signature(sig), "\n", sep = "")
  cat("  Thread-safe: ", if (threadsafe) "yes" else "no", "\n", sep = "")
  cat(
    "  Status: ",
    if (is_callback_valid(x)) "valid" else "invalid",
    "\n",
    sep = ""
  )
  invisible(x)
}

#' Check if callback is still valid
#'
#' @param callback A tcc_callback object
#' @return Logical indicating if callback can be invoked
#' @export
tcc_callback_valid <- function(callback) {
  if (!inherits(callback, "tcc_callback")) {
    return(FALSE)
  }
  is_callback_valid(callback)
}

# Internal function to check validity
is_callback_valid <- function(callback) {
  tryCatch(
    {
      .Call(RC_callback_is_valid, callback)
    },
    error = function(e) FALSE
  )
}

# Parse a C function signature string
# Returns list with return_type and arg_types (character vector)
parse_callback_signature <- function(signature) {
  # Remove whitespace
  sig <- gsub("\\s+", " ", trimws(signature))

  # Pattern: return_type (*)(arg1, arg2, ...)
  # or:      return_type (*name)(arg1, arg2, ...)
  # or:      return_type (name*)(arg1, arg2, ...)

  # Try to match various signature patterns
  patterns <- list(
    # (*name)(args) or (*)(args)
    "^(.*?)\\s*\\(\\*\\w*\\)\\s*\\(([^)]*)\\)",
    # (*name) (args) with space
    "^(.*?)\\s*\\(\\*\\w*\\)\\s+\\(([^)]*)\\)",
    # Just return type and args: void(int, double)
    "^(.*?)\\(([^)]*)\\)$"
  )

  for (pat in patterns) {
    m <- regexec(pat, sig, perl = TRUE)
    if (m[[1]][1] != -1) {
      matches <- regmatches(sig, m)[[1]]
      return_type <- trimws(matches[2])
      args_str <- trimws(matches[3])

      # Parse argument types
      if (args_str == "" || args_str == "void") {
        arg_types <- character(0)
      } else {
        # Split by comma, handling function pointers and nested parens
        arg_types <- parse_arg_list(args_str)
      }

      return(list(
        return_type = return_type,
        arg_types = arg_types,
        raw = signature
      ))
    }
  }

  # If no pattern matched, try simple fallback
  stop("Unable to parse callback signature: ", signature, call. = FALSE)
}

# Parse a comma-separated argument list, handling nested structures
parse_arg_list <- function(args_str) {
  if (args_str == "" || args_str == "void") {
    return(character(0))
  }

  # Simple split by comma - handles most common cases
  # For complex function pointer arguments, they should use callback type
  args <- strsplit(args_str, ",")[[1]]
  args <- trimws(args)

  # Remove parameter names (keep only type)
  # e.g., "int x" -> "int", "double* ptr" -> "double*"
  result <- character(length(args))
  for (i in seq_along(args)) {
    arg <- args[i]

    # Handle function pointer types: void (*)(int)
    # Keep them as-is, they'll be treated as "callback" type
    if (grepl("\\(", arg)) {
      result[i] <- "callback"
    } else {
      # Remove identifier name (last word if it looks like an identifier)
      # This is a simplification - for complex types we may need more parsing
      parts <- strsplit(arg, "\\s+")[[1]]
      if (length(parts) > 1 && !grepl("[*\\[]", parts[length(parts)])) {
        # Last part looks like an identifier, remove it
        parts <- parts[-length(parts)]
      }
      result[i] <- paste(parts, collapse = " ")
    }
  }

  result
}

# Format signature for display
format_signature <- function(sig) {
  if (is.null(sig)) {
    return("unknown")
  }

  args <- if (length(sig$arg_types) == 0) {
    "void"
  } else {
    paste(sig$arg_types, collapse = ", ")
  }
  paste0(sig$return_type, " (*)(", args, ")")
}

# ============================================================================
# FFI Integration - Allow callbacks in tcc_bind()
# ============================================================================

#' Check if a type represents a callback
#'
#' @param type Type string to check
#' @return Logical
is_callback_type <- function(type) {
  if (!is.character(type)) {
    return(FALSE)
  }
  type == "callback" ||
    grepl("^callback:", type) ||
    grepl("^callback_async:", type)
}

is_callback_async_type <- function(type) {
  if (!is.character(type)) {
    return(FALSE)
  }
  grepl("^callback_async:", type)
}

#' Parse callback type specification
#'
#' @param type Type string like "callback:double(int,int)"
#' @return Parsed signature list or NULL
parse_callback_type <- function(type) {
  if (type == "callback") {
    # Generic callback without signature - will need runtime determination
    return(NULL)
  }

  if (grepl("^callback:", type)) {
    # Extract signature after "callback:"
    sig_str <- sub("^callback:", "", type)
    return(parse_callback_signature(sig_str))
  }

  if (grepl("^callback_async:", type)) {
    sig_str <- sub("^callback_async:", "", type)
    return(parse_callback_signature(sig_str))
  }

  NULL
}

#' Enable async callback dispatcher (main-thread queue)
#'
#' Initializes an event-loop handler so callbacks can be scheduled from
#' non-R threads and executed on the main R thread. This is currently
#' supported on Unix-like systems only.
#'
#' @return NULL (invisible)
#' @export
tcc_callback_async_enable <- function() {
  .Call(RC_callback_async_init)
  invisible(NULL)
}

#' Schedule a callback to run on the main thread
#'
#' Enqueue a callback for main-thread execution. Arguments must be basic
#' scalars or external pointers.
#'
#' @param callback A tcc_callback object
#' @param args List of arguments to pass to the callback
#' @return NULL (invisible)
#' @export
tcc_callback_async_schedule <- function(callback, args = list()) {
  if (!inherits(callback, "tcc_callback")) {
    stop("Expected tcc_callback object", call. = FALSE)
  }
  if (!is.list(args)) {
    stop("args must be a list", call. = FALSE)
  }
  .Call(RC_callback_async_schedule, callback, args)
  invisible(NULL)
}

#' Drain the async callback queue
#'
#' This is mainly useful for tests; normally callbacks are executed by the
#' event loop once scheduled.
#'
#' @return NULL (invisible)
#' @export
tcc_callback_async_drain <- function() {
  .Call(RC_callback_async_drain)
  invisible(NULL)
}

#' Generate trampoline code for a callback argument
#'
#' @param trampoline_name Name of the trampoline function
#' @param sig Parsed signature
#' @return C code string for the trampoline
generate_trampoline <- function(trampoline_name, sig) {
  # Generate a trampoline function that:
  # 1. Takes a user data pointer + C arguments
  # 2. Calls RC_invoke_callback_id() to invoke the R function
  # 3. Returns the result

  n_args <- length(sig$arg_types)

  # Build argument list for C function signature
  c_args <- c("void* cb")
  if (n_args > 0) {
    for (i in seq_len(n_args)) {
      c_args <- c(
        c_args,
        sprintf("%s arg%d", map_c_to_sexp_type(sig$arg_types[i]), i)
      )
    }
  }

  # Build the trampoline
  lines <- c(
    sprintf("// Trampoline %s", trampoline_name),
    sprintf(
      "%s %s(%s) {",
      sig$return_type,
      trampoline_name,
      if (length(c_args) > 0) paste(c_args, collapse = ", ") else "void"
    ),
    "  callback_token_t* tok = (callback_token_t*)cb;",
    "  if (!tok || tok->id < 0) {",
    sprintf(
      "    Rf_warning(\"Invalid callback token in %s\");",
      trampoline_name
    ),
    get_c_default_return(sig$return_type, indent = 4L),
    "  }"
  )

  # Build argument conversion
  if (n_args > 0) {
    lines <- c(
      lines,
      sprintf("  SEXP args = PROTECT(allocVector(VECSXP, %d));", n_args)
    )
    for (i in seq_len(n_args)) {
      lines <- c(
        lines,
        sprintf(
          "  SET_VECTOR_ELT(args, %d, %s);",
          i - 1,
          get_sexp_constructor_call(sig$arg_types[i], paste0("arg", i))
        )
      )
    }
  } else {
    lines <- c(lines, "  SEXP args = R_NilValue;")
  }

  # Call the runtime
  lines <- c(lines, "  SEXP result = RC_invoke_callback_id(tok->id, args);")

  if (n_args > 0) {
    lines <- c(lines, "  UNPROTECT(1);")
  }

  # Convert result back to C type
  if (sig$return_type == "void") {
    lines <- c(lines, "  return;")
  } else if (sig$return_type %in% c("SEXP", "sexp")) {
    lines <- c(lines, "  return result;")
  } else {
    lines <- c(
      lines,
      sprintf("  return %s(result);", get_r_to_c_converter(sig$return_type))
    )
  }

  lines <- c(lines, "}")

  paste(lines, collapse = "\n")
}

# Generate async trampoline that schedules on main thread
generate_async_trampoline <- function(trampoline_name, sig) {
  n_args <- length(sig$arg_types)

  c_args <- c("void* cb")
  if (n_args > 0) {
    for (i in seq_len(n_args)) {
      c_args <- c(
        c_args,
        sprintf("%s arg%d", map_c_to_sexp_type(sig$arg_types[i]), i)
      )
    }
  }

  lines <- c(
    sprintf("// Async trampoline %s", trampoline_name),
    sprintf(
      "%s %s(%s) {",
      sig$return_type,
      trampoline_name,
      if (length(c_args) > 0) paste(c_args, collapse = ", ") else "void"
    ),
    "  callback_token_t* tok = (callback_token_t*)cb;",
    "  if (!tok || tok->id < 0) {",
    sprintf(
      "    Rf_warning(\"Invalid callback token in %s\");",
      trampoline_name
    ),
    get_c_default_return(sig$return_type, indent = 4L),
    "  }"
  )

  if (n_args > 0) {
    lines <- c(lines, sprintf("  cb_arg_t args[%d];", n_args))
    for (i in seq_len(n_args)) {
      lines <- c(lines, generate_cb_arg_assignment(i, sig$arg_types[i]))
    }
  } else {
    lines <- c(lines, "  cb_arg_t* args = NULL;")
  }

  lines <- c(
    lines,
    sprintf(
      "  int rc = RC_callback_async_schedule_c(tok->id, %d, %s);",
      n_args,
      if (n_args > 0) "args" else "NULL"
    ),
    "  if (rc != 0) {",
    sprintf(
      "    Rf_warning(\"Async schedule failed (rc=%%d) in %s\", rc);",
      trampoline_name
    ),
    get_c_default_return(sig$return_type, indent = 4L),
    "  }"
  )

  if (sig$return_type == "void") {
    lines <- c(lines, "  return;")
  } else {
    lines <- c(lines, get_c_default_return(sig$return_type, indent = 2L))
  }

  lines <- c(lines, "}")

  paste(lines, collapse = "\n")
}

generate_cb_arg_assignment <- function(index, c_type) {
  kind <- map_c_to_cb_arg_kind(c_type)
  value <- map_c_to_cb_arg_value(c_type, paste0("arg", index))
  c(
    sprintf("  args[%d].kind = %s;", index - 1, kind),
    sprintf("  args[%d].v.%s = %s;", index - 1, value$field, value$expr)
  )
}

map_c_to_cb_arg_kind <- function(c_type) {
  c_type <- trimws(c_type)
  is_ptr <- grepl("\\*", c_type) && !grepl("char\\s*\\*", c_type)

  if (is_ptr || c_type %in% c("void*", "void *", "ptr")) {
    return("CB_ARG_PTR")
  }
  if (c_type %in% c("char*", "const char*", "string", "cstring")) {
    return("CB_ARG_CSTRING")
  }
  if (c_type %in% c("double", "float", "f64", "f32")) {
    return("CB_ARG_REAL")
  }
  if (c_type %in% c("bool", "_Bool")) {
    return("CB_ARG_LOGICAL")
  }
  "CB_ARG_INT"
}

map_c_to_cb_arg_value <- function(c_type, arg_expr) {
  c_type <- trimws(c_type)
  is_ptr <- grepl("\\*", c_type) && !grepl("char\\s*\\*", c_type)

  if (is_ptr || c_type %in% c("void*", "void *", "ptr")) {
    return(list(field = "p", expr = arg_expr))
  }
  if (c_type %in% c("char*", "const char*", "string", "cstring")) {
    return(list(field = "s", expr = arg_expr))
  }
  if (c_type %in% c("double", "float", "f64", "f32")) {
    return(list(field = "d", expr = arg_expr))
  }
  if (c_type %in% c("bool", "_Bool")) {
    return(list(field = "i", expr = arg_expr))
  }
  list(field = "i", expr = arg_expr)
}

# Get default C return statement for a type
get_c_default_return <- function(c_type, indent = 2L) {
  c_type <- trimws(c_type)
  pad <- paste(rep(" ", indent), collapse = "")
  if (c_type == "void") {
    return(paste0(pad, "return;"))
  }
  if (c_type %in% c("SEXP", "sexp")) {
    return(paste0(pad, "return R_NilValue;"))
  }
  is_ptr <- grepl("\\*", c_type) && !grepl("char\\s*\\*", c_type)
  if (is_ptr || c_type %in% c("void*", "void *", "ptr")) {
    return(paste0(pad, "return NULL;"))
  }
  if (c_type %in% c("char*", "const char*", "string", "cstring")) {
    return(paste0(pad, "return NULL;"))
  }
  if (c_type %in% c("double", "float", "f64", "f32")) {
    return(paste0(pad, "return NA_REAL;"))
  }
  if (c_type %in% c("bool", "_Bool")) {
    return(paste0(pad, "return -1;"))
  }
  return(paste0(pad, "return NA_INTEGER;"))
}

# Map C types to R SEXP types for trampoline arguments
map_c_to_sexp_type <- function(c_type) {
  c_type <- trimws(c_type)
  is_ptr <- grepl("\\*", c_type) && !grepl("char\\s*\\*", c_type)

  if (is_ptr) {
    "void*"
  } else if (
    c_type %in% c("int", "int32_t", "i32", "int16_t", "i16", "int8_t", "i8")
  ) {
    "int"
  } else if (c_type %in% c("long", "long long", "int64_t", "i64")) {
    "long long"
  } else if (
    c_type %in%
      c(
        "uint8_t",
        "u8",
        "uint16_t",
        "u16",
        "uint32_t",
        "u32",
        "uint64_t",
        "u64"
      )
  ) {
    "long long"
  } else if (c_type %in% c("double", "float", "f64", "f32")) {
    "double"
  } else if (c_type %in% c("char*", "const char*", "string", "cstring")) {
    "const char*"
  } else if (c_type %in% c("void*", "void *", "ptr")) {
    "void*"
  } else if (c_type %in% c("bool", "_Bool")) {
    "int"
  } else {
    "void*" # Default to pointer
  }
}

# Map C types to FFI types
map_c_to_r_type <- function(c_type) {
  c_type <- trimws(c_type)
  is_ptr <- grepl("\\*", c_type) && !grepl("char\\s*\\*", c_type)

  if (is_ptr) {
    "ptr"
  } else if (
    c_type %in%
      c(
        "int",
        "int32_t",
        "i32",
        "int16_t",
        "i16",
        "int8_t",
        "i8",
        "long",
        "long long",
        "int64_t",
        "i64",
        "uint8_t",
        "u8",
        "uint16_t",
        "u16",
        "uint32_t",
        "u32",
        "uint64_t",
        "u64"
      )
  ) {
    "i32"
  } else if (c_type %in% c("double", "float", "f64", "f32")) {
    "f64"
  } else if (c_type %in% c("char*", "const char*", "string", "cstring")) {
    "cstring"
  } else if (c_type %in% c("void*", "void *", "ptr")) {
    "ptr"
  } else if (c_type %in% c("bool", "_Bool")) {
    "bool"
  } else if (c_type == "void") {
    "void"
  } else if (c_type %in% c("SEXP", "sexp")) {
    "sexp"
  } else {
    "ptr"
  }
}

# Get SEXP constructor for C type
get_sexp_constructor <- function(c_type) {
  c_type <- trimws(c_type)
  is_ptr <- grepl("\\*", c_type) && !grepl("char\\s*\\*", c_type)

  if (is_ptr) {
    "R_MakeExternalPtr"
  } else if (
    c_type %in% c("int", "int32_t", "i32", "int16_t", "i16", "int8_t", "i8")
  ) {
    "ScalarInteger"
  } else if (c_type %in% c("long", "long long", "int64_t", "i64")) {
    "ScalarReal" # R doesn't have 64-bit integers, use double
  } else if (c_type %in% c("uint8_t", "u8", "uint16_t", "u16")) {
    "ScalarInteger"
  } else if (c_type %in% c("uint32_t", "u32", "uint64_t", "u64")) {
    "ScalarReal"
  } else if (c_type %in% c("double", "float", "f64", "f32")) {
    "ScalarReal"
  } else if (c_type %in% c("char*", "const char*", "string", "cstring")) {
    "mkString"
  } else if (c_type %in% c("void*", "void *", "ptr")) {
    "R_MakeExternalPtr"
  } else if (c_type %in% c("bool", "_Bool")) {
    "ScalarLogical"
  } else {
    "R_MakeExternalPtr"
  }
}

# Get SEXP constructor call for a C expression
get_sexp_constructor_call <- function(c_type, arg_expr) {
  ctor <- get_sexp_constructor(c_type)
  if (ctor == "R_MakeExternalPtr") {
    return(sprintf("R_MakeExternalPtr(%s, R_NilValue, R_NilValue)", arg_expr))
  }
  sprintf("%s(%s)", ctor, arg_expr)
}

# Get R to C converter function
get_r_to_c_converter <- function(c_type) {
  c_type <- trimws(c_type)
  is_ptr <- grepl("\\*", c_type) && !grepl("char\\s*\\*", c_type)

  if (is_ptr) {
    "R_ExternalPtrAddr"
  } else if (
    c_type %in% c("int", "int32_t", "i32", "int16_t", "i16", "int8_t", "i8")
  ) {
    "asInteger"
  } else if (c_type %in% c("long", "long long", "int64_t", "i64")) {
    "asReal" # Convert to double then cast
  } else if (c_type %in% c("uint8_t", "u8", "uint16_t", "u16")) {
    "asInteger"
  } else if (c_type %in% c("uint32_t", "u32", "uint64_t", "u64")) {
    "asReal"
  } else if (c_type %in% c("double", "float", "f64", "f32")) {
    "asReal"
  } else if (c_type %in% c("char*", "const char*", "string", "cstring")) {
    "CHAR"
  } else if (c_type %in% c("void*", "void *", "ptr")) {
    "R_ExternalPtrAddr"
  } else if (c_type %in% c("bool", "_Bool")) {
    "asLogical"
  } else {
    "R_ExternalPtrAddr"
  }
}
