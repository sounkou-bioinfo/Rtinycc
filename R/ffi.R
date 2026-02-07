# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# FFI Class and High-Level API
# Bun-style FFI with API mode compilation

# FFI object class
tcc_ffi_object <- function() {
  structure(
    list(
      state = NULL,
      symbols = list(),
      headers = character(0),
      c_code = character(0),
      libraries = character(0),
      lib_paths = character(0),
      include_paths = character(0),
      output = "memory",
      compiled = FALSE,
      wrapper_symbols = character(0),
      globals = list()
    ),
    class = "tcc_ffi"
  )
}

#' Create a new FFI compilation context
#'
#' Initialize a Bun-style FFI context for API-mode compilation.
#' This is the entry point for the modern FFI API.
#'
#' @return A tcc_ffi object with chaining support
#' @export
#' @examples
#' ffi <- tcc_ffi()
tcc_ffi <- function() {
  tcc_ffi_object()
}

#' Set output type for FFI compilation
#'
#' @param ffi A tcc_ffi object
#' @param output One of "memory", "dll", "exe"
#' @return Updated tcc_ffi object (for chaining)
#' @export
tcc_output <- function(ffi, output = c("memory", "dll", "exe")) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }
  output <- match.arg(output)
  ffi$output <- output
  ffi
}

#' Add include path to FFI context
#'
#' @param ffi A tcc_ffi object
#' @param path Include directory path
#' @return Updated tcc_ffi object (for chaining)
#' @export
tcc_include <- function(ffi, path) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }
  ffi$include_paths <- c(ffi$include_paths, path)
  ffi
}

#' Add library path to FFI context
#'
#' @param ffi A tcc_ffi object
#' @param path Library directory path
#' @return Updated tcc_ffi object (for chaining)
#' @export
tcc_library_path <- function(ffi, path) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }
  ffi$lib_paths <- c(ffi$lib_paths, path)
  ffi
}

#' Add library to link against
#'
#' @param ffi A tcc_ffi object
#' @param library Library name (e.g., "m", "sqlite3") or a path to a
#'   shared library (e.g., "libm.so.6"). When a path is provided, the
#'   library directory is added automatically and the library name is
#'   inferred from the file name.
#' @return Updated tcc_ffi object (for chaining)
#' @export
tcc_library <- function(ffi, library) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  libs <- as.character(library)
  for (lib in libs) {
    if (!nzchar(lib)) {
      next
    }

    lib_name <- lib

    # If a path or a file name with extension is provided, resolve and
    # extract the library name while also adding the lib path.
    if (file.exists(lib) || grepl("[/\\\\]", lib)) {
      if (!file.exists(lib)) {
        found_path <- tcc_find_library(lib)
        if (is.null(found_path)) {
          stop("Library not found: ", lib, call. = FALSE)
        }
        lib <- found_path
      }
      ffi$lib_paths <- c(ffi$lib_paths, dirname(lib))
      lib_name <- sub("^lib", "", basename(lib))
      lib_name <- sub("\\.(so|dylib|dll).*$", "", lib_name)
    } else if (grepl("\\.(so|dylib|dll)(\\..*)?$", lib, ignore.case = TRUE)) {
      found_path <- tcc_find_library(lib)
      if (is.null(found_path)) {
        stop("Library not found: ", lib, call. = FALSE)
      }
      ffi$lib_paths <- c(ffi$lib_paths, dirname(found_path))
      lib_name <- sub("^lib", "", basename(found_path))
      lib_name <- sub("\\.(so|dylib|dll).*$", "", lib_name)
    }

    if (nzchar(lib_name)) {
      ffi$libraries <- c(ffi$libraries, lib_name)
    }
  }

  ffi
}

#' Declare a global variable getter
#'
#' Register a global C symbol so the compiled object exposes getter/setter
#' functions `global_<name>_get()` and `global_<name>_set()`.
#'
#' @param ffi A tcc_ffi object
#' @param name Global symbol name
#' @param type FFI type for the global (scalar types only)
#' @return Updated tcc_ffi object (for chaining)
#' @export
#'
#' @examples
#' ffi <- tcc_ffi() |>
#'   tcc_source("int global_counter = 7;") |>
#'   tcc_global("global_counter", "i32") |>
#'   tcc_compile()
#' ffi$global_global_counter_get()
tcc_global <- function(ffi, name, type) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("Global name must be a non-empty string", call. = FALSE)
  }
  if (!is.character(type) || length(type) != 1 || !nzchar(type)) {
    stop("Global type must be a non-empty string", call. = FALSE)
  }

  type_info <- check_ffi_type(type, "global")
  if (!is.null(type_info$kind) && type_info$kind == "array") {
    stop("Global type cannot be an array type", call. = FALSE)
  }
  if (type == "void") {
    stop("Global type cannot be void", call. = FALSE)
  }

  if (is.null(ffi$globals)) {
    ffi$globals <- list()
  }
  ffi$globals[[name]] <- type
  ffi
}

#' Add C headers
#'
#' @param ffi A tcc_ffi object
#' @param header Header string or include directive
#' @return Updated tcc_ffi object (for chaining)
#' @export
tcc_header <- function(ffi, header) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }
  ffi$headers <- c(ffi$headers, header)
  ffi
}

#' Add C source code
#'
#' @param ffi A tcc_ffi object
#' @param code C source code string
#' @return Updated tcc_ffi object (for chaining)
#' @export
tcc_source <- function(ffi, code) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }
  ffi$c_code <- paste(ffi$c_code, code, sep = "\n")
  ffi
}

#' Bind symbols with type specifications
#'
#' Define symbols with Bun-style type specifications for API mode.
#' This is the core of the declarative FFI API.
#'
#' @param ffi A tcc_ffi object
#' @param ... Named list of symbol definitions. Each definition is a list with:
#'   \itemize{
#'     \item args: List of FFI types for arguments (e.g., list("i32", "f64"))
#'     \item returns: FFI type for return value (e.g., "f64", "cstring")
#'     \item code: Optional C code for the symbol (for embedded functions)
#'   }
#'   Callback arguments should use the form \code{callback:<signature>} (e.g.,
#'   \code{callback:double(double)}). The generated trampoline expects a
#'   \code{tcc_callback_ptr(cb)} to the corresponding user-data parameter in
#'   the C API. For thread-safe scheduling, use
#'   \code{callback_async:<signature>} which enqueues the call on the main
#'   thread and returns a default value immediately.
#' @return Updated tcc_ffi object (for chaining)
#' @export
#' @examples
#' ffi <- tcc_ffi() |>
#'   tcc_bind(
#'     add = list(args = list("i32", "i32"), returns = "i32"),
#'     greet = list(args = list("cstring"), returns = "cstring")
#'   )
tcc_bind <- function(ffi, ...) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  symbols <- list(...)

  # Validate each symbol definition
  for (sym_name in names(symbols)) {
    sym <- symbols[[sym_name]]

    # Check required fields
    if (!"returns" %in% names(sym)) {
      stop(
        "Symbol '",
        sym_name,
        "' missing 'returns' specification",
        call. = FALSE
      )
    }

    # Validate return type
    if (is.list(sym$returns)) {
      if (is.null(sym$returns$type)) {
        stop(
          "Symbol '",
          sym_name,
          "' return must include 'type'",
          call. = FALSE
        )
      }
      ret_type <- sym$returns$type
      ret_info <- check_ffi_type(
        ret_type,
        paste0("symbol '", sym_name, "' return")
      )
      if (!is.null(ret_info$kind) && ret_info$kind == "array") {
        if (is.null(sym$returns$length_arg)) {
          stop(
            "Symbol '",
            sym_name,
            "' array return requires 'length_arg'",
            call. = FALSE
          )
        }
        if (!is.null(sym$args) && length(sym$args) > 0) {
          if (
            sym$returns$length_arg < 1 ||
              sym$returns$length_arg > length(sym$args)
          ) {
            stop(
              "Symbol '",
              sym_name,
              "' array return length_arg out of range",
              call. = FALSE
            )
          }
        }
      }
    } else {
      check_ffi_type(sym$returns, paste0("symbol '", sym_name, "' return"))
    }

    # Validate argument types
    if ("args" %in% names(sym)) {
      for (i in seq_along(sym$args)) {
        check_ffi_type(
          sym$args[[i]],
          paste0("symbol '", sym_name, "' argument ", i)
        )
      }
    }

    # Store the symbol
    ffi$symbols[[sym_name]] <- sym
  }

  ffi
}

#' Compile FFI bindings
#'
#' Compile the defined symbols into callable functions.
#' This generates C wrapper code and compiles it with TinyCC.
#'
#' @param ffi A tcc_ffi object
#' @param verbose Print compilation info
#' @return A tcc_compiled object with callable functions
#' @export
tcc_compile <- function(ffi, verbose = FALSE) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  if (
    length(ffi$symbols) == 0 &&
      length(ffi$structs) == 0 &&
      length(ffi$unions) == 0 &&
      length(ffi$enums) == 0 &&
      length(ffi$globals) == 0
  ) {
    stop(
      "No symbols, structs, unions, enums, or globals defined. Use tcc_bind(), tcc_struct(), tcc_union(), tcc_enum(), or tcc_global() first.",
      call. = FALSE
    )
  }

  # Generate C code (always R API mode now)
  c_code <- generate_ffi_code(
    symbols = ffi$symbols,
    headers = ffi$headers,
    c_code = ffi$c_code,
    is_external = FALSE,
    structs = ffi$structs,
    unions = ffi$unions,
    enums = ffi$enums,
    globals = ffi$globals,
    container_of = ffi$container_of,
    field_addr = ffi$field_addr,
    struct_raw_access = ffi$struct_raw_access,
    introspect = ffi$introspect
  )

  if (verbose) {
    message("Generated C code:\n", c_code)
  }

  # Create TinyCC state
  state <- tcc_state(
    output = ffi$output,
    include_path = c(
      tcc_include_paths(),
      ffi$include_paths,
      file.path(R.home("include"))
    ),
    lib_path = c(tcc_lib_paths(), ffi$lib_paths, file.path(R.home("lib")))
  )

  # Add library paths
  for (lib_path in ffi$lib_paths) {
    tcc_add_library_path(state, lib_path)
  }

  # Add libraries
  for (lib in ffi$libraries) {
    tcc_add_library(state, lib)
  }

  # Compile the generated code
  result <- tcc_compile_string(state, c_code)
  if (result != 0) {
    stop("Failed to compile FFI bindings", call. = FALSE)
  }

  # Register host symbols for macOS compatibility
  .Call(RC_libtcc_add_host_symbols, state)

  # Relocate
  result <- tcc_relocate(state)
  if (result != 0) {
    stop("Failed to relocate compiled code", call. = FALSE)
  }

  # Create callable object
  compiled <- tcc_compiled_object(
    state,
    ffi$symbols,
    ffi$output,
    ffi$structs,
    ffi$unions,
    ffi$enums,
    ffi$globals,
    ffi$container_of,
    ffi$field_addr,
    ffi$struct_raw_access,
    ffi$introspect
  )

  # Store the recipe so the object can be recompiled after deserialization
  compiled$.ffi <- ffi

  compiled
}

# Create compiled object with callable functions
tcc_compiled_object <- function(
  state,
  symbols,
  output,
  structs = NULL,
  unions = NULL,
  enums = NULL,
  globals = NULL,
  container_of = NULL,
  field_addr = NULL,
  struct_raw_access = NULL,
  introspect = NULL
) {
  # Build wrapper names for bound symbols
  if (length(symbols) > 0) {
    wrapper_names <- paste0("R_wrap_", names(symbols))
  } else {
    wrapper_names <- character(0)
  }

  # Build helper names for struct/union/enum helpers
  helper_names <- character()
  helper_specs <- list()

  # Struct helpers
  if (!is.null(structs)) {
    for (struct_name in names(structs)) {
      fields <- structs[[struct_name]]
      # new, free
      helper_names <- c(
        helper_names,
        paste0("struct_", struct_name, "_new"),
        paste0("struct_", struct_name, "_free")
      )
      helper_specs[[paste0("struct_", struct_name, "_new")]] <- list(
        args = list(),
        returns = "sexp"
      )
      helper_specs[[paste0("struct_", struct_name, "_free")]] <- list(
        args = list("sexp"),
        returns = "sexp"
      )
      # getters and setters
      for (field_name in names(fields)) {
        field_spec <- fields[[field_name]]
        is_array <- is.list(field_spec) && isTRUE(field_spec$array)

        helper_names <- c(
          helper_names,
          paste0("struct_", struct_name, "_get_", field_name)
        )
        helper_specs[[paste0(
          "struct_",
          struct_name,
          "_get_",
          field_name
        )]] <- list(
          args = list("sexp"),
          returns = "sexp"
        )

        if (is_array) {
          helper_names <- c(
            helper_names,
            paste0("struct_", struct_name, "_get_", field_name, "_elt"),
            paste0("struct_", struct_name, "_set_", field_name, "_elt")
          )
          helper_specs[[paste0(
            "struct_",
            struct_name,
            "_get_",
            field_name,
            "_elt"
          )]] <- list(
            args = list("sexp", "i32"),
            returns = "sexp"
          )
          helper_specs[[paste0(
            "struct_",
            struct_name,
            "_set_",
            field_name,
            "_elt"
          )]] <- list(
            args = list("sexp", "i32", "sexp"),
            returns = "sexp"
          )
        } else {
          helper_names <- c(
            helper_names,
            paste0("struct_", struct_name, "_set_", field_name)
          )
          helper_specs[[paste0(
            "struct_",
            struct_name,
            "_set_",
            field_name
          )]] <- list(
            args = list("sexp", "sexp"),
            returns = "sexp"
          )
        }
      }
      # container_of helpers
      if (!is.null(container_of) && struct_name %in% names(container_of)) {
        for (member_name in container_of[[struct_name]]) {
          helper_names <- c(
            helper_names,
            paste0("struct_", struct_name, "_from_", member_name)
          )
          helper_specs[[paste0(
            "struct_",
            struct_name,
            "_from_",
            member_name
          )]] <- list(
            args = list("sexp"),
            returns = "sexp"
          )
        }
      }
      # field_addr helpers
      if (!is.null(field_addr) && struct_name %in% names(field_addr)) {
        for (field_name in field_addr[[struct_name]]) {
          helper_names <- c(
            helper_names,
            paste0("struct_", struct_name, "_", field_name, "_addr")
          )
          helper_specs[[paste0(
            "struct_",
            struct_name,
            "_",
            field_name,
            "_addr"
          )]] <- list(
            args = list("sexp"),
            returns = "sexp"
          )
        }
      }
      # raw_access helpers
      if (!is.null(struct_raw_access) && struct_name %in% struct_raw_access) {
        helper_names <- c(
          helper_names,
          paste0("struct_", struct_name, "_get_raw"),
          paste0("struct_", struct_name, "_set_raw")
        )
        helper_specs[[paste0("struct_", struct_name, "_get_raw")]] <- list(
          args = list("sexp", "i32"),
          returns = "sexp"
        )
        helper_specs[[paste0("struct_", struct_name, "_set_raw")]] <- list(
          args = list("sexp", "raw"),
          returns = "sexp"
        )
      }
      # introspection helpers
      if (!is.null(introspect) && introspect) {
        helper_names <- c(
          helper_names,
          paste0("struct_", struct_name, "_sizeof"),
          paste0("struct_", struct_name, "_alignof")
        )
        helper_specs[[paste0("struct_", struct_name, "_sizeof")]] <- list(
          args = list(),
          returns = "sexp"
        )
        helper_specs[[paste0("struct_", struct_name, "_alignof")]] <- list(
          args = list(),
          returns = "sexp"
        )
      }
    }
  }

  # Union helpers
  if (!is.null(unions)) {
    for (union_name in names(unions)) {
      union_def <- unions[[union_name]]
      # new, free
      helper_names <- c(
        helper_names,
        paste0("union_", union_name, "_new"),
        paste0("union_", union_name, "_free")
      )
      helper_specs[[paste0("union_", union_name, "_new")]] <- list(
        args = list(),
        returns = "sexp"
      )
      helper_specs[[paste0("union_", union_name, "_free")]] <- list(
        args = list("sexp"),
        returns = "sexp"
      )
      # getters and setters for members
      for (mem_name in names(union_def$members)) {
        helper_names <- c(
          helper_names,
          paste0("union_", union_name, "_get_", mem_name),
          paste0("union_", union_name, "_set_", mem_name)
        )
        helper_specs[[paste0("union_", union_name, "_get_", mem_name)]] <- list(
          args = list("sexp"),
          returns = "sexp"
        )
        helper_specs[[paste0("union_", union_name, "_set_", mem_name)]] <- list(
          args = list("sexp", "sexp"),
          returns = "sexp"
        )
      }
      # introspection
      if (!is.null(introspect) && introspect) {
        helper_names <- c(
          helper_names,
          paste0("union_", union_name, "_sizeof"),
          paste0("union_", union_name, "_alignof")
        )
        helper_specs[[paste0("union_", union_name, "_sizeof")]] <- list(
          args = list(),
          returns = "sexp"
        )
        helper_specs[[paste0("union_", union_name, "_alignof")]] <- list(
          args = list(),
          returns = "sexp"
        )
      }
    }
  }

  # Enum helpers
  if (!is.null(enums)) {
    for (enum_name in names(enums)) {
      enum_def <- enums[[enum_name]]
      # introspection
      if (!is.null(introspect) && introspect) {
        helper_names <- c(helper_names, paste0("enum_", enum_name, "_sizeof"))
        helper_specs[[paste0("enum_", enum_name, "_sizeof")]] <- list(
          args = list(),
          returns = "sexp"
        )
      }
      # constant export
      if (!is.null(enum_def$constants)) {
        for (const_name in enum_def$constants) {
          helper_names <- c(
            helper_names,
            paste0("enum_", enum_name, "_", const_name)
          )
          helper_specs[[paste0("enum_", enum_name, "_", const_name)]] <- list(
            args = list(),
            returns = "sexp"
          )
        }
      }
    }
  }

  # Global helpers
  if (!is.null(globals) && length(globals) > 0) {
    for (global_name in names(globals)) {
      helper_names <- c(
        helper_names,
        paste0("global_", global_name, "_get"),
        paste0("global_", global_name, "_set")
      )
      helper_specs[[paste0("global_", global_name, "_get")]] <- list(
        args = list(),
        returns = "sexp"
      )
      helper_specs[[paste0("global_", global_name, "_set")]] <- list(
        args = list("sexp"),
        returns = "sexp"
      )
    }
  }

  all_wrapper_names <- c(wrapper_names, paste0("R_wrap_", helper_names))
  all_sym_names <- c(names(symbols), helper_names)

  # Create environment with callable functions
  env <- new.env(parent = emptyenv())

  for (i in seq_along(all_sym_names)) {
    sym_name <- all_sym_names[i]
    wrapper_name <- all_wrapper_names[i]

    if (i <= length(symbols)) {
      sym <- symbols[[i]]
      sym$name <- sym_name
    } else {
      sym <- helper_specs[[sym_name]]
      if (is.null(sym)) {
        warning("Unknown helper symbol '", sym_name, "'")
        next
      }
      sym$name <- sym_name
    }

    # Get the wrapper symbol pointer
    tryCatch(
      {
        fn_ptr <- tcc_get_symbol(state, wrapper_name)

        # Validate the pointer before creating callable
        if (!tcc_symbol_is_valid(fn_ptr)) {
          warning(
            "Symbol '",
            sym_name,
            "' returned invalid pointer for '",
            wrapper_name,
            "'"
          )
          next
        }

        # Create callable function
        env[[sym_name]] <- make_callable(fn_ptr, sym, state)
      },
      error = function(e) {
        warning("Could not bind symbol '", sym_name, "': ", conditionMessage(e))
      }
    )
  }

  # Add state and metadata
  env$.state <- state
  env$.symbols <- symbols
  env$.output <- output
  env$.helpers <- list(
    structs = structs,
    unions = unions,
    enums = enums,
    globals = globals
  )
  env$.valid <- TRUE

  structure(env, class = "tcc_compiled")
}

# Make a callable function from symbol pointer
# Uses .Call() directly with the external pointer - R can call external pointers!
make_callable <- function(fn_ptr, sym, state) {
  # Force evaluation to ensure pointer is captured
  force(fn_ptr)
  force(sym)

  arg_types <- sym$args %||% list()
  sym_name <- sym$name

  # Create function that calls the wrapper via .Call()
  f <- function(...) {
    args <- list(...)
    n_args <- length(args)

    # Validate argument count
    if (n_args != length(arg_types)) {
      stop(
        "Expected ",
        length(arg_types),
        " arguments, got ",
        n_args,
        call. = FALSE
      )
    }

    # Validate pointer is still valid before calling
    if (!tcc_symbol_is_valid(fn_ptr)) {
      stop(
        "Function pointer for '",
        sym_name,
        "' is no longer valid",
        call. = FALSE
      )
    }

    # Call the wrapper function pointer directly using .Call()
    # R's .Call() can invoke external pointers as functions!
    if (n_args == 0) {
      .RtinyccCall(fn_ptr)
    } else if (n_args == 1) {
      .RtinyccCall(fn_ptr, args[[1]])
    } else if (n_args == 2) {
      .RtinyccCall(fn_ptr, args[[1]], args[[2]])
    } else if (n_args == 3) {
      .RtinyccCall(fn_ptr, args[[1]], args[[2]], args[[3]])
    } else if (n_args == 4) {
      .RtinyccCall(fn_ptr, args[[1]], args[[2]], args[[3]], args[[4]])
    } else if (n_args == 5) {
      .RtinyccCall(
        fn_ptr,
        args[[1]],
        args[[2]],
        args[[3]],
        args[[4]],
        args[[5]]
      )
    } else {
      # For more than 5 arguments, use do.call
      do.call(.Call, c(list(fn_ptr), args))
    }
  }

  # Store the pointer in the function's environment to prevent GC
  environment(f)$.fn_ptr <- fn_ptr
  environment(f)$.arg_types <- arg_types
  environment(f)$.sym_name <- sym_name

  f
}

#' Print tcc_ffi object
#'
#' @param x A tcc_ffi object
#' @param ... Ignored
#' @export
print.tcc_ffi <- function(x, ...) {
  cat("<tcc_ffi>\n")
  cat("  Output:", x$output, "\n")
  cat("  Symbols:", length(x$symbols), "defined\n")
  if (length(x$symbols) > 0) {
    for (name in names(x$symbols)) {
      sym <- x$symbols[[name]]
      args <- paste(sym$args %||% "void", collapse = ", ")
      cat("    ", name, "(", args, ") -> ", sym$returns, "\n", sep = "")
    }
  }
  if (length(x$libraries) > 0) {
    cat("  Libraries:", paste(x$libraries, collapse = ", "), "\n")
  }
  if (length(x$include_paths) > 0) {
    cat("  Include paths:", length(x$include_paths), "\n")
  }
  invisible(x)
}

#' Print tcc_compiled object
#'
#' @param x A tcc_compiled object
#' @param ... Ignored
#' @export
print.tcc_compiled <- function(x, ...) {
  cat("<tcc_compiled>\n")
  cat("  Output:", x$.output, "\n")
  cat("  Symbols:", length(x$.symbols), "compiled\n")
  if (length(x$.symbols) > 0) {
    for (name in names(x$.symbols)) {
      if (exists(name, envir = x, inherits = FALSE)) {
        cat("    ", name, "() [callable]\n", sep = "")
      } else {
        cat("    ", name, "() [failed]\n", sep = "")
      }
    }
  }
  invisible(x)
}

#' Access a compiled FFI symbol
#'
#' Overrides \code{$} to detect dead pointers after deserialization
#' and recompile transparently from the stored recipe.
#'
#' @param x A tcc_compiled object
#' @param name Symbol name to access
#' @return The callable function or metadata field
#' @export
`$.tcc_compiled` <- function(x, name) {
  # Fast path: internal fields are always accessible
  if (startsWith(name, ".")) {
    return(.subset2(x, name))
  }

  # Detect dead pointers (nil after deserialization) and recompile.
  # tcc_symbol_is_valid() is a single C call; negligible overhead.
  state <- .subset2(x, ".state")
  if (is.null(state) || !tcc_symbol_is_valid(state)) {
    message("[Rtinycc] Recompiling FFI bindings after deserialization")
    recompile_into(x)
  }

  .subset2(x, name)
}

#' Recompile a tcc_compiled object
#'
#' Explicitly recompile from the stored FFI recipe.
#' Useful after deserialization (\code{readRDS}, \code{unserialize})
#' or to force a fresh compilation.
#'
#' @param compiled A tcc_compiled object
#' @return The recompiled tcc_compiled object (invisibly, same environment)
#' @export
tcc_recompile <- function(compiled) {
  if (!inherits(compiled, "tcc_compiled")) {
    stop("Expected tcc_compiled object", call. = FALSE)
  }
  recompile_into(compiled)
  invisible(compiled)
}

# Shared recompilation logic for both tcc_compile and tcc_link objects.
# Rebuilds the compiled code from the stored recipe and copies all
# bindings into the target environment.
recompile_into <- function(target) {
  link_args <- .subset2(target, ".link_args")
  if (!is.null(link_args)) {
    fresh <- do.call(tcc_link, link_args)
  } else {
    ffi <- .subset2(target, ".ffi")
    if (is.null(ffi)) {
      stop("Cannot recompile: no FFI recipe stored in this object", call. = FALSE)
    }
    fresh <- tcc_compile(ffi)
  }
  for (nm in ls(fresh, all.names = TRUE)) {
    assign(nm, get(nm, envir = fresh, inherits = FALSE), envir = target)
  }
  assign(".valid", TRUE, envir = target)
  invisible(target)
}

# Helper for %||%
`%||%` <- function(x, y) if (is.null(x)) y else x

# Platform-dependent library search paths
tcc_platform_lib_paths <- function() {
  sysname <- Sys.info()["sysname"]

  switch(sysname,
    Linux = c(
      "/usr/lib",
      "/usr/lib64",
      "/usr/local/lib",
      "/lib",
      "/lib64",
      "/lib32",
      "/usr/local/lib64",
      "/usr/lib/x86_64-linux-gnu",
      "/usr/lib/i386-linux-gnu",
      "/lib/x86_64-linux-gnu",
      "/lib32/x86_64-linux-gnu",
      "/lib/x86_64-linux-gnu/"
    ),
    Darwin = c(
      "/usr/lib",
      "/usr/local/lib",
      "/opt/homebrew/lib", # Apple Silicon Homebrew
      "/opt/local/lib", # MacPorts
      "/System/Library/Frameworks", # Apple system libs
      "/Library/Frameworks"
    ),
    Windows = c(
      "C:/msys64/mingw64/lib", # MSYS2
      "C:/msys64/mingw32/lib",
      "C:/Rtools/mingw_64/lib", # Rtools
      "C:/Rtools/mingw_32/lib"
    ),
    # Default fallback
    c("/usr/lib", "/usr/local/lib")
  )
}

# Search for library in platform-dependent paths
tcc_find_library <- function(name) {
  if (file.exists(name)) {
    return(name)
  }

  # Try platform-specific paths
  paths <- tcc_platform_lib_paths()
  sysname <- Sys.info()["sysname"]

  # Platform-specific library naming
  # check if name already has ddl subix (including version numbers)
  if (sysname == "Windows" && grepl("\\.dll$", name, ignore.case = TRUE)) {
    lib_name <- name
  } else if (sysname == "Linux" && grepl("\\.so(\\..*)?$", name)) {
    lib_name <- name
  } else if (sysname == "Darwin" && grepl("\\.dylib(\\..*)?$", name)) {
    lib_name <- name
  } else {
    lib_name <- switch(sysname,
      Linux = paste0("lib", name, ".so"),
      Darwin = paste0("lib", name, ".dylib"),
      Windows = paste0(name, ".dll"),
      paste0("lib", name, ".so") # Default
    )
  }

  for (path in paths) {
    if (dir.exists(path)) {
      full_path <- file.path(path, lib_name)
      if (file.exists(full_path)) {
        return(full_path)
      }
    }
  }

  # Not found
  NULL
}

#' Link an external shared library with Bun-style FFI bindings
#'
#' Link a system library (like libsqlite3.so) and generate type-safe
#' wrappers automatically using TinyCC JIT compilation (API mode).
#' Unlike dlopen(), this uses TinyCC to compile bindings that handle
#' type conversion between R and C automatically.
#'
#' @param path Path to the shared library (e.g., "libsqlite3.so")
#' @param symbols Named list of symbol definitions with:
#'   \itemize{
#'     \item args: List of FFI types for arguments
#'     \item returns: FFI type for return value
#'   }
#' @param headers Optional C headers to include
#' @param libs Library names to link (e.g., "sqlite3")
#' @param lib_paths Additional library search paths
#' @param include_paths Additional include search paths
#' @param user_code Optional custom C code to include in the compilation
#' @param verbose Print debug information
#' @return A tcc_compiled object with callable functions
#' @export
#' @examples
#' \dontrun{
#' # Link SQLite with type-safe bindings
#' sqlite <- tcc_link(
#'   "libsqlite3.so",
#'   symbols = list(
#'     sqlite3_libversion = list(args = list(), returns = "cstring"),
#'     sqlite3_open = list(args = list("cstring", "ptr"), returns = "i32")
#'   ),
#'   libs = "sqlite3"
#' )
#'
#' # Call directly - type conversion happens automatically
#' sqlite$sqlite3_libversion()
#'
#' # Example with custom user code for helper functions
#' math_with_helpers <- tcc_link(
#'   "libm.so.6",
#'   symbols = list(
#'     sqrt = list(args = list("f64"), returns = "f64"),
#'     safe_sqrt = list(args = list("f64"), returns = "f64")
#'   ),
#'   user_code = "
#'     #include <math.h>
#'
#'     // Helper function that validates input before calling sqrt
#'     double safe_sqrt(double x) {
#'       if (x < 0) {
#'         return NAN;
#'       }
#'       return sqrt(x);
#'     }
#'   ",
#'   libs = "m"
#' )
#' math_with_helpers$safe_sqrt(16.0)
#' math_with_helpers$safe_sqrt(-4.0) # Returns NaN for negative input
#' }
tcc_link <- function(
  path,
  symbols,
  headers = NULL,
  libs = character(0),
  lib_paths = character(0),
  include_paths = character(0),
  user_code = NULL,
  verbose = FALSE
) {
  # Find library if not absolute path
  if (!file.exists(path) && !grepl("^/", path)) {
    found_path <- tcc_find_library(path)
    if (is.null(found_path)) {
      stop("Library not found: ", path, call. = FALSE)
    }
    path <- found_path
  }

  if (verbose) {
    message("Loading library: ", path)
    message("Symbols: ", paste(names(symbols), collapse = ", "))
  }

  # Create FFI context
  ffi <- tcc_ffi() |>
    tcc_output("memory")

  # Add library
  ffi$libraries <- libs
  ffi$lib_paths <- c(dirname(path), lib_paths)
  ffi$include_paths <- include_paths

  # Process headers
  if (!is.null(headers)) {
    ffi$headers <- headers
  }

  # Process user code
  if (!is.null(user_code)) {
    ffi$c_code <- user_code
  }

  # Add the library path for the specific library
  ffi <- tcc_library_path(ffi, dirname(path))

  # Extract library name from path for linking
  lib_name <- sub("^lib", "", basename(path))
  lib_name <- sub("\\.(so|dylib|dll).*", "", lib_name)
  if (nzchar(lib_name) > 0) {
    ffi <- tcc_library(ffi, lib_name)
  }

  # Store symbols
  for (sym_name in names(symbols)) {
    sym <- symbols[[sym_name]]

    # Validate
    check_ffi_type(sym$returns, paste0("symbol '", sym_name, "' return"))
    if ("args" %in% names(sym)) {
      for (i in seq_along(sym$args)) {
        check_ffi_type(sym$args[[i]], paste0("symbol '", sym_name, "' arg ", i))
      }
    }

    ffi$symbols[[sym_name]] <- sym
  }

  # Mark as external library bindings
  attr(ffi, "external_lib") <- path
  attr(ffi, "is_external") <- TRUE

  # Generate C code with external declarations (always R API mode)
  c_code <- generate_ffi_code(
    symbols = ffi$symbols,
    headers = ffi$headers,
    c_code = ffi$c_code,
    is_external = TRUE,
    structs = ffi$structs,
    unions = ffi$unions,
    enums = ffi$enums,
    globals = ffi$globals,
    container_of = ffi$container_of,
    field_addr = ffi$field_addr,
    struct_raw_access = ffi$struct_raw_access,
    introspect = ffi$introspect
  )

  if (verbose) {
    message("Generated C code:\n", c_code)
  }

  # Create TinyCC state
  state <- tcc_state(
    output = "memory",
    include_path = c(
      tcc_include_paths(),
      ffi$include_paths,
      file.path(R.home("include"))
    ),
    lib_path = c(tcc_lib_paths(), ffi$lib_paths, file.path(R.home("lib")))
  )

  # Add library paths
  for (lp in ffi$lib_paths) {
    if (nzchar(lp) > 0 && dir.exists(lp)) {
      tcc_add_library_path(state, lp)
    }
  }

  # Add libraries
  for (lib in ffi$libraries) {
    tcc_add_library(state, lib)
  }

  # Compile the generated code
  result <- tcc_compile_string(state, c_code)
  if (result != 0) {
    stop("Failed to compile FFI bindings for ", basename(path), call. = FALSE)
  }

  # Register host symbols for macOS compatibility
  .Call(RC_libtcc_add_host_symbols, state)

  # Relocate
  result <- tcc_relocate(state)
  if (result != 0) {
    stop("Failed to relocate compiled code for ", basename(path), call. = FALSE)
  }

  # Create compiled object
  compiled <- tcc_compiled_object(
    state,
    ffi$symbols,
    "memory",
    ffi$structs,
    ffi$unions,
    ffi$enums,
    ffi$globals,
    ffi$container_of,
    ffi$field_addr,
    ffi$struct_raw_access,
    ffi$introspect
  )

  # Store enough to re-link after deserialization
  compiled$.ffi <- ffi
  compiled$.link_args <- list(
    path = path,
    symbols = symbols,
    headers = headers,
    libs = libs,
    lib_paths = lib_paths,
    include_paths = include_paths,
    user_code = user_code
  )

  if (verbose) {
    message("Successfully loaded ", length(ffi$symbols), " symbols")
  }

  compiled
}

#' CString S3 Class
#'
#' Safe handling of C strings (char*) with automatic memory management.
#' Like Bun's CString class.
#'
#' @param ptr External pointer to C string
#' @param clone Whether to clone the string immediately (safe for R use)
#' @param owned Currently unused. Reserved for future finalizer support.
#' @return A tcc_cstring object
#' @export
tcc_cstring_object <- function(ptr, clone = TRUE, owned = FALSE) {
  if (!inherits(ptr, "externalptr")) {
    stop("Expected external pointer", call. = FALSE)
  }

  obj <- list(
    ptr = ptr,
    owned = owned,
    cached_string = NULL
  )

  if (clone) {
    # Immediately copy to R string
    addr <- get_external_ptr_addr(ptr)
    if (addr > 0) {
      # Read C string at address
      obj$cached_string <- tcc_read_cstring(ptr)
    }
  }

  # Register finalizer if owned
  if (owned) {
    reg.finalizer(obj, function(x) {
      if (!is.null(x$ptr)) {
        # Free C memory
        # This would need a C helper to call free()
      }
    })
  }

  class(obj) <- "tcc_cstring"
  obj
}

#' @export
as.character.tcc_cstring <- function(x, ...) {
  if (!is.null(x$cached_string)) {
    return(x$cached_string)
  }
  tcc_read_cstring(x$ptr)
}

#' @export
print.tcc_cstring <- function(x, ...) {
  str <- as.character(x)
  cat("<tcc_cstring> \"", str, "\"\n", sep = "")
  invisible(x)
}

# Helper to read C string from external pointer
read_c_string <- function(ptr) {
  tcc_read_cstring(ptr)
}

# ============================================================================
# Struct, Union, Enum Support
# ============================================================================

#' Declare struct for FFI helper generation
#'
#' Generate R-callable helpers for struct allocation, field access,
#' and pointer management. The struct must be defined in a header.
#'
#' @param ffi A tcc_ffi object
#' @param name Struct name (as defined in C header)
#' @param accessors Named list of field accessors where
#'   names are field names and values are FFI types (e.g., list(x="f64", y="f64"))
#' @return Updated tcc_ffi object
#' @export
#' @examples
#' \dontrun{
#' ffi <- tcc_ffi() |>
#'   tcc_header("#include <point.h>") |>
#'   tcc_struct("point", list(x = "f64", y = "f64", id = "i32"))
#' }
tcc_struct <- function(ffi, name, accessors) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  if (!is.character(name) || length(name) != 1) {
    stop("Struct name must be a single string", call. = FALSE)
  }

  if (!is.list(accessors) && !is.character(accessors)) {
    stop(
      "Accessors must be a named list or named character vector",
      call. = FALSE
    )
  }

  # Convert character vector to list if needed
  if (is.character(accessors)) {
    accessors <- as.list(accessors)
  }

  # Validate field types
  for (field_name in names(accessors)) {
    field_type <- accessors[[field_name]]
    # Handle complex field specs like list(type="cstring", size=20)
    if (is.list(field_type)) {
      type_name <- field_type$type %||% "ptr"
      if (isTRUE(field_type$array)) {
        if (is.null(field_type$size) || !is.numeric(field_type$size)) {
          stop(
            "Array field '",
            field_name,
            "' must include numeric 'size'",
            call. = FALSE
          )
        }
      }
    } else {
      type_name <- field_type
    }

    # Allow "struct:name" for nested structs
    if (!grepl("^struct:", type_name)) {
      check_ffi_type(
        type_name,
        paste0("struct '", name, "' field '", field_name, "'")
      )
    }
  }

  # Store struct definition
  if (is.null(ffi$structs)) {
    ffi$structs <- list()
  }
  ffi$structs[[name]] <- accessors

  ffi
}

#' Declare union for FFI helper generation
#'
#' Generate R-callable helpers for union allocation and member access.
#' The union must be defined in a header.
#'
#' @param ffi A tcc_ffi object
#' @param name Union name (as defined in C header)
#' @param members Named list of union members with FFI types
#' @param active Default active member for accessors
#' @return Updated tcc_ffi object
#' @export
#' @examples
#' \dontrun{
#' ffi <- tcc_ffi() |>
#'   tcc_union("data_variant",
#'     members = list(as_int = "i32", as_float = "f32"),
#'     active = "as_int"
#'   )
#' }
tcc_union <- function(ffi, name, members, active = NULL) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  # Validate member types
  for (mem_name in names(members)) {
    mem_type <- members[[mem_name]]
    if (is.list(mem_type)) {
      # Complex member like struct inside union
      if (!is.null(mem_type$type) && mem_type$type == "struct") {
        # Valid - nested struct
      } else {
        type_name <- mem_type$type %||% "ptr"
        check_ffi_type(
          type_name,
          paste0("union '", name, "' member '", mem_name, "'")
        )
      }
    } else {
      check_ffi_type(
        mem_type,
        paste0("union '", name, "' member '", mem_name, "'")
      )
    }
  }

  if (is.null(ffi$unions)) {
    ffi$unions <- list()
  }
  ffi$unions[[name]] <- list(members = members, active = active)

  ffi
}

#' Declare enum for FFI helper generation
#'
#' Generate R-callable helpers for enum constants and type conversions.
#' The enum must be defined in a header.
#'
#' @param ffi A tcc_ffi object
#' @param name Enum name (as defined in C header)
#' @param constants Character vector of constant names to export
#' @param export_constants Whether to export enum constants as R functions
#' @return Updated tcc_ffi object
#' @export
#' @examples
#' \dontrun{
#' ffi <- tcc_ffi() |>
#'   tcc_header("#include <errors.h>") |>
#'   tcc_enum("error_code", constants = c("OK", "ERROR"), export_constants = TRUE)
#' }
tcc_enum <- function(ffi, name, constants = NULL, export_constants = FALSE) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  if (is.null(ffi$enums)) {
    ffi$enums <- list()
  }
  ffi$enums[[name]] <- list(
    constants = constants,
    export_constants = export_constants || !is.null(constants)
  )

  ffi
}

#' Generate container_of helper for struct member
#'
#' Creates a function that recovers the parent struct pointer from
#' a pointer to one of its members. This is the classic Linux kernel
#' container_of macro made accessible from R.
#'
#' @param ffi A tcc_ffi object
#' @param struct_name Struct name
#' @param member_name Member field name to compute offset from
#' @return Updated tcc_ffi object
#' @export
#' @examples
#' \dontrun{
#' ffi <- tcc_ffi() |>
#'   tcc_struct("student", list(id = "i32", marks = "i32")) |>
#'   tcc_container_of("student", "marks") # Creates struct_student_from_marks()
#' }
tcc_container_of <- function(ffi, struct_name, member_name) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  if (is.null(ffi$container_of)) {
    ffi$container_of <- list()
  }
  ffi$container_of[[struct_name]] <- c(
    ffi$container_of[[struct_name]],
    member_name
  )

  ffi
}

#' Generate field address getter helpers
#'
#' Creates functions that return pointers to specific struct fields.
#' Useful for passing field pointers to C functions or for container_of.
#'
#' @param ffi A tcc_ffi object
#' @param struct_name Struct name
#' @param fields Character vector of field names
#' @return Updated tcc_ffi object
#' @export
#' @examples
#' \dontrun{
#' ffi <- tcc_ffi() |>
#'   tcc_struct("point", list(x = "f64", y = "f64")) |>
#'   tcc_field_addr("point", c("x", "y")) # point_x_addr(), point_y_addr()
#' }
tcc_field_addr <- function(ffi, struct_name, fields) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  if (is.null(ffi$field_addr)) {
    ffi$field_addr <- list()
  }
  ffi$field_addr[[struct_name]] <- c(
    ffi$field_addr[[struct_name]],
    fields
  )

  ffi
}

#' Enable raw byte access for struct
#'
#' Generates helper functions to read/write raw bytes from struct memory.
#' Useful for bitwise operations, debugging, or manual serialization.
#'
#' @param ffi A tcc_ffi object
#' @param struct_name Struct name
#' @return Updated tcc_ffi object
#' @export
tcc_struct_raw_access <- function(ffi, struct_name) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  if (is.null(ffi$struct_raw_access)) {
    ffi$struct_raw_access <- character()
  }
  ffi$struct_raw_access <- c(ffi$struct_raw_access, struct_name)

  ffi
}

#' Enable introspection helpers
#'
#' Generates sizeof, alignof, and offsetof helper functions for
#' structs, unions, and enums. Useful for debugging or when you need
#' to know C layout information from R.
#'
#' @param ffi A tcc_ffi object
#' @return Updated tcc_ffi object
#' @export
tcc_introspect <- function(ffi) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  ffi$introspect <- TRUE
  ffi
}
