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
      wrapper_symbols = character(0)
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
#' @param library Library name (e.g., "m", "sqlite3")
#' @return Updated tcc_ffi object (for chaining)
#' @export
tcc_library <- function(ffi, library) {
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }
  ffi$libraries <- c(ffi$libraries, library)
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
    check_ffi_type(sym$returns, paste0("symbol '", sym_name, "' return"))

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

  if (length(ffi$symbols) == 0) {
    stop(
      "No symbols defined. Use tcc_bind() to define symbols first.",
      call. = FALSE
    )
  }

  # Generate C code (always R API mode now)
  c_code <- generate_ffi_code(
    symbols = ffi$symbols,
    headers = ffi$headers,
    c_code = ffi$c_code,
    is_external = FALSE
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

  # Relocate
  result <- tcc_relocate(state)
  if (result != 0) {
    stop("Failed to relocate compiled code", call. = FALSE)
  }

  # Create callable object
  compiled <- tcc_compiled_object(state, ffi$symbols, ffi$output)

  compiled
}

# Create compiled object with callable functions
tcc_compiled_object <- function(state, symbols, output) {
  # Build wrapper names
  wrapper_names <- paste0("R_wrap_", names(symbols))

  # Create environment with callable functions
  env <- new.env(parent = emptyenv())

  for (i in seq_along(symbols)) {
    sym_name <- names(symbols)[i]
    wrapper_name <- wrapper_names[i]
    sym <- symbols[[i]]
    sym$name <- sym_name

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
      .Call(fn_ptr)
    } else if (n_args == 1) {
      .Call(fn_ptr, args[[1]])
    } else if (n_args == 2) {
      .Call(fn_ptr, args[[1]], args[[2]])
    } else if (n_args == 3) {
      .Call(fn_ptr, args[[1]], args[[2]], args[[3]])
    } else if (n_args == 4) {
      .Call(fn_ptr, args[[1]], args[[2]], args[[3]], args[[4]])
    } else if (n_args == 5) {
      .Call(fn_ptr, args[[1]], args[[2]], args[[3]], args[[4]], args[[5]])
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

# Helper for %||%
`%||%` <- function(x, y) if (is.null(x)) y else x

# Platform-dependent library search paths
tcc_platform_lib_paths <- function() {
  sysname <- Sys.info()["sysname"]

  switch(
    sysname,
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
    lib_name <- switch(
      sysname,
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
#'   user_code = '
#'     #include <math.h>
#'
#'     // Helper function that validates input before calling sqrt
#'     double safe_sqrt(double x) {
#'       if (x < 0) {
#'         return NAN;
#'       }
#'       return sqrt(x);
#'     }
#'   ',
#'   libs = "m"
#' )
#' math_with_helpers$safe_sqrt(16.0)
#' math_with_helpers$safe_sqrt(-4.0)  # Returns NaN for negative input
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
    is_external = TRUE
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

  # Relocate
  result <- tcc_relocate(state)
  if (result != 0) {
    stop("Failed to relocate compiled code for ", basename(path), call. = FALSE)
  }

  # Create compiled object
  compiled <- tcc_compiled_object(state, ffi$symbols, "memory")

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
#' @param owned Whether R should free the C memory when done
#' @return A tcc_cstring object
#' @export
tcc_cstring <- function(ptr, clone = TRUE, owned = FALSE) {
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
      obj$cached_string <- read_c_string(ptr)
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
  read_c_string(x$ptr)
}

#' @export
print.tcc_cstring <- function(x, ...) {
  str <- as.character(x)
  cat("<tcc_cstring> \"", str, "\"\n", sep = "")
  invisible(x)
}

# Helper to read C string from external pointer
# This would need a C implementation
read_c_string <- function(ptr) {
  # Placeholder - would use C code to read null-terminated string
  stop("read_c_string not yet implemented - requires C helper", call. = FALSE)
}
