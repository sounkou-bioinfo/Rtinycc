# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# C Wrapper Code Generator for API Mode
# Generates C code that converts R SEXP to C types and back

# Generate C code to extract R SEXP to C type
generate_c_input <- function(arg_name, r_name, ffi_type) {
  check_ffi_type(ffi_type, paste0("argument '", arg_name, "'"))
  special <- ffi_input_special_rule(ffi_type, arg_name, r_name)
  if (!is.null(special)) {
    return(special)
  }
  ffi_input_rule(ffi_type, arg_name, r_name)
}

# Generate C code to convert C return value to R SEXP
generate_c_return <- function(value_expr, ffi_type, arg_names = character()) {
  if (is.list(ffi_type)) {
    if (is.null(ffi_type$type)) {
      stop("Return spec missing 'type'", call. = FALSE)
    }
    base_type <- ffi_type$type
    type_info <- check_ffi_type(base_type, "return value")

    if (!is.null(type_info$kind) && type_info$kind == "array") {
      len_arg <- ffi_type$length_arg
      if (is.null(len_arg) || !is.numeric(len_arg)) {
        stop("Array return requires numeric 'length_arg'", call. = FALSE)
      }
      if (len_arg < 1 || len_arg > length(arg_names)) {
        stop("length_arg out of range", call. = FALSE)
      }
      len_name <- arg_names[[as.integer(len_arg)]]

      alloc_line <- array_return_alloc_line(base_type, len_name)
      copy_line <- array_return_copy_line(base_type, len_name, value_expr)

      free_line <- if (isTRUE(ffi_type$free)) {
        sprintf("  if (%s) free(%s);", value_expr, value_expr)
      } else {
        NULL
      }

      return(paste(
        c(
          sprintf("if (!%s) return R_NilValue;", value_expr),
          alloc_line,
          copy_line,
          free_line,
          "  UNPROTECT(1);",
          "  return out;"
        ),
        collapse = "\n"
      ))
    }

    ffi_type <- base_type
  }

  check_ffi_type(ffi_type, "return value")
  special <- ffi_return_special_rule(ffi_type, value_expr)
  if (!is.null(special)) {
    return(special)
  }
  ffi_return_rule(ffi_type, value_expr)
}

# Callback helper: generate a unique trampoline name per wrapper argument
callback_trampoline_name <- function(wrapper_name, arg_index) {
  paste0("trampoline_", wrapper_name, "_arg", arg_index)
}

variadic_wrapper_name <- function(wrapper_name, n_varargs) {
  paste0(wrapper_name, "__v", as.integer(n_varargs))
}

variadic_type_token <- function(x) {
  gsub("[^A-Za-z0-9_]", "_", x)
}

variadic_signature_key <- function(vararg_types) {
  if (length(vararg_types) == 0) {
    return("__none__")
  }
  paste(vararg_types, collapse = "|")
}

variadic_wrapper_name_types <- function(wrapper_name, vararg_types) {
  suffix <- if (length(vararg_types) == 0) {
    "none"
  } else {
    paste(
      vapply(vararg_types, variadic_type_token, character(1)),
      collapse = "__"
    )
  }
  paste0(wrapper_name, "__v", length(vararg_types), "__", suffix)
}

generate_variadic_type_sequences <- function(allowed_types, n_varargs) {
  if (n_varargs == 0) {
    return(list(list()))
  }
  if (length(allowed_types) == 0) {
    return(list())
  }

  idx_grid <- expand.grid(
    rep(list(seq_along(allowed_types)), n_varargs),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  lapply(seq_len(nrow(idx_grid)), function(i) {
    as.list(unname(unlist(allowed_types[as.integer(idx_grid[i, ])])))
  })
}

# Callback helper: declare a function pointer bound to a trampoline
callback_funptr_decl <- function(arg_name, sig, trampoline_name) {
  arg_types <- sig$arg_types
  c_args <- if (length(arg_types) > 0) {
    paste(c("void*", arg_types), collapse = ", ")
  } else {
    "void*"
  }
  sprintf(
    "  %s (*%s)(%s) = %s;",
    sig$return_type,
    arg_name,
    c_args,
    trampoline_name
  )
}

# Generate full C wrapper function (SEXP-based only)
generate_c_wrapper <- function(
  symbol_name,
  wrapper_name,
  arg_types,
  vararg_types = list(),
  return_type,
  c_code = NULL,
  is_external = FALSE
) {
  n_args <- length(arg_types) + length(vararg_types)
  return_info <- check_ffi_type(
    if (is.list(return_type)) return_type$type else return_type,
    "return value"
  )

  # Always use R API mode: SEXP arguments and return
  # Build argument list
  if (n_args > 0) {
    fixed_arg_names <- names(arg_types)
    if (is.null(fixed_arg_names) || all(fixed_arg_names == "")) {
      fixed_arg_names <- paste0("arg", seq_along(arg_types))
    }
    vararg_names <- if (length(vararg_types) > 0) {
      paste0("vararg", seq_along(vararg_types))
    } else {
      character(0)
    }
    arg_names <- c(fixed_arg_names, vararg_names)
    all_arg_types <- c(arg_types, vararg_types)
    r_arg_names <- paste0("arg", seq_len(n_args), "_")

    # Input conversions from SEXP to C types
    input_lines <- character(0)
    for (i in seq_len(n_args)) {
      ffi_type <- all_arg_types[[i]]
      arg_name <- arg_names[[i]]
      r_name <- r_arg_names[[i]]

      if (is_callback_type(ffi_type)) {
        sig <- parse_callback_type(ffi_type)
        if (is.null(sig)) {
          stop(
            "callback type requires signature, e.g. callback:double(int)",
            call. = FALSE
          )
        }
        tramp_name <- callback_trampoline_name(
          paste0("R_wrap_", symbol_name),
          i
        )
        input_lines <- c(
          input_lines,
          sprintf(
            "  if (!Rf_inherits(%s, \"tcc_callback\")) Rf_error(\"expected tcc_callback for argument '%s'\");",
            r_name,
            arg_name
          ),
          callback_funptr_decl(arg_name, sig, tramp_name)
        )
      } else {
        input_lines <- c(
          input_lines,
          generate_c_input(arg_name, r_name, ffi_type)
        )
      }
    }

    input_conversions <- paste(input_lines, collapse = "\n")

    # Call the actual function
    call_expr <- sprintf(
      "%s(%s)",
      symbol_name,
      paste(arg_names, collapse = ", ")
    )
  } else {
    input_conversions <- "  // No arguments"
    r_arg_names <- character(0)
    call_expr <- sprintf("%s()", symbol_name)
  }

  # Return conversion from C type to SEXP
  return_conversion <- generate_c_return(call_expr, return_type, arg_names)

  # Build the wrapper with SEXP signature
  wrapper <- c(
    sprintf(
      "SEXP %s(%s) {",
      wrapper_name,
      if (n_args > 0) {
        paste0("SEXP ", r_arg_names, collapse = ", ")
      } else {
        "void"
      }
    ),
    input_conversions,
    "",
    "  // Call and return",
    paste("  ", strsplit(return_conversion, "\n")[[1]], collapse = "\n"),
    "}"
  )

  paste(wrapper, collapse = "\n")
}

# Generate header for external library (declare external functions)
generate_external_declarations <- function(symbols) {
  decls <- c()
  for (sym_name in names(symbols)) {
    sym <- symbols[[sym_name]]
    return_info <- check_ffi_type(
      if (is.list(sym$returns)) sym$returns$type else sym$returns,
      paste0("symbol '", sym_name, "' return")
    )

    arg_types <- sym$args %||% list()
    if (length(arg_types) > 0) {
      arg_info <- lapply(
        arg_types,
        check_ffi_type,
        context = paste0("symbol '", sym_name, "' argument")
      )
      arg_decls <- paste(
        sapply(arg_info, function(x) x$c_type),
        collapse = ", "
      )
    } else {
      arg_decls <- "void"
    }

    if (isTRUE(sym$variadic)) {
      decl <- sprintf(
        "extern %s %s(%s, ...);",
        return_info$c_type,
        sym_name,
        arg_decls
      )
    } else {
      decl <- sprintf(
        "extern %s %s(%s);",
        return_info$c_type,
        sym_name,
        arg_decls
      )
    }
    decls <- c(decls, decl)
  }

  paste(decls, collapse = "\n")
}

# Generate all wrappers for a set of symbols
generate_wrappers <- function(
  symbols,
  prefix = "R_wrap_",
  is_external = FALSE
) {
  wrappers <- c()

  for (sym_name in names(symbols)) {
    sym <- symbols[[sym_name]]
    base_wrapper_name <- paste0(prefix, sym_name)

    if (isTRUE(sym$variadic)) {
      vararg_mode <- sym$varargs_mode %||% "prefix"

      if (identical(vararg_mode, "types")) {
        allowed_types <- sym$varargs_types %||% list()
        min_varargs <- sym$varargs_min %||% 0L
        max_varargs <- sym$varargs_max %||% min_varargs

        for (n_varargs in seq.int(min_varargs, max_varargs)) {
          type_sequences <- generate_variadic_type_sequences(
            allowed_types,
            n_varargs
          )

          for (this_varargs in type_sequences) {
            wrapper <- generate_c_wrapper(
              symbol_name = sym_name,
              wrapper_name = variadic_wrapper_name_types(
                base_wrapper_name,
                this_varargs
              ),
              arg_types = sym$args,
              vararg_types = this_varargs,
              return_type = sym$returns,
              is_external = is_external
            )

            wrappers <- c(wrappers, wrapper, "", "")
          }
        }
      } else {
        all_varargs <- sym$varargs %||% list()
        max_varargs <- length(all_varargs)
        min_varargs <- sym$varargs_min %||% max_varargs

        for (n_varargs in seq.int(min_varargs, max_varargs)) {
          this_varargs <- if (n_varargs > 0) {
            all_varargs[seq_len(n_varargs)]
          } else {
            list()
          }

          wrapper <- generate_c_wrapper(
            symbol_name = sym_name,
            wrapper_name = variadic_wrapper_name(base_wrapper_name, n_varargs),
            arg_types = sym$args,
            vararg_types = this_varargs,
            return_type = sym$returns,
            is_external = is_external
          )

          wrappers <- c(wrappers, wrapper, "", "")
        }
      }
    } else {
      wrapper <- generate_c_wrapper(
        symbol_name = sym_name,
        wrapper_name = base_wrapper_name,
        arg_types = sym$args,
        vararg_types = sym$varargs %||% list(),
        return_type = sym$returns,
        is_external = is_external
      )

      wrappers <- c(wrappers, wrapper, "", "")
    }
  }

  paste(wrappers, collapse = "\n")
}

# ============================================================================
# Struct Helper Generation
# ============================================================================

# Generate struct helper functions (new, free, get/set, container_of, etc.)
generate_struct_helpers <- function(
  structs,
  container_of,
  field_addr,
  raw_access,
  introspect
) {
  if (is.null(structs) || length(structs) == 0) {
    return("")
  }

  helpers <- c("/* Struct helper functions */", "")

  for (struct_name in names(structs)) {
    fields <- structs[[struct_name]]

    # Constructor
    helpers <- c(helpers, generate_struct_new(struct_name))

    # Free function
    helpers <- c(helpers, generate_struct_free(struct_name))

    # Field getters and setters
    for (field_name in names(fields)) {
      field_spec <- fields[[field_name]]
      is_array <- is.list(field_spec) && isTRUE(field_spec$array)
      helpers <- c(
        helpers,
        generate_struct_getter(struct_name, field_name, field_spec)
      )
      if (is_array) {
        helpers <- c(
          helpers,
          generate_struct_array_getter(struct_name, field_name, field_spec),
          generate_struct_array_setter(struct_name, field_name, field_spec)
        )
      } else {
        helpers <- c(
          helpers,
          generate_struct_setter(struct_name, field_name, field_spec)
        )
      }
    }

    # container_of helpers
    if (!is.null(container_of) && struct_name %in% names(container_of)) {
      for (member_name in container_of[[struct_name]]) {
        helpers <- c(helpers, generate_container_of(struct_name, member_name))
      }
    }

    # Field address helpers
    if (!is.null(field_addr) && struct_name %in% names(field_addr)) {
      for (field_name in field_addr[[struct_name]]) {
        helpers <- c(helpers, generate_field_addr(struct_name, field_name))
      }
    }

    # Raw access helpers
    if (!is.null(raw_access) && struct_name %in% raw_access) {
      helpers <- c(helpers, generate_struct_raw_access(struct_name))
    }

    # Introspection helpers
    if (!is.null(introspect) && introspect) {
      helpers <- c(helpers, generate_struct_introspection(struct_name, fields))
    }

    helpers <- c(helpers, "")
  }

  paste(helpers, collapse = "\n")
}

# Generate constructor
generate_struct_new <- function(struct_name) {
  c(
    sprintf("SEXP R_wrap_struct_%s_new(void) {", struct_name),
    sprintf(
      "  struct %s *p = calloc(1, sizeof(struct %s));",
      struct_name,
      struct_name
    ),
    "  if (!p) Rf_error(\"Out of memory\");",
    sprintf(
      "  SEXP ext = R_MakeExternalPtr(p, Rf_install(\"struct_%s\"), R_NilValue);",
      struct_name
    ),
    "  R_RegisterCFinalizerEx(ext, RC_free_finalizer, FALSE);",
    "  return ext;",
    "}",
    ""
  )
}

# Generate free function
generate_struct_free <- function(struct_name) {
  c(
    sprintf("SEXP R_wrap_struct_%s_free(SEXP ext) {", struct_name),
    "  RC_free_finalizer(ext);",
    "  return R_NilValue;",
    "}",
    ""
  )
}

# Generate field getter
generate_struct_getter <- function(struct_name, field_name, field_spec) {
  if (is.list(field_spec) && isTRUE(field_spec$array)) {
    return(c(
      sprintf(
        "SEXP R_wrap_struct_%s_get_%s(SEXP ext) {",
        struct_name,
        field_name
      ),
      sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
      sprintf("  if (!p) Rf_error(\"Null pointer\");"),
      sprintf(
        "  return R_MakeExternalPtr(p->%s, R_NilValue, ext);",
        field_name
      ),
      "}",
      ""
    ))
  }
  if (is.list(field_spec)) {
    type_name <- field_spec$type %||% "ptr"
  } else {
    type_name <- field_spec
  }

  return_code <- struct_field_getter_lines(type_name, field_name)

  c(
    sprintf(
      "SEXP R_wrap_struct_%s_get_%s(SEXP ext) {",
      struct_name,
      field_name
    ),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    paste("  ", return_code, collapse = "\n"),
    "}",
    ""
  )
}

generate_struct_array_getter <- function(struct_name, field_name, field_spec) {
  type_name <- field_spec$type %||% "u8"
  size <- field_spec$size
  if (is.null(size) || !is.numeric(size)) {
    stop("Array field '", field_name, "' missing size", call. = FALSE)
  }

  return_code <- struct_array_field_getter_lines(type_name, field_name)

  c(
    sprintf(
      "SEXP R_wrap_struct_%s_get_%s_elt(SEXP ext, SEXP idx_) {",
      struct_name,
      field_name
    ),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    "  int idx = asInteger(idx_);",
    sprintf(
      "  if (idx < 0 || idx >= %d) Rf_error(\"index out of bounds\");",
      as.integer(size)
    ),
    paste("  ", return_code, collapse = "\n"),
    "}",
    ""
  )
}

generate_struct_array_setter <- function(struct_name, field_name, field_spec) {
  type_name <- field_spec$type %||% "u8"
  size <- field_spec$size
  if (is.null(size) || !is.numeric(size)) {
    stop("Array field '", field_name, "' missing size", call. = FALSE)
  }

  setter_code <- struct_array_field_setter_lines(type_name, field_name)

  c(
    sprintf(
      "SEXP R_wrap_struct_%s_set_%s_elt(SEXP ext, SEXP idx_, SEXP val) {",
      struct_name,
      field_name
    ),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    "  int idx = asInteger(idx_);",
    sprintf(
      "  if (idx < 0 || idx >= %d) Rf_error(\"index out of bounds\");",
      as.integer(size)
    ),
    paste("  ", setter_code, collapse = "\n"),
    "  return ext;",
    "}",
    ""
  )
}

# Generate field setter
generate_struct_setter <- function(struct_name, field_name, field_spec) {
  if (is.list(field_spec)) {
    type_name <- field_spec$type %||% "ptr"
    size <- field_spec$size
  } else {
    type_name <- field_spec
    size <- NULL
  }

  setter_code <- struct_field_setter_lines(type_name, field_name, size)

  c(
    sprintf(
      "SEXP R_wrap_struct_%s_set_%s(SEXP ext, SEXP val) {",
      struct_name,
      field_name
    ),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    paste("  ", setter_code, collapse = "\n"),
    "  return ext;", # Return ext for chaining
    "}",
    ""
  )
}

# Generate container_of helper
generate_container_of <- function(struct_name, member_name) {
  c(
    sprintf(
      "SEXP R_wrap_struct_%s_from_%s(SEXP ext) {",
      struct_name,
      member_name
    ),
    sprintf("  void *member_ptr = R_ExternalPtrAddr(ext);"),
    sprintf("  if (!member_ptr) Rf_error(\"Null pointer\");"),
    sprintf(
      "  struct %s *p = (struct %s *)((char *)member_ptr - offsetof(struct %s, %s));",
      struct_name,
      struct_name,
      struct_name,
      member_name
    ),
    sprintf(
      "  return R_MakeExternalPtr(p, Rf_install(\"struct_%s\"), R_NilValue);",
      struct_name
    ),
    "}",
    ""
  )
}

# Generate field address getter
generate_field_addr <- function(struct_name, field_name) {
  c(
    sprintf(
      "SEXP R_wrap_struct_%s_%s_addr(SEXP ext) {",
      struct_name,
      field_name
    ),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    sprintf("  void *field_ptr = &p->%s;", field_name),
    sprintf("  return R_MakeExternalPtr(field_ptr, R_NilValue, ext);"),
    "}",
    ""
  )
}

# Generate raw access helpers
generate_struct_raw_access <- function(struct_name) {
  c(
    sprintf("SEXP R_wrap_struct_%s_get_raw(SEXP ext, SEXP len) {", struct_name),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    "  int n = asInteger(len);",
    "  SEXP raw = PROTECT(allocVector(RAWSXP, n));",
    sprintf(
      "  memcpy(RAW(raw), p, (n < sizeof(struct %s)) ? n : sizeof(struct %s));",
      struct_name,
      struct_name
    ),
    "  UNPROTECT(1);",
    "  return raw;",
    "}",
    "",
    sprintf("SEXP R_wrap_struct_%s_set_raw(SEXP ext, SEXP raw) {", struct_name),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    "  int n = LENGTH(raw);",
    sprintf(
      "  memcpy(p, RAW(raw), (n < sizeof(struct %s)) ? n : sizeof(struct %s));",
      struct_name,
      struct_name
    ),
    "  return R_NilValue;",
    "}",
    ""
  )
}

# Generate introspection helpers
generate_struct_introspection <- function(struct_name, fields) {
  c(
    sprintf("SEXP R_wrap_struct_%s_sizeof(void) {", struct_name),
    sprintf("  return ScalarInteger(sizeof(struct %s));", struct_name),
    "}",
    "",
    sprintf("SEXP R_wrap_struct_%s_alignof(void) {", struct_name),
    sprintf("  return ScalarInteger(_Alignof(struct %s));", struct_name),
    "}",
    ""
  )
}

# ============================================================================
# Union Helper Generation
# ============================================================================

generate_union_helpers <- function(unions, introspect) {
  if (is.null(unions) || length(unions) == 0) {
    return("")
  }

  helpers <- c("/* Union helper functions */", "")

  for (union_name in names(unions)) {
    union_def <- unions[[union_name]]
    members <- union_def$members

    # Constructor
    helpers <- c(helpers, generate_union_new(union_name))

    # Free function
    helpers <- c(helpers, generate_union_free(union_name))

    # Member getters and setters
    for (mem_name in names(members)) {
      mem_spec <- members[[mem_name]]
      helpers <- c(
        helpers,
        generate_union_getter(union_name, mem_name, mem_spec)
      )
      helpers <- c(
        helpers,
        generate_union_setter(union_name, mem_name, mem_spec)
      )
    }

    # Introspection
    if (!is.null(introspect) && introspect) {
      helpers <- c(helpers, generate_union_introspection(union_name))
    }

    helpers <- c(helpers, "")
  }

  paste(helpers, collapse = "\n")
}

generate_union_new <- function(union_name) {
  c(
    sprintf("SEXP R_wrap_union_%s_new(void) {", union_name),
    sprintf(
      "  union %s *p = calloc(1, sizeof(union %s));",
      union_name,
      union_name
    ),
    "  if (!p) Rf_error(\"Out of memory\");",
    sprintf(
      "  SEXP ext = R_MakeExternalPtr(p, Rf_install(\"union_%s\"), R_NilValue);",
      union_name
    ),
    "  R_RegisterCFinalizerEx(ext, RC_free_finalizer, FALSE);",
    "  return ext;",
    "}",
    ""
  )
}

generate_union_free <- function(union_name) {
  c(
    sprintf("SEXP R_wrap_union_%s_free(SEXP ext) {", union_name),
    "  RC_free_finalizer(ext);",
    "  return R_NilValue;",
    "}",
    ""
  )
}

generate_union_getter <- function(union_name, mem_name, mem_spec) {
  if (
    is.list(mem_spec) && !is.null(mem_spec$type) && mem_spec$type == "struct"
  ) {
    # Nested struct in union
    return(c(
      sprintf("SEXP R_wrap_union_%s_get_%s(SEXP ext) {", union_name, mem_name),
      sprintf("  union %s *p = R_ExternalPtrAddr(ext);", union_name),
      sprintf("  if (!p) Rf_error(\"Null pointer\");"),
      sprintf("  return R_MakeExternalPtr(&p->%s, R_NilValue, ext);", mem_name),
      "}",
      ""
    ))
  }

  type_name <- if (is.list(mem_spec)) mem_spec$type else mem_spec
  return_code <- union_field_getter_lines(type_name, mem_name)

  c(
    sprintf("SEXP R_wrap_union_%s_get_%s(SEXP ext) {", union_name, mem_name),
    sprintf("  union %s *p = R_ExternalPtrAddr(ext);", union_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    paste("  ", return_code, collapse = "\n"),
    "}",
    ""
  )
}

array_return_alloc_line <- function(base_type, len_name) {
  array_return_alloc_line_rule(base_type, len_name)
}

array_return_copy_line <- function(base_type, len_name, value_expr) {
  array_return_copy_line_rule(base_type, len_name, value_expr)
}

struct_field_getter_lines <- function(type_name, field_name) {
  struct_field_getter_rule(type_name, field_name)
}

struct_array_field_getter_lines <- function(type_name, field_name) {
  struct_array_field_getter_rule(type_name, field_name)
}

struct_array_field_setter_lines <- function(type_name, field_name) {
  struct_array_field_setter_rule(type_name, field_name)
}

struct_field_setter_lines <- function(type_name, field_name, size) {
  struct_field_setter_rule(type_name, field_name, size)
}

union_field_getter_lines <- function(type_name, mem_name) {
  union_field_getter_rule(type_name, mem_name)
}

union_field_setter_lines <- function(type_name, mem_name, size) {
  union_field_setter_rule(type_name, mem_name, size)
}

ffi_input_special_rule <- function(ffi_type, arg_name, r_name) {
  special_map <- list(
    "enum:" = function() ffi_input_enum_rule(ffi_type, arg_name, r_name),
    "callback" = function() ffi_input_callback_rule(ffi_type, arg_name, r_name)
  )
  matched <- names(special_map)[vapply(
    names(special_map),
    startsWith,
    logical(1),
    x = ffi_type
  )]
  if (length(matched) == 0) {
    return(NULL)
  }
  special_map[[matched[[1]]]]()
}

ffi_return_special_rule <- function(ffi_type, value_expr) {
  special_map <- list(
    "enum:" = function() ffi_return_enum_rule(ffi_type, value_expr)
  )
  matched <- names(special_map)[vapply(
    names(special_map),
    startsWith,
    logical(1),
    x = ffi_type
  )]
  if (length(matched) == 0) {
    return(NULL)
  }
  special_map[[matched[[1]]]]()
}

generate_union_setter <- function(union_name, mem_name, mem_spec) {
  if (
    is.list(mem_spec) && !is.null(mem_spec$type) && mem_spec$type == "struct"
  ) {
    # TODO : this looks fake, we could allocate memcopy another memory address into it
    return(c()) # Cannot set whole struct easily
  }

  type_name <- if (is.list(mem_spec)) mem_spec$type else mem_spec

  size <- if (is.list(mem_spec)) mem_spec$size else NULL
  setter_code <- union_field_setter_lines(type_name, mem_name, size)

  c(
    sprintf(
      "SEXP R_wrap_union_%s_set_%s(SEXP ext, SEXP val) {",
      union_name,
      mem_name
    ),
    sprintf("  union %s *p = R_ExternalPtrAddr(ext);", union_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    paste("  ", setter_code, collapse = "\n"),
    "  return ext;",
    "}",
    ""
  )
}

generate_union_introspection <- function(union_name) {
  c(
    sprintf("SEXP R_wrap_union_%s_sizeof(void) {", union_name),
    sprintf("  return ScalarInteger(sizeof(union %s));", union_name),
    "}",
    "",
    sprintf("SEXP R_wrap_union_%s_alignof(void) {", union_name),
    sprintf("  return ScalarInteger(_Alignof(union %s));", union_name),
    "}",
    ""
  )
}

# ============================================================================
# Enum Helper Generation
# ============================================================================

generate_enum_helpers <- function(enums, introspect) {
  if (is.null(enums) || length(enums) == 0) {
    return("")
  }

  helpers <- c("/* Enum helper functions */", "")

  for (enum_name in names(enums)) {
    enum_def <- enums[[enum_name]]

    # Sizeof helper
    if (!is.null(introspect) && introspect) {
      helpers <- c(
        helpers,
        c(
          sprintf("SEXP R_wrap_enum_%s_sizeof(void) {", enum_name),
          sprintf("  return ScalarInteger(sizeof(enum %s));", enum_name),
          "}",
          ""
        )
      )
    }

    # Constant export helpers
    if (!is.null(enum_def$constants)) {
      for (const_name in enum_def$constants) {
        helpers <- c(
          helpers,
          c(
            sprintf("SEXP R_wrap_enum_%s_%s(void) {", enum_name, const_name),
            sprintf("  return ScalarInteger(%s);", const_name),
            "}",
            ""
          )
        )
      }
    }

    helpers <- c(helpers, "")
  }

  paste(helpers, collapse = "\n")
}

# Generate global getter helpers
generate_global_helpers <- function(globals) {
  if (is.null(globals) || length(globals) == 0) {
    return("")
  }

  helpers <- c("/* Global getter helpers */", "")

  for (global_name in names(globals)) {
    ffi_type <- globals[[global_name]]
    type_info <- check_ffi_type(ffi_type, "global")
    if (!is.null(type_info$kind) && type_info$kind == "array") {
      stop("Global type cannot be an array type", call. = FALSE)
    }
    if (ffi_type == "void") {
      stop("Global type cannot be void", call. = FALSE)
    }

    c_type <- type_info$c_type
    get_ret <- generate_c_return(global_name, ffi_type)
    get_ret_lines <- paste0("  ", strsplit(get_ret, "\n")[[1]])

    set_arg <- "value_"
    set_input <- generate_c_input("value", set_arg, ffi_type)
    set_input_lines <- strsplit(set_input, "\n")[[1]]
    set_ret <- generate_c_return(global_name, ffi_type)
    set_ret_lines <- paste0("  ", strsplit(set_ret, "\n")[[1]])

    helpers <- c(
      helpers,
      sprintf("extern %s %s;", c_type, global_name),
      sprintf("SEXP R_wrap_global_%s_get(void) {", global_name),
      get_ret_lines,
      "}",
      "",
      sprintf("SEXP R_wrap_global_%s_set(SEXP %s) {", global_name, set_arg),
      set_input_lines,
      sprintf("  %s = value;", global_name),
      set_ret_lines,
      "}",
      ""
    )
  }

  paste(helpers, collapse = "\n")
}

# ============================================================================
# Main Code Generation
# ============================================================================

# Generate complete C code for FFI compilation (always R API mode)
generate_ffi_code <- function(
  symbols,
  headers = NULL,
  c_code = NULL,
  is_external = FALSE,
  structs = NULL,
  unions = NULL,
  enums = NULL,
  globals = NULL,
  container_of = NULL,
  field_addr = NULL,
  struct_raw_access = NULL,
  introspect = NULL
) {
  parts <- c()

  # TinyCC workaround: Define _Complex as empty since TinyCC doesn't support C99 complex types
  # This is required when using R headers which use _Complex
  parts <- c(
    parts,
    "/* TinyCC workaround: _Complex not supported */",
    "#define _Complex",
    ""
  )

  # Always include R headers for SEXP-based wrappers
  parts <- c(
    parts,
    "#include <R.h>",
    "#include <Rinternals.h>",
    "void RC_free_finalizer(SEXP ext);",
    ""
  )

  parts <- c(
    parts,
    "#include <stdint.h>",
    "#include <stdbool.h>",
    "#include <stddef.h>",
    "#include <limits.h>",
    "#include <math.h>",
    ""
  )

  cb_tramps <- generate_callback_trampolines(symbols)
  if (nzchar(cb_tramps$code)) {
    parts <- c(
      parts,
      "",
      "/* Callback trampoline support */",
      "typedef struct { int id; int refs; } callback_token_t;",
      if (cb_tramps$needs_sync) {
        "SEXP RC_invoke_callback_id(int, SEXP);"
      } else {
        NULL
      },
      if (cb_tramps$needs_async) {
        c(
          "typedef enum {",
          "  CB_ARG_INT = 0,",
          "  CB_ARG_REAL = 1,",
          "  CB_ARG_LOGICAL = 2,",
          "  CB_ARG_PTR = 3,",
          "  CB_ARG_CSTRING = 4",
          "} cb_arg_kind_t;",
          "",
          "typedef struct {",
          "  cb_arg_kind_t kind;",
          "  union { int i; double d; void* p; char* s; } v;",
          "} cb_arg_t;",
          "",
          "int RC_callback_async_schedule_c(int id, int n_args, const cb_arg_t *args);"
        )
      } else {
        NULL
      },
      "",
      cb_tramps$code,
      ""
    )
  }

  # Custom headers
  if (!is.null(headers) && length(headers) > 0) {
    parts <- c(parts, "/* User headers */", headers, "")
  }

  # External declarations
  if (is_external) {
    parts <- c(
      parts,
      "/* External library declarations */",
      generate_external_declarations(symbols),
      ""
    )
  }

  # User C code
  if (!is.null(c_code) && length(c_code) > 0) {
    if (nzchar(c_code) > 0) {
      parts <- c(parts, "/* User code */", c_code, "")
    }
  }

  # Struct helpers
  struct_code <- generate_struct_helpers(
    structs,
    container_of,
    field_addr,
    struct_raw_access,
    introspect
  )
  if (nzchar(struct_code)) {
    parts <- c(parts, struct_code, "")
  }

  # Union helpers
  union_code <- generate_union_helpers(unions, introspect)
  if (nzchar(union_code)) {
    parts <- c(parts, union_code, "")
  }

  # Enum helpers
  enum_code <- generate_enum_helpers(enums, introspect)
  if (nzchar(enum_code)) {
    parts <- c(parts, enum_code, "")
  }

  # Global helpers
  global_code <- generate_global_helpers(globals)
  if (nzchar(global_code)) {
    parts <- c(parts, global_code, "")
  }

  # Wrapper functions for bound symbols
  parts <- c(
    parts,
    "/* R callable wrappers for bound symbols */",
    generate_wrappers(symbols, is_external = is_external)
  )

  paste(parts, collapse = "\n")
}

# Generate trampoline functions for callback arguments
generate_callback_trampolines <- function(symbols) {
  if (is.null(symbols) || length(symbols) == 0) {
    return(list(code = "", needs_sync = FALSE, needs_async = FALSE))
  }

  trampolines <- character(0)
  needs_sync <- FALSE
  needs_async <- FALSE
  for (sym_name in names(symbols)) {
    sym <- symbols[[sym_name]]
    arg_types <- sym$args
    if (length(arg_types) == 0) {
      next
    }
    for (i in seq_along(arg_types)) {
      ffi_type <- arg_types[[i]]
      if (is_callback_type(ffi_type)) {
        sig <- parse_callback_type(ffi_type)
        if (is.null(sig)) {
          stop(
            "callback type requires signature, e.g. callback:double(int)",
            call. = FALSE
          )
        }
        wrapper_name <- paste0("R_wrap_", sym_name)
        tramp_name <- callback_trampoline_name(wrapper_name, i)
        if (is_callback_async_type(ffi_type)) {
          needs_async <- TRUE
          trampolines <- c(
            trampolines,
            generate_async_trampoline(tramp_name, sig),
            ""
          )
        } else {
          needs_sync <- TRUE
          trampolines <- c(
            trampolines,
            generate_trampoline(tramp_name, sig),
            ""
          )
        }
      }
    }
  }

  list(
    code = paste(trampolines, collapse = "\n"),
    needs_sync = needs_sync,
    needs_async = needs_async
  )
}
