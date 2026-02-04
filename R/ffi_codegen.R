# C Wrapper Code Generator for API Mode
# Generates C code that converts R SEXP to C types and back

# Generate C code to extract R SEXP to C type
generate_c_input <- function(arg_name, r_name, ffi_type) {
  type_info <- check_ffi_type(ffi_type, paste0("argument '", arg_name, "'"))

  switch(ffi_type,
    i8 = sprintf("  int8_t %s = (int8_t)asInteger(%s);", arg_name, r_name),
    i16 = sprintf("  int16_t %s = (int16_t)asInteger(%s);", arg_name, r_name),
    i32 = sprintf("  int32_t %s = asInteger(%s);", arg_name, r_name),
    i64 = sprintf("  int64_t %s = (int64_t)REAL(%s)[0];", arg_name, r_name),
    u8 = sprintf(
      "  uint8_t %s = (uint8_t)(asInteger(%s) & 0xFF);",
      arg_name,
      r_name
    ),
    u16 = sprintf(
      "  uint16_t %s = (uint16_t)(asInteger(%s) & 0xFFFF);",
      arg_name,
      r_name
    ),
    u32 = sprintf("  uint32_t %s = (uint32_t)REAL(%s)[0];", arg_name, r_name),
    u64 = sprintf("  uint64_t %s = (uint64_t)REAL(%s)[0];", arg_name, r_name),
    f32 = sprintf("  float %s = (float)asReal(%s);", arg_name, r_name),
    f64 = sprintf("  double %s = asReal(%s);", arg_name, r_name),
    bool = sprintf("  bool %s = (bool)asLogical(%s);", arg_name, r_name),
    cstring = sprintf(
      "  const char* %s = CHAR(STRING_ELT(%s, 0));",
      arg_name,
      r_name
    ),
    ptr = sprintf("  void* %s = R_ExternalPtrAddr(%s);", arg_name, r_name),
    buffer = sprintf("  void* %s = RAW(%s);", arg_name, r_name),
    "buffer<f64>" = sprintf("  double* %s = REAL(%s);", arg_name, r_name),
    "buffer<f32>" = sprintf(
      "  float* %s = (float*)REAL(%s);",
      arg_name,
      r_name
    ),
    "buffer<i32>" = sprintf("  int32_t* %s = INTEGER(%s);", arg_name, r_name),
    "buffer<i64>" = sprintf(
      "  int64_t* %s = (int64_t*)REAL(%s);",
      arg_name,
      r_name
    ),
    "buffer<u8>" = sprintf("  uint8_t* %s = RAW(%s);", arg_name, r_name),
    # Array types - R native vectors (zero-copy)
    raw = sprintf("  uint8_t* %s = RAW(%s);", arg_name, r_name),
    integer_array = sprintf("  int32_t* %s = INTEGER(%s);", arg_name, r_name),
    numeric_array = sprintf("  double* %s = REAL(%s);", arg_name, r_name),
    logical_array = sprintf("  int* %s = LOGICAL(%s);", arg_name, r_name),
    character_array = sprintf("  SEXP* %s = STRING_PTR(%s);", arg_name, r_name),
    sexp = sprintf("  SEXP %s = %s;", arg_name, r_name),
    callback = sprintf("  void* %s = R_ExternalPtrAddr(%s);", arg_name, r_name),
    {
      # Handle enum types
      if (grepl("^enum:", ffi_type)) {
        sprintf("  int %s = asInteger(%s);", arg_name, r_name)
      } else if (grepl("^callback(:|$)", ffi_type)) {
        # Callback type - extract pointer from external ptr
        sprintf("  void* %s = R_ExternalPtrAddr(%s);", arg_name, r_name)
      } else {
        stop("Unsupported FFI type: ", ffi_type, call. = FALSE)
      }
    }
  )
}

# Generate C code to convert C return value to R SEXP
generate_c_return <- function(value_expr, ffi_type) {
  type_info <- check_ffi_type(ffi_type, "return value")

  switch(ffi_type,
    i8 = sprintf("return ScalarInteger((int)%s);", value_expr),
    i16 = sprintf("return ScalarInteger((int)%s);", value_expr),
    i32 = sprintf("return ScalarInteger(%s);", value_expr),
    i64 = sprintf("return ScalarReal((double)%s);", value_expr),
    u8 = sprintf("return ScalarInteger((int)%s);", value_expr),
    u16 = sprintf("return ScalarInteger((int)%s);", value_expr),
    u32 = sprintf("return ScalarReal((double)%s);", value_expr),
    u64 = sprintf("return ScalarReal((double)%s);", value_expr),
    f32 = sprintf("return ScalarReal((double)%s);", value_expr),
    f64 = sprintf("return ScalarReal(%s);", value_expr),
    bool = sprintf("return ScalarLogical((int)%s);", value_expr),
    cstring = sprintf(
      paste(
        "if (%s) {",
        "    SEXP out = PROTECT(mkString(%s));",
        "    UNPROTECT(1);",
        "    return out;",
        "} else {",
        "    return R_NilValue;",
        "}",
        sep = "\n"
      ),
      value_expr,
      value_expr
    ),
    ptr = sprintf(
      "return R_MakeExternalPtr(%s, R_NilValue, R_NilValue);",
      value_expr
    ),
    sexp = sprintf("return %s;", value_expr),
    void = sprintf("%s;\n  return R_NilValue;", value_expr),
    {
      # Handle enum types (matched via check_ffi_type returning i32 info)
      if (grepl("^enum:", ffi_type)) {
        sprintf("return ScalarInteger((int)%s);", value_expr)
      } else {
        stop("Unsupported FFI return type: ", ffi_type, call. = FALSE)
      }
    }
  )
}

# Callback helper: generate a unique trampoline name per wrapper argument
callback_trampoline_name <- function(wrapper_name, arg_index) {
  paste0("trampoline_", wrapper_name, "_arg", arg_index)
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
  return_type,
  c_code = NULL,
  is_external = FALSE
) {
  n_args <- length(arg_types)
  return_info <- check_ffi_type(return_type, "return value")

  # Always use R API mode: SEXP arguments and return
  # Build argument list
  if (n_args > 0) {
    arg_names <- names(arg_types) %||% paste0("arg", seq_len(n_args))
    r_arg_names <- paste0("arg", seq_len(n_args), "_")

    # Input conversions from SEXP to C types
    input_lines <- character(0)
    for (i in seq_len(n_args)) {
      ffi_type <- arg_types[[i]]
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
        tramp_name <- callback_trampoline_name(wrapper_name, i)
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
  return_conversion <- generate_c_return(call_expr, return_type)

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
      sym$returns,
      paste0("symbol '", sym_name, "' return")
    )

    arg_types <- sym$args
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

    decl <- sprintf(
      "extern %s %s(%s);",
      return_info$c_type,
      sym_name,
      arg_decls
    )
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
    wrapper_name <- paste0(prefix, sym_name)

    wrapper <- generate_c_wrapper(
      symbol_name = sym_name,
      wrapper_name = wrapper_name,
      arg_types = sym$args,
      return_type = sym$returns,
      is_external = is_external
    )

    wrappers <- c(wrappers, wrapper, "", "")
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

    # Finalizer function
    helpers <- c(helpers, generate_struct_finalizer(struct_name))

    # Constructor
    helpers <- c(helpers, generate_struct_new(struct_name))

    # Free function
    helpers <- c(helpers, generate_struct_free(struct_name))

    # Field getters and setters
    for (field_name in names(fields)) {
      field_spec <- fields[[field_name]]
      helpers <- c(
        helpers,
        generate_struct_getter(struct_name, field_name, field_spec)
      )
      helpers <- c(
        helpers,
        generate_struct_setter(struct_name, field_name, field_spec)
      )
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

# Generate finalizer for struct
generate_struct_finalizer <- function(struct_name) {
  c(
    sprintf("static void %s_finalizer(SEXP ext) {", struct_name),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    "  if (p) free(p);",
    "  R_ClearExternalPtr(ext);",
    "}",
    ""
  )
}

# Generate constructor
generate_struct_new <- function(struct_name) {
  c(
    sprintf("SEXP R_wrap_%s_new(void) {", struct_name),
    sprintf(
      "  struct %s *p = calloc(1, sizeof(struct %s));",
      struct_name,
      struct_name
    ),
    "  if (!p) Rf_error(\"Out of memory\");",
    sprintf(
      "  SEXP ext = R_MakeExternalPtr(p, Rf_install(\"%s\"), R_NilValue);",
      struct_name
    ),
    sprintf("  R_RegisterCFinalizerEx(ext, %s_finalizer, TRUE);", struct_name),
    "  return ext;",
    "}",
    ""
  )
}

# Generate free function
generate_struct_free <- function(struct_name) {
  c(
    sprintf("SEXP R_wrap_%s_free(SEXP ext) {", struct_name),
    sprintf("  %s_finalizer(ext);", struct_name),
    "  return R_NilValue;",
    "}",
    ""
  )
}

# Generate field getter
generate_struct_getter <- function(struct_name, field_name, field_spec) {
  if (is.list(field_spec)) {
    type_name <- field_spec$type %||% "ptr"
  } else {
    type_name <- field_spec
  }

  return_code <- switch(type_name,
    i8 = sprintf("return ScalarInteger((int)p->%s);", field_name),
    i16 = sprintf("return ScalarInteger((int)p->%s);", field_name),
    i32 = sprintf("return ScalarInteger(p->%s);", field_name),
    i64 = sprintf("return ScalarReal((double)p->%s);", field_name),
    u8 = sprintf("return ScalarInteger((int)p->%s);", field_name),
    u16 = sprintf("return ScalarInteger((int)p->%s);", field_name),
    u32 = sprintf("return ScalarReal((double)p->%s);", field_name),
    u64 = sprintf("return ScalarReal((double)p->%s);", field_name),
    f32 = sprintf("return ScalarReal((double)p->%s);", field_name),
    f64 = sprintf("return ScalarReal(p->%s);", field_name),
    bool = sprintf("return ScalarLogical((int)p->%s);", field_name),
    cstring = c(
      sprintf("  if (p->%s) {", field_name),
      sprintf("    SEXP out = PROTECT(mkString(p->%s));", field_name),
      "    UNPROTECT(1);",
      "    return out;",
      "  } else {",
      "    return R_NilValue;",
      "  }"
    ),
    ptr = sprintf(
      "return R_MakeExternalPtr(p->%s, R_NilValue, ext);",
      field_name
    ),
    sprintf("return R_MakeExternalPtr(&p->%s, R_NilValue, ext);", field_name) # Default: return pointer to field
  )

  c(
    sprintf("SEXP R_wrap_%s_get_%s(SEXP ext) {", struct_name, field_name),
    sprintf("  struct %s *p = R_ExternalPtrAddr(ext);", struct_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    paste("  ", return_code, collapse = "\n"),
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

  setter_code <- switch(type_name,
    i8 = sprintf("p->%s = (int8_t)asInteger(val);", field_name),
    i16 = sprintf("p->%s = (int16_t)asInteger(val);", field_name),
    i32 = sprintf("p->%s = asInteger(val);", field_name),
    i64 = sprintf("p->%s = (int64_t)REAL(val)[0];", field_name),
    u8 = sprintf("p->%s = (uint8_t)asInteger(val);", field_name),
    u16 = sprintf("p->%s = (uint16_t)asInteger(val);", field_name),
    u32 = sprintf("p->%s = (uint32_t)REAL(val)[0];", field_name),
    u64 = sprintf("p->%s = (uint64_t)REAL(val)[0];", field_name),
    f32 = sprintf("p->%s = (float)asReal(val);", field_name),
    f64 = sprintf("p->%s = asReal(val);", field_name),
    bool = sprintf("p->%s = (bool)asLogical(val);", field_name),
    cstring = if (!is.null(size)) {
      c(
        sprintf("  const char *src = CHAR(STRING_ELT(val, 0));"),
        sprintf("  strncpy(p->%s, src, %d);", field_name, size - 1),
        sprintf("  p->%s[%d] = '\\0';", field_name, size - 1)
      )
    } else {
      sprintf("p->%s = CHAR(STRING_ELT(val, 0));", field_name)
    },
    ptr = sprintf("p->%s = R_ExternalPtrAddr(val);", field_name),
    sprintf("// Cannot set field of type %s", type_name)
  )

  c(
    sprintf(
      "SEXP R_wrap_%s_set_%s(SEXP ext, SEXP val) {",
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
    sprintf("SEXP R_wrap_%s_from_%s(SEXP ext) {", struct_name, member_name),
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
      "  return R_MakeExternalPtr(p, Rf_install(\"%s\"), R_NilValue);",
      struct_name
    ),
    "}",
    ""
  )
}

# Generate field address getter
generate_field_addr <- function(struct_name, field_name) {
  c(
    sprintf("SEXP R_wrap_%s_%s_addr(SEXP ext) {", struct_name, field_name),
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
    sprintf("SEXP R_wrap_%s_get_raw(SEXP ext, SEXP len) {", struct_name),
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
    sprintf("SEXP R_wrap_%s_set_raw(SEXP ext, SEXP raw) {", struct_name),
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
    sprintf("SEXP R_wrap_%s_sizeof(void) {", struct_name),
    sprintf("  return ScalarInteger(sizeof(struct %s));", struct_name),
    "}",
    "",
    sprintf("SEXP R_wrap_%s_alignof(void) {", struct_name),
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

    # Finalizer
    helpers <- c(helpers, generate_union_finalizer(union_name))

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

generate_union_finalizer <- function(union_name) {
  c(
    sprintf("static void %s_finalizer(SEXP ext) {", union_name),
    sprintf("  union %s *p = R_ExternalPtrAddr(ext);", union_name),
    "  if (p) free(p);",
    "  R_ClearExternalPtr(ext);",
    "}",
    ""
  )
}

generate_union_new <- function(union_name) {
  c(
    sprintf("SEXP R_wrap_%s_new(void) {", union_name),
    sprintf(
      "  union %s *p = calloc(1, sizeof(union %s));",
      union_name,
      union_name
    ),
    "  if (!p) Rf_error(\"Out of memory\");",
    sprintf(
      "  SEXP ext = R_MakeExternalPtr(p, Rf_install(\"%s\"), R_NilValue);",
      union_name
    ),
    sprintf("  R_RegisterCFinalizerEx(ext, %s_finalizer, TRUE);", union_name),
    "  return ext;",
    "}",
    ""
  )
}

generate_union_free <- function(union_name) {
  c(
    sprintf("SEXP R_wrap_%s_free(SEXP ext) {", union_name),
    sprintf("  %s_finalizer(ext);", union_name),
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
      sprintf("SEXP R_wrap_%s_get_%s(SEXP ext) {", union_name, mem_name),
      sprintf("  union %s *p = R_ExternalPtrAddr(ext);", union_name),
      sprintf("  if (!p) Rf_error(\"Null pointer\");"),
      sprintf("  return R_MakeExternalPtr(&p->%s, R_NilValue, ext);", mem_name),
      "}",
      ""
    ))
  }

  type_name <- if (is.list(mem_spec)) mem_spec$type else mem_spec

  return_code <- switch(type_name,
    i32 = sprintf("return ScalarInteger(p->%s);", mem_name),
    f32 = sprintf("return ScalarReal((double)p->%s);", mem_name),
    sprintf("return R_MakeExternalPtr(&p->%s, R_NilValue, ext);", mem_name)
  )

  c(
    sprintf("SEXP R_wrap_%s_get_%s(SEXP ext) {", union_name, mem_name),
    sprintf("  union %s *p = R_ExternalPtrAddr(ext);", union_name),
    sprintf("  if (!p) Rf_error(\"Null pointer\");"),
    paste("  ", return_code, collapse = "\n"),
    "}",
    ""
  )
}

generate_union_setter <- function(union_name, mem_name, mem_spec) {
  if (
    is.list(mem_spec) && !is.null(mem_spec$type) && mem_spec$type == "struct"
  ) {
    return(c()) # Cannot set whole struct easily
  }

  type_name <- if (is.list(mem_spec)) mem_spec$type else mem_spec

  setter_code <- switch(type_name,
    i32 = sprintf("p->%s = asInteger(val);", mem_name),
    f32 = sprintf("p->%s = (float)asReal(val);", mem_name),
    sprintf("// Cannot set union member of type %s", type_name)
  )

  c(
    sprintf(
      "SEXP R_wrap_%s_set_%s(SEXP ext, SEXP val) {",
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
    sprintf("SEXP R_wrap_%s_sizeof(void) {", union_name),
    sprintf("  return ScalarInteger(sizeof(union %s));", union_name),
    "}",
    "",
    sprintf("SEXP R_wrap_%s_alignof(void) {", union_name),
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
  parts <- c(parts, "#include <R.h>", "#include <Rinternals.h>", "")

  parts <- c(
    parts,
    "#include <stdint.h>",
    "#include <stdbool.h>",
    "#include <stddef.h>",
    ""
  )

  callback_trampolines <- generate_callback_trampolines(symbols)
  if (nzchar(callback_trampolines)) {
    parts <- c(
      parts,
      "#include <stdio.h>",
      "",
      "/* Callback trampoline support */",
      "typedef struct { int id; int refs; } callback_token_t;",
      "SEXP RC_invoke_callback(SEXP, SEXP);",
      "",
      callback_trampolines,
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
    return("")
  }

  trampolines <- character(0)
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
        trampolines <- c(
          trampolines,
          generate_trampoline(tramp_name, sig),
          ""
        )
      }
    }
  }

  paste(trampolines, collapse = "\n")
}
