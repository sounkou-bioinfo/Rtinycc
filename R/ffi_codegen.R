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
    stop("Unsupported FFI type: ", ffi_type, call. = FALSE)
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
    stop("Unsupported FFI return type: ", ffi_type, call. = FALSE)
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
    input_conversions <- paste(
      mapply(
        generate_c_input,
        arg_names,
        r_arg_names,
        arg_types,
        USE.NAMES = FALSE
      ),
      collapse = "\n"
    )

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

# Generate complete C code for FFI compilation (always R API mode)
generate_ffi_code <- function(
  symbols,
  headers = NULL,
  c_code = NULL,
  is_external = FALSE
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

  parts <- c(parts, "#include <stdint.h>", "#include <stdbool.h>", "")

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

  # Wrapper functions
  parts <- c(
    parts,
    "/* R callable wrappers */",
    generate_wrappers(symbols, is_external = is_external)
  )

  paste(parts, collapse = "\n")
}
