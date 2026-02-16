# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Internal lambda.r rules for codegen dispatch
#
# This file defines many small "rules" used by the code generator. Each
# rule is registered with a tiny DSL (the lambda.r style rules such as
# `ffi_input_rule`, `ffi_return_rule`, `r_to_c_return_lines_rule`, etc.)
# and returns either a string or a character vector containing C source
# lines to be injected into generated bindings. The sections below are
# grouped by purpose (input conversion, return conversion, array helpers,
# struct/union accessors, callback helpers, type mapping keys, etc.).
#
# NOTE: many groups include nearly-identical rules for multiple integer
# and floating types; these were written out explicitly for clarity.  See
# TODOs later in this file for suggestions where a small generator could
# reduce repetition by producing similar rules programmatically.

## ------------------------------------------------------------------
## FFI input conversion rules
##
## Produce C lines that convert an incoming R `SEXP` (named by `r_name`)
## into a C variable (named by `arg_name`) of the requested FFI type.
## These rules are used when generating the C thunk that bridges R -> C.
## ------------------------------------------------------------------
ffi_input_rule("i8", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  int _%s = asInteger(%s);",
      "  if (_%s == NA_INTEGER) Rf_error(\"integer value is NA\");",
      "  if (_%s < INT8_MIN || _%s > INT8_MAX) Rf_error(\"i8 out of range\");",
      "  int8_t %s = (int8_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("i16", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  int _%s = asInteger(%s);",
      "  if (_%s == NA_INTEGER) Rf_error(\"integer value is NA\");",
      "  if (_%s < INT16_MIN || _%s > INT16_MAX) Rf_error(\"i16 out of range\");",
      "  int16_t %s = (int16_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("i32", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  int _%s = asInteger(%s);",
      "  if (_%s == NA_INTEGER) Rf_error(\"integer value is NA\");",
      "  if (_%s < INT32_MIN || _%s > INT32_MAX) Rf_error(\"i32 out of range\");",
      "  int32_t %s = (int32_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("i64", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  double _%s = asReal(%s);",
      "  if (ISNA(_%s) || ISNAN(_%s)) Rf_error(\"numeric value is NA\");",
      "  if (fabs(_%s) > 9007199254740992.0) Rf_error(\"i64 requires exact integer (|x| <= 2^53)\");",
      "  if (trunc(_%s) != _%s) Rf_error(\"i64 requires integer value\");",
      "  if (_%s < (double)INT64_MIN || _%s > (double)INT64_MAX) Rf_error(\"i64 out of range\");",
      "  int64_t %s = (int64_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("u8", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  int _%s = asInteger(%s);",
      "  if (_%s == NA_INTEGER) Rf_error(\"integer value is NA\");",
      "  if (_%s < 0 || _%s > UINT8_MAX) Rf_error(\"u8 out of range\");",
      "  uint8_t %s = (uint8_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("u16", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  int _%s = asInteger(%s);",
      "  if (_%s == NA_INTEGER) Rf_error(\"integer value is NA\");",
      "  if (_%s < 0 || _%s > UINT16_MAX) Rf_error(\"u16 out of range\");",
      "  uint16_t %s = (uint16_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("u32", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  double _%s = asReal(%s);",
      "  if (ISNA(_%s) || ISNAN(_%s)) Rf_error(\"numeric value is NA\");",
      "  if (_%s < 0 || _%s > (double)UINT32_MAX) Rf_error(\"u32 out of range\");",
      "  if (trunc(_%s) != _%s) Rf_error(\"u32 requires integer value\");",
      "  uint32_t %s = (uint32_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("u64", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  double _%s = asReal(%s);",
      "  if (ISNA(_%s) || ISNAN(_%s)) Rf_error(\"numeric value is NA\");",
      "  if (_%s < 0) Rf_error(\"u64 out of range\");",
      "  if (fabs(_%s) > 9007199254740992.0) Rf_error(\"u64 requires exact integer (|x| <= 2^53)\");",
      "  if (trunc(_%s) != _%s) Rf_error(\"u64 requires integer value\");",
      "  if (_%s > (double)UINT64_MAX) Rf_error(\"u64 out of range\");",
      "  uint64_t %s = (uint64_t)_%s;",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("f32", arg_name, r_name) %as% {
  sprintf("  float %s = (float)asReal(%s);", arg_name, r_name)
}

ffi_input_rule("f64", arg_name, r_name) %as% {
  sprintf("  double %s = asReal(%s);", arg_name, r_name)
}

ffi_input_rule("bool", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  int _%s = asLogical(%s);",
      "  if (_%s == NA_LOGICAL) Rf_error(\"logical value is NA\");",
      "  bool %s = (bool)(_%s != 0);",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("cstring", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  SEXP _%s = STRING_ELT(%s, 0);",
      "  const char* %s = (_%s == NA_STRING) ? NULL : Rf_translateCharUTF8(_%s);",
      sep = "\n"
    ),
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("ptr", arg_name, r_name) %as% {
  sprintf("  void* %s = R_ExternalPtrAddr(%s);", arg_name, r_name)
}

ffi_input_enum_rule(type_name, arg_name, r_name) %as% {
  sprintf("  int %s = asInteger(%s);", arg_name, r_name)
}

ffi_input_callback_rule(type_name, arg_name, r_name) %as% {
  sprintf("  void* %s = R_ExternalPtrAddr(%s);", arg_name, r_name)
}

ffi_input_rule("raw", arg_name, r_name) %as% {
  sprintf("  uint8_t* %s = RAW(%s);", arg_name, r_name)
}

ffi_input_rule("integer_array", arg_name, r_name) %as% {
  sprintf("  int32_t* %s = INTEGER(%s);", arg_name, r_name)
}

ffi_input_rule("numeric_array", arg_name, r_name) %as% {
  sprintf("  double* %s = REAL(%s);", arg_name, r_name)
}

ffi_input_rule("logical_array", arg_name, r_name) %as% {
  sprintf("  int* %s = LOGICAL(%s);", arg_name, r_name)
}

ffi_input_rule("character_array", arg_name, r_name) %as% {
  sprintf("  SEXP* %s = STRING_PTR(%s);", arg_name, r_name)
}

ffi_input_rule("cstring_array", arg_name, r_name) %as% {
  sprintf(
    paste(
      "  if (!Rf_isString(%s)) Rf_error(\"expected character vector\");",
      "  R_xlen_t _%s_n = XLENGTH(%s);",
      "  const char** %s = (const char**) R_alloc((size_t)_%s_n, sizeof(const char*));",
      "  for (R_xlen_t _%s_i = 0; _%s_i < _%s_n; _%s_i++) {",
      "    SEXP _%s_elt = STRING_ELT(%s, _%s_i);",
      "    %s[_%s_i] = (_%s_elt == NA_STRING) ? NULL : Rf_translateCharUTF8(_%s_elt);",
      "  }",
      sep = "\n"
    ),
    r_name,
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    r_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name,
    arg_name
  )
}

ffi_input_rule("sexp", arg_name, r_name) %as% {
  sprintf("  SEXP %s = %s;", arg_name, r_name)
}

ffi_input_rule("callback", arg_name, r_name) %as% {
  sprintf("  void* %s = R_ExternalPtrAddr(%s);", arg_name, r_name)
}

ffi_input_rule(type, arg_name, r_name) %as% {
  stop("Unsupported FFI type: ", type, call. = FALSE)
}

## ------------------------------------------------------------------
## Runtime `.RtinyccCall` dispatch rules
##
## Dispatch small arities directly and fall back to do.call for larger
## signatures.
## ------------------------------------------------------------------
rtinycc_call(0L, call_ptr, args) %as% {
  .RtinyccCall(call_ptr)
}

rtinycc_call(1L, call_ptr, args) %as% {
  .RtinyccCall(call_ptr, args[[1]])
}

rtinycc_call(2L, call_ptr, args) %as% {
  .RtinyccCall(call_ptr, args[[1]], args[[2]])
}

rtinycc_call(3L, call_ptr, args) %as% {
  .RtinyccCall(call_ptr, args[[1]], args[[2]], args[[3]])
}

rtinycc_call(4L, call_ptr, args) %as% {
  .RtinyccCall(call_ptr, args[[1]], args[[2]], args[[3]], args[[4]])
}

rtinycc_call(5L, call_ptr, args) %as% {
  .RtinyccCall(
    call_ptr,
    args[[1]],
    args[[2]],
    args[[3]],
    args[[4]],
    args[[5]]
  )
}

rtinycc_call(n_args, call_ptr, args) %as% {
  do.call(.RtinyccCall, c(list(call_ptr), args))
}

## ------------------------------------------------------------------
## Runtime platform dispatch rules
##
## Keep platform-specific path/name/env branching declarative using
## lambda.r dispatch instead of large switch/if chains.
## ------------------------------------------------------------------
tcc_platform_lib_paths("Linux") %as% {
  c(
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
    "/lib/x86_64-linux-gnu/",
    # linux-unknown-gnu is for Alpine Linux with musl
    # which uses different library paths
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib/i386-linux-gnu",
    "/lib/x86_64-linux-gnu",
    "/lib32/x86_64-linux-gnu",
    "/usr/lib/x86_64-linux-musl",
    "/usr/lib/i386-linux-musl",
    "/lib/x86_64-linux-musl",
    "/lib32/x86_64-linux-musl",
    # amd/aarch64 multiarch paths
    "/usr/lib/amd64-linux-gnu",
    "/usr/lib/aarch64-linux-gnu"
    # manylinux paths
  )
}

tcc_platform_lib_paths("Darwin") %as% {
  c(
    "/usr/lib",
    "/usr/local/lib",
    "/opt/homebrew/lib", # Apple Silicon Homebrew
    "/opt/local/lib", # MacPorts
    "/System/Library/Frameworks", # Apple system libs
    "/Library/Frameworks"
  )
}

tcc_platform_lib_paths("Windows") %as% {
  sysroot <- Sys.getenv("SystemRoot", unset = "C:/Windows")
  r_bin <- file.path(R.home(), "bin")
  r_arch_bin <- file.path(R.home(), "bin", .Platform$r_arch)
  path_dirs <- strsplit(Sys.getenv("PATH", ""), .Platform$path.sep, fixed = TRUE)[[1]]

  unique(c(
    "C:/msys64/mingw64/lib", # MSYS2
    "C:/msys64/mingw32/lib",
    "C:/Rtools45/mingw_64/lib", # Rtools
    "C:/Rtools45/mingw_32/lib",
    "C:/Rtools44/mingw_64/lib", # Rtools
    "C:/Rtools44/mingw_32/lib",
    # Native Windows loader locations / common runtime dirs
    file.path(sysroot, "System32"),
    file.path(sysroot, "SysWOW64"),
    r_bin,
    r_arch_bin,
    # User/runtime PATH entries (non-Rtools setups)
    path_dirs
  ))
}

tcc_platform_lib_paths(sysname) %as% {
  c("/usr/lib", "/usr/local/lib")
}

tcc_short_lib_filename("Linux", name) %as% {
  paste0("lib", name, ".so")
}

tcc_short_lib_filename("Darwin", name) %as% {
  paste0("lib", name, ".dylib")
}

tcc_short_lib_filename("Windows", name) %as% {
  paste0(name, ".dll")
}

tcc_short_lib_filename(sysname, name) %as% {
  paste0("lib", name, ".so")
}

tcc_output_type_rule("memory") %as% 1L
tcc_output_type_rule("obj") %as% 3L
tcc_output_type_rule("dll") %as% 4L
tcc_output_type_rule("exe") %as% 2L
tcc_output_type_rule("preprocess") %as% 5L

tcc_loader_env_key("Windows") %as% "PATH"
tcc_loader_env_key("Darwin") %as% "DYLD_LIBRARY_PATH"
tcc_loader_env_key(sysname) %as% "LD_LIBRARY_PATH"

tcc_loader_env_sep("Windows") %as% .Platform$path.sep
tcc_loader_env_sep(sysname) %as% ":"

## ------------------------------------------------------------------
## FFI return conversion rules
##
## Produce C code that converts a C expression (`value_expr`) into an
## R `SEXP` return value. Used when generating the C -> R return thunk.
## ------------------------------------------------------------------
ffi_return_rule("i8", value_expr) %as% {
  sprintf("return ScalarInteger((int)%s);", value_expr)
}

ffi_return_rule("i16", value_expr) %as% {
  sprintf("return ScalarInteger((int)%s);", value_expr)
}

ffi_return_rule("i32", value_expr) %as% {
  sprintf("return ScalarInteger(%s);", value_expr)
}

ffi_return_rule("i64", value_expr) %as% {
  sprintf(
    paste(
      "  if (fabs((double)%s) > 9007199254740992.0) Rf_warning(\"i64 precision loss in R numeric\");",
      "  return ScalarReal((double)%s);",
      sep = "\n"
    ),
    value_expr,
    value_expr
  )
}

ffi_return_rule("u8", value_expr) %as% {
  sprintf("return ScalarInteger((int)%s);", value_expr)
}

ffi_return_rule("u16", value_expr) %as% {
  sprintf("return ScalarInteger((int)%s);", value_expr)
}

ffi_return_rule("u32", value_expr) %as% {
  sprintf("return ScalarReal((double)%s);", value_expr)
}

ffi_return_rule("u64", value_expr) %as% {
  sprintf(
    paste(
      "  if ((double)%s > 9007199254740992.0) Rf_warning(\"u64 precision loss in R numeric\");",
      "  return ScalarReal((double)%s);",
      sep = "\n"
    ),
    value_expr,
    value_expr
  )
}

ffi_return_rule("f32", value_expr) %as% {
  sprintf("return ScalarReal((double)%s);", value_expr)
}

ffi_return_rule("f64", value_expr) %as% {
  sprintf("return ScalarReal(%s);", value_expr)
}

ffi_return_rule("bool", value_expr) %as% {
  sprintf("return ScalarLogical((int)%s);", value_expr)
}

ffi_return_rule("cstring", value_expr) %as% {
  sprintf(
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
  )
}

ffi_return_rule("ptr", value_expr) %as% {
  sprintf("return R_MakeExternalPtr(%s, R_NilValue, R_NilValue);", value_expr)
}

ffi_return_rule("sexp", value_expr) %as% {
  sprintf("return %s;", value_expr)
}

ffi_return_rule("void", value_expr) %as% {
  sprintf("%s;\n  return R_NilValue;", value_expr)
}

ffi_return_enum_rule(type_name, value_expr) %as% {
  sprintf("return ScalarInteger((int)%s);", value_expr)
}

ffi_return_rule(type, value_expr) %as% {
  stop("Unsupported FFI return type: ", type, call. = FALSE)
}

## ------------------------------------------------------------------
## Array return helpers
##
## Helper rules for allocating an R vector to hold array returns and for
## copying C buffers into the newly allocated R vector (`out`).
## ------------------------------------------------------------------
array_return_alloc_line_rule("raw", len_name) %as% {
  sprintf("SEXP out = PROTECT(allocVector(RAWSXP, %s));", len_name)
}

array_return_alloc_line_rule("integer_array", len_name) %as% {
  sprintf("SEXP out = PROTECT(allocVector(INTSXP, %s));", len_name)
}

array_return_alloc_line_rule("numeric_array", len_name) %as% {
  sprintf("SEXP out = PROTECT(allocVector(REALSXP, %s));", len_name)
}

array_return_alloc_line_rule("logical_array", len_name) %as% {
  sprintf("SEXP out = PROTECT(allocVector(LGLSXP, %s));", len_name)
}

array_return_alloc_line_rule(type, len_name) %as% {
  stop("Unsupported array return type: ", type, call. = FALSE)
}

array_return_copy_line_rule("raw", len_name, value_expr) %as% {
  sprintf(
    "  if (%s > 0) memcpy(RAW(out), %s, sizeof(uint8_t) * %s);",
    len_name,
    value_expr,
    len_name
  )
}

array_return_copy_line_rule("integer_array", len_name, value_expr) %as% {
  sprintf(
    "  if (%s > 0) memcpy(INTEGER(out), %s, sizeof(int32_t) * %s);",
    len_name,
    value_expr,
    len_name
  )
}

array_return_copy_line_rule("numeric_array", len_name, value_expr) %as% {
  sprintf(
    "  if (%s > 0) memcpy(REAL(out), %s, sizeof(double) * %s);",
    len_name,
    value_expr,
    len_name
  )
}

array_return_copy_line_rule("logical_array", len_name, value_expr) %as% {
  sprintf(
    "  if (%s > 0) memcpy(LOGICAL(out), %s, sizeof(int) * %s);",
    len_name,
    value_expr,
    len_name
  )
}

array_return_copy_line_rule(type, len_name, value_expr) %as% {
  stop("Unsupported array return type: ", type, call. = FALSE)
}

## ------------------------------------------------------------------
## Struct field getter rules
##
## Produce code that reads a field from a C struct (pointer `p`) and
## converts it into an R `SEXP` for consumer code. Rules are repeated
## for multiple primitive types for clarity and explicit handling.
## ------------------------------------------------------------------
struct_field_getter_rule("i8", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s);", field_name)
}

struct_field_getter_rule("i16", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s);", field_name)
}

struct_field_getter_rule("i32", field_name) %as% {
  sprintf("return ScalarInteger(p->%s);", field_name)
}

struct_field_getter_rule("i64", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s);", field_name)
}

struct_field_getter_rule("u8", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s);", field_name)
}

struct_field_getter_rule("u16", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s);", field_name)
}

struct_field_getter_rule("u32", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s);", field_name)
}

struct_field_getter_rule("u64", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s);", field_name)
}

struct_field_getter_rule("f32", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s);", field_name)
}

struct_field_getter_rule("f64", field_name) %as% {
  sprintf("return ScalarReal(p->%s);", field_name)
}

struct_field_getter_rule("bool", field_name) %as% {
  sprintf("return ScalarLogical((int)p->%s);", field_name)
}

struct_field_getter_rule("cstring", field_name) %as% {
  c(
    sprintf("  if (p->%s) {", field_name),
    sprintf("    SEXP out = PROTECT(mkString(p->%s));", field_name),
    "    UNPROTECT(1);",
    "    return out;",
    "  } else {",
    "    return R_NilValue;",
    "  }"
  )
}

struct_field_getter_rule("ptr", field_name) %as% {
  sprintf("return R_MakeExternalPtr(p->%s, R_NilValue, ext);", field_name)
}

struct_field_getter_rule(type_name, field_name) %as% {
  sprintf("return R_MakeExternalPtr(&p->%s, R_NilValue, ext);", field_name)
}

## ------------------------------------------------------------------
## Struct array field getter rules
##
## Like the struct field getters, but for array fields inside structs
## (access element `idx`). Returns an R scalar for the selected element.
## ------------------------------------------------------------------
struct_array_field_getter_rule("i8", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("i16", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("i32", field_name) %as% {
  sprintf("return ScalarInteger(p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("i64", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("u8", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("u16", field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("u32", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("u64", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("f32", field_name) %as% {
  sprintf("return ScalarReal((double)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("f64", field_name) %as% {
  sprintf("return ScalarReal(p->%s[idx]);", field_name)
}

struct_array_field_getter_rule("bool", field_name) %as% {
  sprintf("return ScalarLogical((int)p->%s[idx]);", field_name)
}

struct_array_field_getter_rule(type_name, field_name) %as% {
  sprintf("return ScalarInteger((int)p->%s[idx]);", field_name)
}

## ------------------------------------------------------------------
## Struct array field setter rules
##
## Generate code to set an element of an array field inside a struct
## from an R `SEXP` value (named `val`).
## ------------------------------------------------------------------
struct_array_field_setter_rule("i8", field_name) %as% {
  sprintf("p->%s[idx] = (int8_t)asInteger(val);", field_name)
}

struct_array_field_setter_rule("i16", field_name) %as% {
  sprintf("p->%s[idx] = (int16_t)asInteger(val);", field_name)
}

struct_array_field_setter_rule("i32", field_name) %as% {
  sprintf("p->%s[idx] = asInteger(val);", field_name)
}

struct_array_field_setter_rule("i64", field_name) %as% {
  sprintf("p->%s[idx] = (int64_t)REAL(val)[0];", field_name)
}

struct_array_field_setter_rule("u8", field_name) %as% {
  sprintf("p->%s[idx] = (uint8_t)asInteger(val);", field_name)
}

struct_array_field_setter_rule("u16", field_name) %as% {
  sprintf("p->%s[idx] = (uint16_t)asInteger(val);", field_name)
}

struct_array_field_setter_rule("u32", field_name) %as% {
  sprintf("p->%s[idx] = (uint32_t)REAL(val)[0];", field_name)
}

struct_array_field_setter_rule("u64", field_name) %as% {
  sprintf("p->%s[idx] = (uint64_t)REAL(val)[0];", field_name)
}

struct_array_field_setter_rule("f32", field_name) %as% {
  sprintf("p->%s[idx] = (float)REAL(val)[0];", field_name)
}

struct_array_field_setter_rule("f64", field_name) %as% {
  sprintf("p->%s[idx] = REAL(val)[0];", field_name)
}

struct_array_field_setter_rule("bool", field_name) %as% {
  sprintf("p->%s[idx] = (bool)asLogical(val);", field_name)
}

struct_array_field_setter_rule(type_name, field_name) %as% {
  sprintf("p->%s[idx] = (uint8_t)asInteger(val);", field_name)
}

## ------------------------------------------------------------------
## Struct field setter rules
##
## Produce code that assigns an R value (`val`) into a struct field
## (handles scalar fields and fixed-size char arrays). These are the
## counterparts to the getter rules above.
## ------------------------------------------------------------------
struct_field_setter_rule("i8", field_name, size) %as% {
  sprintf("p->%s = (int8_t)asInteger(val);", field_name)
}

struct_field_setter_rule("i16", field_name, size) %as% {
  sprintf("p->%s = (int16_t)asInteger(val);", field_name)
}

struct_field_setter_rule("i32", field_name, size) %as% {
  sprintf("p->%s = asInteger(val);", field_name)
}

struct_field_setter_rule("i64", field_name, size) %as% {
  sprintf("p->%s = (int64_t)REAL(val)[0];", field_name)
}

struct_field_setter_rule("u8", field_name, size) %as% {
  sprintf("p->%s = (uint8_t)asInteger(val);", field_name)
}

struct_field_setter_rule("u16", field_name, size) %as% {
  sprintf("p->%s = (uint16_t)asInteger(val);", field_name)
}

struct_field_setter_rule("u32", field_name, size) %as% {
  sprintf("p->%s = (uint32_t)REAL(val)[0];", field_name)
}

struct_field_setter_rule("u64", field_name, size) %as% {
  sprintf("p->%s = (uint64_t)REAL(val)[0];", field_name)
}

struct_field_setter_rule("f32", field_name, size) %as% {
  sprintf("p->%s = (float)asReal(val);", field_name)
}

struct_field_setter_rule("f64", field_name, size) %as% {
  sprintf("p->%s = asReal(val);", field_name)
}

struct_field_setter_rule("bool", field_name, size) %as% {
  sprintf("p->%s = (bool)asLogical(val);", field_name)
}

struct_field_setter_rule("ptr", field_name, size) %as% {
  sprintf("p->%s = R_ExternalPtrAddr(val);", field_name)
}

struct_field_setter_rule("cstring", field_name, size) %as% {
  if (!is.null(size)) {
    return(c(
      "  const char *src = CHAR(STRING_ELT(val, 0));",
      sprintf("  strncpy(p->%s, src, %d);", field_name, size - 1),
      sprintf("  p->%s[%d] = '\\0';", field_name, size - 1)
    ))
  }
  sprintf("p->%s = CHAR(STRING_ELT(val, 0));", field_name)
}

struct_field_setter_rule(type_name, field_name, size) %as% {
  sprintf("// Cannot set field of type %s", type_name)
}

## ------------------------------------------------------------------
## Union field helper rules
##
## Unions are represented by mapping to the corresponding struct rules
## wherever possible; setting array members in unions is unsupported and
## emits placeholder comments. These rules delegate to struct helpers.
## ------------------------------------------------------------------
union_field_getter_rule(type_name, mem_name) %as% {
  struct_field_getter_rule(type_name, mem_name)
}

union_field_setter_rule("raw", mem_name, size) %as% {
  sprintf("// Cannot set union member array type raw")
}

union_field_setter_rule("integer_array", mem_name, size) %as% {
  sprintf("// Cannot set union member array type integer_array")
}

union_field_setter_rule("numeric_array", mem_name, size) %as% {
  sprintf("// Cannot set union member array type numeric_array")
}

union_field_setter_rule("logical_array", mem_name, size) %as% {
  sprintf("// Cannot set union member array type logical_array")
}

union_field_setter_rule("character_array", mem_name, size) %as% {
  sprintf("// Cannot set union member array type character_array")
}

union_field_setter_rule("cstring_array", mem_name, size) %as% {
  sprintf("// Cannot set union member array type cstring_array")
}

union_field_setter_rule(type_name, mem_name, size) %as% {
  struct_field_setter_rule(type_name, mem_name, size)
}

## ------------------------------------------------------------------
## Callback argument kind/value helpers
##
## Map a callback argument type to an internal CB_ARG_* kind and to the
## field name that holds the value inside the generic callback value
## union struct used by the runtime. These are small lookup rules used
## when generating callback trampoline code.
## ------------------------------------------------------------------
cb_arg_kind_rule("ptr") %as% {
  "CB_ARG_PTR"
}

cb_arg_kind_rule("cstring") %as% {
  "CB_ARG_CSTRING"
}

cb_arg_kind_rule("real") %as% {
  "CB_ARG_REAL"
}

cb_arg_kind_rule("logical") %as% {
  "CB_ARG_LOGICAL"
}

cb_arg_kind_rule("int") %as% {
  "CB_ARG_INT"
}

cb_arg_value_rule("ptr", arg_expr) %as% {
  list(field = "p", expr = arg_expr)
}

cb_arg_value_rule("cstring", arg_expr) %as% {
  list(field = "s", expr = arg_expr)
}

cb_arg_value_rule("real", arg_expr) %as% {
  list(field = "d", expr = arg_expr)
}

cb_arg_value_rule("logical", arg_expr) %as% {
  list(field = "i", expr = arg_expr)
}

cb_arg_value_rule("int", arg_expr) %as% {
  list(field = "i", expr = arg_expr)
}

## ------------------------------------------------------------------
## C default return helpers
##
## When a callback or conversion fails we need a sensible C-level default
## return statement for a given C type. These helpers emit appropriately
## indented return lines (used by many `r_to_c_return_lines_rule` cases).
## ------------------------------------------------------------------
c_default_return_rule("void", indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return;")
}

c_default_return_rule("sexp", indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return R_NilValue;")
}

c_default_return_rule("ptr", indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return NULL;")
}

c_default_return_rule("cstring", indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return NULL;")
}

c_default_return_rule("real", indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return NA_REAL;")
}

c_default_return_rule("logical", indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return -1;")
}

c_default_return_rule("int", indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return NA_INTEGER;")
}

## ------------------------------------------------------------------
## C <-> SEXP type name helpers
##
## Map between C types used in generated stubs and the canonical names
## used for selecting conversion rules and SEXP constructors.
## ------------------------------------------------------------------
c_sexp_type_rule("ptr") %as% {
  "void*"
}

c_sexp_type_rule("int") %as% {
  "int"
}

c_sexp_type_rule("longlong") %as% {
  "long long"
}

c_sexp_type_rule("real") %as% {
  "double"
}

c_sexp_type_rule("cstring") %as% {
  "const char*"
}

c_sexp_type_rule("bool") %as% {
  "int"
}

## ------------------------------------------------------------------
## Canonical R-facing type keys
##
## Map C types into the compact R-facing type keys (e.g. "i32", "f64",
## "ptr") used across many rule lookups.
## ------------------------------------------------------------------
c_r_type_rule("ptr") %as% {
  "ptr"
}

c_r_type_rule("i32") %as% {
  "i32"
}

c_r_type_rule("f64") %as% {
  "f64"
}

c_r_type_rule("cstring") %as% {
  "cstring"
}

c_r_type_rule("bool") %as% {
  "bool"
}

c_r_type_rule("void") %as% {
  "void"
}

c_r_type_rule("sexp") %as% {
  "sexp"
}

## ------------------------------------------------------------------
## SEXP constructor helpers
##
## Provide the name of the constructor (or the full call form) used to
## create a SEXP from a native C value during return conversion.
## ------------------------------------------------------------------
sexp_constructor_rule("ptr") %as% {
  "R_MakeExternalPtr"
}

sexp_constructor_rule("int") %as% {
  "ScalarInteger"
}

sexp_constructor_rule("real") %as% {
  "ScalarReal"
}

sexp_constructor_rule("cstring") %as% {
  "mkString"
}

sexp_constructor_rule("bool") %as% {
  "ScalarLogical"
}

## ------------------------------------------------------------------
## SEXP constructor call forms
##
## Produce the string of C code to call the SEXP constructor for a given
## expression (used when building return expressions inline).
## ------------------------------------------------------------------
sexp_constructor_call_rule("ptr", arg_expr) %as% {
  sprintf("R_MakeExternalPtr(%s, R_NilValue, R_NilValue)", arg_expr)
}

sexp_constructor_call_rule("cstring", arg_expr) %as% {
  sprintf("(%s ? mkString(%s) : R_NilValue)", arg_expr, arg_expr)
}

sexp_constructor_call_rule("int", arg_expr) %as% {
  sprintf("ScalarInteger(%s)", arg_expr)
}

sexp_constructor_call_rule("real", arg_expr) %as% {
  sprintf("ScalarReal(%s)", arg_expr)
}

sexp_constructor_call_rule("bool", arg_expr) %as% {
  sprintf("ScalarLogical(%s)", arg_expr)
}

## ------------------------------------------------------------------
## R -> C converter helpers
##
## Provide the simple converter function name used to extract C values
## from R `SEXP` objects (e.g. `asInteger`, `asReal`, `R_ExternalPtrAddr`).
## ------------------------------------------------------------------
r_to_c_converter_rule("ptr") %as% {
  "R_ExternalPtrAddr"
}

r_to_c_converter_rule("int") %as% {
  "asInteger"
}

r_to_c_converter_rule("real") %as% {
  "asReal"
}

r_to_c_converter_rule("cstring") %as% {
  "CHAR"
}

r_to_c_converter_rule("bool") %as% {
  "asLogical"
}

## ------------------------------------------------------------------
## R -> C return conversion lines
##
## These rules emit one or more lines of C that validate and convert
## a returned R `SEXP` (from a callback) into the expected C return
## value. They handle NA/NA_REAL checks, range checks, and emit warnings
## and default returns on error.
## ------------------------------------------------------------------
r_to_c_return_lines_rule("void", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return;")
}

r_to_c_return_lines_rule("sexp", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return ", result_var, ";")
}

r_to_c_return_lines_rule("ptr", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0(pad, "return R_ExternalPtrAddr(", result_var, ");")
}

r_to_c_return_lines_rule("cstring", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  c(
    paste0(pad, "if (", result_var, " == R_NilValue) return NULL;"),
    paste0(
      pad,
      "if (!Rf_isString(",
      result_var,
      ") || XLENGTH(",
      result_var,
      ") < 1) {"
    ),
    paste0(pad, "  Rf_warning(\"callback returned non-string\");"),
    paste0(pad, "  return NULL;"),
    paste0(pad, "}"),
    paste0(pad, "SEXP _str = STRING_ELT(", result_var, ", 0);"),
    paste0(pad, "if (_str == NA_STRING) return NULL;"),
    paste0(pad, "return Rf_translateCharUTF8(_str);")
  )
}

r_to_c_return_lines_rule("bool", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "int _v = asLogical(", result_var, ");"),
    paste0(pad, "if (_v == NA_LOGICAL) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA logical\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (_v != 0);")
  )
}

r_to_c_return_lines_rule("i8", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "int _v = asInteger(", result_var, ");"),
    paste0(pad, "if (_v == NA_INTEGER) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA integer\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v < INT8_MIN || _v > INT8_MAX) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range i8\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (int8_t)_v;")
  )
}

r_to_c_return_lines_rule("i16", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "int _v = asInteger(", result_var, ");"),
    paste0(pad, "if (_v == NA_INTEGER) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA integer\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v < INT16_MIN || _v > INT16_MAX) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range i16\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (int16_t)_v;")
  )
}

r_to_c_return_lines_rule("i32", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "int _v = asInteger(", result_var, ");"),
    paste0(pad, "if (_v == NA_INTEGER) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA integer\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (int32_t)_v;")
  )
}

r_to_c_return_lines_rule("int", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "int _v = asInteger(", result_var, ");"),
    paste0(pad, "if (_v == NA_INTEGER) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA integer\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return _v;")
  )
}

r_to_c_return_lines_rule("u8", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "int _v = asInteger(", result_var, ");"),
    paste0(pad, "if (_v == NA_INTEGER) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA integer\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v < 0 || _v > UINT8_MAX) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range u8\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (uint8_t)_v;")
  )
}

r_to_c_return_lines_rule("u16", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "int _v = asInteger(", result_var, ");"),
    paste0(pad, "if (_v == NA_INTEGER) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA integer\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v < 0 || _v > UINT16_MAX) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range u16\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (uint16_t)_v;")
  )
}

r_to_c_return_lines_rule("u32", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "double _v = asReal(", result_var, ");"),
    paste0(pad, "if (ISNA(_v) || ISNAN(_v)) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v < 0 || _v > (double)UINT32_MAX) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range u32\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (trunc(_v) != _v) {"),
    paste0(pad, "  Rf_warning(\"callback returned non-integer numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (uint32_t)_v;")
  )
}

r_to_c_return_lines_rule("i64", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "double _v = asReal(", result_var, ");"),
    paste0(pad, "if (ISNA(_v) || ISNAN(_v)) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (fabs(_v) > 9007199254740992.0) {"),
    paste0(
      pad,
      "  Rf_warning(\"callback i64 precision loss in R numeric\");"
    ),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (trunc(_v) != _v) {"),
    paste0(pad, "  Rf_warning(\"callback returned non-integer numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v < (double)INT64_MIN || _v > (double)INT64_MAX) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range i64\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (int64_t)_v;")
  )
}

r_to_c_return_lines_rule("u64", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "double _v = asReal(", result_var, ");"),
    paste0(pad, "if (ISNA(_v) || ISNAN(_v)) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v < 0) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range u64\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (fabs(_v) > 9007199254740992.0) {"),
    paste0(
      pad,
      "  Rf_warning(\"callback u64 precision loss in R numeric\");"
    ),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (trunc(_v) != _v) {"),
    paste0(pad, "  Rf_warning(\"callback returned non-integer numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "if (_v > (double)UINT64_MAX) {"),
    paste0(pad, "  Rf_warning(\"callback returned out-of-range u64\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (uint64_t)_v;")
  )
}

r_to_c_return_lines_rule("double", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "double _v = asReal(", result_var, ");"),
    paste0(pad, "if (ISNA(_v) || ISNAN(_v)) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return _v;")
  )
}

r_to_c_return_lines_rule("float", c_type, result_var, indent) %as% {
  pad <- paste(rep(" ", indent), collapse = "")
  default_line <- get_c_default_return(c_type, indent = indent)
  c(
    paste0(pad, "double _v = asReal(", result_var, ");"),
    paste0(pad, "if (ISNA(_v) || ISNAN(_v)) {"),
    paste0(pad, "  Rf_warning(\"callback returned NA numeric\");"),
    default_line,
    paste0(pad, "}"),
    paste0(pad, "return (float)_v;")
  )
}

## ------------------------------------------------------------------
## Callback kind key mapping
##
## Map C types (and pointer-ness) to the compact kinds used by the
## callback trampoline generator (ptr, cstring, real, logical, int).
## ------------------------------------------------------------------
cb_kind_key_rule(type_name, TRUE) %as% {
  "ptr"
}
cb_kind_key_rule("void*", FALSE) %as% {
  "ptr"
}
cb_kind_key_rule("void *", FALSE) %as% {
  "ptr"
}
cb_kind_key_rule("ptr", FALSE) %as% {
  "ptr"
}
cb_kind_key_rule("char*", FALSE) %as% {
  "cstring"
}
cb_kind_key_rule("const char*", FALSE) %as% {
  "cstring"
}
cb_kind_key_rule("string", FALSE) %as% {
  "cstring"
}
cb_kind_key_rule("cstring", FALSE) %as% {
  "cstring"
}
cb_kind_key_rule("double", FALSE) %as% {
  "real"
}
cb_kind_key_rule("float", FALSE) %as% {
  "real"
}
cb_kind_key_rule("f64", FALSE) %as% {
  "real"
}
cb_kind_key_rule("f32", FALSE) %as% {
  "real"
}
cb_kind_key_rule("bool", FALSE) %as% {
  "logical"
}
cb_kind_key_rule("_Bool", FALSE) %as% {
  "logical"
}
cb_kind_key_rule(type_name, is_ptr) %as% {
  "int"
}

## ------------------------------------------------------------------
## C-to-R type key map for arguments
##
## These rules map C type spellings (and pointer-ness) to the canonical
## R-facing type key used across code generation (e.g. "i32", "f64",
## "ptr"). This centralizes many small spelling variants in one place.
## ------------------------------------------------------------------
c_r_key_rule(type_name, TRUE) %as% {
  "ptr"
}
c_r_key_rule("void*", FALSE) %as% {
  "ptr"
}
c_r_key_rule("void *", FALSE) %as% {
  "ptr"
}
c_r_key_rule("ptr", FALSE) %as% {
  "ptr"
}
c_r_key_rule("int", FALSE) %as% {
  "i32"
}
c_r_key_rule("int32_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("i32", FALSE) %as% {
  "i32"
}
c_r_key_rule("int16_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("i16", FALSE) %as% {
  "i32"
}
c_r_key_rule("int8_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("i8", FALSE) %as% {
  "i32"
}
c_r_key_rule("long", FALSE) %as% {
  "i32"
}
c_r_key_rule("long long", FALSE) %as% {
  "i32"
}
c_r_key_rule("int64_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("i64", FALSE) %as% {
  "i32"
}
c_r_key_rule("uint8_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("u8", FALSE) %as% {
  "i32"
}
c_r_key_rule("uint16_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("u16", FALSE) %as% {
  "i32"
}
c_r_key_rule("uint32_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("u32", FALSE) %as% {
  "i32"
}
c_r_key_rule("uint64_t", FALSE) %as% {
  "i32"
}
c_r_key_rule("u64", FALSE) %as% {
  "i32"
}
c_r_key_rule("double", FALSE) %as% {
  "f64"
}
c_r_key_rule("float", FALSE) %as% {
  "f64"
}
c_r_key_rule("f64", FALSE) %as% {
  "f64"
}
c_r_key_rule("f32", FALSE) %as% {
  "f64"
}
c_r_key_rule("char*", FALSE) %as% {
  "cstring"
}
c_r_key_rule("const char*", FALSE) %as% {
  "cstring"
}
c_r_key_rule("string", FALSE) %as% {
  "cstring"
}
c_r_key_rule("cstring", FALSE) %as% {
  "cstring"
}
c_r_key_rule("bool", FALSE) %as% {
  "bool"
}
c_r_key_rule("_Bool", FALSE) %as% {
  "bool"
}
c_r_key_rule("void", FALSE) %as% {
  "void"
}
c_r_key_rule("SEXP", FALSE) %as% {
  "sexp"
}
c_r_key_rule("sexp", FALSE) %as% {
  "sexp"
}
c_r_key_rule(type_name, is_ptr) %as% {
  "ptr"
}

## ------------------------------------------------------------------
## SEXP constructor key mapping
##
## Map C type names to the key used to select a SEXP constructor (e.g.
## mkString, ScalarReal). Handles pointer cases specially.
## ------------------------------------------------------------------
sexp_ctor_key_rule(type_name, TRUE) %as% {
  "ptr"
}
sexp_ctor_key_rule("void*", FALSE) %as% {
  "ptr"
}
sexp_ctor_key_rule("void *", FALSE) %as% {
  "ptr"
}
sexp_ctor_key_rule("ptr", FALSE) %as% {
  "ptr"
}
sexp_ctor_key_rule("int", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("int32_t", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("i32", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("int16_t", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("i16", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("int8_t", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("i8", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("uint8_t", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("u8", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("uint16_t", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("u16", FALSE) %as% {
  "int"
}
sexp_ctor_key_rule("long", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("long long", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("int64_t", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("i64", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("uint32_t", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("u32", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("uint64_t", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("u64", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("double", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("float", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("f64", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("f32", FALSE) %as% {
  "real"
}
sexp_ctor_key_rule("char*", FALSE) %as% {
  "cstring"
}
sexp_ctor_key_rule("const char*", FALSE) %as% {
  "cstring"
}
sexp_ctor_key_rule("string", FALSE) %as% {
  "cstring"
}
sexp_ctor_key_rule("cstring", FALSE) %as% {
  "cstring"
}
sexp_ctor_key_rule("bool", FALSE) %as% {
  "bool"
}
sexp_ctor_key_rule("_Bool", FALSE) %as% {
  "bool"
}
sexp_ctor_key_rule(type_name, is_ptr) %as% {
  "ptr"
}

## ------------------------------------------------------------------
## R -> C key map
##
## Map C type spellings to the key that selects the appropriate R -> C
## converter (e.g. asInteger, asReal, R_ExternalPtrAddr). Pointer types
## map to "ptr".
## ------------------------------------------------------------------
r_to_c_key_rule(type_name, TRUE) %as% {
  "ptr"
}
r_to_c_key_rule("void*", FALSE) %as% {
  "ptr"
}
r_to_c_key_rule("void *", FALSE) %as% {
  "ptr"
}
r_to_c_key_rule("ptr", FALSE) %as% {
  "ptr"
}
r_to_c_key_rule("int", FALSE) %as% {
  "int"
}
r_to_c_key_rule("int32_t", FALSE) %as% {
  "int"
}
r_to_c_key_rule("i32", FALSE) %as% {
  "int"
}
r_to_c_key_rule("int16_t", FALSE) %as% {
  "int"
}
r_to_c_key_rule("i16", FALSE) %as% {
  "int"
}
r_to_c_key_rule("int8_t", FALSE) %as% {
  "int"
}
r_to_c_key_rule("i8", FALSE) %as% {
  "int"
}
r_to_c_key_rule("uint8_t", FALSE) %as% {
  "int"
}
r_to_c_key_rule("u8", FALSE) %as% {
  "int"
}
r_to_c_key_rule("uint16_t", FALSE) %as% {
  "int"
}
r_to_c_key_rule("u16", FALSE) %as% {
  "int"
}
r_to_c_key_rule("long", FALSE) %as% {
  "real"
}
r_to_c_key_rule("long long", FALSE) %as% {
  "real"
}
r_to_c_key_rule("int64_t", FALSE) %as% {
  "real"
}
r_to_c_key_rule("i64", FALSE) %as% {
  "real"
}
r_to_c_key_rule("uint32_t", FALSE) %as% {
  "real"
}
r_to_c_key_rule("u32", FALSE) %as% {
  "real"
}
r_to_c_key_rule("uint64_t", FALSE) %as% {
  "real"
}
r_to_c_key_rule("u64", FALSE) %as% {
  "real"
}
r_to_c_key_rule("double", FALSE) %as% {
  "real"
}
r_to_c_key_rule("float", FALSE) %as% {
  "real"
}
r_to_c_key_rule("f64", FALSE) %as% {
  "real"
}
r_to_c_key_rule("f32", FALSE) %as% {
  "real"
}
r_to_c_key_rule("char*", FALSE) %as% {
  "cstring"
}
r_to_c_key_rule("const char*", FALSE) %as% {
  "cstring"
}
r_to_c_key_rule("string", FALSE) %as% {
  "cstring"
}
r_to_c_key_rule("cstring", FALSE) %as% {
  "cstring"
}
r_to_c_key_rule("bool", FALSE) %as% {
  "bool"
}
r_to_c_key_rule("_Bool", FALSE) %as% {
  "bool"
}
r_to_c_key_rule(type_name, is_ptr) %as% {
  "ptr"
}

## ------------------------------------------------------------------
## Default type key mapping
##
## A general-purpose fallback mapping used when no more specific key
## mapping is appropriate. This helps many rule lookups remain simple.
## ------------------------------------------------------------------
default_key_rule("void", FALSE) %as% {
  "void"
}
default_key_rule("SEXP", FALSE) %as% {
  "sexp"
}
default_key_rule("sexp", FALSE) %as% {
  "sexp"
}
default_key_rule(type_name, TRUE) %as% {
  "ptr"
}
default_key_rule("void*", FALSE) %as% {
  "ptr"
}
default_key_rule("void *", FALSE) %as% {
  "ptr"
}
default_key_rule("ptr", FALSE) %as% {
  "ptr"
}
default_key_rule("char*", FALSE) %as% {
  "cstring"
}
default_key_rule("const char*", FALSE) %as% {
  "cstring"
}
default_key_rule("string", FALSE) %as% {
  "cstring"
}
default_key_rule("cstring", FALSE) %as% {
  "cstring"
}
default_key_rule("double", FALSE) %as% {
  "real"
}
default_key_rule("float", FALSE) %as% {
  "real"
}
default_key_rule("f64", FALSE) %as% {
  "real"
}
default_key_rule("f32", FALSE) %as% {
  "real"
}
default_key_rule("bool", FALSE) %as% {
  "logical"
}
default_key_rule("_Bool", FALSE) %as% {
  "logical"
}
default_key_rule(type_name, is_ptr) %as% {
  "int"
}

## ------------------------------------------------------------------
## SEXP type key mapping
##
## Determine which SEXP type (int/real/longlong/ptr/cstring/etc.) should
## be used for a C type when generating wrappers that will box/unbox
## values to/from R.
## ------------------------------------------------------------------
sexp_type_key_rule(type_name, TRUE) %as% {
  "ptr"
}
sexp_type_key_rule("int", FALSE) %as% {
  "int"
}
sexp_type_key_rule("int32_t", FALSE) %as% {
  "int"
}
sexp_type_key_rule("i32", FALSE) %as% {
  "int"
}
sexp_type_key_rule("int16_t", FALSE) %as% {
  "int"
}
sexp_type_key_rule("i16", FALSE) %as% {
  "int"
}
sexp_type_key_rule("int8_t", FALSE) %as% {
  "int"
}
sexp_type_key_rule("i8", FALSE) %as% {
  "int"
}
sexp_type_key_rule("long", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("long long", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("int64_t", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("i64", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("uint8_t", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("u8", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("uint16_t", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("u16", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("uint32_t", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("u32", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("uint64_t", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("u64", FALSE) %as% {
  "longlong"
}
sexp_type_key_rule("double", FALSE) %as% {
  "real"
}
sexp_type_key_rule("float", FALSE) %as% {
  "real"
}
sexp_type_key_rule("f64", FALSE) %as% {
  "real"
}
sexp_type_key_rule("f32", FALSE) %as% {
  "real"
}
sexp_type_key_rule("char*", FALSE) %as% {
  "cstring"
}
sexp_type_key_rule("const char*", FALSE) %as% {
  "cstring"
}
sexp_type_key_rule("string", FALSE) %as% {
  "cstring"
}
sexp_type_key_rule("cstring", FALSE) %as% {
  "cstring"
}
sexp_type_key_rule("void*", FALSE) %as% {
  "ptr"
}
sexp_type_key_rule("void *", FALSE) %as% {
  "ptr"
}
sexp_type_key_rule("ptr", FALSE) %as% {
  "ptr"
}
sexp_type_key_rule("bool", FALSE) %as% {
  "bool"
}
sexp_type_key_rule("_Bool", FALSE) %as% {
  "bool"
}
sexp_type_key_rule(type_name, is_ptr) %as% {
  "ptr"
}

## ------------------------------------------------------------------
## Return type key mapping
##
## Map C return types to the compact keys used by the ffi_return_rule
## lookup (e.g. "i8", "i32", "double", "cstring", "ptr").
## ------------------------------------------------------------------
return_key_rule("void", FALSE) %as% {
  "void"
}
return_key_rule("SEXP", FALSE) %as% {
  "sexp"
}
return_key_rule("sexp", FALSE) %as% {
  "sexp"
}
return_key_rule(type_name, TRUE) %as% {
  "ptr"
}
return_key_rule("void*", FALSE) %as% {
  "ptr"
}
return_key_rule("void *", FALSE) %as% {
  "ptr"
}
return_key_rule("ptr", FALSE) %as% {
  "ptr"
}
return_key_rule("char*", FALSE) %as% {
  "cstring"
}
return_key_rule("const char*", FALSE) %as% {
  "cstring"
}
return_key_rule("string", FALSE) %as% {
  "cstring"
}
return_key_rule("cstring", FALSE) %as% {
  "cstring"
}
return_key_rule("bool", FALSE) %as% {
  "bool"
}
return_key_rule("_Bool", FALSE) %as% {
  "bool"
}
return_key_rule("int8_t", FALSE) %as% {
  "i8"
}
return_key_rule("i8", FALSE) %as% {
  "i8"
}
return_key_rule("int16_t", FALSE) %as% {
  "i16"
}
return_key_rule("i16", FALSE) %as% {
  "i16"
}
return_key_rule("int32_t", FALSE) %as% {
  "i32"
}
return_key_rule("i32", FALSE) %as% {
  "i32"
}
return_key_rule("int", FALSE) %as% {
  "int"
}
return_key_rule("long", FALSE) %as% {
  "int"
}
return_key_rule("short", FALSE) %as% {
  "int"
}
return_key_rule("uint8_t", FALSE) %as% {
  "u8"
}
return_key_rule("u8", FALSE) %as% {
  "u8"
}
return_key_rule("uint16_t", FALSE) %as% {
  "u16"
}
return_key_rule("u16", FALSE) %as% {
  "u16"
}
return_key_rule("uint32_t", FALSE) %as% {
  "u32"
}
return_key_rule("u32", FALSE) %as% {
  "u32"
}
return_key_rule("int64_t", FALSE) %as% {
  "i64"
}
return_key_rule("i64", FALSE) %as% {
  "i64"
}
return_key_rule("long long", FALSE) %as% {
  "i64"
}
return_key_rule("uint64_t", FALSE) %as% {
  "u64"
}
return_key_rule("u64", FALSE) %as% {
  "u64"
}
return_key_rule("double", FALSE) %as% {
  "double"
}
return_key_rule("f64", FALSE) %as% {
  "double"
}
return_key_rule("float", FALSE) %as% {
  "float"
}
return_key_rule("f32", FALSE) %as% {
  "float"
}
return_key_rule(type_name, is_ptr) %as% {
  "ptr"
}

## ------------------------------------------------------------------
## FFI C type canonicalization
##
## Map many C type spellings into the canonical internal FFI type keys
## used throughout this file (i8/i16/i32/i64/u8/u16/u32/u64/f32/f64/ptr
## etc.). This central lookup keeps downstream rule selection uniform.
## ------------------------------------------------------------------
ffi_c_type_map_rule(type_name, TRUE, is_ptr) %as% {
  "ptr"
}
ffi_c_type_map_rule(type_name, FALSE, TRUE) %as% {
  "ptr"
}
ffi_c_type_map_rule("void", FALSE, FALSE) %as% {
  "void"
}
ffi_c_type_map_rule("bool", FALSE, FALSE) %as% {
  "bool"
}
ffi_c_type_map_rule("_Bool", FALSE, FALSE) %as% {
  "bool"
}
ffi_c_type_map_rule("SEXP", FALSE, FALSE) %as% {
  "sexp"
}
ffi_c_type_map_rule("sexp", FALSE, FALSE) %as% {
  "sexp"
}
ffi_c_type_map_rule("int", FALSE, FALSE) %as% {
  "i32"
}
ffi_c_type_map_rule("signed", FALSE, FALSE) %as% {
  "i32"
}
ffi_c_type_map_rule("int32_t", FALSE, FALSE) %as% {
  "i32"
}
ffi_c_type_map_rule("__int32", FALSE, FALSE) %as% {
  "i32"
}
ffi_c_type_map_rule("signed int", FALSE, FALSE) %as% {
  "i32"
}
ffi_c_type_map_rule("short", FALSE, FALSE) %as% {
  "i16"
}
ffi_c_type_map_rule("short int", FALSE, FALSE) %as% {
  "i16"
}
ffi_c_type_map_rule("signed short", FALSE, FALSE) %as% {
  "i16"
}
ffi_c_type_map_rule("signed short int", FALSE, FALSE) %as% {
  "i16"
}
ffi_c_type_map_rule("int16_t", FALSE, FALSE) %as% {
  "i16"
}
ffi_c_type_map_rule("char", FALSE, FALSE) %as% {
  "i8"
}
ffi_c_type_map_rule("signed char", FALSE, FALSE) %as% {
  "i8"
}
ffi_c_type_map_rule("int8_t", FALSE, FALSE) %as% {
  "i8"
}
ffi_c_type_map_rule("long", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("long int", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("long long", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("long long int", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("signed long", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("signed long int", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("signed long long", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("signed long long int", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("int64_t", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("__int64", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("intptr_t", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("ptrdiff_t", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("ssize_t", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("off_t", FALSE, FALSE) %as% {
  "i64"
}
ffi_c_type_map_rule("unsigned int", FALSE, FALSE) %as% {
  "u32"
}
ffi_c_type_map_rule("unsigned", FALSE, FALSE) %as% {
  "u32"
}
ffi_c_type_map_rule("uint32_t", FALSE, FALSE) %as% {
  "u32"
}
ffi_c_type_map_rule("unsigned __int32", FALSE, FALSE) %as% {
  "u32"
}
ffi_c_type_map_rule("unsigned short", FALSE, FALSE) %as% {
  "u16"
}
ffi_c_type_map_rule("unsigned short int", FALSE, FALSE) %as% {
  "u16"
}
ffi_c_type_map_rule("uint16_t", FALSE, FALSE) %as% {
  "u16"
}
ffi_c_type_map_rule("unsigned char", FALSE, FALSE) %as% {
  "u8"
}
ffi_c_type_map_rule("uint8_t", FALSE, FALSE) %as% {
  "u8"
}
ffi_c_type_map_rule("unsigned long", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("unsigned long int", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("unsigned long long", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("unsigned long long int", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("uint64_t", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("unsigned __int64", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("size_t", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("uintptr_t", FALSE, FALSE) %as% {
  "u64"
}
ffi_c_type_map_rule("double", FALSE, FALSE) %as% {
  "f64"
}
ffi_c_type_map_rule("long double", FALSE, FALSE) %as% {
  "f64"
}
ffi_c_type_map_rule("float", FALSE, FALSE) %as% {
  "f32"
}
ffi_c_type_map_rule(type_name, is_char_ptr, is_ptr) %as% {
  "ptr"
}
