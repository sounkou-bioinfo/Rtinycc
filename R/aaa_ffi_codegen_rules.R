# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Internal lambda.r rules for codegen dispatch

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

ffi_input_rule(type, arg_name, r_name) %when%
    {
        startsWith(type, "enum:")
    } %as% {
        sprintf("  int %s = asInteger(%s);", arg_name, r_name)
    }

ffi_input_rule(type, arg_name, r_name) %when%
    {
        startsWith(type, "callback")
    } %as% {
        sprintf("  void* %s = R_ExternalPtrAddr(%s);", arg_name, r_name)
    }

ffi_input_rule(type, arg_name, r_name) %as% {
    stop("Unsupported FFI type: ", type, call. = FALSE)
}

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

ffi_return_rule(type, value_expr) %when%
    {
        startsWith(type, "enum:")
    } %as% {
        sprintf("return ScalarInteger((int)%s);", value_expr)
    }

ffi_return_rule(type, value_expr) %as% {
    stop("Unsupported FFI return type: ", type, call. = FALSE)
}
