# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

#' Treesitter helpers for header-driven bindings
#'
#' Convenience wrappers around `treesitter.c` that map C types to the
#' Rtinycc FFI type system and return `tcc_bind()`-ready specs.
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to treesitter.c helpers.
#' @return A data frame of nodes or a named list of binding specs.
#' @name tcc_treesitter_helpers
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' header <- "double sqrt(double x);\nint add(int a, int b);"
#' tcc_treesitter_functions(header)
#' tcc_treesitter_bindings(header)
#' }
#'
#' @section treesitter.c:
#' These helpers require the optional treesitter.c package.
#'
NULL

.tcc_treesitter_root <- function(header) {
  if (!requireNamespace("treesitter.c", quietly = TRUE)) {
    stop("treesitter.c is required for header parsing", call. = FALSE)
  }
  treesitter.c::parse_header_text(header)
}

#' Parse function declarations with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_function_nodes()`.
#' @return A data frame of function nodes.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "double sqrt(double x);"
#' tcc_treesitter_functions(header)
#' }
tcc_treesitter_functions <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_function_nodes(
    root,
    extract_params = TRUE,
    extract_return = TRUE,
    ...
  )
}

#' Parse struct declarations with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_struct_nodes()`.
#' @return A data frame of struct nodes.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "struct point { double x; double y; };"
#' tcc_treesitter_structs(header)
#' }
tcc_treesitter_structs <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_struct_nodes(root, ...)
}

#' Parse union declarations with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_union_nodes()`.
#' @return A data frame of union nodes.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "union data { int i; double d; };"
#' tcc_treesitter_unions(header)
#' }
tcc_treesitter_unions <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_union_nodes(root, ...)
}

#' Parse union members with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_union_members_from_root()`.
#' @return A data frame of union members.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "union data { int i; double d; };"
#' tcc_treesitter_union_members(header)
#' }
tcc_treesitter_union_members <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_union_members_from_root(root, ...)
}

#' Parse enum declarations with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_enum_nodes()`.
#' @return A data frame of enum nodes.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "enum status { OK = 0, ERR = 1 };"
#' tcc_treesitter_enums(header)
#' }
tcc_treesitter_enums <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_enum_nodes(root, ...)
}

#' Parse enum members with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_enum_members_from_root()`.
#' @return A data frame of enum members.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "enum status { OK = 0, ERR = 1 };"
#' tcc_treesitter_enum_members(header)
#' }
tcc_treesitter_enum_members <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_enum_members_from_root(root, ...)
}

#' Parse struct members (including bitfields) with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_struct_members()`.
#' @return A data frame of struct members.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "struct point { double x; double y; };"
#' tcc_treesitter_struct_members(header)
#' }
tcc_treesitter_struct_members <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_struct_members(root, ...)
}

#' Parse global declarations with treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_globals_from_root()`.
#' @return A data frame of global names.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "int global_counter;"
#' tcc_treesitter_globals(header)
#' }
tcc_treesitter_globals <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_globals_from_root(root, ...)
}

#' Parse global declarations with types using treesitter.c
#'
#' @param header Character scalar containing C declarations.
#' @param ... Additional arguments passed to `treesitter.c::get_globals_with_types_from_root()`.
#' @return A data frame of global names and C types.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "int global_counter;"
#' tcc_treesitter_global_types(header)
#' }
tcc_treesitter_global_types <- function(header, ...) {
  root <- .tcc_treesitter_root(header)
  treesitter.c::get_globals_with_types_from_root(root, ...)
}

#' Extract macro defines from a header file
#'
#' @param file Path to a header file.
#' @param use_cpp Logical; use the C preprocessor if available.
#' @param cc Compiler string; passed to `system2()` if `use_cpp = TRUE`.
#' @param ccflags Additional flags for the compiler.
#' @return Character vector of macro names defined in `file`.
#' @export
#'
#' @examples
#' \dontrun{
#' tcc_treesitter_defines("/usr/include/math.h")
#' }
tcc_treesitter_defines <- function(
  file,
  use_cpp = TRUE,
  cc = treesitter.c::r_cc(),
  ccflags = NULL
) {
  if (!requireNamespace("treesitter.c", quietly = TRUE)) {
    stop("treesitter.c is required for header parsing", call. = FALSE)
  }
  treesitter.c::get_defines_from_file(
    file,
    use_cpp = use_cpp,
    cc = cc,
    ccflags = ccflags
  )
}

#' Map a C type string to an Rtinycc FFI type
#'
#' @param c_type C type string (e.g., "int", "double", "char *").
#' @return A single FFI type string.
#' @export
#'
#' @examples
#' \dontrun{
#' tcc_map_c_type_to_ffi("int")
#' tcc_map_c_type_to_ffi("const char *")
#' }
tcc_map_c_type_to_ffi <- function(c_type) {
  x <- trimws(as.character(c_type))
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\b(const|volatile|restrict)\\b", "", x)
  x <- gsub("\\s+", " ", trimws(x))

  # Strings and pointers
  if (grepl("char\\s*\\*", x)) {
    return("cstring")
  }
  if (grepl("\\*", x)) {
    return("ptr")
  }

  x <- sub("\\s+[A-Za-z_][A-Za-z0-9_]*$", "", x)
  x <- trimws(x)

  if (x %in% c("void")) {
    return("void")
  }
  if (x %in% c("bool", "_Bool")) {
    return("bool")
  }
  if (x %in% c("SEXP", "sexp")) {
    return("sexp")
  }

  if (x %in% c("int", "int32_t")) {
    return("i32")
  }
  if (x %in% c("short", "short int", "int16_t")) {
    return("i16")
  }
  if (x %in% c("char", "int8_t")) {
    return("i8")
  }
  if (x %in% c("long", "long int", "long long", "int64_t")) {
    return("i64")
  }

  if (x %in% c("unsigned int", "uint32_t")) {
    return("u32")
  }
  if (x %in% c("unsigned short", "unsigned short int", "uint16_t")) {
    return("u16")
  }
  if (x %in% c("unsigned char", "uint8_t")) {
    return("u8")
  }
  if (
    x %in%
      c("unsigned long", "unsigned long int", "unsigned long long", "uint64_t")
  ) {
    return("u64")
  }
  if (x %in% c("size_t")) {
    return("u64")
  }

  if (x %in% c("double")) {
    return("f64")
  }
  if (x %in% c("float")) {
    return("f32")
  }

  "ptr"
}

#' Generate bindings from a header
#'
#' @param header Character scalar containing C declarations.
#' @param mapper Function to map C types to FFI types.
#' @param ffi Optional `tcc_ffi` object. When provided, returns an updated
#'   FFI object with generated bindings.
#' @param functions Logical; generate `tcc_bind()` specs for functions.
#' @param structs Logical; generate `tcc_struct()` helpers.
#' @param unions Logical; generate `tcc_union()` helpers.
#' @param enums Logical; generate `tcc_enum()` helpers.
#' @param globals Logical; generate `tcc_global()` getters/setters.
#' @param bitfield_type FFI type to use for bitfields.
#' @param include_bitfields Whether to include bitfields.
#' @return Named list suitable for `tcc_bind()` when `ffi` is NULL, otherwise
#'   an updated `tcc_ffi` object.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "double sqrt(double x);"
#' symbols <- tcc_treesitter_bindings(header)
#' }
tcc_treesitter_bindings <- function(
  header,
  mapper = tcc_map_c_type_to_ffi,
  ffi = NULL,
  functions = TRUE,
  structs = FALSE,
  unions = FALSE,
  enums = FALSE,
  globals = FALSE,
  bitfield_type = "u8",
  include_bitfields = TRUE
) {
  if (!is.null(ffi)) {
    return(tcc_generate_bindings(
      ffi,
      header,
      mapper = mapper,
      functions = functions,
      structs = structs,
      unions = unions,
      enums = enums,
      globals = globals,
      bitfield_type = bitfield_type,
      include_bitfields = include_bitfields
    ))
  }

  if (structs || unions || enums || globals) {
    stop(
      "Provide `ffi` to generate struct/union/enum/global bindings",
      call. = FALSE
    )
  }

  if (!functions) {
    return(list())
  }

  funcs <- tcc_treesitter_functions(header)
  if (nrow(funcs) == 0) {
    return(list())
  }

  normalize_params <- function(params) {
    if (is.list(params)) {
      params <- params[[1]]
    }
    params <- as.character(params)
    if (length(params) == 1 && grepl(",", params)) {
      params <- unlist(strsplit(params, ","))
    }
    params <- trimws(params)
    if (
      length(params) == 1 && (is.na(params) || params == "" || params == "void")
    ) {
      return(character(0))
    }
    params
  }

  signature_to_bind <- function(row) {
    params <- normalize_params(row$params)
    args <- if (length(params) > 0) {
      vapply(params, mapper, character(1), USE.NAMES = FALSE)
    } else {
      character(0)
    }

    ret <- row$return_type
    if (is.list(ret)) {
      ret <- ret[[1]]
    }
    ret <- as.character(ret)
    if (length(ret) > 1) {
      ret <- ret[[1]]
    }

    list(args = as.list(args), returns = mapper(ret))
  }

  stats::setNames(
    lapply(seq_len(nrow(funcs)), function(i) signature_to_bind(funcs[i, ])),
    funcs$text
  )
}

#' Generate tcc_struct() accessors from header structs
#'
#' @param header Character scalar containing C declarations.
#' @param mapper Function to map C types to FFI types.
#' @param bitfield_type FFI type to use for bitfields.
#' @param include_bitfields Whether to include bitfields.
#' @return Named list of accessors by struct name.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "struct point { double x; double y; };"
#' tcc_treesitter_struct_accessors(header)
#' }
tcc_treesitter_struct_accessors <- function(
  header,
  mapper = tcc_map_c_type_to_ffi,
  bitfield_type = "u8",
  include_bitfields = TRUE
) {
  members <- tcc_treesitter_struct_members(header)
  if (nrow(members) == 0) {
    return(list())
  }

  split_members <- split(members, members$struct_name)
  out <- lapply(split_members, function(df) {
    acc <- list()
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      name <- row$member_name
      if (is.na(name) || !nzchar(name)) {
        next
      }
      is_bitfield <- !is.na(row$bitfield) && nzchar(row$bitfield)
      if (is_bitfield && !include_bitfields) {
        next
      }

      mtype <- row$member_type
      if (is_bitfield) {
        ffi_type <- bitfield_type
      } else if (is.na(mtype) || !nzchar(mtype)) {
        stop(
          "Missing struct member type for '",
          name,
          "' in struct '",
          row$struct_name,
          "'",
          call. = FALSE
        )
      } else if (mtype %in% c("struct", "struct (anonymous)")) {
        ffi_type <- "ptr"
      } else {
        ffi_type <- mapper(mtype)
      }

      acc[[name]] <- ffi_type
    }
    acc
  })

  out[!vapply(out, function(x) length(x) == 0, logical(1))]
}

#' Generate tcc_union() accessors from header unions
#'
#' @param header Character scalar containing C declarations.
#' @param mapper Function to map C types to FFI types.
#' @param bitfield_type FFI type to use for bitfields.
#' @param include_bitfields Whether to include bitfields.
#' @return Named list of accessors by union name.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "union data { int i; double d; };"
#' tcc_treesitter_union_accessors(header)
#' }
tcc_treesitter_union_accessors <- function(
  header,
  mapper = tcc_map_c_type_to_ffi,
  bitfield_type = "u8",
  include_bitfields = TRUE
) {
  members <- tcc_treesitter_union_members(header)
  if (nrow(members) == 0) {
    return(list())
  }

  split_members <- split(members, members$union_name)
  out <- lapply(split_members, function(df) {
    acc <- list()
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      name <- row$member_name
      if (is.na(name) || !nzchar(name)) {
        next
      }
      is_bitfield <- !is.na(row$bitfield) && nzchar(row$bitfield)
      if (is_bitfield && !include_bitfields) {
        next
      }

      mtype <- row$member_type
      if (is_bitfield) {
        ffi_type <- bitfield_type
      } else if (is.na(mtype) || !nzchar(mtype)) {
        stop(
          "Missing union member type for '",
          name,
          "' in union '",
          row$union_name,
          "'",
          call. = FALSE
        )
      } else if (mtype %in% c("struct", "struct (anonymous)")) {
        ffi_type <- "ptr"
      } else {
        ffi_type <- mapper(mtype)
      }

      acc[[name]] <- ffi_type
    }
    acc
  })

  out[!vapply(out, function(x) length(x) == 0, logical(1))]
}

#' Apply tcc_struct() bindings from a header
#'
#' @param ffi A tcc_ffi object.
#' @param header Character scalar containing C declarations.
#' @param ... Passed to `tcc_treesitter_struct_accessors()`.
#' @return Updated tcc_ffi object.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "struct point { double x; double y; };"
#' ffi <- tcc_ffi()
#' ffi <- tcc_treesitter_struct_bindings(ffi, header)
#' }
tcc_treesitter_struct_bindings <- function(ffi, header, ...) {
  accessors <- tcc_treesitter_struct_accessors(header, ...)
  if (length(accessors) == 0) {
    return(ffi)
  }
  for (nm in names(accessors)) {
    ffi <- tcc_struct(ffi, nm, accessors[[nm]])
  }
  ffi
}

#' Apply tcc_union() bindings from a header
#'
#' @param ffi A tcc_ffi object.
#' @param header Character scalar containing C declarations.
#' @param ... Passed to `tcc_treesitter_union_accessors()`.
#' @return Updated tcc_ffi object.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "union data { int i; double d; };"
#' ffi <- tcc_ffi()
#' ffi <- tcc_treesitter_union_bindings(ffi, header)
#' }
tcc_treesitter_union_bindings <- function(ffi, header, ...) {
  accessors <- tcc_treesitter_union_accessors(header, ...)
  if (length(accessors) == 0) {
    return(ffi)
  }
  for (nm in names(accessors)) {
    ffi <- tcc_union(ffi, nm, accessors[[nm]])
  }
  ffi
}

#' Apply tcc_enum() bindings from a header
#'
#' @param ffi A tcc_ffi object.
#' @param header Character scalar containing C declarations.
#' @param constants Named list of enum constants.
#' @return Updated tcc_ffi object.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "enum status { OK = 0, ERR = 1 };"
#' ffi <- tcc_ffi()
#' ffi <- tcc_treesitter_enum_bindings(ffi, header, constants = list(status = c("OK", "ERR")))
#' }
tcc_treesitter_enum_bindings <- function(ffi, header, constants = NULL) {
  enums <- tcc_treesitter_enums(header)
  if (nrow(enums) == 0 || is.null(constants)) {
    return(ffi)
  }
  for (i in seq_len(nrow(enums))) {
    nm <- enums$text[i]
    if (!is.null(constants[[nm]])) {
      ffi <- tcc_enum(ffi, nm, constants = constants[[nm]])
    }
  }
  ffi
}

#' Generate bindings from header declarations
#'
#' @param ffi A tcc_ffi object. If NULL, a new one is created.
#' @param header Character scalar containing C declarations.
#' @param mapper Function to map C types to FFI types.
#' @param functions Logical; generate `tcc_bind()` specs for functions.
#' @param structs Logical; generate `tcc_struct()` helpers.
#' @param unions Logical; generate `tcc_union()` helpers.
#' @param enums Logical; generate `tcc_enum()` helpers.
#' @param globals Logical; generate `tcc_global()` getters/setters.
#' @param bitfield_type FFI type to use for bitfields.
#' @param include_bitfields Whether to include bitfields.
#' @return Updated tcc_ffi object.
#' @export
#'
#' @examples
#' \dontrun{
#' header <- "double sqrt(double x); struct point { double x; double y; };"
#' ffi <- tcc_generate_bindings(tcc_ffi(), header)
#' }
tcc_generate_bindings <- function(
  ffi = NULL,
  header,
  mapper = tcc_map_c_type_to_ffi,
  functions = TRUE,
  structs = TRUE,
  unions = TRUE,
  enums = TRUE,
  globals = TRUE,
  bitfield_type = "u8",
  include_bitfields = TRUE
) {
  if (is.null(ffi)) {
    ffi <- tcc_ffi()
  }
  if (!inherits(ffi, "tcc_ffi")) {
    stop("Expected tcc_ffi object", call. = FALSE)
  }

  if (functions) {
    bindings <- tcc_treesitter_bindings(header, mapper = mapper)
    if (length(bindings) > 0) {
      ffi <- do.call(tcc_bind, c(list(ffi), bindings))
    }
  }

  if (structs) {
    ffi <- tcc_treesitter_struct_bindings(
      ffi,
      header,
      mapper = mapper,
      bitfield_type = bitfield_type,
      include_bitfields = include_bitfields
    )
  }

  if (unions) {
    ffi <- tcc_treesitter_union_bindings(
      ffi,
      header,
      mapper = mapper,
      bitfield_type = bitfield_type,
      include_bitfields = include_bitfields
    )
  }

  if (enums) {
    enum_members <- tcc_treesitter_enum_members(header)
    if (nrow(enum_members) > 0) {
      split_members <- split(enum_members, enum_members$enum_name)
      for (nm in names(split_members)) {
        consts <- split_members[[nm]]$member_name
        consts <- consts[!is.na(consts) & nzchar(consts)]
        if (length(consts) > 0) {
          ffi <- tcc_enum(ffi, nm, constants = consts)
        }
      }
    }
  }

  if (globals) {
    g <- tcc_treesitter_global_types(header)
    if (nrow(g) > 0) {
      for (i in seq_len(nrow(g))) {
        nm <- g$text[i]
        c_type <- g$c_type[i]
        if (is.na(nm) || !nzchar(nm) || is.na(c_type) || !nzchar(c_type)) {
          next
        }
        ffi <- tcc_global(ffi, nm, mapper(c_type))
      }
    }
  }

  ffi
}
