# Tests for treesitter helpers

library(tinytest)
library(Rtinycc)

composite_semantics <- Rtinycc:::rtinycc_composite_semantics()
bitfield_type <- composite_semantics$bitfield_native$treesitter_bitfield_type


if (!requireNamespace("treesitter.c", quietly = TRUE)) {
  message("Skipping treesitter helper tests: treesitter.c not installed")
  exit_file(msg = "treesitter.c not available")
} else {
  # check if version >= 0.0.4
  version <- packageVersion("treesitter.c")
  if (version < "0.0.4") {
    message("Skipping treesitter helper tests: treesitter.c version too old")
    exit_file(msg = "treesitter.c version too old")
  }
}

# Test 1: function parsing and bindings
expect_true(
  {
    header <- "double sqrt(double x);\nint add(int a, int b);"
    funcs <- tcc_treesitter_functions(header)
    symbols <- tcc_treesitter_bindings(header)

    has_funcs <- all(c("sqrt", "add") %in% funcs$text)
    has_symbols <- all(c("sqrt", "add") %in% names(symbols))
    add_args <- symbols$add$args
    add_ret <- symbols$add$returns

    has_funcs &&
      has_symbols &&
      length(add_args) == 2 &&
      all(unlist(add_args) == c("i32", "i32")) &&
      add_ret == "i32"
  },
  info = "Parse functions and map to FFI bindings"
)

# Test 1b: conservative mapping keeps char pointers as ptr in generated bindings
expect_true(
  {
    symbols <- tcc_treesitter_bindings("int puts(const char *s);")

    identical(symbols$puts$args[[1]], "ptr") &&
      identical(symbols$puts$returns, "i32")
  },
  info = paste(
    "treesitter-generated bindings keep const char * arguments on the",
    "default conservative ptr mapping"
  )
)

# Test 2: struct members and accessors (including bitfield)
expect_true(
  {
    header <- "struct flags { unsigned int flag : 1; unsigned int code : 6; double x; };"
    members <- tcc_treesitter_struct_members(header)
    accessors <- tcc_treesitter_struct_accessors(
      header,
      bitfield_type = bitfield_type
    )

    has_struct <- "flags" %in% names(accessors)
    acc <- accessors[["flags"]]

    has_struct &&
      all(c("flag", "code", "x") %in% names(acc)) &&
      is.list(acc[["flag"]]) &&
      identical(acc[["flag"]]$type, bitfield_type) &&
      isTRUE(acc[["flag"]]$bitfield) &&
      identical(acc[["flag"]]$width, 1L) &&
      is.list(acc[["code"]]) &&
      identical(acc[["code"]]$type, bitfield_type) &&
      isTRUE(acc[["code"]]$bitfield) &&
      identical(acc[["code"]]$width, 6L) &&
      acc[["x"]] == "f64" &&
      nrow(members) >= 3
  },
  info = "Extract struct members and build accessors"
)

# Test 3: struct bindings integrate with tcc_struct()
expect_true(
  {
    header <- "struct point { double x; double y; };"
    ffi <- tcc_ffi() |>
      tcc_source("struct point { double x; double y; };")

    ffi <- tcc_treesitter_struct_bindings(ffi, header)
    compiled <- tcc_compile(ffi)

    p <- compiled$struct_point_new()
    p <- compiled$struct_point_set_x(p, 1.25)
    p <- compiled$struct_point_set_y(p, 2.5)
    x <- compiled$struct_point_get_x(p)
    y <- compiled$struct_point_get_y(p)
    compiled$struct_point_free(p)

    abs(x - 1.25) < 1e-12 && abs(y - 2.5) < 1e-12
  },
  info = "Generate and use struct bindings from header"
)

# Test 4: enum bindings (constants list provided)
expect_true(
  {
    header <- "enum status { OK = 0, ERR = 1 };"
    ffi <- tcc_ffi() |>
      tcc_source("enum status { OK = 0, ERR = 1 };")

    ffi <- tcc_treesitter_enum_bindings(
      ffi,
      header,
      constants = list(status = c("OK", "ERR"))
    )
    compiled <- tcc_compile(ffi)

    ok <- compiled$enum_status_OK()
    err <- compiled$enum_status_ERR()

    ok == 0 && err == 1
  },
  info = "Generate enum bindings from header"
)

# Test 5: unions, globals, and defines
expect_true(
  {
    header <- "union data { int i; double d; struct { int x; } inner; };\nint global_counter = 0;"
    named_header <- "struct inner { int x; }; union named_data { struct inner payload; int y; };"
    unions <- tcc_treesitter_unions(header)
    globals <- tcc_treesitter_globals(header)
    union_members <- tcc_treesitter_union_members(header)
    union_accessors <- tcc_treesitter_union_accessors(header)
    named_union_accessors <- tcc_treesitter_union_accessors(named_header)
    global_types <- tcc_treesitter_global_types(header)

    tmp <- tempfile(fileext = ".h")
    writeLines(c("#define FOO 1", "#define BAR 2"), tmp)
    defs <- tcc_treesitter_defines(tmp, use_cpp = FALSE)

    has_union <- "data" %in% unions$text
    has_global <- "global_counter" %in% globals$text
    has_union_members <- all(
      c("i", "d", "inner") %in% union_members$member_name
    )
    has_nested_union_type <- is.list(union_accessors$data$inner) &&
      identical(union_accessors$data$inner$type, "struct")
    has_named_nested_union_type <- is.list(
      named_union_accessors$named_data$payload
    ) &&
      identical(named_union_accessors$named_data$payload$type, "struct") &&
      identical(named_union_accessors$named_data$payload$struct_name, "inner")
    has_global_types <- any(
      global_types$text == "global_counter" & grepl("int", global_types$c_type)
    )
    has_defs <- all(c("FOO", "BAR") %in% defs)

    has_union &&
      has_global &&
      has_union_members &&
      has_nested_union_type &&
      has_named_nested_union_type &&
      has_global_types &&
      has_defs
  },
  info = "Parse unions, globals, nested union structs, and macro defines"
)

# Test 6: enum members and generate bindings
expect_true(
  {
    header <- paste(
      "double sqrt(double x);",
      "struct point { double x; double y; };",
      "union data { int i; float f; };",
      "enum status { OK = 0, ERR = 1 };",
      "int global_counter;",
      sep = "\n"
    )

    enum_members <- tcc_treesitter_enum_members(header)
    has_enum_members <- all(c("OK", "ERR") %in% enum_members$member_name)

    ffi <- tcc_ffi() |>
      tcc_source(paste(
        "double sqrt(double x) { return x; }",
        "struct point { double x; double y; };",
        "union data { int i; float f; };",
        "enum status { OK = 0, ERR = 1 };",
        "int global_counter = 7;",
        sep = "\n"
      ))

    ffi <- tcc_generate_bindings(
      ffi,
      header,
      functions = TRUE,
      structs = TRUE,
      unions = TRUE,
      enums = TRUE,
      globals = TRUE
    )

    compiled <- tcc_compile(ffi)

    p <- compiled$struct_point_new()
    p <- compiled$struct_point_set_x(p, 1.0)
    x <- compiled$struct_point_get_x(p)
    compiled$struct_point_free(p)

    u <- compiled$union_data_new()
    u <- compiled$union_data_set_i(u, 42L)
    ui <- compiled$union_data_get_i(u)
    compiled$union_data_free(u)

    ok <- compiled$enum_status_OK()
    gc_before <- compiled$global_global_counter_get()
    compiled$global_global_counter_set(9L)
    gc_after <- compiled$global_global_counter_get()

    has_enum_members &&
      abs(x - 1.0) < 1e-12 &&
      ui == 42L &&
      ok == 0 &&
      gc_before == 7L &&
      gc_after == 9L
  },
  info = "Generate bindings for structs, unions, enums, globals"
)

# Test 7: treesitter-generated bitfield bindings preserve helper metadata end-to-end
expect_true(
  {
    header <- "struct flags { unsigned int flag : 1; unsigned int code : 6; };"
    ffi <- tcc_ffi() |>
      tcc_source(header) |>
      tcc_treesitter_struct_bindings(header, bitfield_type = bitfield_type) |>
      tcc_bind() |>
      tcc_compile()

    helper_specs <- get(".helper_specs", envir = ffi, inherits = FALSE)
    s <- ffi$struct_flags_new()
    s <- ffi$struct_flags_set_flag(s, 1L)
    s <- ffi$struct_flags_set_code(s, 17L)
    ok_runtime <- identical(ffi$struct_flags_get_flag(s), 1L) &&
      identical(ffi$struct_flags_get_code(s), 17L)
    ffi$struct_flags_free(s)

    identical(
      Rtinycc:::helper_symbol_operation(helper_specs$struct_flags_get_flag),
      "bitfield_getter"
    ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_flags_set_flag),
        "bitfield_setter"
      ) &&
      ok_runtime
  },
  info = "treesitter-generated bitfield bindings preserve bitfield helper semantics end-to-end"
)

# Test 8: treesitter-generated bitfields still reject address-style helpers
expect_error(
  {
    header <- "struct flags { unsigned int flag : 1; };"
    ffi <- tcc_ffi() |>
      tcc_source(header) |>
      tcc_treesitter_struct_bindings(header, bitfield_type = bitfield_type) |>
      tcc_field_addr("flags", "flag")
    ffi
  },
  info = "treesitter-generated bitfields reject field_addr"
)
expect_error(
  {
    header <- "struct flags { unsigned int flag : 1; };"
    ffi <- tcc_ffi() |>
      tcc_source(header) |>
      tcc_treesitter_struct_bindings(header, bitfield_type = bitfield_type) |>
      tcc_container_of("flags", "flag")
    ffi
  },
  info = "treesitter-generated bitfields reject container_of"
)

# Test 9: treesitter-generated nested union struct bindings preserve nested-view metadata end-to-end
expect_true(
  {
    header <- "struct inner { int x; }; union wrapper { struct inner payload; int raw; };"
    ffi <- tcc_ffi() |>
      tcc_source(header) |>
      tcc_treesitter_struct_bindings(header) |>
      tcc_treesitter_union_bindings(header) |>
      tcc_bind() |>
      tcc_compile()

    helper_specs <- get(".helper_specs", envir = ffi, inherits = FALSE)
    u <- ffi$union_wrapper_new()
    inner <- ffi$union_wrapper_get_payload(u)
    inner <- ffi$struct_inner_set_x(inner, 33L)
    ok_runtime <- identical(ffi$struct_inner_get_x(inner), 33L)
    ffi$union_wrapper_free(u)

    identical(
      Rtinycc:::helper_symbol_operation(helper_specs$union_wrapper_get_payload),
      "nested_view"
    ) &&
      ok_runtime
  },
  info = "treesitter-generated nested union struct bindings preserve nested-view semantics end-to-end"
)

# Test 10: treesitter-generated nested struct members still fall back to ptr-like accessors when the parser does not recover the nested type name
expect_true(
  {
    header <- "struct child { int x; }; struct outer { struct child child; int y; };"
    accessors <- tcc_treesitter_struct_accessors(header)
    is.character(accessors$outer$child) &&
      identical(accessors$outer$child, "ptr") &&
      identical(accessors$outer$y, "i32")
  },
  info = "treesitter-generated nested struct members currently fall back to ptr-like accessors when the nested type name is unavailable"
)
