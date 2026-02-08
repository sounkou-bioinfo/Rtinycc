# Tests for treesitter helpers

library(tinytest)
library(Rtinycc)


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

# Test 2: struct members and accessors (including bitfield)
expect_true(
    {
        header <- "struct flags { unsigned int flag : 1; unsigned int code : 6; double x; };"
        members <- tcc_treesitter_struct_members(header)
        accessors <- tcc_treesitter_struct_accessors(header, bitfield_type = "u8")

        has_struct <- "flags" %in% names(accessors)
        acc <- accessors[["flags"]]

        has_struct &&
            all(c("flag", "code", "x") %in% names(acc)) &&
            acc[["flag"]] == "u8" &&
            acc[["code"]] == "u8" &&
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
        header <- "union data { int i; double d; };\nint global_counter = 0;"
        unions <- tcc_treesitter_unions(header)
        globals <- tcc_treesitter_globals(header)
        union_members <- tcc_treesitter_union_members(header)
        global_types <- tcc_treesitter_global_types(header)

        tmp <- tempfile(fileext = ".h")
        writeLines(c("#define FOO 1", "#define BAR 2"), tmp)
        defs <- tcc_treesitter_defines(tmp, use_cpp = FALSE)

        has_union <- "data" %in% unions$text
        has_global <- "global_counter" %in% globals$text
        has_union_members <- all(c("i", "d") %in% union_members$member_name)
        has_global_types <- any(
            global_types$text == "global_counter" & grepl("int", global_types$c_type)
        )
        has_defs <- all(c("FOO", "BAR") %in% defs)

        has_union && has_global && has_union_members && has_global_types && has_defs
    },
    info = "Parse unions, globals, and macro defines"
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
