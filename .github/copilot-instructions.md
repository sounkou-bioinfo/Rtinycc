# Rtinycc Agent Guidelines (Short)

This file is the minimum guidance for agentic work on Rtinycc. Read relevant code before changes, keep edits small and focused, and update tests/docs when you change behavior.

Agents must read actual implementations before guessing. If you need third-party code or headers, check the `.sync` directory for local copies of libraries we depend on so you can read them directly instead of assuming behavior. We also generate `<package>.llm.txt` files for R package docs; for example, treesitter.c docs can be found in `treesitter.llm.txt`.

## What this package does

Rtinycc embeds TinyCC and exposes both CLI and libtcc APIs to R. It provides a Bun-style FFI that generates SEXP wrappers at runtime, compiles them with TinyCC, and calls them via `.Call`.

## FFI provenance (critical)

`tcc_get_symbol()` calls `RC_libtcc_get_symbol()`, which converts the raw `void*` from TinyCC into a `DL_FUNC` and wraps it with `R_MakeExternalPtrFn` (tagged as a native symbol). `make_callable()` then passes that external pointer to `.RtinyccCall` (which is `base::.Call`). There is no `Rf_install` cast of the function pointer; `Rf_install` only sets the native symbol tag.

## Type system highlights

Scalar types: `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool`, `cstring`.

Array arguments (zero-copy): `raw`, `integer_array`, `numeric_array`, `logical_array`, `character_array`.

Array returns: `returns = list(type = "*_array", length_arg = <index>, free = TRUE)` copies into a new R vector and optionally frees the C buffer.

Struct array fields: `list(type = "u8", size = N, array = TRUE)` generates element accessors `struct_<name>_get_<field>_elt()` / `struct_<name>_set_<field>_elt()`.

Pointers: `ptr` and `sexp` are exposed as external pointers; ownership is tracked and enforced in `RC_free` and `tcc_ptr_is_owned()`.

## Build and test

- `make rd` regenerates roxygen output.
- `make test` runs tinytest.
- For a single file: `R -e "tinytest::run_test_file('inst/tinytest/<file>.R')"`.
- Development plumbing: run `air format` after edits, use `make install` for local installs, `make check` for CRAN-style checks, and `make rdm` to regenerate README. Always add or update tinytests for behavior changes.

## Editing rules

- Use roxygen2 for exported functions and run `make rd` when docs change.
- Keep tidyverse-style formatting (2-space indent, spaces around operators).
- Prefer early returns and explicit error messages with `stop(..., call. = FALSE)`.
- Update README examples when you change user-facing behavior.

## Key files

- R API: `R/ffi.R`, `R/ffi_types.R`, `R/ffi_codegen.R`, `R/tinycc.R`
- C API: `src/RC_libtcc.c`, `src/init.c`
- Tests: `inst/tinytest/`
- Docs: `README.Rmd`, `NEWS.md`