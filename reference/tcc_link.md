# Link an external shared library with Bun-style FFI bindings

Link a system library (like libsqlite3.so) and generate type-safe
wrappers automatically using TinyCC JIT compilation (API mode). Unlike
dlopen(), this uses TinyCC to compile bindings that handle type
conversion between R and C automatically.

## Usage

``` r
tcc_link(
  path,
  symbols,
  headers = NULL,
  libs = character(0),
  lib_paths = character(0),
  include_paths = character(0),
  verbose = FALSE
)
```

## Arguments

- path:

  Path to the shared library (e.g., "libsqlite3.so")

- symbols:

  Named list of symbol definitions with:

  - args: List of FFI types for arguments

  - returns: FFI type for return value

- headers:

  Optional C headers to include

- libs:

  Library names to link (e.g., "sqlite3")

- lib_paths:

  Additional library search paths

- include_paths:

  Additional include search paths

- verbose:

  Print debug information

## Value

A tcc_compiled object with callable functions

## Examples

``` r
if (FALSE) { # \dontrun{
# Link SQLite with type-safe bindings
sqlite <- tcc_link(
  "libsqlite3.so",
  symbols = list(
    sqlite3_libversion = list(args = list(), returns = "cstring"),
    sqlite3_open = list(args = list("cstring", "ptr"), returns = "i32")
  ),
  libs = "sqlite3"
)

# Call directly - type conversion happens automatically
sqlite$sqlite3_libversion()
} # }
```
