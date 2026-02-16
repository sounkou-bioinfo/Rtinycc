# Link an external shared library with Bun-style FFI bindings

Link a system library (like libsqlite3) and generate type-safe wrappers
automatically using TinyCC JIT compilation (API mode). Unlike dlopen(),
this uses TinyCC to compile bindings that handle type conversion between
R and C automatically.

## Usage

``` r
tcc_link(
  path,
  symbols,
  headers = NULL,
  libs = character(0),
  lib_paths = character(0),
  include_paths = character(0),
  user_code = NULL,
  verbose = FALSE
)
```

## Arguments

- path:

  Library short name (e.g., "m", "sqlite3") or full path to the shared
  library. Short names are resolved using platform-appropriate suffixes
  (`.so`, `.dylib`, `.dll`).

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

- user_code:

  Optional custom C code to include in the compilation

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

# Example with custom user code for helper functions
math_with_helpers <- tcc_link(
  "m",
  symbols = list(
    sqrt = list(args = list("f64"), returns = "f64"),
    safe_sqrt = list(args = list("f64"), returns = "f64")
  ),
  user_code = "
    #include <math.h>

    // Helper function that validates input before calling sqrt
    double safe_sqrt(double x) {
      if (x < 0) {
        return NAN;
      }
      return sqrt(x);
    }
  ",
  libs = "m"
)
math_with_helpers$safe_sqrt(16.0)
math_with_helpers$safe_sqrt(-4.0) # Returns NaN for negative input
} # }
```
