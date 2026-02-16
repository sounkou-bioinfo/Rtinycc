# Declare a global variable getter

Register a global C symbol so the compiled object exposes getter/setter
functions `global_<name>_get()` and `global_<name>_set()`.

## Usage

``` r
tcc_global(ffi, name, type)
```

## Arguments

- ffi:

  A tcc_ffi object

- name:

  Global symbol name

- type:

  FFI type for the global (scalar types only)

## Value

Updated tcc_ffi object (for chaining)

## Details

Globals are limited to scalar FFI types. Array types are rejected.
Scalar conversions follow the same rules as wrapper arguments:

- Integer inputs (`i8`, `i16`, `i32`, `u8`, `u16`) must be finite and
  within range; `NA` values error.

- Large integer types (`i64`, `u32`, `u64`) are mediated through R
  numeric (double). Values must be integer-valued and within range; for
  `i64`/`u64` only exact integers up to \$2^53\$ are accepted.

- `bool` rejects `NA` logicals.

Ownership notes:

- `ptr` globals store the raw address from an external pointer. If the
  external pointer owns memory, keep it alive; otherwise the pointer may
  be freed while the global still points to it.

- `cstring` globals store a borrowed pointer to R's string data (UTF-8
  translation). Do not free it; for C-owned strings prefer a `ptr`
  global and manage lifetime explicitly (e.g., with
  [`tcc_cstring()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_cstring.md)).

## Note

Global helpers are generated inside the compiled TCC unit. Recompiling
creates a new instance of the global variable; existing compiled objects
continue to refer to their own copy.

## Examples

``` r
ffi <- tcc_ffi() |>
  tcc_source("int global_counter = 7;") |>
  tcc_global("global_counter", "i32") |>
  tcc_compile()
ffi$global_global_counter_get()
#> [1] 7
```
