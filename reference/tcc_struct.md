# Declare struct for FFI helper generation

Generate R-callable helpers for struct allocation, field access, and
pointer management. The struct must be defined in a header.

## Usage

``` r
tcc_struct(ffi, name, accessors)
```

## Arguments

- ffi:

  A tcc_ffi object

- name:

  Struct name (as defined in C header)

- accessors:

  Named list of field accessors where names are field names and values
  are FFI types (e.g., list(x="f64", y="f64")). Named nested struct
  fields can use `"struct:<name>"` to generate borrowed nested-view
  getters and copy-in setters (for example `child = "struct:child"`).
  Fixed character arrays use `list(type = "cstring", size = n)`. Bare
  `cstring` pointer fields are rejected because setters cannot safely
  retain borrowed R string storage; use `ptr` with explicitly owned
  storage instead.

## Value

Updated tcc_ffi object

## Examples

``` r
if (FALSE) { # \dontrun{
ffi <- tcc_ffi() |>
  tcc_header("#include <point.h>") |>
  tcc_struct("point", list(x = "f64", y = "f64", id = "i32"))
} # }
```
