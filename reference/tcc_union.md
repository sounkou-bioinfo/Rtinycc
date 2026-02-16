# Declare union for FFI helper generation

Generate R-callable helpers for union allocation and member access. The
union must be defined in a header.

## Usage

``` r
tcc_union(ffi, name, members, active = NULL)
```

## Arguments

- ffi:

  A tcc_ffi object

- name:

  Union name (as defined in C header)

- members:

  Named list of union members with FFI types

- active:

  Default active member for accessors

## Value

Updated tcc_ffi object

## Examples

``` r
if (FALSE) { # \dontrun{
ffi <- tcc_ffi() |>
  tcc_union("data_variant",
    members = list(as_int = "i32", as_float = "f32"),
    active = "as_int"
  )
} # }
```
