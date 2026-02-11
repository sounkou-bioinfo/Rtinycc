# Map a C type string to an Rtinycc FFI type

Map a C type string to an Rtinycc FFI type

## Usage

``` r
tcc_map_c_type_to_ffi(c_type)
```

## Arguments

- c_type:

  C type string (e.g., "int", "double", "char \*").

## Value

A single FFI type string.

## Examples

``` r
if (FALSE) { # \dontrun{
tcc_map_c_type_to_ffi("int")
tcc_map_c_type_to_ffi("const char *")
} # }
```
