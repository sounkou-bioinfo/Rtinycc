# Declare enum for FFI helper generation

Generate R-callable helpers for enum constants and type conversions. The
enum must be defined in a header.

## Usage

``` r
tcc_enum(ffi, name, constants = NULL, export_constants = FALSE)
```

## Arguments

- ffi:

  A tcc_ffi object

- name:

  Enum name (as defined in C header)

- constants:

  Character vector of constant names to export

- export_constants:

  Whether to export enum constants as R functions

## Value

Updated tcc_ffi object

## Examples

``` r
if (FALSE) { # \dontrun{
ffi <- tcc_ffi() |>
  tcc_header("#include <errors.h>") |>
  tcc_enum("error_code", constants = c("OK", "ERROR"), export_constants = TRUE)
} # }
```
