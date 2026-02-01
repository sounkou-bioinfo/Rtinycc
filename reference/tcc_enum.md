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

- export_constants:

  Whether to export enum constants as R functions

## Value

Updated tcc_ffi object

## Examples

``` r
ffi <- tcc_ffi() %>%
  tcc_header("#include <errors.h>") %>%
  tcc_enum("error_code", export_constants = TRUE)
#> Error in tcc_ffi() %>% tcc_header("#include <errors.h>") %>% tcc_enum("error_code",     export_constants = TRUE): could not find function "%>%"
```
