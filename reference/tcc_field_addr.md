# Generate field address getter helpers

Creates functions that return pointers to specific struct fields. Useful
for passing field pointers to C functions or for container_of.

## Usage

``` r
tcc_field_addr(ffi, struct_name, fields)
```

## Arguments

- ffi:

  A tcc_ffi object

- struct_name:

  Struct name

- fields:

  Character vector of field names

## Value

Updated tcc_ffi object

## Examples

``` r
if (FALSE) { # \dontrun{
ffi <- tcc_ffi() |>
  tcc_struct("point", list(x = "f64", y = "f64")) |>
  tcc_field_addr("point", c("x", "y")) # point_x_addr(), point_y_addr()
} # }
```
