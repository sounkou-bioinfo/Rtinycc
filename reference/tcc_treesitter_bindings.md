# Generate tcc_bind() specs from a header

Generate tcc_bind() specs from a header

## Usage

``` r
tcc_treesitter_bindings(header, mapper = tcc_map_c_type_to_ffi)
```

## Arguments

- header:

  Character scalar containing C declarations.

- mapper:

  Function to map C types to FFI types.

## Value

Named list suitable for
[`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md).

## Examples

``` r
header <- "double sqrt(double x);"
symbols <- tcc_treesitter_bindings(header)
```
