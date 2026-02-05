# Parse global declarations with types using treesitter.c

Parse global declarations with types using treesitter.c

## Usage

``` r
tcc_treesitter_global_types(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_globals_with_types_from_root()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_globals_with_types_from_root.html).

## Value

A data frame of global names and C types.

## Examples

``` r
header <- "int global_counter;"
tcc_treesitter_global_types(header)
#>   capture_name           text start_line c_type
#> 1  global_name global_counter          1    int
```
