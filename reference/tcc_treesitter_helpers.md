# Treesitter helpers for header-driven bindings

Convenience wrappers around `treesitter.c` that map C types to the
Rtinycc FFI type system and return
[`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md)-ready
specs.

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to treesitter.c helpers.

## Value

A data frame of nodes or a named list of binding specs.

## treesitter.c

These helpers require the optional treesitter.c package.

## Examples

``` r
header <- "double sqrt(double x);\nint add(int a, int b);"
tcc_treesitter_functions(header)
#>   capture_name text start_line start_col   params return_type
#> 1    decl_name sqrt          1         8   double      double
#> 2    decl_name  add          2         5 int, int         int
tcc_treesitter_bindings(header)
#> $sqrt
#> $sqrt$args
#> $sqrt$args[[1]]
#> [1] "f64"
#> 
#> 
#> $sqrt$returns
#> [1] "f64"
#> 
#> 
#> $add
#> $add$args
#> $add$args[[1]]
#> [1] "i32"
#> 
#> $add$args[[2]]
#> [1] "i32"
#> 
#> 
#> $add$returns
#> [1] "i32"
#> 
#> 
```
