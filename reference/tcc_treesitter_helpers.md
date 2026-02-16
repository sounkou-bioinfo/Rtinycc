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
if (FALSE) { # \dontrun{
header <- "double sqrt(double x);\nint add(int a, int b);"
tcc_treesitter_functions(header)
tcc_treesitter_bindings(header)
} # }
```
