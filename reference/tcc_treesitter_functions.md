# Parse function declarations with treesitter.c

Parse function declarations with treesitter.c

## Usage

``` r
tcc_treesitter_functions(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_function_nodes()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_function_nodes.html).

## Value

A data frame of function nodes.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "double sqrt(double x);"
tcc_treesitter_functions(header)
} # }
```
