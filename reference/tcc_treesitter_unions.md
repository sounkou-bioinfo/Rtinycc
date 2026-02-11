# Parse union declarations with treesitter.c

Parse union declarations with treesitter.c

## Usage

``` r
tcc_treesitter_unions(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_union_nodes()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_union_nodes.html).

## Value

A data frame of union nodes.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "union data { int i; double d; };"
tcc_treesitter_unions(header)
} # }
```
