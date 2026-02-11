# Parse struct declarations with treesitter.c

Parse struct declarations with treesitter.c

## Usage

``` r
tcc_treesitter_structs(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_struct_nodes()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_struct_nodes.html).

## Value

A data frame of struct nodes.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "struct point { double x; double y; };"
tcc_treesitter_structs(header)
} # }
```
