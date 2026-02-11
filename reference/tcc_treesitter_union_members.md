# Parse union members with treesitter.c

Parse union members with treesitter.c

## Usage

``` r
tcc_treesitter_union_members(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_union_members_from_root()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_union_members_from_root.html).

## Value

A data frame of union members.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "union data { int i; double d; };"
tcc_treesitter_union_members(header)
} # }
```
