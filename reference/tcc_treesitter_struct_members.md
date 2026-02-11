# Parse struct members (including bitfields) with treesitter.c

Parse struct members (including bitfields) with treesitter.c

## Usage

``` r
tcc_treesitter_struct_members(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_struct_members()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_struct_members.html).

## Value

A data frame of struct members.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "struct point { double x; double y; };"
tcc_treesitter_struct_members(header)
} # }
```
