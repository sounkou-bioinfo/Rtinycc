# Parse enum declarations with treesitter.c

Parse enum declarations with treesitter.c

## Usage

``` r
tcc_treesitter_enums(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_enum_nodes()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_enum_nodes.html).

## Value

A data frame of enum nodes.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "enum status { OK = 0, ERR = 1 };"
tcc_treesitter_enums(header)
} # }
```
