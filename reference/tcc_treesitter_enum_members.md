# Parse enum members with treesitter.c

Parse enum members with treesitter.c

## Usage

``` r
tcc_treesitter_enum_members(header, ...)
```

## Arguments

- header:

  Character scalar containing C declarations.

- ...:

  Additional arguments passed to
  [`treesitter.c::get_enum_members_from_root()`](https://sounkou-bioinfo.github.io/treesitter.c/reference/get_enum_members_from_root.html).

## Value

A data frame of enum members.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "enum status { OK = 0, ERR = 1 };"
tcc_treesitter_enum_members(header)
} # }
```
