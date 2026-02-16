# Apply tcc_union() bindings from a header

Apply tcc_union() bindings from a header

## Usage

``` r
tcc_treesitter_union_bindings(ffi, header, ...)
```

## Arguments

- ffi:

  A tcc_ffi object.

- header:

  Character scalar containing C declarations.

- ...:

  Passed to
  [`tcc_treesitter_union_accessors()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_union_accessors.md).

## Value

Updated tcc_ffi object.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "union data { int i; double d; };"
ffi <- tcc_ffi()
ffi <- tcc_treesitter_union_bindings(ffi, header)
} # }
```
