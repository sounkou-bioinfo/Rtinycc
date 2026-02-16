# Apply tcc_struct() bindings from a header

Apply tcc_struct() bindings from a header

## Usage

``` r
tcc_treesitter_struct_bindings(ffi, header, ...)
```

## Arguments

- ffi:

  A tcc_ffi object.

- header:

  Character scalar containing C declarations.

- ...:

  Passed to
  [`tcc_treesitter_struct_accessors()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_struct_accessors.md).

## Value

Updated tcc_ffi object.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "struct point { double x; double y; };"
ffi <- tcc_ffi()
ffi <- tcc_treesitter_struct_bindings(ffi, header)
} # }
```
