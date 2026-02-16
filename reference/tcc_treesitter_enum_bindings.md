# Apply tcc_enum() bindings from a header

Apply tcc_enum() bindings from a header

## Usage

``` r
tcc_treesitter_enum_bindings(ffi, header, constants = NULL)
```

## Arguments

- ffi:

  A tcc_ffi object.

- header:

  Character scalar containing C declarations.

- constants:

  Named list of enum constants.

## Value

Updated tcc_ffi object.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "enum status { OK = 0, ERR = 1 };"
ffi <- tcc_ffi()
ffi <- tcc_treesitter_enum_bindings(ffi, header, constants = list(status = c("OK", "ERR")))
} # }
```
