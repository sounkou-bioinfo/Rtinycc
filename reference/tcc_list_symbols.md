# List symbols known to a libtcc state

Return the global symbols currently known to a libtcc state. This is a
symbol-table inspection helper for compiled/linked TCC states, not a DLL
export scanner and not a C signature discovery API. For meaningful
runtime addresses, call it after
[`tcc_relocate()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_relocate.md).

## Usage

``` r
tcc_list_symbols(state)
```

## Arguments

- state:

  A `tcc_state`.

## Value

A data frame with columns `name` and `address`, where `address` is a
hexadecimal character string.
