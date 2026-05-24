# List symbols known to a libtcc state

Return the global symbols currently reported by libtcc for a state. This
is a best-effort symbol-table inspection helper for compiled/linked TCC
states, not a portable exhaustive symbol enumerator, not a DLL export
scanner, and not a C signature discovery API. Platform backends may omit
symbols that are still resolvable with
[`tcc_get_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_get_symbol.md).
For meaningful runtime addresses, call it after
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
