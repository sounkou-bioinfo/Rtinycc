# Check if a tcc_symbol external pointer is valid

Check if a tcc_symbol external pointer is valid

## Usage

``` r
tcc_symbol_is_valid(ptr)
```

## Arguments

- ptr:

  External pointer from
  [`tcc_get_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_get_symbol.md).

## Value

TRUE if the pointer address is non-null, FALSE otherwise.
