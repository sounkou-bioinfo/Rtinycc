# Get the address of an external pointer

Extract the memory address from an external pointer as a numeric value.
This is primarily useful for debugging and inspection purposes.

## Usage

``` r
get_external_ptr_addr(ptr)
```

## Arguments

- ptr:

  An external pointer object (e.g., from
  [`tcc_get_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_get_symbol.md)).

## Value

The memory address as a numeric value.
