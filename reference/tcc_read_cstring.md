# Read C-style string from pointer

Convert a C-style null-terminated string pointer back to R character
string. Handles UTF-8 decoding automatically.

## Usage

``` r
tcc_read_cstring(ptr)
```

## Arguments

- ptr:

  External pointer to C string

## Value

Character string
