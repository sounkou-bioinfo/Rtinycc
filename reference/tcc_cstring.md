# Create a C-style string pointer

Convert R character strings to C-style null-terminated string pointers.
This handles UTF-8 encoding and null termination automatically.

## Usage

``` r
tcc_cstring(str)
```

## Arguments

- str:

  Character string

## Value

An external pointer tagged `"rtinycc_owned"` pointing to a malloc'd copy
of the string. Freed on garbage collection or via
[`tcc_free()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_free.md).
