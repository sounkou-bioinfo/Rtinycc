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

External pointer to C string
