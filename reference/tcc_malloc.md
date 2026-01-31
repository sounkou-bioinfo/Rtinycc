# Allocate memory buffer

Allocate a memory buffer of specified size, equivalent to C malloc.
Returns an external pointer that can be passed to FFI functions.

## Usage

``` r
tcc_malloc(size)
```

## Arguments

- size:

  Number of bytes to allocate

## Value

External pointer to allocated memory
