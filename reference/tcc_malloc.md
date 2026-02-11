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

An external pointer tagged `"rtinycc_owned"` with an R finalizer. Freed
on garbage collection or explicitly via
[`tcc_free()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_free.md).
