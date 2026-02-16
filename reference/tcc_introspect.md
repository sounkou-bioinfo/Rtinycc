# Enable introspection helpers

Generates sizeof, alignof, and offsetof helper functions for structs,
unions, and enums. Useful for debugging or when you need to know C
layout information from R.

## Usage

``` r
tcc_introspect(ffi)
```

## Arguments

- ffi:

  A tcc_ffi object

## Value

Updated tcc_ffi object
