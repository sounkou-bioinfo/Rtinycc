# Compile FFI bindings

Compile the defined symbols into callable functions. This generates C
wrapper code and compiles it with TinyCC.

## Usage

``` r
tcc_compile(ffi, verbose = FALSE)
```

## Arguments

- ffi:

  A tcc_ffi object

- verbose:

  Print compilation info

## Value

A tcc_compiled object with callable functions
