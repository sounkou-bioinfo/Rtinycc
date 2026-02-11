# Declare a global variable getter

Register a global C symbol so the compiled object exposes getter/setter
functions `global_<name>_get()` and `global_<name>_set()`.

## Usage

``` r
tcc_global(ffi, name, type)
```

## Arguments

- ffi:

  A tcc_ffi object

- name:

  Global symbol name

- type:

  FFI type for the global (scalar types only)

## Value

Updated tcc_ffi object (for chaining)

## Examples

``` r
ffi <- tcc_ffi() |>
  tcc_source("int global_counter = 7;") |>
  tcc_global("global_counter", "i32") |>
  tcc_compile()
ffi$global_global_counter_get()
#> [1] 7
```
