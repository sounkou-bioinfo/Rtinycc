# Get the C-compatible function pointer

Returns an external pointer that can be passed to compiled C code as a
function pointer. This is the opaque pointer used by trampolines.

## Usage

``` r
tcc_callback_ptr(callback)
```

## Arguments

- callback:

  A tcc_callback object

## Value

An external pointer (address of the callback token)
