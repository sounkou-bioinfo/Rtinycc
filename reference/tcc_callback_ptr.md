# Get the C-compatible function pointer

Returns an external pointer that can be passed to compiled C code as a
function pointer. This is the opaque pointer used by trampolines. The
pointer handle keeps the underlying token alive until it is garbage
collected, even if the original callback is closed.

## Usage

``` r
tcc_callback_ptr(callback)
```

## Arguments

- callback:

  A tcc_callback object

## Value

An external pointer (address of the callback token)

## Details

Pointer arguments and return values are treated as external pointers.
Use \codetcc_read_bytes() or \codetcc_read\_\*() helpers to inspect
pointed data when needed.
