# Get the C-compatible function pointer

Returns an external pointer that can be passed to compiled C code as
user data for trampolines. Keep this handle (and the original
`tcc_callback`) alive for as long as C may call back. The pointer handle
keeps the underlying token alive until it is garbage collected, even if
the original callback is closed.

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
Use
[`tcc_read_bytes()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_bytes.md),
[`tcc_read_u8()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_u8.md),
or
[`tcc_read_f64()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_f64.md)
to inspect pointed data when needed.
