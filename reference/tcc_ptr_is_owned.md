# Check for the `"rtinycc_owned"` tag

Returns `TRUE` only for pointers created by
[`tcc_malloc()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_malloc.md)
or
[`tcc_cstring()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_cstring.md).
Struct pointers (tagged `"struct_<name>"`) and borrowed pointers return
`FALSE`.

## Usage

``` r
tcc_ptr_is_owned(ptr)
```

## Arguments

- ptr:

  External pointer

## Value

Logical scalar
