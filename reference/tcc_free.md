# Free owned memory

Free memory whose external pointer is tagged `"rtinycc_owned"` (e.g.
from
[`tcc_malloc()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_malloc.md)
or
[`tcc_cstring()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_cstring.md)).
Errors on struct pointers (use the generated `struct_<name>_free()`) or
borrowed pointers from
[`tcc_data_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_data_ptr.md).

## Usage

``` r
tcc_free(ptr)
```

## Arguments

- ptr:

  External pointer to free

## Value

`NULL`.
