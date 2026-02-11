# Dereference a pointer-to-pointer

Treats `ptr_ref` as a pointer to a pointer and returns the pointed
address as an external pointer. This is useful for fields like `void**`
or `T**`.

## Usage

``` r
tcc_data_ptr(ptr_ref)
```

## Arguments

- ptr_ref:

  External pointer to a pointer value (e.g., address of a field).

## Value

An external pointer tagged `"rtinycc_borrowed"`. Not owned by Rtinycc
and never freed on garbage collection. Do not pass to
[`tcc_free()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_free.md).
