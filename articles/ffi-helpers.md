# FFI Helper APIs

The helper APIs are the low-level tools you use when a binding needs
explicit memory management, pointer inspection, or typed reads and
writes.

## Allocation and Ownership

The owned-memory helpers are
[`tcc_malloc()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_malloc.md)
and
[`tcc_cstring()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_cstring.md).

``` r

buf <- tcc_malloc(16)
str_ptr <- tcc_cstring("hello")

tcc_ptr_is_owned(buf)
#> [1] TRUE
tcc_ptr_is_owned(str_ptr)
#> [1] TRUE
```

Both return owned external pointers. They can be freed explicitly with
[`tcc_free()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_free.md)
and also carry finalizers.

``` r

tcc_free(buf)
#> NULL
tcc_free(str_ptr)
#> NULL
```

## Reading and Writing Raw Bytes

``` r

buf <- tcc_malloc(8)
tcc_write_bytes(buf, as.raw(1:8))
#> NULL
tcc_read_bytes(buf, 8)
#> [1] 01 02 03 04 05 06 07 08
tcc_free(buf)
#> NULL
```

## Typed Reads and Writes

The helper layer also exposes typed accessors for primitive values at
byte offsets.

``` r

buf <- tcc_malloc(16)
tcc_write_i32(buf, 0L, 42L)
tcc_write_f64(buf, 8L, 3.5)

tcc_read_i32(buf, offset = 0L)
#> [1] 42
tcc_read_f64(buf, offset = 8L)
#> [1] 3.5

tcc_free(buf)
#> NULL
```

These helpers are useful for manual struct-like layouts, output buffers,
or pointer-heavy APIs that do not map cleanly onto ordinary R vectors.

## Pointer-to-Pointer Helpers

Some C APIs fill outputs through `T **` or `void **`. `Rtinycc` exposes
a small set of helpers for that pattern.

``` r

ptr_size <- if (!is.null(.Machine$sizeof.pointer)) .Machine$sizeof.pointer else 8L
ptr_ref <- tcc_malloc(ptr_size)
target <- tcc_malloc(4)

tcc_ptr_set(ptr_ref, target)
#> <pointer: 0x55f9807f6a30>
tcc_ptr_addr(tcc_data_ptr(ptr_ref))
#> [1] "94530119438064"
tcc_ptr_addr(target)
#> [1] "94530119438064"

tcc_ptr_set(ptr_ref, tcc_null_ptr())
#> <pointer: 0x55f9807f6a30>
tcc_ptr_is_null(tcc_data_ptr(ptr_ref))
#> [1] TRUE

tcc_free(target)
#> NULL
tcc_free(ptr_ref)
#> NULL
```

[`tcc_data_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_data_ptr.md)
returns a borrowed pointer view. It is not an owned allocation.

## C String Helpers

``` r

str_ptr <- tcc_cstring("Hello, world")
tcc_read_cstring(str_ptr)
#> [1] "Hello, world"
tcc_read_cstring(str_ptr, max_bytes = 5)
#> [1] "Hello"
tcc_free(str_ptr)
#> NULL
```

This helper is the safe way to allocate a C-owned NUL-terminated string
when the callee expects a mutable or longer-lived `char *`.
