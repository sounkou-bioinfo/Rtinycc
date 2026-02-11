# Read a pointer at byte offset

Dereferences a `void*` at the given byte offset from `ptr`. Equivalent
to `*(void**)(ptr + offset)`. The returned pointer is tagged
`"rtinycc_borrowed"` and will not be freed by the garbage collector.

## Usage

``` r
tcc_read_ptr(ptr, offset = 0L)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset from `ptr` (scalar interface). Ignored when `n` is
  supplied.

## Value

External pointer
