# Read unsigned 32-bit integer

Read unsigned 32-bit integer

## Usage

``` r
tcc_read_u32(ptr, offset = 0L)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset from `ptr` (scalar interface). Ignored when `n` is
  supplied.

## Value

Numeric scalar (double, exact up to 2^32-1).
