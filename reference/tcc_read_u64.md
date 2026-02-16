# Read unsigned 64-bit integer

Read unsigned 64-bit integer

## Usage

``` r
tcc_read_u64(ptr, offset = 0L)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset from `ptr` (scalar interface). Ignored when `n` is
  supplied.

## Value

Numeric scalar (double, exact up to 2^53).
