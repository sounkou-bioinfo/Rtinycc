# Read 32-bit float

Read 32-bit float

## Usage

``` r
tcc_read_f32(ptr, offset = 0L)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset from `ptr` (scalar interface). Ignored when `n` is
  supplied.

## Value

Numeric scalar (promoted to double).
