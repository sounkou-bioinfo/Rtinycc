# Read unsigned 8-bit values from a pointer

Read unsigned 8-bit values from a pointer

## Usage

``` r
tcc_read_u8(ptr, n = NULL, offset = 0L)
```

## Arguments

- ptr:

  External pointer

- n:

  Number of values to read (legacy vectorised interface). If provided,
  reads `n` consecutive u8 values starting at byte 0.

- offset:

  Byte offset from `ptr` (scalar interface). Ignored when `n` is
  supplied.

## Value

Integer scalar (offset form) or integer vector (n form).
