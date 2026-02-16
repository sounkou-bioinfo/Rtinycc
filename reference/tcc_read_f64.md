# Read 64-bit doubles from a pointer

Read 64-bit doubles from a pointer

## Usage

``` r
tcc_read_f64(ptr, n = NULL, offset = 0L)
```

## Arguments

- ptr:

  External pointer

- n:

  Number of values to read (legacy vectorised interface). If provided,
  reads `n` consecutive f64 values starting at byte 0.

- offset:

  Byte offset from `ptr` (scalar interface). Ignored when `n` is
  supplied.

## Value

Numeric scalar (offset form) or numeric vector (n form).
