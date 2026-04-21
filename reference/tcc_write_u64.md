# Write an unsigned 64-bit integer

Write an unsigned 64-bit integer

## Usage

``` r
tcc_write_u64(ptr, offset, value)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset

- value:

  Integer value to write

## Value

`NULL` (invisibly). Called for its side effect of writing one unsigned
64-bit value into native memory at `ptr + offset`.
