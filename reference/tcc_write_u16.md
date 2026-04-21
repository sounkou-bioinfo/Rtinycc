# Write an unsigned 16-bit integer

Write an unsigned 16-bit integer

## Usage

``` r
tcc_write_u16(ptr, offset, value)
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
16-bit value into native memory at `ptr + offset`.
