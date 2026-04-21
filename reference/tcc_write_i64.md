# Write a signed 64-bit integer

Write a signed 64-bit integer

## Usage

``` r
tcc_write_i64(ptr, offset, value)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset

- value:

  Integer value to write

## Value

`NULL` (invisibly). Called for its side effect of writing one signed
64-bit value into native memory at `ptr + offset`.
