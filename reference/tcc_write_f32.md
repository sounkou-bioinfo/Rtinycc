# Write a 32-bit float

Write a 32-bit float

## Usage

``` r
tcc_write_f32(ptr, offset, value)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset

- value:

  Integer value to write

## Value

`NULL` (invisibly). Called for its side effect of writing one 32-bit
floating-point value into native memory at `ptr + offset`.
