# Write a 64-bit double

Write a 64-bit double

## Usage

``` r
tcc_write_f64(ptr, offset, value)
```

## Arguments

- ptr:

  External pointer

- offset:

  Byte offset

- value:

  Integer value to write

## Value

`NULL` (invisibly). Called for its side effect of writing one 64-bit
floating-point value into native memory at `ptr + offset`.
