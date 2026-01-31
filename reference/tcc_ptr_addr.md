# Get pointer address as integer

Get the numeric address of an external pointer, useful for debugging and
when APIs require pointer addresses as integers. Optional hex mode
available.

## Usage

``` r
tcc_ptr_addr(ptr, hex = FALSE)
```

## Arguments

- ptr:

  External pointer

- hex:

  Whether to display in hexadecimal (default: FALSE)

## Value

Character representation of pointer address (hex if requested, decimal
otherwise)
