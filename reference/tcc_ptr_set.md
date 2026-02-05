# Set a pointer-to-pointer value

Assigns the address in `ptr_value` to the location pointed to by
`ptr_ref`.

## Usage

``` r
tcc_ptr_set(ptr_ref, ptr_value)
```

## Arguments

- ptr_ref:

  External pointer to a pointer value (e.g., address of a field).

- ptr_value:

  External pointer to store.

## Value

The updated pointer reference (invisibly).
