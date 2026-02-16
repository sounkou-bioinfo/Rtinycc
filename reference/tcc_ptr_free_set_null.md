# Free the pointed memory and set to NULL

Frees the memory pointed to by `ptr_ref` and sets the pointer to NULL.
Use this only when the pointed memory is not already owned by another
external pointer with its own finalizer.

## Usage

``` r
tcc_ptr_free_set_null(ptr_ref)
```

## Arguments

- ptr_ref:

  External pointer to a pointer value.

## Value

The updated pointer reference (invisibly).
