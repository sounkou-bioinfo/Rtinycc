# Close/unregister a callback

Invalidates a callback immediately, releases the preserved R function
reference, and cleans up callback resources as early as possible. This
is recommended for deterministic lifetime management, but callbacks are
also eventually released by finalizers if you simply drop all
references.

## Usage

``` r
tcc_callback_close(callback)
```

## Arguments

- callback:

  A tcc_callback object returned by tcc_callback()

## Value

NULL (invisible)
