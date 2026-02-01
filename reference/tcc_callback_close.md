# Close/unregister a callback

Releases the preserved R function reference and cleans up resources.
Must be called when the callback is no longer needed.

## Usage

``` r
tcc_callback_close(callback)
```

## Arguments

- callback:

  A tcc_callback object returned by tcc_callback()

## Value

NULL (invisible)
