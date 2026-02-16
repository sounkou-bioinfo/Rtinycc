# Schedule a callback to run on the main thread

Enqueue a callback for main-thread execution. Arguments must be basic
scalars or external pointers.

## Usage

``` r
tcc_callback_async_schedule(callback, args = list())
```

## Arguments

- callback:

  A tcc_callback object

- args:

  List of arguments to pass to the callback

## Value

NULL (invisible)
