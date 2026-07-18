# Schedule a callback to run on the main thread

Enqueue a callback for main-thread execution. Arguments must be basic
scalars or external pointers. The callback must remain open until the
task executes. External-pointer arguments are borrowed addresses: their
owners and pointees must remain valid until execution finishes.

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
