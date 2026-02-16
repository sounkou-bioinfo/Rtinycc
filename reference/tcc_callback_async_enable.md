# Enable async callback dispatcher (main-thread queue)

Initializes an event-loop handler so callbacks can be scheduled from
non-R threads and executed on the main R thread. This is currently
supported on Unix-like systems only.

## Usage

``` r
tcc_callback_async_enable()
```

## Value

NULL (invisible)
