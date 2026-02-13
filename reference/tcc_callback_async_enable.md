# Enable async callback dispatcher (main-thread queue)

Initializes the async callback dispatcher so callbacks can be scheduled
from non-R threads and executed on the main R thread.

## Usage

``` r
tcc_callback_async_enable()
```

## Value

NULL (invisible)

## Details

On Unix-like platforms, this registers an event-loop input handler. On
Windows, this sets up a hidden message-window dispatcher. In both cases,
scheduled callbacks are drained automatically when the R event loop
runs.
