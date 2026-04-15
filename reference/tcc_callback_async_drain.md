# Drain the async callback queue

Execute any pending async callbacks immediately on the main R thread.
Normally callbacks fire automatically via R's event loop (input handler
on POSIX, message pump on Windows), so explicit draining is only needed
in test harnesses or tight batch loops that never yield to R's event
loop.

## Usage

``` r
tcc_callback_async_drain()
```

## Value

NULL (invisible)

## Details

TCC-compiled C code running on the main thread can call
`RC_callback_async_drain_c()` directly instead of returning to R.
