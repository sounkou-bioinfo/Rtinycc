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

TCC-compiled C code that is known to be running on the main R thread can
call `RC_callback_async_drain_c()` directly instead of returning to R.
Functions bound with `callback_async:*` arguments are normally executed
on a worker by the generated wrapper while the main thread drains, so
user code in that target function should not assume it is itself on the
main thread.
