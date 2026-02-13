# Drain the async callback queue

This is mainly useful for tests or when you need deterministic flushing
in non-interactive code paths. Normally callbacks are drained
automatically by the event loop dispatcher.

## Usage

``` r
tcc_callback_async_drain()
```

## Value

NULL (invisible)
