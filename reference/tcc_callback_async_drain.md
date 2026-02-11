# Drain the async callback queue

This is mainly useful for tests; normally callbacks are executed by the
event loop once scheduled.

## Usage

``` r
tcc_callback_async_drain()
```

## Value

NULL (invisible)
