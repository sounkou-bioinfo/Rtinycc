# Register an R function as a callback

Wraps an R function so it can be passed as a C function pointer to
compiled code. The callback will be invoked via a trampoline that
marshals arguments between C and R.

## Usage

``` r
tcc_callback(fun, signature, threadsafe = FALSE)
```

## Arguments

- fun:

  An R function to be called from C

- signature:

  C function signature string (e.g., "double (\*)(int, double)")

- threadsafe:

  Whether to enable thread-safe invocation (experimental)

## Value

A tcc_callback object (externalptr wrapper)
