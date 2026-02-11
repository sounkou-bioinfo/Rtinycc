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

## Details

Thread safety: callbacks are executed on the R main thread only.
Invoking a callback from a worker thread is unsupported and may crash R.
The `threadsafe` flag is currently informational only.

If a callback raises an error, a warning is emitted and a
type-appropriate default value is returned.

When binding callbacks with
[`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md),
use a `callback:<signature>` argument type so a synchronous trampoline
is generated. The trampoline expects a `void*` user-data pointer as its
first argument; pass `tcc_callback_ptr(cb)` as the user-data argument to
the C API. For thread-safe usage from worker threads, use
`callback_async:<signature>` which schedules the call on the main thread
and returns a default value.

Pointer arguments (e.g., `double*`, `int*`) are passed as external
pointers. Lengths must be supplied separately if needed.

The return type may be any scalar type supported by the FFI mappings
(e.g., `i32`, `f64`, `bool`, `cstring`), or `SEXP` to return an R object
directly.
