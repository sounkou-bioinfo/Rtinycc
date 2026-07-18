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

  Informational compatibility flag. Thread dispatch is selected by
  `callback_async:<signature>` in
  [`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md),
  not by this flag.

## Value

A tcc_callback object (externalptr wrapper)

## Details

Thread safety: ordinary `callback:<signature>` trampolines must be
invoked on the R main thread. Use `callback_async:<signature>` when
native workers need to dispatch to R. The `threadsafe` flag is
informational only.

If a callback raises an error, a warning is emitted and a
type-appropriate default value is returned.

When binding callbacks with
[`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md),
use a `callback:<signature>` argument type so a synchronous trampoline
is generated. The trampoline expects a `void*` user-data pointer as its
first argument; pass `tcc_callback_ptr(cb)` as the user-data argument to
the C API. For worker-thread dispatch, use `callback_async:<signature>`,
which schedules the R call on the main thread. Void callbacks are
fire-and-forget; non-void callbacks block the worker until the main
thread returns the real result. Type-appropriate defaults are used only
when dispatch or conversion fails.

Pointer arguments (e.g., `double*`, `int*`) are passed as external
pointers. Lengths must be supplied separately if needed.

A callback `ptr` or `cstring` return is borrowed. C may consume it
during the enclosing compiled call, but must not retain it after that
call unless the owner is kept alive by some other explicit contract.
Copy string data into C-owned storage when it must escape the callback
call chain.

The return type may be any scalar type supported by the FFI mappings
(e.g., `i32`, `f64`, `bool`, `cstring`), or `SEXP` to return an R object
directly.

Callback lifetime: callbacks are eventually released by finalizers. Call
[`tcc_callback_close()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_close.md)
when you want deterministic invalidation and earlier release of the
preserved R function.
