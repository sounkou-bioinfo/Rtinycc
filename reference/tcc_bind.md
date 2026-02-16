# Bind symbols with type specifications

Define symbols with Bun-style type specifications for API mode. This is
the core of the declarative FFI API.

## Usage

``` r
tcc_bind(ffi, ...)
```

## Arguments

- ffi:

  A tcc_ffi object

- ...:

  Named list of symbol definitions. Each definition is a list with:

  - args: List of fixed FFI argument types (e.g., list("i32", "f64"))

  - returns: FFI type for return value (e.g., "f64", "cstring")

  - variadic: Set TRUE for C varargs functions

  - varargs: Legacy typed variadic tail (exact/prefix mode)

  - varargs_types: Allowed scalar FFI types for true variadic tails

  - varargs_min: Minimum number of trailing varargs

  - varargs_max: Maximum number of trailing varargs (required for true
    variadic mode, defaults to `varargs_min`)

  - code: Optional C code for the symbol (for embedded functions)

  Callback arguments should use the form `callback:<signature>` (e.g.,
  `callback:double(double)`). The generated trampoline expects a
  `tcc_callback_ptr(cb)` to the corresponding user-data parameter in the
  C API. For thread-safe scheduling, use `callback_async:<signature>`
  which enqueues the call on the main thread and returns a default value
  immediately.

## Value

Updated tcc_ffi object (for chaining)

## Examples

``` r
ffi <- tcc_ffi() |>
  tcc_bind(
    add = list(args = list("i32", "i32"), returns = "i32"),
    greet = list(args = list("cstring"), returns = "cstring")
  )
```
