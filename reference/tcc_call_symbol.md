# Call a symbol from a TinyCC state

With no additional arguments, `tcc_call_symbol()` preserves its
historical quick-test behavior: call a zero-argument symbol and box an
`int`, `double`, or `void` return value.

## Usage

``` r
tcc_call_symbol(
  .state,
  .NAME,
  ...,
  return = c("int", "double", "void"),
  NAOK = FALSE
)
```

## Arguments

- .state:

  A `tcc_state`. Named `state =` is also accepted for compatibility with
  earlier releases.

- .NAME:

  Symbol name to call. Named `name =` is also accepted for compatibility
  with earlier releases.

- ...:

  Optional arguments for
  [`.C()`](https://rdrr.io/r/base/Foreign.html)-style pointer calls.

- return:

  One of `"int"`, `"double"`, or `"void"`. If `...` is present, the only
  supported value is `"void"`; when omitted with `...`, it defaults to
  `"void"`.

- NAOK:

  If `FALSE`, integer/logical `NA` and non-finite numeric/complex values
  are rejected before the call, matching
  [`.C()`](https://rdrr.io/r/base/Foreign.html)'s default safety check.
  If `TRUE`, those values are passed through.

## Value

For zero-argument scalar calls, the boxed return value (`NULL` for
`void`). For [`.C()`](https://rdrr.io/r/base/Foreign.html)-style calls,
a list mirroring `...` with any C-side modifications copied back.

## Details

With additional arguments, it uses an R
[`.C()`](https://rdrr.io/r/base/Foreign.html)-style calling convention:
the target C function must be `void`, each R argument is copied to a
mutable C buffer and passed by pointer, and the result is a list of the
modified argument values. Supported argument types mirror the common
[`.C()`](https://rdrr.io/r/base/Foreign.html) set: raw, integer,
logical, double, complex, character, lists as `SEXP *`, and other R
objects as `SEXP`. Up to 65 arguments are supported.

This is a low-level convenience interface. For typed scalar returns,
ownership metadata, and clearer signatures, prefer
[`tcc_ffi()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ffi.md).
