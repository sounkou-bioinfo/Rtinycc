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
the target C function must be `void`, each atomic or character R
argument is copied to guarded mutable call storage and passed by
pointer, and the result is a list of the modified argument values.
Supported argument mappings follow R's
[`.C()`](https://rdrr.io/r/base/Foreign.html) interface: raw as
`unsigned char *`, integer/logical as `int *`, numeric as `double *` or
`float *` when `attr(x, "Csingle")` is true, complex as `Rcomplex *`,
character as `char **`, lists as read-only `SEXP *`, and
functions/environments/other R objects as read-only `SEXP`. Up to 65
arguments are supported. Guard bytes around copied buffers are checked
after the call to catch simple native underwrites and overwrites;
character code may edit string contents in place but must not replace
`char *` elements in the `char **` array.

Non-atomic R objects are borrowed for the duration of the call only. C
code must not mutate them through this interface, and must call
`R_PreserveObject()` if it deliberately stores a `SEXP` beyond the call.
This is a low-level convenience interface; for typed scalar returns,
explicit zero-copy arrays, ownership metadata, and clearer signatures,
prefer
[`tcc_ffi()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ffi.md).
