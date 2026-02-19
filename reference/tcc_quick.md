# Compile a small declare()-annotated R subset with TinyCC

`tcc_quick()` is an experimental C-first path for compiling a strict
subset of R functions annotated with `declare(type(...))`. In the
current subset, supported bodies are recursive scalar expressions
(arithmetic, comparisons, logical operators, unary operators, selected
scalar math functions, and scalar
`if (cond) a else b`/`ifelse(cond, a, b)`) over declared scalar
`double`/`integer`/`logical` arguments. Simple statement blocks with
scalar `<-` assignments before the final expression are supported.
Rank-3+ declarations (for example `double(NA, NA, NA)`) are accepted at
parse time but currently reserved for upcoming multidimensional array
support; they fallback in `soft`/`auto` and error in `hard` mode.

## Usage

``` r
tcc_quick(
  fn,
  fallback = c("auto", "soft", "hard", "always", "never"),
  mode = c("compile", "code"),
  debug = FALSE
)
```

## Arguments

- fn:

  Function to compile.

- fallback:

  One of `"auto"`, `"soft"`, `"hard"`. Legacy aliases `"always"` and
  `"never"` map to `"soft"` and `"hard"`.

- mode:

  One of `"compile"` (default) or `"code"`. Use `"code"` to return
  generated C source without compiling.

- debug:

  Print generated C source and lowering diagnostics.

## Value

A function with the same formals as `fn`, or `fn` itself when fallback
is used. When `mode = "code"`, returns a character string containing
generated C source.

## Details

For non-arithmetic symbol calls (e.g. `max(x, y)` or
`pmax(a, b, c, d, e)`), `tcc_quick()` emits a wrapper that constructs
the call pairlist and evaluates it in base via `Rf_eval()`, so R's
primitive/internal implementation is used.

Lowering descends recursively through expressions and stops at boundary
calls (`.Call`, `.C`, `.External`, `.Internal`, `.Primitive`), where it
currently falls back according to `fallback`.

Unsupported functions can either fallback to the original R function or
error, depending on `fallback`.
