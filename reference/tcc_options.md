# Add TinyCC compiler options to FFI context

Append raw options that are passed to
[`tcc_set_options()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_set_options.md)
before compiling generated wrappers (for example `"-O2"` or `"-Wall"`).

## Usage

``` r
tcc_options(ffi, options)
```

## Arguments

- ffi:

  A tcc_ffi object

- options:

  Character vector of option fragments

## Value

Updated tcc_ffi object (for chaining)
