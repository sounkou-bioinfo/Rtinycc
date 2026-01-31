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

  - args: List of FFI types for arguments (e.g., list("i32", "f64"))

  - returns: FFI type for return value (e.g., "f64", "cstring")

  - code: Optional C code for the symbol (for embedded functions)

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
