# Extract macro defines from a header file

Extract macro defines from a header file

## Usage

``` r
tcc_treesitter_defines(
  file,
  use_cpp = TRUE,
  cc = treesitter.c::r_cc(),
  ccflags = NULL
)
```

## Arguments

- file:

  Path to a header file.

- use_cpp:

  Logical; use the C preprocessor if available.

- cc:

  Compiler string; passed to
  [`system2()`](https://rdrr.io/r/base/system2.html) if
  `use_cpp = TRUE`.

- ccflags:

  Additional flags for the compiler.

## Value

Character vector of macro names defined in `file`.

## Examples

``` r
if (FALSE) { # \dontrun{
tcc_treesitter_defines("/usr/include/math.h")
} # }
```
