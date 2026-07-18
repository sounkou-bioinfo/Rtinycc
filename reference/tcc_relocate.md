# Relocate compiled code

Relocation is valid only for a state created with `output = "memory"`
and can be performed only once. Callable symbols are unavailable until
relocation succeeds. Use
[`tcc_output_file()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_output_file.md)
for object, shared-library, executable, or preprocessor output.

## Usage

``` r
tcc_relocate(state)
```

## Arguments

- state:

  A `tcc_state`.

## Value

Integer status code (0 = success).
