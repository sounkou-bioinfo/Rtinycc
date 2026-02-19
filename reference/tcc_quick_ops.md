# Supported operations table for tcc_quick

Returns a data.frame listing every R construct that
[`tcc_quick()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_quick.md)
can lower to C. The table is assembled programmatically from the call
registry and from the lowerer/codegen, so it stays in sync with the
implementation. The `vectorized` column indicates whether the operation
is fused element-wise into vector loops via condensation (no
intermediate allocation).

## Usage

``` r
tcc_quick_ops()
```

## Value

A data.frame with columns `category`, `r`, `c`, `vectorized`.
