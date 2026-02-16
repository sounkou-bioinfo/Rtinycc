# Recompile a tcc_compiled object

Explicitly recompile from the stored FFI recipe. Useful after
deserialization (`readRDS`, `unserialize`) or to force a fresh
compilation.

## Usage

``` r
tcc_recompile(compiled)
```

## Arguments

- compiled:

  A tcc_compiled object

## Value

The recompiled tcc_compiled object (invisibly, same environment)
