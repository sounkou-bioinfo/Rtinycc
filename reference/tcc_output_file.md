# Write a non-memory TinyCC output file

Finalize a state created with `output = "obj"`, `"dll"`, `"exe"`, or
`"preprocess"` and write the resulting artifact. A state can be
finalized only once. Memory states must instead use
[`tcc_relocate()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_relocate.md).

## Usage

``` r
tcc_output_file(state, path)
```

## Arguments

- state:

  A non-memory `tcc_state`.

- path:

  Destination file path.

## Value

Integer status code (0 = success).
