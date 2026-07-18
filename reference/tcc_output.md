# Set output type for FFI compilation

The high-level FFI creates callable in-memory wrappers, so only
`"memory"` output is supported. Executable and shared-library artifact
output requires the low-level
[`tcc_state()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_state.md)
and
[`tcc_output_file()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_output_file.md)
path and is rejected here rather than returning dangling callable
addresses.

## Usage

``` r
tcc_output(ffi, output = "memory")
```

## Arguments

- ffi:

  A tcc_ffi object

- output:

  Must be `"memory"`.

## Value

Updated tcc_ffi object (for chaining)
