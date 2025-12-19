# Create a libtcc state

Initialize a libtcc compilation state, optionally pointing at the
bundled include/lib paths.

## Usage

``` r
tcc_state(
  output = c("memory", "obj", "dll", "exe", "preprocess"),
  include_path = tcc_include_path(),
  lib_path = tcc_lib_path()
)
```

## Arguments

- output:

  Output type: one of "memory", "obj", "dll", "exe", "preprocess".

- include_path:

  Path to headers; defaults to the bundled include dir.

- lib_path:

  Path to libraries; defaults to the bundled lib dir.

## Value

An external pointer of class `tcc_state`.
