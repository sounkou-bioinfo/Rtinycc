# Create a libtcc state

Initialize a libtcc compilation state, optionally pointing at the
bundled include/lib paths.

## Usage

``` r
tcc_state(
  output = c("memory", "obj", "dll", "exe", "preprocess"),
  include_path = tcc_include_paths(),
  lib_path = tcc_lib_paths()
)
```

## Arguments

- output:

  Output type: one of "memory", "obj", "dll", "exe", "preprocess".

- include_path:

  Path(s) to headers; defaults to the bundled include dirs.

- lib_path:

  Path(s) to libraries; defaults to the bundled lib dirs (lib and
  lib/tcc).

## Value

An external pointer of class `tcc_state`.
