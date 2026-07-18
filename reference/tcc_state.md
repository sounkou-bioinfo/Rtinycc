# Create a libtcc state

Initialize a libtcc compilation state, optionally pointing at the
bundled include/lib paths. Memory states are finalized with
[`tcc_relocate()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_relocate.md);
other output modes are written with
[`tcc_output_file()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_output_file.md).
Finalization is single-shot, after which compilation options and source
cannot be changed.

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
