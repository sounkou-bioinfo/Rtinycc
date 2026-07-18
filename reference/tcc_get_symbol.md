# Get a symbol pointer from a libtcc state

The memory state must first be successfully finalized with
[`tcc_relocate()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_relocate.md).

## Usage

``` r
tcc_get_symbol(state, name)
```

## Arguments

- state:

  A `tcc_state`.

- name:

  Symbol name to look up.

## Value

External pointer of class `tcc_symbol`. The pointer retains `state` so
its relocated code remains alive for as long as the symbol is reachable.
