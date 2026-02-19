# Apply raw TinyCC options to a libtcc state

Passes options directly to `tcc_set_options()` for the given state.

## Usage

``` r
tcc_set_options(state, options)
```

## Arguments

- state:

  A `tcc_state`.

- options:

  Character scalar of options (for example `"-O2 -Wall"`).

## Value

Integer status code (`0` on success; negative on parse error).
