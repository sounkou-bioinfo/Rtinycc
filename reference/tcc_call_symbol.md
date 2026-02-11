# Call a zero-argument symbol with a specified return type

Call a zero-argument symbol with a specified return type

## Usage

``` r
tcc_call_symbol(state, name, return = c("int", "double", "void"))
```

## Arguments

- state:

  A `tcc_state`.

- name:

  Symbol name to call.

- return:

  One of "int", "double", "void".

## Value

The return value cast to the requested type (NULL for void).
