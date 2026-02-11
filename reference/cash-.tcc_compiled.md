# Access a compiled FFI symbol

Overrides `$` to detect dead pointers after deserialization and
recompile transparently from the stored recipe.

## Usage

``` r
# S3 method for class 'tcc_compiled'
x$name
```

## Arguments

- x:

  A tcc_compiled object

- name:

  Symbol name to access

## Value

The callable function or metadata field
