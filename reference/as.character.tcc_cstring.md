# Convert a `tcc_cstring` object to an R string

Convert a `tcc_cstring` object to an R string

## Usage

``` r
# S3 method for class 'tcc_cstring'
as.character(x, ...)
```

## Arguments

- x:

  A `tcc_cstring` object.

- ...:

  Ignored.

## Value

A character scalar containing the string value. Returns the cached R
copy when available; otherwise reads the current NUL-terminated C string
from `x$ptr`.
