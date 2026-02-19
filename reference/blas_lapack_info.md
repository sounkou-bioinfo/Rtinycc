# Report active BLAS/LAPACK runtime information from R

Returns the BLAS/LAPACK runtime details as reported by R itself, plus
convenience flags indicating whether `Rblas` and `Rlapack` appear
available in loaded DLLs/shared objects.

## Usage

``` r
blas_lapack_info()
```

## Value

A named list with fields: `blas_path`, `lapack_path`, `has_rblas`,
`has_rlapack`, `loaded_dlls`.
