# CString S3 Class

Wrapper around a C string pointer with an optional cached R copy.
Ownership follows the underlying external pointer; this wrapper does not
add finalizer or freeing behavior on top of that pointer.

## Usage

``` r
tcc_cstring_object(ptr, clone = TRUE, owned = FALSE)
```

## Arguments

- ptr:

  External pointer to C string

- clone:

  Whether to clone the string immediately (safe for R use)

- owned:

  Currently unused. Reserved for future finalizer support.

## Value

A tcc_cstring object
