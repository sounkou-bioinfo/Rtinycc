# CString S3 Class

Safe handling of C strings (char\*) with automatic memory management.
Like Bun's CString class.

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
