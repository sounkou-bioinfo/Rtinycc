# CString S3 Class

Safe handling of C strings (char\*) with automatic memory management.
Like Bun's CString class.

Convert R character strings to C-style null-terminated string pointers.
This handles UTF-8 encoding and null termination automatically.

## Usage

``` r
tcc_cstring(str)

tcc_cstring(str)
```

## Arguments

- str:

  Character string

- ptr:

  External pointer to C string

- clone:

  Whether to clone the string immediately (safe for R use)

- owned:

  Whether R should free the C memory when done

## Value

A tcc_cstring object

External pointer to C string
