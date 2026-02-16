# Add library to link against

Add library to link against

## Usage

``` r
tcc_library(ffi, library)
```

## Arguments

- ffi:

  A tcc_ffi object

- library:

  Library name (e.g., "m", "sqlite3") or a path to a shared library
  (e.g., "libm.so.6"). When a path is provided, the library directory is
  added automatically and the library name is inferred from the file
  name.

## Value

Updated tcc_ffi object (for chaining)
