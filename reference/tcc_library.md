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
  (e.g., "libm.so.6"). When a path or platform library file name is
  provided, the library directory is added automatically and TinyCC is
  asked to link that exact file name. This keeps versioned runtime
  libraries such as `libm.so.6` distinct from generic linker names such
  as `m`/`libm.so`.

## Value

Updated tcc_ffi object (for chaining)
