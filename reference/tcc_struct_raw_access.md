# Enable raw byte access for struct

Generates helper functions to read/write raw bytes from struct memory.
Useful for bitwise operations, debugging, or manual serialization.

## Usage

``` r
tcc_struct_raw_access(ffi, struct_name)
```

## Arguments

- ffi:

  A tcc_ffi object

- struct_name:

  Struct name

## Value

Updated tcc_ffi object
