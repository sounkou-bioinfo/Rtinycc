# Generate bindings from header declarations

Generate bindings from header declarations

## Usage

``` r
tcc_generate_bindings(
  ffi = NULL,
  header,
  mapper = tcc_map_c_type_to_ffi,
  functions = TRUE,
  structs = TRUE,
  unions = TRUE,
  enums = TRUE,
  globals = TRUE,
  bitfield_type = "u8",
  include_bitfields = TRUE
)
```

## Arguments

- ffi:

  A tcc_ffi object. If NULL, a new one is created.

- header:

  Character scalar containing C declarations.

- mapper:

  Function to map C types to FFI types.

- functions:

  Logical; generate
  [`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md)
  specs for functions.

- structs:

  Logical; generate
  [`tcc_struct()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_struct.md)
  helpers.

- unions:

  Logical; generate
  [`tcc_union()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_union.md)
  helpers.

- enums:

  Logical; generate
  [`tcc_enum()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_enum.md)
  helpers.

- globals:

  Logical; generate
  [`tcc_global()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_global.md)
  getters/setters.

- bitfield_type:

  FFI type to use for bitfields.

- include_bitfields:

  Whether to include bitfields.

## Value

Updated tcc_ffi object.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "double sqrt(double x); struct point { double x; double y; };"
ffi <- tcc_generate_bindings(tcc_ffi(), header)
} # }
```
