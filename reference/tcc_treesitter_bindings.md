# Generate bindings from a header

Generate bindings from a header

## Usage

``` r
tcc_treesitter_bindings(
  header,
  mapper = tcc_map_c_type_to_ffi,
  ffi = NULL,
  functions = TRUE,
  structs = FALSE,
  unions = FALSE,
  enums = FALSE,
  globals = FALSE,
  bitfield_type = "u8",
  include_bitfields = TRUE
)
```

## Arguments

- header:

  Character scalar containing C declarations.

- mapper:

  Function to map C types to FFI types.

- ffi:

  Optional `tcc_ffi` object. When provided, returns an updated FFI
  object with generated bindings.

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

Named list suitable for
[`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md)
when `ffi` is NULL, otherwise an updated `tcc_ffi` object.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "double sqrt(double x);"
symbols <- tcc_treesitter_bindings(header)
} # }
```
