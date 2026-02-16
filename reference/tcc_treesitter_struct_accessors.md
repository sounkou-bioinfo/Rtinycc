# Generate tcc_struct() accessors from header structs

Generate tcc_struct() accessors from header structs

## Usage

``` r
tcc_treesitter_struct_accessors(
  header,
  mapper = tcc_map_c_type_to_ffi,
  bitfield_type = "u8",
  include_bitfields = TRUE
)
```

## Arguments

- header:

  Character scalar containing C declarations.

- mapper:

  Function to map C types to FFI types.

- bitfield_type:

  FFI type to use for bitfields.

- include_bitfields:

  Whether to include bitfields.

## Value

Named list of accessors by struct name.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "struct point { double x; double y; };"
tcc_treesitter_struct_accessors(header)
} # }
```
