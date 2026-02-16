# Generate tcc_union() accessors from header unions

Generate tcc_union() accessors from header unions

## Usage

``` r
tcc_treesitter_union_accessors(
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

Named list of accessors by union name.

## Examples

``` r
if (FALSE) { # \dontrun{
header <- "union data { int i; double d; };"
tcc_treesitter_union_accessors(header)
} # }
```
