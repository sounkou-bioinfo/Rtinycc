# Header Parsing with treesitter.c

`Rtinycc` can build FFI bindings from C declarations instead of
requiring every signature to be written manually. The parsing layer is
provided by the optional `treesitter.c` package, while `Rtinycc` is
responsible for mapping parsed C types into its own FFI type system.

This workflow is useful when:

- you already have a C header snippet
- you want a first-pass binding specification quickly
- you want to inspect declarations before deciding what to expose

## Availability

These helpers require the optional `treesitter.c` package.

``` r

has_treesitter
#> [1] TRUE
```

If `FALSE`, the parsing functions are unavailable and the executable
examples in this vignette are skipped.

## Start from a Header Snippet

``` r

header <- paste(
  "double sqrt(double x);",
  "int add(int a, int b);",
  "struct point { double x; double y; };",
  "enum status { OK = 0, ERR = 1 };",
  sep = "\n"
)
```

## Inspect Parsed Functions

The lowest-level helpers return parsed declarations from the header:

``` r

tcc_treesitter_functions(header)
#>   capture_name text start_line start_col   params return_type
#> 1    decl_name sqrt          1         8   double      double
#> 2    decl_name  add          2         5 int, int         int
```

For quick inspection of functions only,
[`tcc_treesitter_bindings()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_bindings.md)
converts the parsed signatures into
[`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md)-ready
specs:

``` r

tcc_treesitter_bindings(header)
#> $sqrt
#> $sqrt$args
#> $sqrt$args[[1]]
#> [1] "f64"
#> 
#> 
#> $sqrt$returns
#> [1] "f64"
#> 
#> 
#> $add
#> $add$args
#> $add$args[[1]]
#> [1] "i32"
#> 
#> $add$args[[2]]
#> [1] "i32"
#> 
#> 
#> $add$returns
#> [1] "i32"
```

## Generate a Working FFI Object

For a fuller workflow, use
[`tcc_generate_bindings()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_generate_bindings.md)
on a `tcc_ffi` object that already has matching C source attached:

``` r

ffi <- tcc_ffi() |>
  tcc_source(
    "
    double sqrt(double x) { return x < 0 ? 0 : x; }
    int add(int a, int b) { return a + b; }
    struct point { double x; double y; };
    enum status { OK = 0, ERR = 1 };
    "
  )

ffi <- tcc_generate_bindings(
  ffi,
  header,
  functions = TRUE,
  structs = TRUE,
  enums = TRUE,
  unions = FALSE,
  globals = FALSE
)

compiled <- tcc_compile(ffi)

compiled$add(2L, 3L)
#> [1] 5
compiled$enum_status_OK()
#> [1] 0
```

This keeps the parsing step separate from the actual compilation step,
which is important when you want to inspect or edit the generated
binding plan first.

## Struct Helpers

You can also extract just the generated struct accessors:

``` r

tcc_treesitter_struct_accessors("struct point { double x; double y; };")
#> $point
#> $point$x
#> [1] "f64"
#> 
#> $point$y
#> [1] "f64"
```

That output is what `Rtinycc` feeds into
[`tcc_struct()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_struct.md)
when you ask it to generate struct bindings from a header.

Bitfields now stay explicit in the accessor metadata rather than
collapsing to a bare scalar type:

``` r

tcc_treesitter_struct_accessors(
  "struct flags { unsigned int flag : 1; unsigned int code : 6; };"
)
#> $flags
#> $flags$flag
#> $flags$flag$type
#> [1] "u8"
#> 
#> $flags$flag$bitfield
#> [1] TRUE
#> 
#> $flags$flag$width
#> [1] 1
#> 
#> 
#> $flags$code
#> $flags$code$type
#> [1] "u8"
#> 
#> $flags$code$bitfield
#> [1] TRUE
#> 
#> $flags$code$width
#> [1] 6
```

Nested struct fields in structs currently still fall back to ptr-like
accessors:

``` r

tcc_treesitter_struct_accessors(
  "struct child { int x; }; struct outer { struct child child; int y; };"
)
#> $child
#> $child$x
#> [1] "i32"
#> 
#> 
#> $outer
#> $outer$child
#> [1] "ptr"
#> 
#> $outer$y
#> [1] "i32"
```

For unions, nested struct members preserve `list(type = "struct", ...)`
so the generated helper remains a borrowed nested view rather than an
opaque raw pointer.

## Conservative Type Mapping

The default mapper is intentionally conservative. In particular, pointer
types are not automatically treated as C strings unless that is
semantically safe.

``` r

tcc_map_c_type_to_ffi("int")
#> [1] "i32"
tcc_map_c_type_to_ffi("double")
#> [1] "f64"
tcc_map_c_type_to_ffi("const char *")
#> [1] "ptr"
```

If you know a specific API uses `const char *` as a real NUL-terminated
string, you can override the mapping:

``` r

string_mapper <- function(type) {
  if (trimws(type) == "const char *") {
    return("cstring")
  }
  tcc_map_c_type_to_ffi(type)
}

tcc_treesitter_bindings(
  "int puts(const char *s);",
  mapper = string_mapper
)
#> $puts
#> $puts$args
#> $puts$args[[1]]
#> [1] "cstring"
#> 
#> 
#> $puts$returns
#> [1] "i32"
```

This is the intended extension point: keep the default mapper strict,
then relax specific cases where you know the source API contract.
