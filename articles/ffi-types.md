# FFI Types

This article describes the FFI type names implemented in `Rtinycc`. The
mapping below is taken from the package type table and
wrapper-generation code, not from a separate design document.

## Scalar Types

The core scalar types are:

- signed integers: `i8`, `i16`, `i32`, `i64`
- unsigned integers: `u8`, `u16`, `u32`, `u64`
- floating point: `f32`, `f64`
- logical: `bool`
- string: `cstring`
- opaque pointer: `ptr`
- direct R object: `sexp`
- return-only sentinel: `void`

`Rtinycc` converts R values to these C types inside generated wrappers.
The scalar integer and logical types are validated, not just cast
silently.

## A Minimal Example

``` r
ffi <- tcc_ffi() |>
  tcc_source(
    "
    int add_i32(int a, int b) { return a + b; }
    double mul_f64(double x, double y) { return x * y; }
    int negate_bool(_Bool x) { return !x; }
    "
  ) |>
  tcc_bind(
    add_i32 = list(args = list("i32", "i32"), returns = "i32"),
    mul_f64 = list(args = list("f64", "f64"), returns = "f64"),
    negate_bool = list(args = list("bool"), returns = "bool")
  ) |>
  tcc_compile()

ffi$add_i32(2L, 3L)
#> [1] 5
ffi$mul_f64(2, 4)
#> [1] 8
ffi$negate_bool(TRUE)
#> [1] FALSE
```

## String and Pointer Types

`cstring` and `ptr` are intentionally different:

- `cstring` converts an R character scalar into a C `char *` view for
  the call
- `ptr` passes an external pointer address through unchanged

``` r
ffi_str <- tcc_ffi() |>
  tcc_source(
    "
    const char* echo_cstring(const char* s) { return s; }
    void* echo_ptr(void* p) { return p; }
    "
  ) |>
  tcc_bind(
    echo_cstring = list(args = list("cstring"), returns = "cstring"),
    echo_ptr = list(args = list("ptr"), returns = "ptr")
  ) |>
  tcc_compile()

ptr <- tcc_malloc(8)

ffi_str$echo_cstring("hello")
#> [1] "hello"
inherits(ffi_str$echo_ptr(ptr), "externalptr")
#> [1] TRUE

tcc_free(ptr)
#> NULL
```

The important semantic difference is discussed in the boundary-semantics
article: returned `cstring` values are copied into R strings, while
returned `ptr` values stay as raw addresses.

## Array Types

The implemented array input types are:

- `raw`
- `integer_array`
- `numeric_array`
- `logical_array`
- `character_array`
- `cstring_array`

The first four map directly onto R vectors. `character_array` passes the
underlying `CHARSXP` cells as a read-only `SEXP *`, not a `char **`. Use
`cstring_array` when the C side expects a temporary `const char **`.

``` r
ffi_arr <- tcc_ffi() |>
  tcc_source(
    "
    int first_int(int* x) { return x[0]; }
    double second_num(double* x) { return x[1]; }
    "
  ) |>
  tcc_bind(
    first_int = list(args = list("integer_array"), returns = "i32"),
    second_num = list(args = list("numeric_array"), returns = "f64")
  ) |>
  tcc_compile()

ffi_arr$first_int(as.integer(c(10, 20, 30)))
#> [1] 10
ffi_arr$second_num(c(1.5, 2.5))
#> [1] 2.5
```

## Direct R Objects with `sexp`

`sexp` passes the R object through the wrapper without conversion.

``` r
ffi_sexp <- tcc_ffi() |>
  tcc_source(
    "
    #include <Rinternals.h>

    SEXP id_sexp(SEXP x) { return x; }
    "
  ) |>
  tcc_bind(
    id_sexp = list(args = list("sexp"), returns = "sexp")
  ) |>
  tcc_compile()

ffi_sexp$id_sexp(list(a = 1, b = 2))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
```

This is the lowest-friction way to cross the boundary when you want to
work in terms of the R C API directly rather than the stricter
scalar/vector FFI types.

## Package Attachment Check

``` r
library(Rtinycc)
```
