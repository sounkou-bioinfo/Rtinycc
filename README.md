
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtinycc

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

We provide a simple R interface to the
[tinycc](https://github.com/TinyCC/tinycc) compiler including the cli
and the libtcc library. This is mainly a vehicule for the tinycc
compiler and libtcc library. Right now only basic functionalities are
implemented and we do not support windows.

## Installation

``` r
remotes::install_github("sounkou-bioinfo/Rtinycc")
```

## Example

### CLI

``` r
library(Rtinycc)
tcc_dir <- tcc_prefix()
# CLI compile to executable
src <- system.file("c_examples", "forty_two.c", package = "Rtinycc")
exe <- tempfile(fileext = if (.Platform$OS.type == "windows") ".exe" else "")
inc_args <- paste0("-I", tcc_include_paths())
lib_args <- paste0("-L", tcc_lib_paths())
status <- tcc_run_cli(c("-B", tcc_dir, inc_args, lib_args, src, "-o", exe))
status
#> [1] 0
Sys.chmod(exe, mode = "0755")
system2(exe, stdout = TRUE)
#> [1] "42"
```

### In memory using libtcc

``` r
# libtcc in-memory compile
state <- tcc_state(output = "memory")
code <- "int forty_two(){ return 42; }"
tcc_compile_string(state, code)
#> [1] 0
tcc_relocate(state)
#> [1] 0
tcc_call_symbol(state, "forty_two", return = "int")
#> [1] 42
tcc_get_symbol(state, "forty_two")
#> <pointer: 0x64cb0cfab000>
#> attr(,"class")
#> [1] "tcc_symbol"
```

### Calling R’s C API functions from compiled C code

Using `Define _Complex` as workaround of `TinyCC`’s lack of support for
complex types, we can link against R’s install headers and `libR` to
call R’s C API function.

``` r
# Create new state for R linking example
state <- tcc_state(output = "memory")

# Add R include and library paths using Rtinycc functions
r_include <- R.home("include")
r_lib <- file.path(R.home("lib"))

tcc_add_include_path(state, r_include)
#> [1] 0
tcc_add_library_path(state, r_lib)
#> [1] 0

# Link against external math library (libm) for real math functions
tcc_add_library(state, "m")
#> [1] 0

# C code that demonstrates actual R C API usage
# Workaround: Define _Complex to empty since TinyCC doesn't support complex types
code <- '
#define _Complex
#include <R.h>
#include <Rinternals.h>

void hello_world() {
  Rprintf("Hello World from compiled C code!\\n");
}

// Create an R numeric vector and calculate its length
int create_r_vector() {
  SEXP vec = PROTECT(Rf_allocVector(REALSXP, 5));
  for(int i = 0; i < 5; i++) {
    REAL(vec)[i] = (double)(i + 1) * 2.0;  // 2, 4, 6, 8, 10
  }
  
  int length = Rf_length(vec);
  Rprintf("Created vector length: %d\\n", length);
  Rprintf("Vector values: ");
  for(int i = 0; i < length; i++) {
    Rprintf("%f ", REAL(vec)[i]);
  }
  Rprintf("\\n");
  
  UNPROTECT(1);
  return length;
}

// Use Rf_install to get a function and call it
double call_r_function() {
  // Get the sqrt function from R base
  SEXP sqrt_fun = PROTECT(Rf_findFun(Rf_install("sqrt"), R_BaseEnv));
  
  // Create a scalar value
  SEXP val = PROTECT(Rf_ScalarReal(16.0));
  
  // Call the function
  SEXP result = PROTECT(Rf_lang2(sqrt_fun, val));
  SEXP eval_result = PROTECT(Rf_eval(result, R_GlobalEnv));
  
  double sqrt_val = REAL(eval_result)[0];
  Rprintf("R sqrt(16.0) = %f\\n", sqrt_val);
  
  UNPROTECT(4);
  return sqrt_val;
}

// Demonstrate creating different R data types
void demonstrate_r_types() {
  // Create different types of R objects
  SEXP int_vec = PROTECT(Rf_allocVector(INTSXP, 3));
  INTEGER(int_vec)[0] = 1;
  INTEGER(int_vec)[1] = 2;
  INTEGER(int_vec)[2] = 3;
  Rprintf("Integer vector created with %d elements\\n", Rf_length(int_vec));
  
  SEXP str_vec = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(str_vec, 0, Rf_mkChar("Hello"));
  SET_STRING_ELT(str_vec, 1, Rf_mkChar("R"));
  Rprintf("String vector created with %d elements\\n", Rf_length(str_vec));
  
  SEXP logical = PROTECT(Rf_ScalarLogical(TRUE));
  Rprintf("Logical value: %d\\n", LOGICAL(logical)[0]);
  
  UNPROTECT(3);
}
'

tcc_compile_string(state, code)
#> [1] 0
tcc_relocate(state)
#> [1] 0


# Call the functions that demonstrate R C API usage
tcc_call_symbol(state, "hello_world", return = "void")
#> Hello World from compiled C code!
#> NULL

result1 <- tcc_call_symbol(state, "create_r_vector", return = "int")
#> Created vector length: 5
#> Vector values: 2.000000 4.000000 6.000000 8.000000 10.000000
result1
#> [1] 5

result2 <- tcc_call_symbol(state, "call_r_function", return = "double") 
#> R sqrt(16.0) = 4.000000
result2
#> [1] 4

result3 <- tcc_call_symbol(state, "demonstrate_r_types", return = "void")
#> Integer vector created with 3 elements
#> String vector created with 2 elements
#> Logical value: 1
```

### Modern FFI API (Bun-style)

The new FFI API provides a clean, declarative interface inspired by
Bun’s FFI. Define types explicitly and let TinyCC generate the binding
code automatically.

#### Type System

The FFI type system maps R types to C types:

- **Scalars**: `i8`, `i16`, `i32`, `i64` (integers), `f32`, `f64`
  (floats), `bool`, `cstring`
- **Arrays** (zero-copy): `raw` → `uint8_t*`, `integer_array` →
  `int32_t*`, `numeric_array` → `double*`
- **Pointers**: `ptr` (externalptr), `sexp` (R object)

#### Example: Simple Function

``` r
library(Rtinycc)

# Define and compile in one chain
ffi <- tcc_ffi() |>
  tcc_bind(
    add = list(args = list("i32", "i32"), returns = "i32")
  ) |>
  tcc_source("
    int add(int a, int b) {
      return a + b;
    }
  ") |>
  tcc_compile()

# Call directly with type conversion
result <- ffi$add(5L, 3L)
result
#> [1] 8
```

#### Example: Working with R Arrays

Pass R vectors to C with zero-copy:

``` r
ffi <- tcc_ffi() |>
  tcc_bind(
    sum_array = list(args = list("integer_array", "i32"), returns = "i64")
  ) |>
  tcc_source("
    int64_t sum_array(int32_t* arr, int32_t n) {
      int64_t sum = 0;
      for(int i = 0; i < n; i++) {
        sum += arr[i];
      }
      return sum;
    }
  ") |>
  tcc_compile()

# Pass R integer vector directly
x <- 1:100
result <- ffi$sum_array(x, length(x))
result
#> [1] 5050
```

#### Linking External Libraries

Link against system libraries like libm (math) or libsqlite3:

``` r
# Link against math library
math_lib <- tcc_link(
  "libm.so",
  symbols = list(
    sqrt = list(args = list("f64"), returns = "f64"),
    sin = list(args = list("f64"), returns = "f64")
  ),
  libs = "m"
)

# Use directly
math_lib$sqrt(16.0)
```

**Note**: The `_Complex` workaround is automatically applied when using
R headers.

## License

GPL-3

# References

- [tinycc](https://github.com/TinyCC/tinycc)
