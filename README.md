
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
#> <pointer: 0x5de73701a000>
#> attr(,"class")
#> [1] "tcc_symbol"
```

### Calling R functions from compiled C code

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

# C code that includes math.h and uses external library functions
# Workaround: Define _Complex to empty since TinyCC doesn't support complex types
code <- '
#define _Complex
#include <R.h>
#include <math.h>

void hello_world() {
  Rprintf("Hello World from compiled C code!\\n");
}

double calculate_sqrt_2() {
  double val = sqrt(2.0);
  Rprintf("sqrt(2) = %f\\n", val);
  return val;
}

double calculate_sin_pi() {
  double val = sin(3.14159265359);
  Rprintf("sin(PI) = %f\\n", val);
  return val;
}

double calculate_log_10() {
  double val = log(10.0);
  Rprintf("log(10) = %f\\n", val);
  return val;
}
'

tcc_compile_string(state, code)
#> [1] 0
tcc_relocate(state)
#> [1] 0


# Call the functions (all zero-argument functions)
tcc_call_symbol(state, "hello_world", return = "void")
#> Hello World from compiled C code!
#> NULL

result1 <- tcc_call_symbol(state, "calculate_sqrt_2", return = "double")
#> sqrt(2) = 1.414214
result1
#> [1] 1.414214

result2 <- tcc_call_symbol(state, "calculate_sin_pi", return = "double") 
#> sin(PI) = -0.000000
result2
#> [1] -2.068231e-13

result3 <- tcc_call_symbol(state, "calculate_log_10", return = "double")
#> log(10) = 2.302585
result3
#> [1] 2.302585
```

## License

GPL-3

# References

- [tinycc](https://github.com/TinyCC/tinycc)
