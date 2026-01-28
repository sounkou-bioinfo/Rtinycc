
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
#> <pointer: 0x55a4c246e000>
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
tcc_add_sysinclude_path(state, r_include)
#> [1] 0
tcc_add_library_path(state, r_lib)
#> [1] 0

# Link against R library
tcc_add_library(state, "R")
#> [1] 0

# C code that includes R headers and calls R functions
# Workaround: Define _Complex to empty since TinyCC doesn't support complex types
code <- '
#define _Complex
#include <R.h>
#include <Rmath.h>

void hello_world() {
  Rprintf("Hello World from compiled C code!\\n");
}

double get_random_normal() {
  double val = rnorm(0.0, 1.0);
  Rprintf("Generated random normal: %f\\n", val);
  return val;
}

double get_pi() {
  Rprintf("Returning PI value\\n");
  return 3.14159265359;
}

double get_euler() {
  Rprintf("Returning Euler\\\"s number\\n");
  return 2.71828182846;
}

int fibonacci_10() {
  Rprintf("Calculating 10th Fibonacci number\\n");
  int a = 0, b = 1, temp;
  for (int i = 0; i < 10; i++) {
    temp = a + b;
    a = b;
    b = temp;
  }
  return a;
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

result1 <- tcc_call_symbol(state, "get_random_normal", return = "double")
#> Generated random normal: -8.773321
result1
#> [1] -8.773321

result2 <- tcc_call_symbol(state, "get_pi", return = "double") 
#> Returning PI value
result2
#> [1] 3.141593

result3 <- tcc_call_symbol(state, "get_euler", return = "double")
#> Returning Euler"s number
result3
#> [1] 2.718282

result4 <- tcc_call_symbol(state, "fibonacci_10", return = "int")
#> Calculating 10th Fibonacci number
result4
#> [1] 55

# Get symbol pointers for external use
hello_ptr <- tcc_get_symbol(state, "hello_world")
normal_ptr <- tcc_get_symbol(state, "get_random_normal")
pi_ptr <- tcc_get_symbol(state, "get_pi")
euler_ptr <- tcc_get_symbol(state, "get_euler")
fib_ptr <- tcc_get_symbol(state, "fibonacci_10")
```

## License

GPL-3

# References

- [tinycc](https://github.com/TinyCC/tinycc)
