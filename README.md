
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtinycc

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

We provide a simple R interface to the
[tinycc](https://github.com/TinyCC/tinycc) compiler including the cli
and the libtcc library. This is mainly a vehicule for the tinycc
compiler and libtcc library. Right now only basic functionalities are
implemented and some warnings remain since libtcc uses printf and other
symbols R CMD check does not like.

## Installation

``` r
remotes::install_github("sounkou-bioinfo/Rtinycc")
```

## Example

### CLI

``` r
library(Rtinycc)
tcc_dir <- tcc_prefix()
# CLI compile to object
src <- system.file("c_examples", "forty_two.c", package = "Rtinycc")
out <- tempfile(fileext = ".o")
inc_args <- as.character(paste0("-I", tcc_include_paths()))
status <- tcc_run_cli(c(inc_args, "-c", src, "-o", out))
status
#> [1] 0
system2(out, stdout = TRUE)
#> Warning in system2(out, stdout = TRUE): running command
#> ''/tmp/RtmpabtLJC/file1169f264373f3.o'' had status 126
#> character(0)
#> attr(,"status")
#> [1] 126
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
#> <pointer: 0x62153d600000>
#> attr(,"class")
#> [1] "tcc_symbol"
```

## License

GPL-3

# References

  - [tinycc](https://github.com/TinyCC/tinycc)
