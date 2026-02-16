# Run the tinycc CLI

Run the tinycc CLI

## Usage

``` r
tcc_run_cli(args = character(), tcc_path = check_cli_exists())
```

## Arguments

- args:

  Character vector of CLI arguments (e.g., `c("-c", file, "-o", out)`).

- tcc_path:

  Optional path to the `tcc` binary; defaults to the bundled CLI.

## Value

Integer status from [`system2()`](https://rdrr.io/r/base/system2.html).
