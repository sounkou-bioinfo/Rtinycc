## Test environments

- local Ubuntu 24.04.3 LTS, R 4.5.2
- GitHub Actions ubuntu-latest (devel)
- GitHub Actions macos-latest (release)
- GitHub Actions windows-latest (release)
- additional GitHub Actions Windows tinytest workflow for focused package tests

## R CMD check results

- `R CMD check --as-cran` on Linux: 0 errors | 0 warnings | 2 notes

Notes:

1. `New submission`
   - This is the first CRAN submission for `Rtinycc`.

2. `Compilation used the following non-portable flag(s): '-mno-omit-leaf-frame-pointer'`
   - This note comes from the check environment toolchain rather than from package-specific `Makevars` flags added by `Rtinycc`.
   - The package's own Unix `src/Makevars` only adds include paths and an rpath entry for the bundled `libtcc`.
   - The package does not rely on this flag for functionality.

## Additional notes

- `Rtinycc` bundles and builds TinyCC for package use in R. Licensing details for the bundled compiler are documented in `inst/LICENSE.note` and reflected in `DESCRIPTION`.
- The package includes Windows support and uses a separate Windows tinytest CI workflow because the work in this release focused heavily on external-pointer/finalizer safety and cross-platform FFI semantics.
- No network access is required in examples, tests, or vignettes.
