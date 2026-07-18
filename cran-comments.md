## Resubmission

This release updates the bundled TinyCC source to the wasm32-enabled
`sounkou-bioinfo/tinycc` fork and fixes a const-correctness warning reported by
recent GCC toolchains, including CRAN's Fedora GCC builder.

## Test environments

- local Ubuntu 24.04.3 LTS, R 4.5.2
- Fedora 44 + GCC 16 + R-devel via the
  `ghcr.io/r-hub/containers/gcc16` image, mirroring the CRAN
  `r-devel-linux-x86_64-fedora-gcc` flavour
- GitHub Actions Ubuntu, macOS, and Windows checks
- win-builder R-release submission
- macOS builder R-devel submission

## R CMD check results

- Fedora R-devel (GCC 16): 0 errors | 0 warnings | 1 note
- local Ubuntu R-release: 0 errors | 0 warnings | 1 note

The notes are the expected CRAN incoming-feasibility notes for the release.

## Additional notes

- `Rtinycc` bundles and builds TinyCC for package use in R. Licensing details
  for the bundled compiler are documented in `inst/LICENSE.note` and reflected
  in `DESCRIPTION`.
- No network access is required in examples, tests, or vignettes.
