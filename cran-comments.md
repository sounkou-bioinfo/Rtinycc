## Resubmission

This release updates the bundled TinyCC source to the wasm32-enabled
`sounkou-bioinfo/tinycc` fork and fixes a const-correctness warning reported by
recent GCC toolchains, including CRAN's Fedora GCC builder. It also hardens
callable-symbol and struct/union ownership, async callback queue identity and
result validation, range checking, and non-memory output handling.

## Test environments

- local Ubuntu 24.04.3 LTS, R 4.5.2
- Fedora 44 + GCC 16 + R-devel via the
  `ghcr.io/r-hub/containers/gcc16` image, mirroring the CRAN
  `r-devel-linux-x86_64-fedora-gcc` flavour
- GitHub Actions Ubuntu, macOS, and Windows checks
- win-builder R-release submission (awaiting results)
- macOS builder R-devel upload attempted; the service returned HTTP 502

## R CMD check results

- Fedora R-devel (GCC 16): 0 errors | 0 warnings | 0 notes
- local Ubuntu R 4.6.0: 0 errors | 0 warnings | 1 note about the
  non-portable compiler flag `-mno-omit-leaf-frame-pointer`.

## Additional notes

- `Rtinycc` bundles and builds TinyCC for package use in R. Licensing details
  for the bundled compiler are documented in `inst/LICENSE.note` and reflected
  in `DESCRIPTION`.
- No network access is required in examples, tests, or vignettes.
