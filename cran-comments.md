## Submission

This submission updates Rtinycc from 0.1.15 to 0.1.16. It removes the runtime
dependency on `lambda.r` by carrying the fixed-arity rule dispatcher used by
the code generator. The upstream attribution and LGPL-3 licensing are recorded
in `DESCRIPTION`, the source file, and `inst/LICENSE.note`.

The bundled TinyCC source is refreshed with upstream linker, TLS, and
bounds-checking fixes. This release also adds native Windows arm64 builds and
improves TinyCC's experimental wasm32 backend; general webR FFI remains
unsupported.

## Test environments

- local Ubuntu 24.04.3 LTS, R 4.6.0
- Fedora 44 + GCC 16 + R-devel via the
  `ghcr.io/r-hub/containers/gcc16` image
- native Linux arm64 + R-devel via the
  `ghcr.io/r-devel/rcheckserver/ubuntu` image
- GitHub Actions Ubuntu, macOS, Windows x86_64, and Windows arm64 checks
- win-builder R-release submission
- macOS builder arm64, R 4.6.1

## R CMD check results

- local Ubuntu R 4.6.0: 0 errors | 0 warnings | 1 note
- native Linux arm64 R-devel: 0 errors | 0 warnings | 0 notes
- Fedora R-devel (GCC 16): 0 errors | 0 warnings | 1 note
- macOS builder arm64, R 4.6.1: 0 errors | 0 warnings | 0 notes

The local note is for `-mno-omit-leaf-frame-pointer`, which is inherited from
the local R toolchain. The Fedora note reports the development version number
`0.1.15.9000`; the release tarball uses `0.1.16`.

## Additional notes

- `Rtinycc` bundles and builds TinyCC for package use in R. Licensing details
  for the bundled compiler are documented in `inst/LICENSE.note` and reflected
  in `DESCRIPTION`.
- No network access is required in examples, tests, or vignettes.
- There are no strong reverse dependencies on CRAN.
