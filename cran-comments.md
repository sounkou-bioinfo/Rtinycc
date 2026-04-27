## Resubmission

This release fixes the install failure observed on the
`r-oldrel-macos-arm64` (Big Sur, clang 14) CRAN flavour for `Rtinycc`
0.1.9, where `libtcc.dylib` failed to link with:

```
ld: can't use -undefined warning or suppress with -twolevel_namespace
```

The bundled TinyCC Makefile pairs `-flat_namespace` with
`-undefined warning` when the detected clang is older than 15. We strip
`-flat_namespace` on macOS to avoid TinyCC SIGSEGV behavior under flat
namespace, which on clang < 15 left `-undefined warning` on its own and
that combination is rejected by the modern macOS linker under the
default `-twolevel_namespace`. The configure step now strips both flags,
which keeps the link valid on every clang generation we test.

All other CRAN flavours were passing for 0.1.9.

## Test environments

- local Ubuntu 24.04.3 LTS, R 4.5.2
- local Fedora 44 + GCC 16 + R-devel via the `ghcr.io/r-hub/containers/gcc16`
  image (mirrors the CRAN `r-devel-linux-x86_64-fedora-gcc` flavour),
  reproduced with `scripts/docker-cran-fedora.sh` and the new
  `.github/workflows/fedora-cran-check.yaml` workflow
- GitHub Actions ubuntu-latest (devel)
- GitHub Actions macos-latest (release, oldrel) — `oldrel` exercises the
  same Apple silicon configure path that the CRAN macOS oldrel arm64
  builder uses
- GitHub Actions windows-latest (release)
- additional GitHub Actions Windows tinytest workflow for focused package tests

## R CMD check results

- `R CMD check --as-cran` on Linux: 0 errors | 0 warnings | 1 note
- `R CMD check --as-cran` on Fedora R-devel (gcc16 container):
  0 errors | 0 warnings | 1 note

Note:

1. `Days since last update: <N>`
   - Triggered because 0.1.10 follows 0.1.9 within the standard CRAN
     update window. The change is targeted entirely at restoring
     installation on macOS oldrel arm64.

## Additional notes

- `Rtinycc` bundles and builds TinyCC for package use in R. Licensing
  details for the bundled compiler are documented in `inst/LICENSE.note`
  and reflected in `DESCRIPTION`.
- No network access is required in examples, tests, or vignettes.
