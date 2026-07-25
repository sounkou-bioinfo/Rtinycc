## Resubmission

This release resolves CRAN's Linux arm64 installation warning:

```
arm64-link.c:384:13: warning: ISO C90 forbids mixed declarations and code
```

The bundled TinyCC source now declares the TLS relocation variables before
executable statements. The same package check is exercised on a native Linux
arm64 `rcheckserver` GitHub Actions runner.

## Test environments

- local Ubuntu 24.04.3 LTS, R 4.6.0
- Fedora 44 + GCC 16 + R-devel via the
  `ghcr.io/r-hub/containers/gcc16` image, mirroring CRAN's
  `r-devel-linux-x86_64-fedora-gcc` flavour
- native Linux arm64 + R-devel via the
  `ghcr.io/r-devel/rcheckserver/ubuntu` image, mirroring the CRAN special
  check
- GitHub Actions Ubuntu, macOS, and Windows checks

## R CMD check results

- native Linux arm64 R-devel: 0 errors | 0 warnings | 0 notes
- Fedora R-devel (GCC 16): 0 errors | 0 warnings | 0 notes
- local Ubuntu R 4.6.0: 0 errors | 0 warnings | 2 notes: the expected
  CRAN incoming feasibility note for an update submitted within a week, and
  the non-portable compiler flag `-mno-omit-leaf-frame-pointer` inherited from
  the local R toolchain.

## Additional notes

- `Rtinycc` bundles and builds TinyCC for package use in R. Licensing details
  for the bundled compiler are documented in `inst/LICENSE.note` and reflected
  in `DESCRIPTION`.
- No network access is required in examples, tests, or vignettes.
