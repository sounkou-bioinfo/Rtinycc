## Submission

This submission updates Rtinycc from 0.1.16 to 0.1.17. It is a bug-fix
release, submitted five weeks after 0.1.16 because of the first item:

- Calls with a `character_array` argument crashed on current R. The generated
  wrappers fell back to the non-API `STRING_PTR()` accessor, which R no longer
  declares. They now call `STRING_PTR_RO()` directly.
- `tcc_read_cstring(max_bytes = )` read exactly `max_bytes` bytes, past the
  terminator of shorter strings. It now stops at the terminator.
- The bundled TinyCC source is refreshed with upstream fixes for qualified
  array types, `restrict`, designated initializers, and arm64 stack offsets.

## Test environments

- local Ubuntu 24.04.3 LTS, R 4.6.2
- Fedora + GCC + R-devel via the `ghcr.io/r-hub/containers` image
- native Linux arm64 + R-devel via the
  `ghcr.io/r-devel/rcheckserver/ubuntu` image
- GitHub Actions: Ubuntu (R-devel), macOS (release and oldrel), Windows
  x86_64 (release), and Windows arm64 (release)

## R CMD check results

- local Ubuntu R 4.6.2: 0 errors | 0 warnings | 1 note
- GitHub Actions and container checks: no errors or warnings

The local note is for `-mno-omit-leaf-frame-pointer`, which is inherited from
the local R toolchain.

## Reverse dependencies

Rduckhts and rdyncall list Rtinycc in `Suggests`. Both pass `R CMD check`
with this version installed (Status: OK).

## Additional notes

- `Rtinycc` bundles and builds TinyCC for package use in R. Licensing details
  for the bundled compiler are documented in `inst/LICENSE.note` and reflected
  in `DESCRIPTION`.
- No network access is required in examples, tests, or vignettes.
