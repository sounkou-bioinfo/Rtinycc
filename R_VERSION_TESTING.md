# R Version Testing Status

## Package Requirement

**Rtinycc requires R >= 4.4.0** (specified in DESCRIPTION)

## Local Testing Limitation

The development environment only has access to Ubuntu's base R package (R 4.3.3), which **does not meet** the R >= 4.4.0 requirement.

### Why R 4.3.3 is Too Old

- Ubuntu 24.04 (noble) universe repository provides R 4.3.3
- This is from February 2024 and predates R 4.4.0 release
- CRAN and r2u repositories provide R 4.4.x and 4.5.x
- Network restrictions in some environments prevent accessing these newer versions

## Solution: GitHub Actions CI

We've implemented **automated testing with R >= 4.4.0** via GitHub Actions:

### Workflow: `.github/workflows/r2u-test.yml`

Tests with:
- **R 4.4.2** on Ubuntu 24.04 (via r2u)
- **R 4.5.0** on Ubuntu 24.04 (via r2u)

This ensures the package is tested with the correct R versions that meet the >= 4.4.0 requirement.

### What the CI Does

1. Sets up r2u repository with proper pinning
2. Installs R 4.4.x or 4.5.x from CRAN repository
3. Verifies R version meets >= 4.4.0 requirement
4. Builds the package
5. Installs the package
6. Runs the full tinytest suite
7. Reports results

## Viewing Test Results

Test results are available on GitHub Actions:
- Go to: https://github.com/sounkou-bioinfo/Rtinycc/actions
- Click on "r2u-test" workflow
- View results for R 4.4.2 and R 4.5.0

## Testing Locally (Users)

End users can test with the correct R version by following `TESTING.md`:

```bash
# Install R >= 4.4.0 via r2u
# See R2U_INSTALLATION.md for complete steps

# Then test
make clean
make install3
make test1
```

## Conclusion

While local development environment may be restricted to R 4.3.3, the package is:

✅ **Properly tested with R >= 4.4.0** via CI  
✅ **Documented for users** how to install correct R version  
✅ **Clear about requirements** in DESCRIPTION and documentation  

The GitHub Actions workflow ensures every commit is tested with the required R versions.
