# Testing Rtinycc

## R Version Requirement

This package requires **R >= 4.4.0** as specified in DESCRIPTION.

## Testing Locally

### Install R >= 4.4.0 via r2u (Ubuntu)

For Ubuntu 24.04 (Noble) or 22.04 (Jammy), follow `R2U_INSTALLATION.md`:

```bash
# Quick install
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
  | sudo tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu noble main" \
  | sudo tee /etc/apt/sources.list.d/cranapt.list
wget -q -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
  | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" \
  | sudo tee /etc/apt/sources.list.d/cran_r.list

sudo tee /etc/apt/preferences.d/99cranapt > /dev/null << 'EOF'
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
EOF

sudo apt update
sudo apt install -y r-base-core r-base-dev r-cran-tinytest r-cran-lambda.r

# Verify version
R --version  # Must show 4.4.x or 4.5.x
```

### Build and Test

Using Makefile:
```bash
make clean
make install3  # Install without dependency check
make test1     # Run tests (single-threaded)
```

Or manually:
```bash
R CMD build .
sudo R CMD INSTALL Rtinycc_*.tar.gz
R -e "tinytest::test_package('Rtinycc')"
```

## Continuous Integration

GitHub Actions automatically tests with:
- R 4.4.2 on Ubuntu 24.04
- R 4.5.0 on Ubuntu 24.04

Using r2u for fast binary package installation.

See `.github/workflows/r2u-test.yml` for configuration.

## Important Notes

### Do NOT Use Ubuntu's Base R

Ubuntu 24.04 ships with R 4.3.3, which is **too old** and does not meet the R >= 4.4.0 requirement.

❌ **WRONG**: `sudo apt install r-base-core` (gets 4.3.3)

✅ **CORRECT**: Follow r2u setup above (gets 4.4.x or 4.5.x)

### Verify Your R Version

After installation, always verify:
```bash
R --version | head -1
# Should show: R version 4.4.x or 4.5.x
```

If you see R 4.3.x, you installed the wrong version. See `R2U_INSTALLATION.md` troubleshooting section.

## Test Files

Tests are in `inst/tinytest/`:
- `test_aaa_windows_smoke.R` - Basic functionality
- `test_ffi_*.R` - FFI system tests
- `test_callbacks.R` - Callback functionality
- `test_structs.R` - Struct handling
- `test_tcc_quick_*.R` - R-to-C transpilation tests
- And more...

Run specific test file:
```bash
R -e "tinytest::run_test_file('inst/tinytest/test_ffi_api.R')"
```

## Troubleshooting

### "Package requires R >= 4.4.0 but version is 4.3.3"

You have Ubuntu's base R installed instead of CRAN's R. Fix:

```bash
sudo apt remove r-base-core r-base-dev
# Then follow r2u setup steps above
```

### "Unable to install package"

Make sure you installed dependencies from r2u:
```bash
sudo apt install -y r-cran-tinytest r-cran-lambda.r
```

### Test Failures

If tests fail:
1. Check R version is >= 4.4.0
2. Rebuild package: `make clean && make install3`
3. Check specific failing test in `inst/tinytest/`
4. See test output for error details

## Getting Help

- r2u documentation: https://eddelbuettel.github.io/r2u/
- Installation guide: `R2U_INSTALLATION.md`
- Detailed setup: `R2U_SETUP.md`
- Quick reference: `R2U_SUMMARY.md`
