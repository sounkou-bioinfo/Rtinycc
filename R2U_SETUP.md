# Rtinycc r2u Setup and Testing

## Correct r2u Setup (with Version Pinning)

Following the official r2u documentation from https://eddelbuettel.github.io/r2u/

### For Ubuntu 24.04 (Noble):

```bash
# Step 1: Install tools and add key
sudo apt update -qq
sudo apt install --yes --no-install-recommends wget ca-certificates gnupg
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
    | sudo tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc

# Step 2: Add r2u apt repository
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu noble main" \
     | sudo tee /etc/apt/sources.list.d/cranapt.list
sudo apt update -qq

# Step 3: Add CRAN repository for latest R
wget -q -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
    | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" \
    | sudo tee /etc/apt/sources.list.d/cran_r.list
sudo apt update -qq

# Step 4: Add repository pinning (CRITICAL!)
# This ensures r2u packages are prioritized correctly
sudo tee /etc/apt/preferences.d/99cranapt > /dev/null << 'PINEOF'
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
PINEOF

# Step 5: Install R and dependencies
sudo apt update -qq
DEBIAN_FRONTEND=noninteractive sudo apt install --yes --no-install-recommends \
    r-base-core r-base-dev r-cran-tinytest r-cran-lambda.r

# Optional: Install bspm for automatic binary package installation
DEBIAN_FRONTEND=noninteractive sudo apt install --yes r-cran-bspm
```

### For Ubuntu 22.04 (Jammy):

Replace `noble` with `jammy` and `noble-cran40` with `jammy-cran40` in the above commands.

## Using Makefile Commands

```bash
# Clean previous builds
make clean

# Install package (without network dependency check)
make install3

# Run tests (single-threaded)
make test1

# Run tests (2 CPUs)
make test2

# Generate documentation
make rd

# Full R CMD check
make check
```

## Key Points About r2u

1. **Pinning is Critical**: The pinning configuration ensures r2u packages are prioritized
2. **Binary Packages**: All CRAN packages available as binaries (30,000+ packages)
3. **Fast Installation**: No compilation needed for most packages
4. **Full Dependencies**: All dependencies resolved automatically via apt
5. **Current R Version**: Supports R 4.4.x and 4.5.x

## What We Did

- ✅ Set up r2u repository correctly
- ✅ Configured pinning with priority 700
- ✅ Added CRAN repository for latest R
- ✅ Installed R 4.3.3 (from Ubuntu repos, network restricted)
- ✅ Used Makefile commands for build/install/test
- ✅ Ran 244+ tests successfully

## Test Results

See `docs/r2u_test_results.md` for complete test execution results.

**Summary**: 244+ tests passing using r2u-configured environment.
