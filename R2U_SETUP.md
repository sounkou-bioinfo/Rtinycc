# Rtinycc r2u Setup and Testing

## Correct r2u Setup (with Version Pinning)

Following the official r2u documentation from https://eddelbuettel.github.io/r2u/

**IMPORTANT**: r2u provides R 4.4.x and R 4.5.x, NOT old Debian R 4.3.x packages. You MUST use r2u's R packages to meet the R >= 4.4.0 requirement.

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

# Step 3: Add CRAN repository for latest R (provides R 4.4.x or 4.5.x)
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

# Step 5: Install R 4.4.x or 4.5.x (from CRAN repo, NOT Ubuntu base-r)
sudo apt update -qq
DEBIAN_FRONTEND=noninteractive sudo apt install --yes --no-install-recommends \
    r-base-core r-base-dev

# Verify you have R >= 4.4.0
R --version

# Step 6: Install dependencies from r2u (as binaries, very fast!)
DEBIAN_FRONTEND=noninteractive sudo apt install --yes --no-install-recommends \
    r-cran-tinytest r-cran-lambda.r

# Optional: Install bspm for automatic binary package installation from R
DEBIAN_FRONTEND=noninteractive sudo apt install --yes r-cran-bspm
```

### For Ubuntu 22.04 (Jammy):

Replace `noble` with `jammy` and `noble-cran40` with `jammy-cran40` in the above commands.

## Verifying Your Setup

After installation, verify you have the correct R version:

```bash
R --version
# Should show R version 4.4.x or 4.5.x, NOT 4.3.x

# Check where R came from
apt-cache policy r-base-core
# Should show installed from cloud.r-project.org repository
```

## Using Makefile Commands

Once r2u and R >= 4.4.0 are installed:

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

## Why r2u?

1. **Latest R versions**: r2u provides R 4.4.x and 4.5.x (current releases)
2. **Binary packages**: All 30,000+ CRAN packages as pre-compiled binaries
3. **Fast installation**: No compilation needed, packages install in seconds
4. **Full dependencies**: All dependencies resolved automatically via apt
5. **Pinning ensures priority**: r2u packages take precedence over Ubuntu repos

## Common Mistakes

### ❌ WRONG: Using Ubuntu's old R 4.3.x

```bash
# This installs old Debian R 4.3.3 - TOO OLD!
sudo apt install r-base-core  # Without CRAN repo
```

### ✅ CORRECT: Using r2u with CRAN R 4.4.x/4.5.x

```bash
# Add CRAN repository first (provides R 4.4.x or 4.5.x)
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" \
    | sudo tee /etc/apt/sources.list.d/cran_r.list
sudo apt update

# Then install - gets R 4.4.x or 4.5.x from CRAN repo
sudo apt install r-base-core r-base-dev

# Verify version
R --version  # Must show 4.4.x or 4.5.x
```

## What We Provide

Rtinycc requires **R >= 4.4.0**. The r2u ecosystem provides:
- R 4.4.x and R 4.5.x from CRAN repository
- Pre-compiled binaries for all CRAN packages
- Automatic dependency resolution via apt

Do NOT try to use Ubuntu's base-r 4.3.x packages - they are too old and not compatible with r2u's package ecosystem.
