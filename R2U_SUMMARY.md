# Summary: r2u Setup for Rtinycc

## The Problem

Rtinycc requires **R >= 4.4.0**, but Ubuntu's default R package is only 4.3.3.

## The Solution

Use r2u (https://eddelbuettel.github.io/r2u/) which provides:
- **R 4.4.x and R 4.5.x** from CRAN repository
- **30,000+ binary packages** pre-compiled for Ubuntu
- **Fast installation** (seconds instead of minutes/hours)

## Quick Install (Copy-Paste)

For Ubuntu 24.04 (Noble):

```bash
# Add repositories
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | sudo tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu noble main" | sudo tee /etc/apt/sources.list.d/cranapt.list
wget -q -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" | sudo tee /etc/apt/sources.list.d/cran_r.list

# Set up pinning
sudo tee /etc/apt/preferences.d/99cranapt > /dev/null << 'PINEOF'
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
PINEOF

# Install R and dependencies
sudo apt update -qq
sudo apt install -y r-base-core r-base-dev r-cran-tinytest r-cran-lambda.r

# VERIFY VERSION
R --version  # Must show 4.4.x or 4.5.x
```

## Build Rtinycc

```bash
cd Rtinycc
make install3
make test1
```

## Common Mistake

❌ **WRONG**: Installing R without CRAN repo gets you Ubuntu's old R 4.3.3

✅ **CORRECT**: Following steps above gets you r2u's R 4.4.x/4.5.x

## More Info

- **Quick guide**: `R2U_INSTALLATION.md`
- **Detailed setup**: `R2U_SETUP.md`
- **r2u homepage**: https://eddelbuettel.github.io/r2u/

## Key Points

1. r2u requires proper setup with CRAN repository
2. Version pinning ensures r2u packages take priority
3. Verify R version after installation (must be >= 4.4.0)
4. Do NOT use Ubuntu's base-r package (it's 4.3.x)
