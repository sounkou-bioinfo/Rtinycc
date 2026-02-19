# R2U Installation Guide for Rtinycc

## CRITICAL: You MUST use R >= 4.4.0

Rtinycc requires **R >= 4.4.0**. Ubuntu's base-r package (4.3.x) is **TOO OLD**.

You must use r2u with the CRAN repository to get R 4.4.x or 4.5.x.

## Quick Start (Ubuntu 24.04 Noble)

```bash
# 1. Add r2u repository
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
    | sudo tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu noble main" \
     | sudo tee /etc/apt/sources.list.d/cranapt.list

# 2. Add CRAN repository (provides R 4.4.x/4.5.x)
wget -q -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
    | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" \
    | sudo tee /etc/apt/sources.list.d/cran_r.list

# 3. Set up pinning (ensures r2u priority)
sudo tee /etc/apt/preferences.d/99cranapt > /dev/null << 'PINEOF'
Package: *
Pin: release o=CRAN-Apt Project
Pin: release l=CRAN-Apt Packages
Pin-Priority: 700
PINEOF

# 4. Install R and dependencies
sudo apt update -qq
sudo apt install -y r-base-core r-base-dev r-cran-tinytest r-cran-lambda.r

# 5. VERIFY you have R >= 4.4.0
R --version  # Must show 4.4.x or 4.5.x, NOT 4.3.x!
```

## Build and Test Rtinycc

```bash
cd Rtinycc
make clean
make install3
make test1
```

## Why This Matters

- **r2u provides current R**: 4.4.x and 4.5.x releases
- **Ubuntu base-r is old**: Still on 4.3.3 (released 2024-02)
- **Rtinycc needs 4.4.0+**: Uses features not in 4.3.x
- **Binary packages**: r2u provides 30,000+ pre-compiled packages

## Troubleshooting

### "R is version 4.3.3, package requires R >= 4.4.0"

You installed Ubuntu's base-r instead of CRAN's R. Fix:

```bash
# Remove old R
sudo apt remove r-base-core r-base-dev

# Add CRAN repo (step 2 above)
# Install again (step 4 above)
# Verify version (step 5 above)
```

### How to check which repository provides R

```bash
apt-cache policy r-base-core
```

Look for:
- ✅ CORRECT: `cloud.r-project.org` (provides 4.4.x/4.5.x)
- ❌ WRONG: `ubuntu.com` or `archive.ubuntu.com` (provides 4.3.x)

## Complete Documentation

See `R2U_SETUP.md` for detailed instructions including:
- Ubuntu 22.04 (Jammy) setup
- bspm configuration
- Common mistakes and solutions
