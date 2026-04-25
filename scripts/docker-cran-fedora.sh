#!/usr/bin/env bash
# Run R CMD build && R CMD check --as-cran inside the rhub Fedora R-devel
# container that mirrors CRAN's r-devel-linux-x86_64-fedora-gcc flavour
# (Fedora 44 + GCC 16 + the same R-devel snapshot used by the CRAN builder).
#
# Usage:
#   scripts/docker-cran-fedora.sh                  # check current package
#   scripts/docker-cran-fedora.sh /path/to/pkg     # check a specific package dir
#
# Optional env vars:
#   RTINYCC_DOCKER_IMAGE   override container image (default: gcc16)
#   RTINYCC_RLIBS_CACHE    host path used to cache R suggests across runs
#                          (default: $HOME/.cache/rtinycc-rhub-fedora-libs)
set -euo pipefail

IMAGE="${RTINYCC_DOCKER_IMAGE:-ghcr.io/r-hub/containers/gcc16:latest}"
WORKDIR="/pkg"
PKGDIR="${1:-$PWD}"
PKGDIR="$(cd "$PKGDIR" && pwd)"
RLIBS_CACHE="${RTINYCC_RLIBS_CACHE:-$HOME/.cache/rtinycc-rhub-fedora-libs}"
mkdir -p "$RLIBS_CACHE"

DOCKER_TTY_FLAGS=()
if [ -t 0 ] && [ -t 1 ]; then
  DOCKER_TTY_FLAGS=(-it)
fi

exec docker run --rm "${DOCKER_TTY_FLAGS[@]}" \
  -v "$PKGDIR":"$WORKDIR" \
  -v "$RLIBS_CACHE":/root/R-libs \
  -w "$WORKDIR" \
  "$IMAGE" \
  bash -lc '
    set -euo pipefail

    # OS deps for: pandoc (NEWS/README), qpdf (PDF check),
    # texlive-scheme-medium (PDF manual), libuv-devel (fs/sass build chain)
    dnf install -y --setopt=install_weak_deps=False \
      libuv-devel pandoc qpdf texlive-scheme-medium >/dev/null

    # Cache R libs across runs on the host
    echo "R_LIBS_USER=/root/R-libs" > ~/.Renviron
    mkdir -p /root/R-libs

    R -q -e "
      pkgs <- c(\"lambda.r\",\"bench\",\"callme\",\"knitr\",\"rmarkdown\",
                \"tinytest\",\"treesitter.c\")
      need <- setdiff(pkgs, rownames(installed.packages(lib.loc=\"/root/R-libs\")))
      if (length(need)) install.packages(need, lib=\"/root/R-libs\",
        repos=\"https://cloud.r-project.org\")
    "

    rm -f Rtinycc_*.tar.gz
    R CMD build .
    PKG=$(ls -t Rtinycc_*.tar.gz | head -n1)
    R CMD check --as-cran "$PKG"
  '
