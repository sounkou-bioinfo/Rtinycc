#!/usr/bin/env bash
set -euo pipefail

IMAGE="${RTINYCC_DOCKER_IMAGE:-rocker/r-devel}"
WORKDIR="/work"
PKGDIR="${1:-$PWD}"
PKGDIR="$(cd "$PKGDIR" && pwd)"

DOCKER_TTY_FLAGS=()
if [ -t 0 ] && [ -t 1 ]; then
  DOCKER_TTY_FLAGS=(-it)
fi

exec docker run --rm "${DOCKER_TTY_FLAGS[@]}" \
  -v "$PKGDIR":"$WORKDIR" \
  -w "$WORKDIR" \
  "$IMAGE" \
  bash -lc '
    set -euo pipefail
    export DEBIAN_FRONTEND=noninteractive

    apt-get update
    apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      devscripts \
      file \
      gfortran \
      git \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libuv1-dev \
      make \
      pandoc \
      pkg-config \
      qpdf \
      tidy

    R -q -e "install.packages(c(\"remotes\", \"roxygen2\"), repos = \"https://cloud.r-project.org\")"
    R -q -e "remotes::install_deps(dependencies = TRUE, repos = \"https://cloud.r-project.org\")"

    R CMD build .
    PKG=$(ls -t Rtinycc_*.tar.gz | head -n1)
    _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran "$PKG"
  '
