#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  cat <<'EOF'
Usage:
  scripts/bootstrap-wine-rtools.sh

Environment:
  R_VERSION=4.5.3
  WINEPREFIX=$HOME/.wine-rtinycc-r45
  WINEARCH=win64
  DOWNLOAD_DIR=/tmp/rtinycc-win-cache
  R_INSTALLER_URL=...
  RTOOLS_INSTALLER_URL=...

This bootstraps Windows R and Rtools45 into a dedicated Wine prefix.
EOF
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*)
    echo "bootstrap-wine-rtools.sh is for Linux/macOS hosts using Wine, not native Windows." >&2
    exit 1
    ;;
esac

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "missing required command: $1" >&2
    exit 1
  }
}

require_cmd wine
require_cmd wineboot
require_cmd curl

R_VERSION="${R_VERSION:-4.5.3}"
WINEARCH="${WINEARCH:-win64}"
WINEPREFIX="${WINEPREFIX:-$HOME/.wine-rtinycc-r45}"
DOWNLOAD_DIR="${DOWNLOAD_DIR:-${TMPDIR:-/tmp}/rtinycc-win-cache}"

R_INSTALLER_URL="${R_INSTALLER_URL:-https://cran.r-project.org/bin/windows/base/R-${R_VERSION}-win.exe}"
RTOOLS_INSTALLER_URL="${RTOOLS_INSTALLER_URL:-https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6768-6492.exe}"

mkdir -p "$DOWNLOAD_DIR"
export WINEARCH WINEPREFIX

if wine --version 2>&1 | grep -q "wine32 is missing"; then
  cat >&2 <<'EOF'
wine is installed, but this host is missing wine32 support.
64-bit installs may still work for some setups, but Wine itself is warning that
multiarch support is incomplete. On Debian/Ubuntu the usual fix is:

  dpkg --add-architecture i386
  apt-get update
  apt-get install wine32:i386
EOF
fi

download() {
  local url="$1"
  local out="$2"
  if [ -f "$out" ]; then
    echo "using cached installer: $out"
    return 0
  fi
  echo "downloading $(basename "$out")"
  curl -L --fail --output "$out" "$url"
}

r_installer="$DOWNLOAD_DIR/$(basename "$R_INSTALLER_URL")"
rtools_installer="$DOWNLOAD_DIR/$(basename "$RTOOLS_INSTALLER_URL")"

download "$R_INSTALLER_URL" "$r_installer"
download "$RTOOLS_INSTALLER_URL" "$rtools_installer"

echo "initializing Wine prefix: $WINEPREFIX"
wineboot -u

echo "installing Windows R ${R_VERSION}"
wine "$r_installer" /VERYSILENT /SUPPRESSMSGBOXES /SP- /NORESTART

echo "installing Rtools45"
wine "$rtools_installer" /VERYSILENT /SUPPRESSMSGBOXES /SP- /NORESTART

echo "verifying Windows R under Wine"
"$script_dir/win-r.sh" R --version

cat <<EOF

Wine bootstrap complete.

Prefix:        $WINEPREFIX
R installer:   $r_installer
Rtools installer: $rtools_installer

Next commands:
  scripts/win-test.sh install
  scripts/win-test.sh union
  scripts/win-test.sh tinytest
EOF
