#!/usr/bin/env bash
set -euo pipefail

native_windows=0
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*) native_windows=1 ;;
esac

R_VERSION="${R_VERSION:-4.5.3}"
WINEARCH="${WINEARCH:-win64}"
WINEPREFIX="${WINEPREFIX:-$HOME/.wine-rtinycc-r45}"
R_WIN_HOME="${R_WIN_HOME:-C:\\Program Files\\R\\R-${R_VERSION}}"
RTOOLS_WIN_HOME="${RTOOLS_WIN_HOME:-C:\\rtools45}"
RTOOLS_USR_BIN="${RTOOLS_USR_BIN:-${RTOOLS_WIN_HOME}\\usr\\bin}"
RTOOLS_MINGW_BIN="${RTOOLS_MINGW_BIN:-${RTOOLS_WIN_HOME}\\x86_64-w64-mingw32.static.posix\\bin}"

usage() {
  cat <<'EOF'
Usage:
  scripts/win-r.sh [R|Rscript|CMD] [args...]

Examples:
  scripts/win-r.sh R --version
  scripts/win-r.sh CMD INSTALL --preclean .
  scripts/win-r.sh Rscript path/to/script.R

On native Windows/MSYS this delegates to local R/Rscript on PATH.
On Linux/macOS it runs the Windows binaries under Wine.
EOF
}

translate_arg() {
  local arg="$1"

  if [ "$native_windows" -eq 1 ]; then
    printf '%s\n' "$arg"
    return 0
  fi

  if [ "$arg" = "." ]; then
    winepath -w "$PWD"
    return 0
  fi

  if [ -e "$arg" ]; then
    local abs
    abs="$(cd "$(dirname "$arg")" && pwd)/$(basename "$arg")"
    winepath -w "$abs"
    return 0
  fi

  printf '%s\n' "$arg"
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

mode="${1:-R}"
case "$mode" in
  R|Rscript|CMD) shift || true ;;
  *)
    set -- "$mode" "$@"
    mode="Rscript"
    ;;
esac

if [ "$native_windows" -eq 1 ]; then
  case "$mode" in
    R) exec R "$@" ;;
    Rscript) exec Rscript "$@" ;;
    CMD) exec R CMD "$@" ;;
  esac
fi

command -v wine >/dev/null 2>&1 || {
  echo "wine not found" >&2
  exit 1
}
command -v winepath >/dev/null 2>&1 || {
  echo "winepath not found" >&2
  exit 1
}

case "$mode" in
  R)
    exe="${R_WIN_HOME}\\bin\\x64\\R.exe"
    ;;
  Rscript)
    exe="${R_WIN_HOME}\\bin\\x64\\Rscript.exe"
    ;;
  CMD)
    exe="${R_WIN_HOME}\\bin\\x64\\R.exe"
    set -- CMD "$@"
    ;;
esac

translated=()
for arg in "$@"; do
  translated+=("$(translate_arg "$arg")")
done

export WINEARCH WINEPREFIX
export WINEPATH="${RTOOLS_USR_BIN};${RTOOLS_MINGW_BIN};${R_WIN_HOME}\\bin\\x64"

exec wine "$exe" "${translated[@]}"
