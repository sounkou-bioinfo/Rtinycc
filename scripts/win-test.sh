#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"
pkg_name="${PKGNAME:-$(sed -n 's/^Package:[[:space:]]*//p' "$repo_root/DESCRIPTION" | head -1)}"
ncpu_expr="${NCPU_EXPR:-${NCPUS_EXPR:-}}"

usage() {
  cat <<'EOF'
Usage:
  scripts/win-test.sh [all|deps|install|tinytest|union|smoke|file <path>|files <path...>|pkgfiles <path...>]

Defaults:
  all      install deps, install the package, then run tinytest in-process

Examples:
  scripts/win-test.sh
  scripts/win-test.sh deps
  scripts/win-test.sh install
  scripts/win-test.sh tinytest
  scripts/win-test.sh union
  scripts/win-test.sh file inst/tinytest/test_unions.R
  scripts/win-test.sh files inst/tinytest/test_structs.R inst/tinytest/test_unions.R
  scripts/win-test.sh pkgfiles inst/tinytest/test_unions.R inst/tinytest/test_variadic.R

Environment:
  NCPU_EXPR=2L   tinytest worker count expression; unset means in-process
  NCPUS_EXPR=2L  backward-compatible alias for NCPU_EXPR
  SKIP_DEPS=1    skip dependency installation in `all`
EOF
}

run_rscript_file() {
  local rfile="$1"
  "$script_dir/win-r.sh" Rscript --vanilla "$rfile"
}

run_with_temp_r() {
  local rfile
  rfile="$(mktemp "${TMPDIR:-/tmp}/rtinycc-win-XXXXXX.R")"
  trap 'rm -f "$rfile"' RETURN
  cat >"$rfile"
  run_rscript_file "$rfile"
  rm -f "$rfile"
  trap - RETURN
}

run_installed_tinytest() {
  cd "$repo_root"
  if [ -n "${ncpu_expr}" ]; then
    run_with_temp_r <<EOF
tinytest::test_package("${pkg_name}", testdir = "inst/tinytest", ncpu = ${ncpu_expr})
EOF
  else
    run_with_temp_r <<EOF
tinytest::test_package("${pkg_name}", testdir = "inst/tinytest")
EOF
  fi
}

install_deps() {
  cd "$repo_root"
  run_with_temp_r <<'EOF'
repos <- "https://cloud.r-project.org"
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = repos)
}
if (!requireNamespace("tinytest", quietly = TRUE)) {
  install.packages("tinytest", repos = repos)
}
remotes::install_deps(dependencies = TRUE)
EOF
}

run_test_file() {
  local test_file="$1"
  cd "$repo_root"
  run_with_temp_r <<EOF
tinytest::run_test_file(normalizePath("${test_file}", winslash = "/", mustWork = TRUE))
EOF
}

run_test_files() {
  cd "$repo_root"
  local files=("$@")
  local files_r=""
  local file
  for file in "${files[@]}"; do
    if [ -n "$files_r" ]; then
      files_r="$files_r, "
    fi
    files_r="$files_r\"$file\""
  done
  run_with_temp_r <<EOF
files <- c(${files_r})
files <- normalizePath(files, winslash = "/", mustWork = TRUE)
results <- list()
for (f in files) {
  message("==> ", basename(f))
  results[[basename(f)]] <- tinytest::run_test_file(f)
}
invisible(results)
EOF
}

run_pkg_test_files() {
  cd "$repo_root"
  local files=("$@")
  local files_r=""
  local file
  for file in "${files[@]}"; do
    if [ -n "$files_r" ]; then
      files_r="$files_r, "
    fi
    files_r="$files_r\"$file\""
  done
  run_with_temp_r <<EOF
pkg_name <- "${pkg_name}"
testdir <- normalizePath("inst/tinytest", winslash = "/", mustWork = TRUE)
files <- c(${files_r})
files <- basename(normalizePath(files, winslash = "/", mustWork = TRUE))
oldwd <- getwd()
on.exit(setwd(oldwd), add = TRUE)
setwd(testdir)
library(pkg_name, character.only = TRUE)
results <- list()
for (f in files) {
  message("==> ", f)
  results[[f]] <- tinytest::run_test_file(f, at_home = FALSE)
}
invisible(results)
EOF
}

mode="${1:-all}"
case "$mode" in
  -h|--help)
    usage
    exit 0
    ;;
  all)
    if [ "${SKIP_DEPS:-0}" != "1" ]; then
      "$0" deps
    fi
    "$0" install
    "$0" tinytest
    ;;
  deps)
    install_deps
    ;;
  install)
    cd "$repo_root"
    "$script_dir/win-r.sh" CMD INSTALL --preclean .
    ;;
  tinytest)
    run_installed_tinytest
    ;;
  union)
    run_test_file "inst/tinytest/test_unions.R"
    ;;
  smoke)
    run_test_file "inst/tinytest/test_aaa_windows_smoke.R"
    ;;
  file)
    shift
    [ $# -ge 1 ] || {
      echo "missing test file path" >&2
      usage >&2
      exit 1
    }
    run_test_file "$1"
    ;;
  files)
    shift
    [ $# -ge 1 ] || {
      echo "missing test file paths" >&2
      usage >&2
      exit 1
    }
    run_test_files "$@"
    ;;
  pkgfiles)
    shift
    [ $# -ge 1 ] || {
      echo "missing test file paths" >&2
      usage >&2
      exit 1
    }
    run_pkg_test_files "$@"
    ;;
  *)
    echo "unknown mode: $mode" >&2
    usage >&2
    exit 1
    ;;
esac
