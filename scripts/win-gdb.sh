#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"
pkg_name="${PKGNAME:-$(sed -n 's/^Package:[[:space:]]*//p' "$repo_root/DESCRIPTION" | head -1)}"
RTOOLS_WIN_HOME="${RTOOLS_WIN_HOME:-C:\\rtools45}"
GDB_EXE="${GDB_EXE:-}"

native_windows=0
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*) native_windows=1 ;;
esac

usage() {
  cat <<'EOF'
Usage:
  scripts/win-gdb.sh [tinytest|union|file <path>|files <path...>|pkgfiles <path...>|script <path>|expr <R code>]

Purpose:
  Run Windows Rscript under gdb and print a backtrace on segfault.

Examples:
  scripts/win-gdb.sh tinytest
  scripts/win-gdb.sh union
  scripts/win-gdb.sh file inst/tinytest/test_unions.R
  scripts/win-gdb.sh pkgfiles inst/tinytest/test_structs.R inst/tinytest/test_unions.R inst/tinytest/test_variadic.R
  scripts/win-gdb.sh expr "tinytest::test_package('Rtinycc', testdir = 'inst/tinytest')"

Notes:
  - Intended for native Windows/MSYS with gdb available from Rtools.
  - For meaningful backtraces, build an unstripped DLL first:
      make dev-install-debug-win
  - Preserves current environment variables such as RTINYCC_TRACE_FINALIZERS=1.
  - `RTINYCC_GDB_BREAK_FINALIZERS=1` adds breakpoint traces for borrowed/free/tcc/callback finalizers.
  - `GDB_EXE=/c/rtools45/ucrt64/bin/gdb.exe` overrides auto-detection.
EOF
}

require_native_windows() {
  if [ "$native_windows" -ne 1 ]; then
    echo "scripts/win-gdb.sh currently supports native Windows/MSYS only" >&2
    exit 1
  fi
}

find_gdb_exe() {
  local candidate
  local rtools_home_unix=""

  if command -v gdb >/dev/null 2>&1; then
    command -v gdb
    return 0
  fi

  if command -v gdb.exe >/dev/null 2>&1; then
    command -v gdb.exe
    return 0
  fi

  if [ -n "$GDB_EXE" ]; then
    if [ -x "$GDB_EXE" ]; then
      printf '%s\n' "$GDB_EXE"
      return 0
    fi
    echo "GDB_EXE is set but not executable: $GDB_EXE" >&2
    exit 1
  fi

  if [ "$native_windows" -eq 1 ]; then
    for candidate in \
      "/c/rtools45/ucrt64/bin/gdb.exe" \
      "/c/rtools45/mingw64/bin/gdb.exe" \
      "/c/rtools45/x86_64-w64-mingw32.static.posix/bin/gdb.exe"
    do
      if [ -n "$candidate" ] && [ -x "$candidate" ]; then
        printf '%s\n' "$candidate"
        return 0
      fi
    done

    if command -v cygpath >/dev/null 2>&1; then
      rtools_home_unix="$(cygpath -u "$RTOOLS_WIN_HOME" 2>/dev/null || true)"
      for candidate in \
        "$rtools_home_unix/ucrt64/bin/gdb.exe" \
        "$rtools_home_unix/mingw64/bin/gdb.exe" \
        "$rtools_home_unix/x86_64-w64-mingw32.static.posix/bin/gdb.exe"
      do
        if [ -n "$candidate" ] && [ -x "$candidate" ]; then
          printf '%s\n' "$candidate"
          return 0
        fi
      done
    fi
  fi

  echo "gdb not found on PATH, under /c/rtools45, or under $RTOOLS_WIN_HOME" >&2
  exit 1
}

find_rscript_exe() {
  local r_home_win
  local r_home_unix
  local candidate

  r_home_win="$(R RHOME 2>/dev/null | tr -d '\r')"
  if [ -z "$r_home_win" ]; then
    echo "failed to resolve R RHOME" >&2
    exit 1
  fi

  r_home_unix="$(cygpath -u "$r_home_win")"
  for candidate in \
    "$r_home_unix/bin/x64/Rscript.exe" \
    "$r_home_unix/bin/Rscript.exe"
  do
    if [ -x "$candidate" ]; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done

  if command -v Rscript.exe >/dev/null 2>&1; then
    command -v Rscript.exe
    return 0
  fi

  echo "unable to locate Rscript.exe from R home: $r_home_win" >&2
  exit 1
}

run_gdb_file() {
  local rfile="$1"
  local rscript_exe="$2"
  local gdb_exe="$3"
  local rfile_win
  local gdb_cmds
  local rc_src_dir

  cd "$repo_root"
  rfile_win="$(cygpath -w "$rfile")"
  rc_src_dir="$(cygpath -u "$repo_root/src")"
  gdb_cmds="$(mktemp "${TMPDIR:-/tmp}/rtinycc-gdbcmds-XXXXXX.txt")"
  trap 'rm -f "$gdb_cmds"' RETURN

  cat >"$gdb_cmds" <<EOF
set pagination off
set confirm off
set breakpoint pending on
set print frame-arguments all
set print symbol-filename on
set auto-solib-add on
directory $rc_src_dir
handle SIGSEGV stop print nopass
catch load
commands
silent
sharedlibrary
continue
end
EOF

  if [ "${RTINYCC_GDB_BREAK_FINALIZERS:-0}" = "1" ]; then
    cat >>"$gdb_cmds" <<EOF
break RC_make_borrowed_view
commands
silent
printf "\n===== breakpoint RC_make_borrowed_view =====\n"
bt 8
continue
end
break RC_borrowed_view_finalizer
commands
silent
printf "\n===== breakpoint RC_borrowed_view_finalizer =====\n"
bt 8
continue
end
break RC_free_finalizer
commands
silent
printf "\n===== breakpoint RC_free_finalizer =====\n"
bt 8
continue
end
break RC_tcc_finalizer
commands
silent
printf "\n===== breakpoint RC_tcc_finalizer =====\n"
bt 8
continue
end
break RC_callback_finalizer
commands
silent
printf "\n===== breakpoint RC_callback_finalizer =====\n"
bt 8
continue
end
break RC_callback_ptr_finalizer
commands
silent
printf "\n===== breakpoint RC_callback_ptr_finalizer =====\n"
bt 8
continue
end
EOF
  fi

  cat >>"$gdb_cmds" <<'EOF'
run
printf "\n===== gdb backtrace =====\n"
thread apply all bt full
EOF

  "$gdb_exe" --batch -x "$gdb_cmds" --args "$rscript_exe" --vanilla "$rfile_win"
  rm -f "$gdb_cmds"
  trap - RETURN
}

run_with_temp_r() {
  local rscript_exe="$1"
  local gdb_exe="$2"
  local rfile
  rfile="$(mktemp "${TMPDIR:-/tmp}/rtinycc-gdb-XXXXXX.R")"
  trap 'rm -f "$rfile"' RETURN
  cat >"$rfile"
  run_gdb_file "$rfile" "$rscript_exe" "$gdb_exe"
  rm -f "$rfile"
  trap - RETURN
}

run_test_files_expr() {
  local mode_name="$1"
  shift
  local files=("$@")
  local files_r=""
  local f
  for f in "${files[@]}"; do
    if [ -n "$files_r" ]; then
      files_r="$files_r, "
    fi
    files_r="$files_r\"$f\""
  done

  case "$mode_name" in
    files)
      cat <<EOF
files <- c(${files_r})
files <- normalizePath(files, winslash = "/", mustWork = TRUE)
results <- list()
for (f in files) {
  message("==> ", basename(f))
  results[[basename(f)]] <- tinytest::run_test_file(f)
}
invisible(results)
EOF
      ;;
    pkgfiles)
      cat <<EOF
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
      ;;
  esac
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

require_native_windows
gdb_exe="$(find_gdb_exe)"
rscript_exe="$(find_rscript_exe)"

mode="${1:-tinytest}"
case "$mode" in
  tinytest)
    run_with_temp_r "$rscript_exe" "$gdb_exe" <<EOF
tinytest::test_package("${pkg_name}", testdir = "inst/tinytest")
EOF
    ;;
  union)
    run_with_temp_r "$rscript_exe" "$gdb_exe" <<'EOF'
tinytest::run_test_file(normalizePath("inst/tinytest/test_unions.R", winslash = "/", mustWork = TRUE))
EOF
    ;;
  file)
    shift
    [ $# -ge 1 ] || {
      echo "missing test file path" >&2
      usage >&2
      exit 1
    }
    run_with_temp_r "$rscript_exe" "$gdb_exe" <<EOF
tinytest::run_test_file(normalizePath("${1}", winslash = "/", mustWork = TRUE))
EOF
    ;;
  files|pkgfiles)
    shift
    [ $# -ge 1 ] || {
      echo "missing test file paths" >&2
      usage >&2
      exit 1
    }
    run_test_files_expr "$mode" "$@" | run_with_temp_r "$rscript_exe" "$gdb_exe"
    ;;
  script)
    shift
    [ $# -ge 1 ] || {
      echo "missing R script path" >&2
      usage >&2
      exit 1
    }
    run_gdb_file "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")" "$rscript_exe" "$gdb_exe"
    ;;
  expr)
    shift
    [ $# -ge 1 ] || {
      echo "missing R expression" >&2
      usage >&2
      exit 1
    }
    printf '%s\n' "$*" | run_with_temp_r "$rscript_exe" "$gdb_exe"
    ;;
  *)
    echo "unknown mode: $mode" >&2
    usage >&2
    exit 1
    ;;
esac
