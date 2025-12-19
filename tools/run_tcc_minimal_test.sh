#!/bin/sh
# Run after configure: build and test minimal TCC JIT example
# Usage: sh run_tcc_minimal_test.sh
set -e
cd "$(dirname "$0")"

# Path to TCC binary and include/lib paths
TCC_BIN="../inst/tinycc/bin/tcc"
TCC_INC="../inst/tinycc/include"
TCC_LIB="../inst/tinycc/lib"

# Build the minimal test
"$TCC_BIN" -I"$TCC_INC" -L"$TCC_LIB" -ltcc -o tcc_minimal_test tcc_minimal_test.c

# Set library path for both Linux and macOS
export LD_LIBRARY_PATH="$TCC_LIB${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
export DYLD_LIBRARY_PATH="$TCC_LIB${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
./tcc_minimal_test
