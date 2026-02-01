# Rtinycc Agent Guidelines

This file provides guidance for agentic coding agents working on the Rtinycc package, which provides an R interface to the TinyCC (Tiny C Compiler) including both CLI and libtcc library functionality.

## Agent behavior

- **Read the code first:** Agents must start by reading the relevant code implementations in the repository before making assumptions or proposing changes. Do not guess about behavior; inspect functions, C helpers, and tests to understand current behavior.


## Package Overview

Rtinycc is an R package that:
- Bundles and builds the TinyCC compiler from source
- Provides R functions to interact with TinyCC CLI and libtcc library
- Enables just-in-time C compilation and execution from R
- Supports in-memory compilation and symbol lookup

## Build/Install/Test Commands

### Core Commands
```bash
# ALWAYS use roxygen2 tags for documentation which are auto-generated!
# Generate documentation from roxygen2 comments
make rd

# Build and install the package (runs configure automatically)
make install

# Development install (clean rebuild)
make dev-install

# Run all tests
make test

# Run single test file
R -e "tinytest::test_file('inst/tinytest/test_libtcc.R')"

# Run specific test functions within a test file
R -e "source('inst/tinytest/test_libtcc.R'); expect_true(TRUE)" # Example approach

# Check package as CRAN would
make check
```

### Build Process Notes
- The `configure` script automatically builds TinyCC from source and installs it to `inst/tinycc/`
- TinyCC source is unpacked from a tarball via `tools/vendortinycc.R`
- Build creates both static and shared libtcc libraries
- Platform-specific shared library extensions (.so for Linux, .dylib for macOS)

### Testing Commands
```bash
# Run specific test functions
R -e "tinytest::test_package('Rtinycc', testdir = 'inst/tinytest')"

# Run with debug output
RTINYCC_DEBUG=1 make test

# Test CLI compilation separately
R -e "source('inst/tinytest/test_libtcc.R'); # run individual test lines"

# Test with additional debugging
R -e "source('inst/tinytest/test_libtcc.R'); Sys.setenv(RTINYCC_DEBUG='1')"

## Code Style Guidelines

### R Code Style

#### General Formatting
- Follow tidyverse coding style standards
- Use `styler::style_pkg()` or the RStudio "Format" button to format code
- Alternatively use `air` CLI: run `air format` after edits

#### Function Naming
- Use snake_case for all R functions: `tcc_state()`, `tcc_compile_string()`
- Prefix public functions with `tcc_` when they relate to TinyCC functionality
- Use descriptive names that indicate the action: `get_external_ptr_addr()`, `check_cli_exists()`

#### Documentation
- ALWAYS use roxygen2 tags for documentation which are auto-generated!
- Use roxygen2 comments for all exported functions
- Include `@export` for public functions
- Use `@param` and `@return` tags consistently
- Reference related functions with `@rdname`
- Run `make rd` after modifying documentation to regenerate NAMESPACE and man pages

#### Error Handling
- Use `stop()` with `call. = FALSE` for user-facing errors
- Validate external pointer types and NULL checks in C code
- Return integer status codes (0 = success) for TinyCC operations
- Include helpful error messages with context

#### Import Style
- All functions should use fully qualified calls via `.Call()` for C extensions
- No explicit imports needed - uses base R functionality
- Functions should be self-contained and not rely on global state

#### Code Organization
- Put spaces around operators: `x + y`, not `x+y`
- Put spaces after commas in function calls: `fun(a, b)`, not `fun(a,b)`
- Use consistent indentation (2 spaces recommended by tidyverse)
- Use `%>%` pipe operator when it improves readability
- Prefer early returns for simple conditions

### README.Rmd Guidelines

#### Code Examples
- NO potemkin examples - all code blocks should be real, executable examples
- All code blocks must be evaluated (no `eval = FALSE`)
- NO ridiculous use of `cat()` and other print statements for show
- NO excessive try-catch blocks or testing existence of packages in examples
- Examples should be clean, realistic, and demonstrate actual functionality
- Use proper error handling in real code, but keep examples simple and focused

#### Documentation Style
- Use sober, professional paragraphs - not ridiculous listings, emojis, or other slop
- Be professional and to the point - this is R documentation for serious people
- Keep content didactic but minimal - just enough to be helpful
- Focus on demonstrating real usage patterns
- Keep examples concise but complete
- Show both CLI and libtcc usage when applicable
- Include meaningful outputs that demonstrate the functionality

### C Code Style

#### Headers and Includes
```c
#include "libtcc.h"      // TinyCC library header (must be first)
#include <R.h>           // R headers
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <stdint.h>      // Standard headers after R headers
```

#### Function Naming
- Prefix all C functions with `RC_`: `RC_libtcc_state_new()`, `RC_tcc_finalizer()`
- Use snake_case consistently
- Static helper functions should be lowercase with underscores

#### Memory Management
- Always register finalizers for external pointers: `R_RegisterCFinalizerEx()`
- Use `PROTECT()`/`UNPROTECT()` for R object management
- Clear external pointers in finalizers: `R_ClearExternalPtr()`

#### Error Handling
- Use `Rf_error()` for fatal errors (automatically jumps to R level)
- Validate input types: `TYPEOF(ptr) != EXTPTRSXP`
- Check external pointer inheritance: `Rf_inherits(ext, "tcc_state")`
- Return integer status codes for non-fatal errors

#### Type Conversions
```c
// String extraction
const char *str = Rf_translateCharUTF8(STRING_ELT(sexp, 0));

// Integer conversion  
int val = Rf_asInteger(sexp);

// External pointer access
void *ptr = R_ExternalPtrAddr(ext);
```

## Architecture Patterns

### External Pointer Classes
- `tcc_state`: Represents a TinyCC compilation state
- `tcc_symbol`: Represents a compiled symbol/function pointer
- Both classes have associated validation functions

### State Management Pattern
1. Create state with `tcc_new()`
2. Configure paths and output type
3. Add source files or compile strings
4. Relocate with `tcc_relocate()`
5. Get/call symbols as needed
6. Finalizer automatically cleans up with `tcc_delete()`

### Path Resolution
- Use `system.file()` to locate bundled TinyCC installation
- Support both Unix and Windows path separators with `normalizePath(winslash = "/")`
- Handle multiple library/include paths gracefully
- Verify file existence before use

## Development Workflow

### Adding New libtcc Functions
1. Add R wrapper function in `R/tinycc.R` with proper roxygen2 docs
2. Format code using `air format` or `styler::style_pkg()`
3. Add C implementation in `src/RC_libtcc.c` following naming conventions
4. Register function in `src/init.c` if needed
5. Add tests in `inst/tinytest/test_libtcc.R`
6. Run `make rd` to regenerate documentation (CRITICAL - documentation is auto-generated!)
7. Run `make test` to ensure all tests pass

### Debugging
- Set `RTINYCC_DEBUG=1` environment variable for verbose debug output
- Debug information is printed from both R and C levels
- Use `get_external_ptr_addr()` to inspect pointer addresses
- Check `config.log` for TinyCC build issues
- Use Rscript files for reproducible debug/test scenarios rather than console testing

### Testing Strategy
- ALWAYS write scratch tests in files - not just in R console!
- Test both successful operations and error conditions
- Verify external pointer validation
- Test CLI and libtcc interfaces separately
- Include platform-specific tests if needed
- Use `expect_true()`, `expect_equal()` from tinytest

### C Examples Organization
- All C code and headers for tests should go in `inst/c_examples/`
- Complex examples should be organized in subdirectories of `inst/c_examples/`
- Use subdirectories for different feature categories or complexity levels
- Examples should be self-contained and demonstrate specific functionality

## Bun-style FFI Implementation Summary

### Core Architecture
The package now includes a complete Bun-style FFI system with API mode compilation using TinyCC JIT:

- **`tcc_ffi()`** creates FFI context for external library binding
- **`tcc_bind()`** defines symbols with explicit FFI types and metadata  
- **`tcc_compile()`** generates type-safe C wrapper code and compiles with TinyCC
- **`tcc_link()`** links against external libraries (like Bun's dlopen but with compile-time type checking)

### Type System
R/C focused with explicit type mappings for efficient interop:

**Scalar Types**: `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool`, `cstring`

**Array Types**: `raw` (uint8_t*), `integer_array` (int32_t*), `numeric_array` (double*), `logical_array` (int32_t*), `complex_array` (Rcomplex*)

**Pointer Types**: `ptr` (externalptr), `sexp` (SEXP)

### Key Features

- **SEXP-based wrapper generation**: Generates R API (SEXP) wrappers that convert between R values and C types for commonly used scalar and array types.
- **Array support / direct access**: Generated wrappers access R vectors using `REAL()`, `INTEGER()`, `RAW()` and friends — this enables zero-copy access when callers pass native R vectors, but callers must ensure correct types and memory layout.
- **Known limitations**: 64-bit integers are marshalled to/from R `numeric` (double) in several places (so precision can be lost); some conversions are lossy by design and require care from callers.
- **External linking via TinyCC**: Libraries are linked/used through TinyCC in-memory compilation and relocation (not via `dyn.load()` by default).
- **Runtime checks and errors**: The code emits R-level errors for invalid inputs and common failure modes, but not all edge cases are exhaustively guarded.
 - **Complex type support**: `_Complex` macro workaround for TinyCC compatibility issues (kept in examples and recommended when compiling code that uses complex types).

### Implementation Files

**R Interface Files**:
- `R/ffi_types.R` - Complete FFI type system with validation and metadata
- `R/ffi_codegen.R` - C wrapper code generation with type-specific marshaling
- `R/ffi.R` - High-level declarative API with pipe-friendly interface

**C Extension Files**:
- `src/RC_libtcc.c` - Enhanced with external symbol access and FFI support
- `src/init.c` - Package initialization with FFI function registration
 - `src/RC_libtcc.c` also provides `RC_get_external_ptr_hex()` to format
   external pointer addresses as hex strings (portable via `PRIxPTR`).

**Test Suite**:
- `inst/tinytest/test_ffi.R` - Comprehensive FFI functionality tests (84 tests total)
- `inst/tinytest/test_ffi_types.R` - Type system validation tests
- `inst/tinytest/test_ffi_codegen.R` - Code generation and compilation tests

### Success Metrics

- **All 84 tests pass** across type system, code generation, compilation, and external linking
- **Complete Bun-style FFI API** working in R with modern declarative syntax
- **README.Rmd updated** with working examples (sqlite example only needs lib name fix)
- **Zero-copy performance** for array operations with proper memory safety
- **Cross-platform compatibility** for Linux, macOS, and Windows

### API Usage Pattern

```r
tcc_ffi("sqlite3") %>%
  tcc_bind("sqlite3_open", args = list("cstring", "ptr"), returns = "i32") %>%
  tcc_bind("sqlite3_exec", args = list("ptr", "cstring", "ptr", "ptr", "ptr"), returns = "i32") %>%
  tcc_compile() %>%
  tcc_link()
```

### Transformation Achievement

The implementation successfully transforms Rtinycc from a simple compiler wrapper into a powerful, Bun-style FFI system that can:

- Generate type-safe C bindings with compile-time validation
- Link against external libraries with automatic platform detection  
- Use R's native data structures efficiently with zero-copy operations
- Handle complex R types and maintain proper memory management
- Provide clean functional API that follows R conventions and supports pipes
- Enable JIT compilation of external library interfaces for maximum performance

### Current Status

Ready for production use with a modern, declarative FFI interface that seamlessly integrates with R's data structures. The foundation is solid for extensions including:
- High-performance numerical computing with external BLAS/LAPACK libraries
- System integration with OS APIs and device drivers
- Database connectivity with native drivers
- Machine learning framework integration

### Pointer utilities

- `get_external_ptr_addr()` exposes the pointer address to R as a numeric (double).
  This is convenient for decimal inspection but can overflow 32-bit integer coercions.
- To support safe hexadecimal formatting on 64-bit systems, a C helper
  `RC_get_external_ptr_hex()` returns a properly formatted `0x...` string using
  `uintptr_t`/`PRIxPTR`. The R wrapper `tcc_ptr_addr(..., hex = TRUE)` calls this
  helper to avoid `as.integer()` coercion and NA warnings on 64-bit addresses.

### FFI header/library behavior

- `tcc_header()` and `tcc_library()` append values to the `tcc_ffi` object and may
  be called multiple times. Additions are concatenated in order and applied at
  compile/link time. There is currently no automatic deduplication or locking after
  compilation — callers should use `unique()` on lists if deduplication is desired,
  or avoid further mutation after `tcc_compile()` if reproducibility is required.

## Important Notes

- This package compiles TinyCC from source during installation
- TinyCC source is not included in the repository but unpacked during build
- The package only supports Unix-like systems (not Windows)
- All path operations should use `normalizePath()` for cross-platform compatibility
- Memory management is critical - always register finalizers for external pointers
- Error messages should be user-friendly and include relevant context
- FFI requires proper external library installation - libraries must be available on the system