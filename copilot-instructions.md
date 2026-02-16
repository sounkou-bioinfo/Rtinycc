# Rtinycc Agent Guidelines (Short)

This file is the minimum guidance for agentic work on Rtinycc. Read
relevant code before changes, keep edits small and focused, and update
tests/docs when you change behavior.

Agents must read actual implementations before guessing. If you need
third-party code or headers, check the `.sync` directory for local
copies of libraries we depend on so you can read them directly instead
of assuming behavior. We also generate `<package>.llm.txt` files for R
package docs; for example, treesitter.c docs can be found in
`treesitter.c.llm.txt`.

## What this package does

Rtinycc embeds TinyCC and exposes both CLI and libtcc APIs to R. It
provides a Bun-style FFI that generates SEXP wrappers at runtime,
compiles them with TinyCC, and calls them via `.Call`.

## FFI provenance (critical)

[`tcc_get_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_get_symbol.md)
calls `RC_libtcc_get_symbol()`, which converts the raw `void*` from
TinyCC into a `DL_FUNC` and wraps it with `R_MakeExternalPtrFn` (tagged
as a native symbol). `make_callable()` then passes that external pointer
to `.RtinyccCall` (which is
[`base::.Call`](https://rdrr.io/r/base/CallExternal.html)). There is no
`Rf_install` cast of the function pointer; `Rf_install` only sets the
native symbol tag.

## Type system highlights

Scalar types: `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool`,
`cstring`.

Array arguments (zero-copy): `raw`, `integer_array`, `numeric_array`,
`logical_array`, `character_array`.

Array returns:
`returns = list(type = "*_array", length_arg = <index>, free = TRUE)`
copies into a new R vector and optionally frees the C buffer.

Struct array fields: `list(type = "u8", size = N, array = TRUE)`
generates element accessors `struct_<name>_get_<field>_elt()` /
`struct_<name>_set_<field>_elt()`.

Pointers: `ptr` and `sexp` are exposed as external pointers; ownership
is tracked and enforced in `RC_free` and
[`tcc_ptr_is_owned()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ptr_is_owned.md).

Variadics (current API):

- Legacy typed-tail mode: `variadic = TRUE`, `varargs = list(...)`,
  optional `varargs_min` (defaults to exact tail).
- True variadic mode: `variadic = TRUE`, `varargs_types = list(...)`,
  `varargs_min`, `varargs_max`.
- In true variadic mode, generated wrappers cover allowed
  arities/signatures and runtime dispatch infers scalar tail types.
- `varargs` and `varargs_types` are mutually exclusive in one symbol
  spec.

## Build and test

- `make rd` regenerates roxygen output.
- `make test` runs tinytest.
- For a single file:
  `R -e "tinytest::run_test_file('inst/tinytest/<file>.R')"`.
- Development plumbing: run `air format` after edits, use `make install`
  for local installs, `make check` for CRAN-style checks, and `make rdm`
  to regenerate README. Always add or update tinytests for behavior
  changes.

## Editing rules

- Use roxygen2 for exported functions and run `make rd` when docs
  change.
- Keep tidyverse-style formatting (2-space indent, spaces around
  operators).
- Prefer early returns and explicit error messages with
  `stop(..., call. = FALSE)`.
- Update README examples when you change user-facing behavior.

## Lambda.r guard patterns (codegen and callbacks)

We prefer lambda.r guard rules over long `if`/`else` or `switch` chains
for type mapping and codegen decisions. Put guards in
`R/aaa_ffi_codegen_rules.R` and route normalization helpers to rule
dispatch. This keeps behavior discoverable, testable, and easy to extend
without touching multiple call sites.

Motivation: codegen logic changes frequently, and rule dispatch makes it
possible to add or override behavior without editing large control
blocks.

## macOS host symbol visibility

On macOS the configure script strips `-flat_namespace` from TCC’s
Makefile to avoid SIGEV-related issues. Without flat namespace, TCC
cannot resolve symbols exported by the R package (`RC_free_finalizer`,
`RC_invoke_callback`, `RC_invoke_callback_id`,
`RC_callback_async_schedule_c`) through the dynamic linker at relocation
time, causing “undefined symbol” errors.

The fix is `RC_libtcc_add_host_symbols()` in `src/RC_libtcc.c`, which
explicitly registers these host symbols via
[`tcc_add_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_symbol.md)
before
[`tcc_relocate()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_relocate.md).
This is called from `R/ffi.R` at both compilation call sites. Any new C
function that generated TCC code references must be added to
`RC_libtcc_add_host_symbols()` or it will break on macOS and Windows.

## Windows support

Windows support required solving several interacting problems. This
section documents every workaround so that future agents don’t undo or
duplicate them.

### Build system (`configure.win`, `Makevars.win`, `install.libs.R`)

`configure.win` unpacks the vendored TCC tarball, builds `libtcc.dll`,
`libtcc.a`, `tcc.exe`, and `libtcc1.a` using the Rtools MinGW toolchain,
then installs them to `inst/tinycc/`. After the build it generates
`.def` files for R and the UCRT (see below) and cleans up the source
tree.

`Makevars.win` dynamically links `Rtinycc.dll` against `libtcc.dll` (not
the static `.a`). This keeps symbols like `_exit` and `abort` out of
`Rtinycc.dll` — they stay in `libtcc.dll`, avoiding R CMD check NOTEs
about entry points that could terminate R.

`install.libs.R` copies `libtcc.dll` next to `Rtinycc.dll` inside the
installed package. Windows has no rpath; the PE loader searches the
directory containing the loading DLL first, so this is the Windows
equivalent.

### Header separation

TCC ships its own `stdlib.h`, `stdio.h`, `math.h`, etc. in `include/`.
These conflict with the Rtools/MinGW headers used to compile
`Rtinycc.dll` itself. To avoid the conflict, `libtcc.h` is installed to
a separate `inst/tinycc/libtcc/` directory, and `Makevars.win` uses
`-I$(TINYCC_DIR)/libtcc` (not `-I$(TINYCC_DIR)/include`). TCC’s system
headers still go to `inst/tinycc/include/` for JIT runtime use only.

### CRT heap mismatch — ucrt.def replaces msvcrt.def

R 4.2+ links against the UCRT (`ucrtbase.dll`). TCC, by default, links
JIT code against `msvcrt.dll` (it auto-links “msvcrt” in `tccpe.c`).
When JIT code calls `calloc()` from msvcrt and the host calls `free()`
from UCRT (as `RC_free_finalizer` does), the heap mismatch causes a
crash.

The fix in `configure.win`: generate a `.def` file from `ucrtbase.dll`
using `tcc -impdef`, but name the output `msvcrt.def`. This overwrites
TCC’s original `msvcrt.def`. Since TCC’s PE linker loads “msvcrt”
automatically, all CRT symbols (`calloc`, `free`, `printf`, etc.) now
resolve from ucrtbase.dll — the same CRT that R and `Rtinycc.dll` use.

### Externalptr ownership and finalizers (Windows crash class)

We observed Windows-only segfaults after running
`tinytest::test_package(...)` in a single R session, especially after
[`gc()`](https://rdrr.io/r/base/gc.html) or during process exit. This
was **not** caused by UCRT mismatches; it was caused by **multiple
externalptr wrappers owning the same `TCCState*`**, leading to
double-finalization when the GC runs. External pointers are not copied
by R, but it is easy to accidentally *create multiple externalptrs
around the same C pointer* in C/R helpers.

Fix pattern:

- Track ownership of `TCCState*` explicitly in C.
- Only one externalptr is “owned” and gets `RC_tcc_finalizer`.
- All other wrappers are “borrowed” and get a no-op finalizer.
- Borrowed wrappers keep the owner alive via the externalptr `prot`
  field.

Implementation lives in `src/RC_libtcc.c`: `tcc_state_entry_t` registry,
`RC_tcc_state_is_owned()`, and ownership tags `rtinycc_tcc_state_owned`
/ `rtinycc_tcc_state_borrowed`.

Do **not** reintroduce shutdown/unload hooks to “solve” this; the
correct fix is ownership tracking and preventing double-finalization.

For debugging GC/finalizer ordering issues, weak references were used to
track externalptr reachability and confirm duplicate
ownership/double-finalization paths during Windows segfault
investigations.

### R API symbol resolution — R.def

TCC JIT code calls R API functions (`Rf_ScalarInteger`, `Rf_error`,
`R_NilValue`, etc.). On Windows these live in `R.dll`. `configure.win`
generates `R.def` from `R.dll` via `tcc -impdef` and places it in
`inst/tinycc/lib/`. At compile time, `R/ffi.R` calls
`tcc_add_library(state, "R")` on Windows, which makes TCC load `R.def`
and resolve R API symbols through `R.dll`.

### Host symbol injection (same as macOS)

On Windows (and macOS), TCC cannot find package-internal C functions
(`RC_free_finalizer`, `RC_invoke_callback_id`,
`RC_callback_async_schedule_c`) through the dynamic linker.
`RC_libtcc_add_host_symbols()` registers them via
[`tcc_add_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_symbol.md)
with direct function pointers before
[`tcc_relocate()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_relocate.md).
These functions are NOT in `init.c` / not exported via `.Call` — they
are only called by TCC-generated JIT code.

### snprintf is not exported from ucrtbase.dll

In UCRT, `snprintf` is an inline function in the headers that calls
`__stdio_common_vsprintf`. It is not a direct DLL export, so TCC’s JIT
linker can’t find it. The original callback trampoline codegen used
`snprintf` to convert the callback token id (an int) to a string, then
called `RC_invoke_callback(SEXP string, SEXP args)` which did `atoi` to
get the int back — a pointless round-trip.

The fix: `RC_invoke_callback_id(int id, SEXP args)` in `RC_libtcc.c`
takes the int directly. The trampoline codegen in `R/callbacks.R` now
calls `RC_invoke_callback_id(tok->id, args)` instead, eliminating the
`snprintf`, `mkString`, and `atoi` round-trip entirely. The forward
declaration in `R/ffi_codegen.R` was updated to match.

### Async callbacks on Windows

Async callbacks are supported on Windows via a thread-safe queue in
`src/platform_async.c`. Scheduling from worker threads is supported, and
queued callbacks execute on the main R thread when
[`tcc_callback_async_drain()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_async_drain.md)
is called. Unlike Unix builds, Windows does not use
[`pipe()`](https://rdrr.io/r/base/connections.html) +
`addInputHandler()` for dispatch.

Tests in `test_callback_invoke_runtime.R` are cross-platform;
worker-thread callback tests use Win32 threads on Windows and `pthread`
on Unix-like systems.

### No libm on Windows

On Unix, math functions live in `libm.so` and require `-lm`. On Windows,
they are part of the CRT (ucrtbase.dll). Any tests using `tcc_link("m")`
or `tcc_library("m")` must be guarded with
`if (.Platform$OS.type != "windows")`.

### No fork on Windows

R’s [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html)
uses `fork()` which does not exist on Windows. Fork-related tests in
`test_fork_serialize.R` are guarded with
`if (.Platform$OS.type != "windows")`.

### Windows-specific files

- `configure.win`: Build TCC from source, generate R.def and ucrt.def
- `src/Makevars.win`: Dynamic linking against libtcc.dll
- `src/install.libs.R`: Copy libtcc.dll next to Rtinycc.dll
- `.github/workflows/windows.yml`: Windows CI

## Key files

- R API: `R/ffi.R`, `R/ffi_types.R`, `R/ffi_codegen.R`, `R/tinycc.R`
- C API: `src/RC_libtcc.c`, `src/init.c`
- Tests: `inst/tinytest/`
- Docs: `README.Rmd`, `NEWS.md`

## tinytest workflow

Use tinytest as the source of truth for behavior checks. When changing
runtime behavior, add or update targeted tests under `inst/tinytest/` in
the same commit. Prefer focused tests that assert one behavior each and
include Windows guards when features are OS-specific.

Typical commands:

- `make test` for the full tinytest suite.
- `R -e "tinytest::run_test_file('inst/tinytest/<file>.R')"` for a
  single test file while iterating.

## README.Rmd workflow

`README.Rmd` is the canonical source for the generated README. Any
user-facing API or behavior change should update the relevant
examples/text in `README.Rmd` first, then regenerate derived docs with
`make rdm`. Do not edit generated README output without also updating
`README.Rmd`.

## README and DOCS Style

- We use a sober style, make paragraphs and avoid ridiculous list/bullet
  points when uncessary.
- We are using Rmarkdown so the user can see the output, so no need for
  ridiculous cats and other things like that.
