# Rtinycc Agent Guidelines (Short)

This file is the minimum guidance for agentic work on Rtinycc. Read relevant code before changes, keep edits small and focused, and update tests/docs when you change behavior.

Agents must read actual implementations before guessing. If you need third-party code or headers, check the `.sync` directory for local copies of libraries we depend on so you can read them directly instead of assuming behavior. We also generate `<package>.llm.txt` files for R package docs; for example, treesitter.c docs can be found in `treesitter.c.llm.txt`.

## What this package does

Rtinycc embeds TinyCC and exposes both CLI and libtcc APIs to R. It provides a Bun-style FFI that generates SEXP wrappers at runtime, compiles them with TinyCC, and calls them via `.Call`.

## FFI provenance (critical)

`tcc_get_symbol()` calls `RC_libtcc_get_symbol()`, which converts the raw `void*` from TinyCC into a `DL_FUNC` and wraps it with `R_MakeExternalPtrFn` (tagged as a native symbol). `make_callable()` then passes that external pointer to `.RtinyccCall` (which is `base::.Call`). There is no `Rf_install` cast of the function pointer; `Rf_install` only sets the native symbol tag.

## Type system highlights

Scalar types: `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool`, `cstring`.

Array arguments (zero-copy): `raw`, `integer_array`, `numeric_array`, `logical_array`, `character_array`.

Array returns: `returns = list(type = "*_array", length_arg = <index>, free = TRUE)` copies into a new R vector and optionally frees the C buffer.

Struct array fields: `list(type = "u8", size = N, array = TRUE)` generates element accessors `struct_<name>_get_<field>_elt()` / `struct_<name>_set_<field>_elt()`.

Pointers: `ptr` and `sexp` are exposed as external pointers; ownership is tracked and enforced in `RC_free` and `tcc_ptr_is_owned()`.

## Build and test

- Always run `make rd` after edits (not only when docs appear to change).
- `make test` runs tinytest.
- For a single file: `R -e "tinytest::run_test_file('inst/tinytest/<file>.R')"`.
- If the package is not installed locally, load it for interactive/dev checks with `devtools::load_all()`.
- Development plumbing: run `air format` after edits, use `make install` for local installs, `make check` for CRAN-style checks, and `make rdm` to regenerate README. Always add or update tinytests for behavior changes.

## Editing rules

- Use roxygen2 for exported functions and always run `make rd` after edits.
- Keep tidyverse-style formatting (2-space indent, spaces around operators).
- Prefer early returns and explicit error messages with `stop(..., call. = FALSE)`.
- Update README examples when you change user-facing behavior.

## macOS host symbol visibility

On macOS the configure script strips `-flat_namespace` from TCC's Makefile to avoid SIGEV-related issues. Without flat namespace, TCC cannot resolve symbols exported by the R package (`RC_free_finalizer`, `RC_invoke_callback`, `RC_invoke_callback_id`, `RC_callback_async_schedule_c`) through the dynamic linker at relocation time, causing "undefined symbol" errors.

The fix is `RC_libtcc_add_host_symbols()` in `src/RC_libtcc.c`, which explicitly registers these host symbols via `tcc_add_symbol()` before `tcc_relocate()`. This is called from `R/ffi.R` at both compilation call sites. Any new C function that generated TCC code references must be added to `RC_libtcc_add_host_symbols()` or it will break on macOS and Windows.

## Windows support

Windows support required solving several interacting problems. This section documents every workaround so that future agents don't undo or duplicate them.

### Build system (`configure.win`, `Makevars.win`, `install.libs.R`)

`configure.win` unpacks the vendored TCC tarball, builds `libtcc.dll`, `libtcc.a`, `tcc.exe`, and `libtcc1.a` using the Rtools MinGW toolchain, then installs them to `inst/tinycc/`. After the build it generates `.def` files for R and the UCRT (see below) and cleans up the source tree.

`Makevars.win` dynamically links `Rtinycc.dll` against `libtcc.dll` (not the static `.a`). This keeps symbols like `_exit` and `abort` out of `Rtinycc.dll` — they stay in `libtcc.dll`, avoiding R CMD check NOTEs about entry points that could terminate R.

`install.libs.R` copies `libtcc.dll` next to `Rtinycc.dll` inside the installed package. Windows has no rpath; the PE loader searches the directory containing the loading DLL first, so this is the Windows equivalent.

### Header separation

TCC ships its own `stdlib.h`, `stdio.h`, `math.h`, etc. in `include/`. These conflict with the Rtools/MinGW headers used to compile `Rtinycc.dll` itself. To avoid the conflict, `libtcc.h` is installed to a separate `inst/tinycc/libtcc/` directory, and `Makevars.win` uses `-I$(TINYCC_DIR)/libtcc` (not `-I$(TINYCC_DIR)/include`). TCC's system headers still go to `inst/tinycc/include/` for JIT runtime use only.

### CRT heap mismatch — ucrt.def replaces msvcrt.def

R 4.2+ links against the UCRT (`ucrtbase.dll`). TCC, by default, links JIT code against `msvcrt.dll` (it auto-links "msvcrt" in `tccpe.c`). When JIT code calls `calloc()` from msvcrt and the host calls `free()` from UCRT (as `RC_free_finalizer` does), the heap mismatch causes a crash.

The fix in `configure.win`: generate a `.def` file from `ucrtbase.dll` using `tcc -impdef`, but name the output `msvcrt.def`. This overwrites TCC's original `msvcrt.def`. Since TCC's PE linker loads "msvcrt" automatically, all CRT symbols (`calloc`, `free`, `printf`, etc.) now resolve from ucrtbase.dll — the same CRT that R and `Rtinycc.dll` use.

### Externalptr ownership and finalizers (Windows crash class)

We observed Windows-only segfaults after running `tinytest::test_package(...)`
in a single R session, especially after `gc()` or during process exit. This
was **not** caused by UCRT mismatches; it was caused by **multiple externalptr
wrappers owning the same `TCCState*`**, leading to double-finalization when the
GC runs. External pointers are not copied by R, but it is easy to accidentally
*create multiple externalptrs around the same C pointer* in C/R helpers.

Fix pattern:

- Track ownership of `TCCState*` explicitly in C.
- Only one externalptr is “owned” and gets `RC_tcc_finalizer`.
- All other wrappers are “borrowed” and get a no-op finalizer.
- Borrowed wrappers keep the owner alive via the externalptr `prot` field.

Implementation lives in `src/RC_libtcc.c`:
`tcc_state_entry_t` registry, `RC_tcc_state_is_owned()`, and ownership tags
`rtinycc_tcc_state_owned` / `rtinycc_tcc_state_borrowed`.

Do **not** reintroduce shutdown/unload hooks to “solve” this; the correct fix
is ownership tracking and preventing double-finalization.

Current checkout note: we do **not** rely on a weakref-based `tcc_state`
ownership model in the active code path. Lifetime safety is provided by
owned/borrowed externalptr tracking in C and by retaining `state` in callable
closure environments in R.

### R API symbol resolution — R.def

TCC JIT code calls R API functions (`Rf_ScalarInteger`, `Rf_error`, `R_NilValue`, etc.). On Windows these live in `R.dll`. `configure.win` generates `R.def` from `R.dll` via `tcc -impdef` and places it in `inst/tinycc/lib/`. At compile time, `R/ffi.R` calls `tcc_add_library(state, "R")` on Windows, which makes TCC load `R.def` and resolve R API symbols through `R.dll`.

### Host symbol injection (same as macOS)

On Windows (and macOS), TCC cannot find package-internal C functions (`RC_free_finalizer`, `RC_invoke_callback_id`, `RC_callback_async_schedule_c`) through the dynamic linker. `RC_libtcc_add_host_symbols()` registers them via `tcc_add_symbol()` with direct function pointers before `tcc_relocate()`. These functions are NOT in `init.c` / not exported via `.Call` — they are only called by TCC-generated JIT code.

### snprintf is not exported from ucrtbase.dll

In UCRT, `snprintf` is an inline function in the headers that calls `__stdio_common_vsprintf`. It is not a direct DLL export, so TCC's JIT linker can't find it. The original callback trampoline codegen used `snprintf` to convert the callback token id (an int) to a string, then called `RC_invoke_callback(SEXP string, SEXP args)` which did `atoi` to get the int back — a pointless round-trip.

The fix: `RC_invoke_callback_id(int id, SEXP args)` in `RC_libtcc.c` takes the int directly. The trampoline codegen in `R/callbacks.R` now calls `RC_invoke_callback_id(tok->id, args)` instead, eliminating the `snprintf`, `mkString`, and `atoi` round-trip entirely. The forward declaration in `R/ffi_codegen.R` was updated to match.

### Async callbacks: Windows stubs

Async callbacks rely on `pipe()`, `pthread_create`, and R's `addInputHandler()` — none of which exist on Windows. `RC_libtcc.c` has `#ifdef _WIN32` stubs that call `Rf_error("Async callbacks are not supported on Windows")` for the R-facing functions, and return `-1` for the C-facing `RC_callback_async_schedule_c`. The async callback tests in `test_callback_invoke_runtime.R` are guarded with `if (.Platform$OS.type != "windows")`.

### No libm on Windows

On Unix, math functions live in `libm.so` and require `-lm`. On Windows, they are part of the CRT (ucrtbase.dll). Any tests using `tcc_link("m")` or `tcc_library("m")` must be guarded with `if (.Platform$OS.type != "windows")`.

### No fork on Windows

R's `parallel::mclapply` uses `fork()` which does not exist on Windows. Fork-related tests in `test_fork_serialize.R` are guarded with `if (.Platform$OS.type != "windows")`.

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

## Roadmap: declare-based R->C JIT (quickr-like)

This is a planned extension: compile a strict subset of R functions to C via TinyCC, with optional fallback to R C API calls when an operation is not yet lowered directly.

### Goal and positioning

We do not target full R semantics. We target a high-value subset for numeric code with explicit `declare(type(...))` contracts, then JIT to `.Call` wrappers through existing Rtinycc infrastructure.

Primary advantage over prior art in `.sync/quickr` and `.sync/r2c`: we already own the JIT + host-symbol pipeline (`tcc_compile_string()`, `tcc_relocate()`, `tcc_get_symbol()`, `make_callable()`), so we can interleave:

1. direct lowered C for hot arithmetic/loops,
2. selective fallback to C API calls (`Rf_install`, `Rf_lang*`, `Rf_eval`) for unsupported nodes,
3. transparent caching/recompile behavior already used by `tcc_compiled`.

### MVP subset (phase 1)

Only compile functions that satisfy all constraints below:

- starts with `declare(type(...))` for all arguments;
- scalar/vector `integer`, `double`, `logical` only;
- ops: `+ - * /`, comparisons, simple `if`, `for (i in seq_len(n))`, `length()`;
- indexing: `x[i]` and assignment `x[i] <- expr`;
- return atomic vector or scalar;
- no promises, NSE, S3/S4 dispatch, environments, or ALTREP-sensitive behavior.

If any AST node is outside subset, emit a fallback stub in generated C that evaluates the original call via `Rf_lang*` + `Rf_eval`.

### Compiler pipeline

Implement a staged pipeline in R:

1. `capture`: obtain function AST and declaration metadata.
2. `normalize`: canonicalize loops and indexing forms.
3. `typecheck`: infer/check static type + shape against `declare()`.
4. `lower`: convert AST to a compact typed IR.
5. `codegen`: emit C from IR, plus fallback branches.
6. `compile`: compile with current `tcc_ffi()` path.
7. `cache`: hash body + decls + platform and reuse compiled callable.

### Bytecode-assisted lowering (optional)

Using R bytecode as an intermediate can help, but should remain optional in phase 1.

Why it is attractive:

- normalizes many surface-level AST forms;
- gives stable control-flow structure (`GOTO`, `BRIFNOT`, etc.);
- mirrors prior compiler work in `r-svn` and PRL-PRG projects.

Why we should not make it mandatory initially:

- R bytecode instructions are numerous and some dispatch back to the AST interpreter;
- bytecode has stack-machine semantics and constant-pool indirection, which is harder to map directly to typed C loops than a small custom IR;
- reproducing exact bytecode behavior is a separate goal from fast subset lowering.

Recommended hybrid strategy:

1. Keep AST + `declare()` as the primary lowering input for MVP.
2. Add a debug mode that emits/disassembles bytecode (`compiler`/`rbytecode`) for diagnostics and parity checks.
3. Optionally lower a restricted bytecode opcode subset (`LDCONST`, `GETVAR`, arithmetic ops, simple branches/loops) into `tcc_quick_ir` when it is cleaner than AST lowering.
4. For unknown opcodes, fallback to generated `Rf_lang*` + `Rf_eval` calls rather than failing hard.

This gives the maintainability benefits of a small typed IR while still leveraging bytecode information where useful.

Suggested new files:

- `R/tcc_quick.R` (public user API, e.g. `tcc_quick()`)
- `R/tcc_quick_declare.R` (parse `declare(type(...))`)
- `R/tcc_quick_ir.R` (IR node constructors and validators)
- `R/tcc_quick_lower.R` (AST -> IR)
- `R/tcc_quick_codegen.R` (IR -> C source)
- `R/tcc_quick_cache.R` (digest key + memo table)
- `inst/tinytest/test_tcc_quick_basic.R`

### API shape

Expose a narrow API first:

```r
tcc_quick <- function(fn, fallback = c("auto", "always", "never"), debug = FALSE) {
	fallback <- match.arg(fallback)
	# parse declare(), lower subset, generate C, compile to callable
	# return R closure with same formal args as fn
}
```

and internal compile entrypoint:

```r
tcc_quick_compile <- function(fn, decl, fallback, debug = FALSE) {
	ir <- tcc_quick_lower(fn, decl)
	c_src <- tcc_quick_codegen(ir, fallback = fallback)
	# compile via existing tcc_ffi/tcc_source/tcc_bind/tcc_compile
}
```

### Fallback strategy (critical)

Fallback must happen inside generated C, not by bouncing back to R wrappers repeatedly.

Policy: when lowering encounters `.Internal`, `.External`, or `.Primitive`-style nodes, prefer emitting `Rf_lang*` + `Rf_eval` calls (e.g. `Rf_lang3`) so R's own primitive/internal implementation executes, rather than re-implementing those semantics in generated C.

Pattern:

```c
SEXP tcc_quick_fallback_call(SEXP rho, SEXP sym, SEXP a, SEXP b) {
	SEXP call = PROTECT(Rf_lang3(sym, a, b));
	SEXP out  = Rf_eval(call, rho);
	UNPROTECT(1);
	return out;
}
```

Use this only for unsupported subexpressions. Keep hot loops in direct C.

When new C helper functions are emitted and referenced by TCC-generated code, add host symbol registration in `RC_libtcc_add_host_symbols()` (macOS/Windows requirement).

### R API symbol policy using `.sync/API.csv`

Use `.sync/API.csv` as an allowlist source for C API symbols used by codegen.

- Build a small parser in `tools/` that extracts permitted function names.
- Reject codegen that emits non-allowlisted API symbols unless explicitly approved.
- Prefer stable wrappers (`Rf_*`) in emitted code where available.

This reduces accidental dependency on internal/unstable entry points.

### IR sketch

Keep IR minimal and typed:

```r
list(
	tag = "for",
	var = "i",
	start = list(tag = "const_i32", value = 1L),
	stop = list(tag = "len", x = "a"),
	body = list(
		list(tag = "assign_index",
				 target = "ab",
				 index = list(tag = "sub", a = "i", b = 1L),
				 value = list(tag = "add", a = list(tag = "index", x = "ab", i = "i"), b = "tmp"))
	)
)
```

IR must carry `ctype`, `r_sexp_type`, and length metadata where known.

### Generated C style

Generated C should:

- be explicit with `PROTECT`/`UNPROTECT` counts;
- coerce once at function boundary when needed;
- hoist `LENGTH()` outside loops;
- use raw pointers (`REAL()`, `INTEGER()`, `LOGICAL()`) in hot loops;
- emit informative `Rf_error` messages for shape/type mismatch.

### Milestones

1. Parse declarations + validate AST eligibility.
2. Lower arithmetic/index loops to IR.
3. Emit C for scalar + double vector kernels.
4. Add selective fallback nodes with `Rf_lang*`/`Rf_eval`.
5. Add caching and invalidation.
6. Add docs/demo in README and tinytests for parity vs source R.

### Test requirements

Add tinytests for:

- parity on representative kernels (sum/product/convolution-like loop);
- fallback activation on unsupported calls;
- error messages for missing or inconsistent `declare()`;
- serialization/recompile behavior of compiled quick functions;
- OS guards for platform-specific library behavior.

### Benchmark requirements

Use `bench::mark()` with warmup and report:

- baseline R function,
- `tcc_quick()` compiled function,
- existing `quickr::quick()` when available (optional comparison),
- optional hand-written `.Call` reference.

Keep benchmark scripts under `tools/` and avoid heavy runtime in CRAN checks.

## README and DOCS Style

- We use a sober style, make paragraphs and avoid ridiculous list/bullet points when uncessary. 
- We are using Rmarkdown so the user can see the output, so no need for ridiculous cats and other things like that.
