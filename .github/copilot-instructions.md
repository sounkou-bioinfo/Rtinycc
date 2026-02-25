# Rtinycc Agent Guidelines (Short)

This file is the minimum guidance for agentic work on Rtinycc. Read relevant code before changes, keep edits small and focused, and update tests/docs when you change behavior.

Agents must read actual implementations before guessing. If you need third-party code or headers, check the `.sync` directory for local copies of libraries we depend on so you can read them directly instead of assuming behavior. We also generate `<package>.llm.txt` files for R package docs; for example, treesitter.c docs can be found in `treesitter.c.llm.txt`.

The `.sync` directory also contains reference implementations that inform the `tcc_quick` compiler design: `codetools` (walker/handler), `quickr` (handler registry, scope tracking), `spy` (typed C AST, codegen), and `Stdlib` (the SaC standard library from the SacBase project — array primitives, reductions, transforms, and with-loop patterns that inform our condensation/ψ-reduction architecture).

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

## Maintenance helpers (dev-only)

Use these helper functions in development sessions to regenerate coverage
snapshots and delegated fallback contract tables. Keep them out of user-facing
README content.

```r
rtinycc_file_coverage <- function(path = ".") {
  cov <- covr::package_coverage(path)
  df <- as.data.frame(cov)
  df$key <- paste(
    df$first_line, df$last_line, df$first_column, df$last_column, sep = ":"
  )
  expr <- aggregate(value ~ filename + key, data = df, FUN = max)
  out <- aggregate(
    value ~ filename,
    data = expr,
    FUN = function(x) 100 * mean(x > 0)
  )
  names(out)[2] <- "coverage_pct"
  out[order(out$coverage_pct, out$filename), ]
}

rtinycc_fallback_contracts_table <- function() {
  allow <- sort(unique(Rtinycc:::tcc_quick_rf_call_allowlist()))
  contracts <- Rtinycc:::tcc_quick_rf_call_contracts()
  quiet <- Rtinycc:::tcc_quick_rf_call_quiet()

  data.frame(
    function_name = allow,
    has_contract = allow %in% names(contracts),
    mode = vapply(
      allow,
      function(fn) if (is.null(contracts[[fn]])) NA_character_ else contracts[[fn]]$mode,
      character(1)
    ),
    shape = vapply(
      allow,
      function(fn) if (is.null(contracts[[fn]])) NA_character_ else contracts[[fn]]$shape,
      character(1)
    ),
    quiet = allow %in% quiet,
    stringsAsFactors = FALSE
  )
}
```

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

### `snprintf` and callback dispatch

UCRT does not export `snprintf` directly for TCC JIT linking. Callback dispatch must use `RC_invoke_callback_id(int id, SEXP args)` and pass ids as integers (no string round-trip through `snprintf`/`atoi`).

### Async callbacks (current behavior)

- Async callbacks are supported on both Unix-like systems and Windows.
- Unix uses `pipe()` + `addInputHandler()` wakeups; Windows uses a queue + mutex and explicit drain calls.
- `RC_platform_async_drain()` must be safe before init (no-op when queue is uninitialized).
- `callback_async` signatures may be non-`void`; scheduled calls still return immediately with a type-default value.
- Marshaling rules for async arguments:
- Pointer-like args (including `char **`) must map to `CB_ARG_PTR`.
- Only single-level `char*` / `const char*` map to `CB_ARG_CSTRING`.
- Unsupported wide integer async args must fail at codegen with a clear error (avoid lossy `int` truncation).

### No libm on Windows

On Unix, math functions live in `libm.so` and require `-lm`. On Windows, they are part of the CRT (ucrtbase.dll). Any tests using `tcc_link("m")` or `tcc_library("m")` must be guarded with `if (.Platform$OS.type != "windows")`.

### No fork on Windows

R's `parallel::mclapply` uses `fork()` which does not exist on Windows. Fork-related tests in `test_fork_serialize.R` are guarded with `if (.Platform$OS.type != "windows")`.

### Windows-specific files

- `configure.win`: Build TCC from source, generate R.def and ucrt.def
- `src/Makevars.win`: Dynamic linking against libtcc.dll
- `src/install.libs.R`: Copy libtcc.dll next to Rtinycc.dll

## Key files

- R API: `R/ffi.R`, `R/ffi_types.R`, `R/ffi_codegen.R`, `R/tinycc.R`
- C API: `src/RC_libtcc.c`, `src/init.c`
- tcc_quick pipeline: `R/tcc_quick.R` (API), `R/tcc_quick_declare.R` (declarations), `R/tcc_quick_walk.R` (codetools-style walker), `R/tcc_quick_lower.R` (AST → IR), `R/tcc_quick_codegen.R` (IR → C), `R/tcc_quick_cache.R` (digest cache)
- IR: `R/tcc_ir.R` (node constructors), `R/tcc_ir_registry.R` (R→C function mapping)
- IR design: `docs/ir_design.md` (authoritative design doc, SSA roadmap)
- R2C architecture plan: `docs/r2c_architecture_plan.md` (determinism, fallback policy, R API mapping)
- Tests: `inst/tinytest/`
- Docs: `README.Rmd`, `NEWS.md`
- Lowering audit and reuse notes: `docs/lowering_audit_notes.md`

## Roadmap: declare-based R→C JIT (`tcc_quick`)

`tcc_quick()` compiles a `declare()`-annotated R subset to C via TinyCC, with optional fallback to R C API calls when an operation is not yet lowered directly. See `docs/ir_design.md` for the full IR design and SSA roadmap.

### Architecture

The compiler pipeline draws on four reference implementations in `.sync/`:

- **codetools**: generic `walkCode`/`makeCodeWalker` dispatcher — adopted in `R/tcc_quick_walk.R`.
- **quickr**: handler registry environment, scope/variable tracking — adopted in `R/tcc_ir_registry.R` and lowering dispatch.
- **spy**: typed C AST with precedence, `emit()` pattern — informs codegen style.
- **SaC Stdlib** (SacBase/Stdlib): the standard library for the Single Assignment C language. Provides dimension-independent array primitives (`dim`, `shape`, `sel`, `reshape`, `take`, `drop`, `rotate`, `cat`), element-wise arithmetic (`ArrayArith`), reductions (`ArrayReduce`: `sum`, `prod`, `minval`, `maxval` via `fold`), and transforms (`ArrayTransform`: `transpose`, `where`, `mask`). All built on with-loops (SAC's genarray/modarray comprehensions) and ψ-reduction (condensation) — the technique of fusing consecutive array operations into single loops that never materialize intermediate arrays.

The pipeline: `R AST → declare parse → walk + lower → IR → codegen → TinyCC`.

### Condensation / ψ-reduction (critical design principle)

The core codegen mechanism is `tccq_cg_vec_elem(node, idx_var)`, which recursively defines the k-th element of any vector expression tree in terms of its children's k-th elements. This IS ψ-reduction (Mullin 1988) / condensation (Schmittgen 1986) as described in Scholz's SAC paper: consecutive array operations are fused into a single loop body that computes each element without materializing intermediates.

For example, `sum(x[i:(i+n-1)] * weights)` compiles to a single loop:
```c
for (k = 0; k < len; ++k) acc += p_x[(i-1)+k] * p_weights[k];
```
No slice array is allocated, no product array is allocated. The composition `reduce(binary(vec_slice, var))` is resolved entirely by recursive element access.

Correspondence with SAC:

| SAC concept | Our implementation |
|---|---|
| Single assignment scoping | R's functional semantics (free) |
| `with` genarray/modarray | `vec_alloc` + `for`/`vec_set` |
| ψ-reduction (condensation) | `tccq_cg_vec_elem` recursion |
| `sel(v, A)` / `A[v]` | `vec_get`, `mat_get` |
| `take`/`drop` (subarray) | `vec_slice` (range view) |
| Element-wise `Op(A, B)` | `binary` with shape="vector" |
| `fold(fun, neutral)` | `reduce` / `reduce_expr` |
| No intermediate arrays | Slices are views, not allocations |

### Current state

- `tcc_quick()` API is exported and working.
- Declaration parsing (`R/tcc_quick_declare.R`), codetools-style walker (`R/tcc_quick_walk.R`), lowering (`R/tcc_quick_lower.R`), codegen (`R/tcc_quick_codegen.R`), and caching (`R/tcc_quick_cache.R`) are all implemented.
- IR node constructors are in `R/tcc_ir.R`; R→C function registry in `R/tcc_ir_registry.R` (39 entries covering math.h and Rmath.h).
- Registry-driven dispatch: the lowerer checks `tcc_ir_c_api_registry()` for any function call and emits `call1` (arity-1) or `call2` (arity-2) IR nodes. Adding a new math function requires one registry entry, no dispatcher or codegen changes.
- Scalar lowering/codegen: arithmetic, comparisons, logical ops, `if`/`ifelse`, unary math, casts, `stop`.
- Vector operations: `vec_alloc`, `vec_get`/`vec_set`, `vec_slice` (range view `x[a:b]`), `vec_rewrite` (element-wise in-place reassignment), element-wise binary/unary on vectors, `rev`, `pmin`/`pmax`, and `raw` vectors (`raw(n)`, `as.raw`, `type(... = raw(...))`).
- Typed mapping subset: `sapply(x, FUN)` for supported symbol `FUN`; `apply(X, MARGIN, FUN)` for matrix `X`, literal `MARGIN` in `{1,2}`, and `FUN` in `{sum, mean}` (native row/col reducers for direct matrix-variable inputs, delegated fallback otherwise).
- Reductions: `reduce` (named variable), `reduce_expr` (arbitrary vector expression — the condensation path), `which_reduce`, `any`/`all` (short-circuit).
- Native stats reducers: `mean`, `sd`, `median`, `quantile` (scalar and vector `probs`), including literal `na.rm = TRUE/FALSE` handling.
- Cumulative operations: `cumsum`, `cumprod`, `cummax`, `cummin` (sequential scan, not condensable).
- Matrix: `mat_alloc`, `mat_get`, `mat_set`, `nrow`, `ncol`.
- Native matrix row/col reducers: `rowSums`, `colSums`, `rowMeans`, `colMeans` (including literal `na.rm`).
- Multidimensional declaration space: rank-3+ types are parsed/tracked as `array` shape but currently reserved (fallback in `soft/auto`, hard error in `hard`).
- BLAS-backed matrix products: `%*%`, `crossprod`, `tcrossprod` lower to `matmul` and codegen to `F77_CALL(dgemm)` via `R_ext/BLAS.h`.
- LAPACK-backed linear solve: direct `solve(A, b)` / `solve(A, B)` lowers to `solve_lin` and codegen to `F77_CALL(dgesv)`.
- Windows BLAS linkage note: when `matmul` is present, `tcc_quick_compile()` links `Rblas` explicitly (in addition to `R`) because `dgemm` often resolves from `Rblas.dll`.
- Debug diagnostics: `tcc_quick(..., debug = TRUE)` prints compile diagnostics including OS, `has_matmul`, linked libraries, and library paths.
- Control flow: `for` (seq_along/seq_len/seq_range), `while`, `repeat`, `break`, `next`, `if`/`if-else` statements.
- Boundary detection (`.Call`, `.C`, `.External`, `.Internal`, `.Primitive`) and fallback policy (`auto`/`soft`/`hard`; aliases `always`/`never`).
- Delegated evaluation environment: `rf_call` evaluates in wrapper call environment (`environment()`), not fixed `R_GlobalEnv`.
- IR validation pass: hard mode rejects any `rf_call` path; malformed IR nodes fail fast.
- ALTREP-safe codegen: read-only input parameters use `REAL_RO()`/`INTEGER_RO()`/`LOGICAL_RO()` with `const` pointer qualifiers. Parameters that are mutated (targeted by `vec_set`/`mat_set`/`vec_rewrite`) are `Rf_duplicate()`d before coercion to avoid corrupting the caller's objects. Mutation tracking is handled by `tccq_scope_mark_mutated()` in the lowerer and propagated as `ir$mutated_params`.
- Capability table helpers are available in `R/tcc_ir_registry.R` (`tcc_rapi_table()`, `tcc_rapi_has()`, `tcc_rapi_summary()`, `tcc_quick_rf_call_allowlist()`, `tcc_quick_rf_call_quiet()`).
- Tests: `test_tcc_quick_scalar.R` (11), `test_tcc_quick_vector.R` (23), `test_tcc_quick_matrix.R` (18), `test_tcc_quick_control.R` (6), `test_tcc_quick_fallback.R` (14), `test_tcc_quick_ops.R` (30). Full package suite currently runs 350 tests.

### Remaining work

- SSA conversion (Phase 2 in `docs/ir_design.md`): Phi/Upsilon form for scalar values, abstract heaps, optimization passes.
- SAC-inspired array primitives not yet implemented: `reshape`, `rotate`, `take`, `drop`, `cat`, `transpose`, `where`.
- Full capability-gated enforcement across all dynamic call sites (beyond `rf_call`) is still future work.
- Dimension-independent operations (SAC's `with`-loops over shape-polymorphic arrays) are future work.

### API hardening review guardrails (architecture consistency)

When working on `tcc_quick` hardening, treat these as required checks:

- Prefer explicit contracts over heuristics for delegated calls (`rf_call`): mode, shape, and length expectations must be declared and enforced.
- Avoid README-shaped implementations: do not special-case examples if a generic lowering/codegen rule is possible.
- Reject run-away slop in tests: add regression tests that validate contract boundaries (type/shape/mode), not only happy-path values.
- Keep semantics aligned across native and delegated paths (especially reductions, `na.rm`, and logical/integer/double mode behavior).
- Keep IR state explicit: no hidden state transitions; node `mode`/`shape`/length assumptions should be representable and verifiable.
- Any fallback or hard rejection must have actionable, deterministic diagnostics tied to policy and capability tables.

### Fallback strategy

In `soft`/`auto`, fallback happens inside generated C via `Rf_lang*` + `Rf_eval`, not by bouncing back to R. In `hard`, any `rf_call` in IR is rejected before codegen. Any new C helper referenced by TCC-generated code must be added to `RC_libtcc_add_host_symbols()` (macOS/Windows requirement).

### Generated C style

Explicit `PROTECT`/`UNPROTECT` counts; coerce once at function boundary; hoist `LENGTH()` outside loops; use `REAL_RO()`/`INTEGER_RO()`/`LOGICAL_RO()` with `const` for read-only inputs, `REAL()`/`INTEGER()`/`LOGICAL()` only for outputs and mutated inputs; `Rf_duplicate()` mutated input parameters before coercion; conditional `#include <Rmath.h>` when registry entries require it; emit informative `Rf_error` messages for shape/type mismatch.

## README and DOCS Style

- We use a sober style, make paragraphs and avoid ridiculous list/bullet points when uncessary. 
- We are using Rmarkdown so the user can see the output, so no need for ridiculous cats and other things like that.
