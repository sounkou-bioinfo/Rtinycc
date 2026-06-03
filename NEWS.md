# Rtinycc 0.1.11.9000

- Add TinyCC SIMD bytecode examples showing CPU feature detection with `cpuid`/`xgetbv` and selected SSE2/AVX2 instructions emitted through `.byte` for mnemonics TinyCC does not parse.
- Add `tcc_list_symbols()` to inspect global symbol names and resolved hexadecimal addresses known to a libtcc state.
- Expand `tcc_call_symbol()` with `.C()`-style pointer argument calls for low-level in-memory symbols, including guarded copy-in/copy-out support for common R vector types, `Csingle`, read-only `SEXP` paths, and `NAOK` checking.
- Improve ALTREP-aware copy-in paths by using `RAW_GET_REGION()` when copying raw vectors into native memory and scalar accessors for callback/struct scalar conversions. Clarify that mutable array FFI inputs can materialize ALTREP vectors when R exposes writable C storage.
- Harden async callback draining so explicit drains only execute R callbacks on the recorded main R thread, direct main-thread async scheduling executes immediately instead of queueing and waiting on itself, and async trampoline scheduling failures are recorded without calling R APIs from worker-capable code.

# Rtinycc 0.1.11

- Fix `PROTECT` and memory-balance hygiene bugs in internal C state initialization (`RC_libtcc_state_new`, `RC_libtcc_get_symbol`, and callback registration paths). Previously, class string attributes could be inadvertently allocated without proper protection.
- Fix a stack-depth and protection imbalance bug in `RC_invoke_callback_internal` where callback execution could unexpectedly unprotect the `call` object prior to evaluation.

# Rtinycc 0.1.10

- Fix installation on musl-based Linux systems, including Alpine Linux, by
  enabling TinyCC's musl configuration when a musl libc environment is
  detected during configure.

- Fix configure failure on macOS oldrel arm64 (Big Sur, clang 14). The bundled
  TinyCC Makefile pairs `-flat_namespace` with `-undefined warning` on clang
  older than 15. The package strips `-flat_namespace` on macOS to avoid TinyCC
  SIGSEGV behavior under flat namespace, but on clang < 15 that left
  `-undefined warning` on its own, which the modern macOS linker rejects under
  the default `-twolevel_namespace`. The configure step now also strips
  `-undefined warning` so the `libtcc.dylib` link succeeds on the CRAN macOS
  oldrel arm64 builder.

# Rtinycc 0.1.9

- Replace the upstream `make install` step with explicit copies of the
  build artifacts (`tcc`, `libtcc.{a,dylib,so}`, `libtcc1.a`, `libtcc.h`,
  and the bundled tinycc C headers under `lib/tcc/include/`). This sidesteps
  BSD/GNU `install` differences and the strict `install-unx` macros in the
  upstream Makefile that cause configuration to fail on macOS arm64 and
  x86_64 CRAN builders. The installed layout under `inst/tinycc/` is
  unchanged.

- On Linux, prefer the unversioned `libm.so` GNU ld script over the bare
  versioned SONAME (`libm.so.6`) when resolving the system math runtime in
  vignettes and tests. TinyCC handles GNU ld scripts directly, which avoids
  link failures on Fedora-style toolchains.

- Make the external-library linking example in tests and the
  "Linking External Libraries" vignette skip cleanly when `tcc_link()` cannot
  resolve the platform math runtime, instead of erroring the whole run.

- Skip runtime benchmark vignette chunks that depend on memory profiling when
  the running R was built without `Rprofmem()`, avoiding vignette rebuild
  failures on platforms built without memory profiling support.

- Add a CRAN version badge to the README.

# Rtinycc 0.1.8

- Clean up vignette sources by removing package-attachment checks and replacing
  hidden `library(Rtinycc)` setup with explicit namespace bindings, while
  keeping executable examples working during vignette builds.

# Rtinycc 0.1.7

- Add missing return-value documentation across exported helpers and print
  methods so generated reference pages consistently describe result class,
  meaning, or side-effect-only behavior.

- Update vignettes to ensure each article executes ordinary R code during
  builds, and correct conservative type-mapping docs so `const char *`
  examples reflect the default `ptr` mapping.

# Rtinycc 0.1.6

- Exclude the top-level `scripts/` directory more explicitly from package
  builds to avoid CRAN notes about non-standard top-level contents.

# Rtinycc 0.1.5

- Fix Debian CRAN incoming failures around external-library linking. Short
  linker names now stay on the normal linker-name path, and the Unix
  math-library vignette/test examples now resolve a real runtime library file
  robustly on both Linux and macOS.

- Improve external library discovery by searching relevant loader and linker
  environment paths in addition to the built-in platform library path tables.

- Improve vignette rendering of generated C code by reusing the package's
  custom knitr C rendering path so highlighted generated code matches other
  vignette C blocks.

- Clean up README rendering around nested-struct and bitfield limitations.

- Add a Docker helper for reproducing a Debian R-devel CRAN-style check locally.

# Rtinycc 0.1.4

- Tighten FFI soundness around ownership, helper generation, and nested composite
  views. Generated wrappers now route pointer and composite wrapper creation
  through host-side helpers more consistently, borrowed/unowned pointer
  semantics are enforced more strictly, and callback/SEXP/pointer wrapper
  invariants are covered with stronger codegen and runtime tests.

- Clarify and extend helper semantics across structs, unions, enums, globals,
  bitfields, and treesitter-derived bindings. Helper metadata now records both
  helper family and helper operation more explicitly, bitfield and constant
  helpers are distinguished from ordinary field helpers, and named nested
  struct helper accessors support borrowed nested views plus copy-in setters.

- Improve cross-platform robustness for generated external-pointer/finalizer
  paths and Windows diagnostics, including host-side registration helpers,
  tighter smoke tests, and CI/debug workflow coverage for the historical
  Windows GC/finalizer crash path.

- Refresh README, vignettes, and generated reference documentation so the user
  docs match current FFI semantics for pointers, callbacks, bitfields, nested
  helpers, and treesitter-derived accessor behavior.

# Rtinycc 0.1.3

- Tighten FFI boundary handling for wide integers and pointers. Generated
  wrappers, struct accessors, typed memory writers, and callback return paths
  now warn or error more consistently when `i64` / `u64` values cannot be
  represented exactly in R numeric vectors, and typed pointer offsets are now
  validated as non-negative whole numbers instead of truncating through `int`.

- Extend async callback support so `i64`, `u32`, and `u64` arguments use the
  same numeric marshaling path as async returns, with matching documentation of
  the `2^53` exactness limit for R doubles.

- Update array and callback code generation to avoid repeated evaluation in an
  additional `cstring` boxing path, use read-only `character_array` access via
  `STRING_PTR_RO()` when available, and improve callback pointer classification
  for non-string pointer types.

- Clarify user-facing documentation for callback limits and draining behavior,
  `character_array` semantics, serialization of external pointers, and the
  benchmark vignette's zero-length array-return example.

- Fix FFI return code generation so array, `cstring`, `i64`, and `u64`
  return expressions are evaluated once before boxing/copying. This avoids
  repeated calls in generated wrappers for side-effectful expressions.

- Declare the temporary `cstring` return variable as `const char*` so the
  generated wrapper accepts `const char*`-returning C functions without a
  const-discarding assignment. This restores vignette builds on macOS and
  Windows (bundled TinyCC treated the implicit const-discard as a fatal
  diagnostic rather than a warning).

- Add regression tests for repeated-evaluation bugs in generated FFI return
  code.

- Add `make vig` and `make vig-md` development targets for rebuilding HTML
  package vignettes and Markdown vignette exports.

- Add an internal knitr engine for reusable C chunks in vignettes, adapted
  from the `callme` package, and use it in the benchmark vignette. The
  executable `callme` comparisons now degrade gracefully when the build
  environment cannot compile the temporary helper DLL, which restores passing
  package checks on Windows CI.

# Rtinycc 0.1.1

- Fix C compiler warnings in the package sources by using strict prototypes for
  zero-argument routines and explicit initialization for async callback result
  structs.

- Remove compiler-specific diagnostic pragmas from the host-symbol registration
  path so package checks no longer report pragma usage notes.

- Rework the benchmark documentation so temporary `callme` DLLs are created,
  benchmarked, and unloaded within helper scopes. This reduces the risk of
  Windows vignette-build failures caused by lingering loaded modules.

# Rtinycc 0.1.0

- Add variadic FFI support in `tcc_bind()` with both legacy typed-tail mode (`variadic = TRUE`, `varargs`) and bounded dynamic-tail mode (`variadic = TRUE`, `varargs_types`, `varargs_min`, `varargs_max`).

- Refactor FFI codegen and callbacks to use lambda.r guard rules instead of long `if`/`else` or `switch` chains. This keeps type-mapping logic centralized in `R/aaa_ffi_codegen_rules.R`, makes behavior easier to extend, and reduces duplication across codegen sites.

- Reduce shutdown-related crash risk by using `onexit = FALSE` for registered external-pointer finalizers, so cleanup runs during normal garbage collection rather than during R process teardown. This change is part of the Windows stabilization work and complements explicit ownership tracking for `TCCState*` wrappers.

- Treesitter helper examples are now wrapped in `\dontrun{}` and the treesitter helper tests are skipped on Windows to avoid process-exit crashes related to `treesitter.c` cleanup.

- Experimental Windows support. The package now builds and passes `R CMD check` on Windows (Rtools 4.5 / UCRT). The Windows build compiles TinyCC from source, dynamically links `libtcc.dll`, and includes the compatibility glue needed for R's UCRT runtime. Async callbacks and `fork()`-based parallelism remain Unix-only.

- `RC_invoke_callback_id()` replaces the `snprintf`-based callback dispatch. The trampoline now passes the callback token as an integer directly, eliminating a round-trip through string conversion that relied on `snprintf` (unavailable as a direct symbol on Windows/UCRT).

- Add new user documentation for core FFI semantics, internals, external library linking, treesitter-based header parsing, and benchmark-oriented comparisons against a conventional `.Call()` workflow.

# Rtinycc 0.0.3

- Add typed memory read/write helpers inspired by Bun's FFI and the ctypesio package: `tcc_read_i8()`, `tcc_read_u8()`, `tcc_read_i16()`, `tcc_read_u16()`, `tcc_read_i32()`, `tcc_read_u32()`, `tcc_read_i64()`, `tcc_read_u64()`, `tcc_read_f32()`, `tcc_read_f64()`, `tcc_read_ptr()` and corresponding `tcc_write_*()` functions. All operate at a byte offset and use `memcpy` internally for alignment safety.

- Legacy vectorised interface preserved: `tcc_read_u8(ptr, n = 4)`, `tcc_read_i32(ptr, n = 2)`, and `tcc_read_f64(ptr, n = 2)` continue to work alongside the new scalar offset API.

- Fix macOS "undefined symbol" errors at relocation time. `RC_libtcc_add_host_symbols()` explicitly registers host symbols (`RC_free_finalizer`, `RC_invoke_callback`, `RC_callback_async_schedule_c`) via `tcc_add_symbol()` before `tcc_relocate()`.

- Fix macOS library linking: short library names like `"m"` that cannot be resolved to a file path are now passed through as `-l<name>` to the linker instead of erroring.

- Fix `stdbool.h` not found on macOS: `tcc_set_lib_path()` now receives the correct `lib/tcc` directory so TCC resolves its own headers via `{B}/include`.

- `tcc_compiled` objects now survive `serialize()`/`unserialize()` and `saveRDS()`/`readRDS()`. The FFI recipe is stored inside the compiled object; on first access after deserialization, `$.tcc_compiled` detects the dead TCC state pointer and recompiles transparently.

- Compiled FFI objects are fork-safe: `parallel::mclapply()` and other `fork()`-based parallelism work out of the box since TCC's compiled code lives in memory mappings that survive `fork()` via copy-on-write.

# Rtinycc 0.0.2

- Update package title and description
- Mark this as the first release with API versioning going forward

# Rtinycc 0.0.1.9000 (development version)

- Generate callback trampolines from `callback:<signature>` bindings so C APIs can accept function pointers directly
- Add `callback_async:<signature>` for thread-safe scheduling on the main thread
- Callback errors now warn and return type-appropriate defaults (avoid longjmp)
- Update README examples for callbacks, async callbacks, enums, and bitfields
- Fix potential exit-time segfaults by using a package-level finalizer (`RC_free_finalizer`) for generated struct/union helpers instead of JIT-local finalizers
- Add treesitter.c helpers (`tcc_treesitter_functions()`, `tcc_treesitter_structs()`, `tcc_treesitter_bindings()`) aligned with Rtinycc type mappings
- Require treesitter.c (>= 0.0.3) for header parsing helpers
- Add global getter/setter helpers via `tcc_global()` with generated `global_<name>_get()` and `global_<name>_set()` accessors
- BREAKING: struct/union helper names now use `struct_<name>_*` and `union_<name>_*` prefixes (including bitfields)
- Add `tcc_generate_bindings()` and expanded tree-sitter helpers for enum/union/global parsing and binding generation
- Add pointer utilities: `tcc_null_ptr()`, `tcc_malloc()`, `tcc_free()`, `tcc_read_*()` helpers, pointer-to-pointer utilities (`tcc_data_ptr()`, `tcc_ptr_set()`, `tcc_ptr_free_set_null()`), and ownership checks via `tcc_ptr_is_owned()`
- Tag external pointers as owned/borrowed and guard frees based on ownership
- Add array field element helpers for structs (`struct_<name>_get_<field>_elt()` / `struct_<name>_set_<field>_elt()`)
- Add array return specifications in `tcc_bind()` via `returns = list(type = "*_array", length_arg = ..., free = TRUE)`

# Rtinycc 0.0.1

- First release

# Rtinycc 0.0.0.9000 (development version)

- Added a bun:ffi inspired FFI API
- Add R callback API: `tcc_callback()`, `tcc_callback_ptr()`, `tcc_callback_close()` with trampoline support for passing R functions to compiled C code
- Add structured type support: declarative `tcc_struct()` and `tcc_union()` with generated allocation/accessor helpers and introspection (`sizeof`/`alignof`)
- Bitfield support (compiled by TinyCC and exposed via accessors) and improved type mappings (scalars, zero-copy arrays, pointers)
- Modify configure script to alter tinycc Makefile to remove flat_namespace option on macOS
- add simple return type for tcc_call_symbol
