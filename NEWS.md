# Rtinycc 0.0.2.9000 (development version)

- Fix macOS "undefined symbol" errors at relocation time. The configure script strips `-flat_namespace` from TCC's Makefile to avoid SIGEV issues, but without it TCC cannot resolve host symbols (`RC_free_finalizer`, `RC_invoke_callback`, `RC_callback_async_schedule_c`) through the dynamic linker. The new `RC_libtcc_add_host_symbols()` explicitly registers these symbols via `tcc_add_symbol()` before `tcc_relocate()`, which is harmless on Linux and fixes macOS.

- `tcc_compiled` objects now survive `serialize()`/`unserialize()` and `saveRDS()`/`readRDS()`. The FFI recipe is stored inside the compiled object; on first access after deserialization, `$.tcc_compiled` detects the dead TCC state pointer and recompiles transparently. Works for both `tcc_compile()` and `tcc_link()` objects. Use `tcc_recompile()` for explicit recompilation.

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