# Rtinycc 0.0.0.9000 (development version)

- Added a bun:ffi inspired FFI API
- Add R callback API: `tcc_callback()`, `tcc_callback_ptr()`, `tcc_callback_close()` with trampoline support for passing R functions to compiled C code
- Add structured type support: declarative `tcc_struct()` and `tcc_union()` with generated allocation/accessor helpers and introspection (`sizeof`/`alignof`)
- Bitfield support (compiled by TinyCC and exposed via accessors) and improved type mappings (scalars, zero-copy arrays, pointers)
- Modify configure script to alter tinycc Makefile to remove flat_namespace option on macOS
- add simple return type for tcc_call_symbol