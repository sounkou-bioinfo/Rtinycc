# Package index

## All functions

- [`` `$`( ``*`<tcc_compiled>`*`)`](https://sounkou-bioinfo.github.io/Rtinycc/reference/cash-.tcc_compiled.md)
  : Access a compiled FFI symbol

- [`.RtinyccCall()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/dot-RtinyccCall.md)
  : we are using .Call directly, this is to make R CMD check happy

- [`generate_trampoline()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/generate_trampoline.md)
  : Generate trampoline code for a callback argument

- [`get_external_ptr_addr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/get_external_ptr_addr.md)
  : Get the address of an external pointer

- [`is_callback_type()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/is_callback_type.md)
  : Check if a type represents a callback

- [`parse_callback_type()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/parse_callback_type.md)
  : Parse callback type specification

- [`print(`*`<tcc_callback>`*`)`](https://sounkou-bioinfo.github.io/Rtinycc/reference/print.tcc_callback.md)
  : Print tcc_callback object

- [`print(`*`<tcc_compiled>`*`)`](https://sounkou-bioinfo.github.io/Rtinycc/reference/print.tcc_compiled.md)
  : Print tcc_compiled object

- [`print(`*`<tcc_ffi>`*`)`](https://sounkou-bioinfo.github.io/Rtinycc/reference/print.tcc_ffi.md)
  : Print tcc_ffi object

- [`tcc_add_file()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_file.md)
  : Add a source file to a libtcc state

- [`tcc_add_include_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_include_path.md)
  : Add an include path to a libtcc state

- [`tcc_add_library()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_library.md)
  : Add a library to a libtcc state

- [`tcc_add_library_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_library_path.md)
  : Add a library path to a libtcc state

- [`tcc_add_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_symbol.md)
  : Add a symbol to a libtcc state

- [`tcc_add_sysinclude_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_add_sysinclude_path.md)
  : Add a system include path to a libtcc state

- [`tcc_bind()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_bind.md)
  : Bind symbols with type specifications

- [`tcc_call_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_call_symbol.md)
  : Call a zero-argument symbol with a specified return type

- [`tcc_callback()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback.md)
  : Register an R function as a callback

- [`tcc_callback_async_drain()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_async_drain.md)
  : Drain the async callback queue

- [`tcc_callback_async_enable()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_async_enable.md)
  : Enable async callback dispatcher (main-thread queue)

- [`tcc_callback_async_is_drained()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_async_is_drained.md)
  : Whether the async callback queue is drained

- [`tcc_callback_async_pending()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_async_pending.md)
  : Number of pending async callbacks

- [`tcc_callback_async_schedule()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_async_schedule.md)
  : Schedule a callback to run on the main thread

- [`tcc_callback_close()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_close.md)
  : Close/unregister a callback

- [`tcc_callback_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_ptr.md)
  : Get the C-compatible function pointer

- [`tcc_callback_valid()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_valid.md)
  : Check if callback is still valid

- [`tcc_compile()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_compile.md)
  : Compile FFI bindings

- [`tcc_compile_string()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_compile_string.md)
  : Compile C code from a character string

- [`tcc_container_of()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_container_of.md)
  : Generate container_of helper for struct member

- [`tcc_cstring()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_cstring.md)
  : Create a C-style string pointer

- [`tcc_cstring_object()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_cstring_object.md)
  : CString S3 Class

- [`tcc_data_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_data_ptr.md)
  : Dereference a pointer-to-pointer

- [`tcc_enum()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_enum.md)
  : Declare enum for FFI helper generation

- [`tcc_ffi()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ffi.md)
  : Create a new FFI compilation context

- [`tcc_field_addr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_field_addr.md)
  : Generate field address getter helpers

- [`tcc_free()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_free.md)
  : Free owned memory

- [`tcc_generate_bindings()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_generate_bindings.md)
  : Generate bindings from header declarations

- [`tcc_get_symbol()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_get_symbol.md)
  : Get a symbol pointer from a libtcc state

- [`tcc_global()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_global.md)
  : Declare a global variable getter

- [`tcc_header()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_header.md)
  : Add C headers

- [`tcc_include()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_include.md)
  : Add include path to FFI context

- [`tcc_include_paths()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_include_paths.md)
  [`tcc_sysinclude_paths()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_include_paths.md)
  : TinyCC include search paths

- [`tcc_introspect()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_introspect.md)
  : Enable introspection helpers

- [`tcc_library()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_library.md)
  : Add library to link against

- [`tcc_library_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_library_path.md)
  : Add library path to FFI context

- [`tcc_link()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_link.md)
  : Link an external shared library with Bun-style FFI bindings

- [`tcc_malloc()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_malloc.md)
  : Allocate memory buffer

- [`tcc_map_c_type_to_ffi()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_map_c_type_to_ffi.md)
  : Map a C type string to an Rtinycc FFI type

- [`tcc_null_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_null_ptr.md)
  : Create a NULL pointer

- [`tcc_output()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_output.md)
  : Set output type for FFI compilation

- [`tcc_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_path.md)
  : Locate the TinyCC executable

- [`tcc_prefix()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_prefix.md)
  [`tcc_lib_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_prefix.md)
  [`tcc_lib_paths()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_prefix.md)
  [`tcc_include_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_prefix.md)
  [`tcc_bin_path()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_prefix.md)
  [`tcc_cli()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_prefix.md)
  : TinyCC paths

- [`tcc_ptr_addr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ptr_addr.md)
  : Get pointer address as integer

- [`tcc_ptr_free_set_null()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ptr_free_set_null.md)
  : Free the pointed memory and set to NULL

- [`tcc_ptr_is_null()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ptr_is_null.md)
  : Check whether an external pointer is NULL

- [`tcc_ptr_is_owned()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ptr_is_owned.md)
  :

  Check for the `"rtinycc_owned"` tag

- [`tcc_ptr_set()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ptr_set.md)
  : Set a pointer-to-pointer value

- [`tcc_ptr_utils`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_ptr_utils.md)
  : Pointer and Buffer Utilities for FFI

- [`tcc_read_bytes()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_bytes.md)
  : Read raw bytes from a pointer

- [`tcc_read_cstring()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_cstring.md)
  : Read C-style string from pointer

- [`tcc_read_f32()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_f32.md)
  : Read 32-bit float

- [`tcc_read_f64()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_f64.md)
  : Read 64-bit doubles from a pointer

- [`tcc_read_i16()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_i16.md)
  : Read signed 16-bit integer

- [`tcc_read_i32()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_i32.md)
  : Read signed 32-bit integers from a pointer

- [`tcc_read_i64()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_i64.md)
  : Read signed 64-bit integer

- [`tcc_read_i8()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_i8.md)
  : Read signed 8-bit integer

- [`tcc_read_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_ptr.md)
  : Read a pointer at byte offset

- [`tcc_read_u16()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_u16.md)
  : Read unsigned 16-bit integer

- [`tcc_read_u32()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_u32.md)
  : Read unsigned 32-bit integer

- [`tcc_read_u64()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_u64.md)
  : Read unsigned 64-bit integer

- [`tcc_read_u8()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_read_u8.md)
  : Read unsigned 8-bit values from a pointer

- [`tcc_recompile()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_recompile.md)
  : Recompile a tcc_compiled object

- [`tcc_relocate()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_relocate.md)
  : Relocate compiled code

- [`tcc_run_cli()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_run_cli.md)
  : Run the tinycc CLI

- [`tcc_source()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_source.md)
  : Add C source code

- [`tcc_state()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_state.md)
  : Create a libtcc state

- [`tcc_struct()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_struct.md)
  : Declare struct for FFI helper generation

- [`tcc_struct_raw_access()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_struct_raw_access.md)
  : Enable raw byte access for struct

- [`tcc_symbol_is_valid()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_symbol_is_valid.md)
  : Check if a tcc_symbol external pointer is valid

- [`tcc_treesitter_bindings()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_bindings.md)
  : Generate bindings from a header

- [`tcc_treesitter_defines()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_defines.md)
  : Extract macro defines from a header file

- [`tcc_treesitter_enum_bindings()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_enum_bindings.md)
  : Apply tcc_enum() bindings from a header

- [`tcc_treesitter_enum_members()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_enum_members.md)
  : Parse enum members with treesitter.c

- [`tcc_treesitter_enums()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_enums.md)
  : Parse enum declarations with treesitter.c

- [`tcc_treesitter_functions()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_functions.md)
  : Parse function declarations with treesitter.c

- [`tcc_treesitter_global_types()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_global_types.md)
  : Parse global declarations with types using treesitter.c

- [`tcc_treesitter_globals()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_globals.md)
  : Parse global declarations with treesitter.c

- [`tcc_treesitter_struct_accessors()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_struct_accessors.md)
  : Generate tcc_struct() accessors from header structs

- [`tcc_treesitter_struct_bindings()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_struct_bindings.md)
  : Apply tcc_struct() bindings from a header

- [`tcc_treesitter_struct_members()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_struct_members.md)
  : Parse struct members (including bitfields) with treesitter.c

- [`tcc_treesitter_structs()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_structs.md)
  : Parse struct declarations with treesitter.c

- [`tcc_treesitter_union_accessors()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_union_accessors.md)
  : Generate tcc_union() accessors from header unions

- [`tcc_treesitter_union_bindings()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_union_bindings.md)
  : Apply tcc_union() bindings from a header

- [`tcc_treesitter_union_members()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_union_members.md)
  : Parse union members with treesitter.c

- [`tcc_treesitter_unions()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_treesitter_unions.md)
  : Parse union declarations with treesitter.c

- [`tcc_union()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_union.md)
  : Declare union for FFI helper generation

- [`tcc_write_bytes()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_bytes.md)
  : Write raw bytes to a pointer

- [`tcc_write_f32()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_f32.md)
  : Write a 32-bit float

- [`tcc_write_f64()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_f64.md)
  : Write a 64-bit double

- [`tcc_write_i16()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_i16.md)
  : Write a signed 16-bit integer

- [`tcc_write_i32()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_i32.md)
  : Write a signed 32-bit integer

- [`tcc_write_i64()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_i64.md)
  : Write a signed 64-bit integer

- [`tcc_write_i8()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_i8.md)
  : Write a signed 8-bit integer

- [`tcc_write_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_ptr.md)
  : Write a pointer at byte offset

- [`tcc_write_u16()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_u16.md)
  : Write an unsigned 16-bit integer

- [`tcc_write_u32()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_u32.md)
  : Write an unsigned 32-bit integer

- [`tcc_write_u64()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_u64.md)
  : Write an unsigned 64-bit integer

- [`tcc_write_u8()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_write_u8.md)
  : Write an unsigned 8-bit integer
