/* Rtinycc - TinyCC for R
 * Copyright (C) 2025-2026 Sounkou Mahamane Toure
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Print.h>
#include <stdint.h>

#ifdef _WIN32
#define RTINYCC_DLLEXPORT __declspec(dllexport)
#else
#define RTINYCC_DLLEXPORT
#endif

SEXP RC_libtcc_state_new(SEXP lib_path, SEXP include_path, SEXP output_type);
SEXP RC_set_shutting_down(SEXP flag);
SEXP RC_libtcc_add_file(SEXP ext, SEXP path);
SEXP RC_libtcc_add_include_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_sysinclude_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library(SEXP ext, SEXP library);
SEXP RC_libtcc_compile_string(SEXP ext, SEXP code);
SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr);
SEXP RC_libtcc_add_host_symbols(SEXP ext);
SEXP RC_libtcc_relocate(SEXP ext);
SEXP RC_libtcc_call_symbol(SEXP ext, SEXP name, SEXP ret_type);
SEXP RC_libtcc_get_symbol(SEXP ext, SEXP name);

SEXP RC_libtcc_ptr_valid(SEXP ptr);
SEXP RC_get_external_ptr_addr(SEXP ext);
SEXP RC_get_external_ptr_hex(SEXP ext);

// Pointer utility functions
SEXP RC_null_pointer(void);
SEXP RC_malloc(SEXP size);
SEXP RC_free(SEXP ptr);
SEXP RC_create_cstring(SEXP str);
SEXP RC_read_cstring(SEXP ptr);
SEXP RC_read_cstring_n(SEXP ptr, SEXP nbytes);
SEXP RC_read_bytes(SEXP ptr, SEXP nbytes);
SEXP RC_write_bytes(SEXP ptr, SEXP raw);
SEXP RC_data_ptr(SEXP ptr_ref);
SEXP RC_ptr_set(SEXP ptr_ref, SEXP ptr_value);
SEXP RC_ptr_free_set_null(SEXP ptr_ref);
SEXP RC_ptr_is_owned(SEXP ptr);

// Typed read/write helpers (Bun-style)
SEXP RC_read_i8(SEXP ptr, SEXP offset);
SEXP RC_read_u8_typed(SEXP ptr, SEXP offset);
SEXP RC_read_i16(SEXP ptr, SEXP offset);
SEXP RC_read_u16(SEXP ptr, SEXP offset);
SEXP RC_read_i32_typed(SEXP ptr, SEXP offset);
SEXP RC_read_u32(SEXP ptr, SEXP offset);
SEXP RC_read_i64(SEXP ptr, SEXP offset);
SEXP RC_read_u64(SEXP ptr, SEXP offset);
SEXP RC_read_f32(SEXP ptr, SEXP offset);
SEXP RC_read_f64_typed(SEXP ptr, SEXP offset);
SEXP RC_read_ptr(SEXP ptr, SEXP offset);
SEXP RC_write_i8(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_u8(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_i16(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_u16(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_i32(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_u32(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_i64(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_u64(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_f32(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_f64(SEXP ptr, SEXP offset, SEXP value);
SEXP RC_write_ptr(SEXP ptr, SEXP offset, SEXP value);

// Callback functions
SEXP RC_register_callback(SEXP fun, SEXP return_type, SEXP arg_types, SEXP threadsafe);
SEXP RC_unregister_callback(SEXP callback_ext);
SEXP RC_get_callback_ptr(SEXP callback_ext);
SEXP RC_callback_is_valid(SEXP callback_ext);
SEXP RC_invoke_callback(SEXP callback_id, SEXP args);
SEXP RC_cleanup_callbacks();
SEXP RC_callback_async_init();
SEXP RC_callback_async_schedule(SEXP callback_ext, SEXP args);
SEXP RC_callback_async_drain();
int RC_callback_async_schedule_c(int id, int n_args, const void *args);
SEXP RC_unload_libtcc(void);


// .Call entries
static const R_CallMethodDef CallEntries[] = {
    {"RC_libtcc_state_new",   (DL_FUNC) &RC_libtcc_state_new,   3},
    {"RC_set_shutting_down",  (DL_FUNC) &RC_set_shutting_down,  1},
    {"RC_libtcc_add_file",    (DL_FUNC) &RC_libtcc_add_file,    2},
    {"RC_libtcc_add_include_path",    (DL_FUNC) &RC_libtcc_add_include_path,    2},
    {"RC_libtcc_add_sysinclude_path", (DL_FUNC) &RC_libtcc_add_sysinclude_path, 2},
    {"RC_libtcc_add_library_path",   (DL_FUNC) &RC_libtcc_add_library_path,   2},
    {"RC_libtcc_add_library",        (DL_FUNC) &RC_libtcc_add_library,        2},
    {"RC_libtcc_compile_string", (DL_FUNC) &RC_libtcc_compile_string, 2},
    {"RC_libtcc_add_symbol",  (DL_FUNC) &RC_libtcc_add_symbol,  3},
    {"RC_libtcc_add_host_symbols", (DL_FUNC) &RC_libtcc_add_host_symbols, 1},
    {"RC_libtcc_relocate",    (DL_FUNC) &RC_libtcc_relocate,    1},
    {"RC_libtcc_call_symbol", (DL_FUNC) &RC_libtcc_call_symbol, 3},
    {"RC_libtcc_get_symbol",  (DL_FUNC) &RC_libtcc_get_symbol,  2},
    
    {"RC_libtcc_ptr_valid",   (DL_FUNC) &RC_libtcc_ptr_valid,   1},
    {"RC_get_external_ptr_addr", (DL_FUNC) &RC_get_external_ptr_addr, 1},
    {"RC_get_external_ptr_hex", (DL_FUNC) &RC_get_external_ptr_hex, 1},
    
    // Pointer utility functions
    {"RC_null_pointer",   (DL_FUNC) &RC_null_pointer,   0},
    {"RC_malloc",        (DL_FUNC) &RC_malloc,        1},
    {"RC_free",          (DL_FUNC) &RC_free,          1},
    {"RC_create_cstring",(DL_FUNC) &RC_create_cstring,1},
    {"RC_read_cstring",  (DL_FUNC) &RC_read_cstring,  1},
    {"RC_read_cstring_n", (DL_FUNC) &RC_read_cstring_n, 2},
    {"RC_read_bytes",    (DL_FUNC) &RC_read_bytes,    2},
    {"RC_write_bytes",   (DL_FUNC) &RC_write_bytes,   2},
    {"RC_data_ptr",     (DL_FUNC) &RC_data_ptr,     1},
    {"RC_ptr_set",      (DL_FUNC) &RC_ptr_set,      2},
    {"RC_ptr_free_set_null", (DL_FUNC) &RC_ptr_free_set_null, 1},
    {"RC_ptr_is_owned", (DL_FUNC) &RC_ptr_is_owned, 1},

    // Typed read/write helpers
    {"RC_read_i8",       (DL_FUNC) &RC_read_i8,       2},
    {"RC_read_u8_typed", (DL_FUNC) &RC_read_u8_typed,  2},
    {"RC_read_i16",      (DL_FUNC) &RC_read_i16,      2},
    {"RC_read_u16",      (DL_FUNC) &RC_read_u16,      2},
    {"RC_read_i32_typed",(DL_FUNC) &RC_read_i32_typed, 2},
    {"RC_read_u32",      (DL_FUNC) &RC_read_u32,      2},
    {"RC_read_i64",      (DL_FUNC) &RC_read_i64,      2},
    {"RC_read_u64",      (DL_FUNC) &RC_read_u64,      2},
    {"RC_read_f32",      (DL_FUNC) &RC_read_f32,      2},
    {"RC_read_f64_typed",(DL_FUNC) &RC_read_f64_typed, 2},
    {"RC_read_ptr",      (DL_FUNC) &RC_read_ptr,      2},
    {"RC_write_i8",      (DL_FUNC) &RC_write_i8,      3},
    {"RC_write_u8",      (DL_FUNC) &RC_write_u8,      3},
    {"RC_write_i16",     (DL_FUNC) &RC_write_i16,     3},
    {"RC_write_u16",     (DL_FUNC) &RC_write_u16,     3},
    {"RC_write_i32",     (DL_FUNC) &RC_write_i32,     3},
    {"RC_write_u32",     (DL_FUNC) &RC_write_u32,     3},
    {"RC_write_i64",     (DL_FUNC) &RC_write_i64,     3},
    {"RC_write_u64",     (DL_FUNC) &RC_write_u64,     3},
    {"RC_write_f32",     (DL_FUNC) &RC_write_f32,     3},
    {"RC_write_f64",     (DL_FUNC) &RC_write_f64,     3},
    {"RC_write_ptr",     (DL_FUNC) &RC_write_ptr,     3},

    // Callback functions
    {"RC_register_callback",   (DL_FUNC) &RC_register_callback,   4},
    {"RC_unregister_callback", (DL_FUNC) &RC_unregister_callback, 1},
    {"RC_get_callback_ptr",    (DL_FUNC) &RC_get_callback_ptr,    1},
    {"RC_callback_is_valid",   (DL_FUNC) &RC_callback_is_valid,   1},
    {"RC_invoke_callback",     (DL_FUNC) &RC_invoke_callback,     2},
    {"RC_cleanup_callbacks",   (DL_FUNC) &RC_cleanup_callbacks,   0},
    {"RC_callback_async_init", (DL_FUNC) &RC_callback_async_init, 0},
    {"RC_callback_async_schedule", (DL_FUNC) &RC_callback_async_schedule, 2},
    {"RC_callback_async_drain", (DL_FUNC) &RC_callback_async_drain, 0},
    {"RC_unload_libtcc",       (DL_FUNC) &RC_unload_libtcc,      0},
    
    {NULL, NULL, 0}
};

// Initialization function
RTINYCC_DLLEXPORT void R_init_Rtinycc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


// diagnostic 

//RTINYCC_DLLEXPORT void R_unload_Rtinycc(DllInfo *info) {
 //   (void)info;
  //  Rprintf("[RTINYCC_DIAG] R_unload_Rtinycc called (DLL unload)\n");
  //  RC_set_shutting_down(Rf_ScalarLogical(1));
//}
