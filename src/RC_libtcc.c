/* Rtinycc - TinyCC for R
 * Copyright (C) 2025-2026 Sounkou Mahamane Toure
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "libtcc.h"
#include "platform_async.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <R_ext/Print.h>
#include <R_ext/Parse.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#if RTINYCC_OS_WINDOWS
#include <windows.h>
#endif

// ============================================================================
// Forward declarations
// ============================================================================

SEXP RC_set_shutting_down(SEXP flag);
static void RC_tcc_finalizer(SEXP ext);
static void RC_null_finalizer(SEXP ext);
void RC_free_finalizer(SEXP ext);
SEXP RC_libtcc_state_new(SEXP lib_path, SEXP include_path, SEXP output_type);
SEXP RC_libtcc_add_file(SEXP ext, SEXP path);
SEXP RC_libtcc_add_include_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_sysinclude_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library(SEXP ext, SEXP library);
SEXP RC_libtcc_compile_string(SEXP ext, SEXP code);
SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr);
SEXP RC_libtcc_relocate(SEXP ext);
SEXP RC_libtcc_get_symbol(SEXP ext, SEXP name);
SEXP RC_libtcc_call_symbol(SEXP ext, SEXP name, SEXP ret_type);
SEXP RC_libtcc_ptr_valid(SEXP ptr);
SEXP RC_libtcc_output_file(SEXP ext, SEXP filename);
SEXP RC_get_external_ptr_addr(SEXP ext);
SEXP RC_get_external_ptr_hex(SEXP ext);
SEXP RC_null_pointer();
SEXP RC_malloc(SEXP size);
SEXP RC_free(SEXP ptr);
SEXP RC_data_ptr(SEXP ptr_ref);
SEXP RC_ptr_set(SEXP ptr_ref, SEXP ptr_value);
SEXP RC_ptr_free_set_null(SEXP ptr_ref);
SEXP RC_ptr_is_owned(SEXP ptr);
SEXP RC_create_cstring(SEXP str);
SEXP RC_read_cstring(SEXP ptr);
SEXP RC_read_cstring_n(SEXP ptr, SEXP nbytes);
SEXP RC_read_bytes(SEXP ptr, SEXP nbytes);
SEXP RC_write_bytes(SEXP ptr, SEXP raw);
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
static int RC_callback_find_free_slot();
SEXP RC_callback_async_init();
SEXP RC_callback_async_schedule(SEXP callback_ext, SEXP args);
SEXP RC_callback_async_drain();
int RC_callback_async_schedule_c(int id, int n_args, const cb_arg_t *args);
static void RC_callback_finalizer(SEXP ext);
static void RC_callback_ptr_finalizer(SEXP ext);
SEXP RC_register_callback(SEXP fun, SEXP return_type, SEXP arg_types, SEXP threadsafe);
SEXP RC_unregister_callback(SEXP callback_ext);
SEXP RC_get_callback_ptr(SEXP callback_ext);
SEXP RC_callback_is_valid(SEXP callback_ext);
static SEXP RC_callback_default_sexp(const char *return_type);
SEXP RC_invoke_callback_internal(int id, SEXP args);
SEXP RC_invoke_callback(SEXP callback_id, SEXP args);
SEXP RC_invoke_callback_id(int id, SEXP args);
SEXP RC_cleanup_callbacks();
SEXP RC_libtcc_add_host_symbols(SEXP ext);

// ============================================================================
// Types and constants
// ============================================================================

// Callback registry entry - stores preserved R function and metadata
typedef struct {
    SEXP fun;              // Preserved R function object
    char *return_type;     // Return type string
    char **arg_types;      // Array of argument type strings
    int n_args;            // Number of arguments
    int threadsafe;        // Thread-safety flag
    int valid;             // Whether this entry is still valid
    void *trampoline;      // Pointer to trampoline function (if generated)
} callback_entry_t;

// Static array of callbacks (simple fixed-size for now)
#define MAX_CALLBACKS 256
static callback_entry_t callback_registry[MAX_CALLBACKS];

// Callback token structure - passed back to R as external ptr
typedef struct {
    int id;                // Index into callback_registry (or -1 when closed)
    int refs;              // Reference count for outstanding external pointers
} callback_token_t;

// ============================================================================
// Shutdown and finalizers
// ============================================================================

/**
 * @section Shutdown and finalizers
 * @param flag Logical scalar from R indicating shutdown state.
 * @example
 *  .Call("RC_set_shutting_down", TRUE)
 */

/* Set during package unload to avoid teardown crashes on some platforms. */
static volatile int g_rtinycc_shutting_down = 0;

/**
 * Set shutdown flag from R (.onUnload).
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_set_shutting_down(SEXP flag) {
    g_rtinycc_shutting_down = Rf_asLogical(flag) ? 1 : 0;
    return R_NilValue;
}

/**
 * Releases a TCCState when its R external pointer is garbage collected.
 * Ownership: frees owned TCCState.
 * Allocation: none.
 * Protection: none.
 */
static void RC_tcc_finalizer(SEXP ext) {
    if (g_rtinycc_shutting_down) {
        return;
    }
    void *ptr = R_ExternalPtrAddr(ext);
    if (ptr) {
        TCCState *s = (TCCState*)ptr;
#if RTINYCC_OS_WINDOWS
        // On Windows, skip tcc_delete() entirely to avoid DLL unload order crashes
        // The OS will reclaim all memory when the process exits
        R_ClearExternalPtr(ext);
#else
        tcc_delete(s);
        R_ClearExternalPtr(ext);
#endif
    }
}

/**
 * No-op finalizer for borrowed / NULL pointers.
 * Ownership: does not free.
 * Allocation: none.
 * Protection: none.
 */
static void RC_null_finalizer(SEXP ext) {
    // NULL pointer doesn't need cleanup
    R_ClearExternalPtr(ext);
}

/**
 * Generic finalizer: calls free() on the pointer and clears the EXTPTR.
 * Ownership: frees owned heap memory (malloc/free).
 * Allocation: none.
 * Protection: none.
 */
void RC_free_finalizer(SEXP ext) {
    void *ptr = R_ExternalPtrAddr(ext);
    if (ptr) {
        free(ptr);
        R_ClearExternalPtr(ext);
    }
}

// ============================================================================
// TCC state and compilation
// ============================================================================

/**
 * @section TCC state and compilation
 * @param ext External pointer to a TCC state.
 * @param lib_path Character vector of library search paths.
 * @param include_path Character vector of include search paths.
 * @param output_type Integer TCC output mode.
 * @example
 *  state <- .Call("RC_libtcc_state_new", lib_paths, include_paths, 1L)
 */

/**
 * Unwrap and validate a tcc_state external pointer.
 * Ownership: borrowed; does not take ownership.
 * Allocation: none.
 * Protection: none.
 */
static inline TCCState *RC_tcc_state(SEXP ext) {
    if (!Rf_inherits(ext, "tcc_state")) {
        Rf_error("expected a 'tcc_state' external pointer");
    }
    TCCState *s = (TCCState *) R_ExternalPtrAddr(ext);
    if (!s) {
        Rf_error("tcc_state pointer is NULL");
    }
    return s;
}

/**
 * Create a new TCCState, configure output type, and add include/lib paths.
 * Ownership: returns owned external pointer (finalizer frees TCCState).
 * Allocation: tcc_new (heap) + external pointer.
 * Protection: PROTECT(1), UNPROTECT(1).
 */
SEXP RC_libtcc_state_new(SEXP lib_path, SEXP include_path, SEXP output_type) {
    TCCState *s = tcc_new();
    if (!s) {
        Rf_error("tcc_new failed");
    }

    /* Route libtcc diagnostics through R (stdout, sink-able) */
    tcc_set_error_func(s, NULL, (void (*)(void *, const char *)) Rprintf);

    /* library paths */
    if (Rf_isString(lib_path) && XLENGTH(lib_path) > 0) {
        for (R_xlen_t i = 0; i < XLENGTH(lib_path); i++) {
            const char *p = Rf_translateCharUTF8(STRING_ELT(lib_path, i));
            if (p && p[0]) {
                if (i == 0) {
                    tcc_set_lib_path(s, p);
                }
                tcc_add_library_path(s, p);
            }
        }
    }

    /* include and sysinclude paths */
    if (Rf_isString(include_path) && XLENGTH(include_path) > 0) {
        for (R_xlen_t i = 0; i < XLENGTH(include_path); i++) {
            const char *p = Rf_translateCharUTF8(STRING_ELT(include_path, i));
            if (p && p[0]) {
                tcc_add_include_path(s, p);
                tcc_add_sysinclude_path(s, p);
            }
        }
    }

    int out_type = Rf_asInteger(output_type);
    if (out_type <= 0) {
        out_type = TCC_OUTPUT_MEMORY;
    }

    if (tcc_set_output_type(s, out_type) != 0) {
        tcc_delete(s);
        Rf_error("tcc_set_output_type failed");
    }
    SEXP ext = PROTECT(R_MakeExternalPtr(s, R_NilValue, R_NilValue));
    Rf_setAttrib(ext, R_ClassSymbol, Rf_mkString("tcc_state"));
    /* onexit = FALSE: skip tcc_delete() during R shutdown.
       tcc_delete() → tcc_run_free() releases DLLs loaded during JIT
       (FreeLibrary on Windows, dlclose on Unix) and frees JIT memory.
       At process exit the OS reclaims everything automatically;
       running these teardown calls while the runtime is shutting down
       causes segfaults (Windows) or is simply pointless (Unix/macOS).
       Normal GC still runs this finalizer while R is alive. */
    R_RegisterCFinalizerEx(ext, RC_tcc_finalizer, FALSE);
    UNPROTECT(1);
    return ext;
}

/**
 * Add a source or object file to the compilation.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_libtcc_add_file(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *fname = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_file(s, fname);
    return Rf_ScalarInteger(rc);
}

/**
 * Append a user include path (-I).
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_libtcc_add_include_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_include_path(s, p);
    return Rf_ScalarInteger(rc);
}

/**
 * Append a system include path (-isystem).
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_libtcc_add_sysinclude_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_sysinclude_path(s, p);
    return Rf_ScalarInteger(rc);
}

/**
 * Append a library search path (-L).
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_libtcc_add_library_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_library_path(s, p);
    return Rf_ScalarInteger(rc);
}

/**
 * Link a library (-l).
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_libtcc_add_library(SEXP ext, SEXP library) {
    TCCState *s = RC_tcc_state(ext);
    const char *lib = Rf_translateCharUTF8(STRING_ELT(library, 0));
    int rc = tcc_add_library(s, lib);
    return Rf_ScalarInteger(rc);
}



/**
 * Compile a C source string in-memory. Returns 0 on success.
 * Ownership: none.
 * Allocation: handled by TCC.
 * Protection: none.
 */
SEXP RC_libtcc_compile_string(SEXP ext, SEXP code) {
    TCCState *s = RC_tcc_state(ext);
     const char *src = Rf_translateCharUTF8(STRING_ELT(code, 0));
    int rc = tcc_compile_string(s, src);
    return Rf_ScalarInteger(rc);
}

/**
 * Register a host symbol so TCC-compiled code can reference it.
 * Ownership: none (borrows function pointer).
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr) {
    TCCState *s = RC_tcc_state(ext);
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    void *ptr = R_ExternalPtrAddr(addr);
    int rc = tcc_add_symbol(s, sym, ptr);
    return Rf_ScalarInteger(rc);
}

/**
 * Relocate compiled code, resolving all symbols. Returns 0 on success.
 * Ownership: none.
 * Allocation: handled by TCC.
 * Protection: none.
 */
SEXP RC_libtcc_relocate(SEXP ext) {
    TCCState *s = RC_tcc_state(ext);
    int rc = tcc_relocate(s);
    return Rf_ScalarInteger(rc);
}

// ============================================================================
// Symbol lookup and invocation
// ============================================================================

/**
 * @section Symbol lookup and invocation
 * @param ext External pointer to a TCC state.
 * @param name Symbol name to resolve.
 * @param ret_type Return type selector ("int", "double", "void").
 * @example
 *  fn <- .Call("RC_libtcc_get_symbol", state, "my_fn")
 */

/**
 * Look up a symbol after relocation and return it as a DL_FUNC external pointer.
 * Ownership: returns owned external pointer (no free required).
 * Allocation: external pointer only.
 * Protection: PROTECT(1), UNPROTECT(1).
 */
/* Look up a symbol after relocation and return it as a DL_FUNC external
 * pointer tagged "native symbol" so .Call() can invoke it directly. */
SEXP RC_libtcc_get_symbol(SEXP ext, SEXP name) {
    TCCState *s = RC_tcc_state(ext);
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    void *fn = tcc_get_symbol(s, sym);
    if (!fn) {
        Rf_error("symbol '%s' not found", sym);
    }
    /* Create a proper native symbol pointer that .Call can use directly */
    static SEXP native_symbol_tag = NULL;
    if (native_symbol_tag == NULL) {
        native_symbol_tag = Rf_install("native symbol");
    }
    DL_FUNC fn_ptr;
    memcpy(&fn_ptr, &fn, sizeof(fn_ptr));
    SEXP ptr = PROTECT(R_MakeExternalPtrFn(fn_ptr, native_symbol_tag, R_NilValue));
    Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("tcc_symbol"));
    UNPROTECT(1);
    return ptr;
}

/**
 * Call a zero-argument symbol, casting to the requested return type.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
/* Call a zero-argument symbol, casting to the requested return type
 * ("int", "double", or "void"). Useful for quick tests. */
SEXP RC_libtcc_call_symbol(SEXP ext, SEXP name, SEXP ret_type) {
    TCCState *s = RC_tcc_state(ext);
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    const char *rtype = Rf_translateCharUTF8(STRING_ELT(ret_type, 0));
    void *fn = tcc_get_symbol(s, sym);
    if (!fn) {
        Rf_error("symbol '%s' not found", sym);
    }
    uintptr_t addr = (uintptr_t) fn;
    /* Debug: print address and alignment if RTINYCC_DEBUG is set */
    int debug_enabled = 0;
    const char *env = getenv("RTINYCC_DEBUG");
    if (env && env[0] == '1') debug_enabled = 1;
    if (debug_enabled) {
        Rprintf("[RTINYCC_DEBUG][C] symbol '%s' address: 0x%lx\n", sym, (unsigned long)addr);
        Rprintf("[RTINYCC_DEBUG][C] address %% 8: %ld\n", (long)(addr % 8));
    }
    if (strcmp(rtype, "int") == 0) {
        int (*callable)(void) = (int (*)(void)) addr;
        return Rf_ScalarInteger(callable());
    } else if (strcmp(rtype, "double") == 0) {
        double (*callable)(void) = (double (*)(void)) addr;
        return Rf_ScalarReal(callable());
    } else if (strcmp(rtype, "void") == 0) {
        void (*callable)(void) = (void (*)(void)) addr;
        callable();
        return R_NilValue;
    }
    Rf_error("unsupported return type '%s' (expected int, double, or void)", rtype);
}

// ============================================================================
// Pointer utilities and memory ownership
// ============================================================================

/**
 * @section Pointer utilities and memory ownership
 * @param ptr External pointer to inspect.
 * @param size Allocation size in bytes.
 * @example
 *  p <- .Call("RC_malloc", 8L)
 */

/**
 * Return TRUE if the external pointer address is non-NULL.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
/* Return TRUE if the external pointer address is non-NULL. */
SEXP RC_libtcc_ptr_valid(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("expected an external pointer");
    }
    void *p = R_ExternalPtrAddr(ptr);
    return Rf_ScalarLogical(p != NULL);
}

/**
 * Write compilation output to a file (obj, dll, or exe).
 * Ownership: none.
 * Allocation: handled by TCC.
 * Protection: none.
 */
/* Write compilation output to a file (obj, dll, or exe). */
SEXP RC_libtcc_output_file(SEXP ext, SEXP filename) {
    TCCState *s = RC_tcc_state(ext);
    const char *fname = Rf_translateCharUTF8(STRING_ELT(filename, 0));
    int rc = tcc_output_file(s, fname);
    return Rf_ScalarInteger(rc);
}

/**
 * Return pointer address as a double (numeric).
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
/* Return pointer address as a double (numeric). */
SEXP RC_get_external_ptr_addr(SEXP ext) {
    void *addr = R_ExternalPtrAddr(ext);
    return Rf_ScalarReal((double)(uintptr_t)addr);
}

/**
 * Return pointer address as a hex string ("0x...").
 * Ownership: returns a new R string (R-managed).
 * Allocation: R_Calloc buffer, freed via R_Free.
 * Protection: none.
 */
/* Return pointer address as a hex string ("0x..."). */
SEXP RC_get_external_ptr_hex(SEXP ext) {
    void *raw = R_ExternalPtrAddr(ext);
    uintptr_t addr = (uintptr_t) raw;
    /* Buffer size: '0x' + two chars per byte + NUL */
    size_t buf_size = 2 + (sizeof(uintptr_t) * 2) + 1;
    char *buf = (char*) R_Calloc(buf_size, char);
    if (!buf) {
        Rf_error("memory allocation failed");
    }
    /* Use PRIxPTR for portable formatting */
    if (raw == NULL) {
        snprintf(buf, buf_size, "0x0");
    } else {
        snprintf(buf, buf_size, "0x%" PRIxPTR, addr);
    }
    SEXP res = Rf_mkString(buf);
    R_Free(buf);
    return res;
}

/**
 * Create a NULL external pointer tagged "rtinycc_null".
 * Ownership: returned external pointer is owned by R.
 * Allocation: external pointer.
 * Protection: none.
 */
/* Create a NULL external pointer tagged "rtinycc_null". */
SEXP RC_null_pointer() {
    SEXP ptr = R_MakeExternalPtr(NULL, Rf_install("rtinycc_null"), R_NilValue);
    R_RegisterCFinalizerEx(ptr, RC_null_finalizer, FALSE);
    return ptr;
}

/**
 * Allocate `size` bytes via malloc.
 * Ownership: returned pointer is owned (rtinycc_owned).
 * Allocation: malloc + external pointer.
 * Protection: none.
 */
/* Allocate `size` bytes via malloc. Returns an external pointer tagged
 * "rtinycc_owned" with RC_free_finalizer registered. */
SEXP RC_malloc(SEXP size) {
    int sz = Rf_asInteger(size);
    if (sz <= 0) {
        Rf_error("Size must be positive");
    }
    
    void *data = malloc(sz);
    if (!data) {
        Rf_error("Memory allocation failed");
    }
    
    SEXP ptr = R_MakeExternalPtr(data, Rf_install("rtinycc_owned"), R_NilValue);
    R_RegisterCFinalizerEx(ptr, RC_free_finalizer, FALSE);
    return ptr;
}

/**
 * Free an owned pointer. Errors if tag is not rtinycc_owned.
 * Ownership: releases owned heap memory.
 * Allocation: none.
 * Protection: none.
 */
/* Free an owned pointer. Errors if the tag is not "rtinycc_owned" or NULL
 * (i.e. refuses to free struct or borrowed pointers). */
SEXP RC_free(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }

    SEXP tag = R_ExternalPtrTag(ptr);
    if (!Rf_isNull(tag) && tag != Rf_install("rtinycc_owned")) {
        Rf_error("Pointer is not owned by Rtinycc; refusing to free");
    }
    
    void *data = R_ExternalPtrAddr(ptr);
    if (data) {
        free(data);
        R_ClearExternalPtr(ptr);
    }
    
    return R_NilValue;
}

/**
 * Dereference a pointer-to-pointer (void**).
 * Ownership: returns borrowed external pointer (no free).
 * Allocation: external pointer only.
 * Protection: none.
 */
/* Dereference a pointer-to-pointer (void**). Returns a borrowed external
 * pointer tagged "rtinycc_borrowed" with a no-op finalizer. */
SEXP RC_data_ptr(SEXP ptr_ref) {
    if (TYPEOF(ptr_ref) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }

    void *ref = R_ExternalPtrAddr(ptr_ref);
    if (!ref) {
        SEXP out = R_MakeExternalPtr(NULL, Rf_install("rtinycc_borrowed"), R_NilValue);
        R_RegisterCFinalizerEx(out, RC_null_finalizer, FALSE);
        return out;
    }

    void *data = *((void**)ref);
    SEXP out = R_MakeExternalPtr(data, Rf_install("rtinycc_borrowed"), R_NilValue);
    R_RegisterCFinalizerEx(out, RC_null_finalizer, FALSE);
    return out;
}

/**
 * Write ptr_value's address into the memory pointed to by ptr_ref.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
/* Write ptr_value's address into the memory pointed to by ptr_ref. */
SEXP RC_ptr_set(SEXP ptr_ref, SEXP ptr_value) {
    if (TYPEOF(ptr_ref) != EXTPTRSXP || TYPEOF(ptr_value) != EXTPTRSXP) {
        Rf_error("Expected external pointers");
    }

    void *ref = R_ExternalPtrAddr(ptr_ref);
    if (!ref) {
        Rf_error("Pointer reference is NULL");
    }

    void *value = R_ExternalPtrAddr(ptr_value);
    *((void**)ref) = value;
    return ptr_ref;
}

/**
 * Free the pointed-to memory and set the pointer to NULL.
 * Ownership: frees heap memory at *ptr_ref.
 * Allocation: none.
 * Protection: none.
 */
/* Free the pointed-to memory and set the pointer to NULL. */
SEXP RC_ptr_free_set_null(SEXP ptr_ref) {
    if (TYPEOF(ptr_ref) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }

    void *ref = R_ExternalPtrAddr(ptr_ref);
    if (!ref) {
        return ptr_ref;
    }

    void **slot = (void**)ref;
    if (*slot) {
        free(*slot);
        *slot = NULL;
    }

    return ptr_ref;
}

/**
 * TRUE if the pointer tag is "rtinycc_owned", FALSE otherwise.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
/* TRUE if the pointer tag is "rtinycc_owned", FALSE otherwise. */
SEXP RC_ptr_is_owned(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    SEXP tag = R_ExternalPtrTag(ptr);
    if (Rf_isNull(tag)) {
        return Rf_ScalarLogical(FALSE);
    }
    return Rf_ScalarLogical(tag == Rf_install("rtinycc_owned"));
}

// ============================================================================
// C string helpers
// ============================================================================

/**
 * @section C string helpers
 * @param str Character vector from R.
 * @param ptr External pointer to a C string.
 * @param nbytes Number of bytes to read.
 * @example
 *  p <- .Call("RC_create_cstring", "hello")
 */

/**
 * Allocate a malloc'd copy of an R string as a C string (NUL-terminated).
 * Ownership: returned pointer is owned (rtinycc_owned).
 * Allocation: malloc + external pointer.
 * Protection: none.
 */
/* Allocate a malloc'd copy of an R string as a C string (NUL-terminated).
 * Returns an external pointer tagged "rtinycc_owned". */
SEXP RC_create_cstring(SEXP str) {
    if (TYPEOF(str) != STRSXP) {
        Rf_error("Expected character vector");
    }
    
    const char *c_str = Rf_translateCharUTF8(STRING_ELT(str, 0));
    char *data = malloc(strlen(c_str) + 1);
    if (!data) {
        Rf_error("Memory allocation failed");
    }
    
    strcpy(data, c_str);
    
    SEXP ptr = R_MakeExternalPtr(data, Rf_install("rtinycc_owned"), R_NilValue);
    R_RegisterCFinalizerEx(ptr, RC_free_finalizer, FALSE);
    return ptr;
}

/**
 * Read a NUL-terminated C string from an external pointer.
 * Ownership: returns R-managed string.
 * Allocation: none.
 * Protection: none.
 */
/* Read a NUL-terminated C string from an external pointer.
 * Returns "" (not NA) if the pointer is NULL. */
SEXP RC_read_cstring(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    
    char *data = (char*)R_ExternalPtrAddr(ptr);
    if (!data) {
        return Rf_ScalarString(Rf_mkChar(""));
    }
    
    return Rf_ScalarString(Rf_mkCharCE(data, CE_UTF8));
}

/**
 * Read exactly nbytes from a C string (fixed-length read).
 * Ownership: returns R-managed string.
 * Allocation: R_Calloc buffer, freed via R_Free.
 * Protection: PROTECT(1), UNPROTECT(1).
 */
/* Read exactly nbytes from a C string (fixed-length read).
 * Returns NA_STRING if the pointer is NULL. */
SEXP RC_read_cstring_n(SEXP ptr, SEXP nbytes) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    int n = Rf_asInteger(nbytes);
    if (n < 0) {
        Rf_error("nbytes must be non-negative");
    }

    char *data = (char*)R_ExternalPtrAddr(ptr);
    if (!data) {
        return Rf_ScalarString(NA_STRING);
    }

    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    if (n == 0) {
        SET_STRING_ELT(out, 0, Rf_mkChar(""));
        UNPROTECT(1);
        return out;
    }

    char *buf = (char*) R_Calloc((size_t)n + 1, char);
    if (!buf) {
        Rf_error("memory allocation failed");
    }
    memcpy(buf, data, (size_t)n);
    buf[n] = '\0';
    SET_STRING_ELT(out, 0, Rf_mkCharCE(buf, CE_UTF8));
    R_Free(buf);
    UNPROTECT(1);
    return out;
}

// ============================================================================
// Raw bytes and typed read/write helpers
// ============================================================================

/**
 * @section Raw bytes and typed read/write helpers
 * @param ptr External pointer to a memory region.
 * @param offset Byte offset into the memory region.
 * @param value Scalar value to write.
 * @example
 *  bytes <- .Call("RC_read_bytes", ptr, 16L)
 */

/**
 * Copy nbytes from a pointer into a raw vector.
 * Ownership: returns R-managed raw vector.
 * Allocation: R alloc (PROTECT/UNPROTECT).
 * Protection: PROTECT(1), UNPROTECT(1).
 */
/* Copy nbytes from a pointer into a raw vector. */
SEXP RC_read_bytes(SEXP ptr, SEXP nbytes) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    int n = Rf_asInteger(nbytes);
    if (n < 0) {
        Rf_error("nbytes must be non-negative");
    }

    unsigned char *data = (unsigned char*)R_ExternalPtrAddr(ptr);
    if (!data) {
        return Rf_allocVector(RAWSXP, 0);
    }

    SEXP out = PROTECT(Rf_allocVector(RAWSXP, n));
    if (n > 0) {
        memcpy(RAW(out), data, (size_t)n);
    }
    UNPROTECT(1);
    return out;
}

/**
 * Copy a raw vector into memory at ptr. Caller must ensure sufficient space.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
/* Copy a raw vector into memory at ptr. Caller must ensure sufficient space. */
SEXP RC_write_bytes(SEXP ptr, SEXP raw) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    if (TYPEOF(raw) != RAWSXP) {
        Rf_error("Expected raw vector");
    }

    unsigned char *data = (unsigned char*)R_ExternalPtrAddr(ptr);
    if (!data) {
        Rf_error("Pointer is NULL");
    }
    R_xlen_t n = XLENGTH(raw);
    if (n > 0) {
        memcpy(data, RAW(raw), (size_t)n);
    }
    return R_NilValue;
}

// ============================================================================
// Typed read/write helpers (Bun-style)
// ============================================================================

/**
 * Helper: validate external pointer and compute base + byte_offset.
 * Ownership: borrowed pointer.
 * Allocation: none.
 * Protection: none.
 */
/* Helper: validate external pointer and compute base + byte_offset. */
static inline unsigned char *ptr_at(SEXP ptr, SEXP offset) {
    if (TYPEOF(ptr) != EXTPTRSXP) Rf_error("Expected external pointer");
    unsigned char *base = (unsigned char *)R_ExternalPtrAddr(ptr);
    if (!base) Rf_error("Pointer is NULL");
    return base + (size_t)Rf_asInteger(offset);
}

/* --- scalar reads -------------------------------------------------------- */

/**
 * Read int8 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_i8(SEXP ptr, SEXP offset) {
    int8_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

/**
 * Read uint8 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_u8_typed(SEXP ptr, SEXP offset) {
    uint8_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

/**
 * Read int16 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_i16(SEXP ptr, SEXP offset) {
    int16_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

/**
 * Read uint16 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_u16(SEXP ptr, SEXP offset) {
    uint16_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

/**
 * Read int32 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_i32_typed(SEXP ptr, SEXP offset) {
    int32_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger(v);
}

/**
 * Read uint32 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_u32(SEXP ptr, SEXP offset) {
    uint32_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

/**
 * Read int64 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_i64(SEXP ptr, SEXP offset) {
    int64_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

/**
 * Read uint64 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_u64(SEXP ptr, SEXP offset) {
    uint64_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

/**
 * Read float32 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_f32(SEXP ptr, SEXP offset) {
    float v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

/**
 * Read float64 at byte offset.
 * Ownership: returns R-managed scalar.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_read_f64_typed(SEXP ptr, SEXP offset) {
    double v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal(v);
}

/**
 * Read a pointer value at byte offset (dereference void**).
 * Ownership: returns borrowed external pointer.
 * Allocation: external pointer only.
 * Protection: none.
 */
/* Read a pointer value at byte offset (dereference void**). */
SEXP RC_read_ptr(SEXP ptr, SEXP offset) {
    void *v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return R_MakeExternalPtr(v, Rf_install("rtinycc_borrowed"), R_NilValue);
}

/* --- scalar writes ------------------------------------------------------- */

/**
 * Write int8 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_i8(SEXP ptr, SEXP offset, SEXP value) {
    int8_t v = (int8_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write uint8 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_u8(SEXP ptr, SEXP offset, SEXP value) {
    uint8_t v = (uint8_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write int16 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_i16(SEXP ptr, SEXP offset, SEXP value) {
    int16_t v = (int16_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write uint16 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_u16(SEXP ptr, SEXP offset, SEXP value) {
    uint16_t v = (uint16_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write int32 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_i32(SEXP ptr, SEXP offset, SEXP value) {
    int32_t v = (int32_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write uint32 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_u32(SEXP ptr, SEXP offset, SEXP value) {
    uint32_t v = (uint32_t)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write int64 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_i64(SEXP ptr, SEXP offset, SEXP value) {
    int64_t v = (int64_t)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write uint64 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_u64(SEXP ptr, SEXP offset, SEXP value) {
    uint64_t v = (uint64_t)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write float32 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_f32(SEXP ptr, SEXP offset, SEXP value) {
    float v = (float)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write float64 at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_f64(SEXP ptr, SEXP offset, SEXP value) {
    double v = Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

/**
 * Write pointer value at byte offset.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
SEXP RC_write_ptr(SEXP ptr, SEXP offset, SEXP value) {
    void *v = NULL;
    if (TYPEOF(value) == EXTPTRSXP) {
        v = R_ExternalPtrAddr(value);
    }
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

// ============================================================================
// Callback Registration and Invocation
// ============================================================================

/**
 * @section Callback registration and invocation
 * @param fun R function to register as a callback.
 * @param return_type Return type string (e.g. "i32", "f64", "void").
 * @param arg_types Character vector of argument types.
 * @param threadsafe Logical scalar indicating thread-safety.
 * @example
 *  cb <- .Call("RC_register_callback", fn, "i32", c("i32"), FALSE)
 */

/**
 * Find a free callback registry slot.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
// Find a free callback registry slot (returns -1 if none available)
static int RC_callback_find_free_slot() {
    for (int i = 0; i < MAX_CALLBACKS; i++) {
        if (!callback_registry[i].valid && callback_registry[i].fun == NULL) {
            return i;
        }
    }
    return -1;
}

/**
 * Initialize async callback queue.
 * @return R_NilValue on success.
 */
SEXP RC_callback_async_init() {
    if (!RC_platform_async_is_supported()) {
        Rf_error("Async callbacks are not supported on Windows");
    }
    if (RC_platform_async_init() != 0) {
        Rf_error("Failed to initialize async callback queue");
    }
    return R_NilValue;
}

/**
 * Schedule async callback execution on the main thread.
 * @param callback_ext tcc_callback external pointer.
 * @param args List of arguments to pass to the callback.
 * @return R_NilValue on success.
 */
SEXP RC_callback_async_schedule(SEXP callback_ext, SEXP args) {
    if (!RC_platform_async_is_supported()) {
        Rf_error("Async callbacks are not supported on Windows");
    }
    if (!Rf_inherits(callback_ext, "tcc_callback")) {
        Rf_error("Expected a 'tcc_callback' external pointer");
    }
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(callback_ext);
    if (!token) {
        Rf_error("Callback is closed");
    }
    if (token->id < 0 || token->id >= MAX_CALLBACKS || !callback_registry[token->id].valid) {
        Rf_error("Invalid or expired callback");
    }
    if (!RC_platform_async_is_initialized()) {
        Rf_error("Async callback queue is not initialized. Call tcc_callback_async_enable().");
    }

    int n_args = (args == R_NilValue) ? 0 : (int)XLENGTH(args);
    cb_arg_t *cb_args = NULL;
    if (n_args > 0) {
        cb_args = (cb_arg_t*) calloc((size_t)n_args, sizeof(cb_arg_t));
        if (!cb_args) {
            Rf_error("Out of memory");
        }
        for (int i = 0; i < n_args; i++) {
            SEXP val = VECTOR_ELT(args, i);
            if (Rf_isInteger(val) || TYPEOF(val) == INTSXP) {
                cb_args[i].kind = CB_ARG_INT;
                cb_args[i].v.i = Rf_asInteger(val);
            } else if (Rf_isLogical(val) || TYPEOF(val) == LGLSXP) {
                cb_args[i].kind = CB_ARG_LOGICAL;
                cb_args[i].v.i = Rf_asLogical(val);
            } else if (Rf_isReal(val) || TYPEOF(val) == REALSXP) {
                cb_args[i].kind = CB_ARG_REAL;
                cb_args[i].v.d = Rf_asReal(val);
            } else if (TYPEOF(val) == STRSXP) {
                const char *s = Rf_translateCharUTF8(STRING_ELT(val, 0));
                cb_args[i].kind = CB_ARG_CSTRING;
                cb_args[i].v.s = (char*) s;
            } else if (TYPEOF(val) == EXTPTRSXP) {
                cb_args[i].kind = CB_ARG_PTR;
                cb_args[i].v.p = R_ExternalPtrAddr(val);
            } else {
                free(cb_args);
                Rf_error("Unsupported async callback argument type");
            }
        }
    }

    int rc = RC_platform_async_schedule(token->id, n_args, cb_args);
    if (cb_args) {
        free(cb_args);
    }
    if (rc != 0) {
        Rf_error("Async callback scheduling failed");
    }
    return R_NilValue;
}

/**
 * Drain pending async callbacks on the main thread.
 * @return R_NilValue on success.
 */
SEXP RC_callback_async_drain() {
    if (!RC_platform_async_is_supported()) {
        Rf_error("Async callbacks are not supported on Windows");
    }
    RC_platform_async_drain();
    return R_NilValue;
}

/**
 * Schedule async callback from C worker threads.
 * @param id Callback registry id.
 * @param n_args Number of arguments.
 * @param args Array of cb_arg_t values.
 * @return 0 on success, negative error code otherwise.
 */
int RC_callback_async_schedule_c(int id, int n_args, const cb_arg_t *args) {
    if (!RC_platform_async_is_supported()) {
        return -1;
    }
    if (!RC_platform_async_is_initialized()) {
        return -1;
    }
    if (id < 0 || id >= MAX_CALLBACKS || !callback_registry[id].valid) {
        return -2;
    }
    return RC_platform_async_schedule(id, n_args, args);
}

/**
 * Finalizer for callback tokens.
 * Ownership: releases preserved R function and frees token when refs hit 0.
 * Allocation: frees heap memory (malloc/free).
 * Protection: none.
 */
// Finalizer for callback tokens
static void RC_callback_finalizer(SEXP ext) {
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(ext);
    if (token) {
        // Release the preserved R function
        if (token->id >= 0 && token->id < MAX_CALLBACKS) {
            callback_entry_t *entry = &callback_registry[token->id];
            if (entry->valid) {
                R_ReleaseObject(entry->fun);
                entry->fun = NULL;
                entry->valid = 0;
                if (entry->return_type) {
                    free(entry->return_type);
                    entry->return_type = NULL;
                }
                if (entry->arg_types) {
                    for (int i = 0; i < entry->n_args; i++) {
                        if (entry->arg_types[i]) {
                            free(entry->arg_types[i]);
                        }
                    }
                    free(entry->arg_types);
                    entry->arg_types = NULL;
                }
            }
        }
        token->id = -1;
        token->refs -= 1;
        if (token->refs <= 0) {
            free(token);
        }
        R_ClearExternalPtr(ext);
    }
}

/**
 * Finalizer for callback pointer handles.
 * Ownership: decrements refs and frees token when refs hit 0.
 * Allocation: frees heap memory (malloc/free).
 * Protection: none.
 */
// Finalizer for callback pointer handles
static void RC_callback_ptr_finalizer(SEXP ext) {
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(ext);
    if (token) {
        token->refs -= 1;
        if (token->refs <= 0) {
            free(token);
        }
        R_ClearExternalPtr(ext);
    }
}

/**
 * Register an R function as a callback.
 * @param fun R function to register.
 * @param return_type Return type string.
 * @param arg_types Character vector of argument types.
 * @param threadsafe Logical scalar indicating thread-safety.
 * @return tcc_callback external pointer.
 */
/* Register an R function as a callback. Returns an external pointer of
 * class "tcc_callback" holding a callback_token_t. */
SEXP RC_register_callback(SEXP fun, SEXP return_type, SEXP arg_types, SEXP threadsafe) {
    if (!Rf_isFunction(fun)) {
        Rf_error("Expected a function");
    }
    
    if (!Rf_isString(return_type) || XLENGTH(return_type) != 1) {
        Rf_error("Expected single string for return_type");
    }
    
    if (!Rf_isString(arg_types)) {
        Rf_error("Expected character vector for arg_types");
    }
    
    // Find an available slot
    int id = RC_callback_find_free_slot();
    if (id < 0) {
        Rf_error("Callback registry full (max %d)", MAX_CALLBACKS);
    }
    callback_entry_t *entry = &callback_registry[id];
    
    // Preserve the R function
    entry->fun = fun;
    R_PreserveObject(fun);
    
    // Store return type
    const char *rtype = Rf_translateCharUTF8(STRING_ELT(return_type, 0));
    entry->return_type = strdup(rtype);
    
    // Store argument types
    entry->n_args = (int)XLENGTH(arg_types);
    if (entry->n_args > 0) {
        entry->arg_types = malloc(entry->n_args * sizeof(char*));
        if (!entry->arg_types) {
            Rf_error("Memory allocation failed");
        }
        for (int i = 0; i < entry->n_args; i++) {
            const char *atype = Rf_translateCharUTF8(STRING_ELT(arg_types, i));
            entry->arg_types[i] = strdup(atype);
        }
    } else {
        entry->arg_types = NULL;
    }
    
    entry->threadsafe = Rf_asInteger(threadsafe);
    entry->valid = 1;
    entry->trampoline = NULL;
    
    // Create token
    callback_token_t *token = malloc(sizeof(callback_token_t));
    if (!token) {
        Rf_error("Memory allocation failed");
    }
    token->id = id;
    token->refs = 1;
    
    SEXP ext = PROTECT(R_MakeExternalPtr(token, R_NilValue, R_NilValue));
    Rf_setAttrib(ext, R_ClassSymbol, Rf_mkString("tcc_callback"));
    R_RegisterCFinalizerEx(ext, RC_callback_finalizer, FALSE);
    UNPROTECT(1);
    
    return ext;
}

/**
 * Unregister a callback, releasing the preserved R function.
 * @param callback_ext tcc_callback external pointer.
 * @return R_NilValue.
 */
/* Unregister a callback, releasing the preserved R function. */
SEXP RC_unregister_callback(SEXP callback_ext) {
    if (!Rf_inherits(callback_ext, "tcc_callback")) {
        Rf_error("Expected a 'tcc_callback' external pointer");
    }
    
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(callback_ext);
    if (!token) {
        Rf_error("Callback is already closed");
    }
    
    // Call finalizer directly to clean up resources
    // The finalizer will clear the external ptr and free the token
    RC_callback_finalizer(callback_ext);
    
    return R_NilValue;
}

/**
 * Return the callback token address as an external pointer (trampoline user-data).
 * @param callback_ext tcc_callback external pointer.
 * @return tcc_callback_ptr external pointer.
 */
/* Return the callback token address as an external pointer (user-data
 * for trampolines). Increments the token refcount. */
SEXP RC_get_callback_ptr(SEXP callback_ext) {
    if (!Rf_inherits(callback_ext, "tcc_callback")) {
        Rf_error("Expected a 'tcc_callback' external pointer");
    }
    
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(callback_ext);
    if (!token) {
        Rf_error("Callback is closed");
    }
    
    // Return the token address as external pointer (this is what trampolines use)
    token->refs += 1;
    SEXP ptr = PROTECT(R_MakeExternalPtr(token, R_NilValue, R_NilValue));
    Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("tcc_callback_ptr"));
    R_RegisterCFinalizerEx(ptr, RC_callback_ptr_finalizer, FALSE);
    UNPROTECT(1);
    return ptr;
}

/**
 * Check whether a callback is still valid (not closed, slot in range).
 * @param callback_ext tcc_callback external pointer.
 * @return Logical scalar.
 */
/* Check whether a callback is still valid (not closed, slot in range). */
SEXP RC_callback_is_valid(SEXP callback_ext) {
    if (!Rf_inherits(callback_ext, "tcc_callback")) {
        return Rf_ScalarLogical(FALSE);
    }
    
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(callback_ext);
    if (!token) {
        return Rf_ScalarLogical(FALSE);
    }
    
    if (token->id < 0 || token->id >= MAX_CALLBACKS) {
        return Rf_ScalarLogical(FALSE);
    }
    
    callback_entry_t *entry = &callback_registry[token->id];
    return Rf_ScalarLogical(entry->valid && entry->fun != NULL);
}

// Invoke callback from trampoline
// This is called by generated trampolines
/**
 * Produce a default SEXP for a callback return type.
 * Ownership: returns R-managed scalar or external pointer.
 * Allocation: R allocations only.
 * Protection: none.
 */
static SEXP RC_callback_default_sexp(const char *return_type) {
    if (return_type == NULL) {
        return R_NilValue;
    }

    if (strcmp(return_type, "void") == 0) {
        return R_NilValue;
    }

    if (strcmp(return_type, "int") == 0 ||
        strcmp(return_type, "i32") == 0 ||
        strcmp(return_type, "int32_t") == 0 ||
        strcmp(return_type, "i8") == 0 ||
        strcmp(return_type, "int8_t") == 0 ||
        strcmp(return_type, "i16") == 0 ||
        strcmp(return_type, "int16_t") == 0 ||
        strcmp(return_type, "u8") == 0 ||
        strcmp(return_type, "uint8_t") == 0 ||
        strcmp(return_type, "u16") == 0 ||
        strcmp(return_type, "uint16_t") == 0) {
        return Rf_ScalarInteger(NA_INTEGER);
    }

    if (strcmp(return_type, "i64") == 0 ||
        strcmp(return_type, "int64_t") == 0 ||
        strcmp(return_type, "u32") == 0 ||
        strcmp(return_type, "uint32_t") == 0 ||
        strcmp(return_type, "u64") == 0 ||
        strcmp(return_type, "uint64_t") == 0 ||
        strcmp(return_type, "double") == 0 ||
        strcmp(return_type, "f64") == 0 ||
        strcmp(return_type, "float") == 0 ||
        strcmp(return_type, "f32") == 0) {
        return Rf_ScalarReal(NA_REAL);
    }

    if (strcmp(return_type, "bool") == 0 ||
        strcmp(return_type, "logical") == 0) {
        return Rf_ScalarLogical(NA_LOGICAL);
    }

    if (strcmp(return_type, "sexp") == 0 ||
        strcmp(return_type, "SEXP") == 0) {
        return R_NilValue;
    }

    if (strcmp(return_type, "string") == 0 ||
        strcmp(return_type, "cstring") == 0 ||
        strcmp(return_type, "char*") == 0 ||
        strcmp(return_type, "const char*") == 0) {
        return Rf_ScalarString(NA_STRING);
    }

    if (strcmp(return_type, "ptr") == 0 ||
        strcmp(return_type, "void*") == 0 ||
        strcmp(return_type, "void *") == 0 ||
        (strstr(return_type, "*") != NULL && strstr(return_type, "char") == NULL)) {
        return R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
    }

    return R_NilValue;
}

/**
 * Invoke a callback by registry id.
 * @param id Callback registry id.
 * @param args List of arguments (VECSXP) or R_NilValue.
 * @return Converted result based on return type.
 */
SEXP RC_invoke_callback_internal(int id, SEXP args) {
    if (id < 0 || id >= MAX_CALLBACKS) {
        Rf_warning("Invalid or expired callback: %d", id);
        return R_NilValue;
    }

    callback_entry_t *entry = &callback_registry[id];
    if (!entry->valid) {
        Rf_warning("Invalid or expired callback: %d", id);
        if (entry->return_type) {
            return RC_callback_default_sexp(entry->return_type);
        }
        return R_NilValue;
    }

    // Build the call with the function as head and arguments following
    SEXP call = R_NilValue;
    if (args != R_NilValue && TYPEOF(args) == VECSXP) {
        int n_args = (int)XLENGTH(args);
        if (n_args == 0) {
            call = PROTECT(Rf_lang1(entry->fun));
        } else if (n_args == 1) {
            call = PROTECT(Rf_lang2(entry->fun, VECTOR_ELT(args, 0)));
        } else if (n_args == 2) {
            call = PROTECT(Rf_lang3(entry->fun, VECTOR_ELT(args, 0), VECTOR_ELT(args, 1)));
        } else if (n_args == 3) {
            call = PROTECT(Rf_lang4(entry->fun, VECTOR_ELT(args, 0), VECTOR_ELT(args, 1), VECTOR_ELT(args, 2)));
        } else {
            SEXP pair = R_NilValue;
            for (int i = n_args - 1; i >= 0; i--) {
                pair = PROTECT(Rf_cons(VECTOR_ELT(args, i), pair));
            }
            call = PROTECT(Rf_lcons(entry->fun, pair));
            UNPROTECT(n_args);
        }
    } else {
        call = PROTECT(Rf_lang1(entry->fun));
    }
    
    // Evaluate the call
    int err = 0;
    SEXP result = PROTECT(R_tryEvalSilent(call, R_GlobalEnv, &err));
    if (err) {
        UNPROTECT(2);  // call and result
        Rf_warning("Callback raised an error");
        return RC_callback_default_sexp(entry->return_type);
    }
    
    // Convert result based on expected return type
    SEXP converted = R_NilValue;
    if (strcmp(entry->return_type, "void") == 0) {
        converted = R_NilValue;
    } else if (strcmp(entry->return_type, "int") == 0 ||
               strcmp(entry->return_type, "i32") == 0 ||
               strcmp(entry->return_type, "int32_t") == 0 ||
               strcmp(entry->return_type, "i8") == 0 ||
               strcmp(entry->return_type, "int8_t") == 0 ||
               strcmp(entry->return_type, "i16") == 0 ||
               strcmp(entry->return_type, "int16_t") == 0) {
        converted = Rf_ScalarInteger(Rf_asInteger(result));
    } else if (strcmp(entry->return_type, "i64") == 0 ||
               strcmp(entry->return_type, "int64_t") == 0 ||
               strcmp(entry->return_type, "u32") == 0 ||
               strcmp(entry->return_type, "uint32_t") == 0 ||
               strcmp(entry->return_type, "u64") == 0 ||
               strcmp(entry->return_type, "uint64_t") == 0 ||
               strcmp(entry->return_type, "double") == 0 ||
               strcmp(entry->return_type, "f64") == 0 ||
               strcmp(entry->return_type, "float") == 0 ||
               strcmp(entry->return_type, "f32") == 0) {
        converted = Rf_ScalarReal(Rf_asReal(result));
    } else if (strcmp(entry->return_type, "u8") == 0 ||
               strcmp(entry->return_type, "uint8_t") == 0 ||
               strcmp(entry->return_type, "u16") == 0 ||
               strcmp(entry->return_type, "uint16_t") == 0) {
        converted = Rf_ScalarInteger(Rf_asInteger(result));
    } else if (strcmp(entry->return_type, "bool") == 0 ||
               strcmp(entry->return_type, "logical") == 0) {
        converted = Rf_ScalarLogical(Rf_asLogical(result));
    } else if (strcmp(entry->return_type, "sexp") == 0 ||
               strcmp(entry->return_type, "SEXP") == 0) {
        converted = result;
    } else if (strcmp(entry->return_type, "string") == 0 ||
               strcmp(entry->return_type, "cstring") == 0 ||
               strcmp(entry->return_type, "char*") == 0 ||
               strcmp(entry->return_type, "const char*") == 0) {
        if (Rf_isString(result)) {
            converted = result;
        } else {
            converted = Rf_ScalarString(Rf_asChar(result));
        }
    } else if (strcmp(entry->return_type, "ptr") == 0 ||
               strcmp(entry->return_type, "void*") == 0 ||
               strcmp(entry->return_type, "void *") == 0 ||
               (strstr(entry->return_type, "*") != NULL &&
                strstr(entry->return_type, "char") == NULL)) {
        if (TYPEOF(result) != EXTPTRSXP) {
            UNPROTECT(2);
            Rf_warning("Callback return is not an external pointer");
            return RC_callback_default_sexp(entry->return_type);
        }
        converted = result;
    } else {
        // Default: return as-is
        converted = result;
    }
    
    UNPROTECT(2);  // call and result
    return converted;
}

/**
 * Invoke callback by string id (trampoline entry point).
 * @param callback_id Character scalar id.
 * @param args List of arguments.
 * @return Converted result.
 */
SEXP RC_invoke_callback(SEXP callback_id, SEXP args) {
    // callback_id is a character string with the callback identifier
    const char *id_str = Rf_translateCharUTF8(STRING_ELT(callback_id, 0));
    int id = atoi(id_str);
    return RC_invoke_callback_internal(id, args);
}

/**
 * Invoke callback by integer id (no snprintf needed).
 * @param id Callback registry id.
 * @param args List of arguments.
 * @return Converted result.
 */
SEXP RC_invoke_callback_id(int id, SEXP args) {
    return RC_invoke_callback_internal(id, args);
}


/**
 * Cleanup all callbacks during package unload.
 * @return R_NilValue.
 */
/**
 * Cleanup all callbacks during package unload.
 * Ownership: releases preserved R functions and frees stored metadata.
 * Allocation: frees heap memory (malloc/free).
 * Protection: none.
 */
// Cleanup all callbacks during package unload
SEXP RC_cleanup_callbacks() {
    // Clean up all valid callbacks in the registry
    for (int i = 0; i < MAX_CALLBACKS; i++) {
        callback_entry_t *entry = &callback_registry[i];
        if (entry->valid && entry->fun != NULL) {
            R_ReleaseObject(entry->fun);
            entry->valid = 0;
            if (entry->return_type) {
                free(entry->return_type);
                entry->return_type = NULL;
            }
            if (entry->arg_types) {
                for (int j = 0; j < entry->n_args; j++) {
                    if (entry->arg_types[j]) {
                        free(entry->arg_types[j]);
                    }
                }
                free(entry->arg_types);
                entry->arg_types = NULL;
            }
        }
    }
    return R_NilValue;
}


// ============================================================================
// Host symbol registration
// ============================================================================

/**
 * @section Host symbol registration
 * @param ext External pointer to a TCC state.
 * @example
 *  .Call("RC_libtcc_add_host_symbols", state)
 */

/**
 * Register host symbols that TCC-compiled code may reference.
 * Ownership: none (borrows function pointers).
 * Allocation: none.
 * Protection: none.
 */
/* Register host symbols that TCC-compiled code may reference.
    On macOS (without -flat_namespace) these are not visible to TCC
    through the dynamic linker, so we must add them explicitly. */
SEXP RC_libtcc_add_host_symbols(SEXP ext) {
    TCCState *s = RC_tcc_state(ext);
    tcc_add_symbol(s, "RC_free_finalizer", RC_free_finalizer);
    tcc_add_symbol(s, "RC_invoke_callback", RC_invoke_callback);
    tcc_add_symbol(s, "RC_invoke_callback_id", RC_invoke_callback_id);
    tcc_add_symbol(s, "RC_callback_async_schedule_c",
                   RC_callback_async_schedule_c);
    return R_NilValue;
}