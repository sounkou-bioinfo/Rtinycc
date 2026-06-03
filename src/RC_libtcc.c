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
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// Forward declarations
// ============================================================================

static void RC_tcc_finalizer(SEXP ext);
static void RC_null_finalizer(SEXP ext);
static void RC_borrowed_view_finalizer(SEXP ext);
void RC_owned_native_finalizer(SEXP ext);
static const char *RC_extptr_tag_name(SEXP tag);
static int RC_trace_finalizers_enabled(void);
static void RC_trace_finalizer_event(const char *event, SEXP ext, SEXP owner, const char *detail);
void RC_free_finalizer(SEXP ext);
SEXP RC_make_borrowed_view(void *ptr, SEXP tag, SEXP owner);
SEXP RC_make_unowned_ptr(void *ptr, SEXP tag);
SEXP RC_make_owned_ptr(void *ptr, SEXP tag);
SEXP RC_make_owned_composite_ptr(void *ptr, SEXP tag);
void *RC_host_calloc_c(size_t n, size_t size);
void RC_host_free_c(void *ptr);

SEXP RC_libtcc_state_new(SEXP lib_path, SEXP include_path, SEXP output_type);
SEXP RC_libtcc_add_file(SEXP ext, SEXP path);
SEXP RC_libtcc_add_include_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_sysinclude_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library(SEXP ext, SEXP library);
SEXP RC_libtcc_set_options(SEXP ext, SEXP options);
SEXP RC_libtcc_compile_string(SEXP ext, SEXP code);
SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr);
SEXP RC_libtcc_relocate(SEXP ext);
SEXP RC_libtcc_get_symbol(SEXP ext, SEXP name);
SEXP RC_libtcc_list_symbols(SEXP ext);
SEXP RC_libtcc_call_symbol(SEXP ext, SEXP name, SEXP ret_type, SEXP args, SEXP naok_);
SEXP RC_libtcc_ptr_valid(SEXP ptr);
SEXP RC_libtcc_output_file(SEXP ext, SEXP filename);
SEXP RC_get_external_ptr_addr(SEXP ext);
SEXP RC_get_external_ptr_hex(SEXP ext);
SEXP RC_null_pointer(void);

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
static int RC_callback_find_free_slot(void);
SEXP RC_callback_async_init(void);
SEXP RC_callback_async_schedule(SEXP callback_ext, SEXP args);
SEXP RC_callback_async_drain(void);
SEXP RC_callback_async_failure_status(void);
SEXP RC_callback_async_failure_reset(void);
void RC_callback_async_note_failure_c(int code);
int RC_callback_async_schedule_c(int id, int n_args, const cb_arg_t *args);
int RC_callback_async_schedule_sync_c(int id, int n_args, const cb_arg_t *args, cb_result_t *result);
void RC_callback_async_drain_loop_c(volatile int *done_flag);
void RC_callback_async_exec_c(void (*func)(void *), void *arg);
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
SEXP RC_cleanup_callbacks(void);
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

/* Async trampoline failures can occur on worker threads, where R warnings and
 * other R API calls are not safe. Keep a small atomic diagnostic counter so the
 * failure path remains observable without touching the R API off-thread. */
static int callback_async_failure_count = 0;
static int callback_async_last_failure = 0;

static void RC_async_diag_increment(int *value) {
#if defined(__GNUC__) || defined(__clang__)
    (void)__sync_fetch_and_add(value, 1);
#else
    *value += 1;
#endif
}

static void RC_async_diag_store(int *value, int new_value) {
#if defined(__GNUC__) || defined(__clang__)
    (void)__sync_lock_test_and_set(value, new_value);
#else
    *value = new_value;
#endif
}

static int RC_async_diag_load(int *value) {
#if defined(__GNUC__) || defined(__clang__)
    return __sync_fetch_and_add(value, 0);
#else
    return *value;
#endif
}

// Callback token structure - passed back to R as external ptr
typedef struct {
    int id;                // Index into callback_registry (or -1 when closed)
    int refs;              // Reference count for outstanding external pointers
    int origin_id;         // Original registry slot for diagnostics after close
} callback_token_t;

// ============================================================================
// Shutdown and finalizers
// ============================================================================

/**
 * Releases a TCCState when its R external pointer is garbage collected.
 * Ownership: frees owned TCCState.
 * Allocation: none.
 * Protection: none.
 */
static void RC_tcc_finalizer(SEXP ext) {
    void *ptr = R_ExternalPtrAddr(ext);
    if (ptr) {
        TCCState *s = (TCCState*)ptr;
#ifdef _WIN32
        /* On Windows, libtcc teardown unloads DLL handles during tcc_delete().
           That is safe at process exit, but repeated GC-driven finalization of
           relocated states has been observed to crash the live R session.
           Leave the state untouched here and let process teardown reclaim it. */
        RC_trace_finalizer_event("tcc_state:skip_delete_windows", ext, R_ExternalPtrProtected(ext), NULL);
        (void)s;
#else
        RC_trace_finalizer_event("tcc_state:delete", ext, R_ExternalPtrProtected(ext), NULL);
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
    RC_trace_finalizer_event("null_clear", ext, R_ExternalPtrProtected(ext), NULL);
    R_ClearExternalPtr(ext);
}

/**
 * Finalizer for borrowed views that explicitly preserve an owner object.
 * Ownership: drops the owner link; does not free pointee storage.
 * Allocation: none.
 * Protection: none.
 */
static void RC_borrowed_view_finalizer(SEXP ext) {
    SEXP owner = R_ExternalPtrProtected(ext);
    RC_trace_finalizer_event("borrowed_release", ext, owner, NULL);
    if (owner != R_NilValue) {
        R_ReleaseObject(owner);
        R_SetExternalPtrProtected(ext, R_NilValue);
    }
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
        RC_trace_finalizer_event("free", ext, R_ExternalPtrProtected(ext), NULL);
        free(ptr);
        R_ClearExternalPtr(ext);
    }
}

void *RC_host_calloc_c(size_t n, size_t size) {
    void *ptr = calloc(n, size);
    if (RC_trace_finalizers_enabled()) {
        Rprintf("[RTINYCC_TRACE_FINALIZERS] host_calloc ptr=%p n=%llu size=%llu\n",
                ptr,
                (unsigned long long)n,
                (unsigned long long)size);
    }
    return ptr;
}

void RC_host_free_c(void *ptr) {
    if (RC_trace_finalizers_enabled()) {
        Rprintf("[RTINYCC_TRACE_FINALIZERS] host_free ptr=%p\n", ptr);
    }
    free(ptr);
}

/**
 * Finalizer for owned native allocations created by generated composite helpers.
 * Ownership: frees owned heap memory on Unix; Windows finalizer clears only.
 * Allocation: none.
 * Protection: none.
 */
void RC_owned_native_finalizer(SEXP ext) {
    void *ptr = R_ExternalPtrAddr(ext);
    if (!ptr) {
        return;
    }
#ifdef _WIN32
    RC_trace_finalizer_event("owned_native:skip_free_windows", ext, R_ExternalPtrProtected(ext), NULL);
    R_ClearExternalPtr(ext);
#else
    RC_free_finalizer(ext);
#endif
}

/**
 * Create a borrowed external pointer view with optional owner retention.
 * Ownership: borrowed; does not free pointee storage.
 * Allocation: external pointer only.
 * Protection: attaches owner in the external pointer protected slot.
 */
SEXP RC_make_borrowed_view(void *ptr, SEXP tag, SEXP owner) {
    SEXP resolved_tag = tag == R_NilValue ? Rf_install("rtinycc_borrowed") : tag;
    SEXP ext = PROTECT(R_MakeExternalPtr(ptr, resolved_tag, R_NilValue));

    if (owner != R_NilValue) {
        R_PreserveObject(owner);
        R_SetExternalPtrProtected(ext, owner);
        R_RegisterCFinalizerEx(ext, RC_borrowed_view_finalizer, FALSE);
    } else {
        R_RegisterCFinalizerEx(ext, RC_null_finalizer, FALSE);
    }

    UNPROTECT(1);
    return ext;
}

SEXP RC_make_unowned_ptr(void *ptr, SEXP tag) {
    SEXP resolved_tag = tag == R_NilValue ? Rf_install("rtinycc_borrowed") : tag;
    SEXP ext = PROTECT(R_MakeExternalPtr(ptr, resolved_tag, R_NilValue));
    R_RegisterCFinalizerEx(ext, RC_null_finalizer, FALSE);
    UNPROTECT(1);
    return ext;
}

SEXP RC_make_owned_ptr(void *ptr, SEXP tag) {
    SEXP resolved_tag = tag == R_NilValue ? Rf_install("rtinycc_owned") : tag;
    SEXP ext = PROTECT(R_MakeExternalPtr(ptr, resolved_tag, R_NilValue));
    R_RegisterCFinalizerEx(ext, RC_free_finalizer, FALSE);
    UNPROTECT(1);
    return ext;
}

SEXP RC_make_owned_composite_ptr(void *ptr, SEXP tag) {
    SEXP resolved_tag = tag == R_NilValue ? Rf_install("rtinycc_owned") : tag;
    SEXP ext = PROTECT(R_MakeExternalPtr(ptr, resolved_tag, R_NilValue));
    R_RegisterCFinalizerEx(ext, RC_owned_native_finalizer, FALSE);
    UNPROTECT(1);
    return ext;
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

static void RC_tcc_error_callback(void *opaque, const char *msg) {
    (void)opaque;
    if (msg && msg[0]) {
        Rprintf("%s\n", msg);
    }
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

    /* Route libtcc diagnostics through R without treating diagnostics as printf formats. */
    tcc_set_error_func(s, NULL, RC_tcc_error_callback);

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
    SEXP class_str = PROTECT(Rf_mkString("tcc_state"));
    Rf_setAttrib(ext, R_ClassSymbol, class_str);
    UNPROTECT(1);
    R_SetExternalPtrTag(ext, Rf_install("rtinycc_tcc_state_owned"));
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
 * Apply raw compiler options to an existing TCC state (tcc_set_options).
 * Ownership: none.
 * Allocation: handled by TCC.
 * Protection: none.
 */
SEXP RC_libtcc_set_options(SEXP ext, SEXP options) {
    TCCState *s = RC_tcc_state(ext);
    const char *opt = Rf_translateCharUTF8(STRING_ELT(options, 0));
    int rc = tcc_set_options(s, opt);
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
    SEXP class_str = PROTECT(Rf_mkString("tcc_symbol"));
    Rf_setAttrib(ptr, R_ClassSymbol, class_str);
    UNPROTECT(1);
    UNPROTECT(1);
    return ptr;
}

typedef struct {
    R_xlen_t count;
} RC_tcc_symbol_count_t;

typedef struct {
    SEXP names;
    SEXP addresses;
    R_xlen_t index;
} RC_tcc_symbol_collect_t;

static void RC_libtcc_count_symbol_cb(void *ctx, const char *name, const void *val) {
    (void) name;
    (void) val;
    RC_tcc_symbol_count_t *counter = (RC_tcc_symbol_count_t *) ctx;
    counter->count++;
}

static void RC_libtcc_collect_symbol_cb(void *ctx, const char *name, const void *val) {
    RC_tcc_symbol_collect_t *collect = (RC_tcc_symbol_collect_t *) ctx;
    R_xlen_t i = collect->index++;
    if (i >= XLENGTH(collect->names)) {
        return;
    }

    char addr_buf[2 + (sizeof(uintptr_t) * 2) + 1];
    if (val == NULL) {
        snprintf(addr_buf, sizeof(addr_buf), "0x0");
    } else {
        snprintf(addr_buf, sizeof(addr_buf), "0x%" PRIxPTR, (uintptr_t) val);
    }

    SET_STRING_ELT(
        collect->names,
        i,
        Rf_mkCharCE(name == NULL ? "" : name, CE_UTF8)
    );
    SET_STRING_ELT(collect->addresses, i, Rf_mkChar(addr_buf));
}

/**
 * List global symbols known to a TCC state.
 * Ownership: none.
 * Allocation: result data frame only.
 * Protection: PROTECT(6), UNPROTECT(6).
 */
SEXP RC_libtcc_list_symbols(SEXP ext) {
    TCCState *s = RC_tcc_state(ext);

    RC_tcc_symbol_count_t counter = {0};
    tcc_list_symbols(s, &counter, RC_libtcc_count_symbol_cb);
    if (counter.count > INT_MAX) {
        Rf_error("too many TCC symbols to return as a data frame");
    }

    SEXP names_vec = PROTECT(Rf_allocVector(STRSXP, counter.count));
    SEXP addresses_vec = PROTECT(Rf_allocVector(STRSXP, counter.count));

    RC_tcc_symbol_collect_t collect = {names_vec, addresses_vec, 0};
    tcc_list_symbols(s, &collect, RC_libtcc_collect_symbol_cb);

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(out, 0, names_vec);
    SET_VECTOR_ELT(out, 1, addresses_vec);

    SEXP out_names = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(out_names, 0, Rf_mkChar("name"));
    SET_STRING_ELT(out_names, 1, Rf_mkChar("address"));
    Rf_setAttrib(out, R_NamesSymbol, out_names);

    SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -(int) counter.count;
    Rf_setAttrib(out, R_RowNamesSymbol, row_names);

    SEXP class_str = PROTECT(Rf_mkString("data.frame"));
    Rf_setAttrib(out, R_ClassSymbol, class_str);

    UNPROTECT(6);
    return out;
}

/**
 * Call a TCC symbol.
 *
 * With no pointer arguments this preserves the historical quick-test helper
 * semantics: call a zero-argument function and box an int/double/void return.
 * With one or more arguments this follows R's .C-style convention: the target
 * function is called as a void routine receiving pointers to mutable argument
 * buffers, and the return value is a list mirroring the input arguments.
 *
 * Ownership: all temporary C buffers are R_alloc-managed.
 * Allocation: R result objects and R_alloc buffers.
 * Protection: protects result list and temporary output vectors until attached.
 */
#define RC_DOTC_MAX_ARGS 65
#define RC_DOTC_CHAR_BUFSIZE_MIN 128
#define RC_DOTC_GUARD_BYTES 64
#define RC_DOTC_GUARD_FILL 0xee

typedef enum {
    RC_DOTC_ARG_DIRECT = 0,
    RC_DOTC_ARG_GUARDED = 1,
    RC_DOTC_ARG_LOGICAL = 2,
    RC_DOTC_ARG_SINGLE = 3,
    RC_DOTC_ARG_CHARACTER = 4
} RC_dotc_arg_kind_t;

typedef struct {
    RC_dotc_arg_kind_t kind;
    SEXP original;
    SEXP out;
    R_xlen_t n;
    void *ptr;
    unsigned char *base;
    size_t nbytes;
    unsigned char **char_bases;
    size_t *char_caps;
} RC_dotc_arg_t;

static unsigned char *RC_dotc_alloc_guarded(size_t nbytes, unsigned char **base_out) {
    if (nbytes > SIZE_MAX - 2 * RC_DOTC_GUARD_BYTES) {
        Rf_error("TCC foreign function argument is too large");
    }
    size_t total = nbytes + 2 * RC_DOTC_GUARD_BYTES;
    unsigned char *base = (unsigned char *)R_alloc(total, 1);
    memset(base, RC_DOTC_GUARD_FILL, total);
    *base_out = base;
    return base + RC_DOTC_GUARD_BYTES;
}

static void RC_dotc_check_guarded(const unsigned char *base, size_t nbytes,
                                  int arg_index, const char *what) {
    const unsigned char *pre = base;
    const unsigned char *post = base + RC_DOTC_GUARD_BYTES + nbytes;
    for (int i = 0; i < RC_DOTC_GUARD_BYTES; i++) {
        if (pre[i] != RC_DOTC_GUARD_FILL) {
            Rf_error("array under-run in TCC foreign function call (%s argument %d)", what, arg_index);
        }
        if (post[i] != RC_DOTC_GUARD_FILL) {
            Rf_error("array over-run in TCC foreign function call (%s argument %d)", what, arg_index);
        }
    }
}

static int RC_dotc_has_nul(const char *s, size_t cap) {
    for (size_t i = 0; i < cap; i++) {
        if (s[i] == '\0') return 1;
    }
    return 0;
}

typedef void (*RC_DOTC_FUNV0)(void);
typedef void (*RC_DOTC_FUNV1)(void *);
typedef void (*RC_DOTC_FUNV2)(void *, void *);
typedef void (*RC_DOTC_FUNV3)(void *, void *, void *);
typedef void (*RC_DOTC_FUNV4)(void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV5)(void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV6)(void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV7)(void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV8)(void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV9)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV10)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV11)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV12)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV13)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV14)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV15)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV16)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV17)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV18)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV19)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV20)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV21)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV22)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV23)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV24)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV25)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV26)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV27)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV28)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV29)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV30)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV31)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV32)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV33)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV34)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV35)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV36)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV37)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV38)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV39)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV40)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV41)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV42)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV43)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV44)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV45)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV46)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV47)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV48)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV49)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV50)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV51)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV52)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV53)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV54)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV55)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV56)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV57)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV58)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV59)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV60)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV61)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV62)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV63)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV64)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
typedef void (*RC_DOTC_FUNV65)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static void RC_libtcc_call_void_symbol(uintptr_t addr, int nargs, void **cargs) {
    switch (nargs) {
    case 0:
        ((RC_DOTC_FUNV0)addr)();
        break;
    case 1:
        ((RC_DOTC_FUNV1)addr)(cargs[0]);
        break;
    case 2:
        ((RC_DOTC_FUNV2)addr)(cargs[0], cargs[1]);
        break;
    case 3:
        ((RC_DOTC_FUNV3)addr)(cargs[0], cargs[1], cargs[2]);
        break;
    case 4:
        ((RC_DOTC_FUNV4)addr)(cargs[0], cargs[1], cargs[2], cargs[3]);
        break;
    case 5:
        ((RC_DOTC_FUNV5)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4]);
        break;
    case 6:
        ((RC_DOTC_FUNV6)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5]);
        break;
    case 7:
        ((RC_DOTC_FUNV7)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6]);
        break;
    case 8:
        ((RC_DOTC_FUNV8)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7]);
        break;
    case 9:
        ((RC_DOTC_FUNV9)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8]);
        break;
    case 10:
        ((RC_DOTC_FUNV10)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9]);
        break;
    case 11:
        ((RC_DOTC_FUNV11)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10]);
        break;
    case 12:
        ((RC_DOTC_FUNV12)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11]);
        break;
    case 13:
        ((RC_DOTC_FUNV13)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12]);
        break;
    case 14:
        ((RC_DOTC_FUNV14)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13]);
        break;
    case 15:
        ((RC_DOTC_FUNV15)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14]);
        break;
    case 16:
        ((RC_DOTC_FUNV16)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15]);
        break;
    case 17:
        ((RC_DOTC_FUNV17)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16]);
        break;
    case 18:
        ((RC_DOTC_FUNV18)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17]);
        break;
    case 19:
        ((RC_DOTC_FUNV19)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18]);
        break;
    case 20:
        ((RC_DOTC_FUNV20)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19]);
        break;
    case 21:
        ((RC_DOTC_FUNV21)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20]);
        break;
    case 22:
        ((RC_DOTC_FUNV22)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21]);
        break;
    case 23:
        ((RC_DOTC_FUNV23)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22]);
        break;
    case 24:
        ((RC_DOTC_FUNV24)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23]);
        break;
    case 25:
        ((RC_DOTC_FUNV25)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24]);
        break;
    case 26:
        ((RC_DOTC_FUNV26)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25]);
        break;
    case 27:
        ((RC_DOTC_FUNV27)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26]);
        break;
    case 28:
        ((RC_DOTC_FUNV28)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27]);
        break;
    case 29:
        ((RC_DOTC_FUNV29)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28]);
        break;
    case 30:
        ((RC_DOTC_FUNV30)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29]);
        break;
    case 31:
        ((RC_DOTC_FUNV31)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30]);
        break;
    case 32:
        ((RC_DOTC_FUNV32)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31]);
        break;
    case 33:
        ((RC_DOTC_FUNV33)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32]);
        break;
    case 34:
        ((RC_DOTC_FUNV34)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33]);
        break;
    case 35:
        ((RC_DOTC_FUNV35)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34]);
        break;
    case 36:
        ((RC_DOTC_FUNV36)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35]);
        break;
    case 37:
        ((RC_DOTC_FUNV37)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36]);
        break;
    case 38:
        ((RC_DOTC_FUNV38)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37]);
        break;
    case 39:
        ((RC_DOTC_FUNV39)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38]);
        break;
    case 40:
        ((RC_DOTC_FUNV40)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39]);
        break;
    case 41:
        ((RC_DOTC_FUNV41)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40]);
        break;
    case 42:
        ((RC_DOTC_FUNV42)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41]);
        break;
    case 43:
        ((RC_DOTC_FUNV43)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42]);
        break;
    case 44:
        ((RC_DOTC_FUNV44)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43]);
        break;
    case 45:
        ((RC_DOTC_FUNV45)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44]);
        break;
    case 46:
        ((RC_DOTC_FUNV46)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45]);
        break;
    case 47:
        ((RC_DOTC_FUNV47)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46]);
        break;
    case 48:
        ((RC_DOTC_FUNV48)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47]);
        break;
    case 49:
        ((RC_DOTC_FUNV49)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48]);
        break;
    case 50:
        ((RC_DOTC_FUNV50)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49]);
        break;
    case 51:
        ((RC_DOTC_FUNV51)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50]);
        break;
    case 52:
        ((RC_DOTC_FUNV52)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51]);
        break;
    case 53:
        ((RC_DOTC_FUNV53)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52]);
        break;
    case 54:
        ((RC_DOTC_FUNV54)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53]);
        break;
    case 55:
        ((RC_DOTC_FUNV55)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54]);
        break;
    case 56:
        ((RC_DOTC_FUNV56)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55]);
        break;
    case 57:
        ((RC_DOTC_FUNV57)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56]);
        break;
    case 58:
        ((RC_DOTC_FUNV58)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57]);
        break;
    case 59:
        ((RC_DOTC_FUNV59)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57], cargs[58]);
        break;
    case 60:
        ((RC_DOTC_FUNV60)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57], cargs[58], cargs[59]);
        break;
    case 61:
        ((RC_DOTC_FUNV61)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57], cargs[58], cargs[59], cargs[60]);
        break;
    case 62:
        ((RC_DOTC_FUNV62)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57], cargs[58], cargs[59], cargs[60], cargs[61]);
        break;
    case 63:
        ((RC_DOTC_FUNV63)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57], cargs[58], cargs[59], cargs[60], cargs[61], cargs[62]);
        break;
    case 64:
        ((RC_DOTC_FUNV64)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57], cargs[58], cargs[59], cargs[60], cargs[61], cargs[62], cargs[63]);
        break;
    case 65:
        ((RC_DOTC_FUNV65)addr)(cargs[0], cargs[1], cargs[2], cargs[3], cargs[4], cargs[5], cargs[6], cargs[7], cargs[8], cargs[9], cargs[10], cargs[11], cargs[12], cargs[13], cargs[14], cargs[15], cargs[16], cargs[17], cargs[18], cargs[19], cargs[20], cargs[21], cargs[22], cargs[23], cargs[24], cargs[25], cargs[26], cargs[27], cargs[28], cargs[29], cargs[30], cargs[31], cargs[32], cargs[33], cargs[34], cargs[35], cargs[36], cargs[37], cargs[38], cargs[39], cargs[40], cargs[41], cargs[42], cargs[43], cargs[44], cargs[45], cargs[46], cargs[47], cargs[48], cargs[49], cargs[50], cargs[51], cargs[52], cargs[53], cargs[54], cargs[55], cargs[56], cargs[57], cargs[58], cargs[59], cargs[60], cargs[61], cargs[62], cargs[63], cargs[64]);
        break;
    default:
        Rf_error("too many arguments in TCC symbol call (maximum is %d)", RC_DOTC_MAX_ARGS);
    }
}

static void RC_libtcc_check_dotc_na(SEXP x, int arg_index, int naok) {
    if (naok) return;

    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP: {
        const int *p = INTEGER(x);
        R_xlen_t n = XLENGTH(x);
        for (R_xlen_t i = 0; i < n; i++) {
            if (p[i] == NA_INTEGER) {
                Rf_error("NAs in TCC foreign function call (arg %d)", arg_index);
            }
        }
        break;
    }
    case REALSXP: {
        const double *p = REAL(x);
        R_xlen_t n = XLENGTH(x);
        for (R_xlen_t i = 0; i < n; i++) {
            if (!R_FINITE(p[i])) {
                Rf_error("NA/NaN/Inf in TCC foreign function call (arg %d)", arg_index);
            }
        }
        break;
    }
    case CPLXSXP: {
        const Rcomplex *p = COMPLEX(x);
        R_xlen_t n = XLENGTH(x);
        for (R_xlen_t i = 0; i < n; i++) {
            if (!R_FINITE(p[i].r) || !R_FINITE(p[i].i)) {
                Rf_error("complex NA/NaN/Inf in TCC foreign function call (arg %d)", arg_index);
            }
        }
        break;
    }
    default:
        break;
    }
}

static int RC_libtcc_is_csingle(SEXP x) {
    static SEXP csingle_symbol = NULL;
    if (csingle_symbol == NULL) {
        csingle_symbol = Rf_install("Csingle");
    }
    return Rf_asLogical(Rf_getAttrib(x, csingle_symbol)) == TRUE;
}

static void RC_libtcc_copy_attrib(SEXP to, SEXP from) {
    SHALLOW_DUPLICATE_ATTRIB(to, from);
}

static R_xlen_t RC_libtcc_dotc_arg_length(SEXP x, int arg_index) {
    switch (TYPEOF(x)) {
    case RAWSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP: {
        R_xlen_t n = XLENGTH(x);
        if (n > INT_MAX) {
            Rf_error("long vectors (argument %d) are not supported in TCC foreign function calls", arg_index);
        }
        return n;
    }
    default:
        return 0;
    }
}

static void RC_libtcc_prepare_dotc_arg(SEXP x, int arg_index, int naok,
                                       SEXP ans, void **cargs,
                                       RC_dotc_arg_t *meta) {
    RC_libtcc_check_dotc_na(x, arg_index, naok);

    meta->kind = RC_DOTC_ARG_DIRECT;
    meta->original = x;
    meta->out = x;
    meta->n = RC_libtcc_dotc_arg_length(x, arg_index);
    meta->ptr = NULL;
    meta->base = NULL;
    meta->nbytes = 0;
    meta->char_bases = NULL;
    meta->char_caps = NULL;

    switch (TYPEOF(x)) {
    case RAWSXP: {
        SEXP out = PROTECT(Rf_allocVector(RAWSXP, meta->n));
        RC_libtcc_copy_attrib(out, x);
        SET_VECTOR_ELT(ans, arg_index - 1, out);
        meta->kind = RC_DOTC_ARG_GUARDED;
        meta->out = out;
        meta->nbytes = (size_t)meta->n * sizeof(Rbyte);
        meta->ptr = RC_dotc_alloc_guarded(meta->nbytes, &meta->base);
        if (meta->nbytes) memcpy(meta->ptr, RAW(x), meta->nbytes);
        cargs[arg_index - 1] = meta->ptr;
        UNPROTECT(1);
        break;
    }
    case LGLSXP: {
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, meta->n));
        RC_libtcc_copy_attrib(out, x);
        SET_VECTOR_ELT(ans, arg_index - 1, out);
        meta->kind = RC_DOTC_ARG_LOGICAL;
        meta->out = out;
        meta->nbytes = (size_t)meta->n * sizeof(int);
        meta->ptr = RC_dotc_alloc_guarded(meta->nbytes, &meta->base);
        if (meta->nbytes) memcpy(meta->ptr, LOGICAL(x), meta->nbytes);
        cargs[arg_index - 1] = meta->ptr;
        UNPROTECT(1);
        break;
    }
    case INTSXP: {
        SEXP out = PROTECT(Rf_allocVector(INTSXP, meta->n));
        RC_libtcc_copy_attrib(out, x);
        SET_VECTOR_ELT(ans, arg_index - 1, out);
        meta->kind = RC_DOTC_ARG_GUARDED;
        meta->out = out;
        meta->nbytes = (size_t)meta->n * sizeof(int);
        meta->ptr = RC_dotc_alloc_guarded(meta->nbytes, &meta->base);
        if (meta->nbytes) memcpy(meta->ptr, INTEGER(x), meta->nbytes);
        cargs[arg_index - 1] = meta->ptr;
        UNPROTECT(1);
        break;
    }
    case REALSXP: {
        if (RC_libtcc_is_csingle(x)) {
            SEXP out = PROTECT(Rf_allocVector(REALSXP, meta->n));
            RC_libtcc_copy_attrib(out, x);
            SET_VECTOR_ELT(ans, arg_index - 1, out);
            meta->kind = RC_DOTC_ARG_SINGLE;
            meta->out = out;
            meta->nbytes = (size_t)meta->n * sizeof(float);
            meta->ptr = RC_dotc_alloc_guarded(meta->nbytes, &meta->base);
            float *buf = (float *)meta->ptr;
            for (R_xlen_t i = 0; i < meta->n; i++) buf[i] = (float)REAL(x)[i];
            cargs[arg_index - 1] = meta->ptr;
            UNPROTECT(1);
        } else {
            SEXP out = PROTECT(Rf_allocVector(REALSXP, meta->n));
            RC_libtcc_copy_attrib(out, x);
            SET_VECTOR_ELT(ans, arg_index - 1, out);
            meta->kind = RC_DOTC_ARG_GUARDED;
            meta->out = out;
            meta->nbytes = (size_t)meta->n * sizeof(double);
            meta->ptr = RC_dotc_alloc_guarded(meta->nbytes, &meta->base);
            if (meta->nbytes) memcpy(meta->ptr, REAL(x), meta->nbytes);
            cargs[arg_index - 1] = meta->ptr;
            UNPROTECT(1);
        }
        break;
    }
    case CPLXSXP: {
        SEXP out = PROTECT(Rf_allocVector(CPLXSXP, meta->n));
        RC_libtcc_copy_attrib(out, x);
        SET_VECTOR_ELT(ans, arg_index - 1, out);
        meta->kind = RC_DOTC_ARG_GUARDED;
        meta->out = out;
        meta->nbytes = (size_t)meta->n * sizeof(Rcomplex);
        meta->ptr = RC_dotc_alloc_guarded(meta->nbytes, &meta->base);
        if (meta->nbytes) memcpy(meta->ptr, COMPLEX(x), meta->nbytes);
        cargs[arg_index - 1] = meta->ptr;
        UNPROTECT(1);
        break;
    }
    case STRSXP: {
        char **cptr = (char **)R_alloc((size_t)meta->n, sizeof(char *));
        meta->char_bases = (unsigned char **)R_alloc((size_t)meta->n, sizeof(unsigned char *));
        meta->char_caps = (size_t *)R_alloc((size_t)meta->n, sizeof(size_t));
        for (R_xlen_t i = 0; i < meta->n; i++) {
            const char *src = (STRING_ELT(x, i) == NA_STRING) ? "NA" : Rf_translateChar(STRING_ELT(x, i));
            size_t len = strlen(src) + 1;
            size_t cap = len < RC_DOTC_CHAR_BUFSIZE_MIN ? RC_DOTC_CHAR_BUFSIZE_MIN : len;
            unsigned char *base = NULL;
            char *buf = (char *)RC_dotc_alloc_guarded(cap, &base);
            memset(buf, 0, cap);
            memcpy(buf, src, len);
            cptr[i] = buf;
            meta->char_bases[i] = base;
            meta->char_caps[i] = cap;
        }
        SEXP out = PROTECT(Rf_allocVector(STRSXP, meta->n));
        RC_libtcc_copy_attrib(out, x);
        SET_VECTOR_ELT(ans, arg_index - 1, out);
        meta->kind = RC_DOTC_ARG_CHARACTER;
        meta->out = out;
        meta->ptr = cptr;
        cargs[arg_index - 1] = meta->ptr;
        UNPROTECT(1);
        break;
    }
    case VECSXP: {
        /* Lists are a read-only legacy .C path.  For ordinary materialized
         * vectors, DATAPTR_OR_NULL() gives the existing SEXP* array without
         * forcing ALTREP materialization.  For ALTREP or otherwise opaque
         * vectors, rebuild a call-lifetime SEXP* view element-by-element. */
        const void *data = DATAPTR_OR_NULL(x);
        SET_VECTOR_ELT(ans, arg_index - 1, x);
        if (data) {
            meta->ptr = (void *)data;
        } else {
            SEXP *lptr = (SEXP *)R_alloc((size_t)meta->n, sizeof(SEXP));
            for (R_xlen_t i = 0; i < meta->n; i++) lptr[i] = VECTOR_ELT(x, i);
            meta->ptr = lptr;
        }
        cargs[arg_index - 1] = meta->ptr;
        break;
    }
    case NILSXP:
        Rf_error("invalid mode (%s) to pass to TCC foreign function (arg %d)",
                 Rf_type2char(TYPEOF(x)), arg_index);
        break;
    default:
        SET_VECTOR_ELT(ans, arg_index - 1, x);
        meta->ptr = (void *)x;
        cargs[arg_index - 1] = meta->ptr;
        break;
    }
}

static void RC_libtcc_finish_dotc_arg(RC_dotc_arg_t *meta, int arg_index) {
    switch (meta->kind) {
    case RC_DOTC_ARG_GUARDED:
        RC_dotc_check_guarded(meta->base, meta->nbytes, arg_index, Rf_type2char(TYPEOF(meta->out)));
        switch (TYPEOF(meta->out)) {
        case RAWSXP:
            if (meta->nbytes) memcpy(RAW(meta->out), meta->ptr, meta->nbytes);
            break;
        case INTSXP:
            if (meta->nbytes) memcpy(INTEGER(meta->out), meta->ptr, meta->nbytes);
            break;
        case REALSXP:
            if (meta->nbytes) memcpy(REAL(meta->out), meta->ptr, meta->nbytes);
            break;
        case CPLXSXP:
            if (meta->nbytes) memcpy(COMPLEX(meta->out), meta->ptr, meta->nbytes);
            break;
        default:
            break;
        }
        break;
    case RC_DOTC_ARG_LOGICAL: {
        RC_dotc_check_guarded(meta->base, meta->nbytes, arg_index, "logical");
        int *p = (int *)meta->ptr;
        int *out = LOGICAL(meta->out);
        for (R_xlen_t i = 0; i < meta->n; i++) {
            int v = p[i];
            out[i] = (v == NA_INTEGER || v == 0) ? v : 1;
        }
        break;
    }
    case RC_DOTC_ARG_SINGLE: {
        RC_dotc_check_guarded(meta->base, meta->nbytes, arg_index, "single");
        float *p = (float *)meta->ptr;
        for (R_xlen_t i = 0; i < meta->n; i++) REAL(meta->out)[i] = (double)p[i];
        break;
    }
    case RC_DOTC_ARG_CHARACTER: {
        char **p = (char **)meta->ptr;
        for (R_xlen_t i = 0; i < meta->n; i++) {
            RC_dotc_check_guarded(meta->char_bases[i], meta->char_caps[i], arg_index, "character");
            char *expected = (char *)(meta->char_bases[i] + RC_DOTC_GUARD_BYTES);
            if (p[i] != expected) {
                Rf_error("character pointer replacement is not supported in TCC foreign function call (argument %d, element %d)",
                         arg_index, (int)i + 1);
            }
            if (!RC_dotc_has_nul(p[i], meta->char_caps[i])) {
                Rf_error("unterminated string in TCC foreign function call (character argument %d, element %d)",
                         arg_index, (int)i + 1);
            }
            SET_STRING_ELT(meta->out, i, Rf_mkChar(p[i]));
        }
        break;
    }
    case RC_DOTC_ARG_DIRECT:
    default:
        break;
    }
}

static SEXP RC_libtcc_call_symbol_dotc(uintptr_t addr, SEXP args, int naok) {
    if (TYPEOF(args) != VECSXP) {
        Rf_error("internal error: TCC symbol arguments must be a list");
    }

    R_xlen_t nargs_x = XLENGTH(args);
    if (nargs_x > RC_DOTC_MAX_ARGS) {
        Rf_error("too many arguments in TCC symbol call (maximum is %d)", RC_DOTC_MAX_ARGS);
    }
    int nargs = (int)nargs_x;

    SEXP ans = PROTECT(Rf_allocVector(VECSXP, nargs));
    SEXP names = Rf_getAttrib(args, R_NamesSymbol);
    if (names != R_NilValue) {
        Rf_setAttrib(ans, R_NamesSymbol, names);
    }

    void **cargs = (void **)R_alloc((size_t)nargs, sizeof(void *));
    RC_dotc_arg_t *meta = (RC_dotc_arg_t *)R_alloc((size_t)nargs, sizeof(RC_dotc_arg_t));

    for (int i = 0; i < nargs; i++) {
        RC_libtcc_prepare_dotc_arg(VECTOR_ELT(args, i), i + 1, naok, ans, cargs, &meta[i]);
    }

    RC_libtcc_call_void_symbol(addr, nargs, cargs);

    for (int i = 0; i < nargs; i++) {
        RC_libtcc_finish_dotc_arg(&meta[i], i + 1);
    }

    UNPROTECT(1);
    return ans;
}

/* Call a TCC symbol. ret_type controls the legacy zero-argument scalar path;
 * when args is non-empty the function is called with .C-style pointer
 * arguments and must be declared void. */
SEXP RC_libtcc_call_symbol(SEXP ext, SEXP name, SEXP ret_type, SEXP args, SEXP naok_) {
    TCCState *s = RC_tcc_state(ext);
    if (!Rf_isString(name) || XLENGTH(name) != 1) {
        Rf_error("symbol name must be a character scalar");
    }
    if (!Rf_isString(ret_type) || XLENGTH(ret_type) != 1) {
        Rf_error("return type must be a character scalar");
    }
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    const char *rtype = Rf_translateCharUTF8(STRING_ELT(ret_type, 0));
    void *fn = tcc_get_symbol(s, sym);
    if (!fn) {
        Rf_error("symbol '%s' not found", sym);
    }

    uintptr_t addr = (uintptr_t) fn;
    int nargs = (args == R_NilValue) ? 0 : (int)XLENGTH(args);
    if (nargs > 0) {
        if (strcmp(rtype, "void") != 0) {
            Rf_error("TCC .C-style symbol calls with arguments require return = 'void'");
        }
        int naok = Rf_asLogical(naok_) == TRUE;
        return RC_libtcc_call_symbol_dotc(addr, args, naok);
    }
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

static const char *RC_extptr_tag_name(SEXP tag) {
    if (tag == R_NilValue) {
        return "<nil>";
    }
    if (TYPEOF(tag) == SYMSXP) {
        return CHAR(PRINTNAME(tag));
    }
    return "<non-symbol>";
}

static int RC_trace_finalizers_enabled(void) {
    const char *value = getenv("RTINYCC_TRACE_FINALIZERS");
    if (!value || !value[0]) {
        return 0;
    }
    return strcmp(value, "0") != 0 &&
        strcmp(value, "false") != 0 &&
        strcmp(value, "FALSE") != 0 &&
        strcmp(value, "no") != 0 &&
        strcmp(value, "NO") != 0;
}

static void RC_trace_finalizer_event(const char *event, SEXP ext, SEXP owner, const char *detail) {
    if (!RC_trace_finalizers_enabled()) {
        return;
    }

    const char *tag_name = "<non-externalptr>";
    void *ptr = NULL;
    if (TYPEOF(ext) == EXTPTRSXP) {
        ptr = R_ExternalPtrAddr(ext);
        tag_name = RC_extptr_tag_name(R_ExternalPtrTag(ext));
    }

    const char *owner_tag = "<nil>";
    void *owner_ptr = NULL;
    void *owner_sexp_ptr = NULL;
    if (owner != R_NilValue) {
        owner_sexp_ptr = (void *) owner;
        if (TYPEOF(owner) == EXTPTRSXP) {
            owner_ptr = R_ExternalPtrAddr(owner);
            owner_tag = RC_extptr_tag_name(R_ExternalPtrTag(owner));
        } else {
            owner_tag = type2char(TYPEOF(owner));
        }
    }

    Rprintf(
        "[RTINYCC_TRACE_FINALIZERS] %s ext=%p ptr=%p tag=%s owner=%p owner_ptr=%p owner_tag=%s%s%s\n",
        event,
        (void *) ext,
        ptr,
        tag_name,
        owner_sexp_ptr,
        owner_ptr,
        owner_tag,
        detail ? " detail=" : "",
        detail ? detail : ""
    );
}

/**
 * Return pointer address as a hex string ("0x...").
 * Ownership: returns a new R string (R-managed).
 * Allocation: R string allocation only.
 * Protection: none.
 */
/* Return pointer address as a hex string ("0x..."). */
SEXP RC_get_external_ptr_hex(SEXP ext) {
    void *raw = R_ExternalPtrAddr(ext);
    uintptr_t addr = (uintptr_t) raw;
    char buf[2 + (sizeof(uintptr_t) * 2) + 1];
    /* Use PRIxPTR for portable formatting */
    if (raw == NULL) {
        snprintf(buf, sizeof(buf), "0x0");
    } else {
        snprintf(buf, sizeof(buf), "0x%" PRIxPTR, addr);
    }
    return Rf_mkString(buf);
}

/**
 * Create a NULL external pointer tagged "rtinycc_null".
 * Ownership: returned external pointer is owned by R.
 * Allocation: external pointer.
 * Protection: none.
 */
/* Create a NULL external pointer tagged "rtinycc_null". */
SEXP RC_null_pointer(void) {
    SEXP ptr = PROTECT(R_MakeExternalPtr(NULL, Rf_install("rtinycc_null"), R_NilValue));
    R_RegisterCFinalizerEx(ptr, RC_null_finalizer, FALSE);
    UNPROTECT(1);
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
    
    SEXP ptr = PROTECT(R_MakeExternalPtr(data, Rf_install("rtinycc_owned"), R_NilValue));
    if (RC_trace_finalizers_enabled()) {
        Rprintf("[RTINYCC_TRACE_FINALIZERS] malloc ext=%p ptr=%p size=%d\n", (void*)ptr, data, sz);
    }
    R_RegisterCFinalizerEx(ptr, RC_free_finalizer, FALSE);
    UNPROTECT(1);
    return ptr;
}

/**
 * Free an owned pointer. Errors if tag is not rtinycc_owned.
 * Ownership: releases owned heap memory.
 * Allocation: none.
 * Protection: none.
 */
/* Free an owned pointer. Errors unless the tag is exactly
 * "rtinycc_owned" (i.e. refuses borrowed, struct, callback, and other
 * non-owned pointers). */
SEXP RC_free(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }

    SEXP tag = R_ExternalPtrTag(ptr);
    if (tag != Rf_install("rtinycc_owned")) {
        Rf_error(
            "Pointer tag '%s' is not owned by Rtinycc; refusing to free",
            RC_extptr_tag_name(tag)
        );
    }
    
    void *data = R_ExternalPtrAddr(ptr);
    if (data) {
        RC_trace_finalizer_event("explicit_free", ptr, R_ExternalPtrProtected(ptr), NULL);
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
        SEXP out = PROTECT(R_MakeExternalPtr(NULL, Rf_install("rtinycc_borrowed"), R_NilValue));
        R_RegisterCFinalizerEx(out, RC_null_finalizer, FALSE);
        UNPROTECT(1);
        return out;
    }

    void *data = *((void**)ref);
    SEXP out = PROTECT(R_MakeExternalPtr(data, Rf_install("rtinycc_borrowed"), R_NilValue));
    R_RegisterCFinalizerEx(out, RC_null_finalizer, FALSE);
    UNPROTECT(1);
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
    
    SEXP ptr = PROTECT(R_MakeExternalPtr(data, Rf_install("rtinycc_owned"), R_NilValue));
    if (RC_trace_finalizers_enabled()) {
        Rprintf("[RTINYCC_TRACE_FINALIZERS] cstring ext=%p ptr=%p len=%llu\n", (void*)ptr, data, (unsigned long long)strlen(c_str));
    }
    R_RegisterCFinalizerEx(ptr, RC_free_finalizer, FALSE);
    UNPROTECT(1);
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

    SET_STRING_ELT(out, 0, Rf_mkCharLenCE(data, n, CE_UTF8));
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
        R_xlen_t copied = RAW_GET_REGION(raw, 0, n, (Rbyte*)data);
        if (copied != n) {
            Rf_error("failed to read raw vector");
        }
    }
    return R_NilValue;
}

// ============================================================================
// Typed read/write helpers (Bun-style)
// ============================================================================

static size_t rtinycc_offset_to_size_t(SEXP offset) {
    double off = Rf_asReal(offset);
    if (ISNA(off) || ISNAN(off)) {
        Rf_error("offset must be a finite non-negative whole number");
    }
    if (off < 0 || trunc(off) != off) {
        Rf_error("offset must be a finite non-negative whole number");
    }
    if (off > (double)SIZE_MAX) {
        Rf_error("offset out of range");
    }
    return (size_t)off;
}

static uint32_t rtinycc_as_u32_value(SEXP value) {
    double v = Rf_asReal(value);
    if (ISNA(v) || ISNAN(v)) {
        Rf_error("numeric value is NA");
    }
    if (v < 0 || v > (double)UINT32_MAX) {
        Rf_error("u32 out of range");
    }
    if (trunc(v) != v) {
        Rf_error("u32 requires integer value");
    }
    return (uint32_t)v;
}

static int64_t rtinycc_as_i64_value(SEXP value) {
    double v = Rf_asReal(value);
    if (ISNA(v) || ISNAN(v)) {
        Rf_error("numeric value is NA");
    }
    if (fabs(v) > 9007199254740992.0) {
        Rf_error("i64 requires exact integer (|x| <= 2^53)");
    }
    if (trunc(v) != v) {
        Rf_error("i64 requires integer value");
    }
    return (int64_t)v;
}

static uint64_t rtinycc_as_u64_value(SEXP value) {
    double v = Rf_asReal(value);
    if (ISNA(v) || ISNAN(v)) {
        Rf_error("numeric value is NA");
    }
    if (v < 0) {
        Rf_error("u64 out of range");
    }
    if (v > 9007199254740992.0) {
        Rf_error("u64 requires exact integer (|x| <= 2^53)");
    }
    if (trunc(v) != v) {
        Rf_error("u64 requires integer value");
    }
    return (uint64_t)v;
}

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
    return base + rtinycc_offset_to_size_t(offset);
}

/* --- scalar reads & writes ----------------------------------------------- */

#define DEFINE_READ_SCALAR(FUN_NAME, C_TYPE, R_BOX_FUN, CAST_TYPE) \
SEXP FUN_NAME(SEXP ptr, SEXP offset) { \
    C_TYPE v; \
    memcpy(&v, ptr_at(ptr, offset), sizeof(v)); \
    return R_BOX_FUN((CAST_TYPE)v); \
}

#define DEFINE_WRITE_SCALAR(FUN_NAME, C_TYPE, R_UNBOX_FUN) \
SEXP FUN_NAME(SEXP ptr, SEXP offset, SEXP value) { \
    C_TYPE v = (C_TYPE)R_UNBOX_FUN(value); \
    memcpy(ptr_at(ptr, offset), &v, sizeof(v)); \
    return R_NilValue; \
}

DEFINE_READ_SCALAR(RC_read_i8, int8_t, Rf_ScalarInteger, int)
DEFINE_READ_SCALAR(RC_read_u8_typed, uint8_t, Rf_ScalarInteger, int)
DEFINE_READ_SCALAR(RC_read_i16, int16_t, Rf_ScalarInteger, int)
DEFINE_READ_SCALAR(RC_read_u16, uint16_t, Rf_ScalarInteger, int)
DEFINE_READ_SCALAR(RC_read_i32_typed, int32_t, Rf_ScalarInteger, int)
DEFINE_READ_SCALAR(RC_read_u32, uint32_t, Rf_ScalarReal, double)
DEFINE_READ_SCALAR(RC_read_i64, int64_t, Rf_ScalarReal, double)
DEFINE_READ_SCALAR(RC_read_u64, uint64_t, Rf_ScalarReal, double)
DEFINE_READ_SCALAR(RC_read_f32, float, Rf_ScalarReal, double)
DEFINE_READ_SCALAR(RC_read_f64_typed, double, Rf_ScalarReal, double)

/* Read a pointer value at byte offset (dereference void**). */
SEXP RC_read_ptr(SEXP ptr, SEXP offset) {
    void *v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return RC_make_unowned_ptr(v, Rf_install("rtinycc_borrowed"));
}

DEFINE_WRITE_SCALAR(RC_write_i8, int8_t, Rf_asInteger)
DEFINE_WRITE_SCALAR(RC_write_u8, uint8_t, Rf_asInteger)
DEFINE_WRITE_SCALAR(RC_write_i16, int16_t, Rf_asInteger)
DEFINE_WRITE_SCALAR(RC_write_u16, uint16_t, Rf_asInteger)
DEFINE_WRITE_SCALAR(RC_write_i32, int32_t, Rf_asInteger)
DEFINE_WRITE_SCALAR(RC_write_u32, uint32_t, rtinycc_as_u32_value)
DEFINE_WRITE_SCALAR(RC_write_i64, int64_t, rtinycc_as_i64_value)
DEFINE_WRITE_SCALAR(RC_write_u64, uint64_t, rtinycc_as_u64_value)
DEFINE_WRITE_SCALAR(RC_write_f32, float, Rf_asReal)
DEFINE_WRITE_SCALAR(RC_write_f64, double, Rf_asReal)

/* Write pointer value at byte offset. */
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
static int RC_callback_find_free_slot(void) {
    for (int i = 0; i < MAX_CALLBACKS; i++) {
        if (!callback_registry[i].valid && callback_registry[i].fun == NULL) {
            return i;
        }
    }
    return -1;
}

/**
 * Initialize async callback queue.
 * Idempotent — safe to call multiple times.  Called automatically at
 * package load (R_init_Rtinycc) but kept in the .Call table so R code
 * can re-init after an unload/reload cycle if needed.
 * @return R_NilValue on success.
 */
SEXP RC_callback_async_init(void) {
    RC_platform_async_init();
    return R_NilValue;
}

/**
 * Schedule async callback execution on the main thread.
 * @param callback_ext tcc_callback external pointer.
 * @param args List of arguments to pass to the callback.
 * @return R_NilValue on success.
 */
SEXP RC_callback_async_schedule(SEXP callback_ext, SEXP args) {
    if (!RC_platform_async_is_initialized()) {
        RC_platform_async_init();
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
                /* Safe borrowed window: no R allocations happen before the
                 * platform layer duplicates CSTRING args for cross-thread use. */
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
SEXP RC_callback_async_drain(void) {
    if (!RC_platform_async_is_initialized()) {
        RC_platform_async_init();
    }
    RC_platform_async_drain();
    return R_NilValue;
}

void RC_callback_async_note_failure_c(int code) {
    RC_async_diag_increment(&callback_async_failure_count);
    RC_async_diag_store(&callback_async_last_failure, code);
}

SEXP RC_callback_async_failure_status(void) {
    SEXP out = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(out)[0] = RC_async_diag_load(&callback_async_failure_count);
    INTEGER(out)[1] = RC_async_diag_load(&callback_async_last_failure);
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, Rf_mkChar("count"));
    SET_STRING_ELT(names, 1, Rf_mkChar("last_code"));
    Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(2);
    return out;
}

SEXP RC_callback_async_failure_reset(void) {
    RC_async_diag_store(&callback_async_failure_count, 0);
    RC_async_diag_store(&callback_async_last_failure, 0);
    return R_NilValue;
}

static SEXP RC_callback_async_args_to_sexp(int n_args, const cb_arg_t *args) {
    if (n_args <= 0) {
        return R_NilValue;
    }
    SEXP out = PROTECT(Rf_allocVector(VECSXP, n_args));
    for (int i = 0; i < n_args; i++) {
        const cb_arg_t *a = &args[i];
        switch (a->kind) {
            case CB_ARG_INT:
                SET_VECTOR_ELT(out, i, Rf_ScalarInteger(a->v.i));
                break;
            case CB_ARG_REAL:
                SET_VECTOR_ELT(out, i, Rf_ScalarReal(a->v.d));
                break;
            case CB_ARG_LOGICAL:
                SET_VECTOR_ELT(out, i, Rf_ScalarLogical(a->v.i));
                break;
            case CB_ARG_PTR:
                SET_VECTOR_ELT(out, i, RC_make_unowned_ptr(a->v.p, R_NilValue));
                break;
            case CB_ARG_CSTRING:
                SET_VECTOR_ELT(out, i, Rf_mkString(a->v.s ? a->v.s : ""));
                break;
        }
    }
    UNPROTECT(1);
    return out;
}

static cb_result_t RC_callback_async_result_from_sexp(SEXP s) {
    cb_result_t r;
    memset(&r, 0, sizeof(r));
    r.kind = CB_RESULT_VOID;
    if (s == R_NilValue) return r;
    if (Rf_isInteger(s) && XLENGTH(s) >= 1) {
        r.kind = CB_RESULT_INT;
        r.v.i = Rf_asInteger(s);
    } else if (Rf_isReal(s) && XLENGTH(s) >= 1) {
        r.kind = CB_RESULT_REAL;
        r.v.d = Rf_asReal(s);
    } else if (Rf_isLogical(s) && XLENGTH(s) >= 1) {
        r.kind = CB_RESULT_LOGICAL;
        r.v.i = Rf_asLogical(s);
    } else if (TYPEOF(s) == EXTPTRSXP) {
        r.kind = CB_RESULT_PTR;
        r.v.p = R_ExternalPtrAddr(s);
    }
    return r;
}

static int RC_callback_async_invoke_now(int id, int n_args, const cb_arg_t *args, cb_result_t *result) {
    /* Return value is dispatch status. Callback-level R errors are converted
     * by RC_invoke_callback_internal() into the declared default result. */
    SEXP r_args = PROTECT(RC_callback_async_args_to_sexp(n_args, args));
    SEXP res = PROTECT(RC_invoke_callback_internal(id, r_args));
    if (result) {
        *result = RC_callback_async_result_from_sexp(res);
    }
    UNPROTECT(2);
    return 0;
}

/**
 * Schedule async callback from C worker threads.
 * @param id Callback registry id.
 * @param n_args Number of arguments.
 * @param args Array of cb_arg_t values.
 * @return 0 on success, negative error code otherwise.
 */
int RC_callback_async_schedule_c(int id, int n_args, const cb_arg_t *args) {
    if (!RC_platform_async_is_initialized()) {
        RC_platform_async_init();
    }
    if (!RC_platform_async_is_initialized()) {
        return -1;
    }
    if (id < 0 || id >= MAX_CALLBACKS || !callback_registry[id].valid) {
        return -2;
    }
    if (RC_platform_async_is_main_thread()) {
        return RC_callback_async_invoke_now(id, n_args, args, NULL);
    }
    return RC_platform_async_schedule(id, n_args, args);
}

/**
 * Schedule a synchronous async callback from C worker threads.
 * Blocks the calling thread until the main thread executes the R callback
 * and returns a result in *result.
 * @param id Callback registry id.
 * @param n_args Number of arguments.
 * @param args Array of cb_arg_t values.
 * @param result Output: filled with the callback's return value.
 * @return 0 on success, negative error code otherwise.
 */
int RC_callback_async_schedule_sync_c(int id, int n_args, const cb_arg_t *args,
                                      cb_result_t *result) {
    if (!RC_platform_async_is_initialized()) {
        RC_platform_async_init();
    }
    if (!RC_platform_async_is_initialized()) {
        return -1;
    }
    if (id < 0 || id >= MAX_CALLBACKS || !callback_registry[id].valid) {
        return -2;
    }
    if (RC_platform_async_is_main_thread()) {
        return RC_callback_async_invoke_now(id, n_args, args, result);
    }
    return RC_platform_async_schedule_sync(id, n_args, args, result);
}

/**
 * Drain pending async callbacks in a polling loop until *done_flag != 0.
 * Delegates to RC_platform_async_drain_loop().
 * This is the TCC-visible entry point; the platform layer does the real work.
 * @param done_flag Pointer to a volatile int that the worker sets to non-zero
 *                  when it finishes.
 */
void RC_callback_async_drain_loop_c(volatile int *done_flag) {
    if (!RC_platform_async_is_initialized()) {
        RC_platform_async_init();
    }
    RC_platform_async_drain_loop(done_flag);
}

/**
 * Run func(arg) on a new thread while draining callbacks on the main thread.
 * Returns after func finishes and the thread is joined.
 * Generated wrappers use this so user C code never touches drain mechanics.
 */
void RC_callback_async_exec_c(void (*func)(void *), void *arg) {
    RC_platform_async_init();
    RC_platform_async_exec(func, arg);
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
        char detail[128];
        snprintf(
            detail,
            sizeof(detail),
            "id=%d origin_id=%d refs=%d valid=%d",
            token->id,
            token->origin_id,
            token->refs,
            (token->id >= 0 && token->id < MAX_CALLBACKS) ? callback_registry[token->id].valid : 0
        );
        RC_trace_finalizer_event("callback_release", ext, R_NilValue, detail);
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
        char detail[128];
        snprintf(detail, sizeof(detail), "id=%d origin_id=%d refs=%d", token->id, token->origin_id, token->refs);
        RC_trace_finalizer_event("callback_ptr_release", ext, R_NilValue, detail);
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
    token->origin_id = id;
    
    SEXP ext = PROTECT(R_MakeExternalPtr(token, R_NilValue, R_NilValue));
    SEXP class_str = PROTECT(Rf_mkString("tcc_callback"));
    Rf_setAttrib(ext, R_ClassSymbol, class_str);
    UNPROTECT(1);
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
    SEXP class_str = PROTECT(Rf_mkString("tcc_callback_ptr"));
    Rf_setAttrib(ptr, R_ClassSymbol, class_str);
    UNPROTECT(1);
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
static int rtinycc_is_cstring_type_name(const char *type_name) {
    if (!type_name) {
        return 0;
    }
    return strcmp(type_name, "string") == 0 ||
           strcmp(type_name, "cstring") == 0 ||
           strcmp(type_name, "char*") == 0 ||
           strcmp(type_name, "char *") == 0 ||
           strcmp(type_name, "const char*") == 0 ||
           strcmp(type_name, "const char *") == 0;
}

static int rtinycc_is_pointer_type_name(const char *type_name) {
    if (!type_name) {
        return 0;
    }
    return strcmp(type_name, "ptr") == 0 ||
           strcmp(type_name, "void*") == 0 ||
           strcmp(type_name, "void *") == 0 ||
           (strstr(type_name, "*") != NULL &&
            !rtinycc_is_cstring_type_name(type_name));
}

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

    if (rtinycc_is_cstring_type_name(return_type)) {
        return Rf_ScalarString(NA_STRING);
    }

    if (rtinycc_is_pointer_type_name(return_type)) {
        SEXP ptr = PROTECT(R_MakeExternalPtr(NULL, Rf_install("rtinycc_null"), R_NilValue));
        UNPROTECT(1);
        return ptr;
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
            UNPROTECT(n_args + 1);
            PROTECT(call);
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
        double numeric_result = Rf_asReal(result);
        if ((strcmp(entry->return_type, "i64") == 0 ||
             strcmp(entry->return_type, "int64_t") == 0) &&
            !ISNA(numeric_result) &&
            !ISNAN(numeric_result) &&
            fabs(numeric_result) > 9007199254740992.0) {
            Rf_warning("callback i64 precision loss in R numeric");
        }
        if ((strcmp(entry->return_type, "u64") == 0 ||
             strcmp(entry->return_type, "uint64_t") == 0) &&
            !ISNA(numeric_result) &&
            !ISNAN(numeric_result) &&
            numeric_result > 9007199254740992.0) {
            Rf_warning("callback u64 precision loss in R numeric");
        }
        converted = Rf_ScalarReal(numeric_result);
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
    } else if (rtinycc_is_cstring_type_name(entry->return_type)) {
        if (Rf_isString(result)) {
            converted = result;
        } else {
            converted = Rf_ScalarString(Rf_asChar(result));
        }
    } else if (rtinycc_is_pointer_type_name(entry->return_type)) {
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
SEXP RC_cleanup_callbacks(void) {
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
/* libtcc stores all symbol addresses as const void*, including callable C
   symbols.  ISO C does not define a portable function-pointer/object-pointer
   conversion, but the platforms supported by Rtinycc use equal-sized code and
   data pointers for dynamically resolved symbols.  Copy the representation
   through memory rather than spelling a function-pointer-to-object-pointer
   cast, which keeps -Wpedantic builds quiet while preserving the libtcc ABI. */
static int RC_tcc_add_function_symbol(TCCState *s, const char *name, DL_FUNC fn) {
    const void *addr = NULL;
    if (sizeof(addr) != sizeof(fn)) {
        Rf_error("cannot register host symbol '%s': function pointer size differs from object pointer size", name);
    }
    memcpy(&addr, &fn, sizeof(addr));
    return tcc_add_symbol(s, name, addr);
}

SEXP RC_libtcc_add_host_symbols(SEXP ext) {
    TCCState *s = RC_tcc_state(ext);
    RC_tcc_add_function_symbol(s, "RC_free_finalizer", (DL_FUNC) RC_free_finalizer);
    RC_tcc_add_function_symbol(s, "RC_owned_native_finalizer", (DL_FUNC) RC_owned_native_finalizer);
    RC_tcc_add_function_symbol(s, "RC_make_borrowed_view", (DL_FUNC) RC_make_borrowed_view);
    RC_tcc_add_function_symbol(s, "RC_make_unowned_ptr", (DL_FUNC) RC_make_unowned_ptr);
    RC_tcc_add_function_symbol(s, "RC_make_owned_ptr", (DL_FUNC) RC_make_owned_ptr);
    RC_tcc_add_function_symbol(s, "RC_make_owned_composite_ptr", (DL_FUNC) RC_make_owned_composite_ptr);
    RC_tcc_add_function_symbol(s, "RC_host_calloc_c", (DL_FUNC) RC_host_calloc_c);
    RC_tcc_add_function_symbol(s, "RC_host_free_c", (DL_FUNC) RC_host_free_c);
    RC_tcc_add_function_symbol(s, "RC_invoke_callback", (DL_FUNC) RC_invoke_callback);
    RC_tcc_add_function_symbol(s, "RC_invoke_callback_id", (DL_FUNC) RC_invoke_callback_id);
    RC_tcc_add_function_symbol(s, "RC_callback_async_schedule_c",
                               (DL_FUNC) RC_callback_async_schedule_c);
    RC_tcc_add_function_symbol(s, "RC_callback_async_note_failure_c",
                               (DL_FUNC) RC_callback_async_note_failure_c);
    RC_tcc_add_function_symbol(s, "RC_callback_async_schedule_sync_c",
                               (DL_FUNC) RC_callback_async_schedule_sync_c);
    /* Expose drain to TCC-compiled C code so the main-thread C side can
       service pending async callbacks without returning to R. */
    RC_tcc_add_function_symbol(s, "RC_callback_async_drain_c",
                               (DL_FUNC) RC_platform_async_drain);
    /* Drain loop: blocks main thread servicing callbacks via select()/
       MsgWaitForMultipleObjects until *done_flag becomes non-zero.
       Zero latency wakeup, zero CPU waste while idle. */
    RC_tcc_add_function_symbol(s, "RC_callback_async_drain_loop_c",
                               (DL_FUNC) RC_callback_async_drain_loop_c);
    /* Exec: run func(arg) on a new thread, drain on main thread until done.
       Used by generated wrappers so user code never touches drain. */
    RC_tcc_add_function_symbol(s, "RC_callback_async_exec_c",
                               (DL_FUNC) RC_callback_async_exec_c);
    return R_NilValue;
}
