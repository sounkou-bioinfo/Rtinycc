/* Rtinycc - TinyCC for R
 * Copyright (C) 2025-2026 Sounkou Mahamane Toure
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include "libtcc.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <R_ext/Print.h>
#include <R_ext/Parse.h>
#ifndef _WIN32
#include <R_ext/eventloop.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#endif
#include <stdint.h>
#include <inttypes.h>
#include <string.h>

/* Set during package unload to avoid teardown crashes on some platforms. */
static volatile int g_rtinycc_shutting_down = 0;

/* Set shutdown flag from R (.onUnload). */
SEXP RC_set_shutting_down(SEXP flag) {
    g_rtinycc_shutting_down = Rf_asLogical(flag) ? 1 : 0;
    return R_NilValue;
}

/* Releases a TCCState when its R external pointer is garbage collected. */
static void RC_tcc_finalizer(SEXP ext) {
    if (g_rtinycc_shutting_down) {
        R_ClearExternalPtr(ext);
        return;
    }
    void *ptr = R_ExternalPtrAddr(ext);
    if (ptr) {
        TCCState *s = (TCCState*)ptr;
        tcc_delete(s);
        R_ClearExternalPtr(ext);
    }
}

/* No-op finalizer for borrowed / NULL pointers. */
static void RC_null_finalizer(SEXP ext) {
    // NULL pointer doesn't need cleanup
    R_ClearExternalPtr(ext);
}

/* Generic finalizer: calls free() on the pointer and clears the EXTPTR.
 * Used for owned pointers (tcc_malloc, tcc_cstring) and struct helpers. */
void RC_free_finalizer(SEXP ext) {
    if (g_rtinycc_shutting_down) {
        R_ClearExternalPtr(ext);
        return;
    }
    void *ptr = R_ExternalPtrAddr(ext);
    if (ptr) {
        free(ptr);
        R_ClearExternalPtr(ext);
    }
}

/* Unwrap and validate a tcc_state external pointer. */
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

/* Create a new TCCState, configure output type, and add include/lib paths.
 * Returns an external pointer of class "tcc_state". */
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

/* Add a source or object file to the compilation. */
SEXP RC_libtcc_add_file(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *fname = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_file(s, fname);
    return Rf_ScalarInteger(rc);
}

/* Append a user include path (-I). */
SEXP RC_libtcc_add_include_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_include_path(s, p);
    return Rf_ScalarInteger(rc);
}

/* Append a system include path (-isystem). */
SEXP RC_libtcc_add_sysinclude_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_sysinclude_path(s, p);
    return Rf_ScalarInteger(rc);
}

/* Append a library search path (-L). */
SEXP RC_libtcc_add_library_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_library_path(s, p);
    return Rf_ScalarInteger(rc);
}

/* Link a library (-l). */
SEXP RC_libtcc_add_library(SEXP ext, SEXP library) {
    TCCState *s = RC_tcc_state(ext);
    const char *lib = Rf_translateCharUTF8(STRING_ELT(library, 0));
    int rc = tcc_add_library(s, lib);
    return Rf_ScalarInteger(rc);
}



/* Compile a C source string in-memory. Returns 0 on success. */
SEXP RC_libtcc_compile_string(SEXP ext, SEXP code) {
    TCCState *s = RC_tcc_state(ext);
     const char *src = Rf_translateCharUTF8(STRING_ELT(code, 0));
    int rc = tcc_compile_string(s, src);
    return Rf_ScalarInteger(rc);
}

/* Register a host symbol so TCC-compiled code can reference it. */
SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr) {
    TCCState *s = RC_tcc_state(ext);
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    void *ptr = R_ExternalPtrAddr(addr);
    int rc = tcc_add_symbol(s, sym, ptr);
    return Rf_ScalarInteger(rc);
}

/* Relocate compiled code, resolving all symbols. Returns 0 on success. */
SEXP RC_libtcc_relocate(SEXP ext) {
    TCCState *s = RC_tcc_state(ext);
    int rc = tcc_relocate(s);
    return Rf_ScalarInteger(rc);
}

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

/* Return TRUE if the external pointer address is non-NULL. */
SEXP RC_libtcc_ptr_valid(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("expected an external pointer");
    }
    void *p = R_ExternalPtrAddr(ptr);
    return Rf_ScalarLogical(p != NULL);
}

/* Write compilation output to a file (obj, dll, or exe). */
SEXP RC_libtcc_output_file(SEXP ext, SEXP filename) {
    TCCState *s = RC_tcc_state(ext);
    const char *fname = Rf_translateCharUTF8(STRING_ELT(filename, 0));
    int rc = tcc_output_file(s, fname);
    return Rf_ScalarInteger(rc);
}

/* Return pointer address as a double (numeric). */
SEXP RC_get_external_ptr_addr(SEXP ext) {
    void *addr = R_ExternalPtrAddr(ext);
    return Rf_ScalarReal((double)(uintptr_t)addr);
}

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

/* Create a NULL external pointer tagged "rtinycc_null". */
SEXP RC_null_pointer() {
    SEXP ptr = R_MakeExternalPtr(NULL, Rf_install("rtinycc_null"), R_NilValue);
    R_RegisterCFinalizerEx(ptr, RC_null_finalizer, FALSE);
    return ptr;
}

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

/* Helper: validate external pointer and compute base + byte_offset. */
static inline unsigned char *ptr_at(SEXP ptr, SEXP offset) {
    if (TYPEOF(ptr) != EXTPTRSXP) Rf_error("Expected external pointer");
    unsigned char *base = (unsigned char *)R_ExternalPtrAddr(ptr);
    if (!base) Rf_error("Pointer is NULL");
    return base + (size_t)Rf_asInteger(offset);
}

/* --- scalar reads -------------------------------------------------------- */

SEXP RC_read_i8(SEXP ptr, SEXP offset) {
    int8_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

SEXP RC_read_u8_typed(SEXP ptr, SEXP offset) {
    uint8_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

SEXP RC_read_i16(SEXP ptr, SEXP offset) {
    int16_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

SEXP RC_read_u16(SEXP ptr, SEXP offset) {
    uint16_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger((int)v);
}

SEXP RC_read_i32_typed(SEXP ptr, SEXP offset) {
    int32_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarInteger(v);
}

SEXP RC_read_u32(SEXP ptr, SEXP offset) {
    uint32_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

SEXP RC_read_i64(SEXP ptr, SEXP offset) {
    int64_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

SEXP RC_read_u64(SEXP ptr, SEXP offset) {
    uint64_t v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

SEXP RC_read_f32(SEXP ptr, SEXP offset) {
    float v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal((double)v);
}

SEXP RC_read_f64_typed(SEXP ptr, SEXP offset) {
    double v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return Rf_ScalarReal(v);
}

/* Read a pointer value at byte offset (dereference void**). */
SEXP RC_read_ptr(SEXP ptr, SEXP offset) {
    void *v;
    memcpy(&v, ptr_at(ptr, offset), sizeof(v));
    return R_MakeExternalPtr(v, Rf_install("rtinycc_borrowed"), R_NilValue);
}

/* --- scalar writes ------------------------------------------------------- */

SEXP RC_write_i8(SEXP ptr, SEXP offset, SEXP value) {
    int8_t v = (int8_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_u8(SEXP ptr, SEXP offset, SEXP value) {
    uint8_t v = (uint8_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_i16(SEXP ptr, SEXP offset, SEXP value) {
    int16_t v = (int16_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_u16(SEXP ptr, SEXP offset, SEXP value) {
    uint16_t v = (uint16_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_i32(SEXP ptr, SEXP offset, SEXP value) {
    int32_t v = (int32_t)Rf_asInteger(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_u32(SEXP ptr, SEXP offset, SEXP value) {
    uint32_t v = (uint32_t)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_i64(SEXP ptr, SEXP offset, SEXP value) {
    int64_t v = (int64_t)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_u64(SEXP ptr, SEXP offset, SEXP value) {
    uint64_t v = (uint64_t)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_f32(SEXP ptr, SEXP offset, SEXP value) {
    float v = (float)Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

SEXP RC_write_f64(SEXP ptr, SEXP offset, SEXP value) {
    double v = Rf_asReal(value);
    memcpy(ptr_at(ptr, offset), &v, sizeof(v));
    return R_NilValue;
}

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

// Find a free callback registry slot (returns -1 if none available)
static int RC_callback_find_free_slot() {
    for (int i = 0; i < MAX_CALLBACKS; i++) {
        if (!callback_registry[i].valid && callback_registry[i].fun == NULL) {
            return i;
        }
    }
    return -1;
}

// Callback token structure - passed back to R as external ptr
typedef struct {
    int id;                // Index into callback_registry (or -1 when closed)
    int refs;              // Reference count for outstanding external pointers
} callback_token_t;

#ifdef _WIN32
SEXP RC_callback_async_init() {
    Rf_error("Async callbacks are not supported on Windows");
    return R_NilValue;
}

SEXP RC_callback_async_schedule(SEXP callback_ext, SEXP args) {
    (void) callback_ext;
    (void) args;
    Rf_error("Async callbacks are not supported on Windows");
    return R_NilValue;
}

SEXP RC_callback_async_drain() {
    Rf_error("Async callbacks are not supported on Windows");
    return R_NilValue;
}

/* Stub: async scheduling is not available on Windows. */
typedef struct { int kind; union { int i; double d; void *p; char *s; } v; } cb_arg_t;
int RC_callback_async_schedule_c(int id, int n_args, const void *args) {
    (void) id; (void) n_args; (void) args;
    return -1;
}
#else
// Async callback dispatch (main-thread queue)
typedef enum {
    CB_ARG_INT,
    CB_ARG_REAL,
    CB_ARG_LOGICAL,
    CB_ARG_PTR,
    CB_ARG_CSTRING
} cb_arg_kind_t;

typedef struct {
    cb_arg_kind_t kind;
    union {
        int i;
        double d;
        void *p;
        char *s;
    } v;
} cb_arg_t;

typedef struct cb_task {
    int id;
    int n_args;
    cb_arg_t *args;
    struct cb_task *next;
} cb_task_t;

static cb_task_t *cbq_head = NULL;
static cb_task_t *cbq_tail = NULL;
static pthread_mutex_t cbq_mutex = PTHREAD_MUTEX_INITIALIZER;
static int cbq_pipe[2] = {-1, -1};
static InputHandler *cbq_ih = NULL;
static int cbq_initialized = 0;
#endif

// Finalizer for callback tokens
static void RC_callback_finalizer(SEXP ext) {
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(ext);
    if (token) {
        if (g_rtinycc_shutting_down) {
            R_ClearExternalPtr(ext);
            return;
        }
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

// Finalizer for callback pointer handles
static void RC_callback_ptr_finalizer(SEXP ext) {
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(ext);
    if (token) {
        if (g_rtinycc_shutting_down) {
            R_ClearExternalPtr(ext);
            return;
        }
        token->refs -= 1;
        if (token->refs <= 0) {
            free(token);
        }
        R_ClearExternalPtr(ext);
    }
}

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

static SEXP RC_invoke_callback_internal(int id, SEXP args) {
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

// Invoke callback from trampoline
// This is called by generated trampolines
SEXP RC_invoke_callback(SEXP callback_id, SEXP args) {
    // callback_id is a character string with the callback identifier
    const char *id_str = Rf_translateCharUTF8(STRING_ELT(callback_id, 0));
    int id = atoi(id_str);
    return RC_invoke_callback_internal(id, args);
}

// Invoke callback by integer id directly (no snprintf needed)
// This avoids CRT stdio dependency in JIT code (snprintf is not a
// direct export from ucrtbase.dll on Windows)
SEXP RC_invoke_callback_id(int id, SEXP args) {
    return RC_invoke_callback_internal(id, args);
}

#ifndef _WIN32
static void cbq_push(cb_task_t *task) {
    pthread_mutex_lock(&cbq_mutex);
    if (cbq_tail) {
        cbq_tail->next = task;
    } else {
        cbq_head = task;
    }
    cbq_tail = task;
    pthread_mutex_unlock(&cbq_mutex);
}

static cb_task_t *cbq_pop_all(void) {
    pthread_mutex_lock(&cbq_mutex);
    cb_task_t *head = cbq_head;
    cbq_head = NULL;
    cbq_tail = NULL;
    pthread_mutex_unlock(&cbq_mutex);
    return head;
}

static SEXP cb_task_to_args(cb_task_t *task) {
    if (task->n_args <= 0) {
        return R_NilValue;
    }
    SEXP args = PROTECT(Rf_allocVector(VECSXP, task->n_args));
    for (int i = 0; i < task->n_args; i++) {
        cb_arg_t *a = &task->args[i];
        switch (a->kind) {
            case CB_ARG_INT:
                SET_VECTOR_ELT(args, i, Rf_ScalarInteger(a->v.i));
                break;
            case CB_ARG_REAL:
                SET_VECTOR_ELT(args, i, Rf_ScalarReal(a->v.d));
                break;
            case CB_ARG_LOGICAL:
                SET_VECTOR_ELT(args, i, Rf_ScalarLogical(a->v.i));
                break;
            case CB_ARG_PTR:
                SET_VECTOR_ELT(args, i, R_MakeExternalPtr(a->v.p, R_NilValue, R_NilValue));
                break;
            case CB_ARG_CSTRING:
                SET_VECTOR_ELT(args, i, Rf_mkString(a->v.s ? a->v.s : ""));
                break;
        }
    }
    UNPROTECT(1);
    return args;
}

static void cbq_free_task(cb_task_t *task) {
    if (!task) return;
    if (task->args) {
        for (int i = 0; i < task->n_args; i++) {
            if (task->args[i].kind == CB_ARG_CSTRING && task->args[i].v.s) {
                free(task->args[i].v.s);
            }
        }
        free(task->args);
    }
    free(task);
}

static void cbq_drain_tasks(void) {
    cb_task_t *task = cbq_pop_all();
    while (task) {
        cb_task_t *next = task->next;
        SEXP args = cb_task_to_args(task);
        int id = task->id;
        // Execute on main thread; errors handled by R_tryEval inside
        RC_invoke_callback_internal(id, args);
        cbq_free_task(task);
        task = next;
    }
}

static void cbq_input_handler(void *data) {
    (void) data;
    // Drain the pipe
    char buf[32];
    while (read(cbq_pipe[0], buf, sizeof(buf)) > 0) {
        // consume
    }
    cbq_drain_tasks();
}

SEXP RC_callback_async_init() {
    if (cbq_initialized) {
        return R_NilValue;
    }
    if (pipe(cbq_pipe) != 0) {
        Rf_error("Failed to create async pipe: %s", strerror(errno));
    }
    int flags = fcntl(cbq_pipe[0], F_GETFL, 0);
    if (flags < 0 || fcntl(cbq_pipe[0], F_SETFL, flags | O_NONBLOCK) < 0) {
        Rf_error("Failed to set async pipe non-blocking: %s", strerror(errno));
    }
    cbq_ih = addInputHandler(R_InputHandlers, cbq_pipe[0], cbq_input_handler, 10);
    if (!cbq_ih) {
        Rf_error("Failed to register input handler");
    }
    cbq_initialized = 1;
    return R_NilValue;
}

SEXP RC_callback_async_schedule(SEXP callback_ext, SEXP args) {
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
    cb_task_t *task = (cb_task_t*) calloc(1, sizeof(cb_task_t));
    if (!task) Rf_error("Out of memory");
    task->id = token->id;
    task->n_args = n_args;
    task->args = NULL;
    task->next = NULL;

    if (n_args > 0) {
        task->args = (cb_arg_t*) calloc((size_t)n_args, sizeof(cb_arg_t));
        if (!task->args) {
            free(task);
            Rf_error("Out of memory");
        }
        for (int i = 0; i < n_args; i++) {
            SEXP val = VECTOR_ELT(args, i);
            if (Rf_isInteger(val) || TYPEOF(val) == INTSXP) {
                task->args[i].kind = CB_ARG_INT;
                task->args[i].v.i = Rf_asInteger(val);
            } else if (Rf_isLogical(val) || TYPEOF(val) == LGLSXP) {
                task->args[i].kind = CB_ARG_LOGICAL;
                task->args[i].v.i = Rf_asLogical(val);
            } else if (Rf_isReal(val) || TYPEOF(val) == REALSXP) {
                task->args[i].kind = CB_ARG_REAL;
                task->args[i].v.d = Rf_asReal(val);
            } else if (TYPEOF(val) == STRSXP) {
                const char *s = Rf_translateCharUTF8(STRING_ELT(val, 0));
                task->args[i].kind = CB_ARG_CSTRING;
                task->args[i].v.s = s ? strdup(s) : NULL;
            } else if (TYPEOF(val) == EXTPTRSXP) {
                task->args[i].kind = CB_ARG_PTR;
                task->args[i].v.p = R_ExternalPtrAddr(val);
            } else {
                cbq_free_task(task);
                Rf_error("Unsupported async callback argument type");
            }
        }
    }

    if (!cbq_initialized) {
        cbq_free_task(task);
        Rf_error("Async callback queue is not initialized. Call tcc_callback_async_enable().");
    }

    cbq_push(task);
    // Wake up event loop
    if (cbq_pipe[1] >= 0) {
        ssize_t wr = write(cbq_pipe[1], "x", 1);
        if (wr < 0) {
            // ignore wakeup failure
        }
    }
    return R_NilValue;
}

// Thread-safe scheduling API for worker threads (no R API usage)
int RC_callback_async_schedule_c(int id, int n_args, const cb_arg_t *args) {
    if (!cbq_initialized) {
        return -1;
    }
    if (id < 0 || id >= MAX_CALLBACKS || !callback_registry[id].valid) {
        return -2;
    }

    cb_task_t *task = (cb_task_t*) calloc(1, sizeof(cb_task_t));
    if (!task) return -3;
    task->id = id;
    task->n_args = n_args;
    task->args = NULL;
    task->next = NULL;

    if (n_args > 0) {
        task->args = (cb_arg_t*) calloc((size_t) n_args, sizeof(cb_arg_t));
        if (!task->args) {
            free(task);
            return -3;
        }
        for (int i = 0; i < n_args; i++) {
            task->args[i] = args[i];
            if (task->args[i].kind == CB_ARG_CSTRING && task->args[i].v.s) {
                task->args[i].v.s = strdup(task->args[i].v.s);
            }
        }
    }

    cbq_push(task);
    if (cbq_pipe[1] >= 0) {
        ssize_t wr = write(cbq_pipe[1], "x", 1);
        if (wr < 0) {
            // ignore wakeup failure
        }
    }
    return 0;
}

SEXP RC_callback_async_drain() {
    cbq_drain_tasks();
    return R_NilValue;
}

static void cbq_shutdown(void) {
    cbq_drain_tasks();
    if (cbq_ih) {
        removeInputHandler(&R_InputHandlers, cbq_ih);
        cbq_ih = NULL;
    }
    if (cbq_pipe[0] >= 0) {
        close(cbq_pipe[0]);
        cbq_pipe[0] = -1;
    }
    if (cbq_pipe[1] >= 0) {
        close(cbq_pipe[1]);
        cbq_pipe[1] = -1;
    }
    cbq_initialized = 0;
}
#endif

// Cleanup all callbacks during package unload
SEXP RC_cleanup_callbacks() {
    // Clean up all valid callbacks in the registry
    for (int i = 0; i < MAX_CALLBACKS; i++) {
        callback_entry_t *entry = &callback_registry[i];
        if (entry->fun != NULL) {
            R_ReleaseObject(entry->fun);
            entry->fun = NULL;
        }
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
        entry->valid = 0;
        entry->n_args = 0;
        entry->threadsafe = 0;
        entry->trampoline = NULL;
    }
#ifndef _WIN32
    cbq_shutdown();
#endif
    return R_NilValue;
}


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