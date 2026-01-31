#include "libtcc.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <stdint.h>
#include <inttypes.h>

static void RC_tcc_finalizer(SEXP ext) {
    void *ptr = R_ExternalPtrAddr(ext);
    if (ptr) {
        TCCState *s = (TCCState*)ptr;
        tcc_delete(s);
        R_ClearExternalPtr(ext);
    }
}

static void RC_null_finalizer(SEXP ext) {
    // NULL pointer doesn't need cleanup
    R_ClearExternalPtr(ext);
}

static void RC_free_finalizer(SEXP ext) {
    void *ptr = R_ExternalPtrAddr(ext);
    if (ptr) {
        free(ptr);
        R_ClearExternalPtr(ext);
    }
}

static void RC_cstring_finalizer(SEXP ext) {
    char **ptr = (char**)R_ExternalPtrAddr(ext);
    if (ptr) {
        if (*ptr) {
            free(*ptr);
        }
        free(ptr);
        R_ClearExternalPtr(ext);
    }
}

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

SEXP RC_libtcc_state_new(SEXP lib_path, SEXP include_path, SEXP output_type) {
    TCCState *s = tcc_new();
    if (!s) {
        Rf_error("tcc_new failed");
    }

    /* Route libtcc diagnostics through R */
    // tcc_set_error_func(s, NULL, (void (*)(void *, const char *)) REprintf);

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
    R_RegisterCFinalizerEx(ext, RC_tcc_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}

SEXP RC_libtcc_add_file(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *fname = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_file(s, fname);
    return Rf_ScalarInteger(rc);
}

SEXP RC_libtcc_add_include_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_include_path(s, p);
    return Rf_ScalarInteger(rc);
}

SEXP RC_libtcc_add_sysinclude_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_sysinclude_path(s, p);
    return Rf_ScalarInteger(rc);
}

SEXP RC_libtcc_add_library_path(SEXP ext, SEXP path) {
    TCCState *s = RC_tcc_state(ext);
    const char *p = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int rc = tcc_add_library_path(s, p);
    return Rf_ScalarInteger(rc);
}

SEXP RC_libtcc_add_library(SEXP ext, SEXP library) {
    TCCState *s = RC_tcc_state(ext);
    const char *lib = Rf_translateCharUTF8(STRING_ELT(library, 0));
    int rc = tcc_add_library(s, lib);
    return Rf_ScalarInteger(rc);
}



SEXP RC_libtcc_compile_string(SEXP ext, SEXP code) {
    TCCState *s = RC_tcc_state(ext);
     const char *src = Rf_translateCharUTF8(STRING_ELT(code, 0));
    int rc = tcc_compile_string(s, src);
    return Rf_ScalarInteger(rc);
}

SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr) {
    TCCState *s = RC_tcc_state(ext);
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    void *ptr = R_ExternalPtrAddr(addr);
    int rc = tcc_add_symbol(s, sym, ptr);
    return Rf_ScalarInteger(rc);
}

SEXP RC_libtcc_relocate(SEXP ext) {
    TCCState *s = RC_tcc_state(ext);
    int rc = tcc_relocate(s);
    return Rf_ScalarInteger(rc);
}

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
    SEXP ptr = PROTECT(R_MakeExternalPtrFn((DL_FUNC)fn, native_symbol_tag, R_NilValue));
    Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("tcc_symbol"));
    UNPROTECT(1);
    return ptr;
}

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

SEXP RC_libtcc_ptr_valid(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("expected an external pointer");
    }
    void *p = R_ExternalPtrAddr(ptr);
    return Rf_ScalarLogical(p != NULL);
}

SEXP RC_libtcc_output_file(SEXP ext, SEXP filename) {
    TCCState *s = RC_tcc_state(ext);
    const char *fname = Rf_translateCharUTF8(STRING_ELT(filename, 0));
    int rc = tcc_output_file(s, fname);
    return Rf_ScalarInteger(rc);
}

// Expose external pointer address to R
SEXP RC_get_external_ptr_addr(SEXP ext) {
    void *addr = R_ExternalPtrAddr(ext);
    return Rf_ScalarReal((double)(uintptr_t)addr);
}

// Return pointer address formatted as hex string using uintptr_t width
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

// Pointer utility functions
SEXP RC_null_pointer() {
    SEXP ptr = R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(ptr, RC_null_finalizer, TRUE);
    return ptr;
}

SEXP RC_malloc(SEXP size) {
    int sz = Rf_asInteger(size);
    if (sz <= 0) {
        Rf_error("Size must be positive");
    }
    
    void *data = malloc(sz);
    if (!data) {
        Rf_error("Memory allocation failed");
    }
    
    SEXP ptr = R_MakeExternalPtr(data, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(ptr, RC_free_finalizer, TRUE);
    return ptr;
}

SEXP RC_free(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    
    void *data = R_ExternalPtrAddr(ptr);
    if (data) {
        free(data);
        R_ClearExternalPtr(ptr);
    }
    
    return R_NilValue;
}

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
    
    SEXP ptr = R_MakeExternalPtr(data, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(ptr, RC_free_finalizer, TRUE);
    return ptr;
}

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