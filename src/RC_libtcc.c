#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <stdint.h>
#include "libtcc.h"

static void RC_tcc_finalizer(SEXP ext) {
    TCCState *s = (TCCState *) R_ExternalPtrAddr(ext);
    if (s) {
        tcc_delete(s);
        R_ClearExternalPtr(ext);
    }
}

static TCCState *RC_tcc_state(SEXP ext) {
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
#ifdef TCC_RELOCATE_AUTO
    int rc = tcc_relocate(s, TCC_RELOCATE_AUTO);
#else
    int rc = tcc_relocate(s);
#endif
    return Rf_ScalarInteger(rc);
}

SEXP RC_libtcc_call_symbol(SEXP ext, SEXP name) {
    TCCState *s = RC_tcc_state(ext);
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    void *fn = tcc_get_symbol(s, sym);
    if (!fn) {
        Rf_error("symbol '%s' not found", sym);
    }
    int (*callable)(void) = (int (*)(void)) fn;
    return Rf_ScalarInteger(callable());
}

SEXP RC_libtcc_get_symbol(SEXP ext, SEXP name) {
    TCCState *s = RC_tcc_state(ext);
    const char *sym = Rf_translateCharUTF8(STRING_ELT(name, 0));
    void *fn = tcc_get_symbol(s, sym);
    if (!fn) {
        Rf_error("symbol '%s' not found", sym);
    }
    SEXP ptr = PROTECT(R_MakeExternalPtr(fn, R_NilValue, R_NilValue));
    Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("tcc_symbol"));
    UNPROTECT(1);
    return ptr;
}
