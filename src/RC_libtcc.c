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
static int next_callback_id = 0;

// Callback token structure - passed back to R as external ptr
typedef struct {
    int id;                // Index into callback_registry
} callback_token_t;

// Finalizer for callback tokens
static void RC_callback_finalizer(SEXP ext) {
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(ext);
    if (token) {
        // Release the preserved R function
        if (token->id >= 0 && token->id < MAX_CALLBACKS) {
            callback_entry_t *entry = &callback_registry[token->id];
            if (entry->valid) {
                R_ReleaseObject(entry->fun);
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
        free(token);
        R_ClearExternalPtr(ext);
    }
}

// Register a callback
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
    if (next_callback_id >= MAX_CALLBACKS) {
        Rf_error("Callback registry full (max %d)", MAX_CALLBACKS);
    }
    
    int id = next_callback_id++;
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
    
    SEXP ext = PROTECT(R_MakeExternalPtr(token, R_NilValue, R_NilValue));
    Rf_setAttrib(ext, R_ClassSymbol, Rf_mkString("tcc_callback"));
    R_RegisterCFinalizerEx(ext, RC_callback_finalizer, TRUE);
    UNPROTECT(1);
    
    return ext;
}

// Unregister a callback
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

// Get the pointer address of a callback token
SEXP RC_get_callback_ptr(SEXP callback_ext) {
    if (!Rf_inherits(callback_ext, "tcc_callback")) {
        Rf_error("Expected a 'tcc_callback' external pointer");
    }
    
    callback_token_t *token = (callback_token_t*)R_ExternalPtrAddr(callback_ext);
    if (!token) {
        Rf_error("Callback is closed");
    }
    
    // Return the token address as external pointer (this is what trampolines use)
    SEXP ptr = PROTECT(R_MakeExternalPtr(token, R_NilValue, R_NilValue));
    Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("tcc_callback_ptr"));
    UNPROTECT(1);
    return ptr;
}

// Check if callback is valid
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
SEXP RC_invoke_callback(SEXP callback_id, SEXP args) {
    // callback_id is a character string with the callback identifier
    const char *id_str = Rf_translateCharUTF8(STRING_ELT(callback_id, 0));
    int id = atoi(id_str);
    
    if (id < 0 || id >= MAX_CALLBACKS || !callback_registry[id].valid) {
        Rf_error("Invalid or expired callback: %s", id_str);
    }
    
    callback_entry_t *entry = &callback_registry[id];
    
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
    SEXP result = PROTECT(Rf_eval(call, R_GlobalEnv));
    
    // Convert result based on expected return type
    SEXP converted = R_NilValue;
    if (strcmp(entry->return_type, "void") == 0) {
        converted = R_NilValue;
    } else if (strcmp(entry->return_type, "int") == 0 || 
               strcmp(entry->return_type, "i32") == 0 ||
               strcmp(entry->return_type, "int32_t") == 0) {
        converted = Rf_ScalarInteger(Rf_asInteger(result));
    } else if (strcmp(entry->return_type, "double") == 0 ||
               strcmp(entry->return_type, "f64") == 0 ||
               strcmp(entry->return_type, "float") == 0) {
        converted = Rf_ScalarReal(Rf_asReal(result));
    } else if (strcmp(entry->return_type, "bool") == 0 ||
               strcmp(entry->return_type, "logical") == 0) {
        converted = Rf_ScalarLogical(Rf_asLogical(result));
    } else if (strcmp(entry->return_type, "string") == 0 ||
               strcmp(entry->return_type, "cstring") == 0 ||
               strcmp(entry->return_type, "char*") == 0) {
        if (Rf_isString(result)) {
            converted = result;
        } else {
            converted = Rf_asChar(result);
        }
    } else {
        // Default: return as-is
        converted = result;
    }
    
    UNPROTECT(2);  // call and result
    return converted;
}

// Cleanup all callbacks during package unload
SEXP RC_cleanup_callbacks() {
    // Clean up all valid callbacks in the registry
    for (int i = 0; i < next_callback_id; i++) {
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
    next_callback_id = 0;
    return R_NilValue;
}

// Dummy function to suppress R CMD check warnings
SEXP RC_dummy() {
    return R_NilValue;
}