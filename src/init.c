#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>

SEXP RC_libtcc_state_new(SEXP lib_path, SEXP include_path, SEXP output_type);
SEXP RC_libtcc_add_file(SEXP ext, SEXP path);
SEXP RC_libtcc_add_include_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_sysinclude_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library_path(SEXP ext, SEXP path);
SEXP RC_libtcc_add_library(SEXP ext, SEXP library);
SEXP RC_libtcc_compile_string(SEXP ext, SEXP code);
SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr);
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

// .Call entries
static const R_CallMethodDef CallEntries[] = {
    {"RC_libtcc_state_new",   (DL_FUNC) &RC_libtcc_state_new,   3},
    {"RC_libtcc_add_file",    (DL_FUNC) &RC_libtcc_add_file,    2},
    {"RC_libtcc_add_include_path",    (DL_FUNC) &RC_libtcc_add_include_path,    2},
    {"RC_libtcc_add_sysinclude_path", (DL_FUNC) &RC_libtcc_add_sysinclude_path, 2},
    {"RC_libtcc_add_library_path",   (DL_FUNC) &RC_libtcc_add_library_path,   2},
    {"RC_libtcc_add_library",        (DL_FUNC) &RC_libtcc_add_library,        2},
    {"RC_libtcc_compile_string", (DL_FUNC) &RC_libtcc_compile_string, 2},
    {"RC_libtcc_add_symbol",  (DL_FUNC) &RC_libtcc_add_symbol,  3},
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
    {NULL, NULL, 0}
};

// Initialization function
void R_init_Rtinycc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
