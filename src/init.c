#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>

SEXP RC_libtcc_state_new(SEXP lib_path, SEXP include_path, SEXP output_type);
SEXP RC_libtcc_add_file(SEXP ext, SEXP path);
SEXP RC_libtcc_compile_string(SEXP ext, SEXP code);
SEXP RC_libtcc_add_symbol(SEXP ext, SEXP name, SEXP addr);
SEXP RC_libtcc_relocate(SEXP ext);
SEXP RC_libtcc_call_symbol(SEXP ext, SEXP name);

// .Call entries
static const R_CallMethodDef CallEntries[] = {
    {"RC_libtcc_state_new",   (DL_FUNC) &RC_libtcc_state_new,   3},
    {"RC_libtcc_add_file",    (DL_FUNC) &RC_libtcc_add_file,    2},
    {"RC_libtcc_compile_string", (DL_FUNC) &RC_libtcc_compile_string, 2},
    {"RC_libtcc_add_symbol",  (DL_FUNC) &RC_libtcc_add_symbol,  3},
    {"RC_libtcc_relocate",    (DL_FUNC) &RC_libtcc_relocate,    1},
    {"RC_libtcc_call_symbol", (DL_FUNC) &RC_libtcc_call_symbol, 2},
    {NULL, NULL, 0}
};

// Initialization function
void R_init_Rtinycc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
