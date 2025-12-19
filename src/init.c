#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>


// .Call entries
static const R_CallMethodDef CallEntries[] = {
   // {"RC_tinycc_version", (DL_FUNC) &RC_tinycc_version, 0},
    {NULL, NULL, 0}
};

// Initialization function
void R_init_Rtinycc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}