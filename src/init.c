#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _RStoolbox_classQA(SEXP, SEXP);
extern SEXP _RStoolbox_entropyCpp(SEXP);
extern SEXP _RStoolbox_gainOffsetRescale(SEXP, SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_nnls_solver(SEXP, SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_normImageCpp(SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_oneHotCpp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_predictMlcCpp(SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_predKmeansCpp(SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_pwSimilarityCpp(SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_rescaleImageCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _RStoolbox_specSimC(SEXP, SEXP);
extern SEXP _RStoolbox_spectralIndicesCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_RStoolbox_classQA",            (DL_FUNC) &_RStoolbox_classQA,             2},
    {"_RStoolbox_entropyCpp",         (DL_FUNC) &_RStoolbox_entropyCpp,          1},
    {"_RStoolbox_gainOffsetRescale",  (DL_FUNC) &_RStoolbox_gainOffsetRescale,   4},
    {"_RStoolbox_nnls_solver",        (DL_FUNC) &_RStoolbox_nnls_solver,         4},
    {"_RStoolbox_normImageCpp",       (DL_FUNC) &_RStoolbox_normImageCpp,        3},
    {"_RStoolbox_oneHotCpp",          (DL_FUNC) &_RStoolbox_oneHotCpp,           5},
    {"_RStoolbox_predictMlcCpp",      (DL_FUNC) &_RStoolbox_predictMlcCpp,       3},
    {"_RStoolbox_predKmeansCpp",      (DL_FUNC) &_RStoolbox_predKmeansCpp,       3},
    {"_RStoolbox_pwSimilarityCpp",    (DL_FUNC) &_RStoolbox_pwSimilarityCpp,     3},
    {"_RStoolbox_rescaleImageCpp",    (DL_FUNC) &_RStoolbox_rescaleImageCpp,     4},
    {"_RStoolbox_specSimC",           (DL_FUNC) &_RStoolbox_specSimC,            2},
    {"_RStoolbox_spectralIndicesCpp", (DL_FUNC) &_RStoolbox_spectralIndicesCpp, 23},
    {NULL, NULL, 0}
};

void R_init_RStoolbox(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}