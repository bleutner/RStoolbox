#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP RStoolbox_classQA(SEXP, SEXP);
extern SEXP RStoolbox_entropyCpp(SEXP);
extern SEXP RStoolbox_gainOffsetRescale(SEXP, SEXP, SEXP, SEXP);
extern SEXP RStoolbox_normImageCpp(SEXP, SEXP, SEXP);
extern SEXP RStoolbox_predictMlcCpp(SEXP, SEXP, SEXP);
extern SEXP RStoolbox_predKmeansCpp(SEXP, SEXP);
extern SEXP RStoolbox_pwSimilarityCpp(SEXP, SEXP, SEXP);
extern SEXP RStoolbox_rescaleImageCpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP RStoolbox_specSimC(SEXP, SEXP);
extern SEXP RStoolbox_spectralIndicesCpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"RStoolbox_classQA",            (DL_FUNC) &RStoolbox_classQA,             2},
    {"RStoolbox_entropyCpp",         (DL_FUNC) &RStoolbox_entropyCpp,          1},
    {"RStoolbox_gainOffsetRescale",  (DL_FUNC) &RStoolbox_gainOffsetRescale,   4},
    {"RStoolbox_normImageCpp",       (DL_FUNC) &RStoolbox_normImageCpp,        3},
    {"RStoolbox_predictMlcCpp",      (DL_FUNC) &RStoolbox_predictMlcCpp,       3},
    {"RStoolbox_predKmeansCpp",      (DL_FUNC) &RStoolbox_predKmeansCpp,       2},
    {"RStoolbox_pwSimilarityCpp",    (DL_FUNC) &RStoolbox_pwSimilarityCpp,     3},
    {"RStoolbox_rescaleImageCpp",    (DL_FUNC) &RStoolbox_rescaleImageCpp,     4},
    {"RStoolbox_specSimC",           (DL_FUNC) &RStoolbox_specSimC,            2},
    {"RStoolbox_spectralIndicesCpp", (DL_FUNC) &RStoolbox_spectralIndicesCpp, 20},
    {NULL, NULL, 0}
};

void R_init_RStoolbox(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
