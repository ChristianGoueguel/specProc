// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// computeGroupedMeans
Rcpp::DataFrame computeGroupedMeans(Rcpp::CharacterVector groups, Rcpp::NumericMatrix values);
RcppExport SEXP _specProc_computeGroupedMeans(SEXP groupsSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type groups(groupsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type values(valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(computeGroupedMeans(groups, values));
    return rcpp_result_gen;
END_RCPP
}
// computeMeans
double computeMeans(Rcpp::NumericMatrix values);
RcppExport SEXP _specProc_computeMeans(SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type values(valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(computeMeans(values));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_specProc_computeGroupedMeans", (DL_FUNC) &_specProc_computeGroupedMeans, 2},
    {"_specProc_computeMeans", (DL_FUNC) &_specProc_computeMeans, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_specProc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
