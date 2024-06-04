// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// computeGroupedMeans
NumericMatrix computeGroupedMeans(NumericMatrix data, IntegerVector group);
RcppExport SEXP _specProc_computeGroupedMeans(SEXP dataSEXP, SEXP groupSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type group(groupSEXP);
    rcpp_result_gen = Rcpp::wrap(computeGroupedMeans(data, group));
    return rcpp_result_gen;
END_RCPP
}
// computeMeans
NumericMatrix computeMeans(NumericMatrix data);
RcppExport SEXP _specProc_computeMeans(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(computeMeans(data));
    return rcpp_result_gen;
END_RCPP
}
// epo_cpp
Rcpp::List epo_cpp(Rcpp::NumericMatrix X, int ncomp);
RcppExport SEXP _specProc_epo_cpp(SEXP XSEXP, SEXP ncompSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type ncomp(ncompSEXP);
    rcpp_result_gen = Rcpp::wrap(epo_cpp(X, ncomp));
    return rcpp_result_gen;
END_RCPP
}
// glsw_cpp
Rcpp::NumericMatrix glsw_cpp(Rcpp::NumericMatrix X_diff, double alpha);
RcppExport SEXP _specProc_glsw_cpp(SEXP X_diffSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type X_diff(X_diffSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(glsw_cpp(X_diff, alpha));
    return rcpp_result_gen;
END_RCPP
}
// yGradientglswCpp
Eigen::MatrixXd yGradientglswCpp(const Eigen::MatrixXd& X_diff, const Eigen::VectorXd& w_i, double alpha);
RcppExport SEXP _specProc_yGradientglswCpp(SEXP X_diffSEXP, SEXP w_iSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type X_diff(X_diffSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type w_i(w_iSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(yGradientglswCpp(X_diff, w_i, alpha));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_specProc_computeGroupedMeans", (DL_FUNC) &_specProc_computeGroupedMeans, 2},
    {"_specProc_computeMeans", (DL_FUNC) &_specProc_computeMeans, 1},
    {"_specProc_epo_cpp", (DL_FUNC) &_specProc_epo_cpp, 2},
    {"_specProc_glsw_cpp", (DL_FUNC) &_specProc_glsw_cpp, 2},
    {"_specProc_yGradientglswCpp", (DL_FUNC) &_specProc_yGradientglswCpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_specProc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
