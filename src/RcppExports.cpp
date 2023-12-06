// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_bsplines
Rcpp::NumericMatrix cpp_bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order);
RcppExport SEXP _cpr_cpp_bsplines(SEXP xSEXP, SEXP iknotsSEXP, SEXP bknotsSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type iknots(iknotsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bknots(bknotsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_bsplines(x, iknots, bknots, order));
    return rcpp_result_gen;
END_RCPP
}
// cpp_bsplinesD1
Rcpp::NumericMatrix cpp_bsplinesD1(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order);
RcppExport SEXP _cpr_cpp_bsplinesD1(SEXP xSEXP, SEXP iknotsSEXP, SEXP bknotsSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type iknots(iknotsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bknots(bknotsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_bsplinesD1(x, iknots, bknots, order));
    return rcpp_result_gen;
END_RCPP
}
// cpp_bsplinesD2
Rcpp::NumericMatrix cpp_bsplinesD2(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order);
RcppExport SEXP _cpr_cpp_bsplinesD2(SEXP xSEXP, SEXP iknotsSEXP, SEXP bknotsSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type iknots(iknotsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bknots(bknotsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_bsplinesD2(x, iknots, bknots, order));
    return rcpp_result_gen;
END_RCPP
}
// W
arma::mat W(double xi_prime, const arma::vec& xi, unsigned int k);
RcppExport SEXP _cpr_W(SEXP xi_primeSEXP, SEXP xiSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type xi_prime(xi_primeSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(W(xi_prime, xi, k));
    return rcpp_result_gen;
END_RCPP
}
// refine_theta
arma::vec refine_theta(double xi_prime, const arma::vec& xi, unsigned int k, const arma::vec& theta);
RcppExport SEXP _cpr_refine_theta(SEXP xi_primeSEXP, SEXP xiSEXP, SEXP kSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type xi_prime(xi_primeSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(refine_theta(xi_prime, xi, k, theta));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cpr_cpp_bsplines", (DL_FUNC) &_cpr_cpp_bsplines, 4},
    {"_cpr_cpp_bsplinesD1", (DL_FUNC) &_cpr_cpp_bsplinesD1, 4},
    {"_cpr_cpp_bsplinesD2", (DL_FUNC) &_cpr_cpp_bsplinesD2, 4},
    {"_cpr_W", (DL_FUNC) &_cpr_W, 3},
    {"_cpr_refine_theta", (DL_FUNC) &_cpr_refine_theta, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_cpr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
