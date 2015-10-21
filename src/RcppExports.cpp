// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// generate_bsplines
Rcpp::List generate_bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int k);
RcppExport SEXP cpr_generate_bsplines(SEXP xSEXP, SEXP iknotsSEXP, SEXP bknotsSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type iknots(iknotsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bknots(bknotsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    __result = Rcpp::wrap(generate_bsplines(x, iknots, bknots, k));
    return __result;
END_RCPP
}
