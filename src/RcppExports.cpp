// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// W
arma::mat W(double x, arma::vec xi, unsigned int k);
RcppExport SEXP cpr_W(SEXP xSEXP, SEXP xiSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    __result = Rcpp::wrap(W(x, xi, k));
    return __result;
END_RCPP
}
// W_hat
arma::mat W_hat(double x, arma::vec xi, unsigned int k);
RcppExport SEXP cpr_W_hat(SEXP xSEXP, SEXP xiSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    __result = Rcpp::wrap(W_hat(x, xi, k));
    return __result;
END_RCPP
}
// iknot_weights
arma::vec iknot_weights(arma::vec xi, arma::vec theta, unsigned int k, unsigned int p);
RcppExport SEXP cpr_iknot_weights(SEXP xiSEXP, SEXP thetaSEXP, SEXP kSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type p(pSEXP);
    __result = Rcpp::wrap(iknot_weights(xi, theta, k, p));
    return __result;
END_RCPP
}
// greville_sites
arma::vec greville_sites(arma::vec xi, unsigned int k);
RcppExport SEXP cpr_greville_sites(SEXP xiSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    __result = Rcpp::wrap(greville_sites(xi, k));
    return __result;
END_RCPP
}
// bsplines_impl
Rcpp::List bsplines_impl(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order);
RcppExport SEXP cpr_bsplines_impl(SEXP xSEXP, SEXP iknotsSEXP, SEXP bknotsSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type iknots(iknotsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bknots(bknotsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    __result = Rcpp::wrap(bsplines_impl(x, iknots, bknots, order));
    return __result;
END_RCPP
}
