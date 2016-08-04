// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// refine_ordinate
arma::vec refine_ordinate(double x, arma::vec xi, arma::vec theta, unsigned int order);
RcppExport SEXP cpr_refine_ordinate(SEXP xSEXP, SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    __result = Rcpp::wrap(refine_ordinate(x, xi, theta, order));
    return __result;
END_RCPP
}
// coarsen_ordinate
arma::vec coarsen_ordinate(double x, arma::vec xi, arma::vec theta, unsigned int order);
RcppExport SEXP cpr_coarsen_ordinate(SEXP xSEXP, SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    __result = Rcpp::wrap(coarsen_ordinate(x, xi, theta, order));
    return __result;
END_RCPP
}
// hat_ordinate
arma::vec hat_ordinate(double x, arma::vec xi, arma::vec theta, unsigned int order);
RcppExport SEXP cpr_hat_ordinate(SEXP xSEXP, SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    __result = Rcpp::wrap(hat_ordinate(x, xi, theta, order));
    return __result;
END_RCPP
}
// insertion_matrix
arma::mat insertion_matrix(double x, arma::vec xi, unsigned int order);
RcppExport SEXP cpr_insertion_matrix(SEXP xSEXP, SEXP xiSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    __result = Rcpp::wrap(insertion_matrix(x, xi, order));
    return __result;
END_RCPP
}
// weigh_iknots
Rcpp::NumericVector weigh_iknots(arma::vec xi, arma::mat theta, unsigned int order, unsigned int p);
RcppExport SEXP cpr_weigh_iknots(SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type p(pSEXP);
    __result = Rcpp::wrap(weigh_iknots(xi, theta, order, p));
    return __result;
END_RCPP
}
// greville_sites
Rcpp::NumericVector greville_sites(arma::vec xi, unsigned int order);
RcppExport SEXP cpr_greville_sites(SEXP xiSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    __result = Rcpp::wrap(greville_sites(xi, order));
    return __result;
END_RCPP
}
// bspline__impl
Rcpp::NumericVector bspline__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots);
RcppExport SEXP cpr_bspline__impl(SEXP xSEXP, SEXP jSEXP, SEXP orderSEXP, SEXP knotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type knots(knotsSEXP);
    __result = Rcpp::wrap(bspline__impl(x, j, order, knots));
    return __result;
END_RCPP
}
// bsplineD1__impl
Rcpp::NumericVector bsplineD1__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots);
RcppExport SEXP cpr_bsplineD1__impl(SEXP xSEXP, SEXP jSEXP, SEXP orderSEXP, SEXP knotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type knots(knotsSEXP);
    __result = Rcpp::wrap(bsplineD1__impl(x, j, order, knots));
    return __result;
END_RCPP
}
// bsplineD2__impl
Rcpp::NumericVector bsplineD2__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots);
RcppExport SEXP cpr_bsplineD2__impl(SEXP xSEXP, SEXP jSEXP, SEXP orderSEXP, SEXP knotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type knots(knotsSEXP);
    __result = Rcpp::wrap(bsplineD2__impl(x, j, order, knots));
    return __result;
END_RCPP
}
// bbasis__impl
Rcpp::NumericMatrix bbasis__impl(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int k);
RcppExport SEXP cpr_bbasis__impl(SEXP xSEXP, SEXP iknotsSEXP, SEXP bknotsSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type iknots(iknotsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bknots(bknotsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    __result = Rcpp::wrap(bbasis__impl(x, iknots, bknots, k));
    return __result;
END_RCPP
}
// diag_only
Rcpp::NumericVector diag_only(arma::mat& A, arma::mat& B);
RcppExport SEXP cpr_diag_only(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type B(BSEXP);
    __result = Rcpp::wrap(diag_only(A, B));
    return __result;
END_RCPP
}
// tp__impl
arma::mat tp__impl(arma::mat A, arma::mat B);
RcppExport SEXP cpr_tp__impl(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type B(BSEXP);
    __result = Rcpp::wrap(tp__impl(A, B));
    return __result;
END_RCPP
}
