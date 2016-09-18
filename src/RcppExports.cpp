// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// refine_ordinate
arma::vec refine_ordinate(double x, const arma::vec& xi, const arma::vec& theta, unsigned int order);
RcppExport SEXP cpr_refine_ordinate(SEXP xSEXP, SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(refine_ordinate(x, xi, theta, order));
    return rcpp_result_gen;
END_RCPP
}
// coarsen_ordinate
arma::vec coarsen_ordinate(double x, const arma::vec& xi, const arma::vec& theta, unsigned int order);
RcppExport SEXP cpr_coarsen_ordinate(SEXP xSEXP, SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(coarsen_ordinate(x, xi, theta, order));
    return rcpp_result_gen;
END_RCPP
}
// hat_ordinate
arma::vec hat_ordinate(double x, const arma::vec& xi, const arma::vec& theta, unsigned int order);
RcppExport SEXP cpr_hat_ordinate(SEXP xSEXP, SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(hat_ordinate(x, xi, theta, order));
    return rcpp_result_gen;
END_RCPP
}
// insertion_matrix
arma::mat insertion_matrix(double x, const arma::vec& xi, unsigned int order);
RcppExport SEXP cpr_insertion_matrix(SEXP xSEXP, SEXP xiSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(insertion_matrix(x, xi, order));
    return rcpp_result_gen;
END_RCPP
}
// weigh_iknots
Rcpp::NumericVector weigh_iknots(const arma::vec& xi, const arma::mat& theta, unsigned int order, unsigned int p);
RcppExport SEXP cpr_weigh_iknots(SEXP xiSEXP, SEXP thetaSEXP, SEXP orderSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(weigh_iknots(xi, theta, order, p));
    return rcpp_result_gen;
END_RCPP
}
// bbasis__impl
Rcpp::NumericMatrix bbasis__impl(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order);
RcppExport SEXP cpr_bbasis__impl(SEXP xSEXP, SEXP iknotsSEXP, SEXP bknotsSEXP, SEXP orderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type iknots(iknotsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type bknots(bknotsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    rcpp_result_gen = Rcpp::wrap(bbasis__impl(x, iknots, bknots, order));
    return rcpp_result_gen;
END_RCPP
}
// bsplineD1__impl
Rcpp::NumericVector bsplineD1__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots);
RcppExport SEXP cpr_bsplineD1__impl(SEXP xSEXP, SEXP jSEXP, SEXP orderSEXP, SEXP knotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type knots(knotsSEXP);
    rcpp_result_gen = Rcpp::wrap(bsplineD1__impl(x, j, order, knots));
    return rcpp_result_gen;
END_RCPP
}
// bsplineD2__impl
Rcpp::NumericVector bsplineD2__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots);
RcppExport SEXP cpr_bsplineD2__impl(SEXP xSEXP, SEXP jSEXP, SEXP orderSEXP, SEXP knotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type knots(knotsSEXP);
    rcpp_result_gen = Rcpp::wrap(bsplineD2__impl(x, j, order, knots));
    return rcpp_result_gen;
END_RCPP
}
// diag_only
Rcpp::NumericVector diag_only(const arma::mat& A, const arma::mat& B);
RcppExport SEXP cpr_diag_only(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(diag_only(A, B));
    return rcpp_result_gen;
END_RCPP
}
// tp__impl
arma::mat tp__impl(const arma::mat& A, const arma::mat& B);
RcppExport SEXP cpr_tp__impl(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(tp__impl(A, B));
    return rcpp_result_gen;
END_RCPP
}
