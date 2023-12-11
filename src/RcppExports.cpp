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
// coarsen_theta
arma::vec coarsen_theta(unsigned int j, const arma::vec& xi, unsigned int k, const arma::vec& theta);
RcppExport SEXP _cpr_coarsen_theta(SEXP jSEXP, SEXP xiSEXP, SEXP kSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(coarsen_theta(j, xi, k, theta));
    return rcpp_result_gen;
END_RCPP
}
// hat_theta
Rcpp::List hat_theta(unsigned int j, const arma::vec& xi, unsigned int k, const arma::vec& theta, bool calculate_F, const arma::mat& Sigma);
RcppExport SEXP _cpr_hat_theta(SEXP jSEXP, SEXP xiSEXP, SEXP kSEXP, SEXP thetaSEXP, SEXP calculate_FSEXP, SEXP SigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type j(jSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type k(kSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< bool >::type calculate_F(calculate_FSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Sigma(SigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(hat_theta(j, xi, k, theta, calculate_F, Sigma));
    return rcpp_result_gen;
END_RCPP
}
// matrix_rank
double matrix_rank(Rcpp::NumericMatrix x);
RcppExport SEXP _cpr_matrix_rank(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_rank(x));
    return rcpp_result_gen;
END_RCPP
}
// tensor_product
arma::mat tensor_product(const arma::mat& A, const arma::mat& B);
RcppExport SEXP _cpr_tensor_product(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(tensor_product(A, B));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cpr_cpp_bsplines", (DL_FUNC) &_cpr_cpp_bsplines, 4},
    {"_cpr_cpp_bsplinesD1", (DL_FUNC) &_cpr_cpp_bsplinesD1, 4},
    {"_cpr_cpp_bsplinesD2", (DL_FUNC) &_cpr_cpp_bsplinesD2, 4},
    {"_cpr_W", (DL_FUNC) &_cpr_W, 3},
    {"_cpr_refine_theta", (DL_FUNC) &_cpr_refine_theta, 4},
    {"_cpr_coarsen_theta", (DL_FUNC) &_cpr_coarsen_theta, 4},
    {"_cpr_hat_theta", (DL_FUNC) &_cpr_hat_theta, 6},
    {"_cpr_matrix_rank", (DL_FUNC) &_cpr_matrix_rank, 1},
    {"_cpr_tensor_product", (DL_FUNC) &_cpr_tensor_product, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_cpr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
