// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

arma::mat tensor_product(const arma::mat& A, const arma::mat& B) {
  arma::mat oneA(1, B.n_cols, arma::fill::ones);
  arma::mat oneB(1, A.n_cols, arma::fill::ones);

  return( arma::kron(oneA, A) % arma::kron(B, oneB) );
}

