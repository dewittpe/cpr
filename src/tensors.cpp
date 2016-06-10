// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::mat tp__impl(arma::mat A, arma::mat B) { 
  arma::mat oneA(1, B.n_cols, arma::fill::ones);
  arma::mat oneB(1, A.n_cols, arma::fill::ones);

  //return(arma::kron(oneA, A));

  return( arma::kron(oneA, A) % arma::kron(B, oneB) );
}

