// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::NumericVector diag_only(const arma::mat& A, const arma::mat& B) {

  if (A.n_rows != B.n_cols) { 
    throw std::invalid_argument("Expecting A.n_rows == B.n_cols");
  }

  if (A.n_cols != B.n_rows) { 
    throw std::invalid_argument("Expecting A.n_cols == B.n_rows");
  }

  arma::vec rtn(A.n_rows);
  rtn.fill(-1);

  for(unsigned int i=0; i < A.n_rows;  ++i) {
    rtn(i) = arma::as_scalar(A.row(i) * B.col(i));
  }
  return Rcpp::NumericVector(rtn.begin(), rtn.end());
}
