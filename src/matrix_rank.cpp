// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

//' Rank of a Matrix
//'
//' Determine the rank (number of linearly independent columns) of a matrix.
//'
//' Implimentation via the Armadillo C++ linear algrebra library.  The function
//' returns the rank of the matix \code{x}.  The computation is based on the
//' singular value decomposition of the matrix; a std::runtime_error excetion
//' will be thrown if the decomposition fails.  Any singular values less than
//' the tolerance are treated as zeros.  The tolerance is max(m, n) * max_sv *
//' datum::eps, where m is the number of rows of x, n is the number of columns
//' of x, max_sv is the maximal singular value of x, and datum::eps is the
//' difference between 1 and the least value greater than 1 that is
//' representable.
//'
//' @author Peter DeWitt \email{dewittpe@gmail.com}
//'
//' @param x a numeric matrix
//'
//' @return
//' the rank of the matrix as a numeric value.
//'
//' @example examples/matrix_rank.R
//'
//' @references
//'
//' Conrad Sanderson and Ryan Curtin.  Armadillo: a template-based C++ library
//' for linear algebra.  Journal of Open Source Software, Vol. 1, pp. 26, 2016.
//'
//' @export
// [[Rcpp::export]]
double matrix_rank(Rcpp::NumericMatrix x) {
  arma::mat X(x.begin(), x.nrow(), x.ncol(), false);
  return arma::rank(X);
}
