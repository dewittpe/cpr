// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

//' Rank of a Matrix
//'
//' Determine the rank (number of linearly independent columns) of a matrix.
//'
//' Implementation via the Armadillo C++ linear algebra library.  The function
//' returns the rank of the matrix \code{x}.  The computation is based on the
//' singular value decomposition of the matrix; a std::runtime_error exception
//' will be thrown if the decomposition fails.  Any singular values less than
//' the tolerance are treated as zeros.  The tolerance is
//' \code{max(m, n) * max_sv * arma::datum::eps}, where \code{m} is the number
//' of rows of \code{x}, \code{n} is the number of columns of \code{x},
//' \code{max_sv} is the maximal singular value of \code{x}, and
//' \code{arma::datum::eps} is the difference between 1 and the least value
//' greater than 1 that is representable.
//'
//' @param x a numeric matrix
//'
//' @return
//' the rank of the matrix as a numeric value.
//'
//' @examples
//' # Check the rank of a matrix
//' set.seed(42)
//' mat <- matrix(rnorm(25000 * 120), nrow = 25000)
//' matrix_rank(mat) == ncol(mat)
//' matrix_rank(mat) == 120L
//'
//' # A full rank B-spline basis
//' bmat <- bsplines(seq(0, 1, length = 100), df = 15)
//' matrix_rank(bmat) == 15L
//'
//' # A rank deficient B-spline basis
//' bmat <- bsplines(seq(0, 1, length = 100), iknots = c(0.001, 0.002))
//' ncol(bmat) == 6L
//' matrix_rank(bmat) == 5L
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
