/******************************************************************************/
/* file: boehm.cpp                                                            */
/* author: "Peter DeWitt" <peter.dewitt@ucdenver.edu>                         */
/*                                                                            */
/* Functions needed to use Boehm (1980) methods for inserting a knot into a   */
/* B-spline.                                                                  */
/*                                                                            */
/******************************************************************************/

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

/* Function
 * omega
 * Args:
 *   x: value of the knot to be inserted
 *   j: index of for the knot vector
 *   k: order of the spline
 *   xi: knot vector
 * Return:
 *   a double between 0 and 1 inclusive
 */

double omega(double x, unsigned int j, const arma::vec& xi, unsigned int k = 4) {
  if (x <= xi(j)) { 
    return(0);
  } 
  else if (x >= xi(j + k - 1)) { 
    return(1);
  } 
  else {
    return((x - xi(j)) / (xi(j + k - 1) - xi(j)));
  } 
}

/* Function:
 * W
 * Build the 'knot insertion' matrix
 * Arguments:
 *   x: value of the knot to be inserted
 *   k: order of the spline
 *   xi: knot vector into which x is to be inserted
 * Return:
 *   a matrix
 */

arma::mat knot_insertion_matrix(double x, const arma::vec& xi, unsigned int k = 4) { 
  double w;
  int r = xi.n_elem - k;

  arma::mat kim(r + 1, r, arma::fill::zeros);
  kim(0, 0) = 1.0;
  kim(r, r - 1) = 1.0;
  for (int i = 1; i < r; ++i) { 
    w = omega(x, i, xi, k);
    kim(i, i - 1) = 1.0 - w;
    kim(i, i)     = w;
  }
  return(kim); 
}

// knot_coarsen_matrix:  
arma::mat knot_coarsen_matrix(double x, const arma::vec& xi, unsigned int k = 4) { 
  arma::mat kim = knot_insertion_matrix(x, xi, k);
  
 return((kim.t() * kim).i() * kim.t());
}

// knot_insertion_hat_matrix is the "hat" matrix built from a W matrix
arma::mat knot_insertion_hat_matrix(double x, const arma::vec& xi, unsigned int k = 4) { 
  arma::mat kim = knot_insertion_matrix(x, xi, k);
  
 return(kim * (kim.t() * kim).i() * kim.t());
}


//' Knot Insertion, Removal, and Reinsertion
//' 
//' Functions for the insertion, removal, and reinsertion of internal knots for
//' B-splines.
//'
//' \code{refine_ordinate} provides the estimated ordinates of the control
//' polygon's vertex sequence after inserting the value \code{x} into the knot
//' vector \code{xi}.
//'
//' \code{coarsen_ordinate} provides the estimated ordinates of the control
//' polygon's vertex sequence after the removal of the the value \code{x} from the
//' knot vector \code{xi}.  The expected input for this function is the value
//' \code{x} to insert into \code{xi}.  That is, to find the estimate of the
//' coarsened ordinates by removing the value 2 from the vector (0, 0, 0, 0, 1,
//' 2, 3, 4, 4, 4, 4) (the knot vector for a cubic B-spline with boundary knots
//' at zero and four and internal knots 1, 2, 3) should be specified by
//' \code{coarsen_ordinate(x = 2, xi = c(0, 0, 0, 0, 1, 3, 4, 4, 4, 4), theta)}.
//'
//' The function \code{hat_ordinate} is the coarsen-then-refine estimate of the
//' ordinate vector.  The name comes from the the use of a hat matrix based on the
//' in knot insertion matrix.
//'
//' Examples for the \code{refine_ordinate}, \code{coarsen_ordinate}, and
//' \code{hat_ordinate} are best shown in the vignette, 
//' \code{vignette("cpr-pkg", package = "cpr")}.
//'
//' \code{iknot_weights} returns a vector with the 'importance weight' of each
//' of the internal knots in \code{xi}.
//'
//' @param x the value of the knot to be inserted into the knot vector
//' @param xi the (whole) knot vector, including the repeated boundary knots.
//'   Regardless of refinement or coarsening, this vector should be the
//'   'reduced' vector such that x will be added to it.  See details and
//'   examples.
//' @param theta the ordinates of the control polygon vertices
//' @param order the order of the B-spline, defaults to 4 for cubic splines
//'
//' @return numeric vectors
//'
//' @examples
//' \dontrun{
//' # See the vignette
//' vignette("cpr-pkg", package = "cpr")
//' }
//'
//' @export
//' @rdname boehm
// [[Rcpp::export]]
arma::vec refine_ordinate(double x, const arma::vec& xi, const arma::vec& theta, unsigned int order = 4) { 
  return(knot_insertion_matrix(x, xi, order) * theta);
}
//' @export
//' @rdname boehm
// [[Rcpp::export]]
arma::vec coarsen_ordinate(double x, const arma::vec& xi, const arma::vec& theta, unsigned int order = 4) { 
  return(knot_coarsen_matrix(x, xi, order) * theta);
}
//' @export
//' @rdname boehm
// [[Rcpp::export]]
arma::vec hat_ordinate(double x, const arma::vec& xi, const arma::vec& theta, unsigned int order = 4) { 
  return(knot_insertion_hat_matrix(x, xi, order) * theta);
}
//' @export
//' @rdname boehm
// [[Rcpp::export]]
arma::mat insertion_matrix(double x, const arma::vec& xi, unsigned int order = 4) { 
  return(knot_insertion_matrix(x, xi, order));
}

// [[Rcpp::export]]
Rcpp::NumericVector weigh_iknots(const arma::vec& xi, const arma::mat& theta, unsigned int order = 4, unsigned int p = 2) {

  int iknots = xi.n_elem - 2 * order;

  arma::mat xi_mat(xi.n_elem - 1, iknots);
  arma::vec xi_to_insert = xi(arma::span(order, order + iknots - 1));

  arma::mat w_mat(iknots, theta.n_cols);
  arma::vec w_vec(iknots);

  int i,j,l;

  for(j = 0; j < iknots; ++j) { 
    l = 0; 
    for(i = 0; i < xi_mat.n_rows; ++i) { 
      if (i == order + j) {
        ++l;
      } 
      xi_mat(i, j) = xi(i + l);
    }
  }

  for(i = 0; i < w_mat.n_cols; ++i) { 
    for(j = 0; j < iknots; ++j) { 
      w_mat(j, i) = arma::norm(theta.col(i) - knot_insertion_hat_matrix(xi_to_insert(j), xi_mat.col(j), order) * theta.col(i), p);
    }
  }

  w_vec = arma::max(w_mat, 1);

  return Rcpp::NumericVector(w_vec.begin(), w_vec.end());
}

/******************************************************************************/
/* End of file                                                                */
/******************************************************************************/
