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

double omega(double x, unsigned int j, unsigned int k, arma::vec xi) {
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

// [[Rcpp::export]]
arma::mat W(double x, unsigned int k, arma::vec xi) { 
  double w;
  int r = xi.n_elem - k;

  // knot insertion matrix (kim)
  arma::mat kim(r + 1, r, arma::fill::zeros);
  kim(0, 0) = 1.0;
  kim(r, r - 1) = 1.0;
  for (int i = 1; i < r; ++i) { 
    w = omega(x, i, k, xi);
    kim(i, i - 1) = 1.0 - w;
    kim(i, i)     = w;
  }
  return(kim); 
}

// W_hat is the "hat" matrix built from a W matrix
// [[Rcpp::export]]
arma::mat W_hat(double x, unsigned int k, arma::vec xi) { 
  arma::mat kim = W(x, k, xi);
  
 return(kim * (kim.t() * kim).i() * kim.t());
}

/******************************************************************************/
/* End of file                                                                */
/******************************************************************************/
