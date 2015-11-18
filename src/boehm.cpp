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

double omega(double x, unsigned int j, arma::vec xi, unsigned int k = 4) {
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
arma::mat knot_insertion_matrix__impl(double x, arma::vec xi, unsigned int k = 4) { 
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

// W_hat is the "hat" matrix built from a W matrix
// [[Rcpp::export]]
arma::mat knot_insertion_hat_matrix__impl(double x, arma::vec xi, unsigned int k = 4) { 
  arma::mat kim = knot_insertion_matrix__impl(x, xi, k);
  
 return(kim * (kim.t() * kim).i() * kim.t());
}

// Function for returning the 'weight of importance' for each interior knot
// [[Rcpp::export]]
arma::vec iknot_weights__impl(arma::vec xi, arma::vec theta, unsigned int k = 4, unsigned int p = 2) {

  int iknots = xi.n_elem - 2 * k;

  arma::mat xi_mat(xi.n_elem - 1, iknots);
  arma::vec xi_to_insert = xi(arma::span(k, k + iknots - 1));

  arma::vec w_vec(iknots);

  int i,j,l;

  for(j = 0; j < iknots; ++j) { 
    l = 0; 
    for(i = 0; i < xi_mat.n_rows; ++i) { 
      if (i == k + j) {
        ++l;
      } 
      xi_mat(i, j) = xi(i + l);
    }
  }

  for(j = 0; j < iknots; ++j) { 
    w_vec(j) = arma::norm(theta - knot_insertion_hat_matrix__impl(xi_to_insert(j), xi_mat.col(j), k) * theta, p);
  }

  return(w_vec); 
}


//' Knot Insertion, Removal, and Reinsertion
//' 
//' Functions for the insertion, removal, and reinsertion of internal knots for
//' B-splines.
//'
//' @param x the value of the knot to be inserted into the knot vector
//' @param xi the (whole) knot vector, including the repeated boundary knots
//' @param theta the ordinates of the control polygon vertices
//' @param order the order of the B-spline, defaults to 4 for cubic splines
//'
//' @return numeric vectors
//'
//' @export
//' @rdname boehm
// [[Rcpp::export]]
arma::vec refine_ordinate(double x, arma::vec xi, arma::vec theta, unsigned int k = 4) { 
  return(knot_insertion_matrix__impl(x, xi, k) * theta);
}

/******************************************************************************/
/* End of file                                                                */
/******************************************************************************/
