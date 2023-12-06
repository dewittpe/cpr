// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

#ifndef CPR_H
#define CPR_H

/* ************************************************************************** */
/*                                 Structures                                 */
/*
 * bbasis  collection of bsplines
 * controlpolygon contains the greville sites and theta for the control polygon
 * hull around f(x) = bbasis %*% theta
 */

struct bbasis {
  // member objects
  unsigned int order;   // polynomial order
  unsigned int df;      // degrees of freedom
  arma::vec iknots;     // internal knots
  arma::vec bknots;     // boundary knot values
  arma::vec xi;         // full knot sequence including the order-fold boundary knots
  arma::vec xi_star;

  arma::vec x;          // data
  arma::mat bmat;       // basis matrix

  // constructors
  // @param x data
  bbasis();
  bbasis(arma::vec& x_, arma::vec& iknots_, arma::vec& bknots_, unsigned int order_);

  // member methods, see de Boor (2001) page 90, used to define bmat via
  // recursion
  double B(unsigned int i_, unsigned int j_, unsigned int k_);
  double w(unsigned int i_, unsigned int j_, unsigned int k_);
};

struct controlpolygon {
  // member objects
  bbasis bmat;
  arma::vec xi_star;
  arma::vec theta;
  arma::vec spline;

  // constructors
  controlpolygon(bbasis& bmat_, arma::vec& theta_);
};

/* ************************************************************************** */
/*                                 Functions                                  */

//arma::vec greville_sites(arma::vec & xi, unsigned int order);

Rcpp::NumericVector arma2vec(const arma::vec & x);

/* omega
 *
 * weight used in the recursion definition of B-splines (alternative defined
 * within bbasis).  This version will be used extensively when building the W
 * matrix used for inserting a knot into a knot sequence without changing the
 * spline function.
 *
 * Arguments:
 *   x: value of the knot to be inserted
 *   j: index of for the knot vector
 *   k: order of the spline
 *   xi: knot vector
 * Return:
 *   a double between 0 and 1 inclusive
 */
double omega(double x, unsigned int j, const arma::vec& xi, unsigned int k);

/* W
 *
 * knot insertion matrix, i.e., theta_{xi U xi'} = W theta_{xi}
 *
 * Arguments:
 *   x: value of the knot to be inserted
 *   k: order of the spline
 *   xi: knot vector into which x is to be inserted
 * Return:
 *   a matrix
 */
arma::mat W(double x, const arma::vec& xi, unsigned int k);

#endif
