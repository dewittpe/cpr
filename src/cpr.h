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
  // @param x_       data
  // @param iknots_  internal knots
  // @param bknots_  boundary knots, expected to be length 2 and will be extended to order_-fold
  // @param order_   polynomial order
  bbasis(arma::vec& x_, arma::vec& iknots_, arma::vec& bknots_, unsigned int order_);

  // member methods, see de Boor (2001) page 90, used to define bmat via
  // recursion
  double B(unsigned int i_, unsigned int j_, unsigned int k_);
  double w(unsigned int i_, unsigned int j_, unsigned int k_);
};

/* ************************************************************************** */
/*                             Utility Functions                              */

Rcpp::NumericVector arma2vec(const arma::vec & x);

/* ************************************************************************** */
/*                          Boehm related functions                           */
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
 *   xi_prime: value of the knot to be inserted
 *   k: order of the spline
 *   xi: knot vector into which xi_prime is to be inserted
 * Return:
 *   a matrix
 */
arma::mat W(double xi_prime, const arma::vec& xi, unsigned int k);

/* refine theta
 *
 * Refine the ordinate of a control polygon by inserting a knot
 *
 * theta_{xi U xi'} = W theta_{xi}
 * |              |   |          |
 * \.. return  ../    \. input ./
 *
 * Arguments:
 *   xi_prime: value of the knot to be inserted
 *   k: order of the spline
 *   xi: knot vector into which xi_prime is to be inserted
 *   theta: ordinate vector
 *
 * Return
 *   a vector
 */
arma::vec refine_theta(double xi_prime, const arma::vec& xi, unsigned int k, const arma::vec& theta);

/* coarsen_theta
 *
 * theta_{xi \ xi_j} = (W^T W)^{-1} W^{T} \theta_xi
 *
 * Arguments:
 *   j : the index of xi to omit
 *   xi: full knot sequence
 *   k: polynomial order
 *   theta: theta_xi
 *
 * Return:
 *   a vector, theta_{xi \ xi_j}
 */
arma::vec coarsen_theta(unsigned int j, const arma::vec& xi, unsigned int k, const arma::vec& theta);

/* hat theta
 *
 * \hat{theta}_{xi,j} = W(W^T W)^{-1} W^{T} \theta_xi after omitting and
 * reinserting xi_j
 *
 * Arguments:
 *   j : the index of xi to omit
 *   xi: full knot sequence
 *   k: polynomial order
 *   theta: theta_xi
 *
 * Return:
 *   a R list with the theta_hat, d for the difference from intial to hat
 *
 */
Rcpp::List hat_theta(unsigned int j, const arma::vec& xi, unsigned int k, const arma::vec& theta);

#endif
