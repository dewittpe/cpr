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
#endif
