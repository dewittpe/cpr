// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

#ifndef CPR_H
#define CPR_H

struct bspline {
  // member objects
  arma::vec knots;    // full knot sequence
  unsigned int j;     // jth spline
  unsigned int order; // polynomial order
  arma::vec Bj;       // the jth B-spline

  // constructors
  bspline();
  bspline(arma::vec & x, unsigned int j_, unsigned int order_, arma::vec & knots_);

  // member methods, see de Boor (2001) page .... These 
  double B(double x, unsigned int j_, unsigned int k_);  
  double w(double x, unsigned int j_, unsigned int k_);
};

struct bbasis {
  // member objects
  unsigned int order;   // polynomial order
  aram::vec iknots;     // internal knots
  arma::vec bknots(2);  // boundary knot values
  arma::vec knots;      // full knot sequence including the order-fold boundary knots

  arma::mat bmat;       // basis matrix

  // constructors
  bbasis::bbasis(arma::vec x, arma::vec iknots_, arma::vec bknots_, unsigned int order_) { 
};

struct controlpolygon {
  // member objects
  arma::vec xi_star;
  bbasis bmat;
  arma::vec theta;

  // constructors
  controlpolygon(arma::vec & bmat_, arma::vec & theta_);
};

arma::vec greville_sites(arma::vec & xi, unsigned int order)

#endif
