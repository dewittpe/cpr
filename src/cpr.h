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
  arma::vec spline;   // the jth B-spline

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
  arma::vec iknots;     // internal knots
  arma::vec bknots;     // boundary knot values
  arma::vec knots;      // full knot sequence including the order-fold boundary knots

  arma::mat bmat;       // basis matrix

  // constructors
  bbasis();
  bbasis(arma::vec & x, arma::vec & iknots_, arma::vec & bknots_, unsigned int order_);

  // operators
  // bbasis &operator =(const bbasis & b) {
  //   order  = b.order;
  //   iknots = b.iknots;
  //   bknots = b.bknots;
  //   knots  = b.knots;
  //   bmat   = b.bmat;
  // } 
};

struct controlpolygon {
  // member objects
  arma::vec xi_star;
  bbasis bmat;
  arma::vec theta;

  // constructors
  controlpolygon(bbasis & bmat_, arma::vec & theta_);
};

arma::vec greville_sites(arma::vec & xi, unsigned int order); 

Rcpp::NumericVector arma2vec(const arma::vec & x);
#endif
