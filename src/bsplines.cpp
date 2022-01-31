// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "cpr.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix bbasis__impl(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order) {
  bbasis bb(x, iknots, bknots, order);

  Rcpp::NumericMatrix out = Rcpp::wrap(bb.bmat);
  out.attr("order")   = bb.order;
  out.attr("iknots")  = arma2vec(bb.iknots);
  out.attr("bknots")  = arma2vec(bb.bknots);
  out.attr("xi")      = arma2vec(bb.knots);
  out.attr("xi_star") = arma2vec(greville_sites(bb.knots, bb.order));
  out.attr("class")   = "cpr_bs";

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector bsplineD1__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots) {

  arma::vec A1;
  arma::vec A2;
  arma::vec A3;
  A1.zeros(x.n_elem);
  A2.zeros(x.n_elem);
  A3.zeros(x.n_elem);

  if (knots(j + order - 1) - knots(j) != 0) {
    bspline A(x, j, order - 1, knots);
    A1 = A.spline/(knots(j + order - 1) - knots(j));
  }

  if (knots(j + order) - knots(j + 1) != 0) {
    bspline B(x, j + 1, order - 1, knots);
    A2 = B.spline / (knots(j + order) - knots(j + 1));
  }

  A3 = double(order-1) * (A1 - A2);
  Rcpp::NumericVector out = arma2vec(A3);

  return out;
}

//[[Rcpp::export]]
Rcpp::NumericVector bsplineD2__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots) {

  double a = 0.0;
  double b = 0.0;
  arma::vec A1;
  arma::vec A2;
  arma::vec A3;
  arma::vec A4;
  A1.zeros(x.n_elem);
  A2.zeros(x.n_elem);
  A3.zeros(x.n_elem);
  A4.zeros(x.n_elem);

  a = knots(j + order) - knots(j + 2);
  if (a != 0.0) {
    bspline Aj(x, j + 2, order - 2, knots);
    A1 = -1.0 * Aj.spline / a;
  }

  a = knots(j + order - 1) - knots(j + 1);
  if (a != 0.0) {
    bspline Bj(x, j + 1, order - 2, knots);
    A2 = Bj.spline / a;
  }

  a = knots(j + order - 2) - knots(j);
  if (a != 0.0) {
    bspline Cj(x, j, order - 2, knots);
    A3 = Cj.spline / a;
  }

  a = knots(j + order) - knots(j + 1);
  if (a != 0.0) {
    a = -1.0 / a;
  }

  b = knots(j + order - 1) - knots(j);
  if (b != 0.0) {
    b = 1.0 / b;
  }

  A4 = double(order - 1) * double(order - 2) * (a * (A1 + A2) + b * (A3 - A2));

  Rcpp::NumericVector out = arma2vec(A4);
  return out;
}

