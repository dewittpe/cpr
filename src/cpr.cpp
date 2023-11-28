#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "cpr.h"

/* ************************************************************************** */
/*                                   bbasis                                   */

bbasis::bbasis() {
}

bbasis::bbasis(arma::vec & x, arma::vec & iknots_, arma::vec & bknots_, unsigned int order_) {
  order = order_;
  iknots = iknots_;
  bknots = bknots_;
  knots.resize(iknots.n_elem + 2 * order);

  bmat.resize(x.n_elem, iknots.n_elem + order);

  unsigned int i,j; // indices in for loops to follow

  // define the knot sequence in two for loops, the boundary knots and then the
  // interior knots.
  for (i = 0; i < order; ++i) {
    knots(i) = bknots(0);
    knots(order + iknots.n_elem + i) = bknots(1);
  }

  for (i = 0; i < iknots.n_elem; ++i) {
    knots(order + i) = iknots(i);
  }

  if (!knots.is_sorted()) {
    Rcpp::warning("Sorting knots");
    knots = arma::sort(knots);
  }

  // define the basis matrix
  for(j = 0; j < order + iknots.n_elem; ++j) {
    bmat.col(j) = bspline(x, j, order, knots).spline;
  }

  arma::uvec bx = arma::find(x == bknots(1));
  arma::uvec jx(bx.n_elem); jx.fill(j - 1);
  bmat(bx, jx).fill(1.0);
}

/* ************************************************************************** */
/*                                  bspline                                   */

bspline::bspline(){}

bspline::bspline(arma::vec & x, unsigned int j_, unsigned int order_, arma::vec & knots_){
  j = j_;
  order = order_;
  knots = knots_;
  spline.zeros(x.n_elem);

  for (unsigned int i = 0; i < x.n_elem; ++i) {
    if (x(i) >= knots(j) && x(i) <= knots(j + order)) {
      spline(i) = B(x(i), j, order);
    }
  }
}

double bspline::w(double x, unsigned int j_, unsigned int k_) {
  double w = 0.0;
  if (knots(j_ + k_ - 1) != knots(j_)) {
    w = (x - knots(j_)) / (knots(j_ + k_ - 1) - knots(j_));
  }

  return(w);
}

double bspline::B(double x, unsigned int j_, unsigned int k_) {
  double rtn;

  if (k_ == 1) {
    if ((knots(j_) <= x) && (x < knots(j_ + 1))) {
      rtn = 1.0;
    } else {
      rtn = 0.0;
    }
  } else {
    rtn = w(x, j_, k_) * B(x, j_, k_ - 1) + (1.0 - w(x, j_ + 1, k_)) * B(x, j_ + 1, k_ - 1);
  }

  return(rtn);
}

/* ************************************************************************** */
/*                               controlpolygon                               */

controlpolygon::controlpolygon(bbasis & bmat_, arma::vec & theta_) {
  bmat = bmat_;
  theta = theta_;

  xi_star = greville_sites(bmat.knots, bmat.order);
}

/* ************************************************************************** */
/*                               greville_sites                               */
arma::vec greville_sites(arma::vec & xi, unsigned int order) {
  arma::vec xi_star(xi.n_elem - order);

  for (unsigned int i = 0; i < xi_star.n_elem; ++i) {
    xi_star(i) = arma::sum(xi(arma::span(i + 1, i + order - 1))) / double (order - 1);
  }

  return xi_star;
}

/* ************************************************************************** */
/*                                  arma2vec                                  */
Rcpp::NumericVector arma2vec(const arma::vec & x) {
    return Rcpp::NumericVector(x.begin(), x.end());
}

/* ************************************************************************** */
/*                                End of File                                 */
/* ************************************************************************** */
