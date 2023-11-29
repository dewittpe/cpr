#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "cpr.h"

/* ************************************************************************** */
/*                                   bbasis                                   */

bbasis::bbasis() {
}

bbasis::bbasis(arma::vec& x_, arma::vec & iknots_, arma::vec & bknots_, unsigned int order_) {
  x = x_;
  order = order_;
  iknots = iknots_;
  bknots = bknots_;

  df = order + iknots.n_elem;

  xi.resize(iknots.n_elem + 2 * order);
  xi_star.resize(xi.n_elem - order);
  bmat.resize(x.n_elem, iknots.n_elem + order);

  unsigned int i = 0, j = 0; // indices in for loops to follow

  // define the knot sequence in two for loops, the boundary knots and then the
  // interior knots.
  for (i = 0; i < order; ++i) {
    xi(i) = bknots(0);
    xi(order + iknots.n_elem + i) = bknots(1);
  }

  for (i = 0; i < iknots.n_elem; ++i) {
    xi(order + i) = iknots(i);
  }

  if (!xi.is_sorted()) {
    Rf_error("Knots are not sorted.");
  }

  // define xi_star
  for (i = 0; i < xi_star.n_elem; ++i) {
    xi_star(i) = arma::sum(xi(arma::span(i + 1, i + order - 1))) / double (order - 1);
  }

  // define the basis matrix
  for(i = 0; i < x.n_elem; ++i) {
    for(j = 0; j < order + iknots.n_elem; ++j) {
      bmat(i,j) = B(i, j, order);
    }
  }

  arma::uvec bx = arma::find(x == bknots(1));
  arma::uvec jx(bx.n_elem); jx.fill(j - 1);
  bmat(bx, jx).ones();
}

double bbasis::w(unsigned int i_, unsigned int j_, unsigned int k_) {
  double w = 0.0;

  if ((xi(j_ + k_ - 1) - xi(j_)) > std::sqrt(arma::datum::eps)) {
    w = (x(i_) - xi(j_)) / (xi(j_ + k_ - 1) - xi(j_));
  }

  return(w);
}

double bbasis::B(unsigned int i_, unsigned int j_, unsigned int k_) {
  double rtn;

  if (k_ == 1) {
    if ((xi(j_) <= x(i_)) && (x(i_) < xi(j_ + 1))) {
      rtn = 1.0;
    } else {
      rtn = 0.0;
    }
  } else {
    rtn = w(i_, j_, k_) * B(i_, j_, k_ - 1) + (1.0 - w(i_, j_ + 1, k_)) * B(i_, j_ + 1, k_ - 1);
  }

  return(rtn);
}

/* ************************************************************************** */
/*                               controlpolygon                               */

controlpolygon::controlpolygon(bbasis& bmat_, arma::vec& theta_) {
  bmat = bmat_;
  theta = theta_;

  xi_star.resize(bmat.xi.n_elem - bmat.order);

  for (unsigned int i = 0; i < xi_star.n_elem; ++i) {
    xi_star(i) = arma::sum(bmat.xi(arma::span(i + 1, i + bmat.order - 1))) / double (bmat.order - 1);
  }

  //spline = bmat * theta;
}

/* ************************************************************************** */
/*                               greville_sites                               */
//arma::vec greville_sites(arma::vec & xi, unsigned int order) {
//  arma::vec xi_star(xi.n_elem - order);
//
//  for (unsigned int i = 0; i < xi_star.n_elem; ++i) {
//    xi_star(i) = arma::sum(xi(arma::span(i + 1, i + order - 1))) / double (order - 1);
//  }
//
//  return xi_star;
//}

/* ************************************************************************** */
/*                                  arma2vec                                  */
Rcpp::NumericVector arma2vec(const arma::vec & x) {
    return Rcpp::NumericVector(x.begin(), x.end());
}

/* ************************************************************************** */
/*                                End of File                                 */
/* ************************************************************************** */
