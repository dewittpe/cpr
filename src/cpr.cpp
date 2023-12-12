// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "cpr.h"

/* ************************************************************************** */
/*                                   bbasis                                   */

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

  // The following three lines were a hack to get a non-zero b-spline when x =
  // bknots(1).  This works, but has some conceptual issues.  First, the
  // B-splines are right continuous and this work forces left continuity at
  // bknots(1).  By omitting this code and returning a warning it is left to the
  // end user to define the boundary knots well, or to .  I'm cons
  //arma::uvec bx = arma::find(x == bknots(1));
  //arma::uvec jx(bx.n_elem); jx.fill(bmat.n_cols - 1);
  //bmat(bx, jx).ones();
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
    if ( ((xi(j_) <= x(i_)) && (x(i_) < xi(j_ + 1)))) {
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
/*                                  arma2vec                                  */
Rcpp::NumericVector arma2vec(const arma::vec & x) {
    return Rcpp::NumericVector(x.begin(), x.end());
}

/* ************************************************************************** */
/*                                   omega                                    */
double omega(double x, unsigned int j, const arma::vec& xi, unsigned int k) {
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

/* ************************************************************************** */
/*                          Knot Insertion Matrices                           */
// [[Rcpp::export]]
arma::mat W(double xi_prime, const arma::vec& xi, unsigned int k) {
  double w;
  int r = xi.n_elem - k;

  arma::mat M(r + 1, r, arma::fill::zeros);
  M(0, 0) = 1.0;
  M(r, r - 1) = 1.0;
  for (int i = 1; i < r; ++i) {
    w           = omega(xi_prime, i, xi, k);
    M(i, i - 1) = 1.0 - w;
    M(i, i)     = w;
  }
  return(M);
}

// [[Rcpp::export]]
arma::vec refine_theta(double xi_prime, const arma::vec& xi, unsigned int k, const arma::vec& theta){
  return(W(xi_prime, xi, k) * theta);
}

// [[Rcpp::export]]
arma::vec coarsen_theta(unsigned int j, const arma::vec& xi, unsigned int k, const arma::vec& theta){

  arma::vec xi_sans_j(xi.n_elem - 1);
  for (unsigned int i = 0; i < xi_sans_j.n_elem; ++i) {
    if (i < j) {
      xi_sans_j(i) = xi(i);
    } else if (i >= j) {
      xi_sans_j(i) = xi(i + 1);
    }
  }

  arma::mat w = W(xi(j), xi_sans_j, k);

  return ((w.t() * w).i() * w.t() * theta);
}

// [[Rcpp::export]]
Rcpp::List hat_theta(unsigned int j, const arma::vec& xi, unsigned int k, const arma::vec& theta, bool calculate_F, const arma::mat& Sigma) {
  arma::vec xi_sans_j(xi.n_elem - 1);
  for (unsigned int i = 0; i < xi_sans_j.n_elem; ++i) {
    if (i < j) {
      xi_sans_j(i) = xi(i);
    } else if (i >= j) {
      xi_sans_j(i) = xi(i + 1);
    }
  }

  arma::mat w = W(xi(j), xi_sans_j, k);
  arma::mat HAT = w * (w.t() * w).i() * w.t();
  arma::mat IHAT = arma::eye(theta.n_elem, theta.n_elem) - HAT;

  double chisq;
  if (calculate_F) {
    arma::mat F = (IHAT * theta).t() * arma::pinv(IHAT * Sigma * IHAT.t()) * (IHAT * theta);
    chisq = F(0,0);
  } else {
    chisq = NA_REAL;
  }

  return (
    Rcpp::List::create(
        Rcpp::Named("theta") = (HAT * theta),
        Rcpp::Named("d") = IHAT * theta,
        //Rcpp::Named("influence") = arma::norm(IHAT * theta, 2),
        Rcpp::Named("influence") = arma::sum(arma::pow(IHAT * theta, 2)),
        Rcpp::Named("chisq") = chisq
        )
  );
}

/* ************************************************************************** */
/*                                End of File                                 */
/* ************************************************************************** */
