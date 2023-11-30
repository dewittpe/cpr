// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "cpr.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix cpp_bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order) {
  bbasis bb(x, iknots, bknots, order);

  Rcpp::NumericMatrix out = Rcpp::wrap(bb.bmat);
  out.attr("order")   = bb.order;
  out.attr("df")      = bb.df;
  out.attr("iknots")  = arma2vec(bb.iknots);
  out.attr("bknots")  = arma2vec(bb.bknots);
  out.attr("xi")      = arma2vec(bb.xi);
  out.attr("xi_star") = arma2vec(bb.xi_star);
  out.attr("class")   = "cpr_bs";

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix cpp_bsplinesD1(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order) {

  if ((order - 1) <= 0) {
    Rf_error("(order - 1) <= 0");
  }

  bbasis B0(x, iknots, bknots, order);
  arma::vec iknots1(iknots.n_elem + 2);

  iknots1(0) = B0.xi(order - 1);
  for (unsigned int i = 0; i < iknots.n_elem; ++i) {
    iknots1(i + 1) = iknots(i);
  }
  iknots1(iknots1.n_elem - 1) = B0.xi(iknots.n_elem + order);

  bbasis B1(x, iknots1, bknots, order - 1);
  Rcpp::Rcout << "B1.bmat.tail_rows(6)" << B1.bmat.tail_rows(6) << "\n";
  arma::mat D(x.n_elem, iknots.n_elem + order);
  arma::vec A1(x.n_elem), A2(x.n_elem);

  for (unsigned int j = 0; j < D.n_cols; ++j) {

    if (B0.xi(j + order - 1) - B0.xi(j) != 0) {
      A1 = double(order - 1) / (B0.xi(j + order - 1) - B0.xi(j)) * B1.bmat.col(j);
    } else {
      A1.zeros();
    }

    if (B0.xi(j + order) - B0.xi(j + 1) != 0) {
      A2 = double(order - 1) / (B0.xi(j + order) - B0.xi(j + 1)) * B1.bmat.col(j + 1);
    } else {
      A2.zeros();
    }

    D.col(j) = A1 - A2;
  }

  Rcpp::NumericMatrix out = Rcpp::wrap(D);
  //out.attr("order")   = B.order;
  //out.attr("df")      = B.df;
  //out.attr("iknots")  = arma2vec(B.iknots);
  //out.attr("bknots")  = arma2vec(B.bknots);
  //out.attr("xi")      = arma2vec(B.xi);
  //out.attr("xi_star") = arma2vec(B.xi_star);
  //out.attr("derivative") = 1;
  out.attr("class")   = "cpr_bsD1";

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix cpp_bsplinesD2(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order) {

  if ((order - 2) <= 0) {
    Rf_error("(order - 2) <= 0");
  }

  bbasis B0(x, iknots, bknots, order);
  arma::vec iknots1(iknots.n_elem + 4);

  iknots1(0) = B0.xi(order - 2);
  iknots1(1) = B0.xi(order - 1);
  for (unsigned int i = 0; i < iknots.n_elem; ++i) {
    iknots1(i + 2) = iknots(i);
  }
  iknots1(iknots1.n_elem - 2) = B0.xi(iknots.n_elem + order);
  iknots1(iknots1.n_elem - 1) = B0.xi(iknots.n_elem + order + 1);

  bbasis B2(x, iknots1, bknots, order - 2);
  arma::vec A0(x.n_elem), A1(x.n_elem), A2(x.n_elem);
  arma::mat D(x.n_elem, iknots.n_elem + order);

  //Rcpp::Rcout << "B0.xi:" << B0.xi << "\n";
  //Rcpp::Rcout << "B2.xi:" << B2.xi << "\n";

  double a,b;

  for (unsigned int j = 0; j < D.n_cols; ++j) {

    a = B0.xi(j + order - 2) - B0.xi(j);
    if (a != 0.0) {
      A0 = double(order - 2) * (1.0 / a) * B2.bmat.col(j);
    } else {
      A0.zeros();
    }

    a = B0.xi(j + order - 1) - B0.xi(j + 1);
    if (a != 0.0) {
      A1 = double(order - 2) * (1.0 / a) * B2.bmat.col(j + 1);
    } else {
      A1.zeros();
    }

    a = B0.xi(j + order) - B0.xi(j + 2);
    if (a != 0.0) {
      A2 = double(order - 2) * ( 1.0 / a) * B2.bmat.col(j + 2);
    } else {
      A2.zeros();
    }

    a = B0.xi(j + order - 1) - B0.xi(j);
    if ( a != 0.0 ) {
      a = double(order - 1) / a;
    } else {
      a = 0.0;
    }

    b = B0.xi(j + order) - B0.xi(j + 1);
    if ( b != 0.0 ) {
      b = double(order - 1) / b;
    } else {
      b = 0.0;
    }

    D.col(j) = (a * (A0 - A1) - b * (A1 - A2));

  }

  Rcpp::NumericMatrix out = Rcpp::wrap(D);
  out.attr("class")   = "cpr_bsD2";

  return out;
}

//Rcpp::NumericVector bsplineD2__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots) {
//
//  double a = 0.0;
//  double b = 0.0;
//  arma::vec A1;
//  arma::vec A2;
//  arma::vec A3;
//  arma::vec A4;
//  A1.zeros(x.n_elem);
//  A2.zeros(x.n_elem);
//  A3.zeros(x.n_elem);
//  A4.zeros(x.n_elem);
//
//  a = knots(j + order) - knots(j + 2);
//  if (a != 0.0) {
//    bspline Aj(x, j + 2, order - 2, knots);
//    A1 = -1.0 * Aj.spline / a;
//  }
//
//  a = knots(j + order - 1) - knots(j + 1);
//  if (a != 0.0) {
//    bspline Bj(x, j + 1, order - 2, knots);
//    A2 = Bj.spline / a;
//  }
//
//  a = knots(j + order - 2) - knots(j);
//  if (a != 0.0) {
//    bspline Cj(x, j, order - 2, knots);
//    A3 = Cj.spline / a;
//  }
//
//  a = knots(j + order) - knots(j + 1);
//  if (a != 0.0) {
//    a = -1.0 / a;
//  }
//
//  b = knots(j + order - 1) - knots(j);
//  if (b != 0.0) {
//    b = 1.0 / b;
//  }
//
//  A4 = double(order - 1) * double(order - 2) * (a * (A1 + A2) + b * (A3 - A2));
//
//  Rcpp::NumericVector out = arma2vec(A4);
//  return out;
//}
//
