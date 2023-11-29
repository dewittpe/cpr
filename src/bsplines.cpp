// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "cpr.h"

//' @title B-Splines
//'
//' @description An implementation of Carl de Boor's recursive algorithm for
//' building B-splines.
//'
//' @details The difference between this function and \code{splines::bs} come in the
//' attributes associated with the output and default options.  The
//' \code{cpr::bsplines} call is intended to simplify the work needed with
//' respect to the control polygon reduction.  Further, the implementation of
//' \code{cpr::bsplines} is in C++ and tends to be faster than
//' \code{splines::bs}.
//'
//' See the \code{vignette("bsplines", package = "cpr")} for a detailed
//' comparison between the \code{bsplines} and \code{\link[splines]{bs}} calls
//' and notes about B-splines in general.
//'
//' @references
//' C. de Boor, "A practical guide to splines. Revised Edition," Springer, 2001.
//'
//' H. Prautzsch, W. Boehm, M. Paluszny, "Bezier and B-spline Techniques," Springer, 2002.
//'
//' @param x a numeric vector
//' @param iknots internal knots
//' @param df degrees of freedom: sum of the order and internal knots.  Ignored
//' if \code{iknots} is specified.
//' @param bknots boundary knot locations, defaults to \code{range(x)}.
//' @param order order of the piecewise polynomials, defaults to 4L.
//'
//' @seealso \code{\link{plot.cpr_bs}} for plotting the basis,
//' \code{\link{bsplineD}} for building the basis matrices for the first and
//' second derivative of a B-spline.
//'
//' See \code{\link{update_bsplines}} for info on a tool for updating a
//' \code{cpr_bs} object.  This is a similar method to the
//' \code{\link[stats]{update}} function from the \code{stats} package.
//'
//' @examples
//' # build a vector of values to transform
//' xvec <- seq(-3, 5, length = 100)
//'
//' # cubic b-spline
//' bmat <- bsplines(xvec, iknots = c(-2, 0, 1.2, 1.2, 3.0))
//' bmat
//'
//' # plot the splines
//' plot(bmat)                # each spline will be colored by default
//' plot(bmat, color = FALSE) # black and white plot
//' plot(bmat, color = FALSE) + ggplot2::aes(linetype = spline) # add a linetype
//'
//' # Axes
//' # The x-axis, by default, show the knot locations.  Other options are numeric
//' # values, and/or to use a second x-axis
//'
//' plot(bmat, show_xi = TRUE,  show_x = FALSE) # default, knot, symbols, on lower axis
//' plot(bmat, show_xi = FALSE, show_x = TRUE)  # Numeric value for the knot locations
//' plot(bmat, show_xi = TRUE,  show_x = TRUE)  # symbols on bottom, numbers on top
//'
//' # quadratic splines
//' bmat <- bsplines(xvec, iknots = c(-2, 0, 1.2, 1.2, 3.0), order = 3L)
//' bmat
//' plot(bmat) + ggplot2::ggtitle("Quadratic B-splines")
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix bsplines(arma::vec x, arma::vec iknots, arma::vec bknots, unsigned int order) {
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

//Rcpp::NumericVector bsplineD1__impl(arma::vec x, unsigned int j, unsigned int order, arma::vec knots) {
//
//  arma::vec A1;
//  arma::vec A2;
//  arma::vec A3;
//  A1.zeros(x.n_elem);
//  A2.zeros(x.n_elem);
//  A3.zeros(x.n_elem);
//
//  if (knots(j + order - 1) - knots(j) != 0) {
//    bspline A(x, j, order - 1, knots);
//    A1 = A.spline/(knots(j + order - 1) - knots(j));
//  }
//
//  if (knots(j + order) - knots(j + 1) != 0) {
//    bspline B(x, j + 1, order - 1, knots);
//    A2 = B.spline / (knots(j + order) - knots(j + 1));
//  }
//
//  A3 = double(order-1) * (A1 - A2);
//  Rcpp::NumericVector out = arma2vec(A3);
//
//  return out;
//}

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
