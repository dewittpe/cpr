#' Insert a Knot into a Control Polygon
#'
#' Insert a knot into a control polygon without changing the spline
#'
#' @param x a \code{cpr_cp} object
#' @param xi_prime the value of the knot to insert
#' @param ... not currently used
#'
#' @return a \code{cpr_cp} object
#'
#' @examples
#' x <- seq(1e-5, 5.99999, length.out = 100)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
#' cp0 <- cp(bmat, theta)
#' cp1 <- insert_a_knot(x = cp0, xi_prime = 3)
#' plot(cp0, cp1, color = TRUE, show_spline = TRUE)
#' @export
insert_a_knot <- function(x, xi_prime, ...) {
  UseMethod("insert_a_knot")
}

#' @export
insert_a_knot.cpr_cp <- function(x, xi_prime, ...) {
  bmat_prime <- bsplines(numeric(0), iknots = sort(c(x$iknots, xi_prime)), bknots = x$bknots, order = x$order)
  theta_prime <- refine_theta(xi_prime = xi_prime, xi = x$xi, k = x$order, x$cp$theta)
  cp(bmat_prime, theta_prime)
}
