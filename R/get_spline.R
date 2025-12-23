#' Get the Control Polygon and the Spline Function
#'
#' Generate \code{data.frame}s for interpolating and plotting a spline
#' function, given a \code{cpr_cp} or \code{cpr_cn} object.
#'
#' A control polygon, \code{cpr\_cp} object, has a spline function f(x).
#' \code{get_spline} returns a list of two \code{data.frames}.  The \code{cp}
#' element is a \code{data.frame} with the (x, y) coordinates of control points and
#' the \code{spline} element is a \code{data.frame} with \code{n} rows for
#' interpolating f(x).
#'
#' For a control net, \code{cpr\_cn} object, the return is the same as for a
#' \code{cpr\_cp} object, but conceptually different.  Where \code{cpr\_cp}
#' objects have a univariate spline function, \code{cpr\_cn} objects have
#' multivariate spline surfaces.  \code{get_spline} returns a "slice" of the
#' higher-dimensional object.  For example, consider a three-dimensional control
#' net defined on the unit cube with marginals \code{x1}, \code{x2}, and
#' \code{x3}.  The implied spline surface is the function f(x1, x2, x3).
#' \code{get_spline(x, margin = 2, at = list(0.2, NA, 0.5))} would
#' return the control polygon and spline surface for f(0.2, x, 0.5).
#'
#' See \code{\link{get_surface}} for taking a two-dimensional slice of a
#' three-plus dimensional control net, or, for generating a useful data set for
#' plotting the surface of a two-dimensional control net.
#'
#' @param x a \code{cpr_cp} or \code{cpr_cn} object.
#' @param margin an integer identifying the marginal of the control net to slice
#' along.  Only used when working with \code{x} as a \code{cpr_cn} object.
#' @param at a point value for marginals not defined in the \code{margin}.  Only
#' used when \code{x} is a \code{cpr_cn} object.  Expected input is a list of
#' length \code{length(attr(x, "bspline_list"))}.  Entries for elements
#' \code{marginal} are ignored.  If omitted, the midpoint between the boundary
#' knots for each marginal is used.
#' @param n the length of sequence to use for interpolating the spline function.
#' @param se if \code{TRUE} return the estimated standard error for the spline
#' or the derivative.
#' @param derivative A value of 0 (default) returns the spline, 1 the first
#' derivative, 2 the second derivative.
#'
#' @return a \code{data.frame} \code{n} rows and two columns \code{x} and
#' \code{y}, the values for the spline.  A third column with the standard error
#' is returned if requested.
#'
#' @seealso \code{\link{get_surface}}
#'
#' @examples
#' data(spdg, package = "cpr")
#'
#' ## Extract the control polygon and spline for plotting.  We'll use base R
#' ## graphics for this example.
#' a_cp <- cp(pdg ~ bsplines(day, df = 10, bknots = c(-1, 1)), data = spdg)
#'
#' spline <- get_spline(a_cp)
#' plot(spline$x, spline$y, type = "l")
#'
#' # compare to the plot.cpr_cp method
#' plot(a_cp, show_spline = TRUE)
#'
#' # derivatives
#' f0 <- function(x) {
#'   #(x + 2) * (x - 1) * (x - 3)
#'   x^3 - 2 * x^2 - 5 * x + 6
#' }
#' f1 <- function(x) {
#'   3 * x^2 - 4 * x - 5
#' }
#' f2 <- function(x) {
#'   6 * x - 4
#' }
#'
#' x <- sort(runif(n = 100, min = -3, max = 5))
#' bknots = c(-3, 5)
#' bmat <- bsplines(x, bknots = bknots)
#' theta <- coef(lm(f0(x) ~ bsplines(x, bknots = bknots) + 0) )
#'
#' cp0 <- cp(bmat, theta)
#' spline0 <- get_spline(cp0, derivative = 0)
#' spline1 <- get_spline(cp0, derivative = 1)
#' spline2 <- get_spline(cp0, derivative = 2)
#'
#' old_par <- par()
#'
#' par(mfrow = c(1, 3))
#' plot(x, f0(x), type = "l", main = "spline")
#' points(spline0$x, spline0$y, pch = 2, col = 'blue')
#'
#' plot(x, f1(x), type = "l", main = "first derivative")
#' points(spline1$x, spline1$y, pch = 2, col = 'blue')
#'
#' plot(x, f2(x), type = "l", main = "second derivative")
#' points(spline2$x, spline2$y, pch = 2, col = 'blue')
#'
#' par(old_par)
#'
#' @export
get_spline <- function(x, margin = 1, at, n = 100, se = FALSE, derivative = 0) {
  UseMethod("get_spline")
}

#' @export
get_spline.cpr_cp <- function(x, margin = 1, at, n = 100, se = FALSE, derivative = 0) {
  xvec <- seq(min(x$bknots), max(x$bknots) - sqrt(.Machine$double.eps), length = n)

  stopifnot(length(derivative) == 1L)

  # basis matrix
  if (derivative == 0) {
    B <- bsplines(xvec, iknots = x$iknots, bknots = x$bknots, order = x$order)
  } else if (derivative == 1) {
    B <- bsplineD(xvec, iknots = x$iknots, bknots = x$bknots, order = x$order, derivative = 1)
  } else if (derivative == 2) {
    B <- bsplineD(xvec, iknots = x$iknots, bknots = x$bknots, order = x$order, derivative = 2)
  } else {
    stop("derivative needs to be 0, 1, or 2")
  }

  rtn <- list(x = xvec, y = as.numeric(B %*% x[["cp"]][["theta"]]), se = NULL)
  if (se) {
    if (!is.null(x$vcov_theta)) {
      rtn[["se"]] <- sqrt(diag(B %*% x$vcov_theta %*% t(B)))
    } else {
      warning(sprintf("vcov_theta of %s is NULL", deparse(substitute(x))))
    }
  }
  rtn <- Filter(Negate(is.null), rtn)
  rtn <- as.data.frame(rtn)
  rtn
}

#' @export
get_spline.cpr_cn <- function(x, margin = 1, at, n = 100, se = FALSE, derivative = c(0, 1, 2)) {

  if (length(margin) > 1) {
    stop("use get_surface when length(margin) > 1.")
  }

  if (missing(at)) {
    at <- lapply(lapply(x$bspline_list, attr, which = "bknots"), mean)
  }

  dfs    <- sapply(x$bspline_list, ncol)
  bknots <- lapply(x$bspline_list, attr, which = "bknots")
  iknots <- lapply(x$bspline_list, attr, which = "iknots")
  orders <- lapply(x$bspline_list, attr, which = "order")

  mbs <- mapply(bsplines, x = at, iknots = iknots, bknots = bknots, order = orders, SIMPLIFY = FALSE)
  tensor <- do.call(build_tensor, mbs[-margin])
  thetas <- apply(array(x$cn$theta, dim = dfs), margin, function(x) x)
  marginal_cp <- cp(x$bspline_list[[margin]], t(tensor %*% thetas))
  get_spline.cpr_cp(marginal_cp, margin = margin, at = at, n = n)
}
