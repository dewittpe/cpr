#' Get the Control Polygon and the Spline Function
#'
#' Generate \code{data.frame}s for interpolating and plotting a spline
#' function, given a \code{cpr_cp} or \code{cpr_cn} object.
#'
#' A control polygon, \code{cpr\_cp} object, has a spline function f(x).
#' \code{get_spline} returns a list of two \code{data.frame}.  The \code{cp}
#' element is a \code{data.frame} with the (x, y) coordinates control points and
#' the \code{spline} element is a \code{data.frame} with \code{n} rows for
#' interpolating f(x).
#'
#' For a control net, \code{cpr\_cn} object, the return is the same as for a
#' \code{cpr\_cp} object, but conceptually different.  Where a \code{cpr\_cp}
#' objects have a uni-variable spline function, \code{cpr\_cn} have
#' multi-variable spline surfaces.  \code{get_spline} returns a "slice" of the
#' higher dimensional object.  For example, consider a three-dimensional control
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
#' along.  Only used when working \code{x} is a \code{cpr_cn} object.
#' @param at point value for marginals not defined in the \code{margin}.  Only
#' used when \code{x} is a \code{cpr_cn} object.  Expected input is a list of
#' length \code{length(attr(x, "bspline_list"))}.  Entries for elements
#' \code{marginal} are ignored.  If omitted, the midpoint between the boundary
#' knots for each marginal is used.
#' @param n the length of sequence to use for interpolating the spline function.
#'
#' @return a \code{data.frame} \code{n} rows and two columns \code{x} and
#' \code{y}, the  values for the spline.
#'
#' @seealso \code{\link{get_surface}}
#'
#' @examples
#' data(spdg, package = "cpr")
#'
#' ## Extract the control polygon and spline for plotting.  We'll use base R
#' ## graphics for this example.
#' a_cp <- cp(pdg ~ bsplines(day, df = 10), data = spdg)
#'
#' spline <- get_spline(a_cp)
#' plot(spline$x, spline$y, type = "b")
#'
#' # compare to the plot.cpr_cp method
#' plot(a_cp, show_spline = TRUE)
#'
#' @export
get_spline <- function(x, margin = 1, at, n = 100) {
  UseMethod("get_spline")
}

#' @export
get_spline.cpr_cp <- function(x, margin = 1, at, n = 100) {
  xvec <- seq(min(x$bknots), max(x$bknots) - sqrt(.Machine$double.eps), length = n)
  bmat <- bsplines(xvec, iknots = x$iknots, bknots = x$bknots, order = x$order)
  data.frame(x = xvec, y = as.numeric(bmat %*% x[["cp"]][["theta"]]))
}

#' @export
get_spline.cpr_cn <- function(x, margin = 1, at, n = 100) {

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
