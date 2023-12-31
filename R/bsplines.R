#' B-Splines
#'
#' An implementation of Carl de Boor's recursive algorithm for building
#' B-splines.
#'
#' There are several differences between this function and \code{\link[splines]{bs}}.
#'
#' The most important difference is how the two methods treat the right-hand end
#' of the support.  \code{\link[splines]{bs}} uses a pivot method to allow for
#' extrapolation and thus returns a basis matrix where non-zero values exist on
#' the \code{max(Boundary.knots)} (\code{\link[splines]{bs}} version of
#' \code{bsplines}'s \code{bknots}).  \code{bsplines} use a strict definition of
#' the splines where the support is open on the right hand side, that is,
#' \code{bsplines} return right-continuous functions.
#'
#' Additionally, the attributes of the object returned by \code{bsplines} are
#' different from the attributes of the object returned by
#' \code{\link[splines]{bs}}. See the \code{vignette(topic = "cpr", package =
#' "cpr")} for a detailed comparison between the \code{bsplines} and
#' \code{\link[splines]{bs}} calls and notes about B-splines in general.
#'
#' @references
#' C. de Boor, "A practical guide to splines. Revised Edition," Springer, 2001.
#'
#' H. Prautzsch, W. Boehm, M. Paluszny, "Bezier and B-spline Techniques," Springer, 2002.
#'
#' @param x a numeric vector
#' @param iknots internal knots
#' @param df degrees of freedom: sum of the order and internal knots.  Ignored
#' if \code{iknots} is specified.
#' @param bknots boundary knot locations, defaults to \code{range(x)}.
#' @param order order of the piecewise polynomials, defaults to 4L.
#'
#' @seealso \code{\link{plot.cpr_bs}} for plotting the basis,
#' \code{\link{bsplineD}} for building the basis matrices for the first and
#' second derivative of a B-spline.
#'
#' See \code{\link{update_bsplines}} for info on a tool for updating a
#' \code{cpr_bs} object.  This is a similar method to the
#' \code{\link[stats]{update}} function from the \code{stats} package.
#'
#' \code{vignette(topic = "cpr", package = "cpr")} for details on B-splines and
#' the control polygon reduction method.
#'
#' @examples
#' # build a vector of values to transform
#' xvec <- seq(-3, 4.9999, length = 100)
#'
#' # cubic b-spline
#' bmat <- bsplines(xvec, iknots = c(-2, 0, 1.2, 1.2, 3.0), bknots = c(-3, 5))
#' bmat
#'
#' # plot the splines
#' plot(bmat)                # each spline will be colored by default
#' plot(bmat, color = FALSE) # black and white plot
#' plot(bmat, color = FALSE) + ggplot2::aes(linetype = spline) # add a linetype
#'
#' # Axes
#' # The x-axis, by default, show the knot locations.  Other options are numeric
#' # values, and/or to use a second x-axis
#'
#' plot(bmat, show_xi = TRUE,  show_x = FALSE) # default, knot, symbols, on lower axis
#' plot(bmat, show_xi = FALSE, show_x = TRUE)  # Numeric value for the knot locations
#' plot(bmat, show_xi = TRUE,  show_x = TRUE)  # symbols on bottom, numbers on top
#'
#' # quadratic splines
#' bmat <- bsplines(xvec, iknots = c(-2, 0, 1.2, 1.2, 3.0), order = 3L)
#' bmat
#' plot(bmat) + ggplot2::ggtitle("Quadratic B-splines")
#'
#' @export
bsplines <- function(x, iknots = NULL, df = NULL, bknots = range(x), order = 4L) {

  if (is.list(x)) {
    stop("x is a list. Use btensor instead of bsplines.")
  }

  stopifnot(length(bknots) == 2L)
  order <- as.integer(order)
  if (order <= 1) {
    stop("order needs to be an integer value >= 2.")
  }

  if (any(x < min(bknots))) {
    warning("At least one x value < min(bknots)")
  }

  if (any(x >= max(bknots))) {
    warning("At least one x value >= max(bknots)")
  }

  iknots <- iknots_or_df(x, iknots, df, order)

  rtn <- cpp_bsplines(x = x, iknots = iknots, bknots = bknots, order = order)
  attr(rtn, "call") <- match.call()
  attr(rtn, "environment") <- parent.frame()
  class(rtn) <- c("cpr_bs", "matrix")
  rtn
}

#' Print bsplines
#'
#' @param x a \code{cpr_bs} object.
#' @param n, number of rows of the B-spline basis matrix to display, defaults to
#' 6L.
#' @param \ldots not currently used.
#'
#' @return the object \code{x} is returned invisibly
#'
#' @method print cpr_bs
#' @export
print.cpr_bs <- function(x, n = 6L, ...) {
  cat("Basis matrix dims: [",
      paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "),
      "]\n", sep = "")
  cat("Order: ", attr(x, "order"), "\n", sep = "")
  cat("Number of internal knots: ", length(attr(x, "iknots")), "\n\n", sep = "")

  if (n < nrow(x)) {
    cat("First", n, "rows:\n\n")
  }

  if (nrow(x) > 0L) {
    print(x[seq(1, min(nrow(x), abs(n)), by = 1L), ])
  }

  invisible(x)
}
