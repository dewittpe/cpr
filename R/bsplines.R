#' B-Splines
#'
#' An implementation of Carl de Boor's recursive algorithm for building
#' B-splines.
#'
#' The difference between this function and \code{splines::bs} come in the
#' attributes associated with the output and default options.  The
#' \code{cpr::bsplines} call is intended to simplify the work needed with
#' respect to the control polygon reduction.  Further, the implementation of
#' \code{cpr::bsplines} is in C++ and tends to be faster than
#' \code{splines::bs}.
#'
#' See the \code{vignette("bsplines", package = "cpr")} for a detailed
#' comparison between the \code{bsplines} and \code{\link[splines]{bs}} calls
#' and notes about B-splines in general.
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
#' @examples
#' # build a vector of values to transform
#' xvec <- seq(-3, 5, length = 100)
#'
#' # cubic b-spline
#' bmat <- bsplines(xvec, iknots = c(-2, 0, 1.2, 1.2, 3.0))
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
    stop("x is a list.  use cpr::btensor instead of cpr::bsplines.")
  }

  stopifnot(length(bknots) == 2L)

  iknots <- iknots_or_df(x, iknots, df, order)

  rtn <- cpp_bsplines(x = x, iknots = iknots, bknots = bknots, order = order)
  attr(rtn, "call") <- match.call()
  attr(rtn, "environment") <- parent.frame()
  class(rtn) <- c("cpr_bs", "matrix")
  rtn
}

#' Print bsplines
#'
#' @method print cpr_bs
#' @export
#' @param x a \code{cpr_bs} object.
#' @param n, number of rows of the B-spline basis matrix to display, defaults to
#' 6L.
#' @param \ldots not currently used.
#'
#' @return the object \code{x} is returned invisibly
print.cpr_bs <- function(x, n = 6L, ...) {
  cat("Basis matrix dims: [",
      paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "),
      "]\n", sep = "")
  cat("Order: ", attr(x, "order"), "\n", sep = "")
  cat("Number of internal knots: ", length(attr(x, "iknots")), "\n\n", sep = "")

  if (n < nrow(x)) {
    cat("First", n, "rows:\n\n")
  }

  print(x[seq(1, min(nrow(x), abs(n)), by = 1L), ])

  invisible(x)
}

#' Plot B-splines
#'
#' @param x a \code{cpr_bs} object
#' @param show_xi logical, show the knot locations, using the Greek letter xi, on the x-axis
#' @param show_x  logical, show the x values of the knots on the x-axis
#' @param color logical, if \code{TRUE} (default) the splines are plotted in
#' color.  If \code{FALSE} all splines are black lines.
#' @param digits number of digits to the right of the decimal place to report
#' for the value of each knot.
#' @param n number of values to use to plot the splines, defaults to 100
#' @param \ldots not currently used
#'
#' @seealso \code{\link{bsplines}}
#'
#' @examples
#' bmat <- bsplines(seq(-3, 2, length = 1000), iknots = c(-2, 0, 0.2))
#' plot(bmat, show_xi = TRUE,  show_x = TRUE)
#' plot(bmat, show_xi = FALSE, show_x = TRUE)
#' plot(bmat, show_xi = TRUE,  show_x = FALSE)  ## Default
#' plot(bmat, show_xi = FALSE, show_x = FALSE)
#' plot(bmat, show_xi = FALSE, show_x = FALSE)
#' plot(bmat, show_xi = FALSE, show_x = FALSE, color = FALSE)
#' @method plot cpr_bs
#' @export
plot.cpr_bs <- function(x, ..., show_xi = TRUE, show_x = FALSE, color = TRUE, digits = 2, n = 100) {
  xvec <- seq(min(attr(x, "bknots")), max(attr(x, "bknots")), length = n)
  bmat <- bsplines(xvec, iknots = attr(x, "iknots"), order = attr(x, "order"))

  # reshape from wide to long and from matrix to data.frame
  plot_data <- utils::stack(as.data.frame(bmat))
  names(plot_data) <- c("value", "spline")
  plot_data <- cbind(plot_data, data.frame(x = rep(xvec, times = ncol(bmat))))
  levels(plot_data$spline) <- sub("V", "B", levels(plot_data$spline))
  levels(plot_data$spline) <- sub("(\\d+)",
                                  paste0("[list(\\1,k==", attr(x, "order"), ",bold(xi))](x)"),
                                        levels(plot_data$spline))

  g <-
    ggplot2::ggplot(plot_data) +
    ggplot2::theme_bw() +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("x"), Y = as.name("value")))) +
    ggplot2::geom_line() +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (color) {
    g <- g + eval(substitute(ggplot2::aes(color = GRP), list(GRP = as.name("spline"))))
    g <- g + ggplot2::scale_color_discrete(labels = scales::parse_format())
  } else {
    g <- g + eval(substitute(ggplot2::aes(group = GRP), list(GRP = as.name("spline"))))
  }

  if (show_xi | show_x) {
    e <- knot_expr(x, digits)

    if (show_xi & !show_x) {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = parse(text = e$xi_expr),
                                           minor_breaks = NULL)
    } else if (!show_xi & show_x) {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = e$num_expr,
                                           minor_breaks = NULL)
    } else {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = parse(text = e$xi_expr),
                                           minor_breaks = NULL,
                                           sec.axis = ggplot2::sec_axis(~ .,
                                                                        breaks = e$breaks,
                                                                        labels = e$num_expr))
    }

  }
  g
}


#' B-spline Derivatives
#'
#' Generate the first and second derivatives of a B-spline Basis.
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
#' @param derivative, (integer) first or second derivative
#'
#' @seealso \code{\link{bsplines}}
#'
#' @examples
#'
#' set.seed(42)
#'
#' xvec <- seq(0.1, 9.9, length = 1000)
#' iknots <- sort(runif(rpois(1, 3), 1, 9))
#' bknots <- c(0, 10)
#'
#' # basis matrix and the first and second derivatives thereof, for cubic (order =
#' # 4) b-splines
#' bmat  <- bsplines(xvec, iknots, bknots = bknots)
#' bmat1 <- bsplineD(xvec, iknots, bknots = bknots, derivative = 1)
#' bmat2 <- bsplineD(xvec, iknots, bknots = bknots, derivative = 2)
#'
#' # control polygon ordinates
#' theta <- runif(length(iknots) + 4L, -5, 5)
#'
#' # plot data
#' plot_data <-
#'   data.frame(Spline            = as.numeric(bmat %*% theta),
#'              First_Derivative  = as.numeric(bmat1 %*% theta),
#'              Second_Derivative = as.numeric(bmat2 %*% theta))
#' plot_data <- stack(plot_data)
#' plot_data <- cbind(plot_data, data.frame(x = xvec))
#'
#' ggplot2::ggplot(plot_data) +
#' ggplot2::aes(x = x, y = values, color = ind) +
#' ggplot2::geom_line() +
#' ggplot2::geom_hline(yintercept = 0) +
#' ggplot2::geom_vline(xintercept = iknots, linetype = 3)
#'
#' @export
#' @rdname bsplineD
bsplineD <- function(x, iknots = NULL, df = NULL, bknots = range(x), order = 4L, derivative = 1L) {

  iknots <- iknots_or_df(x, iknots, df, order)

  if (derivative == 1L) {
    rtn <- cpp_bsplinesD1(x = x, iknots = iknots, bknots = bknots, order = order)
  } else if (derivative == 2L) {
    rtn <- cpp_bsplinesD2(x = x, iknots = iknots, bknots = bknots, order = order)
  } else {
    stop("Only first and second derivatives are supported")
  }
  rtn
}

iknots_or_df <- function(x, iknots, df, order) {
  if (is.null(iknots) & is.null(df)) {
    iknots <- numeric(0)
  } else if (is.null(iknots) & !is.null(df)) {
    if (df < order) {
      warning("df being set to order")
      iknots <- numeric(0)
    } else if (df == order) {
      iknots <- numeric(0)
    } else {
      iknots <- trimmed_quantile(x, probs = seq(1, df - order, by = 1) / (df - order + 1))
    }
  } else if (!is.null(iknots) & !is.null(df)) {
    warning("Both iknots and df defined, using iknots")
  }
  iknots
}
