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
#' @author Peter DeWitt \email{dewittpe@gmail.com}
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
#' @example examples/bsplines.R
#'
#' @export
bsplines <- function(x, iknots = NULL, df = NULL, bknots = range(x), order = 4L) {

  if (is.list(x)) {
    stop("x is a list.  use cpr::btensor instead of cpr::bsplines.")
  }

  iknots <- iknots_or_df(x, iknots, df, order)

  rtn <- .Call('_cpr_bbasis__impl', PACKAGE = 'cpr', x, iknots, bknots, order)
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
print.cpr_bs <- function(x, n = 6L, ...) {
  cat("Matrix dims: [", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "), "]\n\n", sep = "")
  print(x[seq(1, min(nrow(x), abs(n)), by = 1L), ])
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
#' @method plot cpr_bs
#' @export
plot.cpr_bs <- function(x, ..., show_xi = TRUE, show_x = FALSE, color = TRUE, digits = 2, n = 100) {
  xvec <- seq(min(attr(x, "bknots")), max(attr(x, "bknots")), length = n)
  bmat <- bsplines(xvec, iknots = attr(x, "iknots"), order = attr(x, "order"))
  plot_data <- do.call(tidyr::gather,
                       list(data = cbind(as.data.frame(bmat), "x" = xvec),
                            key = "spline", value = "value",
                            paste0("V", seq(1, ncol(x), by = 1L))))

  plot_data$spline <- factor(as.numeric(gsub("V(\\d+)", "\\1", plot_data$spline)))
  plot_data <- dplyr::tbl_df(plot_data)

  g <-
    ggplot2::ggplot(plot_data) +
    ggplot2::theme_bw() +
    ggplot2::aes_string(x = "x", y = "value") +
    ggplot2::geom_line() +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (color) {
    g <- g + ggplot2::aes_string(color = "spline")
  } else {
    g <- g + ggplot2::aes_string(group = "spline")
  }

  if (show_xi | show_x) {
    e <- knot_expr(x, digits)

    if (show_xi & !show_x) {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = e$xi_expr, minor_breaks = NULL)
    } else if (!show_xi & show_x) {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = e$num_expr, minor_breaks = NULL)
    } else {
      g <- g + ggplot2::scale_x_continuous(breaks = e$breaks,
                                           labels = e$xi_expr,
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
#'   dplyr::data_frame(x = xvec,
#'                     Spline = as.numeric(bmat %*% theta),
#'                     "First Derivative" = as.numeric(bmat1 %*% theta),
#'                     "Second Derivative" = as.numeric(bmat2 %*% theta))
#' plot_data <- tidyr::gather(plot_data, key = key, value = value, -x)
#'
#' ggplot2::ggplot(plot_data) +
#' ggplot2::aes(x = x, y = value, color = key) +
#' ggplot2::geom_line() +
#' ggplot2::geom_hline(yintercept = 0) +
#' ggplot2::geom_vline(xintercept = iknots, linetype = 3)
#'
#' @export
#' @rdname bsplineD
bsplineD <- function(x, iknots = NULL, df = NULL, bknots = range(x), order = 4L, derivative = 1L) {

  iknots <- iknots_or_df(x, iknots, df, order)

  xi <- c(rep(min(bknots), order), iknots, rep(max(bknots), order))


  if (derivative == 1L) {
    rtn <- mapply(bsplineD1__impl,
                  j = seq(0L, length(iknots) + order - 1L, by = 1L),
                  MoreArgs = list(x = x, order = order, knots = xi),
                  SIMPLIFY = FALSE)
  } else if (derivative == 2L) {
    rtn <- mapply(bsplineD2__impl,
                  j = seq(0L, length(iknots) + order - 1L, by = 1L),
                  MoreArgs = list(x = x, order = order, knots = xi),
                  SIMPLIFY = FALSE)
  } else {
    stop("Only first and second derivatives are supported")
  }

  do.call(cbind, rtn)
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
