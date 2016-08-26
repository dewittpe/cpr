#' B-Splines
#'
#' An implimentation of Carl de Boor's recursive algorimthm for building
#' B-splines.
#'
#' The difference between this function and \code{splines::bs} come in the
#' attributes associated with the output and default options.  The
#' \code{cpr::bsplines} call is intended to simplify the work needed with
#' respect to the control polygon reduction.  Further, the implimentation of
#' \code{cpr::bsplines} is in C++ and tends to be faster than
#' \code{splines::bs}.
#'
#'
#' @param x a numeric vector
#' @param iknots internal knots
#' @param df degrees of freedom: sum of the order and internal knots.  Ignored
#' if \code{iknots} is specified.
#' @param bknots boundary knot locations, defaults to \code{range(x)}.
#' @param order order of the piecewise polynomials, defualts to 4L.
#'
#' @examples
#' # build a vector of values to transform
#' xvec <- seq(-3, 5, length = 100)
#' 
#' # cubic b-spline
#' bmat <- bsplines(xvec, iknots = c(-2, 0, 0.2))
#' bmat
#' 
#' # view the splines
#' plot(bmat)
#' 
#' # If you want a second x-axis to show the x-values try the following:
#' second_x_axis <- round(stats::quantile(xvec, probs = seq(0, 1, by = .2)), 2)
#' 
#' plot(bmat) + 
#' ggplot2::annotate(geom = "text", x = second_x_axis, y = -0.02, label = second_x_axis) + 
#' ggplot2::annotate(geom = "linerange", x = second_x_axis, ymin = -0.05, ymax = -0.04) + 
#' ggplot2::coord_cartesian(ylim = c(0, 1))
#' 
#' # quadratic splines
#' bmat <- bsplines(xvec, iknots = c(-2, 0, 0.2), order = 3L)
#' bmat
#' plot(bmat) + ggplot2::ggtitle("Quadratic B-splines")
#' 
#' @export
bsplines <- function(x, iknots = NULL, df = NULL, bknots = range(x), order = 4L) { 

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

  rtn <- .Call('cpr_bbasis__impl', PACKAGE = 'cpr', x, iknots, bknots, order) 
  class(rtn) <- c("cpr_bs", "bs", "matrix")
  rtn
}

#' @export
#' @rdname bsplines
is.cpr_bs <- function(x) { 
  inherits(x, "cpr_bs")
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
#' @method plot cpr_bs
#' @export
#' @param x a \code{cpr_bs} object
#' @param digits number of digits to the right of the decimal place to report
#' for the value of each knot.
#' @param n number of values to use to plot the splines, defaults to 100
#' @param \ldots not currently used
plot.cpr_bs <- function(x, ..., digits = 2, n = 100) {
  xvec <- seq(min(attr(x, "bknots")), max(attr(x, "bknots")), length = n)
  bmat <- bsplines(xvec, iknots = attr(x, "iknots"), order = attr(x, "order"))
  .data <- tidyr::gather_(cbind(as.data.frame(bmat), "x" = xvec),
                          key_col = "spline", 
                          value_col = "value", 
                          gather_cols = paste0("V", seq(1, ncol(x), by = 1L)))

  .data <- dplyr::tbl_df(.data)

  xi <- attr(x, "xi")
  k  <- attr(x, "order")
  bk <- attr(x, "bknots")

  expr <- list(bquote(atop(group('{', xi[j], '}')[j == 1]^{.(k)}, .(formatC(bk[1], digits, format = "f")))))

  if (length(xi) > 2 * k) { 
    for(i in seq(k + 1, length(xi) - k, by = 1)) { 
      expr <- c(expr, bquote(atop(xi[.(i)], .(formatC(xi[i], digits, format = "f")))))
    }
  }

  expr <- c(expr, bquote(atop(group('{', xi[j], '}')[j == .(length(xi) - k + 1L)]^{.(length(xi))}, .(formatC(bk[2], digits, format = "f")))))

  ggplot2::ggplot(.data) + 
  ggplot2::theme_bw() + 
  ggplot2::aes_string(x = "x", y = "value", color = "spline") + 
  ggplot2::geom_line() + 
  ggplot2::theme(axis.title = ggplot2::element_blank()) +
  ggplot2::scale_x_continuous(breaks = c(min(attr(x, "bknots")), attr(x, "iknots"), max(attr(x, "bknots"))), labels = do.call(expression, expr))
}

#' B-spline Derivatives
#'
#' Generate the first and second derivatives of a B-spline Basis.
#'
#' @references 
#' C. de Boor, "A practical guide to splines. Revised Edition," Springer, 2001.
#' H. Prautzsch, W. Boehm, M. Paluszny, "Bezier and B-spline Techniques," Springer, 2002.
#'
#' @param x a numeric vector
#' @param iknots internal knots
#' @param df degrees of freedom: sum of the order and internal knots.  Ignored
#' if \code{iknots} is specified.
#' @param bknots boundary knot locations, defaults to \code{range(x)}.
#' @param order order of the piecewise polynomials, defualts to 4L.
#' @param derivative, (integer) first or second derivative
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
#' @rdname bsplinesD
bsplineD <- function(x, iknots = NULL, df = NULL, bknots = range(x), order = 4L, derivative = 1L) { 

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
