#' B-Splines
#'
#' An implimentation of Carl de Boor's recursive algorimthm for building
#' B-splines.
#'
#' The difference between this function and \code{splines::bs} come in the
#' attributes associated with the output and default options.  The
#' \code{cpr::bs} call is intended to simplify the work needed with respect to
#' the control polygon reduction.
#'
#' \code{greville_sites} returns the knot averages.
#'
#' @param x data
#' @param iknots internal knots
#' @param bknots boundary knot locations, defaults to \code{range(x)}.
#' @param order order of the piecewise polynomials
#'
#' @export
bsplines <- function(x, iknots = numeric(0), bknots = range(x), order = 4L) { 
  B <- .Call('cpr_bsplines__impl', PACKAGE = 'cpr', x, iknots, bknots, order) 
  out <- B$Bmat
  attr(out, "order")   <- B$order
  attr(out, "iknots")  <- B$iknots
  attr(out, "bknots")  <- B$bknots
  attr(out, "xi")      <- B$xi
  attr(out, "xi_star") <- B$xi_star
  # attr(out, "x")       <- x
  attr(out, "call")    <- match.call()
  attr(out, "class")   <- c("cpr_bs", "bs", "basis", "matrix")
  out
}

#' @method print cpr_bs
#' @export
#' @rdname bsplines
#' @param n, number of rows of the B-spline basis matrix to display, defaults to
#' 6L.
print.cpr_bs <- function(x, n = 6L, ...) { 
  print(attr(x, "call"))
  cat("Matrix dims: [", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "), "]\n\n", sep = "")
  print(x[seq(1, min(nrow(x), n), by = 1L), ])
}

#' @method plot cpr_bs
#' @export
#' @rdname bsplines
#' @param ggplot2 boolean, if TRUE, return a list of layers to plot with
#' ggplot(), if false, pass x and ... to matplot
#' @param ... passed to \code{graphics::matplot}
plot.cpr_bs <- function(x, ggplot2 = getOption("cpr_ggplot2", FALSE), ...) {
  if (ggplot2) { 
    list("basis" = ggplot2::geom_line(mapping = ggplot2::aes_string(x = "x", y = "value", color = "key"),
                            data    = tidyr::gather_(cbind(as.data.frame(x), "x" = attr(x, "x")), 
                                                     key_col = "key", 
                                                     value_col = "value", 
                                                     gather_cols = paste0("V", seq(1, ncol(x), by = 1L))
                                                     )), 
         "knots" = ggplot2::geom_vline(data = data.frame(xi = attr(x, "xi")), 
                                       mapping = ggplot2::aes_string(x = "1", y = "1", xintercept = "xi")))
  } else {
    graphics::matplot(x, ...)
  }
}

#' @export
#' @param xi full knot vector, 
#' @rdname bsplines
greville_sites <- function(xi, order = 4L) {
    .Call('cpr_greville_sites__impl', PACKAGE = 'cpr', xi, order)
}
