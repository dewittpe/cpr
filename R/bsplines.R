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
#' @param x data
#' @param iknots internal knots
#' @param bknots boundary knot locations
#' @param order order of the piecewise polynomials
#'
#' @export
bsplines <- function(x, iknots = numeric(0), bknots = range(x), order = 4) { 
  B <- .Call('cpr_bsplines_impl', PACKAGE = 'cpr', x, iknots, bknots, order) 
  out <- B$Bmat
  attr(out, "order")   <- B$order
  attr(out, "iknots")  <- B$iknots
  attr(out, "bknots")  <- B$bknots
  attr(out, "xi")      <- B$xi
  attr(out, "xi_star") <- B$xi_star
  attr(out, "x")       <- x
  attr(out, "class")   <- c("cpr_bs", "bs", "basis", "matrix")
  out
}

#' @export
#' @rdname bsplines
print.cpr_bs <- function(x, n = 6, ...) { 
  cat("Matrix dims: [", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "), "]\n\n", sep = "")
  print(x[seq(1, min(nrow(x), n), by = 1L), ])
}

#' @export
#' @rdname bsplines
plot.cpr_bs <- function(x, y, ggplot2 = getOption("cpr_ggplot2", FALSE), ...) {
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
    matplot(x, y, ...)
  }
}


