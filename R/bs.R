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
bs <- function(x, ikntos, bknots, order = 4) { 
  cat('hi\n')
}

