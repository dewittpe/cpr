#' Get Spline Function
#'
#' Generate a \code{data.frame} for interpolating and plotting a spline
#' function, given a \code{cpr_cp} or \code{cpr_cn} object.
#'
#' @param x a \code{cpr_cp} or \code{cpr_cn} object.
#' @param n the length of sequence to use for interpolating the spline function.
#' @param margin an integer vector or length 1 or 2.  Only used when working
#' x is a \code{cpr_cn} object.  #' 
#' @param at point value for marginals not defined in the \code{margin}.  Only
#' used when \code{x} is a \code{cpr_cn} object.
#'
#' @export
get_spline <- function(x, margin, at, n = 100) {
  UseMethod("get_spline")
}

#' @export
get_spline.cpr_cp <- function(x, margin, at, n = 100) {
  xvec <- seq(min(x$bknots), max(x$bknots), length = n)
  bmat <- bsplines(xvec, iknots = x$iknots, bknots = x$bknots, order = x$order)
  out  <- dplyr::data_frame(x = xvec) 
  tibble::add_column(out, y = as.numeric(bmat %*% x[["cp"]][["theta"]]))
}

#' @export
get_spline.cpr_cn <- function(x, margin, at, n = 100) {
  stop("get_spline.cpr_cn not yet implimented.")
}
