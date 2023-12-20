#' Internal Knots or Degrees of Freedom
#'
#' Check order, degrees of freedom (df) and iknots
#'
#' This is an internal function, not to be exported, and used in the calls for
#' \code{\link{bsplines}} and \code{\link{bsplineD}}.
#'
#' @param x
#' @param iknots
#' @param df
#' @param order
#'
#' @return a numeric vector to use as the internal knots defining a B-spline
#'
#' @seealso \code{\link{bsplines}}, \code{\link{bsplineD}}
#'
#' @examples
#'
#'
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