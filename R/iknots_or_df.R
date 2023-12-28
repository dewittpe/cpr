#' Internal Knots or Degrees of Freedom
#'
#' Check order, degrees of freedom (df) and iknots
#'
#' This is an internal function, not to be exported, and used in the calls for
#' \code{\link{bsplines}} and \code{\link{bsplineD}}.
#'
#' Use \code{iknots} preferentially.  If iknots are not provided then return the
#' \code{\link{trimmed_quantile}} for the appropriate \code{df} and \code{order}
#'
#' @param x the support - a numeric vector
#' @param iknots internal knots - a numeric vector
#' @param df degrees of freedom - a numeric value of length 1
#' @param order polynomial order
#'
#' @return a numeric vector to use as the internal knots defining a B-spline.
#'
#' @seealso \code{\link{bsplines}}, \code{\link{bsplineD}},
#' \code{\link{trimmed_quantile}}
#'
#' @examples
#'
#' xvec <- runif(600, min = 0, max = 3)
#'
#' # return the iknots
#' cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = NULL, order = NULL)
#'
#' # return the iknots even when the df and order are provided
#' cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = 56, order = 12)
#'
#' # return numeric(0) when df <= order (df < order will also give a warning)
#' cpr:::iknots_or_df(x = xvec, iknots = NULL, df = 6, order = 6)
#'
#' # return trimmed_quantile when df > order
#' # probs = (df - order) / (df - order + 1)
#' cpr:::iknots_or_df(x = xvec, iknots = NULL, df = 10, order = 4)
#' cpr::trimmed_quantile(xvec, probs = 1:6 / 7)
#'
iknots_or_df <- function(x, iknots, df, order) {
  if (is.null(iknots) & is.null(df)) {
    iknots <- numeric(0)
  } else if (is.null(iknots) & !is.null(df)) {
    if (df < order) {
      iknots <- numeric(0)
      warning("df being set to order")
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
