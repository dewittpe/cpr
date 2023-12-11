#' @title cpr: Control Polygon Reduction
#'
#' @description
#' The cpr package implements the control polygon reduction and control net
#' reduction methods for finding parsimonious B-spline regression models as
#' described in DeWitt (2017).
#'
#' @references
#' DeWitt, Peter Edward. \dQuote{Parsimonious B-spline regression models via
#' control polygon and control net reduction for identifying factors explaining
#' variation in daily hormone profiles during the menopausal transition.}
#' (2017).
#'
#' @docType package
#' @keywords internal
#' @aliases cpr-package
"_PACKAGE"

## usethis namespace: start
#' @useDynLib cpr
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
