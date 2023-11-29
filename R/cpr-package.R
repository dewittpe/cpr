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
#' @useDynLib cpr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom graphics plot
#'
#' @docType package
#' @name cpr-package
NULL
