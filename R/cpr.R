#' @title cpr: Control Polygon Reduction
#'
#' @description 
#' The cpr package implements the control polygon reduction method presented in
#' MY TO BE PUBLISHED paper(s).
#'
#' There are several options that the user can set while working with this
#' package that will help with the reporting and plotting of results.
#' 
#' \itemize{
#'    \item cpr_ggplot2: boolean, default is FALSE.  If set to TRUE the plotting
#'    functions will return a list of layers to be added to a \code{ggplot}
#'    object.  If FALSE, base R plots are returned.
#' }
#'
#' @useDynLib cpr
#' @importFrom Rcpp sourceCpp
#'
#' @docType package
#' @name cpr-package
NULL
