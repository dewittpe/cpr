#' Extract Regression Coefficients for B-Splines and Tensor Products of B-splines
#'
#' An S3 method for extracting the regression coefficients of the
#' \code{bsplines} and \code{btensor} terms.  By Default this uses
#' \code{stats::coef} to extract all the regression coefficients.  A specific
#' method for \code{lmerMod} objects has been provided.  If you are using a
#' regression method which \code{stats::coef} will not return the regression
#' coefficients, you'll need to define an S3 method for \code{stats::coef} to do
#' so.
#'
#' These functions are called in the \code{\link{cp}} and
#' \code{\link{cn}} calls.
#'
#' @param fit a regression model fit
#'
#' @return A list with four elements
#' \describe{
#'   \item{theta}{theta regression coefficients}
#'   \item{coef}{all regression cofficients}
#'   \item{vcov_theta}{subsection of variance-covariance matrix pertaining to the theta values}
#'   \item{vcov}{full variance-covariance matrix}
#' }
#'
#' @seealso \code{\link[stats]{coef}} \code{\link{cp}} \code{\link{cn}}

coef_vcov <- function(fit) {
  UseMethod("coef_vcov")
}

coef_vcov.default <- function(fit) {
  COEF <- tryCatch(stats::coef(fit), warning = function(w) w, error = function(e) e)
  VCOV <- tryCatch(stats::vcov(fit), warning = function(w) w, error = function(e) e)

  if (inherits(COEF, "error") | inherits(VCOV, "error") | !inherits(COEF, "numeric") | !inherits(VCOV, "matrix")) {
    stop("Attemped to extract regression coefficients and variance-covariance matrix via stats::coef and stats::vcov respectivly.  This has failed.  Define S3 methods for the regression tool you are using.")
  }

  coef_vcov_formater(COEF, VCOV)

}

coef_vcov.lmerMod <- function(fit) {
  #COEF <- lme4::fixef(fit)
  COEF <- fit@beta
  VCOV <- as.matrix(stats::vcov(fit))

  coef_vcov_formater(COEF, VCOV)
}

coef_vcov_formater <- function(COEF, VCOV) {
  theta_idx <- grepl("bsplines|btensor", names(COEF))
  list(
         theta = unname(COEF[theta_idx])
       , coef  = COEF
       , vcov_theta = unname(VCOV[theta_idx, theta_idx])
       , vcov = VCOV
  )
}
