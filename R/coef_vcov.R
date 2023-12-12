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

# IMPORTANT NOTE: for the S3 methods to work they need to be registerd.  The
# generic does not need to be exported, but the methods do.
# https://github.com/r-lib/devtools/issues/2293#issuecomment-721357042

#' @export
coef_vcov.default <- function(fit) {
  COEF <- tryCatch(stats::coef(fit), warning = function(w) w, error = function(e) e)
  VCOV <- tryCatch(stats::vcov(fit), warning = function(w) w, error = function(e) e)

  assign("COEF", COEF, .GlobalEnv)
  assign("VCOV", VCOV, .GlobalEnv)


  if (inherits(COEF, "error")) {
    stop(sprintf("Attemped to extract regression coefficients via stats::coef for an object of class %s.  This has failed.", paste(class(fit), colapse = ",")))
  }
  if (inherits(VCOV, "error")) {
    stop(sprintf("Attemped to extract variance-covariance matrix via stats::vcov for an object of class %s.  This has failed.", paste(class(fit), colapse = ",")))
  }

  if (inherits(VCOV, "warning")) {
    if (VCOV$message == "essentially perfect fit: summary may be unreliable") {
      VCOV <- matrix(0)[FALSE, FALSE]
    }
  }

  if (!inherits(COEF, "numeric")) {
    stop(sprintf("Attemped to extract regression coefficients via stats::coef for an object of class %s.  This has failed - expected numeric vector, got %s."
                 , paste(class(fit), colapse = ",")
                 , paste(class(COEF), colapse = ",")
                 ))
  }
  if (!inherits(VCOV, "matrix")) {
    stop(sprintf("Attemped to extract variance-covariance matrix via stats::vcov for an object of class %s.  This has failed - expected numeric matrix, got %s."
                 , paste(class(fit), colapse = ",")
                 , paste(class(VCOV), colapse = ",")
                 ))
  }

  coef_vcov_formater(COEF, VCOV)

}

#' @export
coef_vcov.lmerMod <- function(fit) {
  #COEF <- lme4::fixef(fit)
  COEF <- stats::setNames(fit@beta, dimnames(fit@pp@.xData$X)[[2]])
  VCOV <- as.matrix(stats::vcov(fit))

  coef_vcov_formater(COEF, VCOV)
}

coef_vcov_formater <- function(COEF, VCOV) {
  theta_idx <- grepl("bsplines|btensor", names(COEF))

  list(
         theta = unname(COEF[theta_idx])
       , coef  = COEF
       , vcov_theta = if (nrow(VCOV) > 0) {unname(VCOV[theta_idx, theta_idx])} else {unname(VCOV)}
       , vcov = VCOV
  )
}
