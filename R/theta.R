#' Extract Regression Coefficients for B-Splines and Tensor Products of B-splines
#'
#' An S3 method for extracting the regression coefficients of the
#' \code{bsplines} and \code{btensor} terms.  By Default this uses
#' \code{stats::coef} to extract all the regression coefficients.  A specific
#' method for \code{lmerMod} objects has been provided.  If you are using a
#' regression method which \code{stats::coef} will not return the regression
#' coeficients, you'll need to define an S3 method for \code{stats::coef} to do
#' so.
#'
#' This function is implicitly called in the \code{cpr::cp} and
#' \code{cpr::cn} calls.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#'
#' @param fit a regression model fit
#'
#' @return the regression coeficients associated with terms with names
#' containing either "bsplines" or "btensor".
#'
#' @seealso \code{\link[stats]{coef}} \code{\link{cp}} \code{\link{cn}}

theta <- function(fit) { 
  UseMethod("theta")
}

theta.default <- function(fit) { 
  out <- stats::coef(fit)
  unname(out[grepl("bsplines|btensor", names(out))])
}

theta.lmerMod <- function(fit) { 
  out <- lme4::fixef(fit)
  unname(out[grepl("bsplines|btensor", names(out))])
}

BETA <- function(fit) {
  UseMethod("BETA") 
}

BETA.default <- function(fit) { 
  out <- stats::coef(fit)
  unname(out)
}

BETA.lmerMod <- function(fit) { 
  out <- lme4::fixef(fit)
  unname(out)
}

SIGMA <- function(fit) {
  UseMethod("SIGMA")
}

SIGMA.default <- function(fit) { 
  out <- stats::vcov(fit)
  unname(out)
}

SIGMA.lmerMod <- function(fit) { 
  out <- as.matrix(stats::vcov(fit))
  unname(out)
}
