#' Determine the (quasi) Log Likelihood for a regression object.
#'
#' Return, via \code{\link[stats]{logLik}} or a custom S3 method, the (quasi)
#' log likelihood of a regression object.
#'
#' This function is used by \code{cpr} and \code{cnr} to determine the
#' (quasi) log likelihood returned in the \code{cpr_cpr} and \code{cpr_cnr}
#' objects.
#'
#' Generally this function defaults to \code{\link[stats]{logLik}}.  Therefore, if an S3
#' method for determining the (quasi) log likelihood exists in the workspace
#' everything should work.  If an S3 method does not exist you should define
#' one.
#'
#' See \code{methods(loglikelihood)} for a list of the provided methods.  The
#' default method uses \code{\link[stats]{logLik}}.
#'
#' @param x a regression fit object
#' @param ... passed through to \code{\link[stats]{logLik}}
#'
#' @return the numeric value of the (quasi) log likelihood.
#'
#' @seealso \code{\link{cpr}} \code{\link{cnr}} \code{\link[stats]{logLik}}
#'
#' @examples
#'
#' fit <- lm(mpg ~ wt, data = mtcars)
#' stats::logLik(fit)
#' loglikelihood(fit)
#'
loglikelihood <- function(x, ...) {
  UseMethod("loglikelihood")
}

#' @export
loglikelihood.default <- function(x, ...) {
  as.numeric(stats::logLik(x, ...))
}

#' @export
loglikelihood.geeglm <- function(x, ...) {
  y <- x$y
  mu <- x$fitted.values

  switch(stats::family(x)$family,
         gaussian         = -sum((y - mu)^2)/2,
         binomial         =  sum(y * log(mu/(1 - mu)) + log(1 - mu)),
         poisson          =  sum(y * log(mu) - mu),
         Gamma            = -sum(y/mu + log(mu)),
         inverse.gaussian =  sum(-y/(2 * mu^2) + 1/mu),
         stop(simpleError(gettextf("tool for calculating loglikelihood for geeglms with family '%s' not yet defined.",
                                   stats::family(x)$family)))
         )
}
