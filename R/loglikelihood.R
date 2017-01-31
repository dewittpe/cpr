#' Determine the (quasi) Log Likelihood for a regression object.
#'
#' Return, via stats::logLik or a custom S3 method, the (quasi) log likelihood
#' of a regression object.
#'
#' This function is used by \code{cpr::cpr} and \code{cpr::cnr} to determine the
#' (quasi) log likelihood returned in the \code{cpr_cpr} and \code{cpr_cnr}
#' objects.  
#'
#' Generally this function defaults to stats::logLik.  Therefore, if an S3
#' method for determining the (quasi) log likelihood exists in the workspace
#' everything should work.  If an S3 method does not exist you should define
#' one.
#'
#' See \code{methods(loglikelihood)} for a list of the provided methods.  The
#' default method uses \code{stats::logLik}.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x a regression fit object
#' @param ... passed through to \code{stats::logLik}
#'
#' @return the numeric value of the (quasi) log likelihood.
#'
#' @seealso \code{\link{cpr}} \code{\link{cnr}} \code{\link[stats]{logLik}}
#'
#' @export
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
         stop(simpleError(gettextf("do not know how to calculate quasi-likelihood for geeglms with family '%s'", 
                                   stats::family(x)$family)))
         )
}

# library(lme4)
# library(geepack)
# 
# fit <- lm(mpg ~ wt, data = mtcars)
# loglikelihood(fit)
# 
# fit <- glm(mpg ~ wt, data = mtcars)
# loglikelihood(fit)
# 
# fit <- lmer(mpg ~ wt + (1|cyl), data = mtcars)
# loglikelihood(fit)
# 
# fit <- geeglm(mpg ~ wt, id = cyl, data = mtcars)
# loglikelihood(fit)
# 
