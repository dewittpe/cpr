# Tests for extracting coefficients and vcov matrix from regression fits
library(cpr)
require(lme4)

################################################################################
# There is one method of interest, and it is non exported.  There are several S3
# methods to check.
stopifnot(!grepl("loglikelihood", ls("package:cpr")))
cpr_namespace <- ls( getNamespace("cpr"), all.names = TRUE)
stopifnot(sum(cpr_namespace == "loglikelihood") == 1L)
stopifnot(sum(cpr_namespace == "loglikelihood.default") == 1L)
stopifnot(sum(cpr_namespace == "loglikelihood.geeglm") == 1L)
stopifnot(sum(grepl("^loglikelihood", cpr_namespace)) == 3L)

################################################################################
e <- new.env()
with(e, {
  fit <- lm(mpg ~ wt, data = mtcars)
  stopifnot("recover loglikelihood from lm" =
            isTRUE(all.equal(cpr:::loglikelihood(fit), as.numeric(stats::logLik(fit)))))
})

################################################################################
e <- new.env()
with(e, {
  fit <- glm(I(mpg < 20) ~ wt, data = mtcars, family = quasibinomial())
  stopifnot("recover quasiloglikelihood from glm" =
            isTRUE(all.equal(cpr:::loglikelihood(fit), as.numeric(stats::logLik(fit)))))
})

################################################################################
e <- new.env()
with(e, {
  require(lme4)
  fit <- lmer(mpg ~ wt + (1|cyl), data = mtcars)
  stopifnot("recover loglikelihood from lme4" =
            isTRUE(all.equal(cpr:::loglikelihood(fit), as.numeric(stats::logLik(fit)))))
})

################################################################################
e <- new.env()
with(e, {
  require(geepack)
  fit <- geeglm(mpg ~ wt, id = cyl, data = mtcars, family = gaussian())
  y <- fit$y
  mu <- fit$fitted.values
  stopifnot("recover loglikelihood from gaussian geeglm" =
            isTRUE(all.equal(cpr:::loglikelihood(fit), -sum((y - mu)^2 / 2))))
})

################################################################################
e <- new.env()
with(e, {
  require(geepack)
  fit <- geeglm(I(mpg < 20) ~ wt, id = cyl, data = mtcars, family = binomial())
  y <- fit$y
  mu <- fit$fitted.values
  stopifnot("recover loglikelihood from binomial geeglm" =
            isTRUE(all.equal(cpr:::loglikelihood(fit),
                             sum(y * log(mu/(1 - mu)) + log(1 - mu)))))
})

################################################################################
e <- new.env()
with(e, {
  require(geepack)
  fit <- geeglm(as.integer(mpg) ~ wt, id = cyl, data = mtcars, family = poisson())
  y <- fit$y
  mu <- fit$fitted.values
  stopifnot("recover loglikelihood from poisson geeglm" =
            isTRUE(all.equal(cpr:::loglikelihood(fit),
                             sum(y * log(mu) - mu))))
})

################################################################################
e <- new.env()
with(e, {
  require(geepack)
  fit <- geeglm(mpg ~ wt, id = cyl, data = mtcars, family = Gamma())
  y <- fit$y
  mu <- fit$fitted.values
  stopifnot("recover loglikelihood from Gamma geeglm" =
            isTRUE(all.equal(cpr:::loglikelihood(fit), -sum(y/mu + log(mu)))))
})

################################################################################
#                                 End of File                                  #
################################################################################
