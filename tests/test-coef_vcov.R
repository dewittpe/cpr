# Tests for extracting coefficients and vcov matrix from regression fits
library(cpr)
require(lme4)

################################################################################
# There is one method of interest, and it is non exported.  There are several S3
# methods to check.
stopifnot(!grepl("coef_vcov", ls("package:cpr")))
cpr_namespace <- ls( getNamespace("cpr"), all.names = TRUE)
stopifnot(sum(cpr_namespace == "coef_vcov") == 1L)
stopifnot(sum(cpr_namespace == "coef_vcov.default") == 1L)
stopifnot(sum(cpr_namespace == "coef_vcov.lmerMod") == 1L)
stopifnot(sum(cpr_namespace == "coef_vcov_formater") == 1L) # doesn't need explicit testing, called by the S3 methods
stopifnot(sum(grepl("coef_vcov", cpr_namespace)) == 4L)

################################################################################
# Test that an error will be thrown if stats::coef and/or stats::vcov fail to
# return a vector and matrix
e <- new.env()
with(e, {
  fit <- list(coefficients = LETTERS)
  stopifnot(identical(coef(fit), LETTERS))
  x <- tryCatch(cpr:::coef_vcov(fit), error = function(e) e)
  stopifnot(inherits(x, "error"))
})

e <- new.env()
with(e, {
  fit <- list(coefficients = LETTERS, vcov = matrix(1:10))
  class(fit) <- c("cpr_testing_class", class(fit))
  vcov.cpr_testing_class <- function(x) { x$vcov }
  stopifnot(identical(coef(fit), LETTERS))
  stopifnot(identical(vcov(fit), matrix(1:10)))
  x <- tryCatch(cpr:::coef_vcov(fit), error = function(e) e)
  stopifnot(inherits(x, "error"))
})

e <- new.env()
with(e, {
  fit <- list(coefficients = 1:10, vcov = (1:10))
  class(fit) <- c("cpr_testing_class", class(fit))
  vcov.cpr_testing_class <- function(x) { x$vcov }
  stopifnot(identical(coef(fit), 1:10))
  stopifnot(identical(vcov(fit), (1:10)))
  x <- tryCatch(cpr:::coef_vcov(fit), error = function(e) e)
  stopifnot(inherits(x, "error"))
})

################################################################################
# lm with no cpr::bsplines
e <- new.env()
with(e, {
  fit <- lm(mpg ~ wt, data = mtcars)
  stopifnot(inherits(fit, "lm"))
  COEF_VCOV <- cpr:::coef_vcov(fit)

  stopifnot(identical(names(COEF_VCOV), c("theta", "coef", "vcov_theta", "vcov")))
  stopifnot(identical(COEF_VCOV$theta, numeric(0)))
  stopifnot(identical(COEF_VCOV$coef, coef(fit)))
  stopifnot(identical(COEF_VCOV$vcov_theta, matrix(0)[FALSE, FALSE]))
  stopifnot(identical(COEF_VCOV$vcov, vcov(fit)))
})

################################################################################
# lme4 with no cpr::bsplines
e <- new.env()
with(e, {
  fit <- lmer(mpg ~ wt | am, data = mtcars)
  stopifnot(inherits(fit, "lmerMod"))
  COEF_VCOV <- cpr:::coef_vcov(fit)

  stopifnot(identical(names(COEF_VCOV), c("theta", "coef", "vcov_theta", "vcov")))
  stopifnot(identical(COEF_VCOV$theta, numeric(0)))
  stopifnot(identical(COEF_VCOV$coef, fixef(fit)))
  stopifnot(identical(COEF_VCOV$vcov_theta, matrix(0)[FALSE, FALSE]))
  stopifnot(identical(COEF_VCOV$vcov, as.matrix(vcov(fit))))
})

################################################################################
# lm with cpr::bsplines
e <- new.env()
with(e, {
  fit <- lm(mpg ~ 0 + bsplines(wt, bknots = c(1.5, 5.5)) + hp, data = mtcars)
  stopifnot(inherits(fit, "lm"))
  COEF_VCOV <- cpr:::coef_vcov(fit)

  stopifnot(identical(names(COEF_VCOV), c("theta", "coef", "vcov_theta", "vcov")))
  stopifnot(identical(COEF_VCOV$theta, unname(coef(fit)[1:4])))
  stopifnot(identical(COEF_VCOV$coef, coef(fit)))
  stopifnot(identical(COEF_VCOV$vcov_theta, unname(vcov(fit)[1:4, 1:4])))
  stopifnot(identical(COEF_VCOV$vcov, vcov(fit)))
})

################################################################################
# lmer with cpr::bsplines
e <- new.env()
with(e, {
  fit <- lmer(mpg ~ 0 + bsplines(wt, bknots = c(1.5, 5.5)) + (1 | am), data = mtcars)
  stopifnot(inherits(fit, "lmerMod"))
  COEF_VCOV <- cpr:::coef_vcov(fit)

  fixef(fit)
  setNames(fit@beta, dimnames(fit@pp@.xData$X)[[2]])

  stopifnot(identical(names(COEF_VCOV), c("theta", "coef", "vcov_theta", "vcov")))
  stopifnot(identical(COEF_VCOV$theta, unname(fixef(fit))[1:4]))
  stopifnot(identical(COEF_VCOV$coef, fixef(fit)))
  stopifnot(identical(COEF_VCOV$vcov_theta, unname(as.matrix(vcov(fit))[1:4, 1:4])))
  stopifnot(identical(COEF_VCOV$vcov, as.matrix(vcov(fit))))
})


################################################################################
#                                 End of File                                  #
################################################################################
