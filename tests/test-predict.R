################################################################################
# testing the prediction method

library(cpr)
set.seed(42)

################################################################################
#                         simple lm with just a spline                         #

acp <- cp(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53)), data = spdg)
alm <- lm(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53)) + 0, data = spdg)

stopifnot("same lm coef" = identical(acp$coef, alm$coef))
identical(acp$coef, alm$coef)
# [1] TRUE
nd <- data.frame(pdg = NA_real_,
                 age = runif(n = 123, min = 45, max = 53))

acp_pred <- predict(acp, newdata = nd)
alm_pred <- predict(alm, newdata = nd, se.fit = TRUE)

stopifnot(isTRUE( all.equal(acp_pred$pre, alm_pred$fit, check.attributes = FALSE)))



################################################################################
#                                 End of File                                  #
################################################################################
