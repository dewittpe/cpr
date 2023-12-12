# Tests for extracting coefficients and vcov matrix from regression fits
#
# THESE ARE NON EXPORTED FUNCTIONS
library(cpr)

stopifnot(!grepl("coef_vcov", ls("package:cpr")))
cpr_namespace <- ls( getNamespace("cpr"), all.names = TRUE)
stopifnot(sum(cpr_namespace == "coef_vcov") == 1L)
stopifnot(sum(cpr_namespace == "coef_vcov.default") == 1L)
stopifnot(sum(cpr_namespace == "coef_vcov.lmerMod") == 1L)
stopifnot(sum(grepl("coef_vcov", cpr_namespace)) == 3L)


acp <- cp(mpg ~ bsplines(wt) + hp, data =mtcars, keep_fit = TRUE)
str(acp)



fit0 <- lm(mpg ~ wt, data = mtcars)
fit1 <- lme4::lmer(mpg ~ wt | am, data = mtcars)

coef(fit0) |> str()
coef(fit0) |> class()

coef(fit1) |> str()
coef(fit1) |> class()

vcov(fit0) |> str()
vcov(fit0) |> class()

vcov(fit1) |> str()
vcov(fit1) |> class()

coef_vcov(fit1)

fit1@beta
lme4::fixef(fit1)

stats::vcov(fit1)

################################################################################
#                                 End of File                                  #
################################################################################
