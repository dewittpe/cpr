library(cpr)

# testing of the newknots method

################################################################################
# check that the method is not exported
stopifnot(!grepl("newknots", ls("package:cpr")))
cpr_namespace <- ls( getNamespace("cpr"), all.names = TRUE)
stopifnot(sum(cpr_namespace == "newknots") == 1L)
stopifnot(sum(grepl("^newknots$", cpr_namespace)) == 1L)

################################################################################

cp0 <- cp(log(pdg) ~ bsplines(day, iknots = c(-.25, 0, 0.25), bknots = c(-1, 1)), data = spdg)

expected <- log(pdg) ~ bsplines(day, iknots = c(-0.85, 0, 0.25, 0.3), bknots = c(-1, 1))

test <- cpr:::newknots(cp0$call$formula, c(-0.85, 0, 0.25, 0.3))

stopifnot(isTRUE( all.equal(expected, test)))

################################################################################
#                                 End of File                                  #
################################################################################
