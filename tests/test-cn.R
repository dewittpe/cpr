library(cpr)

################################################################################
# simple test of the cn.cpt_bt and cn.formula, should have the same elements
e <- new.env()
with(e, {
  bt <- btensor(list(spdg$day, spdg$age) , df = list(30, 4) , bknots = list(c(-1, 1), c(44, 53)))
  theta <- rnorm(30 * 4)
  acn <- cn(bt, theta)

  bcn <- cn(pdg ~ btensor(list(day, age)
                          , df = list(30, 4)
                          , bknots = list(c(-1, 1), c(44, 53))
                          ) + ttm
            , data = spdg)

  stopifnot(inherits(acn, "cpr_cn"))
  stopifnot(inherits(bcn, "cpr_cn"))

  stopifnot(identical(names(acn), names(bcn)))

  stopifnot(identical(names(acn),
                      c("cn", "bspline_list", "call", "keep_fit", "fit", "theta", "coefficients", "vcov", "vcov_theta", "loglik", "rss", "rse")))

})

################################################################################
# Verify that an error is thrown if btensor is not used as expected in the
# formula
e <- new.env()
with(e, {
  test <- tryCatch(cn(log10(pdg) ~ age + ttm, data = spdg), error = function(e) e)
  stopifnot(inherits(test, "error"))
  stopifnot(identical(test$message, "btensor() must appear once, with no effect modifiers, on the right hand side of the formula."))
})

e <- new.env()
with(e, {
  test <- tryCatch(cn(log10(pdg) ~ btensor(ttm)*age, data = spdg), error = function(e) e)
  stopifnot(inherits(test, "error"))
  stopifnot(identical(test$message, "btensor() must appear once, with no effect modifiers, on the right hand side of the formula."))
})

################################################################################
#                                 End of File                                  #
################################################################################
