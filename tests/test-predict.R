################################################################################
# testing the prediction method

library(cpr)
set.seed(42)

################################################################################
#                        verify error if fit is missing                        #
e <- new.env()
with(e, {
  acp <- cp(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53))
            , data = spdg
            , keep_fit = FALSE)
  stopifnot(is.null(acp$fit))
  test <- tryCatch(predict(acp), error = function(e) e)
  stopifnot(inherits(test, "error"))
})

################################################################################
#                         simple lm with just a spline                         #

# without newdata
e <- new.env()
with(e, {
  acp <- cp(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53))
            , data = spdg
            , keep_fit = TRUE)
  alm <- lm(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53)) + 0
            , data = spdg)

  stopifnot("same lm coef" = identical(acp$coef, alm$coef))

  # testing without newdata
  acp_pred0 <- predict(acp$fit, se.fit = TRUE)
  acp_pred <- predict(acp, se.fit = TRUE)
  alm_pred <- predict(alm, se.fit = TRUE)

  stopifnot(isTRUE( all.equal(acp_pred0$fit, alm_pred$fit, check.attributes = FALSE)))
  stopifnot("acp prediction without newdata" =
            isTRUE( all.equal(acp_pred$fit, alm_pred$fit, check.attributes = FALSE)))
})

# with newdata
e <- new.env()
with(e, {
  acp <- cp(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53))
            , data = spdg
            , keep_fit = TRUE)
  alm <- lm(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53)) + 0
            , data = spdg)

  stopifnot("same lm coef" = identical(acp$coef, alm$coef))

  # testing with newdata
  nd <- data.frame(age = runif(n = 123, min = 45, max = 53))

  acp_pred0 <- predict(acp$fit, newdata = nd, se.fit = TRUE)
  alm_pred <- predict(alm, newdata = nd, se.fit = TRUE)

  acp_pred <- predict(acp, newdata = nd, se.fit = TRUE)

  stopifnot(isTRUE( all.equal(acp_pred0$fit, alm_pred$fit, check.attributes = FALSE)))
  stopifnot("acp prediction with newdata" =
            isTRUE( all.equal(acp_pred$fit, alm_pred$fit, check.attributes = FALSE)))
})

################################################################################
#                        verify error if fit is missing                        #
e <- new.env()
with(e, {

  acn <- cn(
      pdg ~ btensor(list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(45, 53))) + ttm
    , data = spdg
    , keep_fit = FALSE
  )

  stopifnot(is.null(acn$fit))
  test <- tryCatch(predict(acn), error = function(e) e)
  stopifnot(inherits(test, "error"))
})

################################################################################
#                         simple lm with just a spline                         #
e <- new.env()
with(e, {
  acn <- cn(
      log10(pdg) ~ btensor(list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(45, 53))) + ttm
    , data = spdg
    , keep_fit = TRUE
  )

  alm <- lm(log10(pdg) ~ 0 + btensor(list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(45, 53))) + ttm, data = spdg)

  stopifnot("same lm coef" = identical(acn$coef, alm$coef))

  acn_pred0 <- predict(acn$fit, se.fit = TRUE)
  acn_pred <- predict(acn, se.fit = TRUE)
  alm_pred <- predict(alm, se.fit = TRUE)

  stopifnot(isTRUE( all.equal(acn_pred0$fit, alm_pred$fit, check.attributes = FALSE)))
  stopifnot("acn predict without newdata" =
            isTRUE( all.equal(acn_pred$fit, alm_pred$fit, check.attributes = FALSE)))
})

e <- new.env()
with(e, {
  acn <- cn(
      log10(pdg) ~ btensor(list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(45, 53))) + ttm
    , data = spdg
    , keep_fit = TRUE
  )

  alm <- lm(log10(pdg) ~ 0 + btensor(list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(45, 53))) + ttm, data = spdg)

  stopifnot("same lm coef" = identical(acn$coef, alm$coef))

  nd <- data.frame(
        day = runif(n = 123, min = -1, max = 1)
      , age = runif(n = 123, min = 45, max = 53)
      , ttm = runif(n = 123, min = -8, max = -1)
    )

  acn_pred0 <- predict(acn$fit, newdata = nd, se.fit = TRUE)
  acn_pred <- predict(acn, newdata = nd, se.fit = TRUE)
  alm_pred <- predict(alm, newdata = nd, se.fit = TRUE)

  stopifnot(isTRUE( all.equal(acn_pred0$fit, alm_pred$fit, check.attributes = FALSE)))
  stopifnot("acn predict with newdata" =
            isTRUE( all.equal(acn_pred$fit, alm_pred$fit, check.attributes = FALSE)))
})


################################################################################
#                                 End of File                                  #
################################################################################
