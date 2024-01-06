library(cpr)
require(lme4)
require(geepack)
################################################################################
# Verify that cp.cpr_bs and cp.formula both return similar objects
e <- new.env()
with(e, {

  xvec <- runif(500, 0, 6)
  bknots <- c(0, 6)
  dat <- data.frame(x = xvec, y = sin((xvec - 2)/pi) + 1.4 * cos(xvec/pi))
  acp <- cp(y ~ bsplines(x, df = 8, bknots = bknots), data = dat)
  theta <- coef(lm(y ~ bsplines(x, df = 8, bknots = bknots) - 1, data = dat))
  bcp <- cp(bsplines(xvec, df =8, bknots = bknots), theta)

  stopifnot(isTRUE(all.equal(names(acp), names(bcp))))

  stopifnot(identical(
    names(acp)
    ,
    c("cp", "xi", "iknots", "bknots", "order", "call", "keep_fit", "fit", "theta", "theta_vcov", "coefficients", "vcov", "vcov_theta", "loglik", "rss", "rse")
  ))

})

################################################################################
# Verify that an error is thrown if bsplines is not used as expected in the
# formula
e <- new.env()
with(e, {
  test <- tryCatch(cp(log10(pdg) ~ age + ttm, data = spdg), error = function(e) e)
  stopifnot(inherits(test, "error"))
  stopifnot(identical(test$message, "bsplines() must appear first, once, and with no effect modifiers, on the right hand side of the formula."))
})

e <- new.env()
with(e, {
  test <- tryCatch(cp(log10(pdg) ~ age + bsplines(ttm), data = spdg), error = function(e) e)
  stopifnot(inherits(test, "error"))
  stopifnot(identical(test$message, "bsplines() must appear first, once, and with no effect modifiers, on the right hand side of the formula."))
})

e <- new.env()
with(e, {
  test <- tryCatch(cp(log10(pdg) ~ bsplines(ttm)*age, data = spdg), error = function(e) e)
  stopifnot(inherits(test, "error"))
  stopifnot(identical(test$message, "bsplines() must appear first, once, and with no effect modifiers, on the right hand side of the formula."))
})

################################################################################
# Verify that a control polygon can be build from a lm
e <- new.env()
with(e, {
  xvec <- seq(0, 5.9999, length = 500)
  bknots <- c(0, 6)
  dat <- data.frame(x = xvec, y = sin((xvec - 2)/pi) + 1.4 * cos(xvec/pi))
  cp3 <- cp(y ~ bsplines(x, bknots = bknots), data = dat)

  stopifnot(
    isTRUE(
      all.equal(
        cp3$cp
        ,
        structure(list(xi_star = c(0, 2, 4, 6),
                       theta = c(0.797026006387093, 1.36601191348564, 1.19010324873104, 0.482219646221653)),
                  class = "data.frame", row.names = c(NA, -4L))
      )
    )
  )

  stopifnot(
    isTRUE(
      all.equal(
        summary(cp3)
        ,
        structure(list(dfs = 4L, n_iknots = 0L, iknots = structure(list( numeric(0)), class = "AsIs"), loglik = 2218.47902453217, rss = 0.00409829163655395, rse = 0.0028744886068859, wiggle = structure(0.0685628056150533, abs.error = 7.61200054267312e-16, subdivisions = 1L, message = "OK"), fdsc = 1), row.names = c(NA, -1L), class = c("cpr_summary_cpr_cp", "data.frame"))
      )
    )
  )
})

################################################################################
# verify that a control ploygon can be build via lmer
e <- new.env()
with(e, {

  lmer_cp <- cp(log10(pdg) ~ bsplines(day, bknots = c(-1, 1)) + (1 | id)
            , data = spdg
            , method = lmer)

  stopifnot(
    isTRUE(
      all.equal(
        lmer_cp$cp
        ,
        structure(list(xi_star = c(-1, -0.333333333333333, 0.333333333333333, 1), theta = c(0.192421846281352, -2.18938687153398, 1.98993907207642, 0.0777684276192437)), class = "data.frame", row.names = c(NA, -4L))
      )
    )
  )

  stopifnot(
    isTRUE(
      all.equal(
        summary(lmer_cp)
        ,
        structure(list(dfs = 4L, n_iknots = 0L, iknots = structure(list( numeric(0)), class = "AsIs"), loglik = 7909.94604102081, rss = 622.553425369456, rse = 0.159004352365617, wiggle = structure(60.2815339858114, abs.error = 6.69259469907717e-13, subdivisions = 1L, message = "OK"), fdsc = 2), row.names = c(NA, -1L), class = c("cpr_summary_cpr_cp", "data.frame"))
      )
    )
  )

})

################################################################################
# Verify that a control polygon can be build from a gee
e <- new.env()
with(e, {

  gee_cp <- cp(log10(pdg) ~ bsplines(day, bknots = c(-1, 1))
               , data = spdg
               , method = geeglm
               , method.args = list(id = as.name("id"), corstr = "ar1")
  )

  stopifnot(
    isTRUE(
      all.equal(
        gee_cp$cp
        ,
        structure(list(xi_star = c(-1, -0.333333333333333, 0.333333333333333, 1), theta = c(0.0066448178731621, -1.91923731782891, 1.93439356253956, 0.0233810025421451)), class = "data.frame", row.names = c(NA, -4L))
      )
    )
  )

  stopifnot(
    isTRUE(
      all.equal(
        summary(gee_cp)
        ,
        structure(list(dfs = 4L, n_iknots = 0L, iknots = structure(list( numeric(0)), class = "AsIs"), loglik = -1464.93520368629, rss = 2929.87040737258, rse = 0.344941068561338, wiggle = structure(49.9755793513228, abs.error = 5.54840388648201e-13, subdivisions = 1L, message = "OK"), fdsc = 2), row.names = c(NA, -1L), class = c("cpr_summary_cpr_cp", "data.frame"))
      )
    )
  )

  gee_cp <- cp(log10(pdg) ~ bsplines(day, df = 10, bknots = c(-1, 1))
               , data = spdg
               , method = geepack::geeglm
               , method.args = list(id = as.name("id"), corstr = "ar1")
  )

  gee_cpr <- cpr(gee_cp)
  expected_summary <- structure(list(dfs = 4:10, n_iknots = 0:6, iknots = structure(list( numeric(0), -0.188465250965251, c(-0.188465250965251, 0.522392938868911), c(-0.387864823348694, -0.188465250965251, 0.522392938868911), c(-0.791871921182266, -0.387864823348694, -0.188465250965251, 0.522392938868911), c(-0.791871921182266, -0.591390091390091, -0.387864823348694, -0.188465250965251, 0.522392938868911), c(-0.791871921182266, -0.591390091390091, -0.387864823348694, -0.188465250965251, 0.0576230492196879, 0.522392938868911)), class = "AsIs"), loglik = c(-1464.93520368629, -1376.00702167216, -1366.38011307588, -1362.38424749121, -1362.23759222167, -1362.19963612715, -1362.29607387444), rss = c(2929.87040737258, 2752.01404334433, 2732.76022615176, 2724.76849498243, 2724.47518444335, 2724.39927225431, 2724.59214774888), rse = c(0.344941068561338, 0.334314212839968, 0.333149449966508, 0.332668714884735, 0.332657564802185, 0.332659686294428, 0.33267821811044), wiggle = structure(c(49.9755793513228, 57.2632257630784, 36.42646977805, 39.72463162264, 42.8679143932625, 40.3142809497938, 40.19072967787), abs.error = 5.54840388648201e-13, subdivisions = 1L, message = "OK"), fdsc = c(2, 2, 2, 2, 2, 2, 2), `Pr(>w_(1))` = c(NA, 0, 0, 0, 9.01352735449557e-07, 0.00711887714037818, 0.0103830482941717)), row.names = c(NA, -7L), class = c("cpr_summary_cpr_cpr", "cpr_summary_cpr_cp", "data.frame"), elbow = structure(c(3, 2, 3, 2, 3, 2), dim = 2:3, dimnames = list(c("quadratic", "linear"), c("loglik", "rss", "rse"))))
  stopifnot(isTRUE(all.equal(expected_summary, summary(gee_cpr))))

})

################################################################################
#                                rank deficient?                               #
e <- new.env()
with(e, {

  # First, a good fit
  cp0 <- cp(pdg ~ bsplines(day, bknots = c(-1, 1)) + ttm , data = spdg)

  stopifnot(inherits(cp0, "cpr_cp"))

  # Now update to something that is rank deficient
  cp1 <- tryCatch(update_bsplines(cp0, iknots = c(0, 0, 0, 0, 0)), warning = function(w) w)

  stopifnot(inherits(cp1, 'warning'))
  stopifnot(identical(cp1$message, 'Design Matrix is rank deficient. keep_fit being set to TRUE.'))

  cp2 <- tryCatch(cp(pdg ~ bsplines(day, iknots = c(0, 0, 0, 0, 0), bknots = c(-1, 1)) + ttm , data = spdg), warning = function(w) w)

  stopifnot(inherits(cp2, 'warning'))
  stopifnot(identical(cp2$message, 'Design Matrix is rank deficient. keep_fit being set to TRUE.'))

})

################################################################################
##                              printing method                               ##
e <- new.env()
with(e, {
  xvec <- runif(500, 0, 6)
  bknots <- c(0, 6)
  dat <- data.frame(x = xvec, y = sin((xvec - 2)/pi) + 1.4 * cos(xvec/pi))
  acp <- cp(y ~ bsplines(x, df = 8, bknots = bknots), data = dat)
  theta <- coef(lm(y ~ bsplines(x, df = 8, bknots = bknots) - 1, data = dat))
  bcp <- cp(bsplines(xvec, df =8, bknots = bknots), theta)

  # verify the value is returned from the print call
  stopifnot(identical(acp, print(acp)))
  stopifnot(identical(bcp, print(bcp)))

  acpcap <- capture.output(print(acp))
  expected <- capture.output(print(acp$cp))
  stopifnot(identical(acpcap, expected))

  bcpcap <- capture.output(print(bcp))
  expected <- capture.output(print(bcp$cp))
  stopifnot(identical(bcpcap, expected))

})

################################################################################
#                                 End of File                                  #
################################################################################
