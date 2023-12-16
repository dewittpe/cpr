library(cpr)
require(lme4)
require(geepack)

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
        structure(list(dfs = 4L, n_iknots = 0L, iknots = structure(list( numeric(0)), class = "AsIs"), loglik = 2218.47902453217, rss = 0.00409829163655395, rse = 0.0028744886068859, wiggle = structure(0.0685628056150533, abs.error = 7.61200054267312e-16, subdivisions = 1L, message = "OK"), fdsc = 1), row.names = c(NA, -1L), class = "data.frame")
      )
    )
  )
})

################################################################################
# verify that a control ploygon can be build via lmer
e <- new.env()
with(e, {
  head(spdg)

  lmer_cp <- cp(log10(pdg) ~ bsplines(day, bknots = c(-1, 1)) + (1 | id)
            , data = spdg
            , method = lmer
  )

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
        structure(list(dfs = 4L, n_iknots = 0L, iknots = structure(list( numeric(0)), class = "AsIs"), loglik = 7909.94604102081, rss = 622.553425369456, rse = 0.159004352365617, wiggle = structure(60.2815339858114, abs.error = 6.69259469907717e-13, subdivisions = 1L, message = "OK"), fdsc = 2), row.names = c(NA, -1L), class = "data.frame")
      )
    )
  )

  lmer_cp <- cp(log10(pdg) ~ bsplines(day, df = 10, bknots = c(-1, 1)) + (1 | id)
            , data = spdg
            , method = lmer
  )
  lmer_cpr <- cpr(lmer_cp)
  stopifnot(identical(summary(lmer_cpr)$rse_elbow[3], 1L))
})

################################################################################
# Verify that a control polygon can be build from a gee
e <- new.env()
with(e, {
  head(spdg)

  gee_cp <- cp(log10(pdg) ~ bsplines(day, bknots = c(-1, 1))
               , data = spdg
               , method = geeglm
               , method.args = list(id = as.name("id"), corstr = "ar1")
  )

  stopifnot(
    isTRUE(
      all.equal(
        gee_cp$cp |> dput()
        ,
        structure(list(xi_star = c(-1, -0.333333333333333, 0.333333333333333, 1), theta = c(0.0066448178731621, -1.91923731782891, 1.93439356253956, 0.0233810025421451)), class = "data.frame", row.names = c(NA, -4L))
      )
    )
  )

  stopifnot(
    isTRUE(
      all.equal(
        summary(gee_cp) |> dput()
        ,
        structure(list(dfs = 4L, n_iknots = 0L, iknots = structure(list( numeric(0)), class = "AsIs"), loglik = -1464.93520368629, rss = 2929.87040737258, rse = 0.344941068561338, wiggle = structure(49.9755793513228, abs.error = 5.54840388648201e-13, subdivisions = 1L, message = "OK"), fdsc = 2), row.names = c(NA, -1L), class = "data.frame")
      )
    )
  )

  gee_cp <- cp(log10(pdg) ~ bsplines(day, df = 10, bknots = c(-1, 1))
               , data = spdg
               , method = geepack::geeglm
               , method.args = list(id = as.name("id"), corstr = "ar1")
  )

  gee_cpr <- cpr(gee_cp)
  summary(gee_cpr)

  stopifnot(identical(summary(gee_cpr)$rse_elbow[3], 1L))

})

################################################################################
#                                 End of File                                  #
################################################################################
