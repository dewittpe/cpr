library(cpr)

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

################################################################################
# Verify that a control polygon can be build from a gee
#require(geepack)
#cp_gee  <- cp(log10(pdg) ~ bsplines(day, df = 10),
#              method = geeglm,
#              method.args = list(id = id),
#              data = spdg,
#              keep_fit = TRUE)

################################################################################
#                                 End of File                                  #
################################################################################
