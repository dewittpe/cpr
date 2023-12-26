library(cpr)
require(lme4)

################################################################################

e <- new.env()
with(e, {

  xvec <- runif(500, 0, 6)
  bknots <- c(0, 6)

  dat <- data.frame(x = xvec, y = sin((xvec - 2)/pi) + 1.4 * cos(xvec/pi))
  acp <- cp(y ~ bsplines(x, df = 8, bknots = bknots), data = dat)

  theta <- coef(lm(y ~ bsplines(x, df = 8, bknots = bknots) - 1, data = dat))
  bcp <- cp(bsplines(xvec, df =8, bknots = bknots), theta)

  s1 <- summary(acp, wiggle = TRUE)
  s2 <- summary(acp, wiggle = TRUE)
  s3 <- summary(bcp, wiggle = FALSE)
  s4 <- summary(bcp, wiggle = FALSE)

  stopifnot(inherits(s1, "cpr_summary_cpr_cp"))
  stopifnot(inherits(s2, "cpr_summary_cpr_cp"))
  stopifnot(inherits(s3, "cpr_summary_cpr_cp"))
  stopifnot(inherits(s4, "cpr_summary_cpr_cp"))

  stopifnot(inherits(s1, "data.frame"))
  stopifnot(inherits(s2, "data.frame"))
  stopifnot(inherits(s3, "data.frame"))
  stopifnot(inherits(s4, "data.frame"))

  stopifnot(identical(names(s1), c("dfs", "n_iknots", "iknots", "loglik", "rss", "rse", "wiggle", "fdsc")))
  stopifnot(identical(names(s2), c("dfs", "n_iknots", "iknots", "loglik", "rss", "rse", "wiggle", "fdsc")))
  stopifnot(identical(names(s3), c("dfs", "n_iknots", "iknots", "loglik", "rss", "rse", "wiggle", "fdsc")))
  stopifnot(identical(names(s4), c("dfs", "n_iknots", "iknots", "loglik", "rss", "rse", "wiggle", "fdsc")))

  stopifnot(identical(nrow(s1), 1L))
  stopifnot(identical(nrow(s2), 1L))
  stopifnot(identical(nrow(s3), 1L))
  stopifnot(identical(nrow(s4), 1L))

  stopifnot(identical(s1$dfs, 8L))
  stopifnot(identical(s2$dfs, 8L))
  stopifnot(identical(s3$dfs, 8L))
  stopifnot(identical(s4$dfs, 8L))

  stopifnot(identical(s1$n_iknots, 4L))
  stopifnot(identical(s2$n_iknots, 4L))
  stopifnot(identical(s3$n_iknots, 4L))
  stopifnot(identical(s4$n_iknots, 4L))

  stopifnot(is.list(s1$iknots))
  stopifnot(is.list(s2$iknots))
  stopifnot(is.list(s3$iknots))
  stopifnot(is.list(s4$iknots))

  stopifnot(isTRUE(!is.na(s1$loglik)))
  stopifnot(isTRUE(!is.na(s2$loglik)))
  stopifnot(isTRUE(is.na(s3$loglik)))
  stopifnot(isTRUE(is.na(s4$loglik)))

  stopifnot(isTRUE(!is.na(s1$rss)))
  stopifnot(isTRUE(!is.na(s2$rss)))
  stopifnot(isTRUE(is.na(s3$rss)))
  stopifnot(isTRUE(is.na(s4$rss)))

  stopifnot(isTRUE(!is.na(s1$rse)))
  stopifnot(isTRUE(!is.na(s2$rse)))
  stopifnot(isTRUE(is.na(s3$rse)))
  stopifnot(isTRUE(is.na(s4$rse)))

  stopifnot(isTRUE(!is.na(s1$wiggle)))
  stopifnot(isTRUE(!is.na(s2$wiggle)))
  stopifnot(isTRUE(is.na(s3$wiggle)))
  stopifnot(isTRUE(is.na(s4$wiggle)))

  stopifnot(isTRUE(!is.na(s1$fdsc)))
  stopifnot(isTRUE(!is.na(s2$fdsc)))
  stopifnot(isTRUE(is.na(s3$fdsc)))
  stopifnot(isTRUE(is.na(s4$fdsc)))

})
################################################################################
# summary of cp built via lmer
e <- new.env()
with(e, {

  lmer_cp <- cp(log10(pdg) ~ bsplines(day, bknots = c(-1, 1)) + (1 | id)
            , data = spdg
            , method = lmer)

  stopifnot(
    isTRUE(
      all.equal(
        summary(lmer_cp)
        ,
        structure(list(dfs = 4L, n_iknots = 0L, iknots = structure(list( numeric(0)), class = "AsIs"), loglik = 7909.94604102081, rss = 622.553425369456, rse = 0.159004352365617, wiggle = structure(60.2815339858114, abs.error = 6.69259469907717e-13, subdivisions = 1L, message = "OK"), fdsc = 2), row.names = c(NA, -1L), class = c("cpr_summary_cpr_cp", "data.frame"))
      )
    )
  )

  lmer_cp <- cp(log10(pdg) ~ bsplines(day, df = 10, bknots = c(-1, 1)) + (1 | id)
            , data = spdg
            , method = lmer
  )
  lmer_cpr <- cpr(lmer_cp)
  summary(lmer_cpr)
  expected_summary <- structure(list(dfs = 4:10, n_iknots = 0:6, iknots = structure(list( numeric(0), -0.188465250965251, c(-0.188465250965251, 0.522392938868911), c(-0.387864823348694, -0.188465250965251, 0.522392938868911), c(-0.591390091390091, -0.387864823348694, -0.188465250965251, 0.522392938868911), c(-0.591390091390091, -0.387864823348694, -0.188465250965251, 0.0576230492196879, 0.522392938868911), c(-0.791871921182266, -0.591390091390091, -0.387864823348694, -0.188465250965251, 0.0576230492196879, 0.522392938868911)), class = "AsIs"), loglik = c(7909.94604102081, 10436.9685349426, 10737.2482721693, 10914.6482850108, 10921.8458243132, 10921.6797222902, 10918.8473988721), rss = c(622.553425369456, 503.060849675992, 490.342743968001, 482.912931876825, 482.468659854174, 482.310898509441, 482.293585986326), rse = c(0.159004352365617, 0.14293539213227, 0.141119884726865, 0.140049503551057, 0.139987909982521, 0.139967863538051, 0.139968194161426), wiggle = structure(c(60.2815339858114, 68.4958125786486, 35.8347712135129, 40.5896605338692, 38.0314913723466, 39.1709974050207, 41.1623689089349), abs.error = 6.69259469907717e-13, subdivisions = 1L, message = "OK"), fdsc = c(2, 2, 2, 2, 2, 2, 2), `Pr(>w_(1))` = c(NA, 0, 0, 0, 0, 4.04076772042572e-12, 0.00208664723131902)), row.names = c(NA, -7L), class = c("cpr_summary_cpr_cpr", "cpr_summary_cpr_cp", "data.frame"), elbow = structure(c(3, 2, 3, 2, 3, 2), dim = 2:3, dimnames = list( c("quadratic", "linear"), c("loglik", "rss", "rse"))))
  stopifnot(isTRUE(all.equal(target = expected_summary, current = summary(lmer_cpr))))
})

################################################################################
#                                 End of File                                  #
################################################################################
