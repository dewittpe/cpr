library(cpr)

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
#                                 End of File                                  #
################################################################################
