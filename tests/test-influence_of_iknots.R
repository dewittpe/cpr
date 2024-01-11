library(cpr)
set.seed(42)

################################################################################
e <- new.env()
with(e, {
  x <- runif(n = 100, 0, 6)#seq(0 + 1/5000, 6 - 1/5000, length.out = 100)
  bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
  theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
  DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
  DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

  initial_cp <-
    cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6)), data = DF)

  iok <- influence_of_iknots(initial_cp)

  stopifnot(inherits(iok, "cpr_influence_of_iknots"))

  # if the following test for the names of the object changes make sure to
  # update the @return section of the source file.
  stopifnot(identical(names(iok), c("original_cp", "coarsened_cps", "restored_cps", "d", "influence", "chisq")))

  s <- summary(iok)

  stopifnot(identical(dim(s), c(6L, 8L)))
  stopifnot(identical(names(s), c("j", "iknot", "influence", "influence_rank", "chisq", "chisq_rank", "p_value", "os_p_value")))
})

################################################################################
# verbose vs non-versbose
e <- new.env()
with(e, {
  x <- runif(n = 100, 0, 6)
  bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
  theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
  DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
  DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

  initial_cp <-
    cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6)), data = DF)

  iok0 <- influence_of_iknots(initial_cp, verbose = FALSE, cl = 0)
  iok1 <- influence_of_iknots(initial_cp, verbose = FALSE, cl = 1)
  iok2 <- influence_of_iknots(initial_cp, verbose = FALSE, cl = 2)
  iok0v <- influence_of_iknots(initial_cp, verbose = TRUE, cl = 0)
  iok1v <- influence_of_iknots(initial_cp, verbose = TRUE, cl = 1)
  iok2v <- influence_of_iknots(initial_cp, verbose = TRUE, cl = 2)

  stopifnot(isTRUE(all.equal(iok0, iok1)))
  stopifnot(isTRUE(all.equal(iok0, iok2)))
  stopifnot(isTRUE(all.equal(iok0, iok0v)))
  stopifnot(isTRUE(all.equal(iok0, iok1v)))
  stopifnot(isTRUE(all.equal(iok0, iok2v)))

})

################################################################################
# CPR
# verbose vs non-versbose
e <- new.env()
with(e, {
  x <- runif(n = 100, 0, 6)
  bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
  theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
  DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
  DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

  initial_cp <-
    cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6)), data = DF)
  cpr_run <- cpr(initial_cp)

  ioik_cpr <- influence_of_iknots(cpr_run)

  ioik_list <- lapply(cpr_run, influence_of_iknots)

  stopifnot(identical(ioik_cpr, ioik_cpr))

  stopifnot(
    identical(
      attr(cpr_run, "ioik")
      ,
      lapply(ioik_list, summary)
    )
  )

  stopifnot(
    all.equal(
      do.call(rbind, attr(cpr_run, "ioik"))
      ,
      summary(ioik_cpr)[, 1:8]
      ,
      check.attributes = FALSE
    )
  )

  stopifnot(
    identical(
      summary(ioik_cpr)[, "index"]
      ,
      c(1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7)
    )
  )

})

################################################################################
e <- new.env()
with(e, {

#  acn <- cn(pdg ~ btensor(list(day, age)
#                          , df = list(10, 8)
#                          , bknots = list(c(-1, 1), c(44, 53))
#                          ) + ttm
#            , data = spdg)
#
#  str(acn, max.level = 1)
#
#  influence_of_iknots(acn)
#
#  acnr <- cnr(acn)
#  stop("TEST NEEDS TO BE WRITTEN")



})


################################################################################
#                                 End of File                                  #
################################################################################
