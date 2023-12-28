library(cpr)

################################################################################
##                          Updating a cpr_bs object                          ##
e <- new.env()
with(e, {
  x <- runif(n = 150, min = 1, max = 10)
  bmat0 <- bsplines(x, bknots = c(1, 10), df = 5, order = 3)
  bmat1 <- bsplines(x, bknots = c(1, 10), df = 5, order = 4)
  bmat2 <- bsplines(x, bknots = c(1, 10), df = 12, order = 5)
  stopifnot(isTRUE(all.equal(update_bsplines(bmat0, order = 4), bmat1)))
  stopifnot(isTRUE(all.equal(update_bsplines(bmat0, df = 12, order = 5), bmat2)))
})

################################################################################
##                          Updating a cpr_bt object                          ##
e <- new.env()
with(e, {
  tpmat0 <- btensor(list(seq(0, 1, length = 10), seq(0, 1, length = 10)), df = list(4, 5))
  tpmat1 <- btensor(list(seq(0, 1, length = 10), seq(0, 1, length = 10)), df = list(6, 7))
  stopifnot(isTRUE(all.equal(update_btensor(tpmat0, df = list(6, 7)), tpmat1)))
})

################################################################################
##      Updating bsplines or btensor on the right and side of a formula       ##
e <- new.env()
with(e, {
  f1 <- y ~ bsplines(x, df = 14) + var1 + var2
  f2 <- y ~ btensor(x = list(x1, x2), df = list(50, 31), order = list(3, 5))  + var1 + var2
  stopifnot(identical(update_bsplines(f1, df = 13, order = 5), y ~ bsplines(x, df = 13, order = 5) + var1 + var2))
  stopifnot(identical(update_btensor(f2, df = list(13, 24), order = list(3, 8)), y ~ btensor(x = list(x1, x2), df = list(13, 24), order = list(3, 8)) + var1 + var2))
})

################################################################################
##                          Updating a cpr_cp object                          ##
e <- new.env()
with(e, {
  data(spdg, package = "cpr")
  init_cp <- cp(pdg ~ bsplines(day, df = 30, bknots = c(-1, 1)) + age + ttm, data = spdg)
  updt_cp <- update_bsplines(init_cp, df = 5)
  trgt_cp <- cp(pdg ~ bsplines(day, df = 5, bknots = c(-1, 1)) + age + ttm, data = spdg)
  stopifnot(isTRUE(all.equal(updt_cp, trgt_cp)))
})


################################################################################
##                          Updating a cpr_cn object                          ##
e <- new.env()
with(e, {
  init_cn <- cn(pdg ~ btensor(list(day, age)
                              , df = list(30, 4)
                              , bknots = list(c(-1, 1), c(45, 53))
                              ) + ttm, data = spdg)
  updt_cn <- update_btensor(init_cn, df = list(30, 2), order = list(3, 2))
  trgt_cn <- cn(pdg ~ btensor(list(day, age)
                              , df = list(30, 2)
                              , bknots = list(c(-1, 1), c(45, 53))
                              , order = list(3, 2)
                              ) + ttm, data = spdg)
  stopifnot(isTRUE(all.equal(updt_cn, trgt_cn)))
})

################################################################################
##                          verify unevaluated call                           ##
# update_bsplines and update_btensor need to return unevaluated calls for use in
# the cpr and cnr functions.

e <- new.env()
with(e, {
  init_cn <- cn(pdg ~ btensor(list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(45, 53))) + ttm, data = spdg)
  newcall <- update_btensor(init_cn, df = list(30, 2), order = list(3, 2), evaluate = FALSE)

  expectedcall <- list()
  expectedcall[[1]] <- quote(cn)
  expectedcall[["formula"]] <- pdg ~ btensor(list(day, age) , df = list(30, 2) , bknots = list(c(-1, 1), c(45, 53)) , order = list(3, 2)) + ttm
  expectedcall[["data"]] <- quote(spdg)
  expectedcall <- as.call(expectedcall)

  stopifnot( is.call(newcall) )
  stopifnot(isTRUE(all.equal(newcall, expectedcall, check.attributes = FALSE)))

})

e <- new.env()
with(e, {
  init_cp <- cp(pdg ~ bsplines(day, df = 30, bknots = c(-1, 1)) + age + ttm, data = spdg)
  newcall <- update_bsplines(init_cp, df = 5, evaluate = FALSE)
  expectedcall <- list()
  expectedcall[[1]] <- quote(cp)
  expectedcall[["formula"]] <- pdg ~ bsplines(day, df = 5, bknots = c(-1, 1)) + age + ttm
  expectedcall[["data"]] <- quote(spdg)
  expectedcall <- as.call(expectedcall)
  stopifnot( is.call(newcall) )
  stopifnot(isTRUE(all.equal(newcall, expectedcall, check.attributes = FALSE)))
})

e <- new.env()
with(e, {
  x <- runif(n = 150, min = 1, max = 10)
  bmat0 <- bsplines(x, bknots = c(1, 10), df = 5, order = 3)
  newcall <- update_bsplines(bmat0, iknots = 4, bknots = c(1, 11), df = 5, order = 4, evaluate = FALSE)
  expectedcall <- list()
  expectedcall[[1]] <- quote(bsplines)
  # the order of the arguments is a possible source of a missmatch in the
  # expected calls.
  expectedcall[["x"]] <- quote(x)
  expectedcall[["df"]] <- 5
  expectedcall[["bknots"]] <- c(1, 11)
  expectedcall[["order"]] <- 4
  expectedcall[["iknots"]] <- 4
  expectedcall <- as.call(expectedcall)
  stopifnot( is.call(newcall) )
  stopifnot(isTRUE(all.equal(newcall, expectedcall, check.attributes = FALSE)))
})

################################################################################
#                                 End of File                                  #
################################################################################
