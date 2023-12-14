library(cpr)

################################################################################
##                          Updating a cpr_bs object                          ##
e <- new.env()
with(e, {
  bmat0 <- bsplines(seq(1, 10, length = 150), df = 5, order = 3)
  bmat1 <- bsplines(seq(1, 10, length = 150), df = 5, order = 4)
  bmat2 <- bsplines(seq(1, 10, length = 150), df = 12, order = 5)
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
  init_cp <- cp(pdg ~ bsplines(day, df = 30) + age + ttm, data = spdg)
  updt_cp <- update_bsplines(init_cp, df = 5)
  trgt_cp <- cp(pdg ~ bsplines(day, df = 5) + age + ttm, data = spdg)
  stopifnot(isTRUE(all.equal(updt_cp, trgt_cp)))
})


################################################################################
##                          Updating a cpr_cn object                          ##
e <- new.env()
with(e, {
  init_cn <- cn(pdg ~ btensor(list(day, age), df = list(30, 4)) + ttm, data = spdg)
  updt_cn <- update_btensor(init_cn, df = list(30, 2), order = list(3, 2))
  trgt_cn <- cn(pdg ~ btensor(list(day, age), df = list(30, 2), order = list(3, 2)) + ttm, data = spdg)
  stopifnot(isTRUE(all.equal(updt_cn, trgt_cn)))
})

################################################################################
#                                 End of File                                  #
################################################################################
