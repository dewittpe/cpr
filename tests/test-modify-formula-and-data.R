# what do I need?
#
# I need a way to set no intercept without a factor variable causing a rank
# deficient matrix

library(cpr)

################################################################################
# There is one method of interest, and it is non exported.  There are several S3
# methods to check.
stopifnot(!grepl("generate_cp_formula_data", ls("package:cpr")))
cpr_namespace <- ls( getNamespace("cpr"), all.names = TRUE)
stopifnot(sum(cpr_namespace == "generate_cp_formula_data") == 1L)
stopifnot(sum(grepl("generate_cp_formula_data", cpr_namespace)) == 1L)

################################################################################
# test formula and data construction

e <- new.env()
with(e, {
  data <-
    data.frame(
                 x1 = runif(20)
               , x2 = runif(20)
               , x3 = runif(20)
               , xf = factor(rep(c("l1","l2","l3","l4"), each = 5))
               , xc = rep(c("c1","c2","c3","c4", "c5"), each = 4)
               , pid = gl(n = 2, k = 10)
               , pid2 = rep(1:2, each = 10)
    )

  f <- ~ bsplines(x1, bknots = c(0,1)) + x2 + xf + xc + (x3 | pid2)

  cpr:::generate_cp_formula_data(f, data)

  stopifnot(isTRUE(
    all.equal(
              f_for_use
              ,
              . ~ bsplines(x1, bknots = c(0, 1)) + x2 + (x3 | pid2) + xfl2 + xfl3 + xfl4 + xcc2 + xcc3 + xcc4 + xcc5 - 1
              )
  ))

  stopifnot(isTRUE(identical(
    names(data_for_use)
    ,
    c("x1", "x2", "x3", "pid", "pid2", "xfl2", "xfl3", "xfl4", "xcc2", "xcc3", "xcc4", "xcc5")
  )))

})

e <- new.env()
with(e, {
  data <-
    data.frame(
                 x1 = runif(20)
               , x2 = runif(20)
               , x3 = runif(20)
               , xf = factor(rep(c("l1","l2","l3","l4"), each = 5))
               , xc = rep(c("c1","c2","c3","c4", "c5"), each = 4)
               , pid = gl(n = 2, k = 10)
               , pid2 = rep(1:2, each = 10)
    )

  f <- ~ bsplines(x1, bknots = c(0,1)) + x2 + xf + xc

  cpr:::generate_cp_formula_data(f, data)

  stopifnot(isTRUE(
    all.equal(
              f_for_use
              ,
              . ~ bsplines(x1, bknots = c(0, 1)) + x2 + xfl2 + xfl3 + xfl4 + xcc2 + xcc3 + xcc4 + xcc5 - 1
              )
  ))

  stopifnot(isTRUE(identical(
    names(data_for_use)
    ,
    c("x1", "x2", "x3", "pid", "pid2", "xfl2", "xfl3", "xfl4", "xcc2", "xcc3", "xcc4", "xcc5")
  )))

})



################################################################################
#                                 End of File                                  #
################################################################################
