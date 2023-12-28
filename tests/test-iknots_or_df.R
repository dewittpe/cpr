library(cpr)

# testing of the iknots_or_df method

################################################################################
# check that the method is not exported
stopifnot(!grepl("iknots_or_df", ls("package:cpr")))
cpr_namespace <- ls( getNamespace("cpr"), all.names = TRUE)
stopifnot(sum(cpr_namespace == "iknots_or_df") == 1L)
stopifnot(sum(grepl("^coef_vcov$", cpr_namespace)) == 1L)

################################################################################
# use iknots over all else
xvec <- runif(600, min = 0, max = 3)
stopifnot(identical(
  cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = NULL, order = NULL)
  ,
  1:2
))

# a warning when iknots and df are provided
awarning <- tryCatch(
  cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = 56, order = 12)
  , warning = function(w) w)
stopifnot(inherits(awarning, "warning"))
stopifnot(identical(awarning$message, "Both iknots and df defined, using iknots"))

# rtn numeric(0) when df == order
for (i in 2:10) {
  stopifnot(identical(cpr:::iknots_or_df(x = xvec, iknots = NULL, df = i, order = i),
                      numeric(0)))
}

# trimmed_quantile when df > order
for (i in 2:10) {
  stopifnot(
    isTRUE(
      all.equal(
        cpr:::iknots_or_df(x = xvec, iknots = NULL, df = 4 + i, order = 4)
        ,
        trimmed_quantile(x = xvec, probs = seq(1, i, by = 1) / (i + 1))
        )
      )
  )
}


################################################################################
#                                 End of File                                  #
################################################################################
