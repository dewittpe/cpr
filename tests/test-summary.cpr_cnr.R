library(cpr)

acn <- cn(pdg ~ btensor(list(day, age)
                        , df = list(10, 8)
                        , bknots = list(c(-1, 1), c(44, 53))
                        ) + ttm
          , data = spdg)

cnr0 <- cnr(acn)

s <- summary(cnr0)

stopifnot(inherits(s, "cpr_summary_cpr_cnr"))
stopifnot(inherits(s, "data.frame"))


# print method
printed <- print(cnr0)
stopifnot(identical(printed, cnr0))

printed <- capture.output(print(cnr0))
stopifnot(identical(length(printed), 3L))

################################################################################
#                                 End of File                                  #
################################################################################
