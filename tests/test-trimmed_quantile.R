library(cpr)

################################################################################
# test that trimmed quantile warnings work
old_options <- options()
options(warn = 2)

e <- try(trimmed_quantile(1:100, trim =  3.9, prob = 1:23 / 24, name = FALSE), silent = TRUE)
stopifnot(inherits(e, "try-error"))
stopifnot(attr(e, "condition")$message == "(converted from warning) Overruling non-integer trim with floor(trim)")

e <- try(trimmed_quantile(1:100, trim = -3.9, prob = 1:23 / 24, name = FALSE), silent = TRUE)
stopifnot(inherits(e, "try-error"))
stopifnot(attr(e, "condition")$message == "(converted from warning) Overruling trim less than 1 with trim = 1L")

stopifnot(isTRUE(
  all.equal(suppressWarnings(trimmed_quantile(1:100, trim = 3.9, prob = 1:23 / 24, name = FALSE))
            , quantile(4:97, prob = 1:23 / 24)
            , check.attributes = FALSE
  )
  )
)

################################################################################
# test that trimmed quantile results are as expected
x <- sample(runif(100, 0, 6), size = 500, replace = TRUE)
ux <- unique(x)

xt <- x[!(x %in% range(x))]
uxt <- ux[!(ux %in% range(ux))]#-c(which.min(ux), which.max(ux))]

stopifnot(isTRUE(all.equal(trimmed_quantile(x), quantile(uxt))))
stopifnot(isTRUE(all.equal(trimmed_quantile(x, use_unique = FALSE), quantile(xt))))

################################################################################
# reset options
options(old_options)

################################################################################
##                                End of File                                 ##
################################################################################
