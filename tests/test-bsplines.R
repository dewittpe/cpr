library(cpr)
require(splines)

################################################################################
# print.cpr_bs

bmat       <- bsplines(1:100, df = 52)
print_bmat <- capture.output(bmat)

stopifnot(any(grepl("First\\s\\d+\\srows:", print_bmat)))
stopifnot(print_bmat[1] == "Basis matrix dims: [100 x 52]")
stopifnot(print_bmat[2] == "Order: 4")
stopifnot(print_bmat[3] == "Number of internal knots: 48")
stopifnot(print_bmat[4] == "")
stopifnot(print_bmat[5] == "First 6 rows:")
stopifnot(print_bmat[6] == "")

print_bmat <- capture.output(print(bmat, n = 1000))
stopifnot(!any(grepl("First\\s\\d+\\srows:", print_bmat)))

################################################################################
# Equivalent Basis Matrix
stopifnot(
  all.equal(
      current = unclass(bsplines(0:10, iknots = c(2, 2.6, 7.8), bknots = c(0, 10), order = 4))
    , target = unclass(splines::bs(0:10, knots = c(2, 2.6, 7.8), Boundary.knots = c(0, 10), intercept = TRUE))
    , check.attributes = FALSE)
)

################################################################################
# knots will be sorted
xvec <- seq(-1, 1, length = 25)
iknots <- c(0.34, -0.23)

x <- tryCatch(bsplines(xvec, iknots = iknots), warning = function(w) {w})
stopifnot("testing if warning is given when sorting knots" = 
          class(x) == c("simpleWarning", "warning", "condition"))
stopifnot("test warning message" = x$message == "Sorting knots")

stopifnot("expected bases generated after sorting knots" = 
  all.equal(
              current =   unclass(suppressWarnings(bsplines(xvec, iknots = c(.34, -.23))))
            , target = unclass(splines::bs(xvec, knots = iknots, intercept = TRUE))
            , check.attributes = FALSE
            )
)

################################################################################
x <- tryCatch(bsplines(list(1:10)), error = function(e) { e })
stopifnot(class(x) == c("simpleError", "error", "condition"))
stopifnot("error if list passed to bsplines" = 
          x$message == "x is a list.  use cpr::btensor instead of cpr::bsplines.")


################################################################################


test_that("combinations of iknots and df are handled well",
          {
            xvec <- seq(-1, 1, length = 25)
            expect_warning(bsplines(xvec, df = 2), "df being set to order")
            expect_equivalent(suppressWarnings(bsplines(xvec, df = 2)), bsplines(xvec, df = 4))
            expect_equivalent(suppressWarnings(bsplines(xvec, df = 4, order = 5)), bsplines(xvec, df = 5, order = 5))
            expect_equivalent(suppressWarnings(bsplines(xvec, df = 4, order = 5)), bsplines(xvec, order = 5))

            expect_warning(bsplines(xvec, iknots = 0.2, df = 6), "using iknots")
            expect_equivalent(suppressWarnings(bsplines(xvec, iknots = 0.2, df = 6)),
                              bsplines(xvec, iknots = 0.2))

            expect_equivalent(bsplines(xvec, df = 7),
                              bsplines(xvec, iknots = trimmed_quantile(xvec, probs = 1:3/4)))
          })

test_that("derivatives are as expected.",
          {
            xvec <- seq(-1, 5, length = 100)
            iknots <- c(-0.8, 0.2, 2.3, 2.5)

            xi <- sort(c(rep(range(xvec), 4), iknots))

            baseD1 <- spline.des(xi, xvec, derivs = rep(1, length(xvec)))$design
            baseD2 <- spline.des(xi, xvec, derivs = rep(2, length(xvec)))$design

            cprD1 <- bsplineD(xvec, iknots = iknots)
            cprD2 <- bsplineD(xvec, iknots = iknots, derivative = 2L)

            expect_equal(cprD1[-100, ], baseD1[-100, ])
            expect_equal(cprD2[-100, ], baseD2[-100, ])

            expect_error(bsplineD(xvec, derivative = 1.5))
            expect_error(bsplineD(xvec, derivative = 3))

          })
