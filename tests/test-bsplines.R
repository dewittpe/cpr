library(cpr)
require(splines)

################################################################################
stopifnot("Equivalent Basis Matrix between cpr and splines::bs" =
  isTRUE(
    all.equal(
        current = unclass(bsplines(0:10, iknots = c(2, 2.6, 7.8), bknots = c(0, 10), order = 4))
      , target = unclass(splines::bs(0:10, knots = c(2, 2.6, 7.8), Boundary.knots = c(0, 10), intercept = TRUE))
      , check.attributes = FALSE
    )
  )
)

################################################################################
# print.cpr_bs

bmat       <- bsplines(1:100, df = 52)

iknots <-
#  attr(bmat, "iknots") |> dput()
 c(3.97959183673469, 5.95918367346939, 7.93877551020408, 9.91836734693877,
 11.8979591836735, 13.8775510204082, 15.8571428571429, 17.8367346938775,
 19.8163265306122, 21.7959183673469, 23.7755102040816, 25.7551020408163,
 27.734693877551, 29.7142857142857, 31.6938775510204, 33.6734693877551,
 35.6530612244898, 37.6326530612245, 39.6122448979592, 41.5918367346939,
 43.5714285714286, 45.5510204081633, 47.530612244898, 49.5102040816327,
 51.4897959183673, 53.469387755102, 55.4489795918367, 57.4285714285714,
 59.4081632653061, 61.3877551020408, 63.3673469387755, 65.3469387755102,
 67.3265306122449, 69.3061224489796, 71.2857142857143, 73.265306122449,
 75.2448979591837, 77.2244897959184, 79.2040816326531, 81.1836734693878,
 83.1632653061225, 85.1428571428571, 87.1224489795918, 89.1020408163265,
 91.0816326530612, 93.0612244897959, 95.0408163265306, 97.0204081632653
 )
bknots <-
#  attr(bmat, "bknots") |> dput()
 c(1, 100)

bbasis__impl(1:100, iknots, bknots, 4)


print_bmat <- capture.output(bmat)

stopifnot(any(grepl("First\\s\\d+\\srows:", print_bmat)))
stopifnot(identical(print_bmat[1], "Basis matrix dims: [100 x 52]"))
stopifnot(identical(print_bmat[2], "Order: 4"))
stopifnot(identical(print_bmat[3], "Number of internal knots: 48"))
stopifnot(identical(print_bmat[4], ""))
stopifnot(identical(print_bmat[5], "First 6 rows:"))
stopifnot(identical(print_bmat[6], ""))

print_bmat <- capture.output(print(bmat, n = 1000))
stopifnot(!any(grepl("First\\s\\d+\\srows:", print_bmat)))


################################################################################
# knots will be sorted
xvec <- seq(-1, 1, length = 25)
iknots <- c(0.34, -0.23)

x <- tryCatch(bsplines(xvec, iknots = iknots), warning = function(w) {w})

stopifnot("testing if warning is given when sorting knots" =
          identical(class(x), c("simpleWarning", "warning", "condition"))
        )

stopifnot("test warning message" = identical(x$message, "Sorting knots"))

stopifnot("expected bases generated after sorting knots" = isTRUE(
  all.equal(
              current =   unclass(suppressWarnings(bsplines(xvec, iknots = c(.34, -.23))))
            , target = unclass(splines::bs(xvec, knots = iknots, intercept = TRUE))
            , check.attributes = FALSE
            )
  )
)

################################################################################
x <- tryCatch(bsplines(list(1:10)), error = function(e) { e })
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot("error if list passed to bsplines" = identical(x$message, "x is a list.  use cpr::btensor instead of cpr::bsplines."))


################################################################################
# combinations of iknots and df are handled well

xvec <- seq(-1, 1, length = 25)

x <- tryCatch(bsplines(xvec, df = 2), warning = function(w) {w})
stopifnot(identical(class(x), c("simpleWarning", "warning", "condition")))
stopifnot(identical(x$message, "df being set to order"))

x <- suppressWarnings(bsplines(xvec, df = 2))
y <- bsplines(xvec, df = 4)
stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))

x <- suppressWarnings(bsplines(xvec, df = 4, order = 5))
y <- bsplines(xvec, df = 5, order = 5)
z <- bsplines(xvec, order = 5)
stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(x, z, check.attributes = FALSE)))

x <- tryCatch(bsplines(xvec, iknots = 0.2, df = 6), warning = function(w) {w})
stopifnot(identical(class(x), c("simpleWarning", "warning", "condition")))
stopifnot(identical(x$message, "Both iknots and df defined, using iknots"))

x <- suppressWarnings(bsplines(xvec, iknots = 0.2, df = 6))
y <- bsplines(xvec, iknots = 0.2)
stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))

x <- bsplines(xvec, df = 7)
y <- bsplines(xvec, iknots = trimmed_quantile(xvec, probs = 1:3/4))
stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))


################################################################################
# derivatives are as expected
xvec <- seq(-1, 5, length = 100)
iknots <- c(-0.8, 0.2, 2.3, 2.5)

xi <- sort(c(rep(range(xvec), 4), iknots))

baseD1 <- spline.des(xi, xvec, derivs = rep(1, length(xvec)))$design
baseD2 <- spline.des(xi, xvec, derivs = rep(2, length(xvec)))$design

cprD1 <- bsplineD(xvec, iknots = iknots)
cprD2 <- bsplineD(xvec, iknots = iknots, derivative = 2L)

stopifnot(isTRUE( all.equal(cprD1[-100, ], baseD1[-100, ])))
stopifnot(isTRUE( all.equal(cprD2[-100, ], baseD2[-100, ])))

x <- tryCatch(bsplineD(xvec, derivative = 1.5), error = function(e) {e})
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "Only first and second derivatives are supported"))
rm(x)

x <- tryCatch(bsplineD(xvec, derivative = 3), error = function(e) {e})
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "Only first and second derivatives are supported"))

################################################################################
##                                End of File                                 ##
################################################################################
