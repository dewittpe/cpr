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
# Unsorted knots will result in an error
xvec <- seq(-1, 1, length = 25)
iknots <- c(0.34, -0.23)

x <- tryCatch(bsplines(xvec, iknots = iknots), error = function(e) {e})
stopifnot(inherits(x, "simpleError"))

x <- tryCatch(bsplines(xvec, iknots = sort(iknots)), error = function(e) {e})
stopifnot(inherits(x, "cpr_bs"))

x <- tryCatch(bsplines(xvec, iknots = sort(iknots), bknots = c(1, -1)), error = function(e) {e})
stopifnot(inherits(x, "simpleError"))

x <- tryCatch(bsplines(xvec, iknots = sort(iknots), bknots = c(-1, 1)), error = function(e) {e})
stopifnot(inherits(x, "cpr_bs"))

x <- tryCatch(bsplines(xvec, iknots = sort(iknots), bknots = c(-0.21, 1)), error = function(e) {e})
stopifnot(inherits(x, "simpleError"))


################################################################################
# Error if a list is passed to bsplines
x <- tryCatch(bsplines(list(1:10)), error = function(e) { e })
stopifnot(inherits(x, "simpleError"))
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

f0 <- function(x) {
  #(x + 2) * (x - 1) * (x - 3)
  x^3 - 2 * x^2 - 5 * x + 6
}
f1 <- function(x) {
  3 * x^2 - 4 * x - 5
}
f2 <- function(x) {
  6 * x - 4
}

x <- seq(-3, 5, length = 100)
plot(x, f0(x), type = "l")
bmat <- bsplines(x)
theta <- coef(lm(f0(x) ~ bsplines(x) + 0) )
points(x, bmat %*% theta, col = 'red')

plot(x, f1(x), type = "l")
baseD1 <- spline.des(c(-3, -3, -3, -3, 5, 5, 5, 5), x, derivs = rep(1, length(x)))$design
cprD1 <- bsplineD(x)
points(x, baseD1 %*% theta, col = 'red')
points(x, cprD1 %*% theta, pch = 2, col = 'blue')

plot(x, f2(x), type = "l")
baseD2 <- spline.des(c(-3, -3, -3, -3, 5, 5, 5, 5), x, derivs = rep(2, length(x)))$design
cprD2 <- bsplineD(x)
points(x, baseD2 %*% theta, col = 'red')
points(x, cprD2 %*% theta, pch = 2, col = 'blue')






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
