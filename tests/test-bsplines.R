library(cpr)
require(splines)

################################################################################
e <- new.env()
with(e, {
  base_bs <- unclass(splines::bs(0:10, knots = c(2, 2.6, 7.8), Boundary.knots = c(0, 10.2), intercept = TRUE))
  cpr_bspline <-  unclass(bsplines(0:10, iknots = c(2, 2.6, 7.8), bknots = c(0, 10.2), order = 4))

  base_bs |> tail()
  cpr_bspline |> tail()

  stopifnot("Equivalent Basis Matrix between cpr and splines::bs when all(x < max(bknots)" =
    isTRUE(
      all.equal(current = cpr_bspline, target = base_bs, check.attributes = FALSE)
    )
  )
})

################################################################################
# print.cpr_bs

e <- new.env()
with(e, {
  bmat       <- bsplines(runif(n = 100, 1, 100), bknots = c(1, 100), df = 52)

  stopifnot(identical(bmat, print(bmat)))

  print_bmat <- capture.output(bmat)

  stopifnot(any(grepl("First\\s6\\srows:", print_bmat))) # default
  stopifnot(identical(print_bmat[1], "Basis matrix dims: [100 x 52]"))
  stopifnot(identical(print_bmat[2], "Order: 4"))
  stopifnot(identical(print_bmat[3], "Number of internal knots: 48"))
  stopifnot(identical(print_bmat[4], ""))
  stopifnot(identical(print_bmat[5], "First 6 rows:"))
  stopifnot(identical(print_bmat[6], ""))

  print_bmat <- capture.output(print(bmat, n = 1000))
  stopifnot(!any(grepl("First\\s\\d+\\srows:", print_bmat)))

  print_bmat <- capture.output(print(bmat, n = 1001))
  stopifnot(!any(grepl("First\\s\\d+\\srows:", print_bmat)))

  print_bmat <- capture.output(print(bmat, n = 10))
  stopifnot(any(grepl("First\\s10\\srows:", print_bmat)))
})

e <- new.env()
with(e, {
  bmat       <- bsplines(runif(n = 2, 1, 100), bknots = c(1, 100), df = 52)
  print_bmat <- capture.output(bmat)

  stopifnot(!any(grepl("First\\s\\d+\\srows:", print_bmat)))
  stopifnot(identical(print_bmat[1], "Basis matrix dims: [2 x 52]"))
  stopifnot(identical(print_bmat[2], "Order: 4"))
  stopifnot(identical(print_bmat[3], "Number of internal knots: 48"))
  stopifnot(identical(print_bmat[4], ""))
})

################################################################################
# Unsorted knots will result in an error

e <- new.env()
with(e, {
  xvec <- runif(n = 25, min = -1, max = 1)
  iknots <- c(0.34, -0.23)
  bknots = c(-1, 1)

  x <- tryCatch(bsplines(xvec, iknots = iknots, bknots = bknots), error = function(e) {e})
  stopifnot(inherits(x, "simpleError"))
  stopifnot(identical(x$message, "Knots are not sorted."))

  x <- tryCatch(bsplines(xvec, iknots = sort(iknots), bknots = bknots), error = function(e) {e})
  stopifnot(inherits(x, "cpr_bs"))

  x <- tryCatch(bsplines(xvec, iknots = sort(iknots), bknots = bknots), error = function(e) {e})
  stopifnot(inherits(x, "cpr_bs"))

  x <- tryCatch(bsplines(xvec, iknots = sort(iknots), bknots = c(-0.21, 1)), warning = function(w) {w})
  stopifnot(inherits(x, "warning"))
  stopifnot(identical(x$message, "At least one x value < min(bknots)"))

  x <- tryCatch(bsplines(xvec, iknots = sort(iknots), bknots = c(-1, 0)), warning = function(w) {w})
  stopifnot(inherits(x, "warning"))
  stopifnot(identical(x$message, "At least one x value >= max(bknots)"))

})

################################################################################
# Error if a list is passed to bsplines
e <- new.env()
with(e, {
  x <- tryCatch(bsplines(list(1:10)), error = function(e) { e })
  stopifnot(inherits(x, "simpleError"))
  stopifnot("error if list passed to bsplines" = identical(x$message, "x is a list. Use btensor instead of bsplines."))
})

################################################################################
# combinations of iknots and df are handled well
e <- new.env()
with(e, {
  xvec <- seq(-1, 1-1e-5, length = 25)
  bknots <- c(-1, 1)

  x <- tryCatch(bsplines(xvec, df = 2, bknots = bknots), warning = function(w) {w})
  stopifnot(identical(class(x), c("simpleWarning", "warning", "condition")))
  stopifnot(identical(x$message, "df being set to order"))

  x <- suppressWarnings(bsplines(xvec, df = 2, bknots = bknots))
  y <- bsplines(xvec, df = 4, bknots = bknots)
  stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))

  x <- suppressWarnings(bsplines(xvec, df = 4, order = 5, bknots = bknots))
  y <- bsplines(xvec, df = 5, order = 5, bknots = bknots)
  z <- bsplines(xvec, order = 5, bknots = bknots)
  stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))
  stopifnot(isTRUE(all.equal(x, z, check.attributes = FALSE)))

  x <- tryCatch(bsplines(xvec, iknots = 0.2, df = 6, bknots = bknots), warning = function(w) {w})
  stopifnot(identical(class(x), c("simpleWarning", "warning", "condition")))
  stopifnot(identical(x$message, "Both iknots and df defined, using iknots"))

  x <- suppressWarnings(bsplines(xvec, iknots = 0.2, df = 6, bknots = bknots))
  y <- bsplines(xvec, iknots = 0.2, bknots = bknots)
  stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))

  x <- bsplines(xvec, df = 7, bknots = bknots)
  y <- bsplines(xvec, iknots = trimmed_quantile(xvec, probs = 1:3/4), bknots = bknots)
  stopifnot(isTRUE(all.equal(x, y, check.attributes = FALSE)))
})

################################################################################
# derivatives are as expected

e <- new.env()
with(e, {
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


  #x <- seq(-3, 5, length = 100)
  x <- sort(runif(n = 100, min = -3, max = 5))
  bknots = c(-3, 5)
  bmat <- bsplines(x, bknots = bknots)
  theta <- coef(lm(f0(x) ~ bsplines(x, bknots = bknots) + 0) )

  baseD1 <- splines::spline.des(c(-3, -3, -3, -3, 5, 5, 5, 5), x, derivs = rep(1, length(x)))$design
  cprD1 <- bsplineD(x, bknots = bknots, derivative = 1L)

  head(baseD1); head(cprD1)
  tail(baseD1); tail(cprD1)

  baseD2 <- splines::spline.des(c(-3, -3, -3, -3, 5, 5, 5, 5), x, derivs = rep(2, length(x)))$design
  cprD2 <- bsplineD(x, bknots = bknots, derivative = 2L)

  # verify that I can get cubic correct
  stopifnot(isTRUE(all.equal(f0(x), as.numeric(bmat %*% theta))))

  # verify that I can get the first derivative correct -- this is different from
  # base R in that, by my construction, the derivative at the boundardy knots
  # should be NA, as the k-fold knots should result in no differentiability at
  # these points.
  stopifnot(isTRUE(all.equal(f1(x), as.numeric(cprD1 %*% theta))))

  # verify that I can get the second derivative correct
  stopifnot(isTRUE(all.equal(f2(x), as.numeric(cprD2 %*% theta))))

  if (interactive()) {
    par(mfrow = c(1, 3))
    plot(x, f0(x), type = "l")
    points(x, bmat %*% theta, pch = 2, col = 'blue')

    plot(x, f1(x), type = "l")
    points(x, baseD1 %*% theta, pch = 16, col = 'red', cex = 0.7)
    points(x, cprD1 %*% theta, pch = 2, col = 'blue')

    plot(x, f2(x), type = "l")
    points(x, baseD2 %*% theta, pch = 16, col = 'red', cex = 0.7)
    points(x, cprD2 %*% theta, pch = 2, col = 'blue')
  }

  # another set of tests
  xvec <- seq(-1, 5, length = 100)
  iknots <- c(-0.8, 0.2, 2.3, 2.5)

  xi <- sort(c(rep(range(xvec), 4), iknots))

  baseD1 <- spline.des(xi, xvec, derivs = rep(1, length(xvec)))$design
  baseD2 <- spline.des(xi, xvec, derivs = rep(2, length(xvec)))$design

  cprD1 <- bsplineD(xvec, iknots = iknots, derivative = 1L)
  cprD2 <- bsplineD(xvec, iknots = iknots, derivative = 2L)

  stopifnot(isTRUE(
                   all.equal(
                             cprD1[-100, ]
                             ,
                             baseD1[-100, ]
                   )
                   ))
  stopifnot(isTRUE( all.equal(cprD2[-100, ], baseD2[-100, ])))

  x <- tryCatch(bsplineD(xvec, derivative = 1.5), error = function(e) {e})
  stopifnot(identical(class(x), c("simpleError", "error", "condition")))
  stopifnot(identical(x$message, "Only first and second derivatives are supported"))
  rm(x)

  x <- tryCatch(bsplineD(xvec, derivative = 3), error = function(e) {e})
  stopifnot(identical(class(x), c("simpleError", "error", "condition")))
  stopifnot(identical(x$message, "Only first and second derivatives are supported"))

  x <- tryCatch(bsplineD(xvec, order = 2, derivative = 2), error = function(e) {e})
  stopifnot(identical(class(x), c("simpleError", "error", "condition")))
  stopifnot(identical(x$message, "(order - 2) <= 0"))
})

################################################################################
# verify that order is an integer of at least 2
e <- new.env()
with(e, {
  xvec    <- runif(n = 100, min = -1, max = 5)
  bknots  <- c(-1, 5)
  bmat    <- tryCatch(bsplines(xvec, bknots = bknots, order = 1), error = function(e) e)
  bmatD1  <- tryCatch(bsplineD(xvec, bknots = bknots, order = 1, derivative = 1L), error = function(e) e)
  bmatD2  <- tryCatch(bsplineD(xvec, bknots = bknots, order = 1, derivative = 2L), error = function(e) e)

  stopifnot(inherits(bmat, "simpleError"))
  stopifnot(inherits(bmatD1, "simpleError"))
  stopifnot(inherits(bmatD2, "simpleError"))

  stopifnot(identical(bmat$message, "order needs to be an integer value >= 2."))
  stopifnot(identical(bmatD1$message, "order needs to be an integer value >= 2."))
  stopifnot(identical(bmatD2$message, "order needs to be an integer value >= 2."))

  bmat <- tryCatch(bsplines(xvec, bknots = bknots, order = 1.9), error = function(e) e)
  stopifnot(inherits(bmat, "simpleError"))
  stopifnot(identical(bmat$message, "order needs to be an integer value >= 2."))

  bmat <- tryCatch(bsplines(xvec, bknots = bknots, order = 2.9), error = function(e) e)
  stopifnot(inherits(bmat, "cpr_bs"))
  stopifnot(identical(attr(bmat, "order"), 2))

})

################################################################################
# attributes of bsplines
e <- new.env()
with(e, {
  xvec <- runif(n = 100, min = -1, max = 5)
  iknots <- c(-0.8, 0.2, 2.3, 2.5)
  bknots <- c(-1.2, 5.2)

  bmat   <- tryCatch(bsplines(xvec, iknots = iknots, bknots = bknots), error = function(e) e)

  stopifnot(inherits(bmat, "cpr_bs"))

  stopifnot(identical(
      names(attributes(bmat))
      ,
      c("dim", "order", "df", "iknots", "bknots", "xi", "xi_star", "class", "call", "environment")
    )
  )

  stopifnot(identical(attr(bmat, "dim"), c(100L, 8L)))

  stopifnot(identical(attr(bmat, "order"), 4))

  stopifnot(identical(attr(bmat, "df"), 4 + length(iknots)))

  stopifnot(identical(attr(bmat, "iknots"),   iknots))

  stopifnot(identical(attr(bmat, "bknots"),   bknots))

  stopifnot(identical(attr(bmat, "xi"),   sort(c(rep(bknots, 4), iknots))))

  stopifnot(identical(attr(bmat, "derivative"),   NULL))

})

################################################################################
##                                End of File                                 ##
################################################################################
