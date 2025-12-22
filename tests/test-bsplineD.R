library(cpr)
require(splines)

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

  x <- sort(runif(n = 100, min = -3, max = 5))
  bknots = c(-3, 5)
  bmat <- bsplines(x, bknots = bknots)
  theta <- coef(lm(f0(x) ~ bsplines(x, bknots = bknots) + 0) )

  baseD1 <- splines::spline.des(c(-3, -3, -3, -3, 5, 5, 5, 5), x, derivs = rep(1, length(x)))$design
  cprD1 <- bsplineD(x, bknots = bknots, derivative = 1L)

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
  stopifnot(inherits(x, "error"))
  stopifnot(identical(x$message, "Only first and second derivatives are supported"))

  x <- tryCatch(bsplineD(xvec, derivative = 3), error = function(e) {e})
  stopifnot(inherits(x, "error"))
  stopifnot(identical(x$message, "Only first and second derivatives are supported"))

  x <- tryCatch(bsplineD(xvec, order = 2, derivative = 2), error = function(e) {e})
  stopifnot(inherits(x, "error"))
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

  stopifnot(inherits(bmat, "error"))
  stopifnot(inherits(bmatD1, "error"))
  stopifnot(inherits(bmatD2, "error"))

  stopifnot(identical(bmat$message, "order needs to be an integer value >= 2."))
  stopifnot(identical(bmatD1$message, "order needs to be an integer value >= 2."))
  stopifnot(identical(bmatD2$message, "order needs to be an integer value >= 2."))

  bmat <- tryCatch(bsplines(xvec, bknots = bknots, order = 1.9), error = function(e) e)
  stopifnot(inherits(bmat, "error"))
  stopifnot(identical(bmat$message, "order needs to be an integer value >= 2."))

  bmat <- tryCatch(bsplines(xvec, bknots = bknots, order = 2.9), error = function(e) e)
  stopifnot(inherits(bmat, "cpr_bs"))
  stopifnot(identical(attr(bmat, "order"), 2))

})

################################################################################
# attributes of bsplineD
e <- new.env()
with(e, {
  xvec <- runif(n = 100, min = -1, max = 5)
  iknots <- c(-0.8, 0.2, 2.3, 2.5)
  bknots <- c(-1.2, 5.2)

  bmatD1 <- tryCatch(bsplineD(xvec, iknots = iknots, bknots = bknots, derivative = 1L), error = function(e) e)
  bmatD2 <- tryCatch(bsplineD(xvec, iknots = iknots, bknots = bknots, derivative = 2L), error = function(e) e)

  stopifnot(inherits(bmatD1, "cpr_bsD1"))
  stopifnot(inherits(bmatD2, "cpr_bsD2"))

  stopifnot(identical(
      names(attributes(bmatD1))
      ,
      c("dim", "order", "df", "iknots", "bknots", "xi", "xi_star", "derivative", "class")
    )
  )

  stopifnot(identical(
      names(attributes(bmatD2))
      ,
      c("dim", "order", "df", "iknots", "bknots", "xi", "xi_star", "derivative", "class")
    )
  )

  stopifnot(identical(attr(bmatD1, "dim"), c(100L, 8L)))
  stopifnot(identical(attr(bmatD2, "dim"), c(100L, 8L)))

  stopifnot(identical(attr(bmatD1, "order"), 4))
  stopifnot(identical(attr(bmatD2, "order"), 4))

  stopifnot(identical(attr(bmatD1, "df"), 4 + length(iknots)))
  stopifnot(identical(attr(bmatD2, "df"), 4 + length(iknots)))

  stopifnot(identical(attr(bmatD1, "iknots"), iknots))
  stopifnot(identical(attr(bmatD2, "iknots"), iknots))

  stopifnot(identical(attr(bmatD1, "bknots"), bknots))
  stopifnot(identical(attr(bmatD2, "bknots"), bknots))

  stopifnot(identical(attr(bmatD1, "xi"), sort(c(rep(bknots, 4), iknots))))
  stopifnot(identical(attr(bmatD2, "xi"), sort(c(rep(bknots, 4), iknots))))

  stopifnot(identical(attr(bmatD1, "derivative"), 1L))
  stopifnot(identical(attr(bmatD2, "derivative"), 2L))

})

################################################################################
##                                End of File                                 ##
################################################################################
