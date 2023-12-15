# verify wiggle returns the integeral of the squared second derivative


library(cpr)

f <- function(x) {
  #(x + 2) * (x - 1) * (x - 3)
  x^3 - 2 * x^2 - 5 * x + 6
}

fprime <- function(x) { # first derivatives of f(x)
  3 * x^2 - 4 * x - 5
}

fdoubleprime <- function(x) { # second derivatives of f(x)
  6 * x - 4
}

#'
#' We'll look at the function over
bknots = c(-3, 5)

x     <- seq(-3 + 1/100, 5 - 1/100, length.out = 100)
bmat  <- bsplines(x, bknots = bknots)
theta <- matrix(coef(lm(f(x) ~ bmat + 0)), ncol = 1)

bmatD1 <- bsplineD(x, bknots = bknots, derivative = 1L)
bmatD2 <- bsplineD(x, bknots = bknots, derivative = 2L)

fdoubleprime_squared <- function(x) {
  fdoubleprime(x)^2
}

stopifnot(isTRUE(all.equal(
  integrate(fdoubleprime_squared, lower = -3, upper = 5)$value
  ,
  wiggle(cp(bmat, theta), lower = -3, upper = 5)$value
)))

################################################################################
#                              Count Sign Changes                              #

stop("DEV WORK - PICK UP HERE")
fprime(x) |>
  sign() |>
  diff() |>
  abs() |>
  sum() / 2L


fdoubleprime(x) |>
  sign() |>
  diff() |>
  abs() |>
  sum() / 2L

sign_changes( object = cp(bmat, theta), lower = -3, upper = 5, derivative = 1L)
sign_changes( object = cp(bmat, theta), lower = -3, upper = 5, derivative = 2L)

plot(x, bmatD2 %*% theta)

object <- cp(bmat, theta)
  f <- function(x) {
    bsplineD(x,
              iknots = object$iknots,
              bknots = object$bknots,
              order  = object$order,
              derivative = 2L) %*%
     matrix(object$cp$theta, ncol = 1)
  }

x2 <- seq(min(bknots), max(bknots), length.out = 100000)
points(x2, f(x2), col = "red", type = "l")

################################################################################
#                                 End of File                                  #
################################################################################
