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

# sign changes in fprime
stopifnot(identical(
  sum(abs(diff(sign(fprime(x))))) / 2L
  ,
  sign_changes( object = cp(bmat, theta), lower = -3, upper = 5, derivative = 1)
  )
)

# sign_changes in fdoubleprime
stopifnot(identical(
  sum(abs(diff(sign(fdoubleprime(x))))) / 2L
  ,
  sign_changes( object = cp(bmat, theta), lower = -3, upper = 5, derivative = 2)
  )
)

################################################################################
#                                 End of File                                  #
################################################################################