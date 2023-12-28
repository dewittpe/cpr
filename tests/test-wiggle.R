# verify wiggle returns the integeral of the squared second derivative

library(cpr)

################################################################################
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

fdoubleprime_squared <- function(x) {
  fdoubleprime(x)^2
}

bknots = c(-3, 5)
x     <- (runif(n = 100, min = -3, max = 5))
bmat  <- bsplines(x, bknots = bknots)
theta <- matrix(coef(lm(f(x) ~ bmat + 0)), ncol = 1)

bmatD1 <- bsplineD(x, bknots = bknots, derivative = 1L)
bmatD2 <- bsplineD(x, bknots = bknots, derivative = 2L)

stopifnot(isTRUE(all.equal(
  integrate(fdoubleprime_squared, lower = -3, upper = 5)$value
  ,
  wiggle(cp(bmat, theta), lower = -3, upper = 5)$value
)))

################################################################################
# verify the result is the same if the bounds are not specified
stopifnot(isTRUE(all.equal(
  integrate(fdoubleprime_squared, lower = -3, upper = 5)$value
  ,
  wiggle(cp(bmat, theta))$value
)))

################################################################################
# verify the wiggle result is the same if the bounds are set too wide
stopifnot(isTRUE(all.equal(
  integrate(fdoubleprime_squared, lower = -3, upper = 5)$value
  ,
  wiggle(cp(bmat, theta, lower = -10, upper = 20))$value
)))

################################################################################
#                              Count Sign Changes                              #

# sign changes in fprime
# the data need to be sorted for fprime call
stopifnot(identical(
  sum(abs(diff(sign(fprime(sort(x)))))) / 2L
  ,
  sign_changes( object = cp(bmat, theta), lower = -3, upper = 5, derivative = 1)
  )
)

# sign_changes in fdoubleprime
stopifnot(identical(
  sum(abs(diff(sign(fdoubleprime(sort(x)))))) / 2L
  ,
  sign_changes( object = cp(bmat, theta), lower = -3, upper = 5, derivative = 2)
  )
)

################################################################################
# verify the sign_changes result is the same if the bounds are set too wide

warn <- tryCatch(sign_changes(cp(bmat, theta), lower = -10, upper = 20, derivative = 1)
                 , warning = function(w) w)
stopifnot(inherits(warn, "warning"))
stopifnot(warn$message == "setting lower = min(object$bknots)")


stopifnot(isTRUE(all.equal(
  sign_changes( object = cp(bmat, theta), lower = -3, upper = 5, derivative = 1)
  ,
  suppressWarnings(sign_changes(cp(bmat, theta), lower = -10, upper = 20, derivative = 1))
)))


################################################################################
#                                 End of File                                  #
################################################################################
