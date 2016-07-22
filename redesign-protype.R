library(rbenchmark)
library(Rcpp)
library(RcppArmadillo)

sourceCpp(file = "redesign.cpp")

xvec <- seq(0, 1, length = 12)
j    <- 1
xi   <- c(0, 0, 0, 0, 0.2, 0.3, 1, 1, 1, 1)
k    <- 4

bspline__impl(xvec, 0, k, xi)
bbasis__impl(xvec, c(0.2, 0.3), range(xvec), 4)
cpr::bsplines(xvec, iknots = c(0.2, 0.3))
splines::bs(xvec, knots = c(0.2, 0.3), intercept = TRUE)


benchmark(
  bbasis__impl(xvec, c(0.2, 0.3), range(xvec), 4),
  cpr::bsplines(xvec, iknots = c(0.2, 0.3)),
  splines::bs(xvec, knots = c(0.2, 0.3), intercept = TRUE),
  replications = 10000
)
