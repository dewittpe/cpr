################################################################################
# test_statistic_simulation.R
#
# A simple script for verifying the distribution of the test statistic under the
# null, that is, the influence of a non-influencial knot is chisq(df = 1)
#

library(cpr)
library(data.table)
library(parallel)

f <- function(n, sd = 1) {
  x <- seq(0 + 1/n, 6 - 1/n, length.out = n)
  bmat0 <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
  theta0 <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)

  bmat1 <- bsplines(x, iknots = c(1, 1.5, 2.3, 3, 4, 4.5), bknots = c(0, 6))

  dat <- data.table(x = x, y = as.numeric(bmat0 %*% theta0 + rnorm(n = n, mean = 0, sd = sd)))

  mod <- lm(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3, 4, 4.5), bknots = c(0, 6)) + 0, data = dat)

  theta1 = setNames(coef(mod), paste0("theta", 1:length(coef(mod))))
  Sigma  = unname(vcov(mod))

  Wmat <- cpr:::W(3, attr(bmat0, "xi"), 4)

  HAT  <- Wmat %*% solve(t(Wmat) %*% Wmat) %*% t(Wmat)
  IHAT <- (diag(length(theta1)) - HAT)

  cp1 <- cp(bmat1, theta1)
  icp1 <- influence_of_iknots(cp1)

  #test_stat
  d <- icp1$d[[4]] #cp1$cp  - icp1$restored_cps[[4]]$cp # checked - Okay.
  #all.equal(d, IHAT %*% theta1)

  test_stat <- (t(d) %*% MASS::ginv(IHAT %*% Sigma %*% t(IHAT)) %*% d)
  test_stat
}

f(100)

sim <- mclapply(1:1000, function(x) f(n = 5000, sd = 3), mc.cores = 8L)
sim <- do.call(c, sim)

sim |> ecdf() |> plot()
x <- seq(0, 12, length.out = 100)
points(x, pchisq(x, df = 1), col = 2, type = "b", lty = 2)
points(x, pchisq(x, df = 2), col = 3, type = "b", lty = 2)
points(x, pchisq(x, df = 3), col = 4, type = "b", lty = 2)
points(x, pchisq(x, df = 4), col = 5, type = "b", lty = 2)
points(x, pchisq(x, df = 5), col = 6, type = "b", lty = 2)
legend("bottomright", lty = 2, col = 2:5, legend = paste("df =", 2:5))

################################################################################
#                                 End of File                                  #
################################################################################
