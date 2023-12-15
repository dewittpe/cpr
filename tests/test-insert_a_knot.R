library(cpr)

x     <- seq(1e-5, 5.99999, length.out = 100)
bmat  <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
cp0   <- cp(bmat, theta)
cp1   <- insert_a_knot(x = cp0, xi_prime = 3)

stopifnot(inherits(cp1, "cpr_cp"))

stopifnot(isTRUE(
    all.equal(
      cp1$cp
      ,
      structure(list(xi_star = c(0, 0.333333333333333, 0.833333333333333, 1.6, 2.26666666666667, 3.1, 3.83333333333333, 4.83333333333333, 5.5, 6),
                     theta = c(1, 0, 3.5, 4.2, 3.86666666666667, 1.6, -0.537837837837838, -0.7, 2, 1.5)),
                class = "data.frame",
                row.names = c(NA, -10L))
      )
    )
)
