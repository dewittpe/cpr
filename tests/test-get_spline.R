library(cpr)

x1 <- seq(0, 5.9999, length = 500)
x2 <- seq(0, 6 - sqrt(.Machine$double.eps), length = 123)
bmat1 <- bsplines(x = x1, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat2 <- bsplines(x = x2, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)

cp1 <- cp(bmat1, theta)
cp2 <- cp(bmat2, theta)

spline <- get_spline(cp1, n = 123)

stopifnot(isTRUE(
  all.equal(
            spline
            ,
            data.frame(x = x2, y = as.numeric(bmat2 %*% theta))
  )
))


################################################################################
#                                 End of File                                  #
################################################################################
