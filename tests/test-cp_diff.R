library(cpr)

xvec <- runif(n = 500, min = 0, max = 6)

# Define the basis matrix
bmat1 <- bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat2 <- bsplines(x = xvec, bknots = c(0, 6))

# Define the control vertices ordinates
theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
theta2 <- c(1, 3.4, -2, 1.7)

# build the two control polygons
cp1 <- cp(bmat1, theta1)
cp2 <- cp(bmat2, theta2)

stopifnot(
  isTRUE(
    all.equal(
      cp_diff(cp1, cp2)
      ,
      c(0, 1.4, -1.5, -1.28, -1.92, -0.42, 0.241666666666666, -1.225,  0.2)
    )
  )
)


################################################################################
#                                 End of file                                  #
################################################################################
