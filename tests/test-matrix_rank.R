library(cpr)

################################################################################
# test that matrix rank is as expected
set.seed(42)
mat <- matrix(rnorm(25000 * 120), nrow = 25000)
stopifnot(identical(matrix_rank(mat), 120))

################################################################################
# test that matrix rank of a full rank bspline basis is as expected
x <- runif(100, 0, 1)
bmat <- bsplines(x = x, bknots = c(0, 1), df = 15)
stopifnot(identical(matrix_rank(bmat), 15))

################################################################################
# test that matrix rank of a rank-deficient bspline basis
bmat <- bsplines(x, bknots = c(0, 1), iknots = c(0.5, 0.5, 0.5, 0.5, 0.5))
stopifnot(identical(ncol(bmat), 9L))
stopifnot(identical(matrix_rank(bmat), 8))

################################################################################
##                                End of File                                 ##
################################################################################
