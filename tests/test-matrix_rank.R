library(cpr)

################################################################################
# test that matrix rank is as expected
set.seed(42)
mat <- matrix(rnorm(25000 * 120), nrow = 25000)
stopifnot(identical(matrix_rank(mat), 120))

################################################################################
# test that matrix rank of a full rank bspline basis is as expected
bmat <- bsplines(seq(0, 1, length = 100), df = 15)
stopifnot(identical(matrix_rank(bmat), 15))

################################################################################
# test that matrix rank of a rank-deficient bspline basis
bmat <- bsplines(seq(0, 1, length = 100), iknots = c(0.001, 0.002))
stopifnot(identical(ncol(bmat), 6L))
stopifnot(identical(matrix_rank(bmat), 5))

################################################################################
##                                End of File                                 ##
################################################################################
