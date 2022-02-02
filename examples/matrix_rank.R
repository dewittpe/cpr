# Check the rank of a matrix
set.seed(42)
mat <- matrix(rnorm(25000 * 120), nrow = 25000)
matrix_rank(mat)

# A full rank B-spline basis
bmat <- bsplines(seq(0, 1, length = 100), df = 15)
matrix_rank(bmat)

# A rank deficient B-spline basis
bmat <- bsplines(seq(0, 1, length = 100), iknots = c(0.001, 0.002))
ncol(bmat)
matrix_rank(bmat)

