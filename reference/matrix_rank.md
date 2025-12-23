# Rank of a Matrix

Determine the rank (number of linearly independent columns) of a matrix.

## Usage

``` r
matrix_rank(x)
```

## Arguments

- x:

  a numeric matrix

## Value

the rank of the matrix as a numeric value.

## Details

Implementation via the Armadillo C++ linear algebra library. The
function returns the rank of the matrix `x`. The computation is based on
the singular value decomposition of the matrix; a std::runtime_error
exception will be thrown if the decomposition fails. Any singular values
less than the tolerance are treated as zeros. The tolerance is
`max(m, n) * max_sv * arma::datum::eps`, where `m` is the number of rows
of `x`, `n` is the number of columns of `x`, `max_sv` is the maximal
singular value of `x`, and `arma::datum::eps` is the difference between
1 and the least value greater than 1 that is representable.

## References

Conrad Sanderson and Ryan Curtin. Armadillo: a template-based C++
library for linear algebra. Journal of Open Source Software, Vol. 1, pp.
26, 2016.

## Examples

``` r
# Check the rank of a matrix
set.seed(42)
mat <- matrix(rnorm(25000 * 120), nrow = 25000)
matrix_rank(mat) == ncol(mat)
#> [1] TRUE
matrix_rank(mat) == 120L
#> [1] TRUE

# A full rank B-spline basis
bmat <- bsplines(seq(0, 1, length = 100), df = 15)
#> Warning: At least one x value >= max(bknots)
matrix_rank(bmat) == 15L
#> [1] TRUE

# A rank deficient B-spline basis
bmat <- bsplines(seq(0, 1, length = 100), iknots = c(0.001, 0.002))
#> Warning: At least one x value >= max(bknots)
ncol(bmat) == 6L
#> [1] TRUE
matrix_rank(bmat) == 5L
#> [1] TRUE
```
