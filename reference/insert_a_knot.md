# Insert a Knot into a Control Polygon

Insert a knot into a control polygon without changing the spline

## Usage

``` r
insert_a_knot(x, xi_prime, ...)
```

## Arguments

- x:

  a `cpr_cp` object

- xi_prime:

  the value of the knot to insert

- ...:

  not currently used

## Value

a `cpr_cp` object

## Examples

``` r
x <- seq(1e-5, 5.99999, length.out = 100)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
cp0 <- cp(bmat, theta)
cp1 <- insert_a_knot(x = cp0, xi_prime = 3)
plot(cp0, cp1, color = TRUE, show_spline = TRUE)
#> Warning: Removed 27 rows containing missing values or values outside the scale range
#> (`geom_rug()`).
```
